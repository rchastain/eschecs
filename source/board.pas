
unit Board;

interface

uses
  Classes,
  SysUtils,
  Math,
  TypInfo,
  fpg_main,
  BGRABitmap,
  BGRABitmapTypes,
  Images,
  Settings,
  Utils,
  ChessTypes;

type
  TChessPiece = record
    x, y: integer;
    FColor: TPieceColorStrict;
    FType: TPieceTypeStrict;
  end;

  TAnimData = record
    FAction: boolean;
    FIndex: integer;
    FCurrX, FCurrY, FStepX, FStepY, FTargX, FTargY: integer;
    FPromoType: TPieceType;
  end;

  TBGRAChessboard = class
    private
      FVirtScreen: TBGRABitmap;
      FPieceBackgr: TBGRABitmap;
      FScreenshot: TBGRABitmap;
      FPieces: array[1..32] of TChessPiece;
      FAnimData: TAnimData;
      FUpsideDown: boolean;
    public
      constructor Create(const AStyle: TBoardStyle; const AUpsideDown: boolean = FALSE; const APlacement: string = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR');
      destructor Destroy; override;
      procedure SetPieceXY(const AIndex, AX, AY: integer);
      procedure SetPieceType(const AIndex: integer; const AType: TPieceTypeStrict);
      procedure DrawPiece(const AIndex, AX, AY: integer; const ASetXY: boolean = FALSE); overload;
      procedure ReadPlacement(const APlacement: string; const ADraw: boolean = TRUE);
      procedure ResetAnimatData;
      procedure EraseBoard;
      procedure DrawToFPGCanvas(const ACanvas: TfpgCanvas; const x, y: integer);
      function Animate(out AEnd: boolean): boolean;
      procedure SetAnimatData(const AIndex, ACurrX, ACurrY, AStepX, AStepY, ATargX, ATargY: integer; const APromoType: TPieceType);
      function FindPiece(const AX, AY: integer): integer; overload;
      function FindPiece(const AX, AY: integer; const AColor: TPieceColorStrict): integer; overload;
      function FindPiece(const AX, AY: integer; const AColor: TPieceColorStrict; const AType: TPieceTypeStrict): integer; overload;
      function FindPiece(const AX, AY: integer; const AType: TPieceTypeStrict): integer; overload;
      procedure ErasePiece(const AIndex, AX, AY: integer); overload;
      procedure ErasePiece(const ASquare: string); overload;
      procedure MovePiece(const AIndex, AX, AY, ADX, ADY: integer; const APromoType: TPieceType); overload;
      procedure MovePiece(const AMove: string; const APromoType: TPieceType = ptNil); overload;
      procedure MoveKingRook(const AMove: string; const AComputerMove: boolean);
      procedure FlipBoard;
      procedure Highlight(const AX, AY: integer; const AColor: TBackColor; const APieceIndex: integer);
      procedure HighlightMove(const AMove: string; const APieceIndex: integer);
      procedure ScreenSave;
      procedure ScreenRestore;
      procedure SavePieceBackground(const AImagePos: TPoint; const ACreateFromChessboard: boolean = FALSE);
      procedure RestorePieceBackground(const AImagePos: TPoint);
      procedure DrawPiece(const AImagePos: TPoint; const APieceIndex: integer); overload;
      function XYToScreen(const AX, AY: integer): TPoint;
      function ScreenSaved(): boolean;
      procedure ScreenToXY(const AMousePos: TPoint; out AX, AY: integer);
      property UpsideDown: boolean read FUpsideDown;
  end;

var
  LHighlighted: string;

implementation

constructor TBGRAChessboard.Create(const AStyle: TBoardStyle; const AUpsideDown: boolean; const APlacement: string);
begin
  inherited Create;
  FVirtScreen := TBGRABitmap.Create(8 * LScale, 8 * LScale);
  FPieceBackgr := nil;
  FScreenshot := nil;
  ResetAnimatData;
  FUpsideDown := AUpsideDown;
  EraseBoard();
  ReadPlacement(APlacement);
end;

destructor TBGRAChessboard.Destroy;
begin
  FVirtScreen.Free;
  if Assigned(FPieceBackgr) then
    FPieceBackgr.Free;
  if Assigned(FScreenshot) then
    FScreenshot.Free;
  inherited Destroy;
end;

procedure TBGRAChessboard.SetPieceXY(const AIndex, AX, AY: integer);
begin
  FPieces[AIndex].x := AX;
  FPieces[AIndex].y := AY;
end;

procedure TBGRAChessboard.SetPieceType(const AIndex: integer; const AType: TPieceTypeStrict);
begin
  FPieces[AIndex].FType := AType;
end;

procedure TBGRAChessboard.DrawPiece(const AIndex, AX, AY: integer; const ASetXY: boolean = FALSE);
begin
  Assert(InRange(AIndex, 1, 32) and InRange(AX, 1, 8) and InRange(AY, 1, 8));
  FVirtScreen.PutImage(
    XToScreen(AX, FUpsideDown),
    YToScreen(AY, FUpsideDown),
    LPieceImage[FPieces[AIndex].FColor, FPieces[AIndex].FType],
    dmDrawWithTransparency
  );
  if ASetXY then SetPieceXY(AIndex, AX, AY);
end;

procedure TBGRAChessboard.ReadPlacement(const APlacement: string; const ADraw: boolean = TRUE);
  function DecodeColor(c: char): TPieceColorStrict; inline;
  begin
    if c = 'w' then result := pcWhite else result := pcBlack;
  end;
  function DecodeType(c: char): TPieceTypeStrict; inline;
  begin
    case c of
      'p': result := ptPawn;
      'n': result := ptKnight;
      'b': result := ptBishop;
      'r': result := ptRook;
      'q': result := ptQueen;
      else result := ptKing;
    end;
  end;
const
  CSymbols: set of char = ['B', 'K', 'N', 'P', 'Q', 'R', 'b', 'k', 'n', 'p', 'q', 'r', '1'..'8', '/'];
var
  i: integer;
  x, y: integer;
  c: char;
  LPieceIndex: integer;
  s: string;
begin
  i := 1;
  x := 1;
  y := 8;
  LPieceIndex := 1;
  while (i <= Length(APlacement))
  and (LPieceIndex <= 32)
  and (APlacement[i] in CSymbols) do
  begin
    c := APlacement[i];
    if c in ['1'..'8'] then
      while c > '0' do
      begin
        Inc(x);
        Dec(c);
      end
    else
    if c = '/' then
    begin
      x := 1;
      Dec(y);
    end else
    begin
      if c in ['B', 'K', 'N', 'P', 'Q', 'R'] then
        s := 'w' + LowerCase(c)
      else
        s := 'b' + c;
      FPieces[LPieceIndex].x := x;
      FPieces[LPieceIndex].y := y;
      FPieces[LPieceIndex].FColor := DecodeColor(s[1]);
      FPieces[LPieceIndex].FType := DecodeType(s[2]);
      if ADraw then
        DrawPiece(LPieceIndex, x, y);
      Inc(LPieceIndex);
      Inc(x);
    end;
    Inc(i);
  end;
end;

procedure TBGRAChessboard.ResetAnimatData;
const
  CZero: TAnimData = (
    FAction: FALSE;
    FIndex: 0;
    FCurrX: 0;
    FCurrY: 0;
    FStepX: 0;
    FStepY: 0;
    FTargX: 0;
    FTargY: 0;
    FPromoType: ptNil
  );
begin
  FAnimData := CZero;
end;

procedure TBGRAChessboard.EraseBoard;
begin
  FVirtScreen.PutImage(0, 0, LChessboard, dmSet);
end;

procedure TBGRAChessboard.DrawToFPGCanvas(const ACanvas: TfpgCanvas; const x, y: integer);
begin
  FVirtScreen.Draw(ACanvas, x, y);
end;

function TBGRAChessboard.Animate(out AEnd: boolean): boolean;
begin
  result := FAnimData.FAction;
  AEnd := FALSE;
  with FAnimData do
    if FAction then
    begin
      FVirtScreen.PutImage(FCurrX, FCurrY, FPieceBackgr, dmSet);
      Inc(FCurrX, FStepX);
      Inc(FCurrY, FStepY);
      if Assigned(FPieceBackgr) then
        FPieceBackgr.Free;
      FPieceBackgr := FVirtScreen.GetPart(RectWithSize(FCurrX, FCurrY, LScale, LScale)) as TBGRABitmap;
      if (FPromoType <> ptNil) and (FCurrX = FTargX) and (FCurrY = FTargY) then
        FPieces[FIndex].FType := FPromoType;
      FVirtScreen.PutImage(FCurrX, FCurrY, LPieceImage[FPieces[FIndex].FColor, FPieces[FIndex].FType], dmDrawWithTransparency);
      if (FCurrX = FTargX) and (FCurrY = FTargY) then
      begin
        AEnd := TRUE;
        if Length(LHighlighted) > 0 then
        begin
          ScreenSave;
          HighlightMove(LHighlighted, FIndex);
          LHighlighted := '';
        end;
        ResetAnimatData;
      end;
    end;
end;

procedure TBGRAChessboard.SetAnimatData(const AIndex, ACurrX, ACurrY, AStepX, AStepY, ATargX, ATargY: integer; const APromoType: TPieceType);
begin
  Assert(
    (Sign(ATargX - ACurrX) = Sign(AStepX))
    and ((AStepX = 0) or (Abs(ATargX - ACurrX) mod AStepX = 0))
    and (Sign(ATargY - ACurrY) = Sign(AStepY))
    and ((AStepY = 0) or (Abs(ATargY - ACurrY) mod AStepY = 0))
  );
  with FAnimData do
  begin
    FAction := TRUE;
    FIndex := AIndex;
    FCurrX := ACurrX;
    FCurrY := ACurrY;
    FStepX := AStepX;
    FStepY := AStepY;
    FTargX := ATargX;
    FTargY := ATargY;
    FPromoType := APromoType;
  end;
end;

function TBGRAChessboard.FindPiece(const AX, AY: integer): integer;
var
  i: integer;
begin
  result := 0;
  i := 1;
  while (i <= 32) and (result = 0) do
  begin
    if (FPieces[i].x = AX)
    and (FPieces[i].y = AY) then
      result := i;
    Inc(i);
  end;
end;

function TBGRAChessboard.FindPiece(const AX, AY: integer; const AColor: TPieceColorStrict): integer;
begin
  result := FindPiece(AX, AY);
  if (result <> 0) and (FPieces[result].FColor <> AColor) then
    result := 0;
end;

function TBGRAChessboard.FindPiece(const AX, AY: integer; const AColor: TPieceColorStrict; const AType: TPieceTypeStrict): integer;
begin
  result := FindPiece(AX, AY, AColor);
  if (result <> 0) and (FPieces[result].FType <> AType) then
    result := 0;
end;

function TBGRAChessboard.FindPiece(const AX, AY: integer; const AType: TPieceTypeStrict): integer;
var
  i: integer;
begin
  result := 0;
  i := 1;
  while (i <= 32) and (result = 0) do
  begin
    if (FPieces[i].x = AX)
    and (FPieces[i].y = AY)
    and (FPieces[i].FType = AType) then
      result := i;
    Inc(i);
  end;
end;

procedure TBGRAChessboard.ErasePiece(const AIndex, AX, AY: integer);
var
  LX, LY: integer;
  LEmptySquare: TBGRABitmap;
begin
  Assert(InRange(AIndex, 1, 32) and InRange(AX, 1, 8) and InRange(AY, 1, 8));
  LX := XToScreen(AX, FUpsideDown);
  LY := YToScreen(AY, FUpsideDown);
  LEmptySquare := LChessboard.GetPart(RectWithSize(LX, LY, LScale, LScale)) as TBGRABitmap;
  FVirtScreen.PutImage(
    LX,
    LY,
    LEmptySquare,
    dmSet
  );
  LEmptySquare.Free;
  SetPieceXY(AIndex, 0, 0);
end;

procedure TBGRAChessboard.ErasePiece(const ASquare: string);
var
  x, y, i: integer;
begin
  DecodeSquare(ASquare, x, y);
  i := FindPiece(x, y);
  Assert(i > 0);
  ErasePiece(i, x, y);
end;

procedure TBGRAChessboard.MovePiece(const AIndex, AX, AY, ADX, ADY: integer; const APromoType: TPieceType);
var
  a, b: integer;
  x1, y1, x2, y2: integer;
  LStep: integer;
begin
  Assert(InRange(AIndex, 1, 32) and InRange(AX, 1, 8) and InRange(AY, 1, 8) and InRange(ADX, -8, 8) and InRange(ADY, -8, 8));
  if LScale mod 10 = 0 then
    LStep := 10
  else if LScale mod 8 = 0 then
    LStep := 8
  else
    Assert(FALSE);
  a := 1;
  b := 1;
  if Abs(ADX) = 2 * Abs(ADY) then
    b := 2
  else
    if Abs(ADY) = 2 * Abs(ADX) then
      a := 2;
  x1 := XToScreen(AX, FUpsideDown);
  y1 := YToScreen(AY, FUpsideDown);
  if Assigned(FPieceBackgr) then
    FPieceBackgr.Free;
  FPieceBackgr := LChessboard.GetPart(RectWithSize(x1, y1, LScale, LScale)) as TBGRABitmap;
  x2 := XToScreen(AX + ADX, FUpsideDown);
  y2 := YToScreen(AY + ADY, FUpsideDown);
  SetAnimatData(
    AIndex,
    x1,
    y1,
    (LStep div a) * Sign(ADX) * (2 * Ord(FUpsideDown) - 1) * -1,
    (LStep div b) * Sign(ADY) * (2 * Ord(FUpsideDown) - 1),
    x2,
    y2,
    APromoType
  );
  SetPieceXY(AIndex, AX + ADX, AY + ADY);
end;

procedure TBGRAChessboard.MovePiece(const AMove: string; const APromoType: TPieceType);
var
  x1, y1, x2, y2, i: integer;
begin
  DecodeMove(AMove, x1, y1, x2, y2);
  i := FindPiece(x1, y1);
  Assert(i > 0);
  MovePiece(i, x1, y1, x2 - x1, y2 - y1, APromoType);
end;

procedure TBGRAChessboard.MoveKingRook(const AMove: string; const AComputerMove: boolean);
var
  x1, y1, x2, y2, iKing, iRook: integer;
begin
  DecodeMove(AMove, x1, y1, x2, y2);
  iKing := FindPiece(x1, y1, ptKing);
  iRook := FindPiece(x2, y2, ptRook);
  Assert(iKing > 0);
  Assert(iRook > 0);
  if AComputerMove then
    ErasePiece(iKing, x1, y1)
  else
    ErasePiece(iKing, x2, y2);
  ErasePiece(iRook, x2, y2);
  if x2 > x1 then
  begin
    DrawPiece(iKing, 7, y1, TRUE);
    DrawPiece(iRook, 6, y1, TRUE);
  end else
  begin
    DrawPiece(iKing, 3, y1, TRUE);
    DrawPiece(iRook, 4, y1, TRUE);
  end;
end;

procedure TBGRAChessboard.FlipBoard;
var
  LBoard, LSquare: TBGRABitmap;
  x, y: integer;
begin
  FUpsideDown := not FUpsideDown;
  LBoard := TBGRABitmap.Create(8 * LScale, 8 * LScale);
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      LSquare := FVirtScreen.GetPart(RectWithSize(
        XToScreen(x, not FUpsideDown),
        YToScreen(y, not FUpsideDown),
        LScale,
        LScale
      )) as TBGRABitmap;
      LBoard.PutImage(
        XToScreen(x, FUpsideDown),
        YToScreen(y, FUpsideDown),
        LSquare,
        dmSet
      );
      LSquare.Free;
    end;
  FVirtScreen.PutImage(0, 0, LBoard, dmSet);
  LBoard.Free;
end;

procedure TBGRAChessboard.Highlight(const AX, AY: integer; const AColor: TBackColor; const APieceIndex: integer);
var
  LSquare: TBGRABitmap;
  LPixel: TBGRAPixel;
begin
  LPixel :=  LBackColors[AColor];
  LSquare := LChessboard.GetPart(RectWithSize(XToScreen(AX, FALSE), YToScreen(AY, FALSE), LScale, LScale)) as TBGRABitmap;
  LSquare.FillRect(0, 0, LScale, LScale, LPixel, dmDrawWithTransparency);
  if APieceIndex > 0 then
    LSquare.PutImage(
      0,
      0,
      LPieceImage[
        FPieces[APieceIndex].FColor,
        FPieces[APieceIndex].FType
      ],
      dmDrawWithTransparency
    );
  FVirtScreen.PutImage(XToScreen(AX, FUpsideDown), YToScreen(AY, FUpsideDown), LSquare, dmSet);
  LSquare.Free;
end;

procedure TBGRAChessboard.HighlightMove(const AMove: string; const APieceIndex: integer);
var
  x1, y1, x2, y2: integer;
begin
  DecodeMove(AMove, x1, y1, x2, y2);
  HighLight(x1, y1, bcGreen, 0);
  HighLight(x2, y2, bcGreen, APieceIndex);
end;

procedure TBGRAChessboard.ScreenSave;
begin
  if FScreenshot = nil then
    FScreenshot := FVirtScreen.GetPart(RectWithSize(0, 0, 8 * LScale, 8 * LScale)) as TBGRABitmap;
end;

procedure TBGRAChessboard.ScreenRestore;
begin
  if Assigned(FScreenshot) then
  begin
    FVirtScreen.PutImage(0, 0, FScreenshot, dmSet);
    FScreenshot.Free;
    FScreenshot := nil;
  end;
end;

procedure TBGRAChessboard.SavePieceBackground(const AImagePos: TPoint; const ACreateFromChessboard: boolean = FALSE);
var
  LSrc: TBGRABitmap;
begin
  if ACreateFromChessboard then
    LSrc := LChessboard
  else
    LSrc := FVirtScreen;
  if Assigned(FPieceBackgr) then
    FPieceBackgr.Free;
  FPieceBackgr := LSrc.GetPart(RectWithSize(AImagePos.X, AImagePos.Y, LScale, LScale)) as TBGRABitmap;
end;

procedure TBGRAChessboard.RestorePieceBackground(const AImagePos: TPoint);
begin
  FVirtScreen.PutImage(AImagePos.X, AImagePos.Y, FPieceBackgr, dmSet);
end;

procedure TBGRAChessboard.DrawPiece(const AImagePos: TPoint; const APieceIndex: integer);
begin
  FVirtScreen.PutImage(
    AImagePos.X,
    AImagePos.Y,
    LPieceImage[
      FPieces[APieceIndex].FColor,
      FPieces[APieceIndex].FType
    ],
    dmDrawWithTransparency
  );
end;

function TBGRAChessboard.XYToScreen(const AX, AY: integer): TPoint;
begin
  result.x := XToScreen(AX, FUpsideDown);
  result.y := YToScreen(AY, FUpsideDown);
end;

function TBGRAChessboard.ScreenSaved(): boolean;
begin
  result := Assigned(FScreenshot);
end;

procedure TBGRAChessboard.ScreenToXY(const AMousePos: TPoint; out AX, AY: integer);
begin
  AX := AMousePos.X div LScale + 1;
  AY := 8 - AMousePos.Y div LScale;
  if UpsideDown then
  begin
    AX := 9 - AX;
    AY := 9 - AY;
  end;
end;

begin
  LHighlighted := ''; 
end.
