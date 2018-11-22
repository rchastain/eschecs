
unit Board;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
  fpg_main,
  BGRABitmap,
  BGRABitmapTypes,
  Images,
  Style,
  Utils,
  ChessTypes;

type
  TChessPiece = record
    x, y: integer;
    color: TChessPieceColor;
    kind: TChessPieceKind;
  end;

  TAnimationData = record
    action: boolean;
    index: integer;
    currX, currY, stepX, stepY, targX, targY: integer;
    promotion: boolean;
    promotionKind: TChessPieceKind;
  end;

  TBGRAChessboard = class
    private
      fVirtualScreen: TBGRABitmap;
      fPieceBackground: TBGRABitmap;
      fScreenshot: TBGRABitmap;
      fPieces: array[1..32] of TChessPiece;
      fAnimationData: TAnimationData;
      fUpsideDown: boolean;
    public
      constructor Create(const aBoardStyle: TBoardStyle = bsOriginal; const aUpsideDown: boolean = FALSE; const aPiecePlacement: string = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR');
      destructor Destroy; override;
      procedure SetPieceXY(const aIndex, aX, aY: integer);
      procedure SetPieceKind(const aIndex: integer; const aType: TChessPieceKind);
      procedure DrawPiece(const aIndex, aX, aY: integer; const aSetXY: boolean = FALSE); overload;
      procedure ReadPlacement(const aPlacement: string; const aDrawPieces: boolean = TRUE);
      procedure ResetAnimatData;
      procedure EraseBoard();
      procedure DrawToFPGCanvas(const aCanvas: TfpgCanvas; const x, y: integer);
      function Animate(out aAnimationEnd: boolean): boolean;
      procedure SetAnimatData(const aIndex, aCurrentX, aCurrentY, aStepX, aStepY, aTargetX, aTargetY: integer; const aPromotion: boolean; const aPromotionKind: TChessPieceKind);
      function FindPiece(const aX, aY: integer): integer; overload;
      function FindPiece(const aX, aY: integer; const aColor: TChessPieceColor): integer; overload;
      function FindPiece(const aX, aY: integer; const aColor: TChessPieceColor; const aType: TChessPieceKind): integer; overload;
      procedure ErasePiece(const aIndex, aX, aY: integer); overload;
      procedure ErasePiece(const aSquare: string); overload;
      procedure MovePiece(const aIndex, aX, aY, aDX, aDY: integer; const aPromotion: boolean; const aPromotionKind: TChessPieceKind); overload;
      procedure MovePiece(const aMove: string; const aPromotion: boolean = FALSE; const aPromotionKind: TChessPieceKind = cpkQueen); overload;
      procedure FlipBoard;
      procedure ChangeBoard(const aBoardStyle: TBoardStyle = bsOriginal);
      procedure Highlight(const aX, aY: integer; const aColor: TOutlineColor; const aPieceIndex: integer);
      procedure HighlightMove(const aMove: string; const aPieceIndex: integer);
      procedure ScreenSave;
      procedure ScreenRestore;
      procedure SavePieceBackground(const aImagePos: TPoint; const aCreateFromChessboard: boolean = FALSE);
      procedure RestorePieceBackground(const aImagePos: TPoint);
      procedure DrawPiece(const aImagePos: TPoint; const aPieceIndex: integer); overload;
      function XYToScreen(const aX, aY: integer): TPoint;
      function ScreenSaved(): boolean;
      procedure ScreenToXY(const aMousePos: TPoint; out aX, aY: integer);
      property isUpsideDown: boolean read fUpsideDown;
  end;

var
  vMoveToBeHighlighted: string;
  vComputerCastlingFlag: boolean;
  vKingIndex: integer;

implementation

constructor TBGRAChessboard.Create(const aBoardStyle: TBoardStyle; const aUpsideDown: boolean; const aPiecePlacement: string);
(*
var
  vCapture: TBGRABitmap;
*)
begin
{$IFDEF DEBUG}
  WriteLn('TBGRAChessboard.Create()');
{$ENDIF}
  inherited Create;
  Assert((gStyleData[gStyle].scale mod 2 = 0) and (gStyleData[gStyle].scale mod 5 = 0));
  //CreatePictures();
  fVirtualScreen := TBGRABitmap.Create(8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale);
  fPieceBackground := nil;
  fScreenshot := nil;
  ResetAnimatData;
  fUpsideDown := aUpsideDown;
  EraseBoard();
  ReadPlacement(aPiecePlacement);
  (*
  vCapture := TBGRABitmap.Create(2 * gStyleData[gStyle].scale, 2 * gStyleData[gStyle].scale);
  BGRAReplace(vCapture, fVirtualScreen.GetPart(RectWithSize(0, 0, 2 * gStyleData[gStyle].scale, 2 * gStyleData[gStyle].scale)));
  vCapture.SaveToFile(Format('%d.png', [gStyle]));
  vCapture.Free;
  *)
end;

destructor TBGRAChessboard.Destroy;
begin
{$IFDEF DEBUG}
  WriteLn('TBGRAChessboard.Destroy');
{$ENDIF}
  fVirtualScreen.Free;
  if Assigned(fPieceBackground) then
    fPieceBackground.Free;
  if Assigned(fScreenshot) then
  begin
{$IFDEF DEBUG}
    WriteLn('fScreenshot.Free');
{$ENDIF}
    fScreenshot.Free;
  end;
  //FreePictures;
  inherited Destroy;
end;

procedure TBGRAChessboard.SetPieceXY(const aIndex, aX, aY: integer);
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.SetPieceXY(%d, %d, %d)', [aIndex, aX, aY]));
{$ENDIF}
  fPieces[aIndex].x := aX;
  fPieces[aIndex].y := aY;
end;

procedure TBGRAChessboard.SetPieceKind(const aIndex: integer; const aType: TChessPieceKind);
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.SetPieceKind(%d, %s)', [aIndex, GetEnumName(TypeInfo(TChessPieceKind), Ord(aType))]));
{$ENDIF}
  fPieces[aIndex].kind := aType;
end;

procedure TBGRAChessboard.DrawPiece(const aIndex, aX, aY: integer; const aSetXY: boolean = FALSE);
var
  oc: TOutlineColor;
begin
  Assert(InRange(aIndex, 1, 32) and InRange(aX, 1, 8) and InRange(aY, 1, 8));

  if vCurrentStyle = bsOriginal then
    oc := ocWhite
  else
    oc := ocTransparent;
  fVirtualScreen.PutImage(
    XToScreen(aX, fUpsideDown),
    YToScreen(aY, fUpsideDown),
    vPieceImage[fPieces[aIndex].color, fPieces[aIndex].kind, oc],
    dmDrawWithTransparency
  );

  if aSetXY then SetPieceXY(aIndex, aX, aY);
end;

procedure TBGRAChessboard.ReadPlacement(const aPlacement: string; const aDrawPieces: boolean = TRUE);

  function DecodeColor(c: char): TChessPieceColor; inline;
  begin
    case c of
      'w': result := cpcWhite;
      'b': result := cpcBlack;
    end;
  end;

  function DecodeType(c: char): TChessPieceKind; inline;
  begin
    case c of
      'p': result := cpkPawn;
      'n': result := cpkKnight;
      'b': result := cpkBishop;
      'r': result := cpkRook;
      'q': result := cpkQueen;
      'k': result := cpkKing;
    end;
  end;

const
  SYMBOLS: set of char = ['B', 'K', 'N', 'P', 'Q', 'R', 'b', 'k', 'n', 'p', 'q', 'r', '1'..'8', '/'];
var
  i: integer;
  x, y: integer;
  c: char;
  index: integer;
  id: string;
begin
  i := 1;
  x := 1;
  y := 8;
  index := 1;
  while (i <= Length(aPlacement))
  and (index <= 32)
  and (aPlacement[i] in SYMBOLS) do
  begin
    c := aPlacement[i];
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
        id := 'w' + LowerCase(c)
      else
        id := 'b' + c;
      fPieces[index].x := x;
      fPieces[index].y := y;
      fPieces[index].color := DecodeColor(id[1]);
      fPieces[index].kind := DecodeType(id[2]);
      if aDrawPieces then
        DrawPiece(index, x, y);
      Inc(index);
      Inc(x);
    end;
    Inc(i);
  end;
end;

procedure TBGRAChessboard.ResetAnimatData;
const
  ZERO: TAnimationData = (
    action: FALSE;
    index: 0;
    currX: 0;
    currY: 0;
    stepX: 0;
    stepY: 0;
    targX: 0;
    targY: 0;
    promotion: FALSE;
    promotionKind: cpkQueen
  );
begin
  fAnimationData := ZERO;
end;

procedure TBGRAChessboard.EraseBoard();
begin
  fVirtualScreen.PutImage(0, 0, vChessboard, dmSet);
end;

procedure TBGRAChessboard.DrawToFPGCanvas(const aCanvas: TfpgCanvas; const x, y: integer);
begin
  fVirtualScreen.Draw(aCanvas, x, y);
end;

function TBGRAChessboard.Animate(out aAnimationEnd: boolean): boolean;
var
  oc: TOutlineColor;
begin
  result := fAnimationData.action;
  aAnimationEnd := FALSE;
  with fAnimationData do
    if action then
    begin
      fVirtualScreen.PutImage(currX, currY, fPieceBackground, dmSet);
      Inc(currX, stepX);
      Inc(currY, stepY);
      BGRAReplace(fPieceBackground, fVirtualScreen.GetPart(RectWithSize(currX, currY, gStyleData[gStyle].scale, gStyleData[gStyle].scale)));
      if promotion then
        if (currX = targX) and (currY = targY) then
          fPieces[index].kind := promotionKind;
      if vCurrentStyle = bsOriginal then
        oc := ocWhite
      else
        oc := ocTransparent;
      fVirtualScreen.PutImage(currX, currY, vPieceImage[fPieces[index].color, fPieces[index].kind, oc], dmDrawWithTransparency);
      if (currX = targX) and (currY = targY) then
      begin
        aAnimationEnd := TRUE;
        if Length(vMoveToBeHighlighted) > 0 then
        begin
          if vComputerCastlingFlag then
            vComputerCastlingFlag := FALSE
          else
          begin
            ScreenSave;
            if vKingIndex > 0 then
            begin
              HighlightMove(vMoveToBeHighlighted, vKingIndex);
              vKingIndex := 0;
            end else
              HighlightMove(vMoveToBeHighlighted, index);
            vMoveToBeHighlighted := '';
          end;
        end;
        ResetAnimatData;
      end;
    end;
end;

procedure TBGRAChessboard.SetAnimatData(const aIndex, aCurrentX, aCurrentY, aStepX, aStepY, aTargetX, aTargetY: integer; const aPromotion: boolean; const aPromotionKind: TChessPieceKind);
begin
  Assert(
    (Sign(aTargetX - aCurrentX) = Sign(aStepX))
    and ((aStepX = 0) or (Abs(aTargetX - aCurrentX) mod aStepX = 0))
    and (Sign(aTargetY - aCurrentY) = Sign(aStepY))
    and ((aStepY = 0) or (Abs(aTargetY - aCurrentY) mod aStepY = 0))
  );
  with fAnimationData do
  begin
    action := TRUE;
    index := aIndex;
    currX := aCurrentX;
    currY := aCurrentY;
    stepX := aStepX;
    stepY := aStepY;
    targX := aTargetX;
    targY := aTargetY;
    promotion := aPromotion;
    promotionKind := aPromotionKind;
  end;
end;

function TBGRAChessboard.FindPiece(const aX, aY: integer): integer;
var
  i: integer;
begin
  result := 0;
  i := 1;
  while (i <= 32) and (result = 0) do
  begin
    if (fPieces[i].x = aX)
    and (fPieces[i].y = aY) then
      result := i;
    Inc(i);
  end;
end;

function TBGRAChessboard.FindPiece(const aX, aY: integer; const aColor: TChessPieceColor): integer;
begin
  result := FindPiece(aX, aY);
  if (result <> 0) and (fPieces[result].color <> aColor) then
    result := 0;
end;

function TBGRAChessboard.FindPiece(const aX, aY: integer; const aColor: TChessPieceColor; const aType: TChessPieceKind): integer;
begin
  result := FindPiece(aX, aY, aColor);
  if (result <> 0) and (fPieces[result].kind <> aType) then
    result := 0;
end;

procedure TBGRAChessboard.ErasePiece(const aIndex, aX, aY: integer);
var
  vX, vY: integer;
  vEmptySquare: TBGRABitmap;
begin
  Assert(InRange(aIndex, 1, 32) and InRange(aX, 1, 8) and InRange(aY, 1, 8));
  vX := XToScreen(aX, fUpsideDown);
  vY := YToScreen(aY, fUpsideDown);
  with gStyleData[gStyle] do vEmptySquare := vChessboard.GetPart(RectWithSize(vX, vY, scale, scale)) as TBGRABitmap;
  fVirtualScreen.PutImage(
    vX,
    vY,
    vEmptySquare,
    dmSet
  );
  vEmptySquare.Free;
  SetPieceXY(aIndex, 0, 0);
end;

procedure TBGRAChessboard.ErasePiece(const aSquare: string);
var
  x, y, i: integer;
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.ErasePiece(%s)', [aSquare]));
{$ENDIF}
  DecodeSquare(aSquare, x, y);
  i := FindPiece(x, y);
  Assert(i > 0);
  ErasePiece(i, x, y);
end;

procedure TBGRAChessboard.MovePiece(const aIndex, aX, aY, aDX, aDY: integer; const aPromotion: boolean; const aPromotionKind: TChessPieceKind);
const
  STEP = 10;
var
  a, b: integer;
  vX1, vY1, vX2, vY2: integer;
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.MovePiece(%d, %d, %d, %d, %d, %s, %d)', [aIndex, aX, aY, aDX, aDY, BoolToStr(aPromotion, TRUE), Ord(aPromotionKind)]));
{$ENDIF}
  Assert(InRange(aIndex, 1, 32) and InRange(aX, 1, 8) and InRange(aY, 1, 8) and InRange(aDX, -8, 8) and InRange(aDY, -8, 8));

  a := 1;
  b := 1;

  if Abs(aDX) = 2 * Abs(aDY) then
    b := 2
  else
    if Abs(aDY) = 2 * Abs(aDX) then
      a := 2;

  vX1 := XToScreen(aX, fUpsideDown);
  vY1 := YToScreen(aY, fUpsideDown);

  BGRAReplace(
    fPieceBackground,
    vChessboard.GetPart(RectWithSize(vX1, vY1, gStyleData[gStyle].scale, gStyleData[gStyle].scale))
  );

  vX2 := XToScreen(aX + aDX, fUpsideDown);
  vY2 := YToScreen(aY + aDY, fUpsideDown);

  SetAnimatData(
    aIndex,
    vX1,
    vY1,
    (STEP div a) * Sign(aDX) * (2 * Ord(fUpsideDown) - 1) * -1,
    (STEP div b) * Sign(aDY) * (2 * Ord(fUpsideDown) - 1),
    vX2,
    vY2,
    aPromotion,
    aPromotionKind
  );

  SetPieceXY(aIndex, aX + aDX, aY + aDY);
end;

procedure TBGRAChessboard.MovePiece(const aMove: string; const aPromotion: boolean; const aPromotionKind: TChessPieceKind);
var
  x1, y1, x2, y2, i: integer;
begin
  DecodeMove(aMove, x1, y1, x2, y2);
  i := FindPiece(x1, y1);
  Assert(i > 0);
  MovePiece(i, x1, y1, x2 - x1, y2 - y1, aPromotion, aPromotionKind);
end;

procedure TBGRAChessboard.FlipBoard;
var
  vBoard, vSquare: TBGRABitmap;
  x, y: integer;
begin
  fUpsideDown := not fUpsideDown;

  vBoard := TBGRABitmap.Create(8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale);
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      vSquare := fVirtualScreen.GetPart(RectWithSize(
        XToScreen(x, not fUpsideDown),
        YToScreen(y, not fUpsideDown),
        gStyleData[gStyle].scale,
        gStyleData[gStyle].scale
      )) as TBGRABitmap;
      vBoard.PutImage(
        XToScreen(x, fUpsideDown),
        YToScreen(y, fUpsideDown),
        vSquare,
        dmSet
      );
      vSquare.Free;
    end;
  fVirtualScreen.PutImage(0, 0, vBoard, dmSet);
  vBoard.Free;
end;

procedure TBGRAChessboard.ChangeBoard(const aBoardStyle: TBoardStyle = bsOriginal);
var
  i: integer;
begin
  vChessboard.Free;
  vChessboard := CreateChessboard(aBoardStyle);
  EraseBoard();
  for i := Low(fPieces) to High(fPieces) do
    if fPieces[i].x > 0 then
      DrawPiece(i, fPieces[i].x, fPieces[i].y);
end;

procedure TBGRAChessboard.Highlight(const aX, aY: integer; const aColor: TOutlineColor; const aPieceIndex: integer);
var
  vSquare: TBGRABitmap;
  vColor: TBGRAPixel;
  oc: TOutlineColor;
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.Highlight(%d, %d, %s, %d)', [
    aX,
    aY,
    GetEnumName(TypeInfo(TOutlineColor), Ord(aColor)),
    aPieceIndex
  ]));
{$ENDIF}
  vColor :=  vSpecialColors[aColor];

  case vCurrentStyle of
    bsOriginal:
      begin
        with gStyleData[gStyle] do vSquare := TBGRABitmap.Create(scale, scale, vColor);
        if (aX + aY) mod 2 = 0 then
          vSquare.PutImage(
            0,
            0,
            vDarkSquare,
            dmDrawWithTransparency
          );
      end;
    else
      with gStyleData[gStyle] do
      begin
        vSquare := vChessboard.GetPart(RectWithSize(XToScreen(aX, FALSE), YToScreen(aY, FALSE), scale, scale)) as TBGRABitmap;
        vSquare.FillRect(0, 0, scale, scale, vColor, dmDrawWithTransparency);
      end;
  end;

  if vCurrentStyle = bsOriginal then
    oc := aColor
  else
    oc := ocTransparent;

  if aPieceIndex > 0 then
    vSquare.PutImage(
      0,
      0,
      vPieceImage[
        fPieces[aPieceIndex].color,
        fPieces[aPieceIndex].kind,
        oc
      ],
      dmDrawWithTransparency
    );
  fVirtualScreen.PutImage(XToScreen(aX, fUpsideDown), YToScreen(aY, fUpsideDown), vSquare, dmSet);
  vSquare.Free;
end;

procedure TBGRAChessboard.HighlightMove(const aMove: string; const aPieceIndex: integer);
var
  x1, y1, x2, y2: integer;
begin
{$IFDEF DEBUG}
  WriteLn(Format('TBGRAChessboard.HighlightMove(%s, %d)', [aMove, aPieceIndex]));
{$ENDIF}
  DecodeMove(aMove, x1, y1, x2, y2);
  HighLight(x1, y1, ocGreen, 0);
  HighLight(x2, y2, ocGreen, aPieceIndex);
end;

procedure TBGRAChessboard.ScreenSave;
begin
{$IFDEF DEBUG}
  WriteLn('TBGRAChessboard.ScreenSave');
{$ENDIF}
  if fScreenshot = nil then with gStyleData[gStyle] do
    fScreenshot := fVirtualScreen.GetPart(RectWithSize(0, 0, 8 * scale, 8 * scale)) as TBGRABitmap;
end;

procedure TBGRAChessboard.ScreenRestore;
begin
{$IFDEF DEBUG}
  WriteLn('TBGRAChessboard.ScreenRestore');
{$ENDIF}
  if Assigned(fScreenshot) then
  begin
    fVirtualScreen.PutImage(0, 0, fScreenshot, dmSet);
    fScreenshot.Free;
    fScreenshot := nil;
  end;
end;

procedure TBGRAChessboard.SavePieceBackground(const aImagePos: TPoint; const aCreateFromChessboard: boolean = FALSE);
var
  source: TBGRABitmap;
begin
  if aCreateFromChessboard then
    source := vChessboard
  else
    source := fVirtualScreen;
  BGRAReplace(
    fPieceBackground,
    source.GetPart(RectWithSize(aImagePos.X, aImagePos.Y, gStyleData[gStyle].scale, gStyleData[gStyle].scale))
  );
end;

procedure TBGRAChessboard.RestorePieceBackground(const aImagePos: TPoint);
begin
  fVirtualScreen.PutImage(aImagePos.X, aImagePos.Y, fPieceBackground, dmSet);
end;

procedure TBGRAChessboard.DrawPiece(const aImagePos: TPoint; const aPieceIndex: integer);
var
  oc: TOutlineColor;
begin
  if vCurrentStyle = bsOriginal then
    oc := ocWhite
  else
    oc := ocTransparent;
  fVirtualScreen.PutImage(
    aImagePos.X,
    aImagePos.Y,
    vPieceImage[
      fPieces[aPieceIndex].color,
      fPieces[aPieceIndex].kind,
      oc
    ],
    dmDrawWithTransparency
  );
end;

function TBGRAChessboard.XYToScreen(const aX, aY: integer): TPoint;
begin
  result.SetLocation(XToScreen(aX, fUpsideDown), YToScreen(aY, fUpsideDown));
end;

function TBGRAChessboard.ScreenSaved(): boolean;
begin
  result := Assigned(fScreenshot);
end;

procedure TBGRAChessboard.ScreenToXY(const aMousePos: TPoint; out aX, aY: integer);
begin
  aX := AMousePos.X div gStyleData[gStyle].scale + 1;
  aY := 8 - AMousePos.Y div gStyleData[gStyle].scale;
  if isUpsideDown then
  begin
    aX := 9 - aX;
    aY := 9 - aY;
  end;
end;

initialization
  vMoveToBeHighlighted := '';
  vComputerCastlingFlag := FALSE;
  vKingIndex := 0;

end.
