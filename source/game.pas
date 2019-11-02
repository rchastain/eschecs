
{**
@abstract(Arbitrage d'une partie d'échecs.)
@longcode(
uses
  Game;

begin
  with TChessGame.Create do
  begin
    if IsLegal('e2e4') then
      DoMove('e2e4');
    Free;
  end;
end.
)
}
unit Game;

interface

uses
  SysUtils, Classes, ChessTypes, Position, ChessUtils, Fen;

type
  TChessGame = class
    private
      FCurrent, FNext: TChessPosition;
      FList, FHistory: TStringList;
      FState: TChessState;
    public
      constructor Create(const AFenRecord: string = CFenStartPosition);
      destructor Destroy; override;
      procedure Think;
      function IsLegal(const AMove: string): boolean;
      function ActiveColor: TPieceColorStrict;
      function Check: boolean;
      procedure GetKingCheckedXY(out AX, AY: integer);
      function GetFen(const AFrc: boolean = TRUE): string;
      procedure DoMove(const AMove: string);
      function IsPromotion(const AMove: string): boolean;
      function IsCastling(const AMove: string): boolean;
      function IsEnPassant(const AMove: string): string;
      function Repetition: boolean;
      function CurrPosToStr(): string;
      function GetSan(const AMove: string): string;
      property State: TChessState read FState;
  end;

implementation

constructor TChessGame.Create(const AFenRecord: string);
begin
  inherited Create;
  if FCurrent = nil then FCurrent := TChessPosition.Create(AFenRecord) else FCurrent.Create(AFenRecord);
  if FNext = nil then FNext := TChessPosition.Create else FNext.Create;
  if FList = nil then FList := TStringList.Create else FList.Clear;
  FList.Sorted := TRUE;
  if FHistory = nil then FHistory := TStringList.Create else FHistory.Clear;
  FHistory.Append(AFenRecord);
  Think;
end;

destructor TChessGame.Destroy;
begin
  FCurrent.Free;
  FNext.Free;
  FList.Free;
  FHistory.Free;
  inherited Destroy;
end;

procedure TChessGame.Think;
var
  i: integer;
  s: string;
begin
  FCurrent.SetVariables(FALSE);
  FCurrent.GenerateMoves1(FCurrent.active);
  FCurrent.GenerateMoves2(FCurrent.active);
  
  FList.Clear;
  s := FCurrent.FenRecord;
  for i := 0 to FCurrent.List.Count - 1 do
  begin
    //WriteLn(FCurrent.List[i]);
    FNext.Create(s);
    FNext.DoMove(FCurrent.List[i]);
    FNext.Active := OtherColor(FNext.Active);
    FNext.SetVariables(TRUE);
    if not FNext.Check then
      FList.Add(FCurrent.List[i]);
  end;
  
  if FList.Count = 0 then
  begin
    if FCurrent.Check then
      FState := csCheckmate
    else
      FState := csStalemate;
  end else
    if (FCurrent.Data.FHalfMoves = 50) or Repetition then
      FState := csDraw
    else
      FState := csProgress;
end;

function TChessGame.IsLegal(const AMove: string): boolean;
var
  i: integer;
begin
  result := FList.Find(Copy(AMove, 1, 4), i);
end;

function TChessGame.ActiveColor: TPieceColorStrict;
begin
  result := FCurrent.Active;
end;

function TChessGame.Check: boolean;
begin
  result := FCurrent.Check;
end;

procedure TChessGame.GetKingCheckedXY(out AX, AY: integer);
begin
  AX := FCurrent.KingCheckedX;
  AY := FCurrent.KingCheckedY;
end;

function TChessGame.GetFen(const AFrc: boolean): string;
begin
  result := FCurrent.FenRecord(AFrc);
end;

procedure TChessGame.DoMove(const AMove: string);
var
  LType: TPieceTypeStrict;
begin
  if Length(AMove) = 5 then
    case AMove[5] of
      'n': LType := ptKnight;
      'b': LType := ptBishop;
      'r': LType := ptRook;
      else
        LType := ptQueen;
    end
  else
    LType := ptQueen;
  FCurrent.DoMove(AMove, LType);
  FHistory.Append(GetFen);
  Think;
end;

function TChessGame.IsPromotion(const AMove: string): boolean;
var
  x1, y1, x2, y2: integer;
begin
  StrToMove(AMove, x1, y1, x2, y2);
  result := (FCurrent.Data.FBoard[x1, y1].FType = ptPawn) and (y2 in [1, 8]);
end;

function TChessGame.IsCastling(const AMove: string): boolean;
var
  x1, y1, x2, y2: integer;
begin
  StrToMove(AMove, x1, y1, x2, y2);
  result := (FCurrent.Data.FBoard[x1, y1].FType = ptKing)
    and (FCurrent.Data.FBoard[x2, y2].FType = ptRook)
    and (FCurrent.Data.FBoard[x1, y1].FColor = FCurrent.Data.FBoard[x2, y2].FColor)
end;

function TChessGame.IsEnPassant(const AMove: string): string;
var
  x1, y1, x2, y2: integer;
begin
  result := '';
  StrToMove(AMove, x1, y1, x2, y2);
  if (FCurrent.data.FBoard[x1, y1].FType = ptPawn)
  and (FCurrent.data.FBoard[x2, y2].FType = ptNil)
  and (x2 <> x1) then
    result := SquareToStr(x2, y1);
end;

function TChessGame.Repetition: boolean;
  function SamePiecePlacement(const AFenRecord1, AFenRecord2: string): boolean;
  var
    l: integer;
  begin
    l := Pos(' ', AFenRecord1) - 1;
    result := StrLComp(pchar(AFenRecord1), pchar(AFenRecord2), l) = 0;
  end;
var
  i, n: integer;
begin
  result := FALSE;
  i := FHistory.Count - 2;
  n := 1;
  while (i >= 0) and not result do
  begin
    if SamePiecePlacement(FHistory[i], FHistory[FHistory.Count - 1]) then
    begin
      Inc(n);
      if n = 3 then
        result := TRUE;
    end;
    Dec(i);
  end;
end;

function TChessGame.CurrPosToStr(): string;
begin
  result := FCurrent.ToStr();
end;

function TChessGame.GetSan(const AMove: string): string;
var
  LTargetSquare: string;
  x1, y1, x2, y2, x3, y3: integer;
  LType: TPieceTypeStrict;
  LPieceSymbol: string;
  LExtra: string;
  LLevel: integer;
  LExtraOk: boolean;
  LCapture: string;
  LPromo: string;
  i: integer;
begin
  if not IsLegal(AMove) then
    Exit('');
  StrToMove(AMove, x1, y1, x2, y2);
  if IsCastling(AMove) then
    if x2 > x1 then
      Exit('O-O')
    else
      Exit('O-O-O');
  LType := FCurrent.Data.FBoard[x1, y1].FType;
  case LType of
    ptKnight..ptKing: LPieceSymbol := CPieceSymbol[pcWhite, LType];
    else LPieceSymbol := '';
  end;
  LTargetSquare := Copy(AMove, 3, 2);
  LExtra := '';
  LLevel := 0;
  LExtraOk := FALSE;
  while not LExtraOk do
  begin
    LExtraOk := TRUE;
    for i := 0 to FList.Count - 1 do
      if (Copy(FList[i], 3, 2) = LTargetSquare)
      and (Copy(FList[i], 1, 2) <> Copy(AMove, 1, 2)) then
      begin
        StrToSquare(Copy(FList[i], 1, 2), x3, y3);
        if (FCurrent.Data.FBoard[x3, y3].FType <> FCurrent.Data.FBoard[x1, y1].FType) then
          Continue;
        if (LLevel = 1) and (x3 <> x1) then
          Continue;
        if (LLevel = 2) and (y3 <> y1) then
          Continue;
        case LLevel of
          0: begin LLevel := 1; LExtra := Copy(AMove, 1, 1); LExtraOk := FALSE; end;
          1: begin LLevel := 2; LExtra := Copy(AMove, 2, 1); LExtraOk := FALSE; end;
          2: begin LExtra := Copy(AMove, 1, 2); Break; end;
        end;
      end;
  end;
  if (FCurrent.Data.FBoard[x2, y2].FType <> ptNil)
  and (FCurrent.Data.FBoard[x2, y2].FColor <> FCurrent.Data.FBoard[x1, y1].FColor) then
    LCapture := 'x'
  else
    LCapture := '';
  if Length(AMove) = 5 then
    LPromo := '=' + UpCase(Copy(AMove, 5, 1))
  else
    LPromo := '';
  result := Concat(LPieceSymbol, LExtra, LCapture, LTargetSquare, LPromo);
end;

end.
