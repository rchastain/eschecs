
{**
@abstract(Arbitrage d'une partie d'échecs.)
@longcode(
uses
  chessgame;

begin
  with TChessGame.Create do
  begin
    if IsLegal('e2e4') then
      PlayMove('e2e4');
    
    WriteLn(IsLegal('e7e5')); // true
    
    Free;
  end;
end.
)
}
unit chessgame;

interface

uses
  SysUtils,
  Classes,
  
  ChessTypes,
  ChessPosition,
  chessutils,
  FEN;

type
  TChessGame = class
    private
      fCurrent, fNext: TChessPosition;
      fList, fHistory: TStringList;
      fState: TChessState;
    public
      constructor Create(const aFENRecord: string = FENSTARTPOSITION);
      destructor Destroy; override;
      procedure Think;
      function IsLegal(const aMove: string): boolean;
      function ActiveColor: TChessPieceColor;
      function Check: boolean;
      procedure GetKingCheckedXY(out aX, aY: integer);
      function FENRecord: string;
      procedure PlayMove(const aMove: string);
      function IsPromotion(const aMove: string): boolean;
      function IsCastling(const aMove: string): string;
      function IsEnPassant(const aMove: string): string;
      function Repetition: boolean;
      property state: TChessState read fState;
  end;

implementation

constructor TChessGame.Create(const aFENRecord: string);
begin
  inherited Create;
  if fCurrent = nil then fCurrent := TChessPosition.Create(aFENRecord) else fCurrent.Create(aFENRecord);
  if fNext = nil then fNext := TChessPosition.Create else fNext.Create;
  if fList = nil then fList := TStringList.Create else fList.Clear;
  fList.Sorted := true;
  if fHistory = nil then fHistory := TStringList.Create else fHistory.Clear;
  fHistory.Append(aFENRecord);
  Think;
end;

destructor TChessGame.Destroy;
begin
  fCurrent.Free;
  fNext.Free;
  fList.Free;
  fHistory.Free;
  inherited Destroy;
end;

procedure TChessGame.Think;
var
  i: integer;
  s: string;
begin
  fCurrent.SetVariables;
  fCurrent.GenerateMoves1(fCurrent.activeColor);
  fCurrent.GenerateMoves2(fCurrent.activeColor);
  
  fList.Clear;
  s := fCurrent.FENRecord;
  for i := 0 to fCurrent.list.Count - 1 do
  begin
    fNext.Create(s);
    fNext.PlayMove(fCurrent.list[i]);
    fNext.activeColor := OtherColor(fNext.activeColor);
    fNext.SetVariables;
    if not fNext.check then
      fList.Add(fCurrent.list[i]);
  end;
  
  if fList.Count = 0 then
  begin
    if fCurrent.check then
      fState := csCheckmate
    else
      fState := csStalemate;
  end else
    if (fCurrent.data.halfMoves = 50) or Repetition then
      fState := csDraw
    else
      fState := csProgress;
end;

function TChessGame.IsLegal(const aMove: string): boolean;
var
  i: integer;
begin
  result := fList.Find(Copy(aMove, 1, 4), i);
end;

function TChessGame.ActiveColor: TChessPieceColor;
begin
  result := fCurrent.activeColor;
end;

function TChessGame.Check: boolean;
begin
  result := fCurrent.check;
end;

procedure TChessGame.GetKingCheckedXY(out aX, aY: integer);
begin
  aX := fCurrent.kingCheckedX;
  aY := fCurrent.kingCheckedY;
end;

function TChessGame.FENRecord: string;
begin
  result := fCurrent.FENRecord;
end;

procedure TChessGame.PlayMove(const aMove: string);
var
  vPromotion: TChessPieceKind;
begin
  if Length(aMove) = 5 then
    case aMove[5] of
      'n': vPromotion := cpkKnight;
      'b': vPromotion := cpkBishop;
      'r': vPromotion := cpkRook;
      else
        vPromotion := cpkQueen;
    end
  else
    vPromotion := cpkQueen;
  fCurrent.PlayMove(aMove, vPromotion);
  fHistory.Append(FENRecord);
  Think;
end;

function TChessGame.IsPromotion(const aMove: string): boolean;
var
  x1, y1, x2, y2: integer;
begin
  StrToMove(aMove, x1, y1, x2, y2);
  result := (fCurrent.data.board[x1, y1].kind = cpkPawn) and (y2 in [1, 8]);
end;

function TChessGame.IsCastling(const aMove: string): string;
var
  x1, y1, x2, y2: integer;
begin
  result := '';
  StrToMove(aMove, x1, y1, x2, y2);
  if (fCurrent.data.board[x1, y1].kind = cpkKing) and (Abs(x2 - x1) = 2) then
    case 10 * x2 + y2 of
      71: result := 'h1f1';
      31: result := 'a1d1';
      78: result := 'h8f8';
      38: result := 'a8d8';
    end;
end;

function TChessGame.IsEnPassant(const aMove: string): string;
var
  x1, y1, x2, y2: integer;
begin
  result := '';
  StrToMove(aMove, x1, y1, x2, y2);
  if (fCurrent.data.board[x1, y1].kind = cpkPawn)
  and (fCurrent.data.board[x2, y2].kind = cpkNil)
  and (x2 <> x1) then
    result := SquareToStr(x2, y1);
end;

function TChessGame.Repetition: boolean;
  function SamePiecePlacement(const aFENRecord1, aFENRecord2: string): boolean;
  var
    l: integer;
  begin
    l := Pos(' ', aFENRecord1) - 1;
    result := StrLComp(pchar(aFENRecord1), pchar(aFENRecord2), l) = 0;
  end;
var
  i, n: integer;
begin
  result := false;
  i := fHistory.Count - 2;
  n := 1;
  while (i >= 0) and not result do
  begin
    if SamePiecePlacement(fHistory[i], fHistory[fHistory.Count - 1]) then
    begin
      Inc(n);
      if n = 3 then
        result := true;
    end;
    Dec(i);
  end;
end;

end.
