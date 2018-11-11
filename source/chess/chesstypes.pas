
unit ChessTypes;

interface

type
  TChessPieceKindEx = (cpkPawn, cpkKnight, cpkBishop, cpkRook, cpkQueen, cpkKing, cpkNil);
  TChessPieceKind = cpkPawn..cpkKing;
  TChessPieceColorEx = (cpcWhite, cpcBlack, cpcNil);
  TChessPieceColor = cpcWhite..cpcBlack;
  TChessState = (csProgress, csCheckmate, csStalemate, csDraw);
  TChessPiece = record
    kind: TChessPieceKindEx;
    color: TChessPieceColorEx;
  end;
  TBoard = array[1..8, 1..8] of TChessPiece;
  TChessPositionData = record
    board: TBoard;
    activeColor: TChessPieceColor;
    castling: string;
    enPassant: string;
    halfMoves: integer;
    fullMove: integer;
  end;

function OtherColor(const aColor: TChessPieceColorEx): TChessPieceColorEx;
function ValidPromotionValue(const aValue: TChessPieceKindEx): TChessPieceKind;

const
  SYMBOLS2: array[TChessPieceColor, TChessPieceKind] of char = (
    ('P', 'N', 'B', 'R', 'Q', 'K'),
    ('p', 'n', 'b', 'r', 'q', 'k')
  );
  
implementation

function OtherColor(const aColor: TChessPieceColorEx): TChessPieceColorEx;
begin
  if aColor = cpcNil then
    result := aColor
  else
    result := TChessPieceColor(1 - Ord(aColor));
end;

function ValidPromotionValue(const aValue: TChessPieceKindEx): TChessPieceKind;
begin
  if aValue in [cpkKnight, cpkBishop, cpkRook, cpkQueen] then
    result := TChessPieceKind(aValue)
  else
    result := cpkQueen;
end;

end.
