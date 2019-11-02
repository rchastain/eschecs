
unit ChessTypes;

interface

type
  TPieceType = (ptPawn, ptKnight, ptBishop, ptRook, ptQueen, ptKing, ptNil);
  TPieceTypeStrict = ptPawn..ptKing;
  TPieceColor = (pcWhite, pcBlack, pcNil);
  TPieceColorStrict = pcWhite..pcBlack;
  
  TChessPiece = record
    FType: TPieceType;
    FColor: TPieceColor;
  end;
  
  TBoard = array[1..8, 1..8] of TChessPiece;
  TCastling = (caWH, caWA, caBH, caBA);
  TCastlingRights = array[TCastling] of integer;
  
  TPositionData = record
    FBoard: TBoard;
    FActive: TPieceColorStrict;
    FCastling: TCastlingRights;
    FEnPassant: string;
    FHalfMoves: integer;
    FFullMove: integer;
  end;
  
  TChessState = (csProgress, csCheckmate, csStalemate, csDraw);

const
  CPieceSymbol: array[TPieceColorStrict, TPieceTypeStrict] of char = (
    ('P', 'N', 'B', 'R', 'Q', 'K'),
    ('p', 'n', 'b', 'r', 'q', 'k')
  );
  CColorSymbol: array[TPieceColorStrict] of char = ('w', 'b');

function OtherColor(const AColor: TPieceColor): TPieceColor;
function ValidPromotionValue(const AValue: TPieceType): TPieceTypeStrict;
function DataToStr(const AData: TPositionData): string;

implementation

uses
  SysUtils;
  
function OtherColor(const AColor: TPieceColor): TPieceColor;
begin
  if AColor = pcNil then
    result := AColor
  else
    result := TPieceColorStrict(1 - Ord(AColor));
end;

function ValidPromotionValue(const AValue: TPieceType): TPieceTypeStrict;
begin
  if AValue in [ptKnight, ptBishop, ptRook, ptQueen] then
    result := TPieceTypeStrict(AValue)
  else
    result := ptQueen;
end;

function DataToStr(const AData: TPositionData): string;
var
  x, y: integer;
begin
  result :=  '+   a b c d e f g h   +'#13#10#10;
  for y := 8 downto 1 do
  begin
    result := result + Chr(y + Ord('0')) + '   ';
    for x := 1 to 8 do
    begin
      if AData.FBoard[x, y].FColor = pcNil then
        if (x + y) mod 2 = 1 then
          result := result + '. '
        else
          result := result + ': '
      else
        result := result + CPieceSymbol[AData.FBoard[x, y].FColor, AData.FBoard[x, y].FType] + ' ';
    end;
    result := result + '  ' + Chr(y + Ord('0')) + #13#10;
  end;
  result := result + #10'+   a b c d e f g h   +'#13#10#10;
  with AData do
    result := result + Format(
      '  * Active color: %s'#13#10 +
      '  * Castling rights: %d %d %d %d'#13#10 +
      '  * En passant: %s'#13#10 +
      '  * Halfmoves clock: %d'#13#10 +
      '  * Fullmove number: %d'#13#10,
      [
        CColorSymbol[FActive],
        FCastling[caWH],
        FCastling[caWA],
        FCastling[caBH],
        FCastling[caBA],
        FEnPassant,
        FHalfMoves,
        FFullMove
      ]
    );
end;

end.
