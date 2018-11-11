
{**
@abstract(Lecture et écriture des chaînes au format @html(<a href="http://kirill-kryukov.com/chess/doc/fen.html">FEN</a>).)
}
unit Fen;

interface

uses
  chesstypes;

const
  FENSTARTPOSITION = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
  {** @exclude }
  FENEXAMPLES: array[1..5] of string = (
    'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1',
    'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1',
    'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2',
    'rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2',
    '4k3/8/8/8/8/8/4P3/4K3 w - - 5 39'
  );

function EncodeChessPositionData(const aFENRecord: string = FENSTARTPOSITION): TChessPositionData;
function DecodeChessPositionData(const aPositionData: TChessPositionData): string;

implementation

uses
  SysUtils,
  Classes;

const
  SYMBOLS: array[TChessPieceColor] of char = ('w', 'b');
  TABLE = 'PNBRQK';

function EncodeChessPositionData(const aFENRecord: string = FENSTARTPOSITION): TChessPositionData;
var
  x, y, i: integer;
  c: char;
begin
  with TStringList.Create do
  begin
    DelimitedText := aFENRecord;
    Assert(Count = 6);
    
    with result do
    begin
      x := 1;
      y := 8;
      i := 1;
      
      while i <= Length(Strings[0]) do
      begin
        c := Strings[0][i];
        case c of
          '/':
            begin
              x := 1;
              Dec(y);
            end;
          '1'..'8':
            while c > '0' do
            begin
              board[x, y].color := cpcNil;
              board[x, y].kind := cpkNil;
              Inc(x);
              Dec(c);
            end;
        else
          begin
            board[x, y].color := TChessPieceColor(Ord(c in ['a'..'z']));
            board[x, y].kind := TChessPieceKind(Pred(Pos(UpCase(c), TABLE)));
            Inc(x);
          end;
        end;
        Inc(i);
      end;
      
      activeColor := TChessPieceColor(Ord(Strings[1] = SYMBOLS[cpcBlack]));
      
      castling := Strings[2];
      enPassant := Strings[3];
      
      halfMoves := StrToInt(Strings[4]);
      fullMove := StrToInt(Strings[5]);
    end;
    
    Free;
  end;
end;

function DecodeChessPositionData(const aPositionData: TChessPositionData): string;
var
  x, y, n: integer;
begin
  with aPositionData do
  begin
    result := '';
    
    x := 1;
    y := 8;
    while y >= 1 do
    begin
      if board[x, y].kind = cpkNil then
      begin
        n := 0;
        while (x + n <= 8) and (board[x + n, y].kind = cpkNil) do
          Inc(n);
        result := Concat(result, IntToStr(n));
        Inc(x, n);
      end else
      begin
        result := Concat(result, SYMBOLS2[board[x, y].color, board[x, y].kind]);
        Inc(x);
      end;
      if x > 8 then
      begin
        if y > 1 then
          result := Concat(result, '/');
        x := 1;
        Dec(y);
      end;
    end;
    
    result := Format(
      '%s %s %s %s %d %d',
      [
        result,
        SYMBOLS[activeColor],
        castling,
        enPassant,
        halfMoves,
        fullMove
      ]
    );
  end;
end;

end.
