
uses
  SysUtils, Classes, Expressions;

var
  list: TStringList;

const
  S: array[1..3] of string = (
    'position startpos moves e2e4',
    'position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4',
    'position fen rnbqkbnr/4a1a1/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4'
  );
  
begin
  list := Expressions.list;
  if ExtractMoves(S[3]) then
    WriteLn(list.Text);
end.
