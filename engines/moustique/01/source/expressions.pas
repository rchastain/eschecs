
unit Expressions;

interface

uses
  Classes, SysUtils;

function ExtractFEN(const aCommand: string; var aFEN: string): boolean;
function ExtractMoves(const aCommand: string): boolean;

var
  list: TStringList;
  
implementation

uses
  RegExpr, Log;

const
  PATTERN_PIECES = '[1-8BKNPQRbknpqr]+';
  PATTERN_ACTIVECOLOR = '[wb]';
  PATTERN_CASTLING = '([KQkq]+|-)';
  PATTERN_ENPASSANT = '([a-h][1-8]|-)';
  PATTERN_NUMBER = '\d+';
  PATTERN_MOVE = '\b[a-h][1-8][a-h][1-8][nbrq]?\b';
  
var
  fenPattern: string;
  efen, emove: TRegExpr;

function ExtractFEN(const aCommand: string; var aFEN: string): boolean;
begin
  result := efen.Exec(aCommand);
  if result then
    aFEN := efen.Match[0];
end;

function ExtractMoves(const aCommand: string): boolean;
begin
  list.Clear;
  result := emove.Exec(aCommand);
  if result then 
    repeat 
      list.Append(emove.Match[0]);
    until not emove.ExecNext;
end;

initialization
  fenPattern := Format('%s %s %s %s %s %s', [
    ReplaceRegExpr('x', 'x/x/x/x/x/x/x/x', PATTERN_PIECES, false),
    PATTERN_ACTIVECOLOR,
    PATTERN_CASTLING,
    PATTERN_ENPASSANT,
    PATTERN_NUMBER,
    PATTERN_NUMBER
  ]);
  
  list := TStringList.Create;
  efen := TRegExpr.Create(fenPattern);
  emove := TRegExpr.Create(PATTERN_MOVE);
  
finalization
  list.Free;
  efen.Free;
  emove.Free;
  
end.
