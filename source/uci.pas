 
unit Uci;

interface

function MsgUci: string;
function MsgSetOption(const AName: string; const AValue: boolean): string;
function MsgNewGame: string;
function MsgIsReady: string;
function MsgPosition(const AFenPosition: string): string;
function MsgGo(const AMoveTime: integer): string;
function MsgStop: string;
function MsgQuit: string;

function IsMsgUciOk(const AMsg: string; out AEngineName, AAuthor: string; out AOptChess960: boolean): boolean;
function IsMsgBestMove(const AMsg: string; out ABestMove, APromotion: string): boolean;
function IsMsgReadyOk(const AMsg: string): boolean;

implementation

uses
  SysUtils, RegExpr;

type
  TUciMessage = (
    ucUci,
    ucSetOption,
    ucNewGame,
    ucIsReady,
    ucPosition,
    ucGo,
    ucStop,
    ucQuit
  );

const
  CPatterns: array[TUciMessage] of string = (
    'uci',
    'setoption name %s value %s',
    'ucinewgame',
    'isready',
    'position fen %s',
    'go movetime %d',
    'stop',
    'quit'
  );
  
function MsgUci: string;
begin
  result := CPatterns[ucUci];
end;

function MsgSetOption(const AName: string; const AValue: boolean): string;
begin
  result := Format(CPatterns[ucSetOption], [AName, LowerCase(BoolToStr(AValue, TRUE))]);
end;

function MsgNewGame: string;
begin
  result := CPatterns[ucNewGame];
end;

function MsgIsReady: string;
begin
  result := CPatterns[ucIsReady];
end;

function MsgPosition(const AFenPosition: string): string;
begin
  result := Format(CPatterns[ucPosition], [AFenPosition]);
end;

function MsgGo(const AMoveTime: integer): string;
begin
  result := Format(CPatterns[ucGo], [AMoveTime]);
end;

function MsgStop: string;
begin
  result := CPatterns[ucStop];
end;

function MsgQuit: string;
begin
  result := CPatterns[ucQuit];
end;

function IsMsgUciOk(
  const AMsg: string;
  out AEngineName, AAuthor: string;
  out AOptChess960: boolean
): boolean;
begin
  with TRegExpr.Create('id name ([^\r\n]+).+id author ([^\r\n]+).+uciok') do
  begin
    result := Exec(AMsg);
    if result then
    begin
      AEngineName := Trim(Match[1]);
      AAuthor := Trim(Match[2]);
    end else
    begin
      AEngineName := '';
      AAuthor := '';
    end;
    Free;
  end;
  AOptChess960 := Pos('option name UCI_Chess960 type check default ', AMsg) > 0;
end;

function IsMsgBestMove(const AMsg: string; out ABestMove, APromotion: string): boolean;
const
  CPromoSymbols: set of char = ['n', 'b', 'r', 'q'];
var
  LPos: integer;
  LAux: string;
begin
  LPos := Pos('bestmove', AMsg);
  result := LPos > 0;
  if result then
  begin
    ABestMove := Copy(AMsg, LPos + Length('bestmove '), 4);
    LAux := Copy(AMsg, LPos + Length('bestmove a7a8'), 1);
    if (Length(LAux) = 1) and (LAux[1] in CPromoSymbols) then
      APromotion := LAux
    else
      APromotion := '';
  end else
  begin
    ABestMove := '';
    APromotion := '';
  end;
end;

function IsMsgReadyOk(const AMsg: string): boolean;
begin
  result := Pos('readyok', AMsg) = 1;
end;

end.
