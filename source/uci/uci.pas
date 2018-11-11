 
unit uci;

interface

function MsgUCI(): string;
function MsgNewGame(): string;
function MsgIsReady(): string;
function MsgPosition(const aFENPosition: string): string;
function MsgGo(const aMoveTime: integer): string;
function MsgQuit(): string;

function IsMsgUciOk(const aMsg: string; out aEngineName, aAuthor: string): boolean;
function IsMsgBestMove(const aMsg: string; out aBestMove, aPromotion: string): boolean;
function IsMsgReadyOk(const aMsg: string): boolean;

implementation

uses
  SysUtils,
  RegExpr;

type
  TUCIMessage = (
    ucUCI,
    ucNewGame,
    ucIsReady,
    ucPosition,
    ucGo,
    ucQuit
  );

const
  PATTERNS: array[TUCIMessage] of string = (
    'uci',
    'ucinewgame',
    'isready',
    'position fen %s',
    'go movetime %d',
    'quit'
  );
  
function MsgUCI(): string;
begin
  result := PATTERNS[ucUCI];
end;

function MsgNewGame(): string;
begin
  result := PATTERNS[ucNewGame];
end;

function MsgIsReady(): string;
begin
  result := PATTERNS[ucIsReady];
end;

function MsgPosition(const aFENPosition: string): string;
begin
  result := Format(PATTERNS[ucPosition], [aFENPosition]);
end;

function MsgGo(const aMoveTime: integer): string;
begin
  result := Format(PATTERNS[ucGo], [aMoveTime]);
end;

function MsgQuit(): string;
begin
  result := PATTERNS[ucQuit];
end;

function IsMsgUciOk(const aMsg: string; out aEngineName, aAuthor: string): boolean;
begin
    with TRegExpr.Create('id name ([^\r\n]+).+id author ([^\r\n]+).+uciok') do
    begin
      result := Exec(aMsg);
      if result then
      begin
        aEngineName := Trim(Match[1]);
        aAuthor := Trim(Match[2]);
      end else
      begin
        aEngineName := '';
        aAuthor := '';
      end;
      Free;
    end;
end;

function IsMsgBestMove(const aMsg: string; out aBestMove, aPromotion: string): boolean;
const
  PROMO_SYMBOLS: set of char = ['n', 'b', 'r', 'q'];
var
  vPos: integer;
  vAux: string;
begin
  vPos := Pos('bestmove', aMsg);
  result := vPos > 0;
  if result then
  begin
    aBestMove := Copy(aMsg, vPos + Length('bestmove '), 4);
    vAux := Copy(aMsg, vPos + Length('bestmove a7a8'), 1);
    if (Length(vAux) = 1) and (vAux[1] in PROMO_SYMBOLS) then
      aPromotion := vAux
    else
      aPromotion := '';
  end else
  begin
    aBestMove := '';
    aPromotion := '';
  end;
end;

function IsMsgReadyOk(const aMsg: string): boolean;
begin
  result := Pos('readyok', aMsg) = 1;
end;

end.
