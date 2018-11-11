
unit PGN;

interface

uses
  SysUtils, Classes;

type
  TGroupIndex = 1..6;
  PGroups = ^TGroups;
  TGroups = array[TGroupIndex] of string;
  
  TChessGame = class
    tags: TStringList;
    moves: TList;
    termination: string;
    constructor Create; overload;
    destructor Destroy; override;
  end;
  
function ParsePGNText(aText: string): TList;

implementation

uses
  RegExpr;

constructor TChessGame.Create;
begin
  inherited;
  tags := TStringList.Create;
  tags.Sorted := TRUE;
  moves := TList.Create;
end;

destructor TChessGame.Destroy;
var
  vIndex: integer;
  vGroups: PGroups;
begin
  tags.Free;
  for vIndex := Pred(moves.Count) downto 0 do
  begin
    vGroups := moves[vIndex];
    Dispose(vGroups);
  end;
  moves.Free;
  inherited;
end;

function ParsePGNText(aText: string): TList;
const
  COLUMN = '[a-h]';
  ROW = '[1-8]';
  KINGSIDE_CASTLING = 'O-O';
  QUEENSIDE_CASTLING = 'O-O-O';
  PIECE = '[PNBRQK]';
  DISAMBIGUATION = COLUMN + '|' + ROW + '|' + COLUMN + ROW;
  CAPTURE = 'x';
  SQUARE_OR_CASTLING = COLUMN + ROW + '|' + QUEENSIDE_CASTLING + '|' + KINGSIDE_CASTLING;
  PROMOTION = '=[NBRQ]';
  CHECK_OR_CHECKMATE = '[+#]';
  SAN_MOVE =
    '(' + PIECE + ')?' +
    '(' + DISAMBIGUATION + ')?' +
    '(' + CAPTURE + ')?' +
    '(' + SQUARE_OR_CASTLING + ')' +
    '(' + PROMOTION + ')?' +
    '(' + CHECK_OR_CHECKMATE + ')?';
  FULL_MOVE = '(\d+)\.\s+(' + SAN_MOVE + '\s+){1,2}';
  MOVES_SECTION = '(' + FULL_MOVE + ')+';
  TAG_PAIR = '\[(\w+)\s+"([^"]+)"\]';
  TAG_PAIRS_SECTION  = '(' + TAG_PAIR + '\s+)+';
  GAME_TERMINATION = '(1-0|0-1|1/2-1/2|\*)';
  GAME = TAG_PAIRS_SECTION + MOVES_SECTION + GAME_TERMINATION;
type
  TSearch = (searchGame, searchTagPair, searchFullMove, searchTermination, searchMove);
const
  PATTERNS: array[TSearch] of string = (GAME, TAG_PAIR, FULL_MOVE, GAME_TERMINATION, SAN_MOVE);
var
  vExpr: array[TSearch] of TRegExpr;
  vSearch: TSearch;
  vGame: TChessGame;
procedure ParseFullMove(const aFullMove: string);
var
  vIndex: integer;
  vGroups: PGroups;
begin
  with vExpr[searchMove] do
    if Exec(aFullMove) then
      with vGame do
      repeat
        New(vGroups);
        moves.Add(vGroups);
        for vIndex := 1 to SubExprMatchCount do
          vGroups[vIndex] := Match[vIndex];
      until not ExecNext;
end;
begin
  for vSearch in TSearch do
    vExpr[vSearch] := TRegExpr.Create(PATTERNS[vSearch]);
  
  result := TList.Create;
  
  try
    if vExpr[searchGame].Exec(aText) then
      repeat
        aText := vExpr[searchGame].Match[0];
        
        vGame := TChessGame.Create;
        result.Add(vGame);
        
        with vExpr[searchTagPair] do
          if Exec(aText) then
          repeat
            vGame.tags.Append(Format('%s=%s', [Match[1], Match[2]]));
          until not ExecNext;
        
        with vExpr[searchFullMove] do
          if Exec(aText) then
          repeat
            ParseFullMove(Match[0]);
          until not ExecNext;
        
        with vExpr[searchTermination] do
          if Exec(aText) then
            vGame.termination := Match[0];
            
      until not vExpr[searchGame].ExecNext;
  finally
    for vSearch in TSearch do
      vExpr[vSearch].Free;
  end;
end;

end.
