
{**
@abstract(Joueur d'échecs artificiel.)
Le joueur d'échecs artificiel est composé
1° du livre d'ouvertures Pro Deo 1.4 de Jeroen Noomen,
2° du programme de recherche de mat de Valentin Albillo,
3° du programme d'échecs de Jürgen Schlottke.
@longcode(
)
}

unit Player;

interface

uses
  SysUtils, Classes;

const
  cMinDepth = 3;
  cMaxDepth = 5;
  cMSDepth1 = 3;
  cMSDepth2 = 5;

function GetPlayerMove(
  const aFENRecord: string;
  const aMovesArray: array of string;
  const aMinDepth: integer = cMinDepth;
  const aMaxDepth: integer = cMaxDepth;
  const aMSDepth1: integer = cMSDepth1;
  const aMSDepth2: integer = cMSDepth2
): string;

implementation

uses
  Mater, Schach, Validator, Log;

function Clock(): int64;
begin
  result := Trunc(Now() * SecsPerDay * 1000.0);
end;

var
  vProgram: TJSChess;
  
function GetPlayerMove(const aFENRecord: string; const aMovesArray: array of string; const aMinDepth, aMaxDepth, aMSDepth1, aMSDepth2: integer): string;
var
  vMaterMove,
  vProgMove: string;
  vErr: TBestMoveError;
  vIndex: integer;
  vPosition: string;
begin
  if not IsFEN(aFENRecord) then
    exit('1234');
  vPosition := aFENRecord;
  vProgram.SetPosition(vPosition);
  if Length(aMovesArray) > 0 then
  begin
    for vIndex := Low(aMovesArray) to High(aMovesArray) do vProgram.PlayMove(aMovesArray[vIndex]);
    vPosition := vProgram.FENRecord();
  end;
  vProgram.SetSearchDepth(aMinDepth, aMaxDepth);
  vProgMove := vProgram.BestMove(vErr);
  vMaterMove := SolveMate(vPosition, aMSDepth1, TRUE);
  if vMaterMove = '' then vMaterMove := SolveMate(vPosition, aMSDepth2, FALSE);
  if vMaterMove <> '' then
  begin
    result := vMaterMove;
    TLog.Append('rem mater');
  end else
    if vProgMove <> '' then
    begin
      result := vProgMove;
      TLog.Append(Format('rem schach %d', [Ord(vErr)]));
    end else
    begin
      result := 'a1a1';
      TLog.Append('rem no move');
    end;
end;

initialization
  vProgram := TJSChess.Create;
finalization
  vProgram.Free;
end.
