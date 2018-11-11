
{$ASSERTIONS ON}

uses
  SysUtils, Classes, StrUtils, IOUtils, PGN;

var
  vFileName: string;
  vGameList: TList;
  vGameIndex, vTagIndex, vMoveIndex: integer;
  vGroupIndex: TGroupIndex;
  vGroups: PGroups;
  
begin
  if Length(ParamStr(1)) > 0 then
    vFileName := ParamStr(1)
  else
    vFileName := 'sample.pgn';
  
  Assert(FileExists(vFileName));
  
  vGameList := ParsePGNText(TFile.ReadAllText(vFileName));
  
  for vGameIndex := 0 to Pred(vGameList.Count) do
    with TChessGame(vGameList[vGameIndex]) do
    begin
      WriteLn('White=', tags.Values['White']);
      
      for vTagIndex := 0 to tags.Count - 1 do
        WriteLn(
          tags.Names[vTagIndex], '=',
          tags.ValueFromIndex[vTagIndex]
        );
      
      for vMoveIndex := 0 to Pred(moves.Count) do
      begin
        vGroups := moves[vMoveIndex];
        for vGroupIndex := Low(TGroupIndex) to High(TGroupIndex) do
          Write(vGroups[vGroupIndex], IfThen(vGroupIndex < High(TGroupIndex), ',', ''));
        WriteLn;
      end;
      
      WriteLn(termination);
    end;
  
  for vGameIndex := Pred(vGameList.Count) downto 0 do
    TChessGame(vGameList[vGameIndex]).Free;
  
  vGameList.Free;
end.
