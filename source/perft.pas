
uses
  SysUtils, Classes, ChessTypes, Position;

var
  LPos, LNext: TChessPosition;
  LList: TStringList;
  i: integer;
  s: string;
  
begin
  LPos := TChessPosition.Create('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
  LNext := TChessPosition.Create;
  
  LPos.SetVariables(FALSE);
  LPos.GenerateMoves1(LPos.active);
  LPos.GenerateMoves2(LPos.active);
  
  LList := TStringList.Create;
  s := LPos.FENRecord;
  for i := 0 to LPos.list.Count - 1 do
  begin
    LNext.Create(s);
    LNext.DoMove(LPos.list[i]);
    LNext.active := OtherColor(LNext.active);
    LNext.SetVariables(TRUE);
    if not LNext.check then
      LList.Add(LPos.list[i]);
  end;
  WriteLn(LList.Count);
  
  LPos.Free;
  LNext.Free;
end.
