
uses
  SysUtils, ChessGame;

var
  vText: text;
  vPosition, vMove: string;
  vInfo: integer;
  
begin
  if FileExists(ParamStr(1)) then
  begin
    Assign(vText, ParamStr(1));
    Reset(vText);
    ReadLn(vText, vPosition);
    Close(vText);
  end else
    vPosition := 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
    
  with TChessGame.Create(vPosition) do
  try
    Writeln('hello');
  finally
    Free;
  end;
end.
