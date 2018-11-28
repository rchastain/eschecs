
uses
  SysUtils, Schach;

var
  vText: text;
  vPosition: string;
  
begin
  if FileExists(ParamStr(1)) then
  begin
    Assign(vText, ParamStr(1));
    Reset(vText);
    ReadLn(vText, vPosition);
    Close(vText);
  end else
    vPosition := 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
    
  with TJSChess.Create do
  try
    SetPosition(vPosition);
    WriteLn(CurrentBoardAsText());
    WriteLn(BestMove());
  finally
    Free;
  end;
end.
