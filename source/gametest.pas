
uses
  SysUtils, Game;

var
  LStop: boolean;
  LInput: string;
  
begin
  with TChessGame.Create(
  //'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
  'rn1qk2r/5ppp/2p2n2/p1bPp3/Pp4b1/1B4Q1/NPPPNPPP/R1B1K2R b HAha - 2 10'
  ) do
  try
    LStop := FALSE;
    repeat
      WriteLn(CurrPosToStr());
      ReadLn(LInput);
      if IsLegal(LInput) then
        DoMove(LInput)
      else
        WriteLn('Illegal move.');
    until LStop;
  finally
    Free;
  end;
end.
