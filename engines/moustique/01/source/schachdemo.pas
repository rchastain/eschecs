
uses
  SysUtils, TypInfo, Schach;

function Clock(): int64;
begin
  result := Trunc(Now() * SecsPerDay * 1000.0);
end;

var
  vClock: int64;
  vMove: string;
  vCount: integer;
  vError: TBestMoveError;
  
begin
  vCount := 0;
  with TJSChess.Create do
  try
    SetPosition('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
    repeat
      Sleep(10);
      Write(CurrentBoardAsText(false));
      vClock := Clock();
      vMove := BestMove(vError);
      vClock := Clock() - vClock;
      WriteLn(Format(' %s %0.3d ms %s', [vMove, vClock, GetEnumName(TypeInfo(TBestMoveError), Ord(vError))]));
      Inc(vCount);
    until not (vError in [errSuccess, errCheck]) or (vCount = 500);
  finally
    Free;
  end;
end.
