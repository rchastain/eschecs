
{$ASSERTIONS ON}

uses
  SysUtils,
  ChessGame;

function Clock(): int64;
begin
  result := Trunc(Now() * SecsPerDay * 1000.0);
end;

const
  cMoves: array[0..0] of string = (
    'd2d4 g8f6 c2c4 c7c5 d4d5 e7e6 b1c3 e6d5 c4d5 d7d6 e2e4 g7g6 f1d3 f8g7 h2h3 e8g8 g1f3 b7b5 d3b5 f6e4 c3e4 d8a5 f3d2 a5b5 e4d6 b5a6 d2c4 b8d7 e1g1 d7b6 c4b6 a6b6 d6c8 a8c8 a1b1 f8d8 c1f4 b6b7 d5d6 g7f8 d1d3 f8d6 f4d6 c8c6 f1d1 b7d7 d3a3 c6d6 d1d6 d7d6 a3a7 d8e8 a2a4 d6f4 a4a5 f4d4 a5a6 h7h5 a7c7 f7f5 c7b6 g6g5 b6g6 g8f8 g6f5 f8g7 f5g5 g7f7 g5h5 f7f8 a6a7 e8a8 h5f3 f8e7 b1e1 e7d7 f3a8 d4b2 a8e8 d7c7 a7a8n'
  );
  cExpected: array[0..0] of string = (
    'N3Q3/2k5/8/2p5/8/7P/1q3PP1/4R1K1 b - - 0 41'
  );
  cTestIndex = 0;
  
var
  vMoves,
  m: string;
  i: integer;
  t: int64;
  
begin
  with TChessGame.Create do
  try
    t := Clock();
    vMoves := cMoves[cTestIndex];
    while Length(vMoves) > 0 do
    begin
      i := Pos(' ', vMoves);
      if i = 0 then m := vMoves else m := Copy(vMoves, 1, i - 1);
      Assert(IsLegal(m));
      PlayMove(m);
      if i = 0 then SetLength(vMoves, 0) else vMoves := Copy(vMoves, Succ(i));
    end;
    Assert(FENRecord() = cExpected[cTestIndex]);
    t := Clock() - t;
    WriteLn(t, ' ms');
  finally
    Free;
  end;
end.
