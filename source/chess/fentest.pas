
{$ASSERTIONS ON}

uses
  ChessTypes,
  Fen;

var
  data: TChessPositionData;
  i: integer;
  
begin
  data := EncodeChessPositionData();
  Assert(DecodeChessPositionData(data) = FENSTARTPOSITION);
  
  for i := Low(FENEXAMPLES) to High(FENEXAMPLES) do
  begin
    data := EncodeChessPositionData(FENEXAMPLES[i]);
    Assert(DecodeChessPositionData(data) = FENEXAMPLES[i]);
  end;
end.
