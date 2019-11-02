
{$ASSERTIONS ON}

uses
  SysUtils, Classes, ChessTypes, Fen;

var
  data: TPositionData;
  i: integer;
  
begin
  data := EncodePositionData();
  Assert(DecodePositionData(data, FALSE) = CFenStartPosition);
  
  for i := Low(CFenExamples) to High(CFenExamples) do
  begin
    data := EncodePositionData(CFenExamples[i]);
    WriteLn(CFenExamples[i]);
    WriteLn(data.FCastling[caWH]);
    WriteLn(data.FCastling[caWA]);
    WriteLn(data.FCastling[caBH]);
    WriteLn(data.FCastling[caBA]);
    Assert(DecodePositionData(data, FALSE) = CFenExamples[i]);
  end;
  
  with TStringList.Create do
  try
    LoadFromFile('fischerandom.fen');
    for i := 0 to Pred(Count) do
    begin
      data := EncodePositionData(Strings[i]);
      WriteLn(Strings[i]);
      WriteLn(data.FCastling[caWH]);
      WriteLn(data.FCastling[caWA]);
      WriteLn(data.FCastling[caBH]);
      WriteLn(data.FCastling[caBA]);
      Assert(DecodePositionData(data, TRUE) = Strings[i]);
    end;
  finally
    Free;
  end;
end.
