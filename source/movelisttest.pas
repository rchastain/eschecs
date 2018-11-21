
{$H+}
{$ASSERTIONS ON}

uses
  SysUtils, MoveList;
  
const
  N = 10000;
  
var
  t: TDateTime;
  i: integer;
  s: string;
  
begin
  Randomize;
  
  t := Now;
  s := CreateSample(N);
  with TMoveList.Create(s) do
  try
    Assert(GetCount = N);
    Assert(GetString(N) = s);
  finally
    Free;
  end;
  t := Now - t;
  WriteLn(Trunc(MSECSPERDAY * t));
  
  t := Now;
  with TMoveList.Create('') do
  try
    for i := 1 to N do Append(CreateSample(1));
    Assert(GetCount = N);
  finally
    Free;
  end;
  t := Now - t;
  WriteLn(Trunc(MSECSPERDAY * t));
end.
