
uses
  sysutils, validator;

{$i sample.inc}

var
  i: integer;
  
begin
  for i := Low(SAMPLE) to High(SAMPLE) do
    WriteLn(IsFEN(SAMPLE[i]));
end.
