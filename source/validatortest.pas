
uses
  SysUtils, Validator;

{$I sample.inc}

var
  v: TValidator;
  i: integer;
  
begin
  v := TValidator.Create;
  for i := Low(CSample) to High(CSample) do
    WriteLn(v.IsFen(CSample[i]));
  v.Free;
end.
