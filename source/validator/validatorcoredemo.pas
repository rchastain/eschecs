
uses
  sysutils, validatorcore;

{$i sample.inc}

var
  validator: TValidator;
  i: integer;
  
begin
  validator := TValidator.Create;
  for i := Low(SAMPLE) to High(SAMPLE) do
    WriteLn(validator.IsFEN(SAMPLE[i]));
  validator.Free;
end.
