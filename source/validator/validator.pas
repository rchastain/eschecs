
library validator;

uses
  validatorcore;

function IsFEN(const aInputStr: string): boolean;
var
  validator: TValidator;
begin
  validator := TValidator.Create;
  result := validator.IsFEN(aInputStr);
  validator.Free;
end;

exports
  IsFEN name 'IsFEN';

begin
end.
