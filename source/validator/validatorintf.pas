
unit validatorintf;

interface

function IsFEN(const aInputStr: string): boolean;

implementation

function IsFEN; external 'validator';

end.
