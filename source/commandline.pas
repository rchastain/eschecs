
unit CommandLine;

interface

function HasOption(const AName: string; out AValue: integer): boolean; overload;
function HasOption(const AName: string; out AValue: string): boolean; overload;
function HasOption(const AName: string; out AValue: boolean): boolean; overload;

implementation

uses
  SysUtils, RegExpr;

const
  C1 = '[-/]';
  C2 = '[=: ]';
  
var
  LExpr: TRegExpr;

function HasOption(const AName: string; out AValue: integer): boolean;
begin
  LExpr.Expression := Concat(C1, AName, C2, '(\d+)');
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := StrToInt(LExpr.Match[1])
  else
    AValue := 0;
end;

function HasOption(const AName: string; out AValue: string): boolean;
begin
  LExpr.Expression := Concat(C1, AName, C2, '(\w+)');
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := LExpr.Match[1]
  else
    AValue := '';
end;

function HasOption(const AName: string; out AValue: boolean): boolean;
begin
  LExpr.Expression := Concat(C1, AName, C2, '(FALSE|TRUE)');
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := UpperCase(LExpr.Match[1]) = 'TRUE'
  else
    AValue := FALSE;
end;

initialization
  LExpr := TRegExpr.Create;
  LExpr.ModifierI := TRUE;

finalization
  LExpr.Free;
end.
