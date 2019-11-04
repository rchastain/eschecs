
unit CommandLine;

interface

uses
  SysUtils;

function HasOption(const AName: string; out AValue: integer): boolean; overload;
function HasOption(const AName: string; out AValue: string): boolean; overload;
function HasOption(const AName: string; out AValue: boolean): boolean; overload;

implementation

uses
  RegExpr;

const
  C1 = '[-/]';
  C2 = '[=: ]';

function HasOption(const AName: string; out AValue: integer): boolean;
var
  LExpr: TRegExpr;
begin
  LExpr := TRegExpr.Create(Concat(C1, AName, C2, '(\d+)'));
  LExpr.ModifierI := TRUE;
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := StrToInt(LExpr.Match[1])
  else
    AValue := 0;
  LExpr.Free;
end;

function HasOption(const AName: string; out AValue: string): boolean;
var
  LExpr: TRegExpr;
begin
  LExpr := TRegExpr.Create(Concat(C1, AName, C2, '(\w+)'));
  LExpr.ModifierI := TRUE;
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := LExpr.Match[1]
  else
    AValue := '';
  LExpr.Free;
end;

function HasOption(const AName: string; out AValue: boolean): boolean;
var
  LExpr: TRegExpr;
begin
  LExpr := TRegExpr.Create(Concat(C1, AName, C2, '(FALSE|TRUE)'));
  LExpr.ModifierI := TRUE;
  result := LExpr.Exec(CmdLine);
  if result then
    AValue := UpperCase(LExpr.Match[1]) = 'TRUE'
  else
    AValue := FALSE;
  LExpr.Free;
end;

end.
