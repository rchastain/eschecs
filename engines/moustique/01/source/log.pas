
unit Log;

interface

uses
  SysUtils;

type
  TLog = class
    class procedure Append(const aLine: string);
  end;

implementation

{$IFDEF DEBUG}
var vLog: text;
{$ENDIF}

class procedure TLog.Append(const aLine: string);
begin
{$IFDEF DEBUG}
  WriteLn(vLog, DateTimeToStr(Now()) + ' ' + aLine);
  Flush(vLog);
{$ENDIF}
end;

var
  VFileName: string;
  
initialization
  vFileName := ChangeFileExt(ParamStr(0), '.log');
{$IFDEF DEBUG}
  Assign(vLog, vFileName);
  if FileExists(vFileName) then
    Append(vLog)
  else
    Rewrite(vLog);
finalization
  Close(vLog);
{$ENDIF}
end.
