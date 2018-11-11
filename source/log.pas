
unit Log;

interface

uses
  SysUtils, Settings;

type
  TLog = class
    class procedure Append(const aLine: string);
  end;

implementation

var
  vLog: text;

class procedure TLog.Append(const aLine: string);
begin
  WriteLn(vLog, DateTimeToStr(Now()) + ' ' + aLine);
  Flush(vLog);
end;

var
  VFileName: string;
  
initialization
  vFileName := vLOGPath;
  Assign(vLog, vFileName);
  if FileExists(vFileName) then
    Append(vLog)
  else
    Rewrite(vLog);
    
finalization
  Close(vLog);
  
end.
