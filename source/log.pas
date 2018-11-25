
unit Log;

interface

uses
  SysUtils, Settings;

type
  TLog = class
    class procedure Append(const aLine: string);
  end;
  
var
  vLog: text;  

implementation

class procedure TLog.Append(const aLine: string);
begin
  WriteLn(vLog, DateTimeToStr(Now()) + ' ' + aLine);
  Flush(vLog);
end;
  
end.
