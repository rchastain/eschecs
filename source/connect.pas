
unit Connect;

interface

function CreateConnectedProcess(AProcessName: string): boolean;
procedure FreeConnectedProcess;
function ReadProcessOutput: string;
procedure WriteProcessInput(const AStr: string);

implementation

uses
  SysUtils, Classes, Process;

var
  LProcess: TProcess;

function ReadProcessOutput: string;
var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess(AProcess: TProcess);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead: LongInt;
  begin
    if AProcess.Running then
    begin
      BytesAvailable := AProcess.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable > 0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := AProcess.Output.Read(Buffer[1], BytesAvailable);
        result := result + Copy(Buffer, 1, BytesRead);
        BytesAvailable := AProcess.Output.NumBytesAvailable;
        NoMoreOutput := FALSE;
      end;
    end;
  end;
  
begin
  result := '';
  repeat
    NoMoreOutput := TRUE;
    DoStuffForProcess(LProcess);
  until NoMoreOutput;
end;

function CreateConnectedProcess(AProcessName: string): boolean;
begin
  result := FALSE;
  LProcess := TProcess.Create(nil);
  LProcess.Options := [poUsePipes, poStdErrToOutput, poNoConsole];
  LProcess.Executable := AProcessName;
  LProcess.Execute;
  result := TRUE;
end;

procedure FreeConnectedProcess;
begin
  if LProcess.Running then
    LProcess.Terminate(0);
  LProcess.Free;
end;

procedure WriteProcessInput(const AStr: string);
var
  s: string;
begin
  if LProcess.Running then
  begin
    s := AStr + #10;
    LProcess.Input.Write(s[1], Length(s));
  end;
end;

end.
