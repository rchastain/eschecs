
unit Connect;

interface

function CreateConnectedProcess(aProcessName: string): boolean;
procedure FreeConnectedProcess;
function ReadProcessOutput: string;
function ReadProcessError: string;
procedure WriteProcessInput(const aStr: string);

implementation

uses
  SysUtils, Classes, Process;

var
  vProcess: TProcess;

function ReadProcessOutput: string;
var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess(aProcess: TProcess);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead: LongInt;
  begin
    if aProcess.Running then
    begin
      BytesAvailable := aProcess.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable > 0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := aProcess.OutPut.Read(Buffer[1], BytesAvailable);
        result := result + Copy(Buffer, 1, BytesRead);
        BytesAvailable := aProcess.Output.NumBytesAvailable;
        NoMoreOutput := FALSE;
      end;
    end;
  end;
begin
  result := '';
  repeat
    NoMoreOutput := TRUE;
    DoStuffForProcess(vProcess);
  until NoMoreOutput;
end;

function ReadProcessError: string;
begin
  result := '';
end;

function CreateConnectedProcess(aProcessName: string): boolean;
begin
  result := FALSE;
  
  vProcess := TProcess.Create(nil);
  vProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  
  if not vProcess.Running then
  begin
    vProcess.CommandLine := aProcessName;
    vProcess.Execute;
  end;
  
  result := TRUE;
end;

procedure FreeConnectedProcess;
begin
  (*
  if vProcess.Running then
    vProcess.Terminate(0);
  *)
  vProcess.Free;
end;

procedure WriteProcessInput(const aStr: string);
var
  s: string;
begin
  if vProcess.Running then
  begin
    s := aStr + #13#10;
    vProcess.Input.Write(s[1], Length(s));
  end;
end;

end.
