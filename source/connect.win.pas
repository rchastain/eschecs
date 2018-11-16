
unit Connect.Win;

interface

function CreateConnectedProcess(aProcessName: string): boolean;
procedure FreeConnectedProcess;
function ReadProcessOutput: string;
function ReadProcessError: string;
procedure WriteProcessInput(const aStr: string);

implementation

uses
  Windows;

var
  InputRead,
  InputWrite,
  OutputRead,
  OutputWrite,
  ErrorRead,
  ErrorWrite: THandle;
  vSI: TStartupInfo;
  vPI: TProcessInformation;
  vSA: TSecurityAttributes;

function ReadPipeInput(InputPipe: THandle; var BytesRem: Integer): string;
var
  TextBuffer: array[1..32767] of char;
  BytesRead: longword;
  PipeSize: Integer;
begin
  result := '';
  PipeSize := SizeOf(TextBuffer);
  PeekNamedPipe(InputPipe, nil, PipeSize, @BytesRead, @PipeSize, @BytesRem);
  if BytesRead > 0 then
  begin
    {$hints off}
    FillChar(TextBuffer, SizeOf(TextBuffer), #0);
    {$hints on}
    ReadFile(InputPipe, TextBuffer, PipeSize, BytesRead, nil);
    OemToChar(@TextBuffer, @TextBuffer);
    result := string(TextBuffer);
    SetLength(result, BytesRead);
  end;
end;

procedure WritePipeOut(OutputPipe: THandle; InString: string);
var
  BytesWritten: DWORD;
begin
  InString := InString + #13#10;
  BytesWritten := 0;
  WriteFile(OutputPipe, Instring[1], Length(Instring), BytesWritten, nil);
end;

function CreateConnectedProcess(aProcessName: string): boolean;
begin
  result := FALSE;
  
  with vSA do
  begin
    nLength := SizeOf(TSecurityAttributes);
    bInheritHandle := TRUE;
    lpSecurityDescriptor := nil;
  end;
  
  if (not CreatePipe(InputRead, InputWrite, @vSA, 0))
  or (not CreatePipe(OutputRead, OutputWrite, @vSA, 0))
  or (not CreatePipe(ErrorRead, ErrorWrite, @vSA, 0)) then
    Exit;
  
  FillChar(vSI, SizeOf(TStartupInfo), #0);
  vSI.CB := SizeOf(TStartupInfo);
  vSI.hStdInput := InputRead;
  vSI.hStdOutput := OutputWrite;
  vSI.hStdError := ErrorWrite;
  vSI.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
  vSI.wShowWindow := SW_HIDE;

  if not CreateProcess(
    nil,
    PChar(aProcessName),
    @vSA,
    @vSA,
    TRUE,
    CREATE_NEW_CONSOLE + SYNCHRONIZE,
    nil,
    nil,
    vSI,
    vPI
  ) then
    Exit;
  
  result := TRUE;
end;

procedure FreeConnectedProcess;
begin
  CloseHandle(vPI.hProcess);
  CloseHandle(vPI.hThread);
  CloseHandle(InputRead);
  CloseHandle(InputWrite);
  CloseHandle(OutputRead);
  CloseHandle(OutputWrite);
  CloseHandle(ErrorRead);
  CloseHandle(ErrorWrite);
end;

function ReadProcessOutput: string;
var
  BytesRem: longint;
begin
  BytesRem := 0;
  result := ReadPipeInput(OutputRead, BytesRem);
end;

function ReadProcessError: string;
var
  BytesRem: longint;
begin
  BytesRem := 0;
  result := ReadPipeInput(ErrorRead, BytesRem);
end;

procedure WriteProcessInput(const aStr: string);
begin
  WritePipeOut(InputWrite, aStr);
end;

end.
