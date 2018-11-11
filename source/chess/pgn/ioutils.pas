
unit IOUtils;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

type
  TFile = class
    class function ReadAllText(const Path: ansistring): ansistring;
    class procedure WriteAllText(const Path: ansistring; const Contents: ansistring);
    class procedure AppendAllText(const Path: ansistring; const Contents: ansistring);
  end;

implementation

uses
  Classes, SysUtils;

class function TFile.ReadAllText(const Path: ansistring): ansistring;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(Path, fmOpenRead);
  try
    SetLength(result, stream.Size div SizeOf(ansichar));
    stream.ReadBuffer(pansichar(result)^, stream.Size);
  finally
    stream.Free;
  end;
end;

class procedure TFile.WriteAllText(const Path: ansistring; const Contents: ansistring);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(Path, fmCreate);
  try
    stream.WriteBuffer(pansichar(Contents)^, Length(Contents) * SizeOf(ansichar));
  finally
    stream.Free;
  end;
end;

class procedure TFile.AppendAllText(const Path: ansistring; const Contents: ansistring);
var
  stream: TFileStream;
begin
  if FileExists(Path) then
  begin
    stream := TFileStream.Create(Path, fmOpenWrite);
    try
      stream.Seek(0, soFromEnd); 
      stream.WriteBuffer(pansichar(Contents)^, Length(Contents) * SizeOf(ansichar));
    finally
      stream.Free;
    end;
  end else
    TFile.WriteAllText(Path, Contents);
end;

end.
