
unit IOUtils;

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
  fs: TFileStream;
begin
  fs := TFileStream.Create(Path, fmOpenRead);
  try
    SetLength(result, fs.Size div SizeOf(ansichar));
    fs.ReadBuffer(pansichar(result)^, fs.Size);
  finally
    fs.Free;
  end;
end;

class procedure TFile.WriteAllText(const Path: ansistring; const Contents: ansistring);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Path, fmCreate);
  try
    fs.WriteBuffer(pansichar(Contents)^, Length(Contents) * SizeOf(ansichar));
  finally
    fs.Free;
  end;
end;

class procedure TFile.AppendAllText(const Path: ansistring; const Contents: ansistring);
var
  fs: TFileStream;
begin
  if FileExists(Path) then
  begin
    fs := TFileStream.Create(Path, fmOpenWrite);
    try
      fs.Seek(0, soFromEnd); 
      fs.WriteBuffer(pansichar(Contents)^, Length(Contents) * SizeOf(ansichar));
    finally
      fs.Free;
    end;
  end else
    TFile.WriteAllText(Path, Contents);
end;

end.
