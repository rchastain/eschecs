
unit IOUtils;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface
  
type 
  TFile = class
    class function ReadAllText(const aFileName: ansistring): ansistring;{$IFDEF FPC} inline; static;{$ENDIF}
    class procedure WriteAllText(const aFileName: ansistring; const aText: ansistring);{$IFDEF FPC} inline; static;{$ENDIF}
  end;	

implementation

uses
  Classes, SysUtils;

const
  szChar = SizeOf(ansichar);

class function TFile.ReadAllText(const aFileName: ansistring): ansistring;
var
  s: TFileStream;
begin
  s := TFileStream.Create(aFileName, fmOpenRead);
  try
    SetLength(result, s.Size div szChar);
    s.ReadBuffer(pansichar(result)^, s.Size);
  finally
    s.Free;
  end;
end;

class procedure TFile.WriteAllText(const aFileName: ansistring; const aText: ansistring);
var
  s: TFileStream;
begin
  (*
  if FileExists(aFileName) then
  begin
    s := TFileStream.Create(aFileName, fmOpenWrite);
    try
      s.Seek(0, soFromEnd); 
      s.WriteBuffer(pansichar(aText)^, Length(aText) * szChar);
    finally
      s.Free;
    end;
  end else
  *)
  begin
    s := TFileStream.Create(aFileName, fmCreate);
    try
      s.WriteBuffer(pansichar(aText)^, Length(aText) * szChar);
    finally
      s.Free;
    end;
  end;
end;

end.
