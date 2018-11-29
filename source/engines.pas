
unit Engines;

interface

uses
  SysUtils,
  IOUtils,
  INIFiles;
  
type
  TEngineInfo = record
    vCommand, vName, vDirectory: string;
    vExists: boolean;
  end;

var
  vEngines: array of TEngineInfo;

procedure LoadEnginesDataFromINI(const aFileName: TFileName);

implementation

procedure LoadEnginesDataFromINI(const aFileName: TFileName);
var
  x: integer = 0;
  section: string;
begin
  with TIniFile.Create(aFileName) do
  try
    while x >= 0 do 
    begin
      section := 'engine' + IntToStr(x);
      if ReadString(section, 'name', '') <> '' then
      begin
        SetLength(vEngines, Succ(x));
        with vEngines[x] do
        begin
          vName := ReadString(section, 'name', '');
          vCommand := ReadString(section, 'command', '');
          vDirectory := ExtractFileDir(ParamStr(0)) + ReadString(section, 'workingdirectory', '');
          vExists := FileExists(Concat(vDirectory, vCommand));
        end;
        Inc(x);
      end else
        x := -1;
    end;
  finally
    Free;
  end;
end;

finalization
  SetLength(vEngines, 0);
  
end.
