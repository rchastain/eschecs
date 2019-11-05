
unit Engines;

interface

uses
  SysUtils,
  IniFiles;
  
type
  TEngineInfo = record
    FCommand, FName, FDirectory: string;
    FExists: boolean;
  end;

var
  LEngines: array of TEngineInfo;

procedure LoadEnginesData(const AFileName: TFileName; const AChess960: boolean);

implementation

procedure LoadEnginesData(const AFileName: TFileName; const AChess960: boolean);
var
  x: integer = 0;
  LSection: string;
  n: integer = 0;
  s: string;
begin
  with TIniFile.Create(AFileName) do
  try
    while x >= 0 do 
    begin
      LSection := 'engine' + IntToStr(x);
      if ReadString(LSection, 'name', '') <> '' then
      begin
        if AChess960 and (LowerCase(ReadString(LSection, 'canplaychess960', '')) <> 'true') then
          Inc(n)
        else
        begin
          SetLength(LEngines, Succ(x) - n);
          with LEngines[x - n] do
          begin
            FName := ReadString(LSection, 'name', '');
            FCommand := ReadString(LSection, 'command', '');
            FDirectory := ReadString(LSection, 'workingdirectory', '');
            s := Concat(ExtractFilePath(ParamStr(0)), FDirectory);
            if DirectoryExists(s) then
              FDirectory := s;
            FExists := FileExists(Concat(FDirectory, FCommand));
          end;
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
  SetLength(LEngines, 0);
  
end.
