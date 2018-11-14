
unit Settings;

interface

procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex: integer
);
procedure WriteToINIFile(
  const aCurrentPosition: string;
  const aAutoPlay, aUpsideDown, aMarble: boolean;
  const aExePath, aHistory: string;
  const aIndex: integer
);

var
  vFENPath: string;
  vLOGPath: string;
  
implementation

uses
  SysUtils,
  IniFiles,
  FEN;

const
  SECTION = 'eschecs';
  DEFAULT_POSITION = FENSTARTPOSITION;
  DEFAULT_AUTOPLAY = 'TRUE';
  DEFAULT_UPSIDEDOWN = 'FALSE';
  DEFAULT_MARBLE = 'FALSE';
  
  {$IFDEF WINDOWS}
  DEFAULT_EXEPATH = 'engines\fruit\fruit_21.exe';
  {$else}
  DEFAULT_EXEPATH = '/engines/moustique/01/moustique64';
  {$ENDIF}
  
  DEFAULT_HISTORY = '';
  DEFAULT_INDEX = 0;
  
var
  vINIPath: string;
  
procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex: integer
);
begin
  with TIniFile.Create(vINIPath) do
  try
    aCurrentPosition := ReadString(SECTION, 'position', DEFAULT_POSITION);
    aAutoPlay := ReadString(SECTION, 'autoplay', DEFAULT_AUTOPLAY) = 'TRUE';
    aUpsideDown := ReadString(SECTION, 'upsidedown', DEFAULT_UPSIDEDOWN) = 'TRUE';
    aMarble := ReadString(SECTION, 'marble', DEFAULT_MARBLE) = 'TRUE';
    aExePath := ReadString(SECTION, 'engine', Concat(ExtractFilePath(ParamStr(0)),DEFAULT_EXEPATH));
    aHistory := ReadString(SECTION, 'history', DEFAULT_HISTORY);
    aIndex := ReadInteger(SECTION, 'index', DEFAULT_INDEX)
  finally
    Free;
  end;
end;

procedure WriteToINIFile(
  const aCurrentPosition: string;
  const aAutoPlay, aUpsideDown, aMarble: boolean;
  const aExePath, aHistory: string;
  const aIndex: integer
);
begin
  with TIniFile.Create(vINIPath) do
  try
    WriteString(SECTION, 'position', aCurrentPosition);
    WriteString(SECTION, 'autoplay', UpperCase(BoolToStr(aAutoPlay, TRUE)));
    WriteString(SECTION, 'upsidedown', UpperCase(BoolToStr(aUpsideDown, TRUE)));
    WriteString(SECTION, 'marble', UpperCase(BoolToStr(aMarble, TRUE)));
    WriteString(SECTION, 'engine', aExePath);
    WriteString(SECTION, 'history', aHistory);
    WriteInteger(SECTION, 'index', aIndex);
    UpdateFile;
  finally
    Free;
  end;
end;

begin
  vLOGPath := ChangeFileExt(ParamStr(0), '.log');
  vINIPath := ChangeFileExt(ParamStr(0), '.ini');
  vFENPath := ChangeFileExt(ParamStr(0), '.fen');
  (*
  vINIPath := GetEnvironmentVariable('APPDATA');
  Assert(DirectoryExists(vINIPath));
  vINIPath := Concat(IncludeTrailingPathDelimiter(vINIPath), ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  if not DirectoryExists(vINIPath) then
    CreateDir(vINIPath);
  Assert(DirectoryExists(vINIPath));
  vINIPath := Concat(IncludeTrailingPathDelimiter(vINIPath), ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));
  *)
end.
