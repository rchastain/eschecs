
unit Settings;

interface

uses
  BGRABitmapTypes, Style, Language;
  
procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex, aEngine: integer;
  out aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  out aMoveTime: integer;
  out aFont: string
);
procedure WriteToINIFile(
  const aCurrentPosition: string;
  const aAutoPlay, aUpsideDown, aMarble: boolean;
  const aExePath, aHistory: string;
  const aIndex, aEngine: integer;
  const aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  const aMoveTime: integer;
  const aFont: string
);

procedure ReadStyle(out aStyle: TStyle);
procedure WriteStyle(const aStyle: TStyle);
procedure ReadLanguage(out aLanguage: TLanguage);
procedure WriteLanguage(const aLanguage: TLanguage);
procedure ReadColoring(out aColoring: boolean);
procedure WriteColoring(const aColoring: boolean);

var
  vFENPath, vLOGPath, vConfigFilesPath: string;
  
implementation

uses
  SysUtils,
  IniFiles,
  FEN;

const
  SECTION_OPTIONS = 'options';
  SECTION_COLORS = 'colors';
  DEFAULT_POSITION = FENSTARTPOSITION;
  DEFAULT_AUTOPLAY = 'TRUE';
  DEFAULT_UPSIDEDOWN = 'FALSE';
  DEFAULT_MARBLE = 'FALSE';
{$IFDEF WINDOWS}
  DEFAULT_EXEPATH = 'engines\fruit\fruit_21.exe';
{$ELSE}
  DEFAULT_EXEPATH = '/engines/moustique/01/moustique64';
{$ENDIF}  
  DEFAULT_HISTORY = '';
  DEFAULT_INDEX = 0;
  DEFAULT_ENGINE = -1;
  DEFAULT_STYLE = bsNew;
  
var
  vINIPath: string;
  
procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex, aEngine: integer;
  out aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  out aMoveTime: integer;
  out aFont: string
);
begin
  with TIniFile.Create(vINIPath) do
  try
    aCurrentPosition := ReadString(SECTION_OPTIONS, 'position', DEFAULT_POSITION);
    aAutoPlay := ReadString(SECTION_OPTIONS, 'autoplay', DEFAULT_AUTOPLAY) = 'TRUE';
    aUpsideDown := ReadString(SECTION_OPTIONS, 'upsidedown', DEFAULT_UPSIDEDOWN) = 'TRUE';
    aMarble := ReadString(SECTION_OPTIONS, 'marble', DEFAULT_MARBLE) = 'TRUE';
    aExePath := ReadString(SECTION_OPTIONS, 'engine', Concat(ExtractFilePath(ParamStr(0)),DEFAULT_EXEPATH));
    aHistory := ReadString(SECTION_OPTIONS, 'history', DEFAULT_HISTORY);
    aIndex := ReadInteger(SECTION_OPTIONS, 'index', DEFAULT_INDEX);
    aEngine := ReadInteger(SECTION_OPTIONS, 'engine', DEFAULT_ENGINE);
    aLightSquareColor := StrToBGRA(ReadString(SECTION_COLORS, 'light', 'A9A9A9FF'));
    aDarkSquareColor := StrToBGRA(ReadString(SECTION_COLORS, 'dark', '808080FF'));
    aGreenColor := StrToBGRA(ReadString(SECTION_COLORS, 'green', '60C00080'));
    aRedColor := StrToBGRA(ReadString(SECTION_COLORS, 'red', 'C0000080'));
    aMoveTime := ReadInteger(SECTION_OPTIONS, 'movetime', 1000);
    aFont := ReadString(SECTION_OPTIONS, 'font', '');
  finally
    Free;
  end;
end;

procedure WriteToINIFile(
  const aCurrentPosition: string;
  const aAutoPlay, aUpsideDown, aMarble: boolean;
  const aExePath, aHistory: string;
  const aIndex, aEngine: integer;
  const aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  const aMoveTime: integer;
  const aFont: string
);
begin
  with TIniFile.Create(vINIPath) do
  try
    WriteString(SECTION_OPTIONS, 'position', aCurrentPosition);
    WriteString(SECTION_OPTIONS, 'autoplay', UpperCase(BoolToStr(aAutoPlay, TRUE)));
    WriteString(SECTION_OPTIONS, 'upsidedown', UpperCase(BoolToStr(aUpsideDown, TRUE)));
    WriteString(SECTION_OPTIONS, 'marble', UpperCase(BoolToStr(aMarble, TRUE)));
    WriteString(SECTION_OPTIONS, 'engine', aExePath);
    WriteString(SECTION_OPTIONS, 'history', aHistory);
    WriteInteger(SECTION_OPTIONS, 'index', aIndex);
    WriteInteger(SECTION_OPTIONS, 'engine', aEngine);
    WriteString(SECTION_COLORS, 'light', BGRAToStr(aLightSquareColor));
    WriteString(SECTION_COLORS, 'dark', BGRAToStr(aDarkSquareColor));
    WriteString(SECTION_COLORS, 'green', BGRAToStr(aGreenColor));
    WriteString(SECTION_COLORS, 'red', BGRAToStr(aRedColor));
    WriteInteger(SECTION_OPTIONS, 'movetime', aMoveTime);
    WriteString(SECTION_OPTIONS, 'font', aFont);
    UpdateFile;
  finally
    Free;
  end;
end;

procedure ReadStyle(out aStyle: TStyle);
begin
  with TIniFile.Create(vINIPath) do
  try
    aStyle := ReadInteger(SECTION_OPTIONS, 'style', Ord(DEFAULT_STYLE));
  finally
    Free;
  end;
end;

procedure WriteStyle(const aStyle: TStyle);
begin
  with TIniFile.Create(vINIPath) do
  try
    WriteInteger(SECTION_OPTIONS, 'style', aStyle);
    UpdateFile;
  finally
    Free;
  end;
end;

procedure ReadLanguage(out aLanguage: TLanguage);
begin
  with TIniFile.Create(vINIPath) do
  try
    aLanguage := TLanguage(ReadInteger(SECTION_OPTIONS, 'language', Ord(lgEnglish)));
  finally
    Free;
  end;
end;

procedure WriteLanguage(const aLanguage: TLanguage);
begin
  with TIniFile.Create(vINIPath) do
  try
    WriteInteger(SECTION_OPTIONS, 'language', Ord(aLanguage));
    UpdateFile;
  finally
    Free;
  end;
end;

procedure ReadColoring(out aColoring: boolean);
begin
  with TIniFile.Create(vINIPath) do
  try
    aColoring := ReadString(SECTION_OPTIONS, 'coloring', 'TRUE') = 'TRUE';
  finally
    Free;
  end;
end;

procedure WriteColoring(const aColoring: boolean);
begin
  with TIniFile.Create(vINIPath) do
  try
    WriteString(SECTION_OPTIONS, 'coloring', UpperCase(BoolToStr(aColoring, TRUE)));
    UpdateFile;
  finally
    Free;
  end;
end;

begin
  vConfigFilesPath := Concat(ExtractFilePath(ParamStr(0)), 'config', directoryseparator);
  Assert(DirectoryExists(vConfigFilesPath) or CreateDir(vConfigFilesPath));
  vLOGPath := Concat(vConfigFilesPath, ChangeFileExt(ExtractFileName(ParamStr(0)), '.log'));
  vINIPath := ChangeFileExt(vLOGPath, '.ini');
  vFENPath := ChangeFileExt(vLOGPath, '.fen');
end.
