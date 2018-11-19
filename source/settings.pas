
unit Settings;

interface

uses
  BGRABitmapTypes, Style;
  
procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex, aEngine: integer;
  out aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  out aStyle: TStyle;
  out aMoveTime: integer
);
procedure WriteToINIFile(
  const aCurrentPosition: string;
  const aAutoPlay, aUpsideDown, aMarble: boolean;
  const aExePath, aHistory: string;
  const aIndex, aEngine: integer;
  const aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  const aStyle: TStyle;
  const aMoveTime: integer
);

var
  vFENPath: string;
  vLOGPath: string;
  vConfigFilesPath: string;
  
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
  {$else}
  DEFAULT_EXEPATH = '/engines/moustique/01/moustique64';
  {$ENDIF}
  
  DEFAULT_HISTORY = '';
  DEFAULT_INDEX = 0;
  DEFAULT_ENGINE = -1;
  
var
  vINIPath: string;
  
procedure ReadFromINIFile(
  out aCurrentPosition: string;
  out aAutoPlay, aUpsideDown, aMarble: boolean;
  out aExePath, aHistory: string;
  out aIndex, aEngine: integer;
  out aLightSquareColor, aDarkSquareColor, aGreenColor, aRedColor: TBGRAPixel;
  out aStyle: TStyle;
  out aMoveTime: integer
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
    aStyle := ReadInteger(SECTION_OPTIONS, 'style', 0);
    aMoveTime := ReadInteger(SECTION_OPTIONS, 'movetime', 1000);
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
  const aStyle: TStyle;
  const aMoveTime: integer
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
    WriteInteger(SECTION_OPTIONS, 'style', aStyle);
    WriteInteger(SECTION_OPTIONS, 'movetime', aMoveTime);
    UpdateFile;
  finally
    Free;
  end;
end;

begin
  vConfigFilesPath := Concat(ExtractFilePath(ParamStr(0)), 'config', directoryseparator);
  vLOGPath := Concat(vConfigFilesPath, ChangeFileExt(ExtractFileName(ParamStr(0)), '.log'));
  vINIPath := ChangeFileExt(ParamStr(0), '.ini');
  vFENPath := Concat(vConfigFilesPath, ChangeFileExt(ExtractFileName(ParamStr(0)), '.fen'));
end.
