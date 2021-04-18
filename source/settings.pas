
unit Settings;

interface

uses
  SysUtils, BGRABitmapTypes, Images, Language, Utils, Fen;

procedure LoadSettings(
  out ACurrPos: string;
  out AAutoPlay, AUpsideDown: boolean;
  out AStyle: TBoardStyle;
  out AHist: string;
  out APosIndex{, AEngIndex}: integer;
  out AEngine: TFileName;
  out ALightSquareColor, ADarkSquareColor, AGreen, ARed: TBGRAPixel;
  out AMoveTime: integer;
  out AFont: string;
  out ALang: TLanguage;
  out AColoring: boolean;
  out AScale: integer;
  out AChess960: boolean
);
procedure SaveSettings(
  const ACurrPos: string;
  const AAutoPlay, AUpsideDown: boolean;
  const AStyle: TBoardStyle;
  const AHist: string;
  const APosIndex{, AEngIndex}: integer;
  const AEngine: TFileName;
  const ALightSquareColor, ADarkSquareColor, AGreen, ARed: TBGRAPixel;
  const AMoveTime: integer;
  const AFont: string;
  const ALang: TLanguage;
  const AColoring: boolean;
  const AScale: integer;
  const AChess960: boolean
);

const
  CDefaultEngine = {-1}'engine/cheng4/cheng4_linux_x64';
  CDefaultPosition: array[boolean] of string = (
    CFenStartPosition,
    CFenStartPosition518
  );
  
implementation

uses
  IniFiles;

const
  CSectionOptions = 'options';
  CSectionColors = 'colors';
  CDefaultAutoplay = 'true';
  CDefaultUpsideDown = 'false';
  CDefaultStyle = bsSimple;  
  CDefaultHistory = '';
  CDefaultIndex = 0;
  CDefaultMoveTime = 1000;
  CDefaultFont = 'alpha';
  CDefaultLanguage = lgEnglish;
  CDefaultScale = 50;
  CDefaultChess960 = 'false';
  
var
  LIniFileName: TFileName;
  
procedure LoadSettings(
  out ACurrPos: string;
  out AAutoPlay, AUpsideDown: boolean;
  out AStyle: TBoardStyle;
  out AHist: string;
  out APosIndex{, AEngIndex}: integer;
  out AEngine: TFileName;
  out ALightSquareColor, ADarkSquareColor, AGreen, ARed: TBGRAPixel;
  out AMoveTime: integer;
  out AFont: string;
  out ALang: TLanguage;
  out AColoring: boolean;
  out AScale: integer;
  out AChess960: boolean
);
begin
  with TIniFile.Create(LIniFileName) do
  try
    AChess960 := LowerCase(ReadString(CSectionOptions, 'chess960', CDefaultChess960)) = 'true';
    ACurrPos := ReadString(CSectionOptions, 'position', CDefaultPosition[AChess960]);
    AAutoPlay := LowerCase(ReadString(CSectionOptions, 'autoplay', CDefaultAutoplay)) = 'true';
    AUpsideDown := LowerCase(ReadString(CSectionOptions, 'upsidedown', CDefaultUpsideDown)) = 'true';
    AStyle := TBoardStyle(ReadInteger(CSectionOptions, 'style', Ord(CDefaultStyle)));
    AHist := ReadString(CSectionOptions, 'history', CDefaultHistory);
    APosIndex := ReadInteger(CSectionOptions, 'index', CDefaultIndex);
    AEngine := ReadString(CSectionOptions, 'engine', CDefaultEngine);
    ALightSquareColor := StrToBGRA(ReadString(CSectionColors, 'light', 'A9A9A9FF'));
    ADarkSquareColor := StrToBGRA(ReadString(CSectionColors, 'dark', '808080FF'));
    AGreen := StrToBGRA(ReadString(CSectionColors, 'green', '60C00080'));
    ARed := StrToBGRA(ReadString(CSectionColors, 'red', 'C0000080'));
    AMoveTime := ReadInteger(CSectionOptions, 'movetime', CDefaultMoveTime);
    AFont := ReadString(CSectionOptions, 'font', CDefaultFont);
    ALang := TLanguage(ReadInteger(CSectionOptions, 'language', Ord(CDefaultLanguage)));
    AColoring := LowerCase(ReadString(CSectionOptions, 'coloring', 'true')) = 'true';
    AScale := ReadInteger(CSectionOptions, 'scale', CDefaultScale);
  finally
    Free;
  end;
end;

procedure SaveSettings(
  const ACurrPos: string;
  const AAutoPlay, AUpsideDown: boolean;
  const AStyle: TBoardStyle;
  const AHist: string;
  const APosIndex{, AEngIndex}: integer;
  const AEngine: TFileName;
  const ALightSquareColor, ADarkSquareColor, AGreen, ARed: TBGRAPixel;
  const AMoveTime: integer;
  const AFont: string;
  const ALang: TLanguage;
  const AColoring: boolean;
  const AScale: integer;
  const AChess960: boolean
);
begin
  with TIniFile.Create(LIniFileName) do
  try
    WriteString(CSectionOptions, 'position', ACurrPos);
    WriteString(CSectionOptions, 'autoplay', LowerCase(BoolToStr(AAutoPlay, TRUE)));
    WriteString(CSectionOptions, 'upsidedown', LowerCase(BoolToStr(AUpsideDown, TRUE)));
    WriteInteger(CSectionOptions, 'style', Ord(AStyle));
    WriteString(CSectionOptions, 'history', AHist);
    WriteInteger(CSectionOptions, 'index', APosIndex);
    WriteString(CSectionColors, 'engine', AEngine);
    WriteString(CSectionColors, 'light', BGRAToStr(ALightSquareColor));
    WriteString(CSectionColors, 'dark', BGRAToStr(ADarkSquareColor));
    WriteString(CSectionColors, 'green', BGRAToStr(AGreen));
    WriteString(CSectionColors, 'red', BGRAToStr(ARed));
    WriteInteger(CSectionOptions, 'movetime', AMoveTime);
    WriteString(CSectionOptions, 'font', AFont);
    WriteInteger(CSectionOptions, 'language', Ord(ALang));
    WriteString(CSectionOptions, 'coloring', LowerCase(BoolToStr(AColoring, TRUE)));
    WriteInteger(CSectionOptions, 'scale', AScale);
    WriteString(CSectionOptions, 'chess960', LowerCase(BoolToStr(AChess960, TRUE)));
    UpdateFile;
  finally
    Free;
  end;
end;

begin
  LIniFileName := Concat(LConfigFilesPath, 'eschecs.ini');
end.
