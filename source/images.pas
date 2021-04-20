
unit Images;

interface

uses
  Classes,
  SysUtils,
  Math,
  TypInfo,
  BGRABitmap,
  BGRABitmapTypes,
  BGRAGradients,
 {BGRAGraphics,}
  ChessTypes;

type
  TBoardStyle = (bsSimple, bsMarbleOriginal, bsMarbleNew, bsMarbleCustom, bsWood);
  TBackColor = (bcGreen, bcRed);
  
var
  LChessboard: TBGRABitmap;
  LPieceImage: array[TPieceColorStrict, TPieceTypeStrict] of TBGRABitmap;
  LBackColors: array[TBackColor] of TBGRAPixel;
  LLSColor, LDSColor: TBGRAPixel;
  LLMColor, LLMColor2, LDMColor, LDMColor2: TBGRAPixel; (* Light marble, dark marble *)
  LFont: string;

procedure CreatePictures(const AStyle: TBoardStyle; const AScale: integer);
procedure FreePictures;

implementation

const
  CPicturesPath = 'images/pieces/%s/%d/';
  CLogName = 'images.log';
  
var
  LLogName: TFileName;
  
procedure Log(const ALine: string);
var
  LLog: TextFile;
  LTime: string;
begin
  Assign(LLog, LLogName);
  if FileExists(LLogName) then
    Append(LLog)
  else
    Rewrite(LLog);
  LTime := DateTimeToStr(Now);
  WriteLn(LLog, LTime, ' ', ALine);
  Close(LLog);
end;

function Interp256(value1, value2, position: integer): integer; inline; overload;
begin
  result := (value1 * (256 - position) + value2 * position) shr 8;
end;

function Interp256(color1, color2: TBGRAPixel; position: integer): TBGRAPixel; inline; overload;
begin
  result.red := Interp256(color1.red, color2.red, position);
  result.green := Interp256(color1.green, color2.green, position);
  result.blue := Interp256(color1.blue, color2.blue, position);
  result.alpha := Interp256(color1.alpha, color2.alpha, position);
end;

function CreateMarbleTexture(tx, ty: integer): TBGRABitmap; overload;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1);
  p := result.Data;
  for i := 0 to result.NbPixels - 1 do
  begin
    colorOscillation := Round(Power((Sin(p^.red * Pi / 80) + 1) / 2, 0.2) * 256);
    p^ := Interp256(BGRA(181, 157, 105), BGRA(228, 227, 180), colorOscillation);
    Inc(p);
  end;
end;

function CreateMarbleTexture(tx, ty: integer; c1, c2: TBGRAPixel): TBGRABitmap; overload;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1);
  p := result.Data;
  for i := 0 to result.NbPixels - 1 do
  begin
    colorOscillation := Round(Power((Sin(p^.red * Pi / 80) + 1) / 2, 0.2) * 256);
    p^ := Interp256(c1, c2, colorOscillation);
    Inc(p);
  end;
end;

function CreateLightMarbleTexture(tx, ty: integer): TBGRABitmap;
begin
  result := CreateMarbleTexture(tx, ty, BGRA(181, 157, 105), BGRA(228, 227, 180));
end;

function CreateDarkMarbleTexture(tx, ty: integer): TBGRABitmap;
begin
  result := CreateMarbleTexture(tx, ty, BGRA(211, 187, 135), BGRA(168, 167, 120));
end;

function CreateChessboard(const AStyle: TBoardStyle; const AScale: integer): TBGRABitmap;
var
  x, y: integer;
  textureClaire, textureFoncee: TBGRABitmap;
  LFileName: string;
begin
  case AStyle of
    bsSimple:
      begin
        result := TBGRABitmap.Create(8 * AScale, 8 * AScale, LLSColor);
        for x := 0 to 7 do for y := 0 to 7 do if Odd(x) xor Odd(y) then
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), LDSColor, dmSet);
      end;
    bsMarbleOriginal, bsMarbleNew, bsMarbleCustom:
      begin
        result := TBGRABitmap.Create(8 * AScale, 8 * AScale);
        if AStyle = bsMarbleOriginal then
        begin
          textureClaire := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee.Negative;
          textureFoncee.InplaceGrayscale;
          textureFoncee.FillRect(0, 0, 8 * (AScale div 5), 8 * (AScale div 5), BGRA(80, 60, 0, 128), dmDrawWithTransparency);
        end else
        if AStyle = bsMarbleNew then
        begin
          textureClaire := CreateLightMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee := CreateDarkMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
        end else
        begin
          textureClaire := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5), LLMColor2, LLMColor);
          textureFoncee := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5), LDMColor2, LDMColor);
        end;
        for x := 0 to 7 do for y := 0 to 7 do if Odd(x) xor Odd(y) then
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), textureFoncee, dmSet)
        else
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), textureClaire, dmSet);
        textureClaire.Free;
        textureFoncee.Free;
      end;
    bsWood:
      begin
        LFileName := Concat(
          ExtractFilePath(ParamStr(0)),
          Format(CPicturesPath, ['wood', AScale]),
          'board.png'
        );
        if FileExists(LFileName) then
          result := TBGRABitmap.Create(LFileName)
        else
        begin
          Log(Format('File not found: %s', [LFileName]));
          result := TBGRABitmap.Create(8 * AScale, 8 * AScale, BGRAWhite);
        end;
      end;
  end;
end;

procedure CreatePictures(const AStyle: TBoardStyle; const AScale: integer);
const
  CColorChars: array[TPieceColorStrict] of char = ('w', 'b');
  CTypeChars: array[TPieceTypeStrict] of char = ('p', 'n', 'b', 'r', 'q', 'k');
 {CChessFontChars: array[TPieceColorStrict, TPieceTypeStrict] of char = (
    ('p', 'n', 'b', 'r', 'q', 'k'),
    ('o', 'm', 'v', 't', 'w', 'l')
  );}
var
  c: TPieceColorStrict;
  t: TPieceTypeStrict;
  LFileName: string;
 {LChar: string;
  LSize: TSize;}
begin
  for c := pcWhite to pcBlack do
    for t := ptPawn to ptKing do
    begin
      LFileName := Concat(
        ExtractFilePath(ParamStr(0)),
        Format(CPicturesPath, [LFont, AScale]),
        CColorChars[c],
        CTypeChars[t],
        '.png'
      );
      
      if FileExists(LFileName) then
        LPieceImage[c, t] := TBGRABitmap.Create(LFileName)
      else
      begin
        Log(Format('File not found: %s', [LFileName])); 
        LPieceImage[c, t] := TBGRABitmap.Create(AScale, AScale, BGRAPixelTransparent);
       {TBGRABitmap.AddFreeTypeFontFolder(GetCurrentDir);
        LChar := CChessFontChars[c, t];
        with LPieceImage[c, t] do
        begin         
          FontName := 'Chess Alfonso-X';
          FontAntialias := TRUE;
          FontHeight := AScale div 2;
          FontStyle :=  [fsBold];
          LSize := TextSize(LChar);
          TextOut((AScale - LSize.cx) / 2, (AScale - LSize.cy) / 2, LChar, BGRABlack);
        end;}
      end;
    end;
  LChessboard := CreateChessboard(AStyle, AScale);
end;

procedure FreePictures;
var
  c: TPieceColorStrict;
  t: TPieceTypeStrict;
begin
  for c in TPieceColorStrict do
    for t in TPieceTypeStrict do
      if Assigned(LPieceImage[c, t]) then
          LPieceImage[c, t].Free;
  LChessboard.Free;
end;

begin
  LLogName := ExtractFilePath(ParamStr(0)) + CLogName;
end.
