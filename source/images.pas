
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
  ChessTypes;

type
  TBoardStyle = (bsSimple, bsMarbleI, bsMarbleII, bsWood);
  TBackColor = (bcGreen, bcRed);
  
var
  LChessboard: TBGRABitmap;
  LPieceImage: array[TPieceColorStrict, TPieceTypeStrict] of TBGRABitmap;
  LBackColors: array[TBackColor] of TBGRAPixel;
  LLSColor, LDSColor: TBGRAPixel;
  LFont: string;

procedure CreatePictures(const AStyle: TBoardStyle; const AScale: integer);
procedure FreePictures;

implementation

const
  CPicturesPath = 'images/pieces/%s/%d';

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
begin
  case AStyle of
    bsSimple:
      begin
        result := TBGRABitmap.Create(8 * AScale, 8 * AScale, LLSColor);
        for x := 0 to 7 do for y := 0 to 7 do if Odd(x) xor Odd(y) then
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), LDSColor, dmSet);
      end;
    bsMarbleI, bsMarbleII:
      begin
        result := TBGRABitmap.Create(8 * AScale, 8 * AScale);
        if AStyle = bsMarbleI then
        begin
          textureClaire := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee := CreateMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee.Negative;
          textureFoncee.InplaceGrayscale;
          textureFoncee.FillRect(0, 0, 8 * (AScale div 5), 8 * (AScale div 5), BGRA(80, 60, 0, 128), dmDrawWithTransparency);
        end else
        begin
          textureClaire := CreateLightMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
          textureFoncee := CreateDarkMarbleTexture(8 * (AScale div 5), 8 * (AScale div 5));
        end;
        for x := 0 to 7 do for y := 0 to 7 do if Odd(x) xor Odd(y) then
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), textureFoncee, dmSet)
        else
          result.FillRect(RectWithSize(x * AScale, y * AScale, AScale, AScale), textureClaire, dmSet);
        textureClaire.Free;
        textureFoncee.Free;
      end;
    bsWood:
      result := TBGRABitmap.Create(Concat(
        ExtractFilePath(ParamStr(0)),
        Format(CPicturesPath, ['wood', AScale]),
        DirectorySeparator,
        'board.png'
      ));
  end;
end;

procedure CreatePictures(const AStyle: TBoardStyle; const AScale: integer);
const
  CColorChars: array[TPieceColorStrict] of char = ('w', 'b');
  CTypeChars: array[TPieceTypeStrict] of char = ('p', 'n', 'b', 'r', 'q', 'k');
var
  c: TPieceColorStrict;
  t: TPieceTypeStrict;
  LFileName: string;
begin
  for c := pcWhite to pcBlack do
    for t := ptPawn to ptKing do
    begin
      LFileName := Concat(
        ExtractFilePath(ParamStr(0)),
        Format(CPicturesPath, [LFont, AScale]),
        DirectorySeparator,
        CColorChars[c],
        CTypeChars[t],
        '.png'
      );
      Assert(FileExists(LFileName));
      LPieceImage[c, t] := TBGRABitmap.Create(LFileName);
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

end.
