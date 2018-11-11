
unit Images;

interface

uses
  Classes,
  SysUtils,
  Math,
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
  BGRABitmap,
  BGRABitmapTypes,
  BGRAGradients,
  ChessTypes,
  Style;

var
  vChessboard,
  vDarkSquare: TBGRABitmap;
  vPieceImage: array[TChessPieceColor, TChessPieceKind, TOutlineColor] of TBGRABitmap;
  vCurrentStyle: TBoardStyle;
  vSpecialColors: array[ocGreen..ocRed] of record
    r, g, b: byte;
  end;
  
function CreateChessboard(const aBoardStyle: TBoardStyle): TBGRABitmap;

implementation

const
  RUNTIME_COLORS = 'colors.txt';
  PICTURES_FOLDER = 'images\%s\%d';
  
procedure LoadColors();
var
  vFile: TextFile;
  vOutline: TOutlineColor;
  vByte: byte;
begin
  Assert(FileExists(RUNTIME_COLORS));
  AssignFile(vFile, RUNTIME_COLORS);
  Reset(vFile);
  for vOutline := ocGreen to ocRed do
  begin
    ReadLn(vFile, vByte);
    vSpecialColors[vOutline].r := vByte;
    ReadLn(vFile, vByte);
    vSpecialColors[vOutline].g := vByte;
    ReadLn(vFile, vByte);
    vSpecialColors[vOutline].b := vByte;
  end;
  CloseFile(vFile);
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
  i: Integer;
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

function CreateMarbleTexture(tx,ty: integer; c1, c2: TBGRAPixel): TBGRABitmap; overload;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(power((sin(p^.red*Pi/80)+1)/2,0.2)*256);
    p^ := Interp256(c1,c2,colorOscillation);
    inc(p);
  end;
end;

function CreateLightMarbleTexture(tx,ty: integer): TBGRABitmap;
begin
  result := CreateMarbleTexture(tx,ty,BGRA(181,157,105),BGRA(228,227,180));
end;
 
function CreateDarkMarbleTexture(tx,ty: integer): TBGRABitmap;
begin
  result := CreateMarbleTexture(tx,ty,BGRA(211,187,135),BGRA(168,167,120));
end;

function CreateChessboard(const aBoardStyle: TBoardStyle): TBGRABitmap;
var
  x, y: integer;
  textureClaire, textureFoncee: TBGRABitmap;
begin
{$IFDEF DEBUG}
  WriteLn(Format('CreateChessboard(%d)', [Ord(aBoardStyle)]));
{$ENDIF}
  case aBoardStyle of
    bsOriginal:
      begin
        result := TBGRABitmap.Create(8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale, CSSWhite);
        for x := 0 to 7 do
          for y := 0 to 7 do
            if Odd(x) xor Odd(y) then
              result.PutImage(x * gStyleData[gStyle].scale, y * gStyleData[gStyle].scale, vDarkSquare, dmDrawWithTransparency);
      end;
    bsMarble1, bsMarble2:
      begin
        result := TBGRABitmap.Create(8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale);
        
        if aBoardStyle = bsMarble1 then
        begin
          textureClaire := CreateMarbleTexture(8 * (gStyleData[gStyle].scale div 5), 8 * (gStyleData[gStyle].scale div 5));
          textureFoncee := CreateMarbleTexture(8 * (gStyleData[gStyle].scale div 5), 8 * (gStyleData[gStyle].scale div 5));
          textureFoncee.Negative;
          textureFoncee.InplaceGrayscale;
          textureFoncee.FillRect(0, 0, 8 * (gStyleData[gStyle].scale div 5), 8 * (gStyleData[gStyle].scale div 5), BGRA(80, 60, 0, 128), dmDrawWithTransparency);
        end else
        begin
          textureClaire := CreateLightMarbleTexture(8 * (gStyleData[gStyle].scale div 5), 8 * (gStyleData[gStyle].scale div 5));
          textureFoncee := CreateDarkMarbleTexture(8 * (gStyleData[gStyle].scale div 5), 8 * (gStyleData[gStyle].scale div 5));
        end;
        
        for x := 0 to 7 do
          for y := 0 to 7 do
            if Odd(x) xor Odd(y) then
              result.FillRect(RectWithSize(x * gStyleData[gStyle].scale, y * gStyleData[gStyle].scale, gStyleData[gStyle].scale, gStyleData[gStyle].scale), textureFoncee, dmSet)
            else
              result.FillRect(RectWithSize(x * gStyleData[gStyle].scale, y * gStyleData[gStyle].scale, gStyleData[gStyle].scale, gStyleData[gStyle].scale), textureClaire, dmSet);
        textureClaire.Free;
        textureFoncee.Free;
      end;
  end;
  vCurrentStyle := aBoardStyle;
end;

procedure FreePictures();
var
  c: TChessPieceColor;
  k: TChessPieceKind;
  o: TOutlineColor;
begin
  for c := cpcWhite to cpcBlack do
    for k := cpkPawn to cpkKing do
      for o := ocWhite to ocTransparent do
        if Assigned(vPieceImage[c, k, o]) then
          vPieceImage[c, k, o].Free;
  vDarkSquare.Free;
  vChessboard.Free;
end;

procedure CreatePictures();
const
  COLORCHARS: array[TChessPieceColor] of char = ('w', 'b');
  TYPECHARS: array[TChessPieceKind] of char = ('p', 'n', 'b', 'r', 'q', 'k');
var
  c: TChessPieceColor;
  k: TChessPieceKind;
  s: string;
  d: TDateTime;
begin
{$IFDEF DEBUG}
  WriteLn('CreatePictures()');
{$ENDIF}
  d := Now;
  LoadColors();
  for c := cpcWhite to cpcBlack do
    for k := cpkPawn to cpkKing do
    begin
      s := Concat(
        ExtractFilePath(ParamStr(0)),
        Format(PICTURES_FOLDER, [gStyleData[gStyle].font, gStyleData[gStyle].scale]),
        '\',
        COLORCHARS[c],
        TYPECHARS[k],
        gStyleData[gStyle].imgext
      );
      Assert(FileExists(s));
      vPieceImage[c, k, ocWhite] := TBGRABitmap.Create(s);
      vPieceImage[c, k, ocWhite].ReplaceColor(CSSMidnightBlue, BGRAPixelTransparent);
      vPieceImage[c, k, ocGreen] := TBGRABitmap.Create(vPieceImage[c, k, ocWhite]);
      with vSpecialColors[ocGreen] do vPieceImage[c, k, ocGreen].ReplaceColor(CSSGray, BGRA(r, g, b));
      if (k = cpkKing) then
      begin
        vPieceImage[c, k, ocRed] := TBGRABitmap.Create(vPieceImage[c, k, ocWhite]);
        with vSpecialColors[ocRed] do vPieceImage[c, k, ocRed].ReplaceColor(CSSGray, BGRA(r, g, b));
      end;
      vPieceImage[c, k, ocTransparent] := TBGRABitmap.Create(vPieceImage[c, k, ocWhite]);
      vPieceImage[c, k, ocTransparent].ReplaceColor(CSSGray, BGRAPixelTransparent);
      vPieceImage[c, k, ocWhite].ReplaceColor(CSSGray, CSSWhite);
    end;
  
  s := ExtractFilePath(ParamStr(0)) + Format(PICTURES_FOLDER, [gStyleData[gStyle].font, gStyleData[gStyle].scale]) + '\ds.bmp';
  Assert(FileExists(s));
  vDarkSquare := TBGRABitmap.Create(s);
  vDarkSquare.ReplaceColor(CSSMidnightBlue, BGRAPixelTransparent);
  
  vChessboard := CreateChessboard(gStyleData[gStyle].boardstyle);
  d := Now - d;
{$IFDEF DEBUG}
  WriteLn(Format('Création des images en %d ms.', [Trunc(1000 * SECSPERDAY * d)]));
{$ENDIF}
end;

initialization
  CreatePictures();
  
finalization
  FreePictures;

end.
