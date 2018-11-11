{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRAbitmap4fpGUI;

{$warn 5023 off : no warning about unused units}
interface

uses
  BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, BGRACanvas, 
  BGRACanvas2D, BGRAColorInt, BGRACompressableBitmap, BGRACoordPool3D, 
  BGRADefaultBitmap, BGRADNetDeserial, BGRAFillInfo, BGRAFilters, 
  BGRAGradients, BGRAGradientScanner, BGRALayers, BGRAMatrix3D, 
  BGRAOpenRaster, BGRAPaintNet, BGRAPath, BGRAPen, BGRAPhongTypes, 
  BGRAPolygon, BGRAPolygonAliased, BGRAResample, BGRAScene3D, 
  BGRASliceScaling, BGRASSE, BGRAStreamLayers, BGRATransform, 
  BGRAGrayscaleMask, BGRAReadBMP, BGRAReadGif, BGRAReadPCX, BGRAReadPng, 
  BGRAReadPSD, BGRAThumbnail, BGRAReadTGA, BGRAReadJpeg, BGRAReadLzp, 
  UnzipperExt, BGRALzpCommon, BGRAWriteLzp, BGRAReadXPM, BGRAUnits, 
  BGRAReadBmpMioMap, BGRAArrow, BGRAGraphics, BGRAUTF8, BGRAfpGUIBitmap, 
  BGRATypewriter, BGRASVG, BGRASVGShapes, BGRASVGType, BGRAPalette, 
  BGRAColorQuantization, BGRADithering, BGRAFreeType, BGRACustomTextFX, 
  BGRAWritePNG, BGRAGifFormat, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pl_BGRAbitmap4fpGUI', @Register);
end.
