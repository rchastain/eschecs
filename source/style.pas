
unit Style;

interface

type
  TBoardStyle = (bsOriginal, bsSimple, bsMarble, bsNew, bsWood);
  TOutlineColor = (ocWhite, ocGreen, ocRed, ocTransparent);
  TStyleData = record
    scale: integer;
    font: string;
    boardstyle: TBoardStyle;
    imgext: string;
  end;
  
var
  gStyle: byte;
  gStyleData: array[0..4] of TStyleData = (
    (scale: 40; font: 'fritz';  boardstyle: bsOriginal; imgext: '.bmp'),
    (scale: 60; font: 'mark';   boardstyle: bsSimple;   imgext: '.png'),
    (scale: 60; font: 'mark';   boardstyle: bsMarble;   imgext: '.png'),
    (scale: 60; font: 'mark';   boardstyle: bsNew;      imgext: '.png'),
    (scale: 80; font: 'wood';   boardstyle: bsWood;     imgext: '.png')
  );
  
implementation

uses
  SysUtils;
  
procedure LoadStyle();
const
  STYLE_FILE = 'style.txt';
var
  vFile: TextFile;
  vByte: byte;
begin
  Assert(FileExists(STYLE_FILE));
 
  AssignFile(vFile, STYLE_FILE);
  Reset(vFile);
  ReadLn(vFile, vByte);
  CloseFile(vFile);
  
  gStyle := vByte;
end;

begin
  LoadStyle();
end.
