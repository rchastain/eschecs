
unit Style;

interface

type
  TBoardStyle = (bsMini, bsOriginal, bsSimple, bsMarble, bsNew, bsWood);
  TOutlineColor = (ocWhite, ocGreen, ocRed, ocTransparent);
  TStyleData = record
    scale: integer;
    font: string;
    boardstyle: TBoardStyle;
    imgext: string;
  end;
  TStyle = 0..5;
  
var
  gStyle: TStyle;
  gStyleData: array[TStyle] of TStyleData = (
    (scale: 30; font: 'usual';  boardstyle: bsSimple;   imgext: '.png'),
    (scale: 40; font: 'fritz';  boardstyle: bsOriginal; imgext: '.bmp'),
    (scale: 60; font: 'mark';   boardstyle: bsSimple;   imgext: '.png'),
    (scale: 60; font: 'mark';   boardstyle: bsMarble;   imgext: '.png'),
    (scale: 60; font: 'mark';   boardstyle: bsNew;      imgext: '.png'),
    (scale: 80; font: 'wood';   boardstyle: bsWood;     imgext: '.png')
  );
  
implementation

end.
