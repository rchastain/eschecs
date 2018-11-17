
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
    name: string;
  end;
  TStyle = 0..4;
  
var
  gStyle: TStyle;
  gStyleData: array[TStyle] of TStyleData = (
    (scale: 40; font: 'fritz';  boardstyle: bsOriginal; imgext: '.bmp'; name: 'Chessboard 320 Original'),
    (scale: 60; font: 'mark';   boardstyle: bsSimple;   imgext: '.png'; name: 'Chessboard 480 Simple'),
    (scale: 60; font: 'mark';   boardstyle: bsMarble;   imgext: '.png'; name: 'Chessboard 480 Marble'),
    (scale: 60; font: 'mark';   boardstyle: bsNew;      imgext: '.png'; name: 'Chessboard 480 New'),
    (scale: 80; font: 'wood';   boardstyle: bsWood;     imgext: '.png'; name: 'Chessboard 640 Wood')
  );
  
implementation

end.
