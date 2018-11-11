
unit Style;

interface

type
  TBoardStyle = (bsOriginal, bsMarble1, bsMarble2, bsWood, bsMetal);
  TOutlineColor = (ocWhite, ocGreen, ocRed, ocTransparent);
  TPieceStyle = (psOriginal, psCondal60, psMark60);
  TStyleData = record
    scale: integer;
    font: string;
    boardstyle: TBoardStyle;
    imgext: string;
  end;
  
var
  gStyle: TPieceStyle;
  gStyleData: array[TPieceStyle] of TStyleData = (
    (scale: 40; font: 'fritz'; boardstyle: bsOriginal; imgext: '.bmp'),
    (scale: 60; font: 'condal'; boardstyle: bsMarble1; imgext: '.png'),
    (scale: 60; font: 'mark'; boardstyle: bsMarble2; imgext: '.png')
  );
  
implementation

begin
  gStyle := psMark60;
end.
