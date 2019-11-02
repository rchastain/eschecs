
unit frmPromotion;

interface

uses
  SysUtils, Classes,
  
  fpg_base,
  fpg_main,
  //fpg_widget,
  fpg_form,
  fpg_button,
  
  ChessTypes, Language;

function SelectPieceType: TPieceType;

implementation

type
  TPromotionForm = class(TfpgForm)
  private
    FInt: integer;
    btKnight: TfpgButton;
    btBishop: TfpgButton;
    btRook: TfpgButton;
    btQueen: TfpgButton;
    procedure SetResult(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;
  
function SelectPieceType: TPieceType;
var
  LForm : TPromotionForm;
begin
  fpgApplication.CreateForm(TPromotionForm, LForm);
  with LForm do
  try
    ShowModal;
    result := TPieceType(FInt);
  finally
    Free;
  end;
end;

procedure TPromotionForm.SetResult(Sender: TObject);
begin
  FInt := TfpgButton(Sender).TabOrder;
  Close;
end;

procedure TPromotionForm.AfterCreate;
begin
  Name := 'PromotionForm';
  SetPosition(100, 100, 100 + 2 * 5, 4 * 24 + 5 * 5);
  WindowTitle := 'Sélection de la pièce';
  Hint := '';
  IconName := 'vfd.eschecs';
  WindowPosition := wpOneThirdDown;
  //WindowPosition := wpScreenCenter;

  btKnight := TfpgButton.Create(self);
  with btKnight do
  begin
    Name := 'btKnight';
    SetPosition(5, 0 * (24 + 5) + 5, 100, 24);
    Text := GetText(txKnight);
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @SetResult;
  end;

  btBishop := TfpgButton.Create(self);
  with btBishop do
  begin
    Name := 'btBishop';
    SetPosition(5, 1 * (24 + 5) + 5, 100, 24);
    Text := GetText(txBishop);
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @SetResult;
  end;

  btRook := TfpgButton.Create(self);
  with btRook do
  begin
    Name := 'btRook';
    SetPosition(5, 2 * (24 + 5) + 5, 100, 24);
    Text := GetText(txRook);
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @SetResult;
  end;

  btQueen := TfpgButton.Create(self);
  with btQueen do
  begin
    Name := 'btQueen';
    SetPosition(5, 3 * (24 + 5) + 5, 100, 24);
    Text := GetText(txQueen);
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @SetResult;
  end;
  
  FInt := 0;
end;

end.
