unit messagefrm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fpg_base, fpg_main, language,
  {%units 'Auto-generated GUI code'}
  fpg_form, fpg_button, fpg_label
  {%endunits}
  ;

type

  Tmessagefrm = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: messagefrm}
    Button1: TfpgButton;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    {@VFD_HEAD_END: messagefrm}
    procedure AfterCreate; override;
    procedure closemsg(sender : Tobject);
  end;

{@VFD_NEWFORM_DECL}

{$define read_interface}
{$undef read_implementation}

 procedure ShowMessageFrm(AMessage1, AMessage2, ATitle, AButton : string);

{$I icon.inc} 

implementation

{@VFD_NEWFORM_IMPL}
procedure Tmessagefrm.closemsg(sender : Tobject);
begin
close;
end;

procedure ShowMessageFrm(AMessage1, AMessage2, ATitle, AButton : string);
var
mwidth: integer;
msgfrm : Tmessagefrm;
begin
 fpgApplication.CreateForm(Tmessagefrm, msgfrm);

  try
    msgfrm.Button1.text := Abutton;
    msgfrm.label1.text := AMessage1;
    msgfrm.label2.text := AMessage2;

    msgfrm.WindowTitle := ATitle;

if msgfrm.label1.width > msgfrm.label2.width then
mwidth := msgfrm.label1.width else mwidth := msgfrm.label2.width;

msgfrm.width := mwidth + 40;

if AMessage2 = '' then msgfrm.label1.top := 20 else
msgfrm.label1.top := 12;

msgfrm.label1.left := (msgfrm.width - msgfrm.label1.width) div 2;
msgfrm.label2.left := (msgfrm.width - msgfrm.label2.width) div 2;
msgfrm.button1.left := (msgfrm.width - msgfrm.button1.width) div 2;

msgfrm.ShowModal;

  finally
msgfrm.Free;
  end;
end;

procedure Tmessagefrm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: messagefrm}
  Name := 'messagefrm';
  SetPosition(359, 541, 427, 84);
  WindowTitle := '';
  IconName := 'vfd.eschecs';
  BackGroundColor := $80000001;
  Sizeable := False;
  Visible := False;
  Hint := '';
  WindowPosition := wpOneThirdDown;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(176, 56, 80, 26);
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := '';
    ParentShowHint := False;
    TabOrder := 1;
    ModalResult := mrOK;
   end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(16, 12, 2, 15);
    Alignment := taCenter;
    AutoSize := True;
    FontDesc := '#Label1';
    ParentShowHint := False;
    Text := '';
    Hint := '';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(16, 32, 2, 15);
    Alignment := taCenter;
    AutoSize := True;
    FontDesc := '#Label1';
    ParentShowHint := False;
    Text := '';
    Hint := '';
  end;

  {@VFD_BODY_END: messagefrm}
  {%endregion}
end;

end.
