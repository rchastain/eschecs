unit messagefrm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fpg_base, fpg_main, language,
  {%units 'Auto-generated GUI code'}
  fpg_form, fpg_button, fpg_label, fpg_hyperlink
  {%endunits}
  ;

type

  Tmessagefrm = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: messagefrm}
    Button1: TfpgButton;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Hyperlink1: TfpgHyperlink;
    {@VFD_HEAD_END: messagefrm}
    procedure AfterCreate; override;
    procedure closemsg(sender : Tobject);
  end;

{@VFD_NEWFORM_DECL}

{$define read_interface}
{$undef read_implementation}

 procedure ShowMessageFrm(AMessage1, AMessage2, ATitle, AButton, AWebsite : string);

{$I icon.inc} 

implementation

{@VFD_NEWFORM_IMPL}
procedure Tmessagefrm.closemsg(sender : Tobject);
begin
close;
end;

procedure ShowMessageFrm(AMessage1, AMessage2, ATitle, AButton, AWebsite : string);
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

if AWebsite <> '' then
begin
msgfrm.Hyperlink1.text := Awebsite;
msgfrm.Hyperlink1.left := (msgfrm.width - msgfrm.Hyperlink1.width) div 2;
msgfrm.Hyperlink1.visible := true;
msgfrm.Button1.top := msgfrm.Button1.top + 20;
msgfrm.height := msgfrm.height +20;
end;

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
  SetPosition(443, 310, 427, 84);
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
    SetPosition(168, 54, 83, 26);
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := mrOK;
    ParentShowHint := False;
    TabOrder := 1;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(16, 12, 91, 15);
    Alignment := taCenter;
    AutoSize := True;
    FontDesc := '#Label1';
    ParentShowHint := False;
    Text := 'ddddddddddddd';
    Hint := '';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(16, 32, 105, 15);
    Alignment := taCenter;
    AutoSize := True;
    FontDesc := '#Label1';
    ParentShowHint := False;
    Text := 'xxxxxxxxxxxxxxx';
    Hint := '';
  end;

  Hyperlink1 := TfpgHyperlink.Create(self);
  with Hyperlink1 do
  begin
    Name := 'Hyperlink1';
    SetPosition(16, 50, 120, 15);
    FontDesc := 'Liberation Sans-8:antialias=true:underline';
    HotTrackColor := TfpgColor($B11DBC1D);
    HotTrackFont := 'Liberation Sans-8:antialias=true:underline:bold';
    ParentShowHint := False;
    Text := 'Eschecs GitHub web site';
    TextColor := TfpgColor($B1004001);
    URL := 'https://github.com/rchastain/eschecs';
    Visible := False;
    Hint := '';
  end;

  {@VFD_BODY_END: messagefrm}
  {%endregion}
end;

end.
