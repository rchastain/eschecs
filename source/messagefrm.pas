unit messagefrm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, fpg_base, fpg_main,
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
    procedure ShowMessageFrm(AMessage1, AMessage2, ATitle : string);
    procedure closemsg(sender : Tobject);
  end;

{@VFD_NEWFORM_DECL}

{$I icon.inc} 

implementation

{@VFD_NEWFORM_IMPL}
procedure Tmessagefrm.closemsg(sender : Tobject);
begin
close;
end;

procedure Tmessagefrm.ShowMessageFrm(AMessage1, AMessage2, ATitle : string);
var
mwidth: integer;
begin
label1.text := AMessage1;
label2.text := AMessage2;

WindowTitle := ATitle;

if label1.width > label2.width then
mwidth := label1.width else mwidth := label2.width;

width := mwidth + 40;

if AMessage2 = '' then label1.top := 20 else
label1.top := 12;

label1.left := (width - label1.width) div 2;
label2.left := (width - label2.width) div 2;
button1.left := (width - button1.width) div 2;
end;

procedure Tmessagefrm.AfterCreate;
begin
 fpgImages.AddMaskedBMP('vfd.eschecs', @vfd_eschecs, sizeof(vfd_eschecs), 0, 0);
 
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
    onclick := @closemsg;
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
