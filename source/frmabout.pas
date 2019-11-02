
unit FrmAbout;

{$IFDEF UNIX}
{$DEFINE USECTHREADS}
{$ENDIF}

interface

uses
{$IFDEF UNIX}
  CThreads,
  CWString,
{$ENDIF}
  SysUtils,
  Classes,
  Math,
  
  fpg_base,
  fpg_button,
  fpg_form,
  fpg_hyperlink,
  fpg_label,
  fpg_main,
  
  Language;

procedure ShowAboutForm(const AMsg1, AMsg2, ATitle, AButton, AWebsite: string);

implementation

{$I icon.inc} 

type
  TABoutForm = class(TfpgForm)
  public
    Button1: TfpgButton;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Hyperlink1: TfpgHyperlink;
    procedure AfterCreate; override;
    procedure CloseMsg(Sender: TObject);
  end;

procedure ShowAboutForm(const AMsg1, AMsg2, ATitle, AButton, AWebsite: string);
var
  LForm : TABoutForm;
begin
  fpgApplication.CreateForm(TABoutForm, LForm);
  with LForm do
  try
    Button1.Text := AButton;
    Label1.Text := AMsg1;
    Label2.Text := AMsg2;
    WindowTitle := ATitle;
    Width := Max(Label1.Width, Label2.Width) + 40;
    Label1.Left := (Width - Label1.Width) div 2;
    Label2.Left := (Width - Label2.Width) div 2;
    if AWebsite <> '' then
    begin
      Hyperlink1.Visible := TRUE;
      Hyperlink1.Text := AWebsite;
      Hyperlink1.Left := (Width - Hyperlink1.Width) div 2;
      Button1.Top := Button1.Top + 24;
      Height := Height + 24;
    end;
    Button1.Left := (Width - Button1.Width) div 2;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TABoutForm.AfterCreate;
begin
  Name := 'AboutForm';
  SetPosition(443, 310, 427, 96);
  WindowTitle := '';
  IconName := 'vfd.eschecs';
  BackgroundColor := $80000001;
  Sizeable := FALSE;
  Visible := FALSE;
  Hint := '';
  WindowPosition := wpOneThirdDown;
  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(168, 60, 83, 26);
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := mrOK;
    ParentShowHint := FALSE;
    TabOrder := 1;
  end;
  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(16, 12, 91, 18);
    Alignment := taCenter;
    AutoSize := TRUE;
    FontDesc := '#Label1';
    ParentShowHint := FALSE;
    Text := '';
    Hint := '';
  end;
  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(16, 36, 105, 18);
    Alignment := taCenter;
    AutoSize := TRUE;
    FontDesc := '#Label1';
    ParentShowHint := FALSE;
    Text := '';
    Hint := '';
  end;
  Hyperlink1 := TfpgHyperlink.Create(self);
  with Hyperlink1 do
  begin
    Name := 'Hyperlink1';
    SetPosition(16, 60, 120, 18);
    Alignment := taCenter;
    AutoSize := TRUE;
    FontDesc := 'Liberation Sans-10:antialias=TRUE:bold';
    HotTrackColor := TfpgColor($B1004001);
    HotTrackFont := 'Liberation Sans-10:antialias=TRUE:underline:bold';
    ParentShowHint := FALSE;
    Text := '';
    TextColor := TfpgColor($B1004001);
    URL := 'https://github.com/rchastain/eschecs';
    Visible := FALSE;
    Hint := '';
  end;
end;

procedure TABoutForm.CloseMsg(Sender: TObject);
begin
  Close;
end;

end.
