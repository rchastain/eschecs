
unit frmOptions;

interface

uses
  SysUtils,
  Classes,
  Process,
  TypInfo,
  
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_form,
  fpg_label,
  fpg_edit ,
  fpg_button,
  fpg_combobox,
  fpg_dialogs,
  fpg_checkbox,
  
  BGRABitmapTypes,
  
  Language,
  Images,
  Settings;

type
  TConfigForm = class(TfpgForm)
  private
    lbFont: TfpgLabel;
    cbFont: TfpgComboBox;
    lbSize: TfpgLabel;
    cbSize: TfpgComboBox;
    lbStyle: TfpgLabel;
    cbStyle: TfpgComboBox;
    lbLang: TfpgLabel;
    cbLang: TfpgComboBox;
    lbTime: TfpgLabel;
    edTime: TfpgEdit;
    ckColoring: TfpgCheckBox;
    ckChess960: TfpgCheckBox;
    btDelete: TfpgButton;
    btStart: TfpgButton;
    btQuit: TfpgButton;
    procedure cbFontChange(Sender: TObject);
    procedure ckChess960Change(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btQuitClick(Sender: TObject);
    procedure PopulateFont;
    procedure PopulateSize;
    procedure PopulateLang;
    procedure PopulateStyle;
  public
    procedure AfterCreate; override;
  end;

implementation

var
  LCurrPos: string;
  LAuto, LUpsideDown: boolean;
  LStyle: TBoardStyle;
  LMoveHist: string;
  LPosIndex, LEngIndex: integer;
  LLSColor, LDSColor, LGreen, LRed: TBGRAPixel;
  LMoveTime: integer;
  LFont: string;
  LLang: TLanguage;
  LColoring: boolean;
  LScale: integer;
  LChess960: boolean;
  
function Capitalize(const AStr: string): string;
begin
  if Length(AStr) = 0 then
    result := ''
  else
    result := Concat(
      UpCase(AStr[1]),
      Copy(AStr, 2)
    );
end;

procedure TConfigForm.cbFontChange(Sender: TObject);
var
  i: integer;
begin
  PopulateSize;
  for i := 0 to Pred(cbSize.Items.Count) do
    if cbSize.Items[i] = IntToStr(LScale) then
      cbSize.FocusItem := i;
end;

procedure TConfigForm.ckChess960Change(Sender: TObject);
begin
  LCurrPos := CDefaultPosition[ckChess960.Checked];
end;

procedure TConfigForm.btDeleteClick(Sender: TObject);
begin
  LPosIndex := 0;
  LCurrPos := CDefaultPosition[ckChess960.Checked];
  DeleteFile(Concat(ExtractFilePath(ParamStr(0)), 'config/eschecs.fen'));
  ckChess960.Enabled := TRUE;
  btDelete.Enabled := FALSE;
end;

procedure TConfigForm.btStartClick (Sender: TObject );
var
  p: TProcess;
begin
  LChess960 := ckChess960.Checked;
  LColoring := ckColoring.Checked;
  LFont := LowerCase(cbFont.Text);
  LLang := TLanguage(cbLang.FocusItem);
  LMoveTime := StrToIntDef(edTime.Text, 1000);
  LScale := StrToIntDef(cbSize.Text, 50);
  LStyle := TBoardStyle(cbStyle.FocusItem);
  
  SaveSettings(
    LCurrPos,
    LAuto,
    LUpsideDown,
    LStyle,
    LMoveHist,
    LPosIndex,
    LEngIndex,
    LLSColor,
    LDSColor,
    LGreen,
    LRed,
    LMoveTime,
    LFont,
    LLang,
    LColoring,
    LScale,
    LChess960
  );
  
  p := TProcess.Create(nil);
  p.Executable := 'eschecs';
  p.Execute;
  p.Free;
  btQuitClick(nil);
end;

procedure TConfigForm.btQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TConfigForm.PopulateFont;
var
  LRec: TSearchRec;
  LList: TStringList;
  LIndex: integer;
begin
  cbFont.Items.Clear;
  LList := TStringList.Create;
  LList.Sorted := TRUE;
  if FindFirst(
    Concat(ExtractFilePath(ParamStr(0)), 'images/pieces/*'),
    faAnyFile or faDirectory,
    LRec
  ) = 0 then
  repeat
    with LRec do
      if ((Attr and faDirectory) = faDirectory) and (Name <> '.') and (Name <> '..') then
        LList.Append(Capitalize(Name));
  until FindNext(LRec) <> 0;
  FindClose(LRec);
  for LIndex := 0 to LList.Count - 1 do
    cbFont.Items.Add(LLIst[LIndex]);
  LList.Free;
end;

procedure TConfigForm.PopulateSize;
var
  LRec: TSearchRec;
  LList: TStringList;
  LIndex: integer;
begin
  cbSize.Items.Clear;
  LList := TStringList.Create;
  LList.Sorted := TRUE;
  if FindFirst(
    Concat(ExtractFilePath(ParamStr(0)), 'images/pieces/', LowerCase(cbFont.Text), '/*'),
    faAnyFile or faDirectory,
    LRec
  ) = 0 then
  repeat
    with LRec do
      if ((Attr and faDirectory) = faDirectory) and (Name <> '.') and (Name <> '..') then
        LList.Append(Name);
  until FindNext(LRec) <> 0;
  FindClose(LRec);
  for LIndex := 0 to LList.Count - 1 do
    cbSize.Items.Add(LLIst[LIndex]);
  LList.Free;
end;

procedure TConfigForm.PopulateLang;
var
  LLang: TLanguage;
  LName: string;
begin
  cbLang.Items.Clear;
  for LLang := Low(TLanguage) to High(TLanguage) do
  begin
    LName := Copy(GetEnumName(TypeInfo(TLanguage), Ord(LLang)), 3);
    cbLang.Items.Add(LName);
  end;
end;

procedure TConfigForm.PopulateStyle;
var
  LStyle: TBoardStyle;
  LName: string;
begin
  cbStyle.Items.Clear;
  for LStyle := Low(TBoardStyle) to High(TBoardStyle) do
  begin
    LName := Copy(GetEnumName(TypeInfo(TBoardStyle), Ord(LStyle)), 3);
    cbStyle.Items.Add(LName);
  end;
end;

procedure TConfigForm.AfterCreate;
var
  i: integer;
begin
  Name := 'MainForm';
  SetPosition(300, 200, 170, 377);
  WindowTitle := 'Eschecs Options';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  
  lbFont := TfpgLabel.Create(self);
  with lbFont do
  begin
    Name := 'lbFont';
    SetPosition(10, 10, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Pieces set';
  end;
  
  cbFont := TfpgComboBox.Create(self);
  with cbFont do
  begin
    Name := 'cbFont';
    SetPosition(10, 28, 150, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 0;
    OnChange := @cbFontChange;
  end;

  lbSize := TfpgLabel.Create(self);
  with lbSize do
  begin
    Name := 'lbSize';
    SetPosition(10, 55, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Pieces size';
  end;
  
  cbSize := TfpgComboBox.Create(self);
  with cbSize do
  begin
    Name := 'cbSize';
    SetPosition(10, 73, 150, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
  end;

  lbStyle := TfpgLabel.Create(self);
  with lbStyle do
  begin
    Name := 'lbStyle';
    SetPosition(10, 100, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Chessboard style';
  end;
  
  cbStyle := TfpgComboBox.Create(self);
  with cbStyle do
  begin
    Name := 'cbStyle';
    SetPosition(10, 118, 150, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 2;
  end;

  lbLang := TfpgLabel.Create(self);
  with lbLang do
  begin
    Name := 'lbLang';
    SetPosition(10, 145, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Language';
  end;
  
  cbLang := TfpgComboBox.Create(self);
  with cbLang do
  begin
    Name := 'cbLang';
    SetPosition(10, 163, 150, 22);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 3;
  end;

  lbTime := TfpgLabel.Create(self);
  with lbTime do
  begin
    Name := 'lbTime';
    SetPosition(10, 190, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Move time';
  end;

  edTime := TfpgEdit.Create(self);
  with edTime do
  begin
    Name := 'edTime';
    SetPosition(10, 208, 150, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := 'Time available for computer move, in milliseconds.';
    ShowHint := TRUE;
    TabOrder := 4;
    Text := '500';
  end; 

  ckColoring := TfpgCheckBox.Create(self);
  with ckColoring do
  begin
    Name := 'ckColoring';
    SetPosition(10, 235, 150, 20);
    FontDesc := '#Label1';
    Hint := 'Coloring of last move squares.';
    ShowHint := TRUE;
    TabOrder := 5;
    Text := 'Color last move';
  end;

  ckChess960 := TfpgCheckBox.Create(self);
  with ckChess960 do
  begin
    Name := 'ckChess960';
    SetPosition(10, 260, 150, 20);
    FontDesc := '#Label1';
    Hint := 'Fischer random chess.';
    ShowHint := TRUE;
    TabOrder := 6;
    Text := 'Chess 960';
    OnChange := @ckChess960Change;
  end;

  btDelete := TfpgButton.Create(self);
  with btDelete do
  begin
    Name := 'btDelete';
    SetPosition(10, 285, 150, 24);
    Text := 'Delete current game';
    FontDesc := '#Label1';
    Hint := 'Required to activate or deactivate chess 960.';
    ShowHint := TRUE;
    ImageName := '';
    TabOrder := 7;
    OnClick := @btDeleteClick;
  end;

  btStart := TfpgButton.Create(self);
  with btStart do
  begin
    Name := 'btStart';
    SetPosition(10, 314, 150, 24);
    Text := 'Start Eschecs';
    FontDesc := '#Label1';
    Hint := 'Starts Eschecs and closes this application.';
    ShowHint := TRUE;
    ImageName := '';
    TabOrder := 8;
    OnClick := @btStartClick;
  end;
  
  btQuit := TfpgButton.Create(self);
  with btQuit do
  begin
    Name := 'btQuit';
    SetPosition(10, 343, 150, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 9;
    OnClick := @btQuitClick;
  end;
  
  PopulateFont;
  PopulateStyle;
  PopulateLang;
  
  LoadSettings(
    LCurrPos,
    LAuto,
    LUpsideDown,
    LStyle,
    LMoveHist,
    LPosIndex,
    LEngIndex,
    LLSColor,
    LDSColor,
    LGreen,
    LRed,
    LMoveTime,
    LFont,
    LLang,
    LColoring,
    LScale,
    LChess960
  );
  
  for i := 0 to Pred(cbFont.Items.Count) do
    if UpperCase(cbFont.Items[i]) = UpperCase(LFont) then
    begin
      cbFont.FocusItem := i;
      cbFontChange(nil);
    end;
  (*
  for i := 0 to Pred(cbSize.Items.Count) do
    if cbSize.Items[i] = IntToStr(LScale) then
      cbSize.FocusItem := i;
  *)
  cbStyle.FocusItem := Ord(LStyle);
  cbLang.FocusItem := Ord(LLang);
  edTime.Text := IntToStr(LMoveTime);
  ckColoring.Checked := LColoring;
  ckChess960.Checked := LChess960;
  ckChess960.Enabled := (LPosIndex = 0) and not FileExists(Concat(ExtractFilePath(ParamStr(0)), 'config/eschecs.fen'));
  btDelete.Enabled := not ckChess960.Enabled;
end;

end.
