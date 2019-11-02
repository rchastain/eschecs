
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
  
  Language,
  Images;

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
    btStart: TfpgButton;
    btQuit: TfpgButton;
    procedure cbFontChange(Sender: TObject);
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
begin
  PopulateSize;
  PopulateStyle;
end;

procedure TConfigForm.btStartClick (Sender: TObject );
const
  CBoolStr: array[boolean] of string = ('false', 'true');
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  p.Executable := 'eschecs';
  p.Parameters.Add(Format('/chess960=%s', [CBoolStr[ckChess960.Checked]]));
  p.Parameters.Add(Format('/coloring=%s', [CBoolStr[ckColoring.Checked]]));
  p.Parameters.Add(Format('/font=%s', [LowerCase(cbFont.Text)]));
  p.Parameters.Add(Format('/language=%d', [cbLang.FocusItem]));
  p.Parameters.Add(Format('/movetime=%s', [edTime.Text]));
  p.Parameters.Add(Format('/scale=%s', [cbSize.Text]));
  p.Parameters.Add(Format('/style=%d', [cbStyle.FocusItem]));
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
  
  cbFont.FocusItem := 0;
  cbFont.OnChange := @cbFontChange;
  cbFontChange(nil);
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
    Concat(ExtractFilePath(ParamStr(0)), 'images/pieces/', LowerCase(cbFont.Text),'/*'),
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
  
  cbSize.FocusItem := 0;
  //cbSize.OnChange := @cbSizeChange;
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
  cbLang.FocusItem := 0;
  //cbLang.OnChange := @cbLangChange;
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
  cbStyle.FocusItem := 0;
  //cbLang.OnChange := @cbLangChange;
end;

procedure TConfigForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(300, 200, 170, 361);
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
    SetPosition(10, 29, 150, 22);
    FontDesc := '#List';
    Hint := '';
    //TabOrder := 0;
  end;

  lbSize := TfpgLabel.Create(self);
  with lbSize do
  begin
    Name := 'lbSize';
    SetPosition(10, 57, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Pieces size';
  end;
  
  cbSize := TfpgComboBox.Create(self);
  with cbSize do
  begin
    Name := 'cbSize';
    SetPosition(10, 76, 150, 22);
    FontDesc := '#List';
    Hint := '';
    //TabOrder := 0;
  end;

  lbStyle := TfpgLabel.Create(self);
  with lbStyle do
  begin
    Name := 'lbStyle';
    SetPosition(10, 104, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Chessboard style';
  end;
  
  cbStyle := TfpgComboBox.Create(self);
  with cbStyle do
  begin
    Name := 'cbStyle';
    SetPosition(10, 123, 150, 22);
    FontDesc := '#List';
    Hint := '';
    //TabOrder := 0;
  end;

  lbLang := TfpgLabel.Create(self);
  with lbLang do
  begin
    Name := 'lbLang';
    SetPosition(10, 151, 150, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Language';
  end;
  
  cbLang := TfpgComboBox.Create(self);
  with cbLang do
  begin
    Name := 'cbLang';
    SetPosition(10, 170, 150, 22);
    FontDesc := '#List';
    Hint := '';
    //TabOrder := 0;
  end;

  lbTime := TfpgLabel.Create(self);
  with lbTime do
  begin
    Name := 'lbTime';
    SetPosition(10, 198, 150, 16);
    FontDesc := '#Label1';
    Hint := 'Time available for computer move (in milliseconds)';
    Text := 'Move time';
  end;

  edTime := TfpgEdit.Create(self);
  with edTime do
  begin
    Name := 'edTime';
    SetPosition(10, 217, 150, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    //TabOrder := 1;
    Text := '500';
  end; 

  ckColoring := TfpgCheckBox.Create(self);
  with ckColoring do
  begin
    Name := 'ckColoring';
    SetPosition(10, 245, 150, 20);
    FontDesc := '#Label1';
    Hint := 'Color last move squares';
    //TabOrder := 9;
    Text := 'Color last move';
    //OnChange := @ckColoringChange;
  end;

  ckChess960 := TfpgCheckBox.Create(self);
  with ckChess960 do
  begin
    Name := 'ckChess960';
    SetPosition(10, 271, 150, 20);
    FontDesc := '#Label1';
    Hint := 'Fischer random chess';
    //TabOrder := 9;
    Text := 'Chess 960';
    //OnChange := @ckColoringChange;
  end;

  btStart := TfpgButton.Create(self);
  with btStart do
  begin
    Name := 'btStart';
    SetPosition(10, 297, 150, 24);
    Text := 'Start Eschecs';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    //TabOrder := 7;
    OnClick := @btStartClick;
  end;
  
  btQuit := TfpgButton.Create(self);
  with btQuit do
  begin
    Name := 'btQuit';
    SetPosition(10, 327, 150, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    //TabOrder := 7;
    OnClick := @btQuitClick;
  end;
  
  PopulateFont;
  PopulateLang;
  PopulateStyle;
end;

end.
