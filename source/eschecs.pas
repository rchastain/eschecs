
program Eschecs;

{$IFDEF UNIX}
{$DEFINE USECTHREADS}
{$ENDIF}

uses
{$IFDEF UNIX}
  CThreads,
  CWString,
{$ENDIF}
  Classes,
  SysUtils,
  StrUtils,
  RegExpr,
  
  fpg_base,
  fpg_dialogs,
  fpg_form,
  fpg_main,
  fpg_menu,
  fpg_panel,
  fpg_stylemanager,
  fpg_widget,
  
  BGRABitmap,
  BGRABitmapTypes,
  
  Images,
  Board,
  Utils,
  Language,
  Connect,
{$IFDEF UNIX}
  Permission,
{$ENDIF}
  Settings,
  Validator,
  ChessTypes,
  Game,
  Uci,
  Fen,
  Engines,
  Sound,
  MoveList,
  FrmAbout,
  FrmPromotion,
  Style,
  Pgn;

{$IFDEF WINDOWS}
{$R eschecs.res}
{$ENDIF}
{$WARN 5024 OFF}
{$I version.inc}
{$I VERSION_FILE.inc}

type
  TNavigation = (nvPrevious, nvNext, nvLast, nvFirst);

  TListener = class(TThread)
  private
    FMessage: string;
    procedure OnEngineMessage;
  protected
    procedure Execute; override;
  end;

  TMainForm = class(TfpgForm)
  protected
    FChessboard: TBGRAChessboard;
    FStyle: TBoardStyle;
    FUpsideDown: boolean;
    FGame: TChessGame;
    FUserMove: string;
    FUserColor: TPieceColorStrict;
    FComputerColor: TPieceColor;
    FWaiting: boolean;
    FEngine: integer;
    FConnected: boolean;
    FMoveHist: TMoveList;
    FPosHist: TStringList;
    FCurrPosIndex: integer;
    FMoveTime: integer;
    FValidator: TValidator;
    FDragging: boolean;
    FMousePos, FDragPos, FInitPos: TPoint;
    FPieceIndex: integer;
    FWaitingForAnimation: boolean;
    FWaitingForReadyOk: integer;
    FWaitingForUserMove: boolean;
    FPgnData: TStringList;
    FColoring: boolean;
    FComputerCastling: boolean;
    FFenFileName: TFileName;
    FXLegend, FYLegend, FXLegendInv, FYLegendInv: TBGRABitmap;
    FChess960: boolean;
    procedure HandleKeyPress(var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean); override;
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
    procedure InitForm;
    procedure WidgetPaint(Sender: TObject);
    procedure TopWidgetPaint(Sender: TObject);
    procedure LeftWidgetPaint(Sender: TObject);
    procedure RightWidgetPaint(Sender: TObject);
    procedure BottomWidgetPaint(Sender: TObject);
    procedure WidgetMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure WidgetMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure WidgetMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  private
    FChessboardWidget: TfpgWidget;
    FTopLegendWidget: TfpgWidget;
    FLeftLegendWidget: TfpgWidget;
    FRightLegendWidget: TfpgWidget;
    FBottomLegendWidget: TfpgWidget;
    FStatusBar: TfpgPanel;
    FMenuBar: TfpgMenuBar;
    FEschecsSubMenu: TfpgPopupMenu;
    FMovesSubMenu: TfpgPopupMenu;
    FBoardSubMenu: TfpgPopupMenu;
    FOptionsSubMenu: TfpgPopupMenu;
    FAudioSubMenu: TfpgPopupMenu;
    FTimer: TfpgTimer;
    procedure ItemExitClicked(Sender: TObject);
    procedure ItemNewGameClicked(Sender: TObject);
    procedure ItemNewGame960Clicked(Sender: TObject);
    procedure OtherItemClicked(Sender: TObject);
    procedure InternalTimerFired(Sender: TObject);
    procedure DoMove(const AMove: string; const APromotion: TPieceType; const AComputerMove: boolean; out ASkip: boolean);
    procedure OnMoveDone(const AHistory: string; const ASound: boolean = TRUE);
    procedure OnComputerMove;
    procedure OnUserIllegalMove;
    procedure SetComputerColor(const AAutoPlay: boolean);
    procedure NewPosition(const APos: string; const AHistory: string = '');
    function TryNavigate(const ACurrIndex: integer; const ANavig: TNavigation): integer;
    procedure PlaySound(const ASound: integer);
    procedure CloseAll(Sender: TObject);
    procedure SaveGame(Sender: TObject);
    procedure OnResized(Sender: TObject);
    procedure OnShown(Sender: TObject);
    function LoadFrcPos(const ANumber: integer): string;
  end;

{$I icon.inc}

const
  CFirstEngineItem = 3;
  CLogName = 'eschecs.log';
  
var
  LListener: TThread;

procedure Log(const ALine: string); overload;
var
  LLog: TextFile;
  LFileName: string;
  LTime: string;
begin
  LFileName := Concat(LConfigFilesPath, CLogName);
  Assign(LLog, LFileName);
  if FileExists(LFileName) then
    Append(LLog)
  else
    Rewrite(LLog);
  LTime := DateTimeToStr(Now);
  WriteLn(LLog, LTime, ' ', ALine);
  Close(LLog);
end;

procedure Log(const AText, AInsert: string); overload;
var
  LLog: TextFile;
  LFileName: string;
  LTime: string;
  LList: TStringList;
  i: integer;
begin
  LFileName := Concat(LConfigFilesPath, CLogName);
  Assign(LLog, LFileName);
  if FileExists(LFileName) then
    Append(LLog)
  else
    Rewrite(LLog);
  LTime := DateTimeToStr(Now);
  LList := TStringList.Create;
  ExtractStrings([#10, #13], [' '], PChar(AText), LList);
  for i := 0 to Pred(LList.Count) do
    WriteLn(LLog, LTime, ' ', AInsert, ' ', LList[i]);
  LList.Free;
  Close(LLog);
end;

procedure Send(const ACommand: string);
begin
  WriteProcessInput(ACommand);
  Log(ACommand, '>');
end;

function ArbitratorMessage(const ACheck: boolean; const AActive: TPieceColorStrict; const AState: TChessState): string;
begin
  case AState of
    csProgress:
      result := Concat(
        IfThen(ACheck, Concat(GetText(txCheck), ' '), ''),
        IfThen(AActive = pcWhite, GetText(txWhiteToMove), GetText(txBlackToMove))
      );
    csCheckmate:
      result := Concat(
        GetText(txCheckmate), ' ',
        IfThen(AActive = pcWhite, GetText(txBlackWins), GetText(txWhiteWins))
      );
    csStalemate:
      result := GetText(txStalemate);
    csDraw:
      result := GetText(txDraw);
  end;
end;

procedure TMainForm.HandleKeyPress(var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    KeyLeft,
    KeyBackspace: FCurrPosIndex := TryNavigate(FCurrPosIndex, nvPrevious);
    KeyRight:     FCurrPosIndex := TryNavigate(FCurrPosIndex, nvNext);
    KeyUp:        FCurrPosIndex := TryNavigate(FCurrPosIndex, nvLast);
    KeyDown:      FCurrPosIndex := TryNavigate(FCurrPosIndex, nvFirst);
  end;
end;

destructor TMainForm.Destroy;
begin
  FChessboard.Free;
  FGame.Free;
  if FConnected then
  begin
    Send(MsgQuit);
    FreeConnectedProcess;
    LListener.Terminate;
    LListener.WaitFor;
  end;
  LListener.Free;
  FMoveHist.Free;
  FPosHist.Free;
  FPgnData.Free;
  FValidator.Free;
  FreePictures;
  FXLegend.Free;
  FYLegend.Free;
  FXLegendInv.Free;
  FYLegendInv.Free;
  (*
  FChessboardWidget.Free;
  FTimer.Free;
  FTopLegendWidget.Free;
  FLeftLegendWidget.Free;
  FRightLegendWidget.Free;
  FBottomLegendWidget.Free;
  *)
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  Randomize;
  Name := 'MainForm';
  WindowTitle := 'Eschecs';
  BackGroundColor := $80000001;
  Hint := '';
  IconName := 'vfd.eschecs';
  ShowHint := TRUE;
  WindowPosition := wpOneThirdDown;
  OnResize := @OnResized;
  OnShow := @OnShown;
  FChessboardWidget := TfpgWidget.Create(self);
  with FChessboardWidget do
  begin
    Name := 'FChessboardWidget';
    BackgroundColor := clNone;
    OnPaint := @WidgetPaint;
    OnMouseDown := @WidgetMouseDown;
    OnMouseUp := @WidgetMouseUp;
    OnMouseMove := @WidgetMouseMove;
  end;
  FTopLegendWidget := TfpgWidget.Create(self);
  with FTopLegendWidget do
  begin
    Name := 'FTopLegendWidget';
    BackgroundColor := clNone;
    OnPaint := @TopWidgetPaint;
  end;
  FLeftLegendWidget := TfpgWidget.Create(self);
  with FLeftLegendWidget do
  begin
    Name := 'FLeftLegendWidget';
    BackgroundColor := clNone;
    OnPaint := @LeftWidgetPaint;
  end;
  FRightLegendWidget := TfpgWidget.Create(self);
  with FRightLegendWidget do
  begin
    Name := 'FRightLegendWidget';
    BackgroundColor := clNone;
    OnPaint := @RightWidgetPaint;
  end;
  FBottomLegendWidget := TfpgWidget.Create(self);
  with FBottomLegendWidget do
  begin
    Name := 'FBottomLegendWidget';
    BackgroundColor := clNone;
    OnPaint := @BottomWidgetPaint;
  end;
  FStatusBar := TfpgPanel.Create(self);
  with FStatusBar do
  begin
    Name := 'FStatusBar';
    Align := alBottom;
    Alignment := taLeftJustify;
    BackgroundColor := TfpgColor($FFFFFF);
    FontDesc := '#Label1';
    ParentShowHint := FALSE;
    Style := bsLowered;
    Text := '';
    TextColor := TfpgColor($000000);
    Hint := '';
  end;
  FMenuBar := TfpgMenuBar.Create(self);
  with FMenuBar do
  begin
    Name := 'FMenuBar';
    //Align := alTop;
    SetPosition(0, 0, 9 * 40, 24);
    Anchors := [anLeft, anRight, anTop];
  end;
  FEschecsSubMenu := TfpgPopupMenu.Create(self);
  with FEschecsSubMenu do Name := 'FEschecsSubMenu';
  FMovesSubMenu   := TfpgPopupMenu.Create(self);
  with FMovesSubMenu   do Name := 'FMovesSubMenu';
  FBoardSubMenu   := TfpgPopupMenu.Create(self);
  with FBoardSubMenu   do Name := 'FBoardSubMenu';
  FOptionsSubMenu := TfpgPopupMenu.Create(self);
  with FOptionsSubMenu do Name := 'FOptionsSubMenu';
  FAudioSubMenu   := TfpgPopupMenu.Create(self);
  with FAudioSubMenu   do Name := 'FAudioSubMenu';
  InitForm;
end;

procedure TMainForm.InitForm;
const
  CMenuBarHeight = 24;
var
  LCurrPos: string;
  LAuto: boolean;
  LIndex: integer;
  LFileName: TFileName;
  LMoveHist: string;
  (*
  LStr: string;
  LInt: integer;
  LBool: boolean;
  *)
  LLegend: TBGRABitmap;
begin
  LoadSettings(
    LCurrPos,
    LAuto,
    FUpsideDown,
    FStyle,
    LMoveHist,
    FCurrPosIndex,
    FEngine,
    LLSColor,
    LDSColor,
    LBackColors[bcGreen],
    LBackColors[bcRed],
    FMoveTime,
    LFont,
    LLang,
    FColoring,
    LScale,
    FChess960
  );
  (*
  if CommandLine.HasOption('coloring', LBool) then FColoring := LBool;
  if CommandLine.HasOption('font', LStr) then LFont := LStr;
  if CommandLine.HasOption('language', LInt) then LLang := TLanguage(LInt);
  if CommandLine.HasOption('movetime', LInt) then FMoveTime := LInt;
  if CommandLine.HasOption('scale', LInt) then LScale := LInt;
  if CommandLine.HasOption('style', LInt) then FStyle := TBoardStyle(LInt);
  if CommandLine.HasOption('chess960', LBool) then FChess960 := LBool;
  *)
  FFenFileName := Concat(LConfigFilesPath, 'eschecs.fen');
  LFileName := Concat(LConfigFilesPath, COsType, '.eng');
  if FileExists(LFileName) then
    LoadEnginesData(LFileName, FChess960)
  else
  begin
    LFileName := Concat(LConfigFilesPath, 'eschecs.eng');
    if FileExists(LFileName) then
      LoadEnginesData(LFileName, FChess960)
    else
      ShowMessage(Format('Fichier introuvable : %s', [LFileName]));
  end;
  
  FValidator := TValidator.Create;
  Assert(FValidator.IsFEN(LCurrPos));
  FMoveHist := TMoveList.Create(LMoveHist);
  FPosHist := TStringList.Create;
  if FileExists(FFenFileName) then
    FPosHist.LoadfromFile(FFenFileName)
  else
    FPosHist.Append(LCurrPos);
  
  FPgnData := TStringList.Create;
  FPgnData.Append(LCurrPos);
  
  with FMenuBar do
  begin
    AddMenuItem(GetText(txEschecs),   nil).SubMenu := FEschecsSubMenu;
    AddMenuItem(GetText(txMoves),     nil).SubMenu := FMovesSubMenu;
    AddMenuItem(GetText(txBoard),     nil).SubMenu := FBoardSubMenu;
    AddMenuItem(GetText(txOptions),   nil).SubMenu := FOptionsSubMenu;
  end;
  with FEschecsSubMenu do
  begin
    AddMenuItem(GetText(txSave), 'Ctrl+S', @SaveGame);
    AddMenuItem(GetText(txSave) + ' + ' + GetText(txQuit), 'Esc', @ItemExitClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem(GetText(txQuit), 'Ctrl+Q', @CloseAll);
    AddMenuItem('-', '', nil);
    AddMenuItem(GetText(txAbout), '', @OtherItemClicked);
  end;
  with FOptionsSubMenu do
  begin
    AddMenuItem(GetText(txSound), '',    nil).SubMenu := FAudioSubMenu;
  end;
  with FAudioSubMenu do
  begin
    AddMenuItem(GetText(txEnabled), '', @OtherItemClicked).Checked := TRUE;
    AddMenuItem('-', '', nil);
    AddMenuItem(GetText(txVolume) + ':', '', nil);
    AddMenuItem('100 %', '', @OtherItemClicked).Checked := FALSE;
    AddMenuItem('75 %',  '', @OtherItemClicked).Checked := FALSE;
    AddMenuItem('50 %',  '', @OtherItemClicked).Checked := TRUE;
    AddMenuItem('25 %',  '', @OtherItemClicked).Checked := FALSE;
  end;
  with FBoardSubMenu do
  begin
    AddMenuItem(GetText(txNew),    '', @ItemNewGameClicked);
    AddMenuItem(GetText(txNew960), '', @ItemNewGame960Clicked).Enabled := FChess960;
    AddMenuItem(GetText(txFlip),   '', @OtherItemClicked);
  end;
  with FMovesSubMenu do
  begin
    AddMenuItem(GetText(txComputerMove), '', @OtherItemClicked);
    AddMenuItem(GetText(txAutoPlay),     '', @OtherItemClicked).Checked := LAuto;
    AddMenuItem('-', '', nil);
    for LIndex := 0 to High(LEngines) do
      with AddMenuItem(LEngines[LIndex].FName, '', @OtherItemClicked) do
      begin
        with LEngines[LIndex] do
          Enabled := FExists{$IFDEF UNIX} and IsFileExecutable(Concat(FDirectory, FCommand)) or MakeFileExecutable(Concat(FDirectory, FCommand)){$ENDIF};
        Checked := FALSE;
        if Enabled and (FEngine = CDefaultEngine) then
          FEngine := LIndex;
      end;
  end;
  SetPosition(0, 0, 9 * LScale, 24 + 9 * LScale + 24);
  WindowTitle := CDefaultTitle;
  MinWidth := 9 * LScale;
  MinHeight := 24 + 9 * LScale + 24;
  FChessboardWidget.SetPosition(0 + LScale div 2, CMenuBarHeight + LScale div 2, 8 * LScale, 8 * LScale);
  FStatusBar.SetPosition(0, 24 + 9 * LScale, 9 * LScale, 24);
  FMenuBar.SetPosition(0, 0, 9 * LScale, 24);
  
  FXLegend := TBGRABitmap.Create(8 * LScale, LScale div 2, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/x/%d.png', [ExtractFilePath(ParamStr(0)), LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName])); 
  LLegend := TBGRABitmap.Create(LFileName);
  FXLegend.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FYLegend := TBGRABitmap.Create(LScale div 2, 8 * LScale, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/y/%d.png', [ExtractFilePath(ParamStr(0)),LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName]));
  LLegend := TBGRABitmap.Create(LFileName);
  FYLegend.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FXLegendInv := TBGRABitmap.Create(8 * LScale, LScale div 2, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/x/inv/%d.png', [ExtractFilePath(ParamStr(0)),LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName])); 
  LLegend := TBGRABitmap.Create(LFileName);
  FXLegendInv.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FYLegendInv := TBGRABitmap.Create(LScale div 2, 8 * LScale, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/y/inv/%d.png', [ExtractFilePath(ParamStr(0)),LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName]));
  LLegend := TBGRABitmap.Create(LFileName);
  FYLegendInv.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FTopLegendWidget.SetPosition(LScale div 2, CMenuBarHeight, 8 * LScale, LScale div 2);
  FLeftLegendWidget.SetPosition(0, CMenuBarHeight + LScale div 2, LScale div 2, 8 * LScale);
  FRightLegendWidget.SetPosition(8 * LScale + LScale div 2, CMenuBarHeight + LScale div 2, LScale div 2, 8 * LScale);
  FBottomLegendWidget.SetPosition(LScale div 2, CMenuBarHeight + 8 * LScale + LScale div 2, 8 * LScale, LScale div 2);
  
  CreatePictures(FStyle, LScale);
  FChessboard := TBGRAChessboard.Create(FStyle, FUpsideDown, LCurrPos);
  FGame := TChessGame.Create(LCurrPos);
  FUserMove := '';
  OnMoveDone(FMoveHist.GetString(FCurrPosIndex), FALSE);
  SetComputerColor(FMovesSubMenu.MenuItem(1).Checked);
  LListener := TListener.Create(TRUE);
  LListener.Priority := tpHigher;
  FWaiting := FALSE;
  FConnected := FALSE;
  FWaitingForAnimation := FALSE;
  FWaitingForReadyOk := 0;
  FWaitingForUserMove := TRUE;
  
  Log(Format('Eschecs %s %s %s %s FPC %s fpGUI %s BGRABitmap %s', [
    CVersion,
    COsType,
    {$I %DATE%},
    {$I %TIME%},
    {$I %FPCVERSION%},
    FPGUI_VERSION,
    BGRABitmapVersionStr
  ]));
  
  FTimer := TfpgTimer.Create(10);
  FTimer.OnTimer := @InternalTimerFired;
  FTimer.Enabled := TRUE;
  with FMovesSubMenu do
    if MenuItem(FEngine + CFirstEngineItem).Enabled then
      OtherItemClicked(MenuItem(FEngine + CFirstEngineItem));
  if LoadSoundLib < 0 then
  begin
    FAudioSubMenu.MenuItem(0).Checked := FALSE;
    FAudioSubMenu.MenuItem(0).Enabled := FALSE;
  end else 
  begin
    FAudioSubMenu.MenuItem(0).Checked := TRUE;
    FAudioSubMenu.MenuItem(0).Enabled := TRUE;
  end;
  FComputerCastling := FALSE;
end;

procedure TMainForm.WidgetPaint(Sender: TObject);
begin
  FChessboard.DrawToFPGCanvas(FChessboardWidget.Canvas, 0, 0);
end;

procedure TMainForm.TopWidgetPaint(Sender: TObject);
begin
  if FUpsideDown then
    FXLegendInv.Draw(FTopLegendWidget.Canvas, 0, 0)
  else
    FXLegend.Draw(FTopLegendWidget.Canvas, 0, 0);
end;

procedure TMainForm.LeftWidgetPaint(Sender: TObject);
begin
  if FUpsideDown then
    FYLegendInv.Draw(FLeftLegendWidget.Canvas, 0, 0)
  else
    FYLegend.Draw(FLeftLegendWidget.Canvas, 0, 0);
end;

procedure TMainForm.RightWidgetPaint(Sender: TObject);
begin
  if FUpsideDown then
    FYLegendInv.Draw(FRightLegendWidget.Canvas, 0, 0)
  else
    FYLegend.Draw(FRightLegendWidget.Canvas, 0, 0);
end;

procedure TMainForm.BottomWidgetPaint(Sender: TObject);
begin
  if FUpsideDown then
    FXLegendInv.Draw(FBottomLegendWidget.Canvas, 0, 0)
  else
    FXLegend.Draw(FBottomLegendWidget.Canvas, 0, 0);
end;

procedure TMainForm.WidgetMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  X, Y: integer;
begin
  if (FGame.state = csProgress) and FWaitingForUserMove then
  begin
    FMousePos := AMousePos;
    FDragPos.X := AMousePos.X mod LScale;
    FDragPos.Y := AMousePos.Y mod LScale;
    FInitPos := AMousePos - FDragPos;
    FChessboard.ScreenToXY(AMousePos, X, Y);
    FPieceIndex := FChessboard.FindPiece(X, Y, TPieceColorStrict(Ord(FGame.ActiveColor)));
    if FPieceIndex > 0 then
    begin
      FUserMove := EncodeSquare(X, Y);
      FDragging := TRUE;
      FChessboard.SavePieceBackground(FInitPos, TRUE);
      if FColoring then
        FChessboard.ScreenRestore;
    end;
  end;
end;

procedure TMainForm.WidgetMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  X, Y: integer;
begin
  if FDragging then
  begin
    FChessboard.RestorePieceBackground(FMousePos - FDragPos);
    FChessboard.SavePieceBackground(AMousePos - FDragPos);
    FChessboard.DrawPiece(AMousePos - FDragPos, FPieceIndex);
    FChessboardWidget.Invalidate;
    FMousePos := AMousePos;
  end else
  begin
    FChessboard.ScreenToXY(AMousePos, X, Y);
    if FWaitingForUserMove and (FChessboard.FindPiece(X, Y, TPieceColorStrict(Ord(FGame.ActiveColor))) > 0) then
      TfpgWidget(Sender).MouseCursor := mcHand
    else
      TfpgWidget(Sender).MouseCursor := mcDefault;
  end;
end;

procedure TMainForm.WidgetMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  LType: TPieceType;
  X, Y: integer;
  LSkip: boolean;
begin
  if not FDragging then
    Exit;
  FDragging := FALSE;
  FChessboard.ScreenToXY(AMousePos, X, Y);
  FUserMove := Concat(FUserMove, EncodeSquare(X, Y));
  
  if (FUserMove = 'e1g1') and FGame.IsLegal('e1h1') and FGame.IsCastling('e1h1') then FUserMove := 'e1h1';
  if (FUserMove = 'e1c1') and FGame.IsLegal('e1a1') and FGame.IsCastling('e1a1') then FUserMove := 'e1a1';
  if (FUserMove = 'e8g8') and FGame.IsLegal('e8h8') and FGame.IsCastling('e8h8') then FUserMove := 'e8h8';
  if (FUserMove = 'e8c8') and FGame.IsLegal('e8a8') and FGame.IsCastling('e8a8') then FUserMove := 'e8a8';
  
  if FGame.IsLegal(FUserMove) then
  begin
    FChessboard.RestorePieceBackground(FMousePos - FDragPos);
    if FGame.IsPromotion(FUserMove) then
    begin
      FChessboard.SavePieceBackground(FChessboard.XYToScreen(X, Y), TRUE);
      FChessboard.RestorePieceBackground(FChessboard.XYToScreen(X, Y));
      FChessboard.DrawPiece(FChessboard.XYToScreen(X, Y), FPieceIndex);
      FChessboardWidget.Invalidate;
      LType := SelectPieceType;
    end else
      LType := ptNil;
    DoMove(FUserMove, LType, FALSE, LSkip);
    if LType <> ptNil then
      FChessboard.SetPieceType(FPieceIndex, LType);
    if not LSkip then
    begin
      FChessboard.SetPieceXY(FPieceIndex, X, Y);
      FChessboard.DrawPiece(FChessboard.XYToScreen(X, Y), FPieceIndex);
      if FColoring then
      begin
        FChessboard.ScreenSave;
        FChessboard.HighlightMove(FUserMove, FPieceIndex);
      end;
    end;
    FChessboardWidget.Invalidate;
    OnMoveDone(FMoveHist.GetString(FCurrPosIndex));
  end else
  begin
    FChessboard.RestorePieceBackground(FMousePos - FDragPos);
    FChessboard.DrawPiece(FInitPos, FPieceIndex);
    FChessboardWidget.Invalidate;
    if Copy(FUserMove, 3, 2) <> Copy(FUserMove, 1, 2) then
      OnUserIllegalMove;
  end;
end;
  
procedure TMainForm.ItemExitClicked(Sender: TObject);
begin
  FTimer.Enabled := FALSE;
  SaveGame(Sender);
  Close;
end;

procedure TMainForm.ItemNewGameClicked(Sender: TObject);
const
  CTraditionalStartPosition = 518;
var
  LPos: string;
begin
  if FChess960 then
    LPos := LoadFrcPos(CTraditionalStartPosition)
  else
    LPos := CFenStartPosition;
  NewPosition(LPos);
  FMoveHist.Clear;
  FPosHist.Clear;
  FPosHist.Append(LPos);
  FPgnData.Clear;
  FPgnData.Append(LPos);
  FCurrPosIndex := 0;
  FWaitingForUserMove := TRUE;
end;

procedure TMainForm.ItemNewGame960Clicked(Sender: TObject);
var
  LPos: string;
begin
  LPos := LoadFrcPos(Random(960));
  NewPosition(LPos);
  FMoveHist.Clear;
  FPosHist.Clear;
  FPosHist.Append(LPos);
  FPgnData.Clear;
  FPgnData.Append(LPos);
  FCurrPosIndex := 0;
  FWaitingForUserMove := TRUE;
end;

procedure TMainForm.OtherItemClicked(Sender: TObject);
var
  i, j: integer;
begin
  if Sender is TfpgMenuItem then
    with TfpgMenuItem(Sender) do
      if Text = GetText(txAbout) then
        ShowAboutForm(
          Concat('Eschecs ', CVersion),
          GetText(txAboutMessage),
          GetText(txAbout),
          GetText(txQuit),
          'Website'
        )
    else
      if Text = GetText(txComputerMove) then
        FComputerColor := FGame.ActiveColor
      else
      if Text = GetText(txAutoPlay) then
      begin
        Checked := not Checked;
        SetComputerColor(Checked);
      end else
      if Text = GetText(txFlip) then
      begin
        FChessboard.ScreenRestore;
        FChessboard.FlipBoard;
        FChessboardWidget.Invalidate;
        FChessboard.ScreenSave;
        FUpsideDown := FChessboard.UpsideDown;
        FTopLegendWidget.Invalidate;
        FLeftLegendWidget.Invalidate;
        FRightLegendWidget.Invalidate;
        FBottomLegendWidget.Invalidate;
      end else
      if Text = GetText(txEnabled) then
      begin
        Checked := not Checked;
      end else
      if Text = '100 %' then
      begin
        for i := 3 to 6 do
          FAudioSubMenu.MenuItem(i).Checked := FALSE;
        Checked := TRUE;
        SetSoundVolume(100);
      end else
      if Text = '75 %' then
      begin
        for i := 3 to 6 do
          FAudioSubMenu.MenuItem(i).Checked := FALSE;
        Checked := TRUE;
        SetSoundVolume(75);
      end else
      if Text = '50 %' then
      begin
        for i := 3 to 6 do
          FAudioSubMenu.MenuItem(i).Checked := FALSE;
        Checked := TRUE;
        SetSoundVolume(50);
      end else
      if Text = '25 %' then
      begin
        for i := 3 to 6 do
          FAudioSubMenu.MenuItem(i).Checked := FALSE;
        Checked := TRUE;
        SetSoundVolume(25);
      end else
      for i := 0 to High(LEngines) do if Text = LEngines[i].FName then
      begin
        for j := 0 to High(LEngines) do
          FMovesSubMenu.MenuItem(j + CFirstEngineItem).Checked := j = i;
        if FConnected then
        begin
          Send(MsgQuit);
          FreeConnectedProcess;
        end;
        with LEngines[i] do
          FConnected :=
            SetCurrentDir(ExtractFileDir(ParamStr(0)))
            and FileExists(Concat(FDirectory, FCommand))
            and SetCurrentDir(FDirectory)
            and CreateConnectedProcess(FCommand);
        if FConnected then
        begin
          Log(Format('Connecté à %s.', [LEngines[i].FName]));
          LListener.Start;
          Send(MsgUci);
          FEngine := i;
        end else
          ShowAboutForm(GetText(txConnectionFailure), '',  GetText(txTitleMessage), GetText(txQuit), '');
      end;
end;

procedure TMainForm.InternalTimerFired(Sender: TObject);
var
  LAnimationTerminated: boolean;
begin
  if FChessboard.Animate(LAnimationTerminated) or FComputerCastling then
    FChessboardWidget.Invalidate
  else
    if FConnected
    and (FComputerColor = FGame.ActiveColor)
    and (FGame.State = csProgress)
    and not FWaiting then
    begin
      case FWaitingForReadyOk of
        0:
          begin
            FWaitingForReadyOk := 1;
            Send(MsgPosition(FGame.GetFen(FChess960)));
            Send(MsgIsReady);
          end;
        1:
          begin
          end;
        2:
          begin
            FWaitingForReadyOk := 0;
            Send(MsgGo(FMoveTime));
            MouseCursor := mcHourGlass;
            FWaiting := TRUE;
            FStatusBar.Text := GetText(txWaiting);
            FWaitingForUserMove := FALSE;
          end;
      end;
    end;
  if (FWaitingForAnimation and LAnimationTerminated) or FComputerCastling then
  begin
    FWaitingForAnimation := FALSE;
    OnComputerMove;
  end;
  FComputerCastling := FALSE;
end;

procedure TMainForm.DoMove(const AMove: string; const APromotion: TPieceType; const AComputerMove: boolean; out ASkip: boolean);
const
  CSymbols: array[ptKnight..ptQueen] of char = ('n', 'b', 'r', 'q');
var
  LX, LY: integer;
  LSquare: string;
  LSymbol, LSanMove: string;
begin
  ASkip := FALSE;
  if FGame.IsCastling(AMove) then
  begin
    FChessboard.MoveKingRook(AMove, AComputerMove);
    ASkip := TRUE;
    FComputerCastling := AComputerMove;
  end else
  begin
    LSquare := Copy(AMove, 3, 2);
    DecodeSquare(LSquare, LX, LY);
    if FChessboard.FindPiece(LX, LY) > 0 then
      FChessboard.ErasePiece(LSquare);
    LSquare := FGame.IsEnPassant(AMove);
    if LSquare <> '' then
      FChessboard.ErasePiece(LSquare);
    if APromotion <> ptNil then
      LSymbol := CSymbols[APromotion]
    else
      LSymbol := '';
    if AComputerMove then
      FChessboard.MovePiece(AMove, APromotion);
    if FColoring then
      LHighlighted := AMove;
  end;
  LSanMove := FGame.GetSan(AMove);
  FGame.DoMove(Concat(AMove, LSymbol));
  if FGame.Check then
    if FGame.State = csCheckmate then
      LSanMove := LSanMove + '#'
    else
      LSanMove := LSanMove + '+';
  FPgnData.Append(LSanMove);
  FMoveHist.Append(AMove, FCurrPosIndex);
  while FPosHist.Count > Succ(FCurrPosIndex) do
    FPosHist.Delete(FPosHist.Count - 1);
  FPosHist.Append(FGame.GetFen);
  Inc(FCurrPosIndex);
end;

procedure TMainForm.OnMoveDone(const AHistory: string; const ASound: boolean);
var
  LX, LY: integer;
  LIndex: integer;
begin
  if FColoring and FGame.Check and FChessboard.ScreenSaved then
  begin
    FGame.GetKingCheckedXY(LX, LY);
    LIndex := FChessboard.FindPiece(LX, LY);
    FChessboard.Highlight(LX, LY, bcRed, LIndex);
    FChessboardWidget.Invalidate;
  end;
  if ASound then
    if FGame.state in [csCheckmate, csStalemate, csDraw] then
      PlaySound(sndEndOfGame)
    else if FGame.Check then
      PlaySound(sndCheck)
    (*
    else if FALSE then
      PlaySound(sndPromotion)
    else if FALSE then
      PlaySound(sndCapture)
    *)
    else
      PlaySound(sndMove);
  FStatusBar.Text := ArbitratorMessage(FGame.Check, FGame.ActiveColor, FGame.state);
  if FGame.state in [csCheckmate, csStalemate, csDraw] then
    FStatusBar.BackgroundColor := $FFF692
  else if FGame.Check then
    FStatusBar.BackgroundColor := $FFB3B8
  else
    FStatusBar.BackgroundColor := $FFFFFF;
  FWaitingForUserMove := not (FGame.state in [csCheckmate, csStalemate, csDraw]);
end;

procedure TMainForm.OnComputerMove;
begin
  if not FMovesSubMenu.MenuItem(1).Checked then
    FComputerColor := pcNil;
  MouseCursor := mcHand;
  OnMoveDone(FMoveHist.GetString(FCurrPosIndex));
  FWaiting := FALSE;
  FWaitingForUserMove := TRUE;
end;

procedure TMainForm.OnUserIllegalMove;
begin
  PlaySound(sndIllegal);
end;

procedure TMainForm.SetComputerColor(const AAutoPlay: boolean);
begin
  if AAutoPlay then
    FComputerColor := TPieceColorStrict(1 - Ord(FGame.ActiveColor))
  else
    FComputerColor := pcNil;
end;

procedure TMainForm.NewPosition(const APos: string; const AHistory: string);
begin
  FChessboard.Free;
  FChessboard := TBGRAChessboard.Create(FStyle, FUpsideDown, APos);
  FGame.Create(APos);
  OnMoveDone(AHistory, FALSE);
  SetComputerColor(FMovesSubMenu.MenuItem(1).Checked);
  FChessboardWidget.Invalidate;
  FStatusBar.BackgroundColor := $FFFFFF;
end;

function TMainForm.TryNavigate(const ACurrIndex: integer; const ANavig: TNavigation): integer;
begin
  result := ACurrIndex;
  case ANavig of
    nvPrevious:
      if ACurrIndex > 0 then
        result := Pred(ACurrIndex);
    nvNext:
      if ACurrIndex < Pred(FPosHist.Count) then
        result := Succ(ACurrIndex);
    nvLast:
      if ACurrIndex < Pred(FPosHist.Count) then
        result := Pred(FPosHist.Count);
    nvFirst:
      if ACurrIndex > 0 then
        result := 0;
  end;
  if result <> ACurrIndex then
    NewPosition(
      FPosHist[result],
      IfThen(
        result = 0,
        '',
        FMoveHist.GetString(result)
      )
    );
end;

procedure TMainForm.PlaySound(const ASound: integer);
begin
  if FAudioSubMenu.MenuItem(0).Checked then
    Play(ASound);
end;

procedure TMainForm.CloseAll(Sender: TObject);
begin
  FTimer.Enabled := FALSE;
  Close;
end;

procedure TMainForm.SaveGame(Sender: TObject);
var
  LMoveHist: string;
begin
  LMoveHist := FMoveHist.GetString;
  SaveSettings(
    FGame.GetFen,
    FMovesSubMenu.MenuItem(1).Checked,
    FUpsideDown,
    FStyle,
    LMoveHist,
    FCurrPosIndex,
    FEngine,
    LLSColor, LDSColor, LBackColors[bcGreen], LBackColors[bcRed],
    FMoveTime,
    LFont,
    LLang,
    FColoring,
    LScale,
    FChess960
  );
  FPosHist.SaveToFile(FFenFileName);
  case FGame.State of
    csProgress: FPgnData.Append('*');
    csCheckmate: if FGame.ActiveColor = pcBlack then FPgnData.Append('1-0') else FPgnData.Append('0-1');
    csStalemate, csDraw: FPgnData.Append('1/2-1/2');
  end;
  WritePgnFile(Concat(ExtractFilePath(ParamStr(0)), 'game.pgn'), FPgnData, FChess960);
end;

procedure TMainForm.OnResized(Sender: TObject);
begin
  FChessboardWidget.Top := (Height -FChessboardWidget.Height) div 2;
  FChessboardWidget.Left := (Width - FChessboardWidget.Width) div 2;
  FTopLegendWidget.Top := FChessboardWidget.Top - FTopLegendWidget.Height;
  FTopLegendWidget.Left := FChessboardWidget.Left;
  FLeftLegendWidget.Top := FChessboardWidget.Top;
  FLeftLegendWidget.Left := FChessboardWidget.Left - FLeftLegendWidget.Width;
  FRightLegendWidget.Top := FChessboardWidget.Top;
  FRightLegendWidget.Left := FChessboardWidget.Left + FChessboardWidget.Width;
  FBottomLegendWidget.Top := FChessboardWidget.Bottom;
  FBottomLegendWidget.Left := FChessboardWidget.Left;
  FChessboardWidget.UpdateWindowPosition;
  FTopLegendWidget.UpdateWindowPosition;
  FLeftLegendWidget.UpdateWindowPosition;
  FRightLegendWidget.UpdateWindowPosition;
  FBottomLegendWidget.UpdateWindowPosition;
end;

procedure TMainForm.OnShown(Sender: TObject);
begin
  WriteLn('OnShown');
  Invalidate;
end;

function TMainForm.LoadFrcPos(const ANumber: integer): string;
var
  LFile: TFileName;
begin
  result := CFenStartPosition;
  LFile := Concat(LConfigFilesPath, 'fischerandom.fen');
  if FileExists(LFile) then
    with TStringList.Create do
    try
      LoadFromFile(LFile);
      result := Strings[ANumber];
      Log(Format('Position de départ n° %d.', [ANumber]));
    finally
      Free;
    end
  else
    ShowMessage(Format('Fichier introuvable: %s', [LFile]));
end;

var
  LForm: TMainForm;

procedure TListener.Execute;
const
  CDelay = 100;
begin
  while not Terminated do
  begin
    FMessage := ReadProcessOutput;
    if FMessage <> '' then
      Synchronize(@OnEngineMessage);
    Sleep(CDelay);
  end;
end;

procedure TListener.OnEngineMessage;
var
  LName, LAuthor, LMove, LTypeStr: string;
  LType: TPieceType;
  LFrcAvail: boolean;
  LSkip: boolean;
begin
  LSkip := FALSE;
  Log(FMessage, '<');
  if IsMsgUciOk(FMessage, LName, LAuthor, LFrcAvail) then
  begin
    Log(Format('Protocole accepté. Moteur %s. Auteur %s.', [LName, LAuthor]));
    if LForm.FChess960 then
    begin
      if LFrcAvail then
        Send(MsgSetOption('UCI_Chess960', TRUE))
      else
        ShowMessage('Option UCI_Chess960 not available.');
    end;
    Send(MsgNewGame);
    LForm.WindowTitle := LName;
  end else
  if IsMsgBestMove(FMessage, LMove, LTypeStr) then
  begin
    if not LForm.FChess960 then
    begin
      if (LMove = 'e1g1') and LForm.FGame.IsLegal('e1h1') and LForm.FGame.IsCastling('e1h1') then LMove := 'e1h1';
      if (LMove = 'e1c1') and LForm.FGame.IsLegal('e1a1') and LForm.FGame.IsCastling('e1a1') then LMove := 'e1a1';
      if (LMove = 'e8g8') and LForm.FGame.IsLegal('e8h8') and LForm.FGame.IsCastling('e8h8') then LMove := 'e8h8';
      if (LMove = 'e8c8') and LForm.FGame.IsLegal('e8a8') and LForm.FGame.IsCastling('e8a8') then LMove := 'e8a8';
    end;
    if LForm.FGame.IsLegal(LMove) then
    begin
      if Length(LTypeStr) = 1 then
        case LTypeStr[1] of
          'n': LType := ptKnight;
          'b': LType := ptBishop;
          'r': LType := ptRook;
          'q': LType := ptQueen;
        end
      else
        LType := ptNil;
      if LForm.FColoring then
        LForm.FChessboard.ScreenRestore;
      LForm.DoMove(LMove, LType, TRUE, LSkip);
    end else
    begin
      ShowAboutForm(GetText(txIllegalMove), LMove,  GetText(txTitleMessage), GetText(txQuit), '');
      LForm.FMovesSubMenu.MenuItem(1).Checked := FALSE;
      LForm.FComputerColor := pcNil;
    end;
    if not LSkip then
      LForm.FWaitingForAnimation := TRUE;
  end else
  if IsMsgReadyOk(FMessage) then
  begin
    Assert(LForm.FWaitingForReadyOk = 1);
    LForm.FWaitingForReadyOk := 2;
  end;
end;

begin
  Assert(DirectoryExists(LConfigFilesPath), Format('Répertoire introuvable : %s', [LConfigFilesPath]));
  
  try
    fpgApplication.Initialize;
    fpgImages.AddMaskedBMP('vfd.eschecs', @vfd_eschecs, SizeOf(vfd_eschecs), 0, 0);
    if fpgStyleManager.SetStyle('eschecs_style') then
      fpgStyle := fpgStyleManager.Style;
    fpgApplication.CreateForm(TMainForm, LForm);
    fpgApplication.MainForm := LForm;
    LForm.Show;
    fpgApplication.Run;
    FreeUos;
    LForm.Free;
    fpgApplication.Terminate;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
