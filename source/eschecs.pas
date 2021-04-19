
program Eschecs;

uses
{$IFDEF UNIX}
  CThreads,
  CWString,
{$ENDIF}
  Classes,
  SysUtils,
  StrUtils,
  RegExpr,
  TypInfo,
  Math,
  
  fpg_base,
  fpg_dialogs,
  fpg_form,
  fpg_main,
  fpg_menu,
  fpg_panel,
  fpg_stylemanager,
  fpg_widget,
  fpg_cmdlineparams,
  
  BGRABitmap,
  BGRABitmapTypes,
  
  Images,
  Board,
  Utils,
  Language,
  Connect,
  Permission,
  Settings,
  Validator,
  ChessTypes,
  Game,
  Uci,
  Fen,
  Sound,
  MoveList,
  FrmPromotion,
  Style,
  Pgn,
  Eco;

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
    FEngine: TFileName;
    FEngineExists: boolean;
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
    FComputerCastling: boolean;
    FFenFileName: TFileName;
    FXLegend, FYLegend, FXLegendInv, FYLegendInv: TBGRABitmap;
    FChess960: boolean;
    FOpeningName: string;
    FSendMsgGoTime: cardinal;
    FCheckTimeElapsed: boolean;
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
    FTimer: TfpgTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure ItemQuitClicked(Sender: TObject);
    procedure SaveGame(Sender: TObject);
    procedure OnResized(Sender: TObject);
    function LoadFrcPos(const ANumber: integer): string;
    procedure DropPiece(const AMousePos: TPoint; const ABortMove: boolean = FALSE);
  end;

{$I icon.inc}

const
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
  try
    WriteProcessInput(ACommand);
  except
    on E: Exception do
      Log('EXCEPTION ' + {$I %FILE%} + ' (' + {$I %LINE%} + '): ' + E.Message);
  end;
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DebugLn('FormCreate');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DebugLn('FormShow');
end;

procedure TMainForm.HandleKeyPress(var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  DebugLn('HandleKeyPress');
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
  DebugLn('Destroy');
  FChessboard.Free;
  FGame.Free;
  if FConnected then
  begin
    Send(MsgQuit);
    Sleep(200);
  end;
  FreeConnectedProcess;
  LListener.Terminate;
  LListener.WaitFor;
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
  DebugLn('AfterCreate');
  Randomize;
  Name := 'MainForm';
  WindowTitle := 'Eschecs';
  BackGroundColor := $80000001;
  Hint := '';
  IconName := 'vfd.eschecs';
  ShowHint := TRUE;
  WindowPosition := wpOneThirdDown;
  OnCreate := @FormCreate;
  OnShow := @FormShow;
  OnResize := @OnResized;
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
    Align := alTop;
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
  
  InitForm;
end;

procedure TMainForm.InitForm;
const
  CMenuBarHeight = 24;
var
  LCurrPos: string;
  LAuto: boolean;
  LFileName: TFileName;
  LMoveHist: string;
  LLegend: TBGRABitmap;
  LCmdIntf: ICmdLineParams;
  LErr: Tfpgstring;
  LArr: TStringArray;
  s: string;
  LVolume: integer;
begin
  DebugLn('InitForm');
  
  (* Load settings from *.ini file *)
  
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
    LScale,
    FChess960
  );
  
  (* Read command line parameters *)
  
  if Supports(fpgApplication, ICmdLineParams, LCmdIntf) then
  begin
    if LCmdIntf.ParamCount > 0 then
    begin
      try
        LErr := LCmdIntf.CheckOptions('a:b:c:f:g:l:m:p:r:s:u:v:w:', 'autoplay: black: chessboard: font: green: language: movetime: position: red: size: upsidedown: volume: white:');
      except
        on E: Exception do
          Log('EXCEPTION ' + {$I %FILE%} + ' (' + {$I %LINE%} + '): ' + E.Message);
      end;
      if Length(LErr) > 0 then
        DebugLn('LErr=', LErr)
      else
      begin
        if LCmdIntf.HasOption('p', 'position') then
        begin
          DebugLn('position = [', LCmdIntf.GetOptionValue('p', 'position'), ']');
          LCurrPos := Trim(LCmdIntf.GetOptionValue('p', 'position'));
        end;
        if LCmdIntf.HasOption('a', 'autoplay') then
        begin
          DebugLn('autoplay = [', LCmdIntf.GetOptionValue('a', 'autoplay'), ']');
          s := Trim(LCmdIntf.GetOptionValue('a', 'autoplay'));
          try
            LAuto := StrToBool(s);
          except
          end;
        end;
        if LCmdIntf.HasOption('u', 'upsidedown') then
        begin
          DebugLn('upsidedow = [', LCmdIntf.GetOptionValue('u', 'upsidedow'), ']');
          s := Trim(LCmdIntf.GetOptionValue('u', 'upsidedown'));
          try
            FUpsideDown := StrToBool(s);
          except
          end;
        end;
        if LCmdIntf.HasOption('c', 'chessboard') then
        begin
          DebugLn('chessboard = [', LCmdIntf.GetOptionValue('c', 'chessboard'), ']');
          s := Trim(LCmdIntf.GetOptionValue('c', 'chessboard'));
          s := LowerCase(s);
          if      s = 'simple'  then FStyle := bsSimple
          else if s = 'marble'  then FStyle := bsMarbleI
          else if s = 'marble2' then FStyle := bsMarbleII
          else if s = 'wood'    then FStyle := bsWood;
        end;
        if LCmdIntf.HasOption('m', 'movetime') then
        begin
          DebugLn('movetime = [', LCmdIntf.GetOptionValue('m', 'movetime'), ']');
          s := Trim(LCmdIntf.GetOptionValue('m', 'movetime'));
          FMoveTime := StrToIntDef(s, 999);
        end;
        if LCmdIntf.HasOption('f', 'font') then
        begin
          DebugLn('font = [', LCmdIntf.GetOptionValue('f', 'font'), ']');
          s := Trim(LCmdIntf.GetOptionValue('f', 'font'));
          s := LowerCase(s);
          LFont := s;
        end;
        if LCmdIntf.HasOption('l', 'language') then
        begin
          DebugLn('language = [', LCmdIntf.GetOptionValue('l', 'language'), ']');
          s := Trim(LCmdIntf.GetOptionValue('l', 'language'));
          try
            LLang := TLanguage(GetEnumValue(TypeInfo(TLanguage), 'lg' + s));
          except
          end;
        end;
        if LCmdIntf.HasOption('s', 'size') then
        begin
          DebugLn('size = [', LCmdIntf.GetOptionValue('s', 'size'), ']');
          s := Trim(LCmdIntf.GetOptionValue('s', 'size'));
          LScale := StrToIntDef(s, 40);
        end;
        (*
        if LCmdIntf.HasOption('f', 'fischerandom') then
        begin
          DebugLn('fischerandom = [', LCmdIntf.GetOptionValue('f', 'fischerandom'), ']');
          s :=Trim(LCmdIntf.GetOptionValue('f', 'fischerandom'));
          try
            FChess960 := StrToBool(s);
          except
          end;
        end;
        *)
        if LCmdIntf.HasOption('w', 'white') then
        begin
          DebugLn('white = [', LCmdIntf.GetOptionValue('w', 'white'), ']');          
        end;
        if LCmdIntf.HasOption('b', 'black') then
        begin
          DebugLn('black = [', LCmdIntf.GetOptionValue('b', 'black'), ']');          
        end;
        if LCmdIntf.HasOption('g', 'green') then
        begin
          DebugLn('green = [', LCmdIntf.GetOptionValue('g', 'green'), ']');          
        end;
        if LCmdIntf.HasOption('r', 'red') then
        begin
          DebugLn('red = [', LCmdIntf.GetOptionValue('r', 'red'), ']');          
        end;
        if LCmdIntf.HasOption('v', 'volume') then
        begin
          DebugLn('volume = [', LCmdIntf.GetOptionValue('v', 'volume'), ']');
          s := Trim(LCmdIntf.GetOptionValue('v', 'volume'));
          LVolume := StrToIntDef(s, 50);
          LVolume := Min(LVolume, 100);
          LVolume := Max(LVolume, 0);
          SetSoundVolume(LVolume);          
        end;
        try
          LArr := LCmdIntf.GetNonOptions('a:b:c:f:g:l:m:p:r:s:u:v:w:', ['autoplay:', 'black:', 'chessboard:', 'font:', 'green:', 'language:', 'movetime:', 'position:', 'red:', 'size:', 'upsidedown:', 'volume:', 'white:']);
        except
          on E: Exception do
            WriteLn('EXCEPTION ' + {$I %FILE%} + ' (' + {$I %LINE%} + '): ' + E.Message);
        end;
        if Length(LArr) = 1 then
        begin
          FEngine := LArr[0];
        end;
      end;
    end;
  end else
    Log('Cannot process command line parameters');
  
  FFenFileName := Concat(LConfigFilesPath, 'eschecs.fen');
  
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
    (*
    AddMenuItem(GetText(txSave), 'Ctrl+S', @SaveGame);
    *)
    AddMenuItem(GetText(txQuit), 'Ctrl+Q', @ItemQuitClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem(GetText(txAbout), '', @OtherItemClicked);
  end;
  with FOptionsSubMenu do
  begin
    AddMenuItem(GetText(txSound), '', @OtherItemClicked).Checked := TRUE;
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
  LFileName := Format('%simages/legend/y/%d.png', [ExtractFilePath(ParamStr(0)), LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName]));
  LLegend := TBGRABitmap.Create(LFileName);
  FYLegend.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FXLegendInv := TBGRABitmap.Create(8 * LScale, LScale div 2, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/x/inv/%d.png', [ExtractFilePath(ParamStr(0)), LScale]);
  Assert(FileExists(LFileName), Format('File not found: %s', [LFileName])); 
  LLegend := TBGRABitmap.Create(LFileName);
  FXLegendInv.PutImage(0, 0, LLegend, dmDrawWithTransparency);
  LLegend.Free;
  
  FYLegendInv := TBGRABitmap.Create(LScale div 2, 8 * LScale, ColorToBGRA(clWindowBackground));
  LFileName := Format('%simages/legend/y/inv/%d.png', [ExtractFilePath(ParamStr(0)), LScale]);
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
  
  Log(Format('Eschecs %s %s %s %s %s FPC %s fpGUI %s BGRABitmap %s', [CVersion, {$I %DATE%}, {$I %TIME%}, {$I %FPCTARGETCPU%}, {$I %FPCTARGETOS%}, {$I %FPCVERSION%}, FPGUI_VERSION, BGRABitmapVersionStr]));
  
  FCheckTimeElapsed := FALSE;
  FTimer := TfpgTimer.Create(10);
  FTimer.OnTimer := @InternalTimerFired;
  FTimer.Enabled := TRUE;
  
  FConnected := FileExists(FEngine)
    and MakeFileExecutableIf(FEngine) (* Make file executable if necessary. *)
    and SetCurrentDir(ExtractFileDir(FEngine))
    and CreateConnectedProcess(ExtractFileName(FEngine));
    
  if FConnected then
  begin
    Log(Format('Engine connected [%s]', [FEngine]));
    LListener.Start;
    Send(MsgUci);
  end else
    ShowMessage(GetText(txConnectionFailure));
  
  if LoadSoundLib < 0 then
  begin
    FOptionsSubMenu.MenuItem(0).Checked := FALSE;
    FOptionsSubMenu.MenuItem(0).Enabled := FALSE;
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
      FChessboard.ScreenRestore;
    end;
  end;
end;

procedure TMainForm.WidgetMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  X, Y: integer;
  LMousePos: TPoint;
  LTolerance: integer;
begin
  if FDragging then
  begin
    LMousePos := AMousePos;
    
    LTolerance := LScale div 3;
    if (LMousePos.X - FDragPos.X < 0 - LTolerance)
    or (LMousePos.X - FDragPos.X > 7 * LScale + LTolerance)
    or (LMousePos.Y - FDragPos.Y < 0 - LTolerance)
    or (LMousePos.Y - FDragPos.Y > 7 * LScale + LTolerance) then
    begin
      DropPiece(LMousePos, TRUE);
      Exit;
    end;
    
    FChessboard.RestorePieceBackground(FMousePos - FDragPos);
    FChessboard.SavePieceBackground(LMousePos - FDragPos);
    FChessboard.DrawPiece(LMousePos - FDragPos, FPieceIndex);
    FChessboardWidget.Invalidate;
    FMousePos := LMousePos;
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
begin
  if not FDragging then
    Exit;
  DropPiece(AMousePos);
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
begin
  if Sender is TfpgMenuItem then
    with TfpgMenuItem(Sender) do
      if Text = GetText(txAbout) then
        ShowMessage('Eschecs ' + CVersion + LineEnding + GetText(txAboutMessage))
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
      if Text = GetText(txSound) then
        Checked := not Checked;
end;

procedure TMainForm.InternalTimerFired(Sender: TObject);
var
  LAnimationTerminated: boolean;
begin
  if FWaiting and FCheckTimeElapsed then
  begin
    if GetTickCount64 - FSendMsgGoTime > FMoveTime then
    begin
      Send(MsgStop);
      FCheckTimeElapsed := FALSE;
    end;
  end;
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
            FCheckTimeElapsed := FALSE;
          end;
        2:
          begin
            FWaitingForReadyOk := 0;
            Send(MsgGo(FMoveTime));
            MouseCursor := mcHourGlass;
            FWaiting := TRUE;
            FStatusBar.Text := GetText(txWaiting);
            FWaitingForUserMove := FALSE;
            FCheckTimeElapsed := TRUE;
            FSendMsgGoTime := GetTickCount64;
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
  if FGame.Check and FChessboard.ScreenSaved then
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
  if (AHistory <> '') and not FChess960 then
  begin
    FOpeningName := GetOpening(AHistory);
    //WindowTitle := FOpeningName;
  end;
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
  if FOptionsSubMenu.MenuItem(0).Checked then
    Play(ASound);
end;

procedure TMainForm.ItemQuitClicked(Sender: TObject);
begin
  DebugLn('ItemQuitClicked');
  FTimer.Enabled := FALSE;
  SaveGame(Sender);
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
  FChessboardWidget.UpdatePosition;
  FTopLegendWidget.UpdatePosition;
  FLeftLegendWidget.UpdatePosition;
  FRightLegendWidget.UpdatePosition;
  FBottomLegendWidget.UpdatePosition;
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
      Log(Format('Start position n. %d.', [ANumber]));
    finally
      Free;
    end
  else
    ShowMessage(Format('File not found: %s', [LFile]));
end;

procedure TMainForm.DropPiece(const AMousePos: TPoint; const ABortMove: boolean);
var
  LType: TPieceType;
  X, Y: integer;
  LSkip: boolean;
begin
  FDragging := FALSE;
  FChessboard.ScreenToXY(AMousePos, X, Y);
  FUserMove := Concat(FUserMove, EncodeSquare(X, Y));
  
  if (FUserMove = 'e1g1') and FGame.IsLegal('e1h1') and FGame.IsCastling('e1h1') then FUserMove := 'e1h1';
  if (FUserMove = 'e1c1') and FGame.IsLegal('e1a1') and FGame.IsCastling('e1a1') then FUserMove := 'e1a1';
  if (FUserMove = 'e8g8') and FGame.IsLegal('e8h8') and FGame.IsCastling('e8h8') then FUserMove := 'e8h8';
  if (FUserMove = 'e8c8') and FGame.IsLegal('e8a8') and FGame.IsCastling('e8a8') then FUserMove := 'e8a8';
  
  if FGame.IsLegal(FUserMove) and not ABortMove then
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
      FChessboard.ScreenSave;
      FChessboard.HighlightMove(FUserMove, FPieceIndex);
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
    Log(Format('UCI protocol accepted [%s, %s]', [LName, LAuthor]));
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
      LForm.FChessboard.ScreenRestore;
      LForm.DoMove(LMove, LType, TRUE, LSkip);
    end else
    begin
      ShowMessage(GetText(txIllegalMove) + LineEnding + LMove);
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

procedure MainProc;
begin
  Assert(DirectoryExists(LConfigFilesPath), Format('Directory not found: %s', [LConfigFilesPath]));
  
  try
    fpgApplication.Initialize;
    fpgImages.AddMaskedBMP('vfd.eschecs', @vfd_eschecs, SizeOf(vfd_eschecs), 0, 0);
    if fpgStyleManager.SetStyle('eschecs') then
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
      Log(E.ClassName + ': ' + E.Message);
  end;
end;

begin
  MainProc;
end.
