
{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}

program Eschecs;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

uses
{$IFDEF UNIX}
  cthreads, 
  cwstring, 
{$ENDIF}
  Classes,
  SysUtils,
  StrUtils,
  RegExpr,
  fpg_base,
  fpg_main,
  fpg_dialogs,
  fpg_menu,
  fpg_widget,
  Images,
  Board,
  Style,
  Utils,
  Language,
  Connect,
  Settings,
  ValidatorCore,
  ChessTypes,
  ChessGame,
  UCI,
  FEN,
  Engines,
  Log,
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
{$IFDEF OPT_SOUND}
  Sound,
{$ENDIF}
{$IFDEF OPT_ECO}
  ECO,
{$ENDIF}
  rcmdline,
  messagefrm,
  fpg_style_eschecs,
  fpg_stylemanager,
  {%units 'Auto-generated GUI code'}
  fpg_form, fpg_panel
  {%endunits}
  ;

{$IFDEF windows}
{$R eschecs.res}
{$ENDIF}

{$WARN 5024 OFF}
{$I version.inc}

type
  TNavigation = (nvPrevious, nvNext, nvLast, nvFirst);
  
  TListener = class(TThread)
  private
    FEngineMessage: string;
    procedure OnEngineMessage;
  protected
    procedure Execute; override;
  end;
  
  TMainForm = class(TfpgForm)
  protected
    FBGRAChessboard: TBGRAChessboard;
    FBoardStyle: TBoardStyle;
    FUpsideDown: boolean;
    FGame: TChessGame;
    FUserMove, FRookMove: string;
    FUserColor: TChessPieceColor;
    FComputerColor: TChessPieceColorEx;
    FWaiting: boolean;
    FExePath: string;
    FEngine: integer;
    FEngineConnected: boolean;
    FMoveHistory: string;
    FPositionHistory: TStringList;
    FCurrPosIndex: integer;
    FTimeAvailable: integer;
    FValidator: TValidator;
    FDragging: boolean;
    FMousePos, FDragPos, FInitPos: TPoint;
    FPieceIndex: integer;
    FCastlingFlag: boolean;
    FWaitingForAnimationEnd: boolean;
    FWaitingForReadyOk: integer;
    procedure HandleKeyPress(var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean); override;
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
    procedure InitForm; 
    procedure WidgetPaint(Sender: TObject);
    procedure WidgetMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure WidgetMouseEnter(Sender: TObject);
    procedure WidgetMouseExit(Sender: TObject);
    procedure WidgetMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure WidgetMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    FChessboardWidget: TfpgWidget;
    FStatusBar: TfpgPanel;
    FMenuBar: TfpgMenuBar;
    FEschecsSubMenu: TfpgPopupMenu;
    FMovesSubMenu: TfpgPopupMenu;
    FBoardSubMenu: TfpgPopupMenu;
    FOptionsSubMenu: TfpgPopupMenu;
    FPromotionSubMenu: TfpgPopupMenu;
    FAudioSubMenu: TfpgPopupMenu;
    FStyleSubMenu: TfpgPopupMenu;
    FLanguageSubMenu: TfpgPopupMenu;
      
    {@VFD_HEAD_END: MainForm}
    FTimer: TfpgTimer;
    procedure ItemExitClicked(Sender: TObject);
    procedure ItemNewGameClicked(Sender: TObject);
    procedure ItemStyleClicked(Sender: TObject);
    procedure OtherItemClicked(Sender: TObject);
    procedure InternalTimerFired(Sender: TObject);
    function DoMove(const aMove: string; const aPromotion: TChessPieceKindEx = cpkNil; aIsComputerMove: boolean = true): boolean;
    procedure OnMoveDone(const aHistory: string = '');
    procedure OnComputerMove;
    procedure OnUserIllegalMove;
    procedure SetComputerColor(const aAutoPlayEnabled: boolean);
    procedure NewPosition(const aPosition: string = FENSTARTPOSITION; const aHistory: string = '');
    function TryNavigate(const aCurrentIndex: integer; const aNavigation: TNavigation): integer;
{$IFDEF OPT_SOUND}
    procedure PlaySound(const aSound: TSound);
{$ENDIF}
    procedure CloseAll(Sender: TObject);
    procedure SaveGame(Sender: TObject);
    procedure OnResized(Sender: TObject);
    procedure ShowMessageFrm(AMessage1, AMessage2, ATitle : string);
  end;
  
{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

{$I icon.inc} 

const
  FIRST_ENGINE_ITEM_INDEX = 3;
  
var
  vListener: TThread;
  vUCILog: text;
  vColoring: boolean;
  vSelectedStyle: TStyle = 0;
  vStyleHasChanged: boolean = FALSE;
  
procedure UCILogAppend(const aText, aInsert: string);
var
  vList: TStringList;
  vLine, vDateTime: string;
begin
  vList := TStringList.Create;
  ExtractStrings([#10, #13], [' '], PChar(aText), vList);
  vDateTime := DateTimeToStr(Now());
  for vLine in vList do
    WriteLn(vUCILog, vDateTime, ' ', aInsert, ' ', vLine);
  Flush(vUCILog);
  vList.Free;
end;

procedure WriteProcessInput_(const aUciCommand: string);
begin
  WriteProcessInput(aUciCommand);
  UCILogAppend(aUciCommand, '<');
end;

procedure TMainForm.HandleKeyPress(var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    KeyLeft, KeyBackspace:
      FCurrPosIndex := TryNavigate(FCurrPosIndex, nvPrevious);
    KeyRight:
      FCurrPosIndex := TryNavigate(FCurrPosIndex, nvNext);
    KeyUp:
      FCurrPosIndex := TryNavigate(FCurrPosIndex, nvLast);
    KeyDown:
      FCurrPosIndex := TryNavigate(FCurrPosIndex, nvFirst);
    KeyEscape:
      ItemExitClicked(nil);
  end;
end;

destructor TMainForm.Destroy;
begin
  FBGRAChessboard.Free;
  FGame.Free;
  if FEngineConnected then
  begin
    WriteProcessInput_(MsgQuit());
    FreeConnectedProcess;
    vListener.Terminate;
    vListener.WaitFor;
    vListener.Free;
  end;
  FPositionHistory.Free;
  FValidator.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  fpgImages.AddMaskedBMP('vfd.eschecs', @vfd_eschecs, sizeof(vfd_eschecs), 0, 0);
 
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(351, 150, 640, 495);
  WindowTitle := 'Eschecs';
  IconName := 'vfd.eschecs';
  BackGroundColor := $80000001;
  Hint := '';
  WindowPosition := wpOneThirdDown;
  OnResize := @onresized;

  FChessboardWidget := TfpgWidget.Create(self);
  with FChessboardWidget do
  begin
    Name := 'FChessboardWidget';
    SetPosition(0, 0, 640, 472);
    BackgroundColor := clNone;
    OnPaint := @WidgetPaint;
    OnMouseDown := @WidgetMouseDown;
    OnMouseUp := @WidgetMouseUp;
    OnMouseMove := @WidgetMouseMove;
    OnMouseEnter := @WidgetMouseEnter;
    OnMouseExit := @WidgetMouseExit;
  end;

  FStatusBar := TfpgPanel.Create(self);
  with FStatusBar do
  begin
    Name := 'FStatusBar';
    SetPosition(0, 471, 640, 24);
    Align := alBottom;
    Alignment := taLeftJustify;
    BackgroundColor := TfpgColor($FFFFFF);
    FontDesc := '#Label1';
    ParentShowHint := False;
    Style := bsLowered;
    Text := '';
    TextColor := TfpgColor($000000);
    Hint := '';
  end;

  FMenuBar := TfpgMenuBar.Create(self);
  with FMenuBar do
  begin
    Name := 'FMenuBar';
    SetPosition(0, 0, 640, 28);
    Align := alTop;
  end;

  FEschecsSubMenu := TfpgPopupMenu.Create(self);
  with FEschecsSubMenu do
  begin
    Name := 'FEschecsSubMenu';
    SetPosition(68, 56, 228, 28);
  end;

  FMovesSubMenu := TfpgPopupMenu.Create(self);
  with FMovesSubMenu do
  begin
    Name := 'FMovesSubMenu';
    SetPosition(68, 268, 228, 28);
  end;

  FBoardSubMenu := TfpgPopupMenu.Create(self);
  with FBoardSubMenu do
  begin
    Name := 'FBoardSubMenu';
    SetPosition(68, 220, 228, 28);
  end;

  FOptionsSubMenu := TfpgPopupMenu.Create(self);
  with FOptionsSubMenu do
  begin
    Name := 'FOptionsSubMenu';
    SetPosition(68, 168, 228, 28);
  end;
  
  FAudioSubMenu := TfpgPopupMenu.Create(self);
  with FAudioSubMenu do
  begin
    Name := 'FAudioSubMenu';
    SetPosition(68, 168, 228, 28);
  end;
  
  FStyleSubMenu := TfpgPopupMenu.Create(self);
  with FStyleSubMenu do
  begin
    Name := 'FStyleSubMenu';
    SetPosition(68, 168, 228, 28);
  end;
  
  FLanguageSubMenu := TfpgPopupMenu.Create(self);
  with FLanguageSubMenu do
  begin
    Name := 'FLanguageSubMenu';
    SetPosition(68, 168, 228, 28);
  end;

  FPromotionSubMenu := TfpgPopupMenu.Create(self);
  with FPromotionSubMenu do
  begin
    Name := 'FPromotionSubMenu';
    SetPosition(68, 112, 228, 28);
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}  
  
  InitForm;
end;

procedure TMainForm.InitForm; 
const
  MENU_BAR_HEIGHT = 24;
{$if defined(cpu64) and defined(Windows)}
  DEFAULT_TITLE = 'Eschecs win64';
{$endif}
{$if defined(cpu86) and defined(Windows)}
  DEFAULT_TITLE = 'Eschecs win32';
{$endif}
{$if defined(cpu64) and defined(Linux)}
  DEFAULT_TITLE = 'Eschecs lin64';
{$endif}
{$if defined(cpu86) and defined(Linux)}
  DEFAULT_TITLE = 'Eschecs lin32';
{$endif}
{$if defined(cpu64) and defined(freebsd)}
  DEFAULT_TITLE = 'Eschecs bsd64';
{$endif}
{$if defined(cpu86) and defined(freebsd)}
  DEFAULT_TITLE = 'Eschecs bsd32';
{$endif}
{$if defined(linux) and defined(cpuarm)}
  DEFAULT_TITLE = 'Eschecs arm';
{$endif}
var
  vCurrentPosition: string;
  vAutoPlay, vMarble: boolean;
  vIndex, x: integer;
  vENGPath: TFileName;
begin
  with TCommandLineReader.Create do
  try
    DeclareInt('style', '', 0);
    DeclareInt('coloring', '', 1);
    Parse(CmdLine);
    gStyle := ReadInt('style');
    vColoring := boolean(ReadInt('coloring'));
  finally
    Free;
  end;
  
  vENGPath := ChangeFileExt(ParamStr(0), '.eng');
  if FileExists(vENGPath) then
    LoadEnginesDataFromINI(vENGPath)
  else
    LoadEnginesData('engines.json');
  
  ReadFromINIFile(vCurrentPosition, vAutoPlay, FUpsideDown, vMarble, FExePath, FMoveHistory, FCurrPosIndex, FEngine, vLightSquareColor, vDarkSquareColor, vSpecialColors[ocGreen], vSpecialColors[ocRed], gStyle);
  
  FValidator := TValidator.Create;
  Assert(FValidator.IsFEN(vCurrentPosition));
  FTimeAvailable := 1000;
  FPositionHistory := TStringList.Create;
 
  if FileExists(vFENPath) then
    FPositionHistory.LoadfromFile(vFENPath)
  else
    FPositionHistory.Append(FENSTARTPOSITION);
    
  with FMenuBar do
  begin
    AddMenuItem(TEXTS[txEschecs], nil).SubMenu := FEschecsSubMenu;
    AddMenuItem(TEXTS[txMoves], nil).SubMenu := FMovesSubMenu;
    AddMenuItem(TEXTS[txBoard], nil).SubMenu := FBoardSubMenu;
    AddMenuItem(TEXTS[txOptions], nil).SubMenu := FOptionsSubMenu;
    AddMenuItem(TEXTS[txPromotion], nil).SubMenu := FPromotionSubMenu;
  end;  
  
   with FEschecsSubMenu do
  begin
    AddMenuItem(TEXTS[txSave], 'Ctrl+S', @savegame);
    AddMenuItem( TEXTS[txSave] + ' + ' + TEXTS[txquit], 'Esc', @ItemExitClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem(TEXTS[txQuit], 'Ctrl+Q', @closeall);
    AddMenuItem('-', '', nil);
    AddMenuItem(TEXTS[txAbout], '', @OtherItemClicked);
  end;
  
  with FOptionsSubMenu do
  begin
   AddMenuItem(TEXTS[txStyle], '',nil).SubMenu := FStyleSubMenu;
   AddMenuItem(TEXTS[txLanguage], '', nil).SubMenu := FLanguageSubMenu;
   AddMenuItem(TEXTS[txSound], '', nil).SubMenu := FAudioSubMenu;  
  end; 
  
   with FStyleSubMenu do
  begin
      for vIndex := Low(TStyle) to High(TStyle) do
      AddMenuItem(STYLENAME[vIndex], '', @ItemStyleClicked).Checked := vIndex = gStyle;
   end;
  
   with FLanguageSubMenu do
  begin
   // Waring dummy methods...
   x:=0;
   AddMenuItem('Language ' + inttostr(x), '', nil).Checked := true;
   for x := 1 to 9 do
    AddMenuItem('Language ' + inttostr(x), '', nil).Checked := false;
   end; 
  
  with FAudioSubMenu do
  begin
  Enabled := FALSE;
  with AddMenuItem('Enabled', '', @OtherItemClicked) do
    begin
      Checked := FALSE;
      Enabled := FALSE;
    end;
    
    
  end; 
    
  with FBoardSubMenu do
  begin
    AddMenuItem(TEXTS[txNew], '', @ItemNewGameClicked);
    AddMenuItem(TEXTS[txFlip], '', @OtherItemClicked);
  end;
  
  with FMovesSubMenu do
  begin
    AddMenuItem(TEXTS[txComputerMove], '', @OtherItemClicked);
    AddMenuItem(TEXTS[txAutoPlay], '', @OtherItemClicked).Checked := vAutoPlay;
    AddMenuItem('-', '', nil);
    for vIndex := 0 to High(vEngines) do with AddMenuItem(vEngines[vIndex].vName, '', @OtherItemClicked) do
    begin
     Enabled := vEngines[vIndex].vExists;
     Checked := FALSE;
     if (FEngine = -1) and (Pos(UpperCase('Fruit'), UpperCase(vEngines[vIndex].vName)) > 0) then
       FEngine := vIndex;
    end;
{$IFDEF DEBUG}
    WriteLn('FEngine=', FEngine);
{$ENDIF}
  end;  
  
  with FPromotionSubMenu do
  begin
    AddMenuItem(TEXTS[txKnight], '', @OtherItemClicked).Checked := FALSE;
    AddMenuItem(TEXTS[txBishop], '', @OtherItemClicked).Checked := FALSE;
    AddMenuItem(TEXTS[txRook], '', @OtherItemClicked).Checked := FALSE;
    AddMenuItem(TEXTS[txQueen], '', @OtherItemClicked).Checked := TRUE;
  end;
  
  SetPosition(0, 0, 8 * gStyleData[gStyle].scale, 24 + 8 * gStyleData[gStyle].scale + 24);
  WindowTitle := DEFAULT_TITLE;
  MinWidth := 8 * gStyleData[gStyle].scale;
  MinHeight := 24 + 8 * gStyleData[gStyle].scale + 24;
  
  FChessboardWidget.SetPosition(0, MENU_BAR_HEIGHT, 8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale);
  FStatusBar.SetPosition(0, 24 + 8 * gStyleData[gStyle].scale, 8 * gStyleData[gStyle].scale, 24);
  FMenuBar.SetPosition(0, 0, 8 * gStyleData[gStyle].scale, 24);
     
  FBoardStyle := TBoardStyle(Ord(vMarble));
  FBGRAChessboard := TBGRAChessboard.Create(FBoardStyle, FUpsideDown, vCurrentPosition);
  
  FGame := TChessGame.Create(vCurrentPosition);
  FUserMove := '';
  FRookMove := '';
  OnMoveDone(IfThen(FCurrPosIndex = 0, '', Copy(FMoveHistory, 1, 4 * FCurrPosIndex)));
  SetComputerColor(FMovesSubMenu.MenuItem(1).Checked);
  vListener := TListener.Create(TRUE);
  vListener.Priority := tpHigher;
  FWaiting := FALSE;
  FEngineConnected := FALSE;
  FCastlingFlag := FALSE;
  FWaitingForAnimationEnd := FALSE;
  FWaitingForReadyOk := 0;
  TLog.Append(Format('Eschecs %s %s %s FPC %s', [VERSION, {$I %DATE%}, {$I %TIME%}, {$I %FPCVERSION%}]));
  
  FTimer := TfpgTimer.Create(10);
  FTimer.OnTimer := @InternalTimerFired;
  FTimer.Enabled := TRUE;
  
  with FMovesSubMenu do if MenuItem(FEngine + FIRST_ENGINE_ITEM_INDEX).Enabled then
    OtherItemClicked(MenuItem(FEngine + FIRST_ENGINE_ITEM_INDEX))
  else
  begin
{$IFDEF DEBUG}
    WriteLn('MenuItem(1).Checked=', MenuItem(1).Checked);
    WriteLn('MenuItem(', FEngine + FIRST_ENGINE_ITEM_INDEX,').Enabled=', MenuItem(FEngine + FIRST_ENGINE_ITEM_INDEX).Enabled);
{$ENDIF}
  end;
end;

procedure TMainForm.WidgetPaint(Sender: TObject);
begin
  with FBGRAChessboard do
    DrawToFPGCanvas(FChessboardWidget.Canvas, 0, 0);
end;

procedure TMainForm.WidgetMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  X, Y: integer;
begin
  if FGame.state = csProgress then
  begin
    FMousePos := AMousePos;
    FDragPos.X := AMousePos.X mod gStyleData[gStyle].scale;
    FDragPos.Y := AMousePos.Y mod gStyleData[gStyle].scale;
    FInitPos := AMousePos - FDragPos;
    FBGRAChessboard.ScreenToXY(AMousePos, X, Y);
    FPieceIndex := FBGRAChessboard.FindPiece(X, Y, TChessPieceColor(Ord(FGame.ActiveColor)));
    if FPieceIndex > 0 then
    begin
      FUserMove := EncodeSquare(X, Y);
      FDragging := True;
      FBGRAChessboard.SavePieceBackground(FInitPos, TRUE);
      if vColoring then
        FBGRAChessboard.ScreenRestore;
    end;
  end;
end;

procedure TMainForm.WidgetMouseEnter(Sender: TObject);
begin
  //TfpgWidget(Sender).MouseCursor := mcHand;
end;

procedure TMainForm.WidgetMouseExit(Sender: TObject);
begin
  //TfpgWidget(Sender).MouseCursor := mcDefault;
end;

procedure TMainForm.WidgetMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  X, Y: integer;
begin
  if FDragging then
  begin
    FBGRAChessboard.RestorePieceBackground(FMousePos - FDragPos);
    FBGRAChessboard.SavePieceBackground(AMousePos - FDragPos);
    FBGRAChessboard.DrawPiece(AMousePos - FDragPos, FPieceIndex);
    FChessboardWidget.Invalidate;
    FMousePos := AMousePos;
  end else
  begin
    FBGRAChessboard.ScreenToXY(AMousePos, X, Y);
    if FBGRAChessboard.FindPiece(X, Y, TChessPieceColor(Ord(FGame.ActiveColor))) > 0 then
      TfpgWidget(Sender).MouseCursor := mcHand
    else
      TfpgWidget(Sender).MouseCursor := mcDefault;
  end;
end;

procedure TMainForm.WidgetMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  vPromotion: TChessPieceKind;
  X, Y: integer;
begin
{$IFDEF DEBUG}
  WriteLn('TMainForm.WidgetMouseUp()');
{$ENDIF}
  if not FDragging then
    Exit;
  FDragging := False;
  FBGRAChessboard.ScreenToXY(AMousePos, X, Y);
  FUserMove := Concat(FUserMove, EncodeSquare(X, Y));
  if FGame.IsLegal(FUserMove) then
  begin
    if      FPromotionSubMenu.MenuItem(0).Checked then vPromotion := cpkKnight
    else if FPromotionSubMenu.MenuItem(1).Checked then vPromotion := cpkBishop
    else if FPromotionSubMenu.MenuItem(2).Checked then vPromotion := cpkRook
    else if FPromotionSubMenu.MenuItem(3).Checked then vPromotion := cpkQueen;
    FBGRAChessboard.RestorePieceBackground(FMousePos - FDragPos);
    if DoMove(FUserMove, vPromotion, FALSE) then
      FBGRAChessboard.SetPieceKind(FPieceIndex, vPromotion);
    FBGRAChessboard.SetPieceXY(FPieceIndex, X, Y);
    FBGRAChessboard.DrawPiece(FBGRAChessboard.XYToScreen(X, Y), FPieceIndex);
    if vColoring then
    begin
      if FCastlingFlag then
        FCastlingFlag := FALSE
      else
      begin
        FBGRAChessboard.ScreenSave;
        FBGRAChessboard.HighlightMove(FUserMove, FPieceIndex);
      end;
    end;
    FChessboardWidget.Invalidate;
    OnMoveDone(IfThen(FCurrPosIndex = 0, '', Copy(FMoveHistory, 1, 4 * FCurrPosIndex)));
  end else
  begin  
    FBGRAChessboard.RestorePieceBackground(FMousePos - FDragPos);
    FBGRAChessboard.DrawPiece(FInitPos, FPieceIndex);
    FChessboardWidget.Invalidate;
    OnUserIllegalMove;
  end;
end;

procedure TMainForm.ItemExitClicked(Sender: TObject);
begin
SaveGame(sender);
Close;
end;

procedure TMainForm.ItemNewGameClicked(Sender: TObject);
begin
  NewPosition(FENSTARTPOSITION);
  SetLength(FMoveHistory, 0);
  FPositionHistory.Clear;
  FPositionHistory.Append(FENSTARTPOSITION);
  FCurrPosIndex := 0;
end;

procedure TMainForm.ItemStyleClicked(Sender: TObject);
const
  FIRST_ITEM_INDEX = 2;
var
  vStyle: integer;
begin
  for vStyle := Low(TStyle) to High(TStyle) do if STYLENAME[vStyle] = TfpgMenuItem(Sender).Text then
    vSelectedStyle := vStyle;
{$IFDEF DEBUG}
  WriteLn('vSelectedStyle=', vSelectedStyle);
{$ENDIF}
  for vStyle := Low(TStyle) to High(TStyle) do
  FStyleSubMenu.MenuItem(vStyle).Checked := vStyle = vSelectedStyle;
  vStyleHasChanged := TRUE;
  ShowMessagefrm(TEXTS[txStyleInfo], '',  TEXTS[txTitleMessage])
end;

procedure TMainForm.OtherItemClicked(Sender: TObject);
var
  i, j: integer;
begin
  if Sender is TfpgMenuItem then
    with TfpgMenuItem(Sender) do
      if Text = TEXTS[txHelp] then
        ShowMessagefrm(TEXTS[txHelpMessage], '',  TEXTS[txHelp])
      else
      if Text = TEXTS[txAbout] then
        ShowMessagefrm('Eschecs ' + VERSION, TEXTS[txAboutMessage], TEXTS[txAbout])
      else
      if Text = TEXTS[txComputerMove] then
        FComputerColor := FGame.ActiveColor
      else
      if Text = TEXTS[txAutoPlay] then begin
        Checked := not Checked;
        SetComputerColor(Checked);
      end else
      if Text = TEXTS[txFlip] then begin
        FBGRAChessboard.ScreenRestore;
        FBGRAChessboard.FlipBoard;
        FChessboardWidget.Invalidate;
        FBGRAChessboard.ScreenSave;
        FUpsideDown := FBGRAChessboard.isUpsideDown;
      end else
      if Text = TEXTS[txColoring]
      then
      begin
        Checked := not Checked;
      end
      else
      if Text = TEXTS[txSound]
      then
      begin
{$IFDEF OPT_SOUND}
        Checked := not Checked;
{$ENDIF}
      end
      else
      if (Text = TEXTS[txKnight])
      or (Text = TEXTS[txBishop])
      or (Text = TEXTS[txRook])
      or (Text = TEXTS[txQueen]) then
      begin
        for i := 0 to 3 do
          FPromotionSubMenu.MenuItem(i).Checked := FALSE;
        Checked := TRUE;
      end else
      for i := 0 to High(vEngines) do
        if Text = vEngines[i].vName then
        begin
          for j := 0 to High(vEngines) do
            FMovesSubMenu.MenuItem(j + FIRST_ENGINE_ITEM_INDEX).Checked := j = i;
          if FEngineConnected then
          begin
            WriteProcessInput_(MsgQuit());
            FreeConnectedProcess;
          end;
          FEngineConnected :=
            FileExists(Concat(vEngines[i].vDirectory, vEngines[i].vCommand))
            and SetCurrentDir(vEngines[i].vDirectory)
            and CreateConnectedProcess(vEngines[i].vCommand);
          if FEngineConnected then
          begin
            TLog.Append('Connexion établie.');
            vListener.Start;
            WriteProcessInput_(MsgUCI());
            FEngine := i;
          end else
            ShowMessagefrm(TEXTS[txConnectionFailure], '',  TEXTS[txTitleMessage]);
        end;
end;

procedure TMainForm.InternalTimerFired(Sender: TObject);
var
  vAnimationTerminated: boolean;
begin
  if FBGRAChessboard.Animate(vAnimationTerminated) then
    FChessboardWidget.Invalidate
  else
    if FRookMove <> '' then
    begin
      FBGRAChessboard.MovePiece(FRookMove);
      FRookMove := '';
    end else
      if FEngineConnected
      and (FComputerColor = FGame.ActiveColor)
      and (FGame.state = csProgress)
      and not FWaiting then
      begin
{$IFDEF DEBUG}
        WriteLn('FWaitingForReadyOk = ', FWaitingForReadyOk);
{$ENDIF}
        case FWaitingForReadyOk of
          0:
            begin
              FWaitingForReadyOk := 1;
              WriteProcessInput_(MsgPosition(FGame.FENRecord));
              WriteProcessInput_(MsgIsReady());
            end;
          1:
            begin
            end;
          2:
            begin
              FWaitingForReadyOk := 0;
              WriteProcessInput_(MsgGo(FTimeAvailable));
              MouseCursor := mcHourGlass;
              FWaiting := TRUE;
              FStatusBar.Text := Concat(' ', TEXTS[txWaiting]);
            end;
        end;
      end;
  
  if FWaitingForAnimationEnd and vAnimationTerminated then
  begin
    FWaitingForAnimationEnd := FALSE;
    OnComputerMove;
  end;
end;

function TMainForm.DoMove(const aMove: string; const aPromotion: TChessPieceKindEx; aIsComputerMove: boolean = true): boolean;
const
  SYMBOLS: array[cpkKnight..cpkQueen] of char = ('n', 'b', 'r', 'q');
var
  vX, vY: integer;
  vSquare: string;
  vPromotion: TChessPieceKind;
  vSymbol: string;
begin
{$IFDEF DEBUG}
  WriteLn(Format('TMainForm.DoMove(%s, %s, %s)', [
    aMove,
    GetEnumName(TypeInfo(TChessPieceKindEx), Ord(aPromotion)),
    BoolToStr(aIsComputerMove, TRUE)
  ]));
{$ENDIF}
  vSquare := Copy(aMove, 3, 2);
  DecodeSquare(vSquare, vX, vY);
  if FBGRAChessboard.FindPiece(vX, vY) > 0 then
    FBGRAChessboard.ErasePiece(vSquare);
  
  vSquare := FGame.IsEnPassant(aMove);
  if vSquare <> '' then
    FBGRAChessboard.ErasePiece(vSquare);
  
  result := FGame.IsPromotion(aMove);
  if result then
  begin
    vPromotion := ValidPromotionValue(aPromotion);
    vSymbol := SYMBOLS[vPromotion];
    if aIsComputerMove then
      FBGRAChessboard.MovePiece(aMove, TRUE, vPromotion);
  end else
  begin
    vSymbol := '';
    FRookMove := FGame.IsCastling(aMove);
    if vColoring then
    begin
      FCastlingFlag := Length(FRookMove) > 0;
      vMoveToBeHighlighted := aMove;
      vComputerCastlingFlag := FCastlingFlag and aIsComputerMove;
      if FCastlingFlag then
      begin
        DecodeSquare(Copy(aMove, 1, 2), vX, vY);
        vKingIndex := FBGRAChessboard.FindPiece(vX, vY);
      end;
    end;
    if aIsComputerMove then
      FBGRAChessboard.MovePiece(aMove, FALSE);
  end;
  
  FGame.PlayMove(Concat(aMove, vSymbol));
  
  FMoveHistory := Concat(Copy(FMoveHistory, 1, 4 * FCurrPosIndex), aMove);
  while FPositionHistory.Count > Succ(FCurrPosIndex) do
    FPositionHistory.Delete(FPositionHistory.Count - 1);
  FPositionHistory.Append(FGame.FENRecord);
  Inc(FCurrPosIndex);
end;

procedure TMainForm.OnMoveDone(const aHistory: string);
var
  vX, vY: integer;
  vIndex: integer;
  vOpeningName: string;
begin
{$IFDEF DEBUG}
  WriteLn('TMainForm.OnMoveDone()');
{$ENDIF}
  if vColoring and FGame.Check and FBGRAChessboard.ScreenSaved() then
  begin
    FGame.GetKingCheckedXY(vX, vY);
    vIndex := FBGRAChessboard.FindPiece(vX, vY);
    FBGRAChessboard.Highlight(vX, vY, ocRed, vIndex);
    FChessboardWidget.Invalidate;
  end;
{$IFDEF OPT_SOUND}
    if FGame.state in [csCheckmate, csStalemate, csDraw] then
      PlaySound(sndEndOfGame)
    else if FGame.Check and FALSE then
      PlaySound(sndCheck)
    else if FALSE then // <--- to do
      PlaySound(sndPromotion)
    else if FALSE then // <--- to do
      PlaySound(sndCapture)
    else if FALSE then // <--- to do
      PlaySound(sndMove);
{$ENDIF}
  FStatusBar.Text := Concat(
    ' ',
    ArbitratorMessage(FGame.Check, FGame.ActiveColor, FGame.state)
  );
{$IFDEF OPT_ECO}
  vOpeningName := ECO.GetOpening(aHistory);
  if Length(vOpeningName) > 0 then
    TLog.Append(Format('Ouverture "%s".', [vOpeningName]));
{$ENDIF}
end;

procedure TMainForm.OnComputerMove;
begin
  if not FMovesSubMenu.MenuItem(1).Checked then
    FComputerColor := cpcNil;
  MouseCursor := mcHand;
  OnMoveDone(IfThen(FCurrPosIndex = 0, '', Copy(FMoveHistory, 1, 4 * FCurrPosIndex)));
  FWaiting := FALSE;
end;

procedure TMainForm.OnUserIllegalMove;
begin
{$IFDEF OPT_SOUND}
  PlaySound(sndIllegal);
{$ENDIF}
end;

procedure TMainForm.SetComputerColor(const aAutoPlayEnabled: boolean);
begin
  if aAutoPlayEnabled then
    FComputerColor := TChessPieceColor(1 - Ord(FGame.ActiveColor))
  else
    FComputerColor := cpcNil;
end;

procedure TMainForm.NewPosition(const aPosition: string; const aHistory: string);
begin
  FBGRAChessboard.Create(FBoardStyle, FUpsideDown, aPosition);
  FGame.Create(aPosition);
  OnMoveDone(aHistory);
  SetComputerColor(FMovesSubMenu.MenuItem(1).Checked);
  FChessboardWidget.Invalidate;
end;

function TMainForm.TryNavigate(const aCurrentIndex: integer; const aNavigation: TNavigation): integer;
begin
  result := aCurrentIndex;
  case aNavigation of
    nvPrevious:
      if aCurrentIndex > 0 then
        result := Pred(aCurrentIndex);
    nvNext:
      if aCurrentIndex < Pred(FPositionHistory.Count) then
        result := Succ(aCurrentIndex);
    nvLast:
      if aCurrentIndex < Pred(FPositionHistory.Count) then
        result := Pred(FPositionHistory.Count);
    nvFirst:
      if aCurrentIndex > 0 then
        result := 0;
  end;
  if result <> aCurrentIndex then
    NewPosition(
      FPositionHistory[result],
      IfThen(result = 0, '', Copy(FMoveHistory, 1, 4 * result))
    );
end;

{$IFDEF OPT_SOUND}
procedure TMainForm.PlaySound(const aSound: TSound);
begin
  if FAudioSubMenu.MenuItem(1).Checked then
    Play(aSound);
end;
{$ENDIF}

procedure TMainForm.CloseAll(Sender: TObject);
begin
close;
end;

procedure TMainForm.SaveGame(Sender: TObject);
begin
  if vStyleHasChanged then
  begin
    gStyle := vSelectedStyle;
{$IFDEF DEBUG}
    WriteLn(Format('gStyle=%d', [gStyle]));
{$ENDIF}
  end;
  WriteToINIFile(
    FGame.FENRecord,
    FMovesSubMenu.MenuItem(1).Checked,
    FUpsideDown,
    FBoardStyle = bsMarble,
    FExePath,
    FMoveHistory,
    FCurrPosIndex,
    FEngine,
    vLightSquareColor, vDarkSquareColor, vSpecialColors[ocGreen], vSpecialColors[ocRed],
    gStyle
  );
  FPositionHistory.SaveToFile(vFENPath);
end;

procedure TMainForm.OnResized(Sender: TObject);
begin
 FChessboardWidget.top := (height -FChessboardWidget.height) div 2; 
 FChessboardWidget.left := (width - FChessboardWidget.width) div 2; 
 FChessboardWidget.updatewindowposition; 
end;

procedure TMainForm.ShowMessageFrm(AMessage1, AMessage2, ATitle : string);
var
 msgfrm : Tmessagefrm;
begin
  fpgApplication.CreateForm(Tmessagefrm, msgfrm);
  try
    msgfrm.Button1.text := TEXTS[txQuit];
   // msgfrm.WindowTitle := ATitle;
    msgfrm.ShowMessageFrm(AMessage1, AMessage2, ATitle);
    msgfrm.ShowModal;
  finally
    msgfrm.Free;
  end;
end;

var
  frm: TMainForm;

procedure TListener.Execute;
const
  DELAY = 100;
begin
  while not Terminated do
  begin
    FEngineMessage := ReadProcessOutput; if FEngineMessage <> '' then Synchronize(@OnEngineMessage);
    FEngineMessage := ReadProcessError;  if FEngineMessage <> '' then Synchronize(@OnEngineMessage);
    Sleep(DELAY);
  end;
end;

procedure TListener.OnEngineMessage;
var
  vName, vAuthor, vMove, vPromotion: string;
  vPieceKind: TChessPieceKindEx;
begin
  UCILogAppend(FEngineMessage, '>');
  
  if IsMsgUciOk(FEngineMessage, vName, vAuthor) then
   begin
    TLog.Append(Format('Protocole accepté. Moteur "%s". Auteur "%s".', [vName, vAuthor]));
    //WriteProcessInput_(MsgNewGame());
    frm.WindowTitle := vName;
  end else
  
  if IsMsgBestMove(FEngineMessage, vMove, vPromotion) then
  begin
    if frm.FGame.IsLegal(vMove) then
    begin
      if Length(vPromotion) = 1 then
        case vPromotion[1] of
          'n': vPieceKind := cpkKnight;
          'b': vPieceKind := cpkBishop;
          'r': vPieceKind := cpkRook;
          'q': vPieceKind := cpkQueen;
        end
      else
        vPieceKind := cpkNil;
      
      if vColoring then
        frm.FBGRAChessboard.ScreenRestore;
      frm.DoMove(vMove, vPieceKind);
    end else
    begin
      frm.ShowMessagefrm(TEXTS[txIllegalMove], vMove,  TEXTS[txTitleMessage]);
      frm.FMovesSubMenu.MenuItem(1).Checked := FALSE;
      frm.FComputerColor := cpcNil;
    end;
    frm.FWaitingForAnimationEnd := TRUE;
  end else
  
  if IsMsgReadyOk(FEngineMessage) then
  begin
    Assert(frm.FWaitingForReadyOk = 1);
    frm.FWaitingForReadyOk := 2;
  end;
end;

const
  UCI_LOG = 'eschecs.debug';
  
begin
  Assign(vUCILog, UCI_LOG);
  if FileExists(UCI_LOG) then
    Append(vUCILog)
  else
    Rewrite(vUCILog);
   
  fpgApplication.Initialize;
  
  if fpgStyleManager.SetStyle('eschecs_style') then
  fpgStyle := fpgStyleManager.Style;
  
  fpgApplication.CreateForm(TMainForm, frm);
 
  frm.Show;
  
  fpgApplication.Run;
 
  frm.Free;
  
  Close(vUCILog);
end.
