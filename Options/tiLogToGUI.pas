{
  Log to a window above the application's main form, but only if
  the -lv parameter is passed on the command line
  
  This in normally controlled by the tiLogReg unit.
}
unit tiLogToGUI;

{$I tiDefines.inc}

interface
uses
  Classes
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows, Messages
  {$ENDIF MSWINDOWS}
  ,Graphics, Controls, Forms
  ,Dialogs, StdCtrls, Menus, ToolWin, ComCtrls
  ,tiLog
 ;


type
  TtiLogToGUI = class(TtiLogToCacheAbs)
  private
    FForm: TForm;
    FMemoLog: TMemo;
    FToolBar: TToolBar;
    FPopupMenu: TPopupMenu;
    FLogMenuItem: TMenuItem;
    FViewLogMenuItem: TMenuItem;
    FWordWrapMenuItem: TMenuItem;
    function    GetFormParent: TWinControl;
    procedure   SetFormParent(const AValue: TWinControl);
    function    CreateForm: TForm;
    procedure   FormClearMenuItemClick(Sender: TObject);
    procedure   FormWordWrapMenuItemClick(Sender: TObject);
    procedure   FormLogMenuItemClick(Sender: TObject);
    procedure   FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure   FormLogLevelMenuItemClick(Sender: TObject);
    procedure   FormLogLevelButtonClick(Sender: TObject);
    procedure   DoViewLogFile(Sender: TObject);
    procedure   DoOnPopup(Sender: TObject);
  protected
    procedure   WriteToOutput; override;
    procedure   SetSevToLog(const AValue: TtiSevToLog); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    FormParent: TWinControl read GetFormParent write SetFormParent;
    procedure   Log(const ADateTime, AThreadID, AMessage: string; ASeverity: TtiLogSeverity); override;
  end;


implementation
uses
  tiUtils
  ,tiCommandLineParams
  {$IFDEF FPC}
  ,LCLProc
  ,LResources
  {$ENDIF}
  ,tiGUIConstants
 ;


{ TtiLogToGUI }

constructor TtiLogToGUI.Create;
begin
  inherited Create;
  FForm := CreateForm;
  {$IFNDEF FPC}
    FForm.Visible := true;
  {$ENDIF}
  ThrdLog.Resume;
end;


destructor TtiLogToGUI.Destroy;
begin
  // 1. If the log's parent property was set, then
  //    the form may have been freed elswhere, so
  //    don't call FForm.Free, call FreeAndNil(FForm)
  // 2. Do not change the sequence. Inherited must
  //    be called first to close down any threads.
  inherited;
  FreeAndNil(FForm);
end;


function TtiLogToGUI.CreateForm: TForm;
var
  lMenuItem  : TMenuItem;
  lLogSev    : TtiLogSeverity;
  lToolButton : TToolButton;
begin
  FForm                     := TForm.Create(nil);
  FForm.Position            := poDesigned;
  FForm.Top                 := 10;
  FForm.Left                := 10;
  FForm.Height              := 150;
  FForm.Width               := Screen.Width - 20;
  FForm.Caption             := 'Application event log - ' + Application.Title;
  FForm.OnCloseQuery        := FormCloseQuery;


  FPopupMenu                := TPopupMenu.Create(FForm);
  FPopupMenu.Name           := 'PopupMenu';
  FPopupMenu.OnPopup        := DoOnPopup;

  FMemoLog                  := TMemo.Create(FForm);
  FMemoLog.Name             := 'MemoLog';
  FMemoLog.Parent           := FForm;
  FMemoLog.Top              := 29;
  FMemoLog.Align            := alClient;
  FMemoLog.Font.Name        := cDefaultFixedFontName;
  FMemoLog.PopupMenu        := FPopupMenu;
  FMemoLog.ReadOnly         := True;
  FMemoLog.ScrollBars       := ssBoth;
  FMemoLog.TabOrder         := 0;
  FMemoLog.WordWrap         := False;
  FMemoLog.Clear;

  FToolBar                  := TToolBar.Create(FForm);
  FToolBar.Name             := 'ToolBar';
  FToolBar.Parent           := FForm;
  FToolBar.AutoSize         := True;
  FToolBar.Caption          := '';
  FToolBar.ShowCaptions     := True;
  FToolBar.TabOrder         := 1;

  FViewLogMenuItem          := TMenuItem.Create(FForm);
  FViewLogMenuItem.Name     := 'Viewlogfile1';
  FViewLogMenuItem.Caption  := '&View log file';
  FViewLogMenuItem.Visible  := True;
  FViewLogMenuItem.OnClick  := DoViewLogFile;
  FPopupMenu.Items.Add(FViewLogMenuItem);

  lMenuItem                 := TMenuItem.Create(FForm);
  lMenuItem.Name            := 'N1';
  lMenuItem.Caption         := '-';
  FPopupMenu.Items.Add(lMenuItem);

  lMenuItem                 := TMenuItem.Create(FForm);
  lMenuItem.Name            := 'ClearMenuItem';
  lMenuItem.Caption         := '&Clear';
  lMenuItem.ShortCut        := 16460;
  lMenuItem.OnClick         := FormClearMenuItemClick;
  FPopupMenu.Items.Add(lMenuItem);

  FWordWrapMenuItem         := TMenuItem.Create(FForm);
  FWordWrapMenuItem.Name    := 'WordWrapMenuItem';
  FWordWrapMenuItem.Caption := '&Word wrap';
  FWordWrapMenuItem.ShortCut := 16471;
  FWordWrapMenuItem.OnClick := FormWordWrapMenuItemClick;
  FPopupMenu.Items.Add(FWordWrapMenuItem);

  FLogMenuItem              := TMenuItem.Create(FForm);
  FLogMenuItem.Name         := 'LogMenuItem';
  FLogMenuItem.Caption      := '&Log';
  FLogMenuItem.OnClick      := FormLogMenuItemClick;
  FPopupMenu.Items.Add(FLogMenuItem);

  for lLogSev := Low(TtiLogSeverity) to High(TtiLogSeverity) do
  begin
    lMenuItem              := TMenuItem.Create(FForm);
    lMenuItem.Caption      := cTILogSeverityStrings[ lLogSev ];
    lMenuItem.Tag          := Ord(lLogSev);
    lMenuItem.OnClick      := FormLogLevelMenuItemClick;
    FLogMenuItem.Add(lMenuItem);
  end;

// These menus and toolbutton creation loops were one, but for neatness
// we need to create the buttons in reverse (to show in the correct order)
  for lLogSev := High(TtiLogSeverity) downto Low(TtiLogSeverity) do
  begin
    lToolButton            := TToolButton.Create(FToolBar);
    lToolButton.Parent     := FToolBar;
    {$IFDEF FPC}
    lToolButton.AutoSize   := True;
    {$ENDIF}
    lToolButton.Caption    := cTILogSeverityStrings[ lLogSev ];
    lToolButton.Tag        := Ord(lLogSev);
    lToolButton.Style      := tbsCheck;
    lToolButton.Down       := lLogSev in gLog.SevToLog;
    lToolButton.OnClick    := FormLogLevelButtonClick;
  end;
  
  {$IFDEF FPC}
  FToolBar.ButtonWidth     := 50;
  {$ENDIF}
 Result := FForm;
end;

function TtiLogToGUI.GetFormParent: TWinControl;
begin
  result := FForm.Parent;
end;

procedure TtiLogToGUI.Log(const ADateTime, AThreadID, AMessage: string; ASeverity: TtiLogSeverity);
begin
  if Terminated then
    Exit; //==>
  {$IFDEF FPC}
  { Note: A workaround.It seems that Lazarus does not allow to show this form
    before Application.Run call. }
  if not FForm.Visible then FForm.Show;
  {$ENDIF}
  inherited Log(ADateTime, AThreadID, AMessage, ASeverity);
end;

procedure TtiLogToGUI.SetFormParent(const AValue: TWinControl);
begin
  FForm.Parent     := AValue;
  FForm.Align      := alClient;
  FForm.BorderStyle := bsNone;
end;


procedure TtiLogToGUI.SetSevToLog(const AValue: TtiSevToLog);
var
  i: integer;
  lLogSev : TtiLogSeverity;
begin
  // Let parent perform important task(s)
  inherited;
// All we do here is reflect any changes to LogSeverity in the visual controls
  for i := 0 to FToolBar.ControlCount - 1 do
  begin
    lLogSev                 := TtiLogSeverity(FToolBar.Buttons[i].Tag);
    FToolBar.Buttons[i].Down := lLogSev in AValue;
  end;
end;


procedure TtiLogToGUI.WriteToOutput;
var
  i : integer;
  lLogEvent : TtiLogEvent;
  liStart  : integer;
  liEnd    : integer;
const
  ciMaxLineCount = 200;
begin
  if ThrdLog.Terminated then
    Exit; //==>

  inherited WriteToOutput;

  if ListWorking.Count > ciMaxLineCount * 2 then
  begin
    FMemoLog.Lines.Clear;
    liStart := ListWorking.Count - 1 - ciMaxLineCount;
    liEnd  := ListWorking.Count - 1;
  end else
  begin
    if FMemoLog.Lines.Count > ciMaxLineCount then
    begin
      for i := 0 to ciMaxLineCount div 2 do
        FMemoLog.Lines.Delete(0);
      //FMemoLog.selstart := 0;
      //SendMessage(FMemoLog.handle, em_scrollcaret, 0, 0);
      {$IFDEF MSWINDOWS}
      SendMessage(FMemoLog.handle, WM_VSCROLL, SB_Bottom, 0);
      {$ENDIF MSWINDOWS}
    end;
    liStart := 0;
    liEnd  := ListWorking.Count - 1;
  end;

  for i := liStart to liEnd do begin
    if ThrdLog.Terminated then
      Break; //==>
    lLogEvent := TtiLogEvent(ListWorking.Items[i]);
    FMemoLog.Lines.Add(lLogEvent.AsStringStripCrLf);
    //lLogEvent.Free;
  end;

  ListWorking.Clear;
end;


procedure TtiLogToGUI.FormClearMenuItemClick(Sender: TObject);
begin
  FMemoLog.Lines.Clear;
end;


procedure TtiLogToGUI.FormWordWrapMenuItemClick(Sender: TObject);
begin
  FMemoLog.WordWrap        := not FMemoLog.WordWrap;
  FWordWrapMenuItem.Checked := FMemoLog.WordWrap;
  if FMemoLog.WordWrap then
    FMemoLog.ScrollBars := ssVertical
  else
    FMemoLog.ScrollBars := ssBoth;
end;


procedure TtiLogToGUI.FormLogMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FLogMenuItem.Count - 1 do
    FLogMenuItem.Items[i].Checked := TtiLogSeverity(FLogMenuItem.Items[i].Tag) in gLog.SevToLog;
end;


procedure TtiLogToGUI.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  FForm.WindowState := wsMinimized;
end;


procedure TtiLogToGUI.FormLogLevelMenuItemClick(Sender : TObject);
var
  lLogSev : TtiLogSeverity;
  lLogChecked : boolean;
begin
  if not (Sender is TMenuItem) then
    exit; //==>

  lLogSev    := TtiLogSeverity(TWinControl(Sender).Tag);

  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  lLogChecked := TMenuItem(Sender).Checked;
  FToolBar.Buttons[TMenuItem(Sender).Tag].Down := lLogChecked;

  if lLogChecked then  // NB Should these refer to local SevToLog rather than the global gLog?
    gLog.SevToLog := gLog.SevToLog + [lLogSev]
  else
    gLog.SevToLog := gLog.SevToLog - [lLogSev];
end;


procedure TtiLogToGUI.FormLogLevelButtonClick(Sender: TObject);
var
  lLogSev : TtiLogSeverity;
  lLogChecked : boolean;
begin
  if not (Sender is TToolButton) then
    Exit; //==>

  lLogSev    := TtiLogSeverity(TWinControl(Sender).Tag);

  lLogChecked := TToolButton(Sender).Down;
  if lLogChecked then  // NB These refer to just SevToLog, (and keep changes local) rather than the global gLog?
    gLog.SevToLog := gLog.SevToLog + [lLogSev]
  else
    gLog.SevToLog := gLog.SevToLog - [lLogSev];
end;


procedure TtiLogToGUI.DoViewLogFile(Sender: TObject);
begin
  if (gLog.LogToFileName <> '') and
     (FileExists(gLog.LogToFileName)) then
    tiEditFile(gLog.LogToFileName);
end;


procedure TtiLogToGUI.DoOnPopup(Sender: TObject);
begin
  FViewLogMenuItem.Visible:=
    (gLog.LogToFileName <> '') and
    (FileExists(gLog.LogToFileName));
end;


initialization
  if gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToGUI);

end.

