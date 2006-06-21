{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiLogViewForm_Cli;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows, Messages, Graphics, Controls, Forms
  ,Dialogs, StdCtrls, Menus, ToolWin, ComCtrls
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QGraphics, QControls, QForms, QDialogs
  ,QStdCtrls, QMenus, QTypes, QComCtrls
  {$ENDIF LINUX}
  ,tiLog
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
  ;

type
  TLogViewForm = class(TForm)
    PopupMenu: TPopupMenu;
    MemoLog: TMemo;
    ClearMenuItem: TMenuItem;
    WordWrapMenuItem: TMenuItem;
    LogMenuItem: TMenuItem;
    ToolBar: TToolBar;
    N1: TMenuItem;
    Viewlogfile1: TMenuItem;
    procedure ClearMenuItemClick(Sender: TObject);
    procedure WordWrapMenuItemClick(Sender: TObject);
    procedure LogMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure LogLevelMenuItemClick(Sender: TObject);
    procedure LogLevelButtonClick(Sender: TObject);
    procedure Viewlogfile1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetSevToLog(const Value: TSevToLog);
  public
    { Public declarations }
    property SevToLog : TSevToLog write SetSevToLog;
  end;

  // ---------------------------------------------------------------------------
  TLogToForm = class( TLogToCacheAbs )
  private
    FForm      : TLogViewForm ;
    function  GetParent: TWinControl;
    procedure SetParent(const Value: TWinControl);
  protected
    procedure   WriteToOutput ; override ;
    procedure SetSevToLog(const Value: TSevToLog); override  ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Parent : TWinControl read GetParent write SetParent ;
    procedure   Log( const psDateTime : string ;
                     const psThreadID : string ;
                     const psMessage  : string;
                     pSeverity  : TLogSeverity ) ; override ;
  end;


implementation
uses
  tiUtils
  ;

{$IFDEF MSWINDOWS}
  {$R *.dfm}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  {$R *.xfm}
{$ENDIF LINUX}



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogToForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TLogToForm.Create ;
begin
// Note: We must create the form before anything else - notably a WriteToOutput
//       request initiated by our thread, since it will access the form's memo
  FForm := TLogViewForm.Create( nil ) ;
  FForm.Top    := 10 ;
  FForm.Left   := 10 ;
  FForm.Height := 150 ;
  FForm.Width  := Screen.Width - 20 ;
  FForm.Caption := ' Application event log - ' + Application.Title ;

// then we can proceed with other create steps...
  inherited Create ;

  FForm.Visible := true ;
end;

// -----------------------------------------------------------------------------
destructor TLogToForm.Destroy;
begin
  // 1. If the log's parent property was set, then
  //    the form may have been freed elswhere, so
  //    don't call FForm.Free, call FreeAndNil(FForm)
  // 2. Do not change the sequence. Inherited must
  //    be called first to close down any threads.
  inherited;
  FreeAndNil( FForm ) ; 
end;


// -----------------------------------------------------------------------------
function TLogToForm.GetParent: TWinControl;
begin
  result := FForm.Parent ;
end;



// -----------------------------------------------------------------------------
procedure TLogToForm.Log(const psDateTime, psThreadID, psMessage: string; pSeverity: TLogSeverity);
begin
  if Terminated then
    Exit ; //==>
  inherited log(psDateTime, psThreadID, psMessage, pSeverity);
end;

procedure TLogToForm.SetParent(const Value: TWinControl);
begin
  FForm.Parent      := Value ;
  FForm.Align       := alClient ;
  {$IFDEF MSWINDOWS}
  FForm.BorderStyle := bsNone ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FForm.BorderStyle := fbsNone ;
  {$ENDIF LINUX}
end;


procedure TLogToForm.SetSevToLog(const Value: TSevToLog);
begin
  // Let parent perform important task(s)
  inherited;
// All we do here is reflect any changes to LogSeverity in the visual controls
  FForm.SevToLog := Value;
end;

procedure TLogToForm.WriteToOutput;
var
  i : integer ;
  lLogEvent : TLogEvent ;
  liStart   : integer ;
  liEnd     : integer ;
const
  ciMaxLineCount = 200 ;
begin
  if ThrdLog.Terminated then
    Exit ; //==>

  inherited WriteToOutput ;

  if ListWorking.Count > ciMaxLineCount * 2 then
  begin
    FForm.MemoLog.Lines.Clear ;
    liStart := ListWorking.Count - 1 - ciMaxLineCount ;
    liEnd   := ListWorking.Count - 1 ;
  end else
  begin
    if FForm.MemoLog.Lines.Count > ciMaxLineCount then
    begin
      for i := 0 to ciMaxLineCount div 2 do
        FForm.MemoLog.Lines.Delete( 0 ) ;
      //FMemoLog.selstart := 0;
      //SendMessage( FMemoLog.handle, em_scrollcaret, 0, 0 );
      {$IFDEF MSWINDOWS}
      SendMessage( FForm.MemoLog.handle, WM_VSCROLL, SB_Bottom, 0 ) ;
      {$ENDIF MSWINDOWS}
    end ;
    liStart := 0 ;
    liEnd   := ListWorking.Count - 1 ;
  end ;

  for i := liStart to liEnd do begin
    if ThrdLog.Terminated then
      Break ; //==>
    lLogEvent := TLogEvent( ListWorking.Items[i] ) ;
    FForm.MemoLog.Lines.Add( lLogEvent.AsStringStripCrLf ) ;
    //lLogEvent.Free ;
  end ;

  ListWorking.Clear ;

end;



procedure TLogViewForm.ClearMenuItemClick(Sender: TObject);
begin
  MemoLog.Lines.Clear ;
end;

procedure TLogViewForm.WordWrapMenuItemClick(Sender: TObject);
begin
  MemoLog.WordWrap         := not MemoLog.WordWrap ;
  WordWrapMenuItem.Checked := MemoLog.WordWrap ;
  if MemoLog.WordWrap then
    MemoLog.ScrollBars := ssVertical
   else
    MemoLog.ScrollBars := ssBoth;
end;

procedure TLogViewForm.LogMenuItemClick(Sender: TObject);
var
  i: integer ;
begin
  for i := 0 to LogMenuItem.Count - 1 do
    LogMenuItem.Items[i].Checked := TLogSeverity(LogMenuItem.Items[i].Tag) in gLog.SevToLog;
end;

procedure TLogViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false ;
  WindowState := wsMinimized ;
end;

procedure TLogViewForm.FormCreate(Sender: TObject);
var
  lMenuItem   : TMenuItem ;
  lLogSev     : TLogSeverity;
  lToolButton : TToolButton;
begin
  for lLogSev := Low(TLogSeverity) to High(TLogSeverity) do
  begin
    lMenuItem                 := TMenuItem.Create( self ) ;
    lMenuItem.Caption         := caLogSeverityStrings[ lLogSev ] ;
    lMenuItem.Tag             := Ord(lLogSev);
    lMenuItem.OnClick         := LogLevelMenuItemClick ;
    LogMenuItem.Add( lMenuItem ) ;
  end;

// These menus and toolbutton creation loops were one, but for neatness
// we need to create the buttons in reverse (to show in the correct order)
  for lLogSev := High(TLogSeverity) downto Low(TLogSeverity) do
  begin
    lToolButton          := TToolButton.Create(ToolBar);
    lToolButton.Parent   := ToolBar;
    lToolButton.Caption  := caLogSeverityStrings[ lLogSev ] ;
    lToolButton.Tag      := Ord(lLogSev);
    lToolButton.Style    := tbsCheck;
    lToolButton.Down     := lLogSev in gLog.SevToLog;  // Not sure if this has been set yet
    lToolButton.OnClick  := LogLevelButtonClick ;
    end;
end;

// -----------------------------------------------------------------------------
procedure TLogViewForm.LogLevelMenuItemClick( Sender : TObject ) ;
var
  lLogSev : TLogSeverity;
  lLogChecked : boolean ;
begin
  if not (Sender is TMenuItem) then
    exit; // ========================>

  {$IFDEF MSWINDOWS}
  lLogSev     := TLogSeverity(TWinControl(Sender).Tag);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  lLogSev     := TLogSeverity(TWidgetControl(Sender).Tag);
  {$ENDIF LINUX}

  TMenuItem( Sender ).Checked                    := not TMenuItem( Sender ).Checked;
  lLogChecked                                    := TMenuItem( Sender ).Checked;
  {$IFDEF MSWINDOWS}
  ToolBar.Buttons[TMenuItem( Sender ).Tag].Down  := lLogChecked;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TToolButton(ToolBar.Controls[TMenuItem( Sender ).Tag]).Down  := lLogChecked;
  {$ENDIF LINUX}

  if lLogChecked then  // NB Should these refer to local SevToLog rather than the global gLog?
    gLog.SevToLog := gLog.SevToLog + [lLogSev]
  else
    gLog.SevToLog := gLog.SevToLog - [lLogSev];

end ;

procedure TLogViewForm.LogLevelButtonClick(Sender: TObject);
var
  lLogSev : TLogSeverity;
  lLogChecked : boolean ;
begin
  if not (Sender is TToolButton) then
    exit; // ========================>

  lLogSev     := TLogSeverity(TWinControl(Sender).Tag);

  lLogChecked := TToolButton( Sender ).Down;
  if lLogChecked then  // NB These refer to just SevToLog, (and keep changes local) rather than the global gLog?
    gLog.SevToLog := gLog.SevToLog + [lLogSev]
  else
    gLog.SevToLog := gLog.SevToLog - [lLogSev];

end;

procedure TLogViewForm.SetSevToLog(const Value: TSevToLog);
var
  i: integer ;
  lLogSev : TLogSeverity;
begin
  for i := 0 to ToolBar.ControlCount - 1 do
  begin
    {$IFDEF MSWINDOWS}
    lLogSev                  := TLogSeverity(ToolBar.Buttons[i].Tag);
    ToolBar.Buttons[i].Down  := lLogSev in Value;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    lLogSev                  := TLogSeverity(ToolBar.Controls[i].Tag);
    TToolButton(ToolBar.Controls[i]).Down  := lLogSev in Value;
    {$ENDIF LINUX}
  end;
end;

procedure TLogViewForm.Viewlogfile1Click(Sender: TObject);
begin
  tiEditFile( gLog.LogFileName ) ;
end;

end.
