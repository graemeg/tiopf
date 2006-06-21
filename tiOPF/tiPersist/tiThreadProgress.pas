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

  Created: Mid 1998

  Purpose: A TThread descendant which knows how to register it self with an
           Observer [GoF 293] which keeps track of the progress of the thread.

  Revision History:
    Feb 1999, PWH, Created

  Useage:

    TThrdMyProcess = class( TtiThreadProgress )
    protected
      // Put some code to be run when the thread is finished, but call
      // inherited first.
      procedure   DoOnTerminate( sender : TObject ) ; override ;
    public
      // Put your working code here, but Execute is an abstract so don't
      // call inherited.
      procedure Execute ; override ;
    end ;

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiThreadProgress;

interface
uses
  Classes
  {$IFDEF MSWINDOWS}
  ,Forms
  ,Buttons
  ,StdCtrls
  ,ExtCtrls
  ,Controls
  ,Graphics
  ,ComCtrls
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  ,QButtons
  ,QStdCtrls
  ,QExtCtrls
  ,QControls
  ,QGraphics
  ,QComCtrls
  ,QTypes
  {$ENDIF LINUX}
  ,SyncObjs
  ,tiThread
  ;

type

  TProgInd = class ;

  {$IFNDEF NOTHREADS}
  // A dummy stub with the same interface as a TThread which can be used for
  // debugging processes which will eventually be run in a TtiThreadProgress.
  // Use of the TThreadDebugger as the parent of TtiThreadProgress makes
  // debugging easier as exceptions and GUI interaction is difficult within
  // a thread.
  // Note: This class still needs a little work as OnTerminate will not be called
  //       when the thread finished.
  //----------------------------------------------------------------------------
  TThreadDebugger = class( TObject )
  private
    FbTerminated: boolean;
    FOnTerminate: TNotifyEvent;
    FbSuspended: boolean;
    FbFreeOnTerminate: boolean;
  protected
    procedure   Synchronize( Method : TThreadMethod ) ;
  public
    constructor Create( Suspended : boolean ) ; virtual ;
    procedure   Terminate ; virtual ;
    procedure   Resume ;

    procedure   Execute ; virtual ; abstract ;

    property    Terminated : boolean read FbTerminated write FbTerminated ;
    property    OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate ;
    property    Suspended   : boolean read FbSuspended write FbSuspended ;
    property    FreeOnTerminate : boolean read FbFreeOnTerminate write FbFreeOnTerminate ;
  end ;
  {$ENDIF}


  // To remove the multi-threaded functionality of the TtiThreadProgress,
  // use TThreadDebugger as the parent of TtiThreadProgress.
  // To make long processes multi-threaded, use TThread as the parent.
  {$IFDEF NOTHREADS}
    TtiThreadProgress = class( TThreadDebugger )
  {$ELSE}
    TtiThreadProgress = class( TtiThread )
  {$ENDIF}
  private
    FProgInd : TProgInd ;
    FiPosition  : integer ;
    FsText : string ;
    FiMin: integer;
    FiMax: integer;
    FbAutoProgress: boolean ;
    FsCaption: TCaption;
    FCanCancel: boolean;
    FConfirmCancel : boolean ;
    FOnCancel : TNotifyEvent;
    procedure SynchronizeProgress ;
    procedure SetPosition(const Value: integer);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetAutoProgress(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetCanCancel(const Value: boolean);
  protected
    procedure   DoOnTerminate( sender : TObject ) ; override ;
  public
    constructor Create(CreateSuspended: Boolean); override ;
    Destructor  Destroy ; override ;
    Property    ProgInd : TProgInd     read FProgInd       write FProgInd ;
    Property    Max     : integer      read FiMax          write SetMax ;
    Property    Min     : integer      read FiMin          write SetMin ;
    Property    Position : integer     read FiPosition     write SetPosition ;
    Property    Text     : string      read FsText         write SetText ;
    Property    Caption  : TCaption    read FsCaption      write SetCaption ;
    Property    AutoProgress : Boolean read FbAutoProgress write SetAutoProgress ;
    Property    CanCancel : boolean    read FCanCancel     write SetCanCancel ;
    property    ConfirmCancel : boolean    read FConfirmCancel   write FConfirmCancel ;
    property    OnCancel      : TNotifyEvent read FOnCancel      write FOnCancel ;
    Procedure   IncPosition ; virtual;
    //DO NOT call inherited in the overridden execute method !!!
  end ;

  TFormThreadProgress = class( TForm )
  private
    FTimer    : TTimer ;
    FProgInds : TList ;
    FCritSect : TCriticalSection ;
    FPanelBevelInner : TBevelCut ;
    FPanelBevelOuter : TBevelCut ;
    FColumnCount: integer;
    FProgressBarColor: TColor;
    FOnChangeThreadCount: TNotifyEvent;

    function    FindByThread( pThread : TtiThreadProgress ) : integer ;
    procedure   ArrangePanels ;
    procedure   DoCloseQuery( sender : TObject ; var CanClose : boolean ) ;
    procedure   UpdateAutoProgressThreads( Sender : TObject ) ;
    function    GetThreadCount: integer;
  protected
    {$IFDEF MSWINDOWS}
    procedure   SetParent(AParent: TWinControl); override;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    procedure   SetParent(const AParent: TWidgetControl); override;
    {$ENDIF LINUX}
  public
    Constructor Create( Owner : TComponent ) ; override ;
    Constructor CreateNew( Owner : TComponent ; Dummy : integer = 0 ) ; override ;
    Destructor  Destroy ; override ;
    Procedure   AttachThread( pThread : TtiThreadProgress ) ;
    Function    DetachThread( pThread : TtiThreadProgress ) : boolean ;

    property    PanelBevelInner     : TBevelCut    read FPanelBevelInner  write FPanelBevelInner ;
    property    PanelBevelOuter     : TBevelCut    read FPanelBevelOuter  write FPanelBevelOuter ;
    property    ProgressBarColor    : TColor       read FProgressBarColor write FProgressBarColor ;
    property    ColumnCount         : integer      read FColumnCount      write FColumnCount ;
    property    OnChangeThreadCount : TNotifyEvent read FOnChangeThreadCount   write FOnChangeThreadCount ;
    property    ThreadCount         : integer      read GetThreadCount ;

  end ;

  TtiProgressBar = class(TProgressBar)
  private
  public
    {$IFDEF MSWINDOWS}
    property  BevelInner ;
    property  BevelOuter ;
    {$ENDIF MSWINDOWS}
    property  Color ;
  end ;

  TProgInd = class( TCustomPanel )
  private
    FLabel       : TLabel ;
    FProgressBar : TtiProgressBar ;
    FSpeedButton : TSpeedButton ;
    FThread      : TtiThreadProgress ;
    FCaption     : TCaption ;
    function    GetMax: integer;
    function    GetMin: integer;
    function    GetPosition: integer;
    function    GetText: String ;
    procedure   SetMax(const Value: integer);
    procedure   SetMin(const Value: integer);
    procedure   SetPosition(const Value: integer);
    procedure   SetText(const Value: string);
    procedure   TerminateOnClick( sender : TObject ) ;
    function    GetCaption: TCaption;
    procedure   SetCaption(const Value: TCaption);
    function    GetCanCancel: boolean;
    procedure   SetCanCancel(const Value: boolean);
    function    GetProgressBarWidth: integer;
  public
    Constructor Create( Owner : TComponent ) ; override ;
    property    Position : integer read GetPosition  write SetPosition ;
    property    Max : integer read GetMax  write SetMax ;
    property    Min : integer read GetMin  write SetMin ;
    property    Text : string read GetText write SetText ;
    Property    Caption : TCaption read GetCaption write SetCaption ;
    property    Thread : TtiThreadProgress read FThread write FThread ;
    property    CanCancel : boolean read GetCanCancel write SetCanCancel ;
  end ;

// The FormThreadProgress is a Singleton
function gFormThreadProgress : TFormThreadProgress ;

implementation
uses
  tiUtils
  ,tiLog
  ,tiDialogs
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,tiRegINI
  ;

var
  uFormThreadProgress : TFormThreadProgress ;
  uFormThreadProgressIsOwned : boolean ;

const
  cuiProgIndHeight   =  24 ;
  cuiFormWidth       = 450 ;
  cuiLabelWidth      = 150 ;
  cuCancelButtonSize =  17 ;
  cuWaitForTerminate = 'Waiting to terminate' ;

// -----------------------------------------------------------------------------
function gFormThreadProgress : TFormThreadProgress ;
begin
  if uFormThreadProgress = nil then
    uFormThreadProgress := TFormThreadProgress.CreateNew( nil ) ;
  result := uFormThreadProgress ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TProgInd
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TProgInd.Create(Owner: TComponent);
begin
  inherited Create( Owner )    ;
  Parent       := ( Owner as TFormThreadProgress ) ;
  BevelInner   := ( Owner as TFormThreadProgress ).PanelBevelInner ;
  BevelOuter   := ( Owner as TFormThreadProgress ).PanelBevelOuter ;
  Color        := ( Owner as TFormThreadProgress ).Color ;
  Height       := cuiProgIndHeight ;
  Left         := 4 ;
  Anchors      := [akLeft,akTop,akRight] ;
  Width        := TForm( Owner ).ClientWidth - 8 ;

  FLabel                  := TLabel.Create( self ) ;
  FLabel.Parent           := self ;
  FLabel.Left             :=  10 ;
  FLabel.Top              :=   6 ;
  FLabel.Width            := cuiLabelWidth ;
  FLabel.Height           :=  13 ;

  FProgressBar            := TtiProgressBar.Create( self ) ;
  FProgressBar.Parent     := self ;
  FProgressBar.Left       := 124 ;
  FProgressBar.Left       := cuiLabelWidth + 20 ;
  FProgressBar.Top        :=   4 ;
  FProgressBar.Width      := GetProgressBarWidth ;
  FProgressBar.Height     :=  17 ;
  FProgressBar.Anchors    := [akLeft, akTop, akRight] ;
  FProgressBar.Smooth     := true ;
  {$IFDEF MSWINDOWS}
  FProgressBar.BevelInner := Self.BevelInner ;
  FProgressBar.BevelOuter := Self.BevelOuter ;
  {$ENDIF MSWINDOWS}
  FProgressBar.Color      := ( Owner as TFormThreadProgress ).ProgressBarColor ;

  FSpeedButton          := TSpeedButton.Create( self ) ;
  FSpeedButton.Parent   := self ;
  FSpeedButton.Left     := Self.ClientWidth - cuCancelButtonSize - 4 ;
  FSpeedButton.Top      :=   4 ;
  FSpeedButton.Width    :=  cuCancelButtonSize ;
  FSpeedButton.Height   :=  cuCancelButtonSize ;
  FSpeedButton.Anchors  := [akTop, akRight] ;
  FSpeedButton.Flat     := True  ;
  FSpeedButton.Caption  := 'X' ;
  FSpeedButton.Hint     := 'Cancel this process' ;
  FSpeedButton.ShowHint := true ;
  FSpeedButton.OnClick  := TerminateOnClick ;
  FSpeedButton.Visible  := false ;

end;

// -----------------------------------------------------------------------------
function TProgInd.GetMax: integer;
begin
  result := FProgressBar.Max ;
end;

// -----------------------------------------------------------------------------
function TProgInd.GetMin: integer;
begin
  result := FProgressBar.Min ;
end;

// -----------------------------------------------------------------------------
function TProgInd.GetPosition: integer;
begin
  result := FProgressBar.Position ;
end;

// -----------------------------------------------------------------------------
procedure TProgInd.SetMax(const Value: integer);
begin
  FProgressBar.Max := Value ;
end;

// -----------------------------------------------------------------------------
procedure TProgInd.SetMin(const Value: integer);
begin
  FProgressBar.Min := Value ;
end;

// -----------------------------------------------------------------------------
procedure TProgInd.SetPosition(const Value: integer);
begin
  FProgressBar.Position := Value ;
end;

// -----------------------------------------------------------------------------
function TProgInd.GetText: string;
begin
  result := FLabel.Caption ;
end;

// -----------------------------------------------------------------------------
procedure TProgInd.SetText(const Value: string);
begin
  if FLabel.Caption <> cuWaitForTerminate then
    FLabel.Caption := Value ;
end;

// -----------------------------------------------------------------------------
function TProgInd.GetCaption: TCaption;
begin
  Result := FCaption;
end;

// -----------------------------------------------------------------------------
procedure TProgInd.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  if (Owner is TCustomForm) then TCustomForm(Owner).Caption := ' Progress : ' + FCaption;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TFormThreadProgress.CreateNew(Owner: TComponent ; Dummy : Integer = 0 );
begin
  inherited;
  if Application.MainForm = nil then
    FormStyle := fsNormal
  else if TForm(Application.MainForm).FormStyle = fsMDIForm then    // In Linux this is protected. Mainform = TCustomForm
    FormStyle    := fsMDIChild
  else
    FormStyle    := fsStayOnTop ;

  FProgInds := TList.Create ;
  BorderIcons  := [biSystemMenu,biMinimize] ;
  {$IFDEF MSWINDOWS}
  BorderStyle  := bsSizeable   ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  BorderStyle  := fbsSizeable   ;
  {$ENDIF LINUX}
  Caption      := ' Progress'  ;
  ClientHeight := 30           ;
  ClientWidth  := cuiFormWidth ;
  FCritSect    := TCriticalSection.Create ;
  {$IFDEF MSWINDOWS}
  gReg.ReadFormState( self ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  gINI.ReadFormState( self );
  {$ENDIF LINUX}
  Visible      := true ;
  OnCloseQuery := DoCloseQuery ;
  FTimer       := TTimer.Create( nil ) ;
  FTimer.OnTimer := UpdateAutoProgressThreads ;
  FPanelBevelInner   := bvLowered    ;
  FPanelBevelOuter   := bvNone       ;
  FProgressBarColor  := Color ;
  FColumnCount       := 1 ;


  if Application.MainForm <> nil then
  begin
    Constraints.MinHeight := Height ;
    Constraints.MinWidth  := cuiFormWidth ;
    Constraints.MaxHeight := Height ;
    Constraints.MaxWidth  := cuiFormWidth ;
  end {else
  begin
    Constraints.MinHeight := 0 ;
    Constraints.MinWidth  := 0 ;
    Constraints.MaxHeight := 0 ;
    Constraints.MaxWidth  := 0 ;
  end} ;
  uFormThreadProgress := nil ;

end;

// -----------------------------------------------------------------------------
destructor TFormThreadProgress.Destroy;
var
  i : integer ;
begin
  FTimer.Free ;
  for i := 0 to FProgInds.Count - 1 do
    TtiThreadProgress( FProgInds.Items[i] ).Terminate ;

  {$IFDEF MSWINDOWS}
  gReg.WriteFormState( self ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  gINI.WriteFormState( self ) ;
  {$ENDIF LINUX}
  FProgInds.Free ;
  FCritSect.Free ;
  if uFormThreadProgress = Self then
    uFormThreadProgress := nil ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TFormThreadProgress.AttachThread(pThread: TtiThreadProgress);
var
  lProgInd : TProgInd ;
begin
  FTimer.Enabled := false ;
  try
    FCritSect.Enter ;
    try
      Assert( FindByThread( pThread ) = -1, 'Thread already attached.' ) ;
      lProgInd := TProgInd.Create( self ) ;
      lProgInd.Thread := pThread ;
      pThread.ProgInd := lProgInd ;
      FProgInds.Add( lProgInd ) ;
      ArrangePanels ;
    finally
      FCritSect.Leave ;
    end ;
  finally
    FTimer.Enabled := true ;
  end ;
  if Assigned(FOnChangeThreadCount) then
    FOnChangeThreadCount(Self);
end;

// -----------------------------------------------------------------------------
function TFormThreadProgress.DetachThread(pThread: TtiThreadProgress ) : boolean ;
var
  i : integer ;
  lProgInd : TProgInd ;
begin
  FTimer.Enabled := false ;
  try
    FCritSect.Enter ;
    try
      i := FindByThread( pThread ) ;
      Assert( i <> -1, 'Thread not attached.' ) ;

      lProgInd := TProgInd( FProgInds.Items[i] ) ;
      lProgInd.Thread := nil ;
      pThread.ProgInd := nil ;
      lProgInd.Free ;
      FProgInds.Delete( i ) ;
      ArrangePanels ;
      result := ( FProgInds.Count = 0 ) ;
    finally
      FCritSect.Leave ;
    end ;
  finally
    FTimer.Enabled := true ;
  end;
  if Assigned(FOnChangeThreadCount) then
    FOnChangeThreadCount(Self);
end ;

// -----------------------------------------------------------------------------
function TFormThreadProgress.FindByThread( pThread : TtiThreadProgress ) : integer ;
var
  i : integer ;
begin
  FCritSect.Enter ;
  try
    result := -1 ;
    for i := 0 to FProgInds.Count - 1 do
      if TProgInd( FProgInds.Items[i] ).Thread = pThread then begin
        result := i ;
        Break ; //==>
      end ;
  finally
    FCritSect.Leave ;
  end ;
end ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
destructor TtiThreadProgress.Destroy;
begin
  if AutoProgress then
    {$IFDEF MSWINDOWS}
    gReg.WriteInteger( 'ThreadProgress',
                       ClassName,
                       FiPosition + 1 );   // Was: Trunc( FiPosition * 1.1 )) ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    gINI.WriteInteger( 'ThreadProgress',
                       ClassName,
                       FiPosition + 1 );
    {$ENDIF LINUX}
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.DoOnTerminate( sender : TObject ) ;
begin
  gFormThreadProgress.DetachThread( self ) ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.IncPosition ;
begin
  Position := Position + 1 ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.SetAutoProgress(const Value: Boolean);
begin
  FbAutoProgress := Value;
  if AutoProgress then begin
    FiPosition := 0 ;
    FiMin      := 0 ;
    {$IFDEF MSWINDOWS}
    Max := gReg.ReadInteger( 'ThreadProgress', ClassName, 10 ) ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    Max := gINI.ReadInteger( 'ThreadProgress', ClassName, 10 ) ;
    {$ENDIF LINUX}
    if Max = 0 then
      Max := 10 ;
  end ;
end;

//------------------------------------------------------------------------------

procedure TtiThreadProgress.SetMax(const Value: integer);
begin
  FiMax := Value;
  Synchronize( SynchronizeProgress ) ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.SetMin(const Value: integer);
begin
  FiMin := Value;
  Synchronize( SynchronizeProgress ) ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.SetPosition(const Value: integer);
begin
  FiPosition := Value ;
  Synchronize( SynchronizeProgress ) ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.SetText(const Value: string);
begin
  FsText := Value;
  Synchronize( SynchronizeProgress ) ;
end;

//------------------------------------------------------------------------------
procedure TtiThreadProgress.SetCaption(const Value: TCaption);
begin
  FsCaption := Value;
  Synchronize( SynchronizeProgress ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiThreadProgress.SynchronizeProgress;
begin
  FProgInd.Max      := FiMax ;
  FProgInd.Min      := FiMin ;
  FProgInd.Position := FiPosition ;
  FProgInd.Text     := FsText ;
  FProgInd.Caption  := FsCaption ;
  FProgInd.CanCancel := FCancancel ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TFormThreadProgress.Create(Owner: TComponent);
begin
  Assert( False, 'Do not call TFormThreadProgress.Create, call CreatNew instead.' ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormThreadProgress.ArrangePanels;
var
  lProgInd : TProgInd ;
  i        : integer ;
  lRow     : integer ;
  lCol     : integer ;
  lTop     : integer ;
  lLeft    : integer ;
  lWidth   : integer ;
begin
  lRow := 1 ;
  lCol := 1 ;
  for i := 0 to FProgInds.Count - 1 do
  begin
    lProgInd := TProgInd( FProgInds.items[i] ) ;

    if lCol = 1 then
      lLeft := 4
    else
      lLeft := ( ClientWidth div ColumnCount ) {+ 8} ;

    lWidth := ( ClientWidth div ColumnCount ) {- 8} ;
    lTop := ( lRow - 1 ) * ( lProgInd.Height + 2 ) + 2 ;

    lProgInd.SetBounds(lLeft, lTop, lWidth, cuiProgIndHeight );

    if lCol < ColumnCount then
      Inc(lCol)
    else begin
      lCol := 1 ;
      Inc(lRow);
    end;
  end ;

  ClientHeight := lRow * ( cuiProgIndHeight + 4 ) + 4 ;

  if Parent = nil then
  begin
    Constraints.MaxHeight := 0 ;
    Constraints.MinHeight := 0 ;
    if FProgInds.Count > 0 then
    begin
      Constraints.MinHeight := Height ;
      Constraints.MinWidth  := cuiFormWidth ;
      Constraints.MaxHeight := Height ;
      Constraints.MaxWidth  := cuiFormWidth ;
    end
  end ;

  Visible := (FProgInds.Count > 0);

end;

// -----------------------------------------------------------------------------
procedure TFormThreadProgress.DoCloseQuery(sender: TObject ; var CanClose : boolean );
begin
  CanClose    := false ;
  WindowState := wsMinimized ;
end;

// -----------------------------------------------------------------------------
procedure TFormThreadProgress.UpdateAutoProgressThreads(Sender: TObject);
var
  i : integer ;
  lProgInds : TProgInd ;
begin
  {TODO 1 -oFramework: Change the critical section in TFormThreadProgress.UpdateAutoProgressThreads to a semaphore. }
  FCritSect.Enter ;
  try
    for i := 0 to FProgInds.Count - 1 do begin
      lProgInds := TProgInd( FProgInds.Items[i] ) ;
      if lProgInds.Thread.AutoProgress then
        if lProgInds.Thread.Position < lProgInds.Thread.Max then
          lProgInds.Thread.Position := lProgInds.Thread.Position + 1
        else
          lProgInds.Thread.Max := Trunc( lProgInds.Thread.Max * 2 ) ;
    end ;
  finally
    FCritSect.Leave ;
  end;
end;

procedure TProgInd.TerminateOnClick(sender: TObject);
begin
//  tiAppWarning( 'Sorry, this feature is not yet available.' ) ;
  if (not Thread.ConfirmCancel)
  or tiAppConfirmation( 'Are you sure you want to terminate <' + Text + '> ?' ) then
  begin
    if Assigned(Thread.OnCancel) then
      Thread.OnCancel( Thread );
    FSpeedButton.Enabled := false ;
//    Thread.OnTerminate := nil ;
    Thread.ReturnValue := 1;
    Thread.Terminate ;
    Text := cuWaitForTerminate ;
//    if gFormThreadProgress.DetachThread( Thread ) then
//    begin
//      uFormThreadProgress.Free ;
//      uFormThreadProgress := nil ;
//    end ;
//    Thread.WaitFor ;
  end ;
end;

procedure TtiThreadProgress.SetCanCancel(const Value: boolean);
begin
  FCanCancel := Value;
end;

function TProgInd.GetCanCancel: boolean;
begin
  result := ( FSpeedButton <> nil ) and FSpeedButton.Visible ;
end;

procedure TProgInd.SetCanCancel(const Value: boolean);
begin
  FSpeedButton.Visible := Value ;
  FProgressBar.Width := GetProgressBarWidth ;
end;

function TProgInd.GetProgressBarWidth : integer ;
begin
  result := Self.ClientWidth - FProgressBar.Left - 8 ;
  if CanCancel then
    Dec( Result, cuCancelButtonSize ) ;
end;

{$IFNDEF NOTHREADS}
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TThreadDebugger
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  constructor TThreadDebugger.Create(Suspended: boolean);
  begin
    inherited Create ;
    FbSuspended := Suspended ;
    FbTerminated := false ;
    if not Suspended then
      Resume ;
  end;

  // -----------------------------------------------------------------------------
  procedure TThreadDebugger.Resume;
  begin
    FbSuspended := false ;
    Execute ;
  end;

  // -----------------------------------------------------------------------------
  procedure TThreadDebugger.Synchronize(Method: TThreadMethod);
  begin
    if Assigned( Method ) then
      Method ;
  end;

  // -----------------------------------------------------------------------------
  procedure TThreadDebugger.Terminate;
  begin
    Terminated := true ;
  end;
{$ENDIF}

constructor TtiThreadProgress.Create(CreateSuspended: Boolean);
begin
  inherited Create(true);
  gFormThreadProgress.AttachThread( self ) ;
  AutoProgress   := true ;
  CanCancel      := false ;
  ConfirmCancel  := True ;
  ReturnValue    := 0;
end;

function TFormThreadProgress.GetThreadCount: integer;
begin
  FCritSect.Enter;
  try
    result := FProgInds.Count ;
  finally
    FCritSect.Leave;
  end;
end;

procedure TFormThreadProgress.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  uFormThreadProgressIsOwned := ( AParent <> nil ) ;
end;

initialization
  uFormThreadProgress := nil ;
  uFormThreadProgressIsOwned := false ;

finalization

  if (uFormThreadProgress <> nil) and
     (not uFormThreadProgressIsOwned) then
    FreeAndNil(uFormThreadProgress);

end.
