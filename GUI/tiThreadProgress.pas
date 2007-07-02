{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  Purpose: A TThread descendant which knows how to register it self with an
           Observer [GoF 293] which keeps track of the progress of the thread.

  Revision History:
    Feb 1999, PWH, Created

  Usage:

    TThrdMyProcess = class(TtiThreadProgress)
    protected
      // Put some code to be run when the thread is finished, but call
      // inherited first.
      procedure   DoOnTerminate(sender : TObject); override;
    public
      // Put your working code here, but Execute is an abstract so don't
      // call inherited.
      procedure Execute; override;
    end;

  Notes:

   Under Lazarus+FPC there is AV when closing application with running
   TtiThreadProgress threads.
   Possible solutions (if you worry about this AV):

   1.Put into mainform OnClose or OnDestroy event:

    gtiOPFManager.TerminateThreads(Period)
    (where Period could be 0 if not waiting
    or X seconds to wait)


   or

   2. Put into mainform OnCloseQuery event:
      (gTIOPFManager.ActiveThreadList.RunningThreadCount could also be used)

     if gFormThreadProgress.ThreadCount>0 then
     begin
      tiAppWarning('Cannot exit now.Cancel running threads and try again.');
      CanClose := false;
     end;

     and of course thread Execute method should use try..except and test
     Terminated property

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }


unit tiThreadProgress;

{$I tiDefines.inc}

interface
uses
  Forms
  ,Classes
  ,Buttons
  ,StdCtrls
  ,ComCtrls
  ,ExtCtrls
  ,Controls
  ,SyncObjs
  ,tiThread
  ,Graphics
 ;

type

  TProgInd = class;

  {$IFDEF NOTHREADS}
  // A dummy stub with the same interface as a TThread which can be used for
  // debugging processes which will eventually be run in a TtiThreadProgress.
  // Use of the TThreadDebugger as the parent of TtiThreadProgress makes
  // debugging easier as exceptions and GUI interaction is difficult within
  // a thread.
  // Note: This class still needs a little work as OnTerminate will not be called
  //       when the thread finished.

  { TThreadDebugger }

  TThreadDebugger = class(TObject)
  private
    FbTerminated: boolean;
    FOnTerminate: TNotifyEvent;
    FbSuspended: boolean;
    FbFreeOnTerminate: boolean;
    FReturnValue: Integer;
  protected
    procedure   Synchronize(Method : TThreadMethod);
    procedure   DoOnTerminate(sender : TObject); virtual;
  public
    constructor Create(Suspended : boolean); virtual;
    procedure   Terminate; virtual;
    procedure   Resume;

    procedure   Execute; virtual; abstract;

    property    Terminated : boolean read FbTerminated write FbTerminated;
    property    OnTerminate : TNotifyEvent read FOnTerminate write FOnTerminate;
    property    Suspended  : boolean read FbSuspended write FbSuspended;
    property    FreeOnTerminate : boolean read FbFreeOnTerminate write FbFreeOnTerminate;
    property    ReturnValue : Integer read FReturnValue write FReturnValue;
  end;
  {$ENDIF}


  // To remove the multi-threaded functionality of the TtiThreadProgress,
  // use TThreadDebugger as the parent of TtiThreadProgress.
  // To make long processes multi-threaded, use TThread as the parent.
  {$IFDEF NOTHREADS}
    TtiThreadProgress = class(TThreadDebugger)
  {$ELSE}
    TtiThreadProgress = class(TtiThread)
  {$ENDIF}
  private
    FProgInd : TProgInd;
    FPosition : integer;
    FText : string;
    FMin: integer;
    FMax: integer;
    FAutoProgress: boolean;
    FCaption: TCaption;
    FCanCancel: boolean;
    FConfirmCancel : boolean;
    FOnCancel : TNotifyEvent;
    procedure SynchronizeProgress;
    procedure SetPosition(const AValue: integer);
    procedure SetMax(const AValue: integer);
    procedure SetMin(const AValue: integer);
    procedure SetText(const AValue: string);
    procedure SetAutoProgress(const AValue: Boolean);
    procedure SetCaption(const AValue: TCaption);
    procedure SetCanCancel(const AValue: boolean);
  protected
    procedure   DoOnTerminate(sender : TObject); override;
  public
    constructor Create(ACreateSuspended: Boolean); override;
    Destructor  Destroy; override;
    Property    ProgInd : TProgInd     read FProgInd       write FProgInd;
    Property    Max    : integer      read FMax          write SetMax;
    Property    Min    : integer      read FMin          write SetMin;
    Property    Position : integer     read FPosition     write SetPosition;
    Property    Text    : string      read FText         write SetText;
    Property    Caption : TCaption    read FCaption      write SetCaption;
    Property    AutoProgress : Boolean read FAutoProgress write SetAutoProgress;
    Property    CanCancel : boolean    read FCanCancel     write SetCanCancel;
    property    ConfirmCancel : boolean    read FConfirmCancel   write FConfirmCancel;
    property    OnCancel     : TNotifyEvent read FOnCancel      write FOnCancel;
    Procedure   IncPosition; virtual;
    //DO NOT call inherited in the overridden execute method !!!
  end;

  TFormThreadProgress = class(TForm)
  private
    FTimer   : TTimer;
    FProgInds : TList;
    FCritSect : TCriticalSection;
    FPanelBevelInner : TBevelCut;
    FPanelBevelOuter : TBevelCut;
    FColumnCount: integer;
    FProgressBarColor: TColor;
    FOnChangeThreadCount: TNotifyEvent;

    function    FindByThread(pThread : TtiThreadProgress): integer;
    procedure   ArrangePanels;
    procedure   DoCloseQuery(sender : TObject; var CanClose : boolean);
    procedure   UpdateAutoProgressThreads(Sender : TObject);
    function    GetThreadCount: integer;
  protected
    procedure   SetParent(AParent: TWinControl); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Constructor CreateNew(AOwner : TComponent; Dummy : integer = 0); override;
    Destructor  Destroy; override;
    Procedure   AttachThread(pThread : TtiThreadProgress);
    Function    DetachThread(pThread : TtiThreadProgress): boolean;

    property    PanelBevelInner    : TBevelCut    read FPanelBevelInner  write FPanelBevelInner;
    property    PanelBevelOuter    : TBevelCut    read FPanelBevelOuter  write FPanelBevelOuter;
    property    ProgressBarColor   : TColor       read FProgressBarColor write FProgressBarColor;
    property    ColumnCount        : integer      read FColumnCount      write FColumnCount;
    property    OnChangeThreadCount : TNotifyEvent read FOnChangeThreadCount   write FOnChangeThreadCount;
    property    ThreadCount        : integer      read GetThreadCount;

  end;

  TtiProgressBar = class(TProgressBar)
  private
  public
   {$IFNDEF FPC}
    property  BevelInner;
    property  BevelOuter;
   {$ENDIF}
    property  Color;
  end;

  { TProgInd }

  TProgInd = class(TCustomPanel)
  private
    FLabel      : TLabel;
    FProgressBar : TtiProgressBar;
    FSpeedButton : TSpeedButton;
    FThread     : TtiThreadProgress;
    FCaption    : TCaption;
    function    GetMax: integer;
    function    GetMin: integer;
    function    GetPosition: integer;
    function    GetText: String;
    procedure   SetMax(const AValue: integer);
    procedure   SetMin(const AValue: integer);
    procedure   SetPosition(const AValue: integer);
    procedure   SetText(const AValue: string);
    procedure   TerminateOnClick(sender : TObject);
    function    GetCaption: TCaption;
    procedure   SetCaption(const AValue: TCaption);
    function    GetCanCancel: boolean;
    procedure   SetCanCancel(const AValue: boolean);
    function    GetProgressBarWidth: integer;
    {$IFDEF FPC}
    procedure Async(Data: PtrInt);
    {$ENDIF}
  public
    Constructor Create(AOwner : TComponent); override;
    property    Position : integer read GetPosition  write SetPosition;
    property    Max : integer read GetMax  write SetMax;
    property    Min : integer read GetMin  write SetMin;
    property    Text : string read GetText write SetText;
    Property    Caption : TCaption read GetCaption write SetCaption;
    property    Thread : TtiThreadProgress read FThread write FThread;
    property    CanCancel : boolean read GetCanCancel write SetCanCancel;
  end;

// The FormThreadProgress is a Singleton
function gFormThreadProgress : TFormThreadProgress;

implementation
uses
  tiUtils
  ,tiLog
  ,tiDialogs
  ,SysUtils
  {$IFNDEF FPC}
  ,Windows
  {$ELSE}
  ,Lmessages
  ,LCLIntf
  {$ENDIF}
  ,tiINI
  ,Dialogs
 ;

var
  uFormThreadProgress : TFormThreadProgress;
  uFormThreadProgressIsOwned : boolean;

const
  cuiProgIndHeight   =  24;
  cuiFormWidth       = 450;
  cuiLabelWidth      = 150;
  cuCancelButtonSize = 17;
  cuWaitForTerminate = 'Waiting to terminate';

function gFormThreadProgress : TFormThreadProgress;
begin
  if uFormThreadProgress = nil then
    uFormThreadProgress := TFormThreadProgress.CreateNew(nil);
  result := uFormThreadProgress;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TProgInd
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TProgInd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner)   ;
  Parent      := (AOwner as TFormThreadProgress);
  BevelInner  := (AOwner as TFormThreadProgress).PanelBevelInner;
  BevelOuter  := (AOwner as TFormThreadProgress).PanelBevelOuter;
  Color       := (AOwner as TFormThreadProgress).Color;
  Height      := cuiProgIndHeight;
  Left        := 4;
  Anchors     := [akLeft,akTop,akRight];
  Width       := TForm(Owner).ClientWidth - 8;

  FLabel                 := TLabel.Create(self);
  FLabel.Parent          := self;
  FLabel.Left            :=  10;
  FLabel.Top             :=   6;
  FLabel.Width           := cuiLabelWidth;
  FLabel.Height          :=  13;

  FProgressBar           := TtiProgressBar.Create(self);
  FProgressBar.Parent    := self;
  FProgressBar.Left      := 124;
  FProgressBar.Left      := cuiLabelWidth + 20;
  FProgressBar.Top       :=   4;
  FProgressBar.Width     := GetProgressBarWidth;
  FProgressBar.Height    :=  17;
  FProgressBar.Anchors   := [akLeft, akTop, akRight];
  FProgressBar.Smooth    := true;
  {$IFNDEF FPC}
  FProgressBar.BevelInner := Self.BevelInner;
  FProgressBar.BevelOuter := Self.BevelOuter;
  {$ENDIF}
  FProgressBar.Color     := (Owner as TFormThreadProgress).ProgressBarColor;

  FSpeedButton         := TSpeedButton.Create(self);
  FSpeedButton.Parent  := self;
  FSpeedButton.Left    := Self.ClientWidth - cuCancelButtonSize - 4;
  FSpeedButton.Top     :=   4;
  FSpeedButton.Width   :=  cuCancelButtonSize;
  FSpeedButton.Height  :=  cuCancelButtonSize;
  FSpeedButton.Anchors := [akTop, akRight];
  FSpeedButton.Flat    := True ;
  FSpeedButton.Caption := 'X';
  FSpeedButton.Hint    := 'Cancel this process';
  FSpeedButton.ShowHint := true;
  FSpeedButton.OnClick := TerminateOnClick;
  FSpeedButton.Visible := false;
end;


function TProgInd.GetMax: integer;
begin
  result := FProgressBar.Max;
end;


function TProgInd.GetMin: integer;
begin
  result := FProgressBar.Min;
end;


function TProgInd.GetPosition: integer;
begin
  result := FProgressBar.Position;
end;


procedure TProgInd.SetMax(const AValue: integer);
begin
  FProgressBar.Max := AValue;
end;


procedure TProgInd.SetMin(const AValue: integer);
begin
  FProgressBar.Min := AValue;
end;


procedure TProgInd.SetPosition(const AValue: integer);
begin
  FProgressBar.Position := AValue;
end;


function TProgInd.GetText: string;
begin
  result := FLabel.Caption;
end;


procedure TProgInd.SetText(const AValue: string);
begin
  if FLabel.Caption <> cuWaitForTerminate then
    FLabel.Caption := AValue;
end;


function TProgInd.GetCaption: TCaption;
begin
  Result := FCaption;
end;


procedure TProgInd.SetCaption(const AValue: TCaption);
begin
  FCaption := AValue;
  if (Owner is TCustomForm) then TCustomForm(Owner).Caption := ' Progress : ' + FCaption;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TFormThreadProgress.CreateNew(AOwner: TComponent; Dummy : Integer = 0);
begin
  inherited CreateNew(AOwner,Dummy);
  if Application.MainForm = nil then
    FormStyle := fsNormal
  else if Application.MainForm.FormStyle = fsMDIForm then
    FormStyle   := fsMDIChild
  else
    FormStyle   := fsStayOnTop;

  Name := 'ThreadProgress';
  FProgInds := TList.Create;
  BorderIcons := [biSystemMenu,biMinimize];
  BorderStyle := bsSizeable  ;
  Caption     := ' Progress' ;
  ClientHeight := 30          ;
  ClientWidth := cuiFormWidth;
  FCritSect   := TCriticalSection.Create;
  gINI.ReadFormState(self);
  OnCloseQuery := DoCloseQuery;
  FTimer      := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.OnTimer := UpdateAutoProgressThreads;
  FPanelBevelInner  := bvLowered   ;
  FPanelBevelOuter  := bvNone      ;
  FProgressBarColor := Color;
  FColumnCount      := 1;

  if Application.MainForm <> nil then
  begin
    Constraints.MinHeight := Height;
    Constraints.MinWidth := cuiFormWidth;
    Constraints.MaxHeight := Height;
    Constraints.MaxWidth := cuiFormWidth;
  end;
  uFormThreadProgress := nil;

end;


destructor TFormThreadProgress.Destroy;
var
  i : integer;
begin
  for i := 0 to FProgInds.Count - 1 do
    TtiThreadProgress(FProgInds.Items[i]).Terminate;
  FTimer.Free;
  gINI.WriteFormState(self);
  FProgInds.Free;
  FCritSect.Free;
  if uFormThreadProgress = Self then
    uFormThreadProgress := nil;
  inherited;
end;


procedure TFormThreadProgress.AttachThread(pThread: TtiThreadProgress);
var
  lProgInd : TProgInd;
begin
  FTimer.Enabled := false;
  try
    FCritSect.Enter;
    try
      Assert(FindByThread(pThread) = -1, 'Thread already attached.');
      lProgInd := TProgInd.Create(self);
      lProgInd.Thread := pThread;
      pThread.ProgInd := lProgInd;
      FProgInds.Add(lProgInd);
      ArrangePanels;
    finally
      FCritSect.Leave;
    end;
  finally
    FTimer.Enabled := true;
  end;
  if Assigned(FOnChangeThreadCount) then
    FOnChangeThreadCount(Self);
end;


function TFormThreadProgress.DetachThread(pThread: TtiThreadProgress): boolean;
var
  i : integer;
  lProgInd : TProgInd;
  LCount: integer;
begin
  FTimer.Enabled := false;
  LCount:= 0;
  try
    FCritSect.Enter;
    try
        i := FindByThread(pThread);
        Assert(i <>-1,'Thread not attached');
        lProgInd := TProgInd(FProgInds.Items[i]);
        lProgInd.Thread := nil;
        pThread.ProgInd := nil;
        lProgInd.Free;
        FProgInds.Delete(i);
        ArrangePanels;
        LCount:= FProgInds.Count;
        result := (LCount = 0);
    finally
      FCritSect.Leave;
    end;
  finally
    FTimer.Enabled := LCount <> 0;
  end;
  if Assigned(FOnChangeThreadCount) then
    FOnChangeThreadCount(Self);
end;



function TFormThreadProgress.FindByThread(pThread : TtiThreadProgress): integer;
var
  i : integer;
begin
  FCritSect.Enter;
  try
    result := -1;
    for i := 0 to FProgInds.Count - 1 do
      if TProgInd(FProgInds.Items[i]).Thread = pThread then begin
        result := i;
        Break; //==>
      end;
  finally
    FCritSect.Leave;
  end;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
destructor TtiThreadProgress.Destroy;
begin
  if AutoProgress then
    gINI.WriteInteger('ThreadProgress',
                       ClassName,
                       FPosition + 1);
  inherited;
end;


procedure TtiThreadProgress.DoOnTerminate(sender : TObject);
begin
  gFormThreadProgress.DetachThread(self);
end;


procedure TtiThreadProgress.IncPosition;
begin
  Position := Position + 1;
end;


procedure TtiThreadProgress.SetAutoProgress(const AValue: Boolean);
begin
  FAutoProgress := AValue;
  if AutoProgress then begin
    FPosition := 0;
    FMin     := 0;
    Max := gINI.ReadInteger('ThreadProgress', ClassName, 10);
    if Max = 0 then
      Max := 10;
  end;
end;


procedure TtiThreadProgress.SetMax(const AValue: integer);
begin
  FMax := AValue;
  Synchronize(SynchronizeProgress);
end;


procedure TtiThreadProgress.SetMin(const AValue: integer);
begin
  FMin := AValue;
  Synchronize(SynchronizeProgress);
end;


procedure TtiThreadProgress.SetPosition(const AValue: integer);
begin
  FPosition := AValue;
  Synchronize(SynchronizeProgress);
end;


procedure TtiThreadProgress.SetText(const AValue: string);
begin
  FText := AValue;
  Synchronize(SynchronizeProgress);
end;


procedure TtiThreadProgress.SetCaption(const AValue: TCaption);
begin
  FCaption := AValue;
  Synchronize(SynchronizeProgress);
end;


procedure TtiThreadProgress.SynchronizeProgress;
const
  CLimit = 65535; // Under some circumstances, the progress bar has a limit of 65535
                  // See ComCtrls for more info
begin
  FProgInd.Min     := FMin;
  if (FMax > CLimit) or (FPosition > CLimit) then
  begin
    FProgInd.Max     := FMax div 2;
    FProgInd.Position := FPosition div 2;
  end else
  begin
    FProgInd.Max     := FMax;
    FProgInd.Position := FPosition;
  end;

  FProgInd.Text    := FText;
  FProgInd.Caption := FCaption;
  FProgInd.CanCancel := FCancancel;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormThreadProgress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TFormThreadProgress.Create(AOwner: TComponent);
begin
  Assert(False, 'Do not call TFormThreadProgress.Create, call CreateNew instead.');
end;


procedure TFormThreadProgress.ArrangePanels;
var
  lProgInd : TProgInd;
  i       : integer;
  lRow    : integer;
  lCol    : integer;
  lTop    : integer;
  lLeft   : integer;
  lWidth  : integer;
begin
  lRow := 1;
  lCol := 1;
  for i := 0 to FProgInds.Count - 1 do
  begin
    lProgInd := TProgInd(FProgInds.items[i]);

    if lCol = 1 then
      lLeft := 4
    else
      lLeft := (ClientWidth div ColumnCount) {+ 8};

    lWidth := (ClientWidth div ColumnCount) {- 8};
    lTop := (lRow - 1) * (lProgInd.Height + 2) + 2;

    lProgInd.SetBounds(lLeft, lTop, lWidth, cuiProgIndHeight);

    if lCol < ColumnCount then
      Inc(lCol)
    else begin
      lCol := 1;
      Inc(lRow);
    end;
  end;

  ClientHeight := lRow * (cuiProgIndHeight + 4) + 4;

  if Parent = nil then
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    if FProgInds.Count > 0 then
    begin
      Constraints.MinHeight := Height;
      Constraints.MinWidth := cuiFormWidth;
      Constraints.MaxHeight := Height;
      Constraints.MaxWidth := cuiFormWidth;
    end
  end;

  Visible := (FProgInds.Count > 0);
end;


procedure TFormThreadProgress.DoCloseQuery(sender: TObject; var CanClose : boolean);
begin
  CanClose   := false;
  WindowState := wsMinimized;
end;


procedure TFormThreadProgress.UpdateAutoProgressThreads(Sender: TObject);
var
  i : integer;
  lProgInds : TProgInd;
begin
  FCritSect.Enter;
  try
    for i := 0 to FProgInds.Count - 1 do begin
      lProgInds := TProgInd(FProgInds.Items[i]);
      if (not lProgInds.Thread.Suspended) and lProgInds.Thread.AutoProgress then
        if lProgInds.Thread.Position < lProgInds.Thread.Max then
          lProgInds.Thread.Position := lProgInds.Thread.Position + 1
        else
          lProgInds.Thread.Max := Trunc(lProgInds.Thread.Max * 2);
    end;
  finally
    FCritSect.Leave;
  end;
end;


procedure TProgInd.TerminateOnClick(sender: TObject);
begin
  if (not Thread.ConfirmCancel)
  or tiAppConfirmation('Are you sure you want to terminate <' + Text + '> ?') then
  begin
    {$IF Defined(FPC) and Defined(UNIX) }
    {Under GTK event should not continue if object has already gone.
     We need to do it asynchronously,because if user confirmed too late
     and thread terminated - this object (TProgInd) has been destroyed
     in DetachThread}
    Application.QueueAsyncCall(Async,PtrInt(Thread));
    {$ELSE}
    if uFormThreadProgress.FindByThread(Thread)=-1 then Exit;
    try
    if Assigned(Thread.OnCancel) then
      Thread.OnCancel(Thread);
    Thread.ReturnValue := 1;
    Thread.Resume;
    Thread.Terminate;
    FSpeedButton.Enabled := false;
    Text := cuWaitForTerminate;
    except
      {In rare situation AV occur - when thread finished
       after FindByThread check.
       (for example when OnCancel has taken too much time)
      }
    end;
     {$IFEND}
  end;
end;


procedure TtiThreadProgress.SetCanCancel(const AValue: boolean);
begin
  FCanCancel := AValue;
end;


function TProgInd.GetCanCancel: boolean;
begin
  result := (FSpeedButton <> nil) and FSpeedButton.Visible;
end;


procedure TProgInd.SetCanCancel(const AValue: boolean);
begin
  FSpeedButton.Visible := AValue;
  FProgressBar.Width := GetProgressBarWidth;
end;


function TProgInd.GetProgressBarWidth : integer;
begin
  result := Self.ClientWidth - FProgressBar.Left - 8;
  if CanCancel then
    Dec(Result, cuCancelButtonSize);
end;

{$IFDEF FPC}
procedure TProgInd.Async(Data: PtrInt);
var
 Thread : TtiThreadProgress;
begin
    Thread := TtiThreadProgress(Data);
    if uFormThreadProgress.FindByThread(Thread)=-1 then Exit;
    try
    if Assigned(Thread.OnCancel) then
      Thread.OnCancel(Thread);
    Thread.ReturnValue := 1;
    Thread.Terminate;
    FSpeedButton.Enabled := false;
    Text := cuWaitForTerminate;
    except
      {In rare situation AV occur - when thread finished
       after FindByThread check.
       (for example when OnCancel has taken too much time)
      }
    end;
end;
{$ENDIF}

{$IFDEF NOTHREADS}
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TThreadDebugger
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  constructor TThreadDebugger.Create(Suspended: boolean);
  begin
    inherited Create;
    FbSuspended := Suspended;
    FbTerminated := false;
    if not Suspended then
      Resume;
  end;

  procedure TThreadDebugger.Resume;
  begin
    FbSuspended := false;
    Execute;
  end;

  procedure TThreadDebugger.Synchronize(Method: TThreadMethod);
  begin
    if Assigned(Method) then
      Method;
  end;

  procedure TThreadDebugger.Terminate;
  begin
    Terminated := true;
  end;
  
  procedure  TThreadDebugger.DoOnTerminate(sender : TObject);
  begin
  end;
{$ENDIF}

constructor TtiThreadProgress.Create(ACreateSuspended: Boolean);
begin
  inherited Create(true);
  gFormThreadProgress.AttachThread(self);
  AutoProgress := true;
  CanCancel := false;
  ConfirmCancel := True;
  ReturnValue   := 0;
  if not ACreateSuspended then
    Resume;
end;

function TFormThreadProgress.GetThreadCount: integer;
begin
  FCritSect.Enter;
  try
    result := FProgInds.Count;
  finally
    FCritSect.Leave;
  end;
end;

procedure TFormThreadProgress.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  uFormThreadProgressIsOwned := (AParent <> nil);
end;

initialization
  uFormThreadProgress := nil;
  uFormThreadProgressIsOwned := false;

finalization

  if (uFormThreadProgress <> nil) and
     (not uFormThreadProgressIsOwned) then
    FreeAndNil(uFormThreadProgress);

end.
