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

    GTIOPFManager.TerminateThreads(Period)
    (where Period could be 0 if not waiting
    or X seconds to wait)


   or

   2. Put into mainform OnCloseQuery event:
      (GTIOPFManager.ActiveThreadList.RunningThreadCount could also be used)

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

{ Ian K - 2010-07-08
  Up until V3, THTMLViewer has provided a 'hot' label behaviour.
  This however has required the 3rd party Html (PBear) component which
  needed updating for D2009+ (Unicode support). At this point in time it has not been ported
  to tiOPF3 but I have a copy if needed.
  It may however be better to refactor the hot behaviour to avoid this component altogether.
  For now, I have introduced a directive - USEHTMLVIEW - so it can be turned on if the Html component
  is available.
  Note also that this is only applicable for Win platform.
  cf tiOPF newsgroup topic early 2009(?).

}
{$IFNDEF FPC}
  {.$DEFINE USEHTMLVIEW}
{$ENDIF}

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
  {$IFDEF USEHTMLVIEW}
  ,Htmlview
  {$ENDIF}
  {$IFNDEF FPC}
  ,tiAnimatedGIF
  {$ENDIF}
 ;

type

  TProgInd = class;
  TProgressVisibleChangeEvent = procedure(const AVisible: Boolean) of object;

  TtiThreadProgress = class(TtiThread)
  private
    FProgInd : TProgInd;
    FPosition : integer;
    FMin: integer;
    FMax: integer;
    FAutoProgress: boolean;
    FCaption: TCaption;
    FCanCancel: boolean;
    FConfirmCancel : boolean;
    FOnCancel : TNotifyEvent;
    {$IFNDEF FPC}
    FShowAnimation: boolean;
    {$ENDIF}

    //FCrtiSectLabelHotspotClick: TCriticalSection;

    procedure SynchronizeProgress;
    procedure SetPosition(const AValue: integer);
    procedure SetMax(const AValue: integer);
    procedure SetMin(const AValue: integer);
    procedure SetAutoProgress(const AValue: Boolean);
    procedure SetCaption(const AValue: TCaption);
    procedure SetCanCancel(const AValue: boolean);
    {$IFNDEF FPC}
    procedure SetShowAnimation(const Value: boolean);
    {$ENDIF}
    {$IFDEF USEHTMLVIEW}
    procedure OnLabelHotspotClick(Sender: TObject; const SRC: string;
                     var Handled: boolean);
    {$ENDIF}
  protected
    procedure   DoOnTerminate(sender : TObject); override;
    {$IFNDEF FPC}
    procedure   DoLabelHotspotClick(Sender: TObject; const SRC: string;
                     var Handled: boolean); virtual;
    {$ENDIF}
    procedure   SetText(const AValue: string); override;
  public
    constructor Create(ACreateSuspended: Boolean); override;
    Destructor  Destroy; override;
    Property    ProgInd : TProgInd     read FProgInd       write FProgInd;
    Property    Max    : integer      read FMax          write SetMax;
    Property    Min    : integer      read FMin          write SetMin;
    Property    Position : integer     read FPosition     write SetPosition;
    Property    Caption : TCaption    read FCaption      write SetCaption;
    Property    AutoProgress : Boolean read FAutoProgress write SetAutoProgress;
    Property    CanCancel : boolean    read FCanCancel     write SetCanCancel;
    property    ConfirmCancel : boolean    read FConfirmCancel   write FConfirmCancel;
    property    OnCancel     : TNotifyEvent read FOnCancel      write FOnCancel;
    {$IFNDEF FPC}
    property    ShowAnimation: boolean read FShowAnimation write SetShowAnimation;
    {$ENDIF}
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
    FOnVisibleChange: TProgressVisibleChangeEvent;

    function    FindByThread(pThread : TtiThreadProgress): integer;
    procedure   ArrangePanels;
    procedure   DoCloseQuery(sender : TObject; var CanClose : boolean);
    procedure   DoResize(Sender: TObject);
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
    property    OnVisibleChange: TProgressVisibleChangeEvent read FOnVisibleChange write FOnVisibleChange;

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
    {$IFDEF USEHTMLVIEW}
    FLabel      : THTMLViewer;
    {$ELSE}
    FLabel      : TLabel;
    {$ENDIF}
    FProgressBar: TtiProgressBar;
    FSpeedButton: TSpeedButton;
    FThread     : TtiThreadProgress;
    // Store thread ID separately to be able to check if thread is still
    // running when the app is being terminated and thread and thread object
    // may have been removed under our feet
    {$IFDEF MSWINDOWS}
    FThreadID   : THandle;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FThreadID   : Cardinal;
    {$ENDIF LINUX}
    FCaption    : TCaption;
    {$IFNDEF FPC}
    FShowAnimation: boolean;
    FAnimation  : TtiAnimatedGIF;
    {$ENDIF}
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
    function    GetLabelWidth: integer;
    {$IFNDEF FPC}
    procedure   SetShowAnimation(const AValue: boolean);
    function    AnimationShowing: boolean;
    procedure   AddAnimation;
    procedure   RemoveAnimation;
    procedure   SetThread(const AValue: TtiThreadProgress);
    {$ENDIF}
    {$IFDEF USEHTMLVIEW}
    function    GetOnLabelHotspotClick: THotSpotClickEvent;
    procedure   SetOnLabelHotspotClick(const AValue: THotSpotClickEvent);
    {$ENDIF}
    {$IFDEF FPC}
    procedure Async(Data: PtrInt);
    {$ENDIF}
  public
    Constructor Create(AOwner : TComponent); override;
    procedure   Attach(AThread: TtiThreadProgress);
    procedure   Detach;
    property    Position : integer read GetPosition  write SetPosition;
    property    Max : integer read GetMax  write SetMax;
    property    Min : integer read GetMin  write SetMin;
    property    Text : string read GetText write SetText;
    Property    Caption : TCaption read GetCaption write SetCaption;
    property    Thread : TtiThreadProgress read FThread write SetThread;
    {$IFDEF MSWINDOWS}
    property    ThreadID: THandle read FThreadID;
    {$ENDIF}
    {$IFDEF LINUX}
    property    ThreadID: Cardinal read FThreadID;
    {$ENDIF}
    property    CanCancel : boolean read GetCanCancel write SetCanCancel;
    {$IFNDEF FPC}
    property    ShowAnimation: boolean read FShowAnimation write SetShowAnimation;
    {$ENDIF}
    {$IFDEF USEHTMLVIEW}
    property    OnLabelHotspotClick: THotSpotClickEvent read GetOnLabelHotspotClick write SetOnLabelHotspotClick;
    {$ENDIF}
  end;

// The FormThreadProgress is a Singleton
function gFormThreadProgress : TFormThreadProgress;

implementation
uses
  tiUtils
  ,tiLog
  ,tiDialogs
  ,tiResources
  ,tiOPFManager
  ,SysUtils
  {$IFNDEF FPC}
  ,Windows
  {$ELSE}
  ,Lmessages
  ,LCLIntf
  {$ENDIF}
  ,tiGUIINI
  ,Dialogs
  {$IFDEF MSWINDOWS}
  ,tiWin32
  {$ENDIF MSWINDOWS}
 ;

var
  uFormThreadProgress : TFormThreadProgress;
  uFormThreadProgressIsOwned : boolean;

const
  cuiProgIndHeight   =  30;
  cuiFormWidth       = 450;
  cuiLabelWidth      = 150;
  cuiAnimationWidth  = 16;
  cuCancelButtonSize = 17;
  cuWaitForTerminate = 'Waiting to terminate';
  {$IFDEF FPC}
  INVALID_HANDLE_VALUE = Cardinal(-1);
  {$ENDIF FPC}

function gFormThreadProgress : TFormThreadProgress;
begin
  if uFormThreadProgress = nil then
    uFormThreadProgress := TFormThreadProgress.CreateNew(nil);
  result := uFormThreadProgress;
end;

{ TProgInd }

constructor TProgInd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner)   ;
  Parent      := (AOwner as TFormThreadProgress);
  BevelInner  := (AOwner as TFormThreadProgress).PanelBevelInner;
  BevelOuter  := (AOwner as TFormThreadProgress).PanelBevelOuter;
  Color       := (AOwner as TFormThreadProgress).Color;
  Height      := cuiProgIndHeight;
  Left        := 4;
  Anchors     := [akLeft,akTop];
  Width       := TForm(Owner).ClientWidth - 8;

  FThreadID   := INVALID_HANDLE_VALUE;

  {$IFDEF USEHTMLVIEW}
  FLabel                 := THtmlviewer.Create(self);
  {$ELSE}
  FLabel                 := TLabel.Create(self);
  {$ENDIF}
  FLabel.Parent          := self;
  FLabel.Left            := 6;
  FLabel.Top             := 1;
  {$IFDEF USEHTMLVIEW}
  FLabel.DefFontSize := 8;
  FLabel.MarginHeight := 0;
  Flabel.MarginWidth := 0;
  FLabel.BorderStyle := htNone;
  FLabel.DefBackground := (AOwner as TFormThreadProgress).Color;
  {$ENDIF}
  FLabel.Width           := GetLabelWidth;
  FLabel.Height          :=  16;
  {$IFDEF USEHTMLVIEW}
  FLabel.DefFontName := TForm(Owner).Font.Name;
  {$ENDIF}

  FProgressBar           := TtiProgressBar.Create(self);
  FProgressBar.Parent    := self;
  FProgressBar.Left      := 4;//cuiLabelWidth + 20;
  FProgressBar.Top       := 18;
  FProgressBar.Width     := GetProgressBarWidth;
  FProgressBar.Height    :=  12;
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


procedure TProgInd.Attach(AThread: TtiThreadProgress);
begin
  Assert(Assigned(AThread), 'AThread must be assigned');
  Thread := AThread;
  AThread.ProgInd := Self;
end;


procedure TProgInd.Detach;
begin
  Assert(Assigned(Thread), 'Thread must be assigned');
  Thread.ProgInd := nil;
  Thread := nil;
end;


function TProgInd.GetMax: integer;
begin
  result := FProgressBar.Max;
end;


function TProgInd.GetMin: integer;
begin
  result := FProgressBar.Min;
end;


{$IFDEF USEHTMLVIEW}
function TProgInd.GetOnLabelHotspotClick: THotSpotClickEvent;
begin
  Assert(Assigned(FLabel));
  result := FLabel.OnHotspotClick;
end;
{$ENDIF}

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


{$IFDEF USEHTMLVIEW}
procedure TProgInd.SetOnLabelHotspotClick(const AValue: THotSpotClickEvent);
begin
  Assert(Assigned(FLabel));
  FLabel.OnHotspotClick := AValue;
end;
{$ENDIF}

procedure TProgInd.SetPosition(const AValue: integer);
begin
  FProgressBar.Position := AValue;
end;


{$IFNDEF FPC}
procedure TProgInd.SetShowAnimation(const AValue: boolean);
begin
  if AValue <> FShowAnimation then
  begin
    FShowAnimation := AValue;
    if FShowAnimation and (not AnimationShowing) then
      AddAnimation
    else if (not FShowAnimation) and AnimationShowing then
      RemoveAnimation;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
function TProgInd.AnimationShowing: boolean;
begin
  result := Assigned(FAnimation);
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TProgInd.AddAnimation;
begin
  if not AnimationShowing then
  begin
    FAnimation := TtiAnimatedGIF.Create(self);
    FAnimation.ResourceName := cResTI_NetworkTraffic;
    FAnimation.AutoSize := true;
    // Right aligned to progress area.
    FAnimation.Left := self.ClientWidth - FAnimation.Width - 8;
    FAnimation.Anchors := [akTop, akRight];
    // Centre vertically.
    FAnimation.Top := FProgressBar.Top + ((FProgressBar.Height - FAnimation.Height) div 2) - 2;
//    FAnimation.Top := (Height - FAnimation.Height) div 2;
    FAnimation.Transparent := true;
    FAnimation.Parent := self;
    FAnimation.AnimationSpeed := 300;
    FAnimation.AnimationEnabled := true;

    // Adjust the progress bar to make room for the animation.
    FProgressBar.Width := GetProgressBarWidth;
    FLabel.Width       := GetLabelWidth;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TProgInd.RemoveAnimation;
begin
  if AnimationShowing then
  begin
    FreeAndNil(FAnimation);
    // Adjust the progress bar to fill the space where the animation was displayed.
    FProgressBar.Width := GetProgressBarWidth;
    FLabel.Width       := GetLabelWidth;
  end;
end;
{$ENDIF}

function TProgInd.GetText: string;
begin
  {$IFDEF USEHTMLVIEW}
  result := FLabel.DocumentSource;
  {$ELSE}
  result := FLabel.Caption;
  {$ENDIF}
end;


procedure TProgInd.SetText(const AValue: string);
begin
  {$IFDEF USEHTMLVIEW}
  if FLabel.DocumentSource <> cuWaitForTerminate then
    FLabel.LoadFromString(AValue);
  {$ELSE}
  FLabel.Caption := AValue;
  {$ENDIF}
end;


procedure TProgInd.SetThread(const AValue: TtiThreadProgress);
begin
  FThread := AValue;
  if Assigned(AValue) then
    FThreadID := AValue.ThreadID
  else
    FThreadID := INVALID_HANDLE_VALUE;
end;


function TProgInd.GetCaption: TCaption;
begin
  Result := FCaption;
end;


function TProgInd.GetLabelWidth: integer;
begin
  result := Self.ClientWidth - FLabel.Left - 8;
  {$IFNDEF FPC}
  if AnimationShowing then
    result := result - FAnimation.Width - 6;
  {$ENDIF}
  if CanCancel then
    Dec(Result, cuCancelButtonSize);
end;


procedure TProgInd.SetCaption(const AValue: TCaption);
begin
  FCaption := AValue;
  if (Owner is TCustomForm) then TCustomForm(Owner).Caption := ' Progress : ' + FCaption;
end;

{ TFormThreadProgress }

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
  gGUIINI.ReadFormState(self);
  OnCloseQuery := DoCloseQuery;
  OnResize := DoResize;
  FTimer      := TTimer.Create(Self);
  FTimer.Interval:= 500;
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


//TODO: Better co-ordination during destroy and thread termination.
// There are a few potential problems here if TFormThreadProgress is being
// destroyed and the threads are terminating at the same time. When threads
// call DetachThread we could be in the late stages of Destroy, or destroyed
// altogether (in which case a new global TFormThreadProgress will be created
// which will cause an assertion failure as the thread will not be found).
destructor TFormThreadProgress.Destroy;
var
  i : integer;
  LProgInd: TProgInd;
begin
  FCritSect.Enter;
  try
    for i := 0 to FProgInds.Count - 1 do
    begin
      LProgInd := TProgInd(FProgInds.Items[i]);
      if Assigned(LProgInd) then
      begin
        if Assigned(LProgInd.Thread) then
        begin
          {$IFDEF MSWINDOWS}
          // Thread might have been removed from under our feet during app
          // termination.
          // On Windows ThreadID is the same as the thread handle
          if tiWin32ThreadRunning(LProgInd.ThreadID) then
            LProgInd.Thread.Terminate;
          {$ELSE}
          LProgInd.Thread.Terminate;
          {$ENDIF MSWINDOWS}
        end;

        LProgInd.Detach;
      end;
    end;
  finally
    FCritSect.Leave;
  end;

  FTimer.Free;
  gGUIINI.WriteFormState(self);
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
      lProgInd.Attach(pThread);
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
        lProgInd.Detach;
        lProgInd.Free;
        FProgInds.Delete(i);
        if (not ShuttingDown) and (not GTIOPFManager.Terminated) then
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


{ TtiThreadProgress }


destructor TtiThreadProgress.Destroy;
begin
  if AutoProgress then
    gGUIINI.WriteInteger('ThreadProgress',
                       ClassName,
                       FPosition + 1);
//  FCrtiSectLabelHotspotClick.Free;
  inherited;
end;


{$IFNDEF FPC}
procedure TtiThreadProgress.DoLabelHotspotClick(Sender: TObject;
  const SRC: string; var Handled: boolean);
begin
  if not Handled then
  begin
    tiShellExecute(SRC);
    Handled := true;
  end;
end;
{$ENDIF}

procedure TtiThreadProgress.DoOnTerminate(sender : TObject);
begin
  inherited;
  if (not ShuttingDown) and (not GTIOPFManager.Terminated) then
    gFormThreadProgress.DetachThread(self);
end;


procedure TtiThreadProgress.IncPosition;
begin
  Position := Position + 1;
end;


{$IFDEF USEHTMLVIEW}
procedure TtiThreadProgress.OnLabelHotspotClick(Sender: TObject;
  const SRC: string; var Handled: boolean);
begin
//  FCrtiSectLabelHotspotClick.Enter;
//  try
    DoLabelHotspotClick(Sender, SRC, Handled);
//  finally
//    FCrtiSectLabelHotspotClick.Leave;
//  end;
end;
{$ENDIF}

procedure TtiThreadProgress.SetAutoProgress(const AValue: Boolean);
begin
  FAutoProgress := AValue;
  if AutoProgress then
  begin
    FPosition := 0;
    FMin     := 0;
    Max := gGUIINI.ReadInteger('ThreadProgress', ClassName, 10);
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

{$IFNDEF FPC}
procedure TtiThreadProgress.SetShowAnimation(const Value: boolean);
begin
  if Value <> FShowAnimation then
  begin
    FShowAnimation := Value;
    if Assigned(FProgInd) then
      FProgInd.ShowAnimation := FShowAnimation;
  end;
end;
{$ENDIF}

procedure TtiThreadProgress.SetText(const AValue: string);
begin
  inherited;
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
  if Assigned(FProgInd) and (not ShuttingDown) and
     (not GTIOPFManager.Terminated) then
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

    FProgInd.Text    := Text;
    FProgInd.Caption := FCaption;
    FProgInd.CanCancel := FCancancel;
  end;
end;


{ TFormThreadProgress }

constructor TFormThreadProgress.Create(AOwner: TComponent);
begin
  Assert(False, 'Do not call TFormThreadProgress.Create, call CreateNew instead.');
end;


procedure TFormThreadProgress.ArrangePanels;
var
  LProgInd : TProgInd;
  i       : integer;
  LRow    : integer;
  LCol    : integer;
  LTop    : integer;
  LLeft   : integer;
  LWidth  : integer;
  LVisible: Boolean;
  LRows   : Integer;
  LColumns: Integer;
  LRowsFirst: Boolean;
const
  CClientSpace = 0; // Space inside the edges of the form
  CVerticalSpace = 0; // Vertical space between progress bar panels.
begin
  LRowsFirst := ColumnCount = 0;
  if LRowsFirst then
  begin
    // Fit as many rows as possible first
    LRows := (ClientHeight - CClientSpace) div (cuiProgIndHeight + CVerticalSpace);
    if LRows = 0 then
      LRows := 1;
    // How many columns required for this number of rows
    if FProgInds.Count > 0 then
      LColumns := ((FProgInds.Count - 1) div LRows) + 1
    else
      LColumns := 0;
  end
  else
  begin
    // Fit minimum number of columns first.
    if FProgInds.Count > ColumnCount then
      LColumns := ColumnCount
    else
      LColumns := FProgInds.Count;
    // How many rows required for this number of columns
    if FProgInds.Count > 0 then
      LRows := ((FProgInds.Count - 1) div LColumns) + 1
    else
      LRows := 0;
  end;

  if LColumns > 0 then
    LWidth := (ClientWidth - (CClientSpace * 2)) div LColumns
  else
    LWidth := 0;

  LRow := 0;
  LCol := 0;
  for i := 0 to FProgInds.Count - 1 do
  begin
    LProgInd := TProgInd(FProgInds.items[i]);

    LLeft := (LWidth * LCol) + CClientSpace;
    LTop := ((cuiProgIndHeight + CVerticalSpace) * LRow) + CClientSpace;

    LProgInd.SetBounds(LLeft, LTop, LWidth, cuiProgIndHeight);

    if LRowsFirst then
    begin
      if LRow < (LRows - 1) then
        Inc(LRow)
      else
      begin
        LRow := 0;
        Inc(LCol);
      end;
    end
    else
    begin
      if LCol < (LColumns - 1) then
        Inc(LCol)
      else
      begin
        LCol := 0;
        Inc(LRow);
      end;
    end;
  end;

  if not LRowsFirst then
    ClientHeight := ((cuiProgIndHeight + CVerticalSpace) * LRows) + (CClientSpace * 2);

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

  LVisible := Visible;
  Visible := (FProgInds.Count > 0);
  if Visible <> LVisible then
    if Assigned(FOnVisibleChange) then
      FOnVisibleChange(Visible);
end;


procedure TFormThreadProgress.DoCloseQuery(sender: TObject; var CanClose : boolean);
begin
  CanClose   := false;
  WindowState := wsMinimized;
end;


procedure TFormThreadProgress.DoResize(Sender: TObject);
begin
  ArrangePanels;
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
    Thread.Start;
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
  FLabel.Width       := GetLabelWidth;
end;


function TProgInd.GetProgressBarWidth : integer;
begin
  result := Self.ClientWidth - FProgressBar.Left - 8;
  {$IFNDEF FPC}
  if AnimationShowing then
    result := result - FAnimation.Width - 6;
  {$ENDIF}
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
{ TThreadDebugger }

  constructor TThreadDebugger.Create(Suspended: boolean);
  begin
    inherited Create;
    FbSuspended := Suspended;
    FbTerminated := false;
    if not Suspended then
      Resume;
  end;

  procedure TThreadDebugger.Start;
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
  inherited Create(ACreateSuspended);
  gFormThreadProgress.AttachThread(self);
  Assert(Assigned(FProgInd), 'Expected FProgInd to be assigned after attach');
  AutoProgress := true;
  CanCancel := false;
  ConfirmCancel := True;
  ReturnValue   := 0;

  {$IFDEF USEHTMLVIEW}
  //FCrtiSectLabelHotspotClick := TCriticalSection.Create;
  FProgInd.OnLabelHotspotClick := OnLabelHotspotClick;
  {$ENDIF}
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
