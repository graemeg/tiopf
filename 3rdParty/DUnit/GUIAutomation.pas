{ #(@)$Id: GUIAutomation.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.35 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Serge Beaumont
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2008.
 * All rights reserved.
 *
 * Contributor(s):
 * Serge Beaumont <beaumose@iquip.nl>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * Jon Bertrand <jonbsfnet@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 * TODO:
 *   Allow replay of a text script.
 *)

{$I DUnit.inc}

unit GUIAutomation;

interface
uses
{$IFDEF LINUX}
  Types,
{$ELSE}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF DUNIT_CLX}
  Qt,
  QControls,
  QForms,
{$ELSE}
  Controls,
  Forms,
{$ENDIF}
  SysUtils,
  Classes,
  SyncObjs;

const
  rcs_id: string = '#(@)$Id: GUIAutomation.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $';

{$IFDEF DUNIT_CLX}
const
  VK_TAB = KEY_TAB;
{$ENDIF}

const
  CDefaultControlWaitInterval = 5000; // Milliseconds
  CDefaultMouseMovePixelsPerSecond = 1000;

type
  EGUIAutomation = class(Exception);
  EGUIAutomationControlNotFound = class(EGUIAutomation);

  TOnGetContinueExecutionEvent = procedure(var AContinueExecution: boolean) of object;

  TGUIAutomation = class(TObject)
  private
    FWakeEvent: TEvent;
    FActionDelay: Cardinal;
    FMouseMoveDelay: Cardinal;
    FKeyDownDelay: Cardinal;
    FTextEntryDelay: Cardinal;
    FControlWaitPeriod: Cardinal;
    FOnGetContinueExecution: TOnGetContinueExecutionEvent;

    function ContinueExecution: boolean;
    procedure SleepAndCheckContinue(const AInterval: Cardinal);
    function FindControlInstance(const AComp: TComponent; const AControlName: string): TControl; overload;
    function FindControlInstance(const AComp: TComponent; const AControlName: string;
        const AInterval: Cardinal): TControl; overload;
    function GetWinControl(var AControl: TControl; var AX: Integer;
        var AY: Integer): boolean;
    // Low-level message sending
    procedure MoveMouse(const AHwnd: HWND; const AX: Integer; const AY: Integer);
    procedure MouseButtonDownOn(const AHwnd: HWND; const AMouseDownMsg: UINT;
        const AX: Integer; const AY: Integer); overload;
    procedure MouseButtonUpOn(const AHwnd: HWND; const AMouseUpMsg: UINT;
        const AX: Integer; const AY: Integer); overload;
    procedure ClickMouseButtonOn(const AHwnd: HWND;
        const AMouseDownMsg: UINT; const AMouseUpMsg: UINT;
        const AX: Integer; const AY: Integer); overload;
    // Mid-level mouse actions with action delay
    procedure MouseButtonDownOn(const AHwnd: HWND; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure MouseButtonDownOn(const AControl: TControl; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure MouseButtonUpOn(const AHwnd: HWND; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure MouseButtonUpOn(const AControl: TControl; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure ClickMouseButtonOn(const AHwnd: HWND; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure ClickMouseButtonOn(const AControl: TControl; const AButton: TMouseButton;
        const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure DoubleClickMouseButtonOn(const AHwnd: HWND; const AButton: TMouseButton;
        const AX: Integer; const AY: Integer); overload;
    procedure DoubleClickMouseButtonOn(const AControl: TControl; const AButton: TMouseButton;
        const AX: Integer = -1; const AY: Integer = -1); overload;
  protected
    procedure SyncMessages;
    function WaitForWindowEnabled(const AHwnd: HWND): boolean;
    function ControlAtActiveWindowCoord(const AX: Integer; const AY: Integer;
        out AControlHwnd: HWND; out APoint: TPoint): boolean;
    function  FindParentWinControl(const AControl: TControl):TWinControl;

{$IFNDEF DUNIT_CLX}
    { Windows specific keyboard state code }
    procedure SetKeyboardStateDown(pShiftState: TShiftState);
    procedure SetKeyboardStateUp(pShiftState: TShiftState);
{$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;
{$IFDEF DELPHI2009_UP}
    procedure ThreadedExecute(AProc: TThreadProcedure);
{$ENDIF DELPHI2009_UP}
    // Find in active form
    function FindControl(const AControlName: string; const AAddrs: Pointer = nil): TControl; overload;
    // Find in given parent component
    function FindControl(const AComp: TComponent; const AControlName: string; const AAddrs: Pointer = nil): TControl; overload;

    function WaitForControlExists(const AControlName: string; const AExists: Boolean = true; const AInterval: Cardinal = CDefaultControlWaitInterval): boolean;
    function WaitForControlVisible(const AControlName: string; const AVisible: Boolean = true; const AInterval: Cardinal = CDefaultControlWaitInterval): boolean;
    function WaitForControlEnabled(const AControlName: string; const AEnabled: Boolean = true; const AInterval: Cardinal = CDefaultControlWaitInterval): boolean;

    // Given windowed Control
    procedure MoveMouseTo(const AHwnd: HWND; const AX: Integer = -1; const AY: Integer = -1;
        const APixelInterval: Integer = -1; const APixelsPerSecond: Integer = CDefaultMouseMovePixelsPerSecond); overload;

    // Control (VCL or other) under mouse cursor, position is relative to GUI
    procedure LeftMouseDownAt(const AX: Integer; const AY: Integer);
    procedure LeftMouseUpAt(const AX: Integer; const AY: Integer);
    procedure LeftClickAt(const AX: Integer; const AY: Integer);
    procedure LeftDoubleClickAt(const AX: Integer; const AY: Integer);
    procedure RightMouseDownAt(const AX: Integer; const AY: Integer);
    procedure RightMouseUpAt(const AX: Integer; const AY: Integer);
    procedure RightClickAt(const AX: Integer; const AY: Integer);
    procedure RightDoubleClickAt(const AX: Integer; const AY: Integer);

    // Active Control, default position is centre of Control
    procedure MoveMouseTo(const AX: Integer = -1; const AY: Integer = -1;
        const APixelInterval: Integer = -1; const APixelsPerSecond: Integer = CDefaultMouseMovePixelsPerSecond); overload;
    procedure LeftMouseDown(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftMouseUp(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftDoubleClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseDown(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseUp(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightDoubleClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    // Given Control, default position is centre of Control
    procedure MoveMouseTo(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1;
        const APixelInterval: Integer = -1; const APixelsPerSecond: Integer = CDefaultMouseMovePixelsPerSecond); overload;
    procedure MoveMouseTo(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1;
        const APixelInterval: Integer = -1; const APixelsPerSecond: Integer = CDefaultMouseMovePixelsPerSecond); overload;
    procedure LeftMouseDown(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftMouseDown(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftMouseUp(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftMouseUp(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftClick(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftClick(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1);   overload;
    procedure LeftDoubleClick(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftDoubleClick(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseDown(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseDown(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseUp(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightMouseUp(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightClick(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightClick(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1);   overload;
    procedure RightDoubleClick(const AControlName: string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightDoubleClick(const AControl: TControl; const AX: Integer = -1; const AY: Integer = -1); overload;

    procedure EnterKey(AKey :Word; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControlHwnd: HWND;  AKey: Word; const AShiftState: TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControl: TControl;   AKey :Word; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControlName :string; AKey :Word; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;

    procedure EnterKey(AKey :Char; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControlHwnd: HWND;  AKey: Char; const AShiftState: TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControl: TControl;   AKey :Char; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;
    procedure EnterKeyInto(const AControlName :string; AKey :Char; const AShiftState :TShiftState = []; const ACount: Integer = 1); overload;

    procedure EnterText(AText :string);
    procedure EnterTextInto(const AControlHwnd: HWND; AText: string); overload;
    procedure EnterTextInto(const AControl: TControl;   AText :string); overload;
    procedure EnterTextInto(const AControlName: string; AText: string); overload;

    procedure Show(const AControl: TControl; AOnOff: boolean = true); overload;
    procedure Show(const AControlName: string; AOnOff: boolean = true); overload;

    procedure Hide(const AControl: TControl); overload;
    procedure Hide(const AControlName: string); overload;

    procedure Tab(n :Integer =1);

    procedure SetFocus(const AControl: TControl; const AAddrs: Pointer = nil); overload;
    procedure SetFocus(const AControlName: string); overload;

    procedure SyncSleep(const AInterval: Cardinal);
    procedure WakeUp;

    property ActionDelay: Cardinal read FActionDelay write FActionDelay;
    property MouseMoveDelay: Cardinal read FMouseMoveDelay write FMouseMoveDelay;
    property KeyDownDelay: Cardinal read FKeyDownDelay write FKeyDownDelay;
    property TextEntryDelay: Cardinal  read FTextEntryDelay write FTextEntryDelay;
    property ControlWaitPeriod: Cardinal  read FControlWaitPeriod write FControlWaitPeriod;
    property OnGetContinueExecution: TOnGetContinueExecutionEvent read FOnGetContinueExecution write FOnGetContinueExecution;
  end;

const
  CDefaultGUIActionDelay = 100; // Milliseconds
  CDefaultGUIMouseMoveDelay = 100; // Milliseconds
  CDefaultGUIKeyDownDelay = 50; // Milliseconds
  CDefaultGUITextEntryDelay = 100; // Milliseconds
  CDefaultGUIControlWaitPeriod = 1000; // Milliseconds
  CGUIPositionalClickDelay = 400; // Milliseconds

implementation

uses
{$IFDEF DELPHI2007_UP}
  Dialogs
  ,Types
  ,
{$ENDIF}
  TestUtils
  ;

type
  // Provide a non-blocking call to Application.ProcessMessages.
  TGUISyncMessagesThread = class(TThread)
  private
    procedure ProcessMessages;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    class procedure SyncMessages;
  end;

{$IFDEF DELPHI2009_UP}
  // Ability to run code in a separate thread. Required for GUI testing
  // so that code doesn't block with modal windows.
  TGUITestThread = class(TThread)
  private
    FProc: TThreadProcedure;
    procedure OnTerminateHandler(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AProc: TThreadProcedure; CreateSuspended: Boolean);
  end;
{$ENDIF DELPHI2009_UP}

// assertions are always on so we can check for own consistency
{$ASSERTIONS ON}
// need stack frames to use CallerAddr
{$STACKFRAMES ON}

const
  CControlRetryWaitPeriod = 100;

{ TGUISyncMessagesThread }

class procedure TGUISyncMessagesThread.SyncMessages;
begin
  TGUISyncMessagesThread.Create(False {CreateSuspended});
end;

constructor TGUISyncMessagesThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FreeOnTerminate := true;
end;

procedure TGUISyncMessagesThread.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TGUISyncMessagesThread.Execute;
begin
  Synchronize(ProcessMessages);
end;

{ TGUITestThread }

{$IFDEF DELPHI2009_UP}
constructor TGUITestThread.Create(AProc: TThreadProcedure;
  CreateSuspended: Boolean);
begin
  Assert(Assigned(AProc), 'AProc must be assigned');
  inherited Create(CreateSuspended);
  FProc := AProc;
  OnTerminate := OnTerminateHandler;
end;

procedure TGUITestThread.Execute;
begin
  // Ensure that we can read and set keyboard state of the main thread
  AttachThreadInput(ThreadID, MainThreadID, true);
  try
    FProc;
  finally
    AttachThreadInput(ThreadID, MainThreadID, false);
  end;
end;

procedure TGUITestThread.OnTerminateHandler(Sender: TObject);
begin
  // OnTerminate is called from main thread.
  // Raise any caught exception rather than swallow it.
  if Assigned(FatalException) then
    raise FatalException;
end;
{$ENDIF DELPHI2009_UP}

{ TGUIAutomation }

constructor TGUIAutomation.Create;
begin
  inherited;
  FWakeEvent := TEvent.Create;
  FActionDelay := CDefaultGUIActionDelay;
  FMouseMoveDelay := CDefaultGUIMouseMoveDelay;
  FKeyDownDelay := CDefaultGUIKeyDownDelay;
  FTextEntryDelay := CDefaultGUITextEntryDelay;
  FControlWaitPeriod := CDefaultGUIControlWaitPeriod;
end;

destructor TGUIAutomation.Destroy;
begin
  FWakeEvent.Free;
end;

procedure TGUIAutomation.SyncMessages;
begin
  if GetCurrentThreadId = MainThreadID then
    Application.ProcessMessages
  else
    TGUISyncMessagesThread.SyncMessages;
end;

procedure TGUIAutomation.SleepAndCheckContinue(const AInterval: Cardinal);
var
  LNow: DWORD;
  LWaitUntil: DWORD;
const
  CSleepCheckInterval = 50;
begin
  LNow := GetTickCount;
  LWaitUntil := LNow + AInterval;
  FWakeEvent.ResetEvent;
  while (LNow < LWaitUntil) and ContinueExecution do
  begin
    if LWaitUntil - LNow < CSleepCheckInterval then
      FWakeEvent.WaitFor(LWaitUntil - LNow)
    else
      FWakeEvent.WaitFor(CSleepCheckInterval);
    LNow := GetTickCount;
  end;
end;

procedure TGUIAutomation.SyncSleep(const AInterval: Cardinal);
begin
  SyncMessages;
  SleepAndCheckContinue(AInterval);
end;

procedure TGUIAutomation.WakeUp;
begin
  FWakeEvent.SetEvent;
end;

{$IFDEF DELPHI2009_UP}
procedure TGUIAutomation.ThreadedExecute(AProc: TThreadProcedure);
var
  LThread: TThread;
begin
  LThread := TGUITestThread.Create(AProc, False {CreateSuspended});
  try
    LThread.WaitFor;
  finally
    LThread.Free;
  end;
  SyncMessages;
end;
{$ENDIF DELPHI2009_UP}

function TGUIAutomation.FindControlInstance(const AComp: TComponent;
  const AControlName: string): TControl;
var
  LControlName: string;
  LWinControl: TWinControl;
  i: Integer;
begin
  LControlName := UpperCase(AControlName);
  if (AComp is TControl) and (UpperCase(AComp.Name) = LControlName) then
    Result := AComp as TControl
  else
  begin
    Result := nil;

    if AComp is TWinControl then
    begin
      LWinControl := AComp as TWinControl;
      i := 0;
      while (Result = nil) and (i < LWinControl.ControlCount) do
      begin
        Result := FindControlInstance(LWinControl.Controls[i], AControlName);
        Inc(i);
      end;
    end;
  end;
end;

function TGUIAutomation.FindControlInstance(const AComp: TComponent;
  const AControlName: string; const AInterval: Cardinal): TControl;
var
  LWaitUntil: DWORD;
begin
  LWaitUntil := GetTickCount + AInterval;
  Result := FindControlInstance(AComp, AControlName);
  while (not Assigned(Result)) and ContinueExecution and
        (GetTickCount < LWaitUntil) do
  begin
    SleepAndCheckContinue(CControlRetryWaitPeriod);
    SyncMessages;
    Result := FindControlInstance(AComp, AControlName);
  end;
end;

function TGUIAutomation.FindControl(const AComp: TComponent;
  const AControlName: string; const AAddrs: Pointer): TControl;
var
  LAddrs: Pointer;
begin
  if AAddrs = nil then
    LAddrs := CallerAddr
  else
    LAddrs := AAddrs;

  if Trim(AControlName) = '' then
    raise EGUIAutomation.Create('No control name') at LAddrs;

  Result := FindControlInstance(AComp, AControlName, FControlWaitPeriod);
  if Result = nil then
    raise EGUIAutomationControlNotFound.Create(
        Format('Control named "%s" not found in active form %s',
            [AControlName, Screen.ActiveForm.Name])) at LAddrs;
end;

function TGUIAutomation.GetWinControl(var AControl: TControl; var AX: Integer;
  var AY: Integer): boolean;
var
  LPoint: TPoint;
begin
  if Assigned(AControl) then
  begin
    // We need an actual position to work with
    // If the default position is specified then use the centre of the control
    if (AX = -1) and (AY = -1) then
    begin
      AX := AControl.Width div 2;
      AY := AControl.Height div 2;
    end;

    // If the control does not have a window handle then find the parent
    // control that does and translate the co-ords to the parent control
    if not (AControl is TWinControl) then
    begin
      // Step 1: Get control co-ords in screen co-ords
      LPoint := Point(AX, AY);
      LPoint := AControl.ClientToScreen(LPoint);

      // Target the parent windowed control
      AControl := FindParentWinControl(AControl);

      // Step 2: Now get the parent co-ords from screen co-ords
      if Assigned(AControl) then
      begin
        LPoint := AControl.ScreenToClient(LPoint);
        AX := LPoint.X;
        AY := LPoint.Y;
      end;
    end;

    result := AControl <> nil;
  end
  else
    result := false;
end;

function TGUIAutomation.WaitForWindowEnabled(const AHwnd: HWND): boolean;
var
  LWaitUntil: DWORD;
begin
  if AHwnd <> 0 then
  begin
    if (GetWindowLong(AHwnd, GWL_STYLE) and WS_DISABLED) = WS_DISABLED then
    begin
      LWaitUntil := GetTickCount + FControlWaitPeriod;
      while ContinueExecution and (GetTickCount < LWaitUntil) and
          ((GetWindowLong(AHwnd, GWL_STYLE) and WS_DISABLED) = WS_DISABLED) do
      begin
        SleepAndCheckContinue(CControlRetryWaitPeriod);
        SyncMessages;
      end;
      // Fixed delay to let the dust settle
      if ContinueExecution then
      begin
        SleepAndCheckContinue(CControlRetryWaitPeriod div 2);
        SyncMessages;
      end;
    end;
  end;

  result := ContinueExecution;
end;

procedure TGUIAutomation.MouseButtonDownOn(const AHwnd: HWND;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
  if AButton = mbLeft then
    MouseButtonDownOn(AHwnd, Integer(ButtonState_LeftButton), AX, AY)
  else if AButton = mbRight then
    MouseButtonDownOn(AHwnd, Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  if AButton = mbLeft then
    MouseButtonDownOn(AHwnd, WM_LBUTTONDOWN, AX, AY)
  else if AButton = mbRight then
    MouseButtonDownOn(AHwnd, WM_RBUTTONDOWN, AX, AY);
{$ENDIF}
  SyncSleep(ActionDelay);
end;

procedure TGUIAutomation.MouseButtonDownOn(const AControl: TControl;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
var
  LControl: TControl;
  LX: Integer;
  LY: Integer;
begin
  Assert(AControl <> nil, 'No control');
  LControl := AControl;
  LX := AX;
  LY := AY;
  if GetWinControl(LControl, LX, LY) then
    MouseButtonDownOn(TWinControl(LControl).Handle, AButton, LX, LY);
end;

procedure TGUIAutomation.MouseButtonUpOn(const AHwnd: HWND;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
  if AButton = mbLeft then
    MouseButtonUpOn(AHwnd, Integer(ButtonState_LeftButton), AX, AY)
  else if AButton = mbRight then
    MouseButtonUpOn(AHwnd, Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  if AButton = mbLeft then
    MouseButtonUpOn(AHwnd, WM_LBUTTONUP, AX, AY)
  else if AButton = mbRight then
    MouseButtonUpOn(AHwnd, WM_RBUTTONUP, AX, AY);
{$ENDIF}
  SyncSleep(ActionDelay);
end;

procedure TGUIAutomation.MouseButtonUpOn(const AControl: TControl;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
var
  LControl: TControl;
  LX: Integer;
  LY: Integer;
begin
  Assert(AControl <> nil, 'No control');
  LControl := AControl;
  LX := AX;
  LY := AY;
  if GetWinControl(LControl, LX, LY) then
    MouseButtonUpOn(TWinControl(LControl).Handle, AButton, LX, LY);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickMouseButtonOn(const AControl: TControl;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
var
  LControl: TControl;
  LX: Integer;
  LY: Integer;
begin
  Assert(AControl <> nil, 'No control');
  LControl := AControl;
  LX := AX;
  LY := AY;
  if GetWinControl(LControl, LX, LY) then
    ClickMouseButtonOn(TWinControl(LControl).Handle, AButton, LX, LY);
end;

procedure TGUIAutomation.DoubleClickMouseButtonOn(const AControl: TControl;
  const AButton: TMouseButton; const AX, AY: Integer);
var
  LControl: TControl;
  LX: Integer;
  LY: Integer;
begin
  Assert(AControl <> nil, 'No control');
  LControl := AControl;
  LX := AX;
  LY := AY;
  if GetWinControl(LControl, LX, LY) then
    DoubleClickMouseButtonOn(TWinControl(LControl).Handle, AButton, LX, LY);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickMouseButtonOn(const AHwnd: HWND;
  const AButton: TMouseButton; const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
  if AButton = mbLeft then
    ClickMouseButtonOn(AHwnd, Integer(ButtonState_LeftButton),
        Integer(ButtonState_LeftButton), AX, AY)
  else if AButton = mbRight then
    ClickMouseButtonOn(AHwnd, Integer(ButtonState_RightButton),
        Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  if AButton = mbLeft then
    ClickMouseButtonOn(AHwnd, WM_LBUTTONDOWN, WM_LBUTTONUP, AX, AY)
  else if AButton = mbRight then
    ClickMouseButtonOn(AHwnd, WM_RBUTTONDOWN, WM_RBUTTONUP, AX, AY);
{$ENDIF}
  SyncSleep(ActionDelay);
end;

procedure TGUIAutomation.DoubleClickMouseButtonOn(const AHwnd: HWND;
  const AButton: TMouseButton;const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
//TODO: Work out how to do double click in CLX
//  if AButton = mbLeft then
//    ClickMouseButtonOn(AHwnd, Integer(ButtonState_LeftButton),
//        Integer(ButtonState_LeftButton), AX, AY)
//  else if AButton = mbRight then
//    ClickMouseButtonOn(AHwnd, Integer(ButtonState_RightButton),
//        Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  if AButton = mbLeft then
    ClickMouseButtonOn(AHwnd, WM_LBUTTONDBLCLK, WM_LBUTTONUP, AX, AY)
  else if AButton = mbRight then
    ClickMouseButtonOn(AHwnd, WM_RBUTTONDBLCLK, WM_RBUTTONUP, AX, AY);
{$ENDIF}
  SyncSleep(ActionDelay);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.MoveMouse(const AHwnd: HWND;
  const AX: Integer; const AY: Integer);
var
  LPoint: TPoint;
{$IFDEF DUNIT_CLX}
  evMouse: QMouseEventH;
{$ELSE}
  LSmallPoint: TSmallPoint;
{$ENDIF}
begin
  if not ContinueExecution then
    Exit; //==>

  // Move mouse cursor in screen co-ords. X, Y are specified as client co-ords
  LPoint := Point(AX, AY);
  ClientToScreen(AHwnd, LPoint);
  SetCursorPos(LPoint.X, LPoint.Y);

  // Send move message to notify control
{$IFDEF DUNIT_CLX}
  LPoint := Point(AX, AY);
  evMouse := QMouseEvent_create(QEventType_MouseMove, @LPoint, 1, 0);
  QApplication_sendEvent(AHwnd, evMouse);
{$ELSE}
  LSmallPoint := SmallPoint(AX, AY);
  PostMessage(AHwnd, WM_MOUSEMOVE, 0, Longint(LSmallPoint));
{$ENDIF}
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.MouseButtonDownOn(const AHwnd: HWND;
  const AMouseDownMsg: UINT; const AX: Integer; const AY: Integer);
var
{$IFDEF DUNIT_CLX}
  LPoint: TPoint;
  evMouse: QMouseEventH;
{$ELSE}
  LSmallPoint: TSmallPoint;
{$ENDIF}
begin
  if not ContinueExecution then
    Exit; //==>

  // Move mouse cursor
  MoveMouse(AHwnd, AX, AY);
  SyncSleep(MouseMoveDelay);

{$IFDEF DUNIT_CLX}
  LPoint := Point(AX, AY);
  evMouse := QMouseEvent_create(QEventType_MouseButtonPress, @LPoint,
      AMouseDownMsg, AMouseDownMsg);
  QApplication_sendEvent(AHwnd, evMouse);
{$ELSE}
  LSmallPoint := SmallPoint(AX, AY);
  WaitForWindowEnabled(AHwnd);
  PostMessage(AHwnd, AMouseDownMsg, 0, Longint(LSmallPoint));
{$ENDIF}
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.MouseButtonUpOn(const AHwnd: HWND;
  const AMouseUpMsg: UINT; const AX: Integer; const AY: Integer);
var
{$IFDEF DUNIT_CLX}
  LPoint: TPoint;
  evMouse: QMouseEventH;
{$ELSE}
  LSmallPoint: TSmallPoint;
{$ENDIF}
begin
  if not ContinueExecution then
    Exit; //==>

  // Move mouse cursor
  MoveMouse(AHwnd, AX, AY);
  SyncSleep(MouseMoveDelay);

{$IFDEF DUNIT_CLX}
  LPoint := Point(AX, AY);
  evMouse := QMouseEvent_create(QEventType_MouseButtonRelease, @LPoint,
      AMouseUpMsg, AMouseUpMsg);
  QApplication_sendEvent(AHwnd, evMouse);
{$ELSE}
  LSmallPoint := SmallPoint(AX, AY);
  WaitForWindowEnabled(AHwnd);
  PostMessage(AHwnd, AMouseUpMsg, 0,   Longint(LSmallPoint));
{$ENDIF}
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickMouseButtonOn(const AHwnd: HWND;
  const AMouseDownMsg: UINT; const AMouseUpMsg: UINT;
  const AX: Integer; const AY: Integer);
begin
  if not ContinueExecution then
    Exit; //==>

  MouseButtonDownOn(AHwnd, AMouseDownMsg, AX, AY);
  MouseButtonUpOn(AHwnd, AMouseUpMsg, AX, AY);
end;

function TGUIAutomation.FindControl(const AControlName: string;
  const AAddrs: Pointer): TControl;
begin
  Result := FindControl(Screen.ActiveForm, AControlName, AAddrs);
end;

function TGUIAutomation.FindParentWinControl(const AControl: TControl): TWinControl;
var
  LControl: TControl;
begin
  LControl := AControl;
  while (LControl <> nil) and not (LControl is TWinControl) do
    LControl := LControl.Parent;
  Result := TWinControl(LControl);
end;

{$ifndef DUNIT_CLX}
{ Windows specific keyboard state code }
procedure TGUIAutomation.SetKeyboardStateDown(pShiftState: TShiftState);
var KeyboardState : TKeyboardState;
begin
  GetKeyboardState(KeyboardState);

  if ( ssAlt in pShiftState ) then
  begin
    KeyboardState[VK_MENU] := $80;
    SetKeyboardState(KeyboardState);
  end;

  if ( ssShift in pShiftState ) then
  begin
    KeyboardState[VK_SHIFT] := $80;
    SetKeyboardState(KeyboardState);
  end;

  if ( ssCtrl in pShiftState ) then
  begin
    KeyboardState[VK_CONTROL] := $80;
    SetKeyboardState( KeyboardState );
  end;
end;

{ Windows specific keyboard state code }
procedure TGUIAutomation.SetKeyboardStateUp(pShiftState: TShiftState);
var KeyboardState : TKeyboardState;
begin
  { Get the current keyboard state. }
  GetKeyboardState( KeyboardState );

  if ( ssAlt in pShiftState ) then
  begin
    KeyboardState[VK_MENU] := $00;
    SetKeyboardState(KeyboardState);
  end;

  if ( ssShift in pShiftState ) then
  begin
    { Modify the keyboard state. }
    KeyboardState[VK_SHIFT] := $00;
    SetKeyboardState(KeyboardState);
  end;

  if ( ssCtrl in pShiftState ) then
  begin
    { Modify the keyboard state. }
    KeyboardState[VK_CONTROL] := $00;
    SetKeyboardState(KeyboardState);
  end;
end;
{$endif}

function TGUIAutomation.ControlAtActiveWindowCoord(const AX: Integer;
  const AY: Integer; out AControlHwnd: HWND; out APoint: TPoint): boolean;
var
  LPoint: TPoint;
  LForegroundHwnd: HWND;
begin
{$IFDEF DELPHI2007_UP}
  // NOTE: Themeing in Vista and later doesn't set the foreground window handle
  //   until the window animation effect is complete (at least with task
  //   dialogs) so if we are too fast we might get the control on the wrong
  //   window.
  // TODO: Improve this: Know *if* we need to wait for a new window to appear
  //   and *wait* for it to appear (instead of always waiting and instead of
  //   having a fixed wait time). e.g. Add a WaitForNewForegroundWindow command
  //   (and add this to the GUIActionRecorder) or use a different mechanism to
  //   identify the target window (such as window caption).
  if Dialogs.UseLatestCommonDialogs then
  begin
    SleepAndCheckContinue(CGUIPositionalClickDelay);
    SyncMessages;
  end;
{$ENDIF}

  // Get screen position of the co-ord relative to the active window
  LPoint := Point(AX, AY);
  LForegroundHwnd := GetForegroundWindow;
  result := LForegroundHwnd <> 0;
  if result then
  begin
    ClientToScreen(LForegroundHwnd, LPoint);
    // Now get the window handle of the control at that position
    AControlHwnd := WindowFromPoint(LPoint);
    result := AControlHwnd <> 0;
    // Get the co-ords relative to the control
    if result then
    begin
      ScreenToClient(AControlHwnd, LPoint);
      APoint := LPoint;
    end;
  end;
end;

function TGUIAutomation.WaitForControlExists(const AControlName: string;
  const AExists: Boolean; const AInterval: Cardinal): boolean;
var
  LWaitUntil: DWORD;
  LControl: TControl;

  function _CriteriaMet(const AControl: TControl): Boolean;
  begin
    Result :=
        (AExists and Assigned(LControl)) or
        ((not AExists) and (not Assigned(LControl)));
  end;

begin
  if Trim(AControlName) = '' then
    raise EGUIAutomation.Create('No control name') at CallerAddr;

  LWaitUntil := GetTickCount + AInterval;
  LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  while (not _CriteriaMet(LControl)) and
        ContinueExecution and (GetTickCount < LWaitUntil) do
  begin
    SleepAndCheckContinue(CControlRetryWaitPeriod);
    SyncMessages;
    LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  end;

  Result := _CriteriaMet(LControl);
end;

function TGUIAutomation.WaitForControlVisible(const AControlName: string;
  const AVisible: Boolean; const AInterval: Cardinal): boolean;
var
  LWaitUntil: DWORD;
  LControl: TControl;

  function _CriteriaMet(const AControl: TControl): Boolean;
  begin
    Result :=
        (AVisible and Assigned(AControl) and AControl.Visible) or
        ((not AVisible) and ((not Assigned(AControl)) or (not AControl.Visible)));
  end;

begin
  if Trim(AControlName) = '' then
    raise EGUIAutomation.Create('No control name') at CallerAddr;

  LWaitUntil := GetTickCount + AInterval;
  LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  while (not _CriteriaMet(LControl)) and
        ContinueExecution and (GetTickCount < LWaitUntil) do
  begin
    SleepAndCheckContinue(CControlRetryWaitPeriod);
    SyncMessages;
    LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  end;

  Result := _CriteriaMet(LControl);
end;

function TGUIAutomation.WaitForControlEnabled(const AControlName: string;
  const AEnabled: Boolean; const AInterval: Cardinal): boolean;
var
  LWaitUntil: DWORD;
  LControl: TControl;

  function _CriteriaMet(const AControl: TControl): Boolean;
  begin
    Result :=
        (AEnabled and Assigned(AControl) and AControl.Visible and AControl.Enabled) or
        ((not AEnabled) and ((not Assigned(AControl)) or (not AControl.Visible) or
          (not AControl.Enabled)));
  end;

begin
  if Trim(AControlName) = '' then
    raise EGUIAutomation.Create('No control name') at CallerAddr;

  LWaitUntil := GetTickCount + AInterval;
  LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  while (not _CriteriaMet(LControl)) and
        ContinueExecution and (GetTickCount < LWaitUntil) do
  begin
    SleepAndCheckContinue(CControlRetryWaitPeriod);
    SyncMessages;
    LControl := FindControlInstance(Screen.ActiveForm, AControlName);
  end;

  Result := _CriteriaMet(LControl);
end;

procedure TGUIAutomation.MoveMouseTo(const AHwnd: HWND; const AX: Integer;
  const AY: Integer; const APixelInterval: Integer; const APixelsPerSecond: Integer);
var
  LInitialPoint: TPoint;
  LFinalPoint: TPoint;
  LDeltaX: double;
  LDeltaY: double;
  LX: Integer;
  LY: Integer;
  LIntervals: double;
  LMoveIntervals: Cardinal;
  LWaitPerInterval: Cardinal;
  i: Cardinal;
begin
  if APixelInterval > 0 then
  begin
    // Move the cursor along the path to the specified position at the
    // specified interval

    if GetCursorPos(LInitialPoint) then
    begin
      // Start from the current cursor position relative to the windowed control
      ScreenToClient(AHwnd, LInitialPoint);

      // Move in steps to the specified position
      LFinalPoint := Point(AX, AY);
      LIntervals := LFinalPoint.Distance(LInitialPoint) / APixelInterval;
      LMoveIntervals := Trunc(LIntervals);
      if LMoveIntervals > 0 then
      begin
        if APixelsPerSecond > 0 then
          LWaitPerInterval := Trunc((APixelInterval / APixelsPerSecond) * 1000)
        else
          LWaitPerInterval := 0;
        LDeltaX := (AX - LInitialPoint.X) / LIntervals;
        LDeltaY := (AY - LInitialPoint.Y) / LIntervals;
        i := 1;
        while (i <= LMoveIntervals) and ContinueExecution do
        begin
          LX := Trunc(LInitialPoint.X + (LDeltaX * i));
          LY := Trunc(LInitialPoint.Y + (LDeltaY * i));
          MoveMouse(AHwnd, LX, LY);
          if (LWaitPerInterval > 0) and (i < LMoveIntervals) then
            SyncSleep(LWaitPerInterval);
          Inc(i);
        end;
      end;
    end;
  end;

  // Move to the final position
  MoveMouse(AHwnd, AX, AY);

  SyncSleep(ActionDelay);
end;

procedure TGUIAutomation.MoveMouseTo(const AX: Integer; const AY: Integer;
  const APixelInterval: Integer; const APixelsPerSecond: Integer);
begin
  MoveMouseTo(Screen.ActiveControl, AX, AY, APixelInterval, APixelsPerSecond);
end;

procedure TGUIAutomation.MoveMouseTo(const AControlName: string; const AX: Integer;
  const AY: Integer; const APixelInterval: Integer; const APixelsPerSecond: Integer);
begin
  MoveMouseTo(FindControl(AControlName, CallerAddr), AX, AY, APixelInterval, APixelsPerSecond);
end;

procedure TGUIAutomation.MoveMouseTo(const AControl: TControl; const AX: Integer;
  const AY: Integer; const APixelInterval: Integer; const APixelsPerSecond: Integer);
var
  LControl: TControl;
  LX: Integer;
  LY: Integer;
begin
  Assert(AControl <> nil, 'No control');
  LControl := AControl;
  LX := AX;
  LY := AY;
  if GetWinControl(LControl, LX, LY) then
    MoveMouseTo(TWinControl(LControl).Handle, LX, LY, APixelInterval, APixelsPerSecond);
end;

// X, Y are in co-ords of the active window
procedure TGUIAutomation.LeftClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    ClickMouseButtonOn(LHwnd, mbLeft, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftDoubleClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    DoubleClickMouseButtonOn(LHwnd, mbLeft, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftMouseDown(const AX, AY: Integer);
begin
  LeftMouseDown(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftMouseDown(const AControlName: string; const AX,
  AY: Integer);
begin
  LeftMouseDown(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftMouseDown(const AControl: TControl; const AX,
  AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  MouseButtonDownOn(AControl, mbLeft, AX, AY);
end;

procedure TGUIAutomation.LeftMouseDownAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    MouseButtonDownOn(LHwnd, mbLeft, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftMouseUp(const AControl: TControl; const AX, AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  MouseButtonUpOn(AControl, mbLeft, AX, AY);
end;

procedure TGUIAutomation.LeftMouseUp(const AControlName: string; const AX,
  AY: Integer);
begin
  LeftMouseUp(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftMouseUp(const AX, AY: Integer);
begin
  LeftMouseUp(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftMouseUpAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    MouseButtonUpOn(LHwnd, mbLeft, LPoint.X, LPoint.Y);
end;

// X, Y are in co-ords of the active window
procedure TGUIAutomation.RightClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    ClickMouseButtonOn(LHwnd, mbRight, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.RightDoubleClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    DoubleClickMouseButtonOn(LHwnd, mbRight, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.RightMouseDown(const AX, AY: Integer);
begin
  RightMouseDown(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightMouseDown(const AControlName: string; const AX,
  AY: Integer);
begin
  RightMouseDown(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightMouseDown(const AControl: TControl; const AX,
  AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  MouseButtonDownOn(AControl, mbRight, AX, AY);
end;

procedure TGUIAutomation.RightMouseDownAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    MouseButtonDownOn(LHwnd, mbRight, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.RightMouseUp(const AControlName: string; const AX,
  AY: Integer);
begin
  RightMouseUp(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightMouseUp(const AControl: TControl; const AX, AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  MouseButtonUpOn(AControl, mbRight, AX, AY);
end;

procedure TGUIAutomation.RightMouseUp(const AX, AY: Integer);
begin
  RightMouseUp(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightMouseUpAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    MouseButtonUpOn(LHwnd, mbRight, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftClick(const AX: Integer; const AY: Integer);
begin
  LeftClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftClick(const AControlName: string; const AX: Integer;
  const AY: Integer);
begin
  LeftClick(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftClick(const AControl: TControl; const AX: Integer;
  const AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  ClickMouseButtonOn(AControl, mbLeft, AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(const AX, AY: Integer);
begin
  LeftDoubleClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(const AControlName: string; const AX,
  AY: Integer);
begin
  LeftDoubleClick(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(const AControl: TControl; const AX,
  AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  DoubleClickMouseButtonOn(AControl, mbLeft, AX, AY);
end;

procedure TGUIAutomation.RightClick(const AX: Integer; const AY: Integer);
begin
  RightClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(const AX, AY: Integer);
begin
  RightDoubleClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightClick(const AControlName: string; const AX: Integer;
  const AY: Integer);
begin
  RightClick(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(const AControlName: string; const AX,
  AY: Integer);
begin
  RightDoubleClick(FindControl(AControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightClick(const AControl: TControl; const AX: Integer;
  const AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  ClickMouseButtonOn(AControl, mbRight, AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(const AControl: TControl; const AX,
  AY: Integer);
begin
  Assert(AControl <> nil, 'No control');
  DoubleClickMouseButtonOn(AControl, mbRight, AX, AY);
end;

procedure TGUIAutomation.EnterKey(AKey: Word; const AShiftState: TShiftState;
  const ACount: Integer);
begin
  EnterKeyInto(GetFocus, AKey, AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlHwnd: HWND; AKey: Word;
  const AShiftState: TShiftState; const ACount: Integer);
var
  i: Integer;
{$IFDEF DUNIT_CLX}
  E: QKeyEventH;
  Ch: char;
  S: WideString;
  state: integer;
  function KeyChar(AKey: word; Shift: boolean): char;
  begin
    Result := Char(AKey);
    if Shift then
      Result := UpCase(Result)
    else
      Result := LowerCase(Result)[1];
  end;
{$ENDIF}
begin
  if not ContinueExecution then
    Exit; //==>

  Assert(AControlHwnd <> 0, 'No window handle');
  if AControlHwnd <> 0 then
  begin
    WaitForWindowEnabled(AControlHwnd);
{$IFDEF DUNIT_CLX}
    if AKey <= 255 then
    begin
      Ch := KeyChar(AKey, ssShift in AShiftState);
      S := Ch;
    end
    else
    begin
      Ch := #0;
      S := '';
    end;
    State := 0;
    if ssAlt in AShiftState then
      State := integer(ButtonState_AltButton);
    if ssCtrl in AShiftState then
      State := State or Integer(ButtonState_ControlButton);
    if ssShift in AShiftState then
      State := State or Integer(ButtonState_ShiftButton);
{$ENDIF}

    // Repeat the key presses
    for i := 1 to ACount do
    begin
      if i > 1 then
        SyncSleep(TextEntryDelay);

      // Key down
{$IFDEF DUNIT_CLX}
      E := QKeyEvent_create(QEventType_KeyPress, AKey, Ord(Ch), State, @S, false, 1);
      try
        QApplication_sendEvent(AControlHwnd, E);
      finally
        QKeyEvent_destroy(E);
      end;
{$ELSE}
      SetKeyboardStateDown(AShiftState);
      if ssAlt in AShiftState then
        PostMessage(AControlHwnd, WM_SYSKEYDOWN, AKey, integer($20000000))
      else
        PostMessage(AControlHwnd, WM_KEYDOWN, AKey, 0);
{$ENDIF}

      SyncSleep(KeyDownDelay);

      // Key up
{$IFDEF DUNIT_CLX}
      E := QKeyEvent_create(QEventType_KeyRelease, AKey, Ord(Ch), State, @S, false, 1);
      try
        QApplication_sendEvent(AControlHwnd, E);
      finally
        QKeyEvent_destroy(E);
      end;
{$ELSE}
      if ssAlt in AShiftState then
        PostMessage(AControlHwnd, WM_SYSKEYUP, AKey, integer($E0000000))
      else
        PostMessage(AControlHwnd, WM_KEYUP, AKey, integer($C0000000));
      SetKeyboardStateUp(AShiftState);
{$ENDIF}
    end;

    SyncSleep(ActionDelay);
  end;
end;

procedure TGUIAutomation.EnterKeyInto(const AControl: TControl; AKey: Word;
  const AShiftState: TShiftState; const ACount: Integer);
var
  LWinControl: TWinControl;
begin
  Assert(AControl <> nil, 'No control');
  LWinControl := FindParentWinControl(AControl);
  if LWinControl <> nil then
    EnterKeyInto(LWinControl.Handle, AKey, AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlName: string; AKey: Word;
  const AShiftState: TShiftState; const ACount: Integer);
begin
  EnterKeyInto(FindControl(AControlName, CallerAddr), AKey, AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKey(AKey: Char; const AShiftState: TShiftState;
  const ACount: Integer);
begin
  EnterKey(Ord(AKey), AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlHwnd: HWND; AKey: Char;
  const AShiftState: TShiftState; const ACount: Integer);
begin
  EnterKeyInto(AControlHwnd, Ord(AKey), AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKeyInto(const AControl: TControl; AKey: Char;
  const AShiftState: TShiftState; const ACount: Integer);
begin
  EnterKeyInto(AControl, Ord(AKey), AShiftState, ACount);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlName: string; AKey: Char;
  const AShiftState: TShiftState; const ACount: Integer);
begin
  EnterKeyInto(AControlName, Ord(AKey), AShiftState, ACount);
end;

procedure TGUIAutomation.EnterText(AText: string);
begin
  EnterTextInto(GetFocus, AText);
end;

procedure TGUIAutomation.EnterTextInto(const AControlName: string; AText: string);
begin
  EnterTextInto(FindControl(AControlName, CallerAddr), AText);
end;

procedure TGUIAutomation.EnterTextInto(const AControlHwnd: HWND; AText: string);
var
  i :Integer;
{$IFDEF DUNIT_CLX}
  E: QKeyEventH;
  S: WideString;
{$ENDIF}
begin
  if not ContinueExecution then
    Exit; //==>

  Assert(AControlHwnd <> 0, 'No window handle');
  if AControlHwnd <> 0 then
  begin
    WaitForWindowEnabled(AControlHwnd);

    for i := 1 to Length(AText) do
    begin
      if not ContinueExecution then
        Break;
{$IFDEF DUNIT_CLX}
      S := AText[i];
      E := QKeyEvent_create(QEventType_KeyPress, Ord(AText[i]), Ord(AText[i]), 0, @S, false, 1);
      QApplication_sendEvent(AControlHwnd, E);
      QKeyEvent_destroy(E);
{$ELSE}
      PostMessage(AControlHwnd, WM_CHAR, Ord(AText[i]), 0);
{$ENDIF}
      SyncSleep(TextEntryDelay);
    end;
  end;
end;

procedure TGUIAutomation.EnterTextInto(const AControl: TControl; AText: string);
var
  LWinControl: TWinControl;
begin
  Assert(AControl <> nil, 'No control');
  LWinControl := FindParentWinControl(AControl);
  if LWinControl <> nil then
    EnterTextInto(LWinControl.Handle, AText);
end;

procedure TGUIAutomation.Show(const AControlName: string; AOnOff: boolean);
begin
  Show(FindControl(AControlName, CallerAddr), AOnOff);
end;

procedure TGUIAutomation.Show(const AControl: TControl; AOnOff: boolean);
begin
  Assert(AControl <> nil);
  AControl.Visible := AOnOff;
  Assert(AControl.Visible = AOnOff);
  SyncSleep(ActionDelay);
end;

function TGUIAutomation.ContinueExecution: boolean;
begin
  result := true;
  if Assigned(FOnGetContinueExecution) then
    FOnGetContinueExecution(result);
end;

procedure TGUIAutomation.Hide(const AControlName: string);
begin
  Show(AControlName, false);
end;

procedure TGUIAutomation.Hide(const AControl: TControl);
begin
  Show(AControl, false);
end;

procedure TGUIAutomation.Tab(n: Integer);
var
  i :Integer;
  s :TShiftState;
begin
  s := [];
  if n < 0 then
  begin
    s := [ssShift];
    n := -n;
  end;

  for i := 1 to n do
  begin
    if not ContinueExecution then
      Break;
    EnterKey(VK_TAB, s);
  end;

  SyncSleep(ActionDelay);
end;

procedure TGUIAutomation.SetFocus(const AControl: TControl;
  const AAddrs: Pointer);
var
  LAddrs: Pointer;
begin
  if AAddrs = nil then
    LAddrs := CallerAddr
  else
    LAddrs := AAddrs;

  if not (AControl is TWinControl) then
    raise EGUIAutomation.Create(
        Format('Expected a TWinControl, but %s is a %s',
               [AControl.Name, AControl.ClassName])) at LAddrs;
  if not TWinControl(AControl).CanFocus then
    raise EGUIAutomation.Create(
        Format('Control %s:%s cannot focus',
               [AControl.Name, AControl.ClassName])) at LAddrs;
  TWinControl(AControl).SetFocus;
end;

procedure TGUIAutomation.SetFocus(const AControlName: string);
begin
  SetFocus(FindControl(AControlName, CallerAddr), CallerAddr);
end;

end.

