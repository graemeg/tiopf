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
  TestFramework,
  TestFrameworkIfaces,

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
  Classes;

const
  rcs_id: string = '#(@)$Id: GUIAutomation.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $';

{$IFDEF DUNIT_CLX}
const
  VK_TAB = KEY_TAB;
{$ENDIF}

type
  EGUIAutomation = class(Exception)
  end;

  TOnGetContinueExecutionEvent = procedure(var AContinueExecution: boolean) of object;

  TGUIAutomation = class
  private
    FActionDelay: Integer;
    FMouseMoveDelay: Integer;
    FKeyDownDelay: Integer;
    FTextEntryDelay: Integer;
    FOnGetContinueExecution: TOnGetContinueExecutionEvent;

    function ContinueExecution: boolean;
    function GetWinControl(var AControl: TControl; var AX: Integer;
        var AY: Integer): boolean;
    procedure ClickLeftMouseButtonOn(Control: TControl;
        const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure DoubleClickLeftMouseButtonOn(Control: TControl;
        const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure ClickRightMouseButtonOn(Control: TControl;
        const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure DoubleClickRightMouseButtonOn(Control: TControl;
        const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure ClickLeftMouseButtonOn(const AHwnd: HWND;
        const AX: Integer; const AY: Integer); overload;
    procedure DoubleClickLeftMouseButtonOn(const AHwnd: HWND;
        const AX: Integer; const AY: Integer); overload;
    procedure ClickRightMouseButtonOn(const AHwnd: HWND;
        const AX: Integer; const AY: Integer); overload;
    procedure DoubleClickRightMouseButtonOn(const AHwnd: HWND;
        const AX: Integer; const AY: Integer); overload;
    procedure ClickMouseButtonOn(const AHwnd: HWND;
        const AMouseDownMsg: UINT; const AMouseUpMsg: UINT;
        const AX: Integer; const AY: Integer);

  protected
    procedure SyncMessages;
    procedure SyncSleep(const AInterval: Integer);
    function WaitForWindowEnabled(const AHwnd: HWND): boolean;
    function ControlAtActiveWindowCoord(const AX: Integer; const AY: Integer;
        out AControlHwnd: HWND; out APoint: TPoint): boolean;
    function  FindParentWinControl(Control :TControl):TWinControl;

{$IFNDEF DUNIT_CLX}
    { Windows specific keyboard state code }
    procedure SetKeyboardStateDown(pShiftState: TShiftState);
    procedure SetKeyboardStateUp(pShiftState: TShiftState);
{$ENDIF}

  public
    constructor Create;
    function  FindControl(const AName: string; Addrs :Pointer = nil): TControl;                      overload;
{$IFDEF DELPHI2009_UP}
    procedure ThreadedExecute(AProc: TThreadProcedure);
{$ENDIF DELPHI2009_UP}
    function  FindControl(Comp: TComponent; const CtlName: string; Addrs :Pointer = nil): TControl; overload;

    // Control (VCL or other) under mouse cursor, position is relative to GUI
    procedure LeftClickAt(const AX: Integer; const AY: Integer);
    procedure LeftDoubleClickAt(const AX: Integer; const AY: Integer);
    procedure RightClickAt(const AX: Integer; const AY: Integer);
    procedure RightDoubleClickAt(const AX: Integer; const AY: Integer);

    // Active control, default position is centre of control
    procedure LeftClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftDoubleClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    // Given control, default position is centre of control
    procedure LeftClick(ControlName :string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftDoubleClick(ControlName :string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure LeftClick(Control :TControl; const AX: Integer = -1; const AY: Integer = -1);   overload;
    procedure LeftDoubleClick(Control :TControl; const AX: Integer = -1; const AY: Integer = -1); overload;
    // Active control, default position is centre of control
    procedure RightClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightDoubleClick(const AX: Integer = -1; const AY: Integer = -1); overload;
    // Given control, default position is centre of control
    procedure RightClick(ControlName :string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightDoubleClick(ControlName :string; const AX: Integer = -1; const AY: Integer = -1); overload;
    procedure RightClick(Control :TControl; const AX: Integer = -1; const AY: Integer = -1);   overload;
    procedure RightDoubleClick(Control :TControl; const AX: Integer = -1; const AY: Integer = -1); overload;

    procedure EnterKey(Key :Word; const ShiftState :TShiftState = []); overload;
    procedure EnterKeyInto(const AControlHwnd: HWND;  Key: Word; const ShiftState: TShiftState = []); overload;
    procedure EnterKeyInto(Control :TControl;   Key :Word; const ShiftState :TShiftState = []); overload;
    procedure EnterKeyInto(ControlName :string; Key :Word; const ShiftState :TShiftState = []); overload;

    procedure EnterKey(Key :Char; const ShiftState :TShiftState = []); overload;
    procedure EnterKeyInto(const AControlHwnd: HWND;  Key: Char; const ShiftState: TShiftState = []); overload;
    procedure EnterKeyInto(Control :TControl;   Key :Char; const ShiftState :TShiftState = []); overload;
    procedure EnterKeyInto(ControlName :string; Key :Char; const ShiftState :TShiftState = []); overload;

    procedure EnterText(Text :string);
    procedure EnterTextInto(const AControlHwnd: HWND; Text: string); overload;
    procedure EnterTextInto(Control :TControl;   Text :string); overload;
    procedure EnterTextInto(ControlName :string; Text :string); overload;

    procedure Show(Control :TControl; OnOff :boolean = true);   overload;
    procedure Show(ControlName :string; OnOff :boolean = true); overload;

    procedure Hide(Control :TControl);   overload;
    procedure Hide(ControlName :string); overload;

    procedure Tab(n :Integer =1);

    procedure SetFocus(Control :TControl; Addrs :Pointer = nil); overload;
    procedure SetFocus(ControlName :string);                    overload;

    property ActionDelay: Integer read FActionDelay write FActionDelay;
    property MouseMoveDelay: Integer read FMouseMoveDelay write FMouseMoveDelay;
    property KeyDownDelay: Integer read FKeyDownDelay write FKeyDownDelay;
    property TextEntryDelay: Integer  read FTextEntryDelay write FTextEntryDelay;
    property OnGetContinueExecution: TOnGetContinueExecutionEvent read FOnGetContinueExecution write FOnGetContinueExecution;
  end;

const
  CDefaultGUIActionDelay = 100; // Milliseconds
  CDefaultGUIMouseMoveDelay = 100; // Milliseconds
  CDefaultGUIKeyDownDelay = 50; // Milliseconds
  CDefaultGUITextEntryDelay = 100; // Milliseconds
  CGUIPositionalClickDelay = 400; // Milliseconds

implementation

{$IF COMPILERVERSION >= 11} // D2007
uses
  Dialogs
  ,Types
  ;
{$IFEND}

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
  FActionDelay := CDefaultGUIActionDelay;
  FMouseMoveDelay := CDefaultGUIMouseMoveDelay;
  FKeyDownDelay := CDefaultGUIKeyDownDelay;
  FTextEntryDelay := CDefaultGUITextEntryDelay;
end;

procedure TGUIAutomation.SyncMessages;
begin
  if GetCurrentThreadId = MainThreadID then
    Application.ProcessMessages
  else
    TGUISyncMessagesThread.SyncMessages;
end;

procedure TGUIAutomation.SyncSleep(const AInterval: Integer);
begin
  SyncMessages;
  Sleep(AInterval);
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

function TGUIAutomation.FindControl(Comp: TComponent; const CtlName: string; Addrs :Pointer): TControl;

  function DoFind(C :TComponent; const CName :string) :TControl;
  var
    i: Integer;
  begin
    Result := nil;
    i := 0;
    while (Result = nil) and (i < C.ComponentCount) do
    begin
      with C do
      begin
        if (Components[i] is TControl)
        and (UpperCase(Components[i].Name) = CName) then
          Result := Components[i] as TControl
        else
          Result := DoFind(Components[i], CName);
      end;
      Inc(i);
    end;
  end;
begin
  if Addrs = nil then
    Addrs := CallerAddr;


  if Trim(CtlName) = '' then
    raise EGUIAutomation.Create('No control name') at Addrs;

  Result := DoFind(Comp, UpperCase(CtlName));

  if Result = nil then
    raise EGUIAutomation.Create(Format('Control named "%s" not found in active form %s',
        [CtlName, Screen.ActiveForm.Name]));
end;

function TGUIAutomation.GetWinControl(var AControl: TControl; var AX: Integer;
  var AY: Integer): boolean;
var
  LPoint: TPoint;
begin
  if not (AControl is TWinControl) then
  begin
    // Translate actual control co-ords to parent window control co-ords
    if (AX <> -1) and (AY <> -1) then
    begin
      LPoint.X := AX;
      LPoint.Y := AY;
      LPoint := AControl.ClientToScreen(LPoint);
    end;
    AControl := FindParentWinControl(AControl);
    if Assigned(AControl) and (AX <> -1) and (AY <> -1) then
    begin
      LPoint := AControl.ScreenToClient(LPoint);
      AX := LPoint.X;
      AY := LPoint.Y;
    end;
  end;

  result := AControl <> nil;
  if result then
  begin
    if AX = -1 then
      AX := AControl.Width  div 2;
    if AY = -1 then
      AY := AControl.Height  div 2;
  end;
end;

function TGUIAutomation.WaitForWindowEnabled(const AHwnd: HWND): boolean;
begin
  if AHwnd <> 0 then
  begin
    while ContinueExecution and
        ((GetWindowLong(AHwnd, GWL_STYLE) and WS_DISABLED) = WS_DISABLED) do
    begin
      Sleep(100);
      SyncMessages;
    end;
    if ContinueExecution then
    begin
      Sleep(50);
      SyncMessages;
    end;
  end;

  result := ContinueExecution;
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickLeftMouseButtonOn(Control: TControl;
  const AX: Integer; const AY: Integer);
var
  LX: Integer;
  LY: Integer;
begin
  Assert(Control <> nil, 'No control');
  LX := AX;
  LY := AY;
  if GetWinControl(Control, LX, LY) then
    ClickLeftMouseButtonOn(TWinControl(Control).Handle, LX, LY);
end;

procedure TGUIAutomation.DoubleClickLeftMouseButtonOn(Control: TControl; const AX,
  AY: Integer);
var
  LX: Integer;
  LY: Integer;
begin
  Assert(Control <> nil, 'No control');
  LX := AX;
  LY := AY;
  if GetWinControl(Control, LX, LY) then
    DoubleClickLeftMouseButtonOn(TWinControl(Control).Handle, LX, LY);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickRightMouseButtonOn(Control: TControl;
  const AX: Integer; const AY: Integer);
var
  LX: Integer;
  LY: Integer;
begin
  Assert(Control <> nil, 'No control');
  LX := AX;
  LY := AY;
  if GetWinControl(Control, LX, LY) then
    ClickRightMouseButtonOn(TWinControl(Control).Handle, LX, LY);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickLeftMouseButtonOn(const AHwnd: HWND;
  const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
  ClickMouseButtonOn(AHwnd, Integer(ButtonState_LeftButton),
      Integer(ButtonState_LeftButton), AX, AY);
{$ELSE}
  ClickMouseButtonOn(AHwnd, WM_LBUTTONDOWN, WM_LBUTTONUP, AX, AY);
{$ENDIF}
end;

procedure TGUIAutomation.DoubleClickLeftMouseButtonOn(const AHwnd: HWND;
  const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
//TODO: Work out how to do double click in CLX
//  ClickMouseButtonOn(AHwnd, Integer(ButtonState_LeftButton),
//      Integer(ButtonState_LeftButton), AX, AY);
{$ELSE}
  ClickMouseButtonOn(AHwnd, WM_LBUTTONDBLCLK, WM_LBUTTONUP, AX, AY);
{$ENDIF}
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickRightMouseButtonOn(const AHwnd: HWND;
  const AX: Integer; const AY: Integer);
begin
{$IFDEF DUNIT_CLX}
  ClickMouseButtonOn(AHwnd, Integer(ButtonState_RightButton),
      Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  ClickMouseButtonOn(AHwnd, WM_RBUTTONDOWN, WM_RBUTTONUP, AX, AY);
{$ENDIF}
end;

procedure TGUIAutomation.DoubleClickRightMouseButtonOn(const AHwnd: HWND;
  const AX, AY: Integer);
begin
{$IFDEF DUNIT_CLX}
//TODO: Work out how to do double click in CLX
//  ClickMouseButtonOn(AHwnd, Integer(ButtonState_RightButton),
//      Integer(ButtonState_RightButton), AX, AY);
{$ELSE}
  ClickMouseButtonOn(AHwnd, WM_RBUTTONDBLCLK, WM_RBUTTONUP, AX, AY);
{$ENDIF}
end;

procedure TGUIAutomation.DoubleClickRightMouseButtonOn(Control: TControl;
  const AX, AY: Integer);
var
  LX: Integer;
  LY: Integer;
begin
  Assert(Control <> nil, 'No control');
  LX := AX;
  LY := AY;
  if GetWinControl(Control, LX, LY) then
    DoubleClickRightMouseButtonOn(TWinControl(Control).Handle, LX, LY);
end;

// X, Y are in client co-ordinates
procedure TGUIAutomation.ClickMouseButtonOn(const AHwnd: HWND;
  const AMouseDownMsg: UINT; const AMouseUpMsg: UINT;
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

  // Move mouse cursor to click position. X, Y are specified as client co-ords
  LPoint := Point(AX, AY);
  ClientToScreen(AHwnd, LPoint);
  SetCursorPos(LPoint.X, LPoint.Y);

{$IFDEF DUNIT_CLX}
  LPoint := Point(AX, AY);
  evMouse := QMouseEvent_create(QEventType_MouseMove, @LPoint, 1, 0);
  QApplication_sendEvent(AHwnd, evMouse);
  SyncSleep(MouseMoveDelay);
  evMouse := QMouseEvent_create(QEventType_MouseButtonPress, @LPoint,
      AMouseDownMsg, AMouseDownMsg);
  QApplication_sendEvent(AHwnd, evMouse);
  evMouse := QMouseEvent_create(QEventType_MouseButtonRelease, @LPoint,
      AMouseDownMsg, AMouseDownMsg);
  QApplication_sendEvent(AHwnd, evMouse);
{$ELSE}
  LSmallPoint := SmallPoint(AX, AY);
  PostMessage(AHwnd, WM_MOUSEMOVE, 0, Longint(LSmallPoint));
  SyncSleep(MouseMoveDelay);
  WaitForWindowEnabled(AHwnd);
  PostMessage(AHwnd, AMouseDownMsg, 0, Longint(LSmallPoint));
  PostMessage(AHwnd, AMouseUpMsg, 0,   Longint(LSmallPoint));
{$ENDIF}
  SyncSleep(ActionDelay);
end;

function TGUIAutomation.FindControl(const AName: string; Addrs :Pointer): TControl;
begin
  Result := FindControl(Screen.ActiveForm, AName, Addrs);
end;

function TGUIAutomation.FindParentWinControl(Control: TControl): TWinControl;
begin
  while (Control <> nil) and not (Control is TWinControl) do
    Control := Control.Parent;
  Result := TWinControl(Control);
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
begin
{$IF COMPILERVERSION >= 11} // D2007 or later
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
    Sleep(CGUIPositionalClickDelay);
    SyncMessages;
  end;
{$IFEND}

  // Get screen position of the co-ord relative to the active window
  LPoint := Point(AX, AY);
  ClientToScreen(GetForegroundWindow, LPoint);
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

// X, Y are in co-ords of the active window
procedure TGUIAutomation.LeftClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    ClickLeftMouseButtonOn(LHwnd, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftDoubleClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    DoubleClickLeftMouseButtonOn(LHwnd, LPoint.X, LPoint.Y);
end;

// X, Y are in co-ords of the active window
procedure TGUIAutomation.RightClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    ClickRightMouseButtonOn(LHwnd, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.RightDoubleClickAt(const AX, AY: Integer);
var
  LPoint: TPoint;
  LHwnd: HWND;
begin
  if ControlAtActiveWindowCoord(AX, AY, LHwnd, LPoint) then
    DoubleClickRightMouseButtonOn(LHwnd, LPoint.X, LPoint.Y);
end;

procedure TGUIAutomation.LeftClick(const AX: Integer; const AY: Integer);
begin
  LeftClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftClick(ControlName: string; const AX: Integer;
  const AY: Integer);
begin
  LeftClick(FindControl(ControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftClick(Control: TControl; const AX: Integer;
  const AY: Integer);
begin
  Assert(Control <> nil, 'No control');
  ClickLeftMouseButtonOn(Control, AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(const AX, AY: Integer);
begin
  LeftDoubleClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(ControlName: string; const AX,
  AY: Integer);
begin
  LeftDoubleClick(FindControl(ControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.LeftDoubleClick(Control: TControl; const AX,
  AY: Integer);
begin
  Assert(Control <> nil, 'No control');
  DoubleClickLeftMouseButtonOn(Control, AX, AY);
end;

procedure TGUIAutomation.RightClick(const AX: Integer; const AY: Integer);
begin
  RightClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(const AX, AY: Integer);
begin
  RightDoubleClick(Screen.ActiveControl, AX, AY);
end;

procedure TGUIAutomation.RightClick(ControlName: string; const AX: Integer;
  const AY: Integer);
begin
  RightClick(FindControl(ControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(ControlName: string; const AX,
  AY: Integer);
begin
  RightDoubleClick(FindControl(ControlName, CallerAddr), AX, AY);
end;

procedure TGUIAutomation.RightClick(Control: TControl; const AX: Integer;
  const AY: Integer);
begin
  Assert(Control <> nil, 'No control');
  ClickRightMouseButtonOn(Control, AX, AY);
end;

procedure TGUIAutomation.RightDoubleClick(Control: TControl; const AX,
  AY: Integer);
begin
  Assert(Control <> nil, 'No control');
  DoubleClickRightMouseButtonOn(Control, AX, AY);
end;

procedure TGUIAutomation.EnterKey(Key: Word; const ShiftState: TShiftState);
begin
  EnterKeyInto(GetFocus, Key, ShiftState);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlHwnd: HWND; Key: Word;
  const ShiftState: TShiftState);
{$IFDEF DUNIT_CLX}
var
  E: QKeyEventH;
  Ch: char;
  S: WideString;
  state: integer;
  function KeyChar(Key: word; Shift: boolean): char;
  begin
    Result := Char(Key);
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
    if Key <= 255 then
    begin
      Ch := KeyChar(Key, ssShift in ShiftState);
      S := Ch;
    end
    else
    begin
      Ch := #0;
      S := '';
    end;
    State := 0;
    if ssAlt in ShiftState then
      State := integer(ButtonState_AltButton);
    if ssCtrl in ShiftState then
      State := State or Integer(ButtonState_ControlButton);
    if ssShift in ShiftState then
      State := State or Integer(ButtonState_ShiftButton);

    E := QKeyEvent_create(QEventType_KeyPress, Key, Ord(Ch), State, @S, false, 1);
    try
      QApplication_sendEvent(AControlHwnd, E);
    finally
      QKeyEvent_destroy(E);
    end;
{$ELSE}
    SetKeyboardStateDown(ShiftState);
    if ssAlt in ShiftState then
      PostMessage(AControlHwnd, WM_SYSKEYDOWN, Key, integer($20000000))
    else
      PostMessage(AControlHwnd, WM_KEYDOWN, Key, 0);
{$ENDIF}
    SyncSleep(KeyDownDelay);
{$IFDEF DUNIT_CLX}
    E := QKeyEvent_create(QEventType_KeyRelease, Key, Ord(Ch), State, @S, false, 1);
    try
      QApplication_sendEvent(AControlHwnd, E);
    finally
      QKeyEvent_destroy(E);
    end;
{$ELSE}
    if ssAlt in ShiftState then
      PostMessage(AControlHwnd, WM_SYSKEYUP, Key, integer($E0000000))
    else
      PostMessage(AControlHwnd, WM_KEYUP, Key, integer($C0000000));
    SetKeyboardStateUp(ShiftState);
{$ENDIF}
    SyncSleep(ActionDelay);
  end;
end;

procedure TGUIAutomation.EnterKeyInto(Control: TControl; Key: Word;
  const ShiftState: TShiftState);
var
  LWinControl: TWinControl;
begin
  Assert(Control <> nil, 'No control');
  LWinControl := FindParentWinControl(Control);
  if LWinControl <> nil then
    EnterKeyInto(LWinControl.Handle, Key, ShiftState);
end;

procedure TGUIAutomation.EnterKeyInto(ControlName: string; Key: Word; const ShiftState: TShiftState);
begin
  EnterKeyInto(FindControl(ControlName, CallerAddr), Key, ShiftState);
end;

procedure TGUIAutomation.EnterKey(Key: Char; const ShiftState: TShiftState);
begin
  EnterKey(Ord(Key), ShiftState);
end;

procedure TGUIAutomation.EnterKeyInto(const AControlHwnd: HWND; Key: Char;
  const ShiftState: TShiftState);
begin
  EnterKeyInto(AControlHwnd, Ord(Key), ShiftState);
end;

procedure TGUIAutomation.EnterKeyInto(Control: TControl; Key: Char; const ShiftState: TShiftState);
begin
  EnterKeyInto(Control, Ord(Key), ShiftState);
end;

procedure TGUIAutomation.EnterKeyInto(ControlName: string; Key: Char; const ShiftState: TShiftState);
begin
  EnterKeyInto(ControlName, Ord(Key), ShiftState);
end;

procedure TGUIAutomation.EnterText(Text: string);
begin
  EnterTextInto(GetFocus, Text);
end;

procedure TGUIAutomation.EnterTextInto(ControlName, Text: string);
begin
  EnterTextInto(FindControl(ControlName, CallerAddr), Text);
end;

procedure TGUIAutomation.EnterTextInto(const AControlHwnd: HWND; Text: string);
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

    for i := 1 to Length(Text) do
    begin
      if not ContinueExecution then
        Break;
{$IFDEF DUNIT_CLX}
      S := Text[i];
      E := QKeyEvent_create(QEventType_KeyPress, Ord(Text[i]), Ord(Text[i]), 0, @S, false, 1);
      QApplication_sendEvent(AControlHwnd, E);
      QKeyEvent_destroy(E);
{$ELSE}
      PostMessage(AControlHwnd, WM_CHAR, Ord(Text[i]), 0);
{$ENDIF}
      SyncSleep(TextEntryDelay);
    end;
  end;
end;

procedure TGUIAutomation.EnterTextInto(Control: TControl; Text: string);
var
  LWinControl: TWinControl;
begin
  Assert(Control <> nil, 'No control');
  LWinControl := FindParentWinControl(Control);
  if LWinControl <> nil then
    EnterTextInto(LWinControl.Handle, Text);
end;

procedure TGUIAutomation.Show(ControlName: string; OnOff: boolean);
begin
  Show(FindControl(ControlName, CallerAddr), OnOff);
end;

procedure TGUIAutomation.Show(Control: TControl; OnOff: boolean);
begin
  Assert(Control <> nil);
  Control.Visible := OnOff;
  Assert(Control.Visible = OnOff);
  SyncSleep(ActionDelay);
end;

function TGUIAutomation.ContinueExecution: boolean;
begin
  result := true;
  if Assigned(FOnGetContinueExecution) then
    FOnGetContinueExecution(result);
end;

procedure TGUIAutomation.Hide(ControlName: string);
begin
  Show(ControlName, false);
end;

procedure TGUIAutomation.Hide(Control: TControl);
begin
  Show(Control, false);
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

procedure TGUIAutomation.SetFocus(Control: TControl; Addrs: Pointer);
begin
  if Addrs = nil then
    Addrs := CallerAddr;

  if not (Control is TWinControl) then
    raise EGUIAutomation.Create(
        Format('Expected a TWinControl, but %s is a %s',
               [Control.Name, Control.ClassName])) at Addrs;
  if not TWinControl(Control).CanFocus then
    raise EGUIAutomation.Create(
        Format('Control %s:%s cannot focus',
               [Control.Name, Control.ClassName])) at Addrs;
  TWinControl(Control).SetFocus;
end;

procedure TGUIAutomation.SetFocus(ControlName: string);
begin
  SetFocus(FindControl(ControlName, CallerAddr), CallerAddr);
end;

end.

