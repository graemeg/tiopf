{ #(@)$Id: GUIActionRecorder.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $ }
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
 *   Select popup and main menu items (WM_SYSCOMMAND?)
 *   Copy/paste using keyboard (simulate with keypresses or need WM_COPY,
 *     WM_CUT, WM_PASTE?)
 *   Mouse wheel messages
 *   Context menu key
 *   Drag and drop
 *   TComboBox item selection using keyboard
 *   Use control hierarchy and index to find window handle of control at
 *     runtime for unnamed controls instead of using an absolute cursor
 *     position. This allows the control position to change such as when
 *     the window resized. e.g. Use '#<index>' as control name
 *   Optionally record timing. Calculate time between actions and add delay
 *     to commands to simulate actual usage.
 *   Detect change in foreground window and add a WaitForNewForegroundWindow
 *     command.
 *   Optionally record all mouse movement at set intervals.
 *   Set mouse position prior to text/key entry command (if pos changed since
 *     prev click command).
 *)

{$I DUnit.inc}

unit GUIActionRecorder;

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
  Classes,
  Contnrs;

const
  rcs_id: string = '#(@)$Id: GUIActionRecorder.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $';

  CEnterTextIntoCommandName = 'EnterTextInto';
  CEnterKeyIntoCommandName = 'EnterKeyInto';
  CEnterTextCommandName = 'EnterText';
  CEnterKeyCommandName = 'EnterKey';
  CSelectMenuItemCommandName = 'SelectMenuItem';
  CLeftButtonCommand = 'Left';
  CRightButtonCommand = 'Right';
  CMiddleButtonCommand = 'Middle';
  CMouseMoveCommand = 'Move';
  CMouseCommand = 'Mouse';
  CMouseDownCommand = 'Down';
  CMouseUpCommand = 'Up';
  CDoubleClickCommand = 'Double';
  CClickCommand = 'Click';
  CClickAtCommand = 'At';
  CClickToCommand = 'To';

type
  TGUIActionAbs = class;
  TGUIActionMouseDown = class;
  TGUIActionRecorder = class;

  TGUIActionRecorderEvent = procedure(Sender: TGUIActionRecorder) of object;
  TGUIActionRecorderStopRecordingEvent = procedure(Sender: TGUIActionRecorder;
      var AStopRecording: boolean) of object;
  TGUIActionRecorderActionEvent = procedure(AAction: TGUIActionAbs;
      var AContinue: boolean) of object;

  TMouseClickState = (mcsSingle, mcsDouble);

  TGUIActionCommandFormat = (acfNative, acfScript);

  TGUIActionMode = (amMonitor, amRecord);

  TGUIActionAbs = class {$IFDEF DELPHI2006_UP}abstract{$ENDIF} (TObject)
  private
    FHwnd: HWND;
    FControl: TControl;
    FControlName: string;
  protected
    function GetCommandName: string; virtual; abstract;
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; virtual;
    function JoinParams(const AFirst: string; const ASecond: string): string;
  public
    constructor Create(const AHwnd: HWND; const AControl: TControl; const AControlName: string);
    function AsString(const ACommandFormat: TGUIActionCommandFormat): string;
    function CommandParameters(const ACommandFormat: TGUIActionCommandFormat = acfNative): string;
    property Hwnd: HWND read FHwnd;
    property Control: TControl read FControl;
    property ControlName: string read FControlName;
    property CommandName: string read GetCommandName;
  end;

  TGUIActionList = class(TObjectList)
  protected
    function GetItem(AIndex: Integer): TGUIActionAbs;
    procedure SetItem(AIndex: Integer; AObject: TGUIActionAbs);
  public
    function AsString(const ACommandFormat: TGUIActionCommandFormat = acfNative): string;
    property Items[AIndex: Integer]: TGUIActionAbs read GetItem write SetItem; default;
  end;

  // Singleton
  TGUIActionRecorder = class(TObject)
  private
    FActions: TGUIActionList;
    FControlsToIgnore: TList;
    FEnteredText: string;
    FControl: TControl;
    FHwnd: HWND;
    FHighlightHwnd: HWND;
    FMouseDownAction: TGUIActionMouseDown;
    FMouseDownMoved: Boolean;

    FGUI: TWinControl;
    FActive: boolean;
    FActionMode: TGUIActionMode;
    FRecordMouseMove: Boolean;
    FHighlightControl: Boolean;
    FOnStopRecording: TGUIActionRecorderStopRecordingEvent;
    FOnAction: TGUIActionRecorderActionEvent;
    FOnRecordingStarted: TGUIActionRecorderEvent;
    FOnRecordingStopped: TGUIActionRecorderEvent;

    procedure AddAction(AAction: TGUIActionAbs; var AContinue: Boolean);
    procedure FlushTextEntry(var AContinue: Boolean);
    function CharFromVirtualKey(const AKey: Word): string;
    function CheckKeyState(const AVKCode: Word; var AKeyState: string;
        const ANewKeyState: string): boolean;
    procedure SetActive(const AValue: boolean);
    procedure SetControl(const AHwnd: HWND; var APoint: TPoint; var AContinue: Boolean);
    function ControlByName(const AControl: TControl): boolean;
    function ControlName: string;
    function ValidControl: boolean;
    procedure SetHighlightControl(const AValue: Boolean);
    procedure ToggleHighlight(const AHwnd: HWND);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure Finalize;
    procedure ProcessMessage(const AMessage: UINT; const AHwnd: HWND;
        const AWParam: WPARAM; var AContinue: boolean;
        const AX: Integer = -1; const AY: Integer = -1);
    procedure PerformHighlight;
    procedure StopRecording;
    procedure AddControlToIgnore(const AControl: TComponent); overload;

    property Actions: TGUIActionList read FActions;

    property GUI: TWinControl read FGUI write FGUI;
    property Active: boolean read FActive write SetActive;
    property ActionMode: TGUIActionMode read FActionMode write FActionMode;
    property RecordMouseMove: Boolean read FRecordMouseMove write FRecordMouseMove;
    property HighlightControl: Boolean read FHighlightControl write SetHighlightControl;
    property OnStopRecording: TGUIActionRecorderStopRecordingEvent read
        FOnStopRecording write FOnStopRecording;
    property OnRecordingStarted: TGUIActionRecorderEvent read FOnRecordingStarted write FOnRecordingStarted;
    property OnRecordingStopped: TGUIActionRecorderEvent read FOnRecordingStopped write FOnRecordingStopped;
    property OnAction: TGUIActionRecorderActionEvent read FOnAction write FOnAction;
  end;

  TGUIActionEnterTextInto = class(TGUIActionAbs)
  private
    FText: string;
  protected
    function GetCommandName: string; override;
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl;
        const AControlName: string; const AText: string);
    property Text: string read FText;
  end;

  TGUIActionEnterKey = class(TGUIActionAbs)
  private
    FKeyCode: Word;
    FKeyState: string;
    FCount: Integer;
  protected
    function GetCommandName: string; override;
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const AKeyCode: Word; const AKeyState: string; const ACount: Integer);
    property KeyCode: Word read FKeyCode;
    property KeyState: string read FKeyState;
    property Count: Integer read FCount write FCount;
  end;

  TGUIActionMouseAbs = class(TGUIActionAbs)
  private
    FX: Integer;
    FY: Integer;
  protected
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const AX: Integer; const AY: Integer);
    property X: Integer read FX;
    property Y: Integer read FY;
  end;

  TGUIActionMouseMove = class(TGUIActionMouseAbs)
  protected
    function GetCommandName: string; override;
  end;

  TGUIActionMouseButtonAbs = class(TGUIActionMouseAbs)
  private
    FButton: TMouseButton;
  protected
    function GetCommandNamePrefix: string;
    function GetCommandNameSuffix: string;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const AX: Integer; const AY: Integer; const AButton: TMouseButton);
    property Button: TMouseButton read FButton;
  end;

  TGUIActionMouseDown = class(TGUIActionMouseButtonAbs)
  protected
    function GetCommandName: string; override;
  end;

  TGUIActionMouseUp = class(TGUIActionMouseButtonAbs)
  protected
    function GetCommandName: string; override;
  end;

  TGUIActionClick = class(TGUIActionMouseButtonAbs)
  private
    FClickState: TMouseClickState;
  protected
    function GetCommandName: string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const AX: Integer; const AY: Integer; const AButton: TMouseButton;
        const AClickState: TMouseClickState);
    property ClickState: TMouseClickState read FClickState;
  end;

  TGUIActionSelectMenuItem = class(TGUIActionAbs)
  private
    FID: Integer;
  protected
    function GetCommandName: string; override;
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const AID: Integer);
    property ID: Integer read FID;
  end;

function GGUIActionRecorder: TGUIActionRecorder;

implementation

uses
   SysUtils
  ,Types
{$IFDEF MSWINDOWS}
  ,Menus
{$ENDIF}
  ;

var
  URecorder: TGUIActionRecorder;
  UProcessGetMessageHook: HHOOK;
//  UCallWndProcHook: HHOOK;

{$IFNDEF LINUX}
  {$IFNDEF DELPHI2010_UP}

const
  MAPVK_VK_TO_VSC    = 0;
  {$EXTERNALSYM MAPVK_VK_TO_VSC}
  MAPVK_VSC_TO_VK    = 1;
  {$EXTERNALSYM MAPVK_VSC_TO_VK}
  MAPVK_VK_TO_CHAR   = 2;
  {$EXTERNALSYM MAPVK_VK_TO_CHAR}
  MAPVK_VSC_TO_VK_EX = 3;
  {$EXTERNALSYM MAPVK_VSC_TO_VK_EX}
  MAPVK_VK_TO_VSC_EX = 4;
  {$EXTERNALSYM MAPVK_VK_TO_VSC_EX}

  {$ENDIF}
{$ENDIF}

resourcestring
  CSingleInstanceErrorMessage = 'Only one instance of TGUIActionRecorder is permitted';

function GGUIActionRecorder: TGUIActionRecorder;
begin
  if Assigned(URecorder) then
    Result := URecorder
  else
    Result := TGUIActionRecorder.Create;
end;

function ProcessGetMessageProcHook(AnCode: Integer;
  AwParam: WPARAM; AlParam: LPARAM): LRESULT; stdcall;
var
  LProcess: Boolean;
  LMsg: PMsg;
  LContinue: boolean;
begin
  LProcess := (AnCode = HC_ACTION) and (AwParam = PM_REMOVE) and
      Assigned(URecorder) and URecorder.Active;

  LContinue := true;
  try
    if LProcess then
    begin
      LMsg := PMsg(AlParam);
      URecorder.ProcessMessage(LMsg.message, LMsg.hwnd, LMsg.wParam,
          LContinue, LoWord(LMsg.lParam), HiWord(LMsg.lParam));
    end;
  finally
    if LContinue then
      Result := CallNextHookEx(UProcessGetMessageHook, AnCode, AwParam, AlParam)
    else
      Result := 0;
  end;

  // Highlight after the window has processed the message
  if LProcess and URecorder.HighlightControl then
    URecorder.PerformHighlight;

  // Must only unhook after CallNextHookEx
  if LProcess and (not LContinue) then
    URecorder.StopRecording;
end;

//function ProcessCallWndProcHook(AnCode: Integer;
//  AwParam: WPARAM; AlParam: LPARAM): Integer; stdcall;
//var
//  LStruct: PCWPStruct;
//  LContinue: boolean;
//begin
//  LContinue := true;
//  if Assigned(URecorder) and URecorder.Active then
//  begin
//    LStruct := PCWPStruct(AlParam);
//    URecorder.ProcessMessage(LStruct.message, LStruct.hwnd, LStruct.wParam,
//        LContinue);
//  end;
//
//  Result := CallNextHookEx(UCallWndProcHook, AnCode, AwParam, AlParam);
//
//  // Must only unhook after CallNextHookEx
//  if Assigned(URecorder) and (not LContinue) then
//    URecorder.StopRecording;
//end;

{ TGUIActionAbs }

constructor TGUIActionAbs.Create(const AHwnd: HWND; const AControl: TControl;
  const AControlName: string);
begin
  inherited Create;
  FHwnd := AHwnd;
  FControl := AControl;
  FControlName := AControlName;
end;

function TGUIActionAbs.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
begin
  if FControlName = '' then
    Result := ''
  else
    Result := Format('''%s''', [FControlName]);
end;

function TGUIActionAbs.CommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
begin
  Result := GetCommandParameters(ACommandFormat);
end;

function TGUIActionAbs.AsString(const ACommandFormat: TGUIActionCommandFormat): string;
var
  LParameters: string;
begin
  Result := CommandName;
  LParameters := CommandParameters(ACommandFormat);
  if LParameters <> '' then
    Result := Result + '(' + LParameters + ')';
end;

function TGUIActionAbs.JoinParams(const AFirst: string;
  const ASecond: string): string;
begin
  Result := AFirst;
  if (AFirst <> '') and (ASecond <> '') then
    Result := Result + ', ';
  Result := Result + ASecond;
end;

{ TGUIActionList }

function TGUIActionList.GetItem(AIndex: Integer): TGUIActionAbs;
begin
  Result := (inherited GetItem(AIndex)) as TGUIActionAbs;
end;

procedure TGUIActionList.SetItem(AIndex: Integer; AObject: TGUIActionAbs);
begin
  inherited SetItem(AIndex, AObject);
end;

function TGUIActionList.AsString(const ACommandFormat: TGUIActionCommandFormat): string;
var
  LActions: TStringList;
  I: Integer;
begin
  LActions := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      LActions.Add(Items[I].AsString(ACommandFormat));
    Result := LActions.Text;
  finally
    LActions.Free;
  end;
end;

{ TGUIActionRecorder }

constructor TGUIActionRecorder.Create;
begin
  if URecorder <> nil then
    raise Exception.Create(CSingleInstanceErrorMessage);
  inherited;
  URecorder := Self;

  FActions := TGUIActionList.Create;
  FControlsToIgnore := TList.Create;
  // Explicit initialization
  FEnteredText := '';
  FControl := nil;
  FHwnd := 0;
  FHighlightHwnd := 0;
  FMouseDownAction := nil;
  FMouseDownMoved := false;

  FGUI := nil;
  FActive := false;
  FActionMode := amRecord;
  FRecordMouseMove := false;
  FHighlightControl := false;
  FOnStopRecording := nil;
  FOnAction := nil;
end;

destructor TGUIActionRecorder.Destroy;
begin
  Active := false; // Use setter
  FControlsToIgnore.Free;
  FActions.Free;
  URecorder := nil;
  inherited;
end;

procedure TGUIActionRecorder.Initialize;
begin
  FActions.Clear;
end;

procedure TGUIActionRecorder.Finalize;
var
  LContinue: Boolean;
begin
  LContinue := false;
  FlushTextEntry(LContinue);
  FControl := nil;
  FHwnd := 0;
end;

function TGUIActionRecorder.ControlByName(const AControl: TControl):
  boolean;
begin
  Result := Assigned(FControl) and (FControl.Name <> '');
end;

function TGUIActionRecorder.ControlName: string;
begin
  if ControlByName(FControl) then
    Result := FControl.Name
  else
    Result := '';
end;

procedure TGUIActionRecorder.AddAction(AAction: TGUIActionAbs;
  var AContinue: Boolean);
begin
  try
    if Assigned(FOnAction) then
      FOnAction(AAction, AContinue);
    if FActionMode = amRecord then
    begin
      FActions.Add(AAction);
      AAction := nil;
    end;
  finally
    AAction.Free;
  end;
end;

procedure TGUIActionRecorder.SetActive(const AValue: boolean);
begin
  if FActive <> AValue then
  begin
    if AValue then
    begin
      if UProcessGetMessageHook = 0 then
        UProcessGetMessageHook := SetWindowsHookEx(WH_GETMESSAGE,
            ProcessGetMessageProcHook, 0, GetCurrentThreadId);
//      if UCallWndProcHook = 0 then
//        UCallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC,
//            ProcessCallWndProcHook, 0, GetCurrentThreadId);
// Also look at WH_JOURNALRECORD as a possibile alternative
    end
    else
    begin
      if FHighlightHwnd <> 0 then
      begin
        ToggleHighlight(FHighlightHwnd);
        FHighlightHwnd := 0;
      end;

//      UnHookWindowsHookEx(UCallWndProcHook);
//      UCallWndProcHook := 0;
      UnHookWindowsHookEx(UProcessGetMessageHook);
      UProcessGetMessageHook := 0;
      Finalize;
    end;

    FActive := AValue;
    if FActive and Assigned(FOnRecordingStarted) then
      FOnRecordingStarted(Self);
    if (not FActive) and Assigned(FOnRecordingStopped) then
      FOnRecordingStopped(Self);
  end;
end;

function _SourceToTarget(const ASourcePoint: TPoint;
  const ASourceControl: TControl; const ATargetControl: TControl): TPoint;
begin
  Result := ATargetControl.ScreenToClient(ASourceControl.ClientToScreen(ASourcePoint));
end;

// Finds the highest z-order (closest/visible) child control at a given point
// Based on TWinControl.ControlAtPos to fix the bug with nested TWinControls
// where it does not adjust the point for the child control co-ords
function _ControlAtPos(const AControl: TWinControl; const APos: TPoint;
  const AAllowDisabled: Boolean): TControl;
var
  I: Integer;
  LControl: TControl;

  function _ControlHit(const AControl: TControl; const AHitPos: TPoint): Boolean;
  var
    LP: TPoint;
  begin
    LP := Point(AHitPos.X - AControl.Left, AHitPos.Y - AControl.Top);
    Result :=
        AControl.Visible and (AControl.Enabled or AAllowDisabled) and
        PtInRect(AControl.ClientRect, LP) and
        (AControl.Perform(CM_HITTEST, 0, PointToLParam(LP)) <> 0);
  end;

begin
  Result := nil;

  // A control's non-windowed child controls are listed first followed by its
  // windowed child controls.
  // Windowed controls are displayed over the top of non-windowed controls and
  // later controls are displayed over the top of earlier controls so we check
  // them in reverse order.
  for I := AControl.ControlCount - 1 downto 0 do
    begin
      LControl := AControl.Controls[I];
      if _ControlHit(LControl, APos) then
      begin
        if LControl is TWinControl then
        begin
          Result := _ControlAtPos(LControl as TWinControl,
              _SourceToTarget(APos, AControl, LControl), AAllowDisabled);
          if not Assigned(Result) then
            Result := LControl;
        end
        else
          Result := LControl;
        break;
      end;
    end;
end;

procedure TGUIActionRecorder.SetControl(const AHwnd: HWND; var APoint: TPoint;
  var AContinue: Boolean);
var
  LWinControl: TWinControl;
  LHitControl: TControl;
begin
  // If the target control has a name then we can use that instead of the
  // windowed parent but we need to translate the co-ords

  // Find the VCL control with the given window handle
  LWinControl := FindControl(AHwnd);
  if Assigned(LWinControl) then
  begin
    LHitControl := _ControlAtPos(LWinControl, APoint, true {AllowDisabled});
    if Assigned(LHitControl) and ControlByName(LHitControl) then
      // Get point relative to child
      APoint := _SourceToTarget(APoint, LWinControl, LHitControl)
    else
      LHitControl := LWinControl;
  end else
    LHitControl := nil;

  if LHitControl <> FControl then
  begin
    FlushTextEntry(AContinue);
    FControl := LHitControl;
  end;

  FHwnd := AHwnd;
end;

procedure TGUIActionRecorder.SetHighlightControl(const AValue: Boolean);
begin
  if AValue <> FHighlightControl then
  begin
    FHighlightControl := AValue;
    if (not FHighlightControl) and (FHighlightHwnd <> 0) then
    begin
      ToggleHighlight(FHighlightHwnd);
      FHighlightHwnd := 0;
    end;
  end;
end;

procedure TGUIActionRecorder.StopRecording;
var
  LStopRecording: boolean;
begin
  LStopRecording := true;
  if Assigned(FOnStopRecording) then
    FOnStopRecording(Self, LStopRecording);

  if LStopRecording then
    Active := false;
end;

procedure TGUIActionRecorder.ToggleHighlight(const AHwnd: HWND);
var
  LRect: TRect;
  LDC: HDC;
  LPrevPen: HPen;
  LNewPen: HPen;
  LPrevBrush: HBrush;
  LNewBrush: HBrush;
begin
  LDC := GetWindowDC(AHwnd);
  try
    SetROP2(LDC, R2_NOT);

    LNewPen := CreatePen(PS_INSIDEFRAME, 2, 0);
    try
      LPrevPen := SelectObject(LDC, LNewPen);
      try
        LNewBrush := GetStockObject(Null_Brush);
        LPrevBrush := SelectObject(LDC, LNewBrush);
        try
          // Draw the highlight
          GetWindowRect(AHwnd, LRect);
          LRect.Inflate(1, 1, -1, -1);
          Rectangle(LDC, 1, 1, LRect.Width, LRect.Height);
        finally
          SelectObject(LDC, LPrevBrush);
        end;
      finally
        SelectObject(LDC, LPrevPen);
      end;
    finally
      DeleteObject(LNewPen);
    end;
  finally
    ReleaseDC(AHwnd, LDC);
  end;
end;

procedure TGUIActionRecorder.FlushTextEntry(var AContinue: Boolean);
begin
  if Assigned(FControl) and (FEnteredText <> '') then
  begin
    AddAction(TGUIActionEnterTextInto.Create(FHwnd, FControl, ControlName,
        FEnteredText), AContinue);
    FEnteredText := '';
  end;
end;

procedure TGUIActionRecorder.AddControlToIgnore(const AControl: TComponent);
begin
  FControlsToIgnore.Add(AControl);
end;

function TGUIActionRecorder.ValidControl: boolean;
begin
  result := FControlsToIgnore.IndexOf(FControl) = -1;
end;

function TGUIActionRecorder.CharFromVirtualKey(const AKey: Word): string;
var
  LState: TKeyboardState;
  LResult: Integer;
  LPResult: PChar;
begin
  GetKeyboardState(LState);
  SetLength(Result, 2);
  LPResult := PChar(Result);

  LResult := ToUnicode(AKey, MapVirtualKey(AKey, MAPVK_VK_TO_VSC),
      LState, LPResult, 2, 0);
  if LResult = 1 then
    SetLength(Result, 1)
  else if LResult <> 2 then
    Result := '';
end;

function TGUIActionRecorder.CheckKeyState(const AVKCode: Word;
  var AKeyState: string; const ANewKeyState: string): boolean;
begin
  Result := (GetKeyState(AVKCode) and $80) = $80;
  if Result then
  begin
    if AKeyState <> '' then
      AKeyState := AKeyState + ',';
    AKeyState := AKeyState + ANewKeyState;
  end;
end;

procedure TGUIActionRecorder.ProcessMessage(const AMessage: UINT;
  const AHwnd: HWND; const AWParam: WPARAM; var AContinue: boolean;
  const AX: Integer; const AY: Integer);
var
  LKeyState: string;
  LText: string;
  LPoint: TPoint;
  LButton: TMouseButton;

  // Translate control co-ords to window co-ords as we don't have a
  // repeatable window name or handle to rely on so we record it as
  // a click at the position relative to root window of the control.
  function _ControlToWindowCoords(const APoint: TPoint): TPoint;
  var
    LWindowHwnd: HWND;
  begin
    Result := APoint;
    ClientToScreen(AHwnd, Result);
    //LWindowHwnd := GetAncestor(AHwnd, GA_ROOT);
    LWindowHwnd := GetForegroundWindow;
    ScreenToClient(LWindowHwnd, Result);
  end;

begin
  if not FActive then
    Exit; //==>

  case AMessage of
    WM_SYSKEYDOWN, WM_KEYDOWN:
      begin
        // ctrl-break stops recording
        if (AWParam = VK_CANCEL) and
           ((GetKeyState(VK_CONTROL) and $80) = $80) then
        begin
          FlushTextEntry(AContinue);
          AContinue := false;
          Exit; //==>
        end;

        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);

        if ValidControl then
        begin
          if (AWParam <> VK_SHIFT) and (AWParam <> VK_CONTROL) and
             (AWParam <> VK_MENU) then
          begin
            LKeyState := '';
            CheckKeyState(VK_SHIFT, LKeyState, 'ssShift');
            CheckKeyState(VK_CONTROL, LKeyState, 'ssCtrl');
            CheckKeyState(VK_MENU, LKeyState, 'ssAlt');

            // Build up as much regular text to enter as possible
            if ((LKeyState = '') or (LKeyState = 'ssShift')) then
              LText := CharFromVirtualKey(AWParam)
            else
              LText := '';
            // Regular printable characters excluding single quote which we treat
            // as a special key (alternatively we could escape it)
            if (LText >= ' ') and (LText <= '~') and (LText <> '''') then
              FEnteredText := FEnteredText + LText
            else
            begin // Special characters
              FlushTextEntry(AContinue);

              // If the last action was EnterKey and it is the same key then
              // increment the count instead of creating a new action
              if (FActions.Last is TGUIActionEnterKey) and
                 (AHwnd = TGUIActionEnterKey(FActions.Last).Hwnd) and
                 (AWParam = TGUIActionEnterKey(FActions.Last).KeyCode) and
                 (LKeyState = TGUIActionEnterKey(FActions.Last).KeyState) then
                TGUIActionEnterKey(FActions.Last).Count :=
                    TGUIActionEnterKey(FActions.Last).Count + 1
              else
                AddAction(TGUIActionEnterKey.Create(AHwnd, FControl, ControlName,
                    AWParam, LKeyState, 1), AContinue);
            end;
          end;
        end;
      end;
    WM_LBUTTONDOWN,
    WM_RBUTTONDOWN:
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          if AMessage = WM_LBUTTONDOWN then
            LButton := mbLeft
          else
            LButton := mbRight;

          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if not ControlByName(FControl) then
            LPoint := _ControlToWindowCoords(LPoint);

          // For single click we record a mouse down but if the mouse up is at the
          // same position the action will be changed to a click for simplicity
          FMouseDownAction := TGUIActionMouseDown.Create(AHwnd, FControl,
              ControlName, LPoint.X, LPoint.Y, LButton);
          AddAction(FMouseDownAction, AContinue);
          FMouseDownMoved := false;
        end;
      end;
    WM_LBUTTONDBLCLK,
    WM_RBUTTONDBLCLK:
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          if AMessage = WM_LBUTTONDBLCLK then
            LButton := mbLeft
          else
            LButton := mbRight;

          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if not ControlByName(FControl) then
            LPoint := _ControlToWindowCoords(LPoint);

          AddAction(TGUIActionClick.Create(AHwnd, FControl, ControlName,
              LPoint.X, LPoint.Y, LButton, mcsDouble), AContinue);
        end;
      end;
    WM_LBUTTONUP,
    WM_RBUTTONUP:
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          if AMessage = WM_LBUTTONUP then
            LButton := mbLeft
          else
            LButton := mbRight;

          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if not ControlByName(FControl) then
            LPoint := _ControlToWindowCoords(LPoint);

          // If same control and position as mouse down then change to single click
          if Assigned(FMouseDownAction) and (AHwnd = FMouseDownAction.Hwnd) and
             (not FMouseDownMoved) and (LPoint.X = FMouseDownAction.X) and
             (LPoint.Y = FMouseDownAction.Y) then
          begin
            FActions.Remove(FMouseDownAction);
            FMouseDownAction := nil;
            AddAction(TGUIActionClick.Create(AHwnd, FControl, ControlName,
                LPoint.X, LPoint.Y, LButton, mcsSingle), AContinue);
          end
          else
            AddAction(TGUIActionMouseUp.Create(AHwnd, FControl, ControlName,
                LPoint.X, LPoint.Y, LButton), AContinue);
        end
        else
          FMouseDownAction := nil;
      end;
    WM_MOUSEMOVE:
      if FRecordMouseMove or Assigned(FMouseDownAction) then
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if not ControlByName(FControl) then
            LPoint := _ControlToWindowCoords(LPoint);

          // Record if position is different to mouse down
          if (not Assigned(FMouseDownAction)) or
             ((AHwnd <> FMouseDownAction.Hwnd) or
              (LPoint.X <> FMouseDownAction.X) or
              (LPoint.Y <> FMouseDownAction.Y)) then
          begin
            FMouseDownMoved := true;

            if FRecordMouseMove then
              AddAction(TGUIActionMouseMove.Create(AHwnd, FControl, ControlName,
                  LPoint.X, LPoint.Y), AContinue);
          end;
        end;
      end;
    WM_COMMAND:
{$IFDEF MSWINDOWS}
      // Menu commands are sent to the hidden window that tracks the menu
      // messages, not the visible menu control
      if AHwnd = Menus.PopupList.Window then
{$ENDIF}
      begin
        if HiWord(AWParam) = 0 {menu} then
          AddAction(TGUIActionSelectMenuItem.Create(AHwnd, FControl, ControlName,
              LoWord(AWParam) {ID_xxxx}), AContinue);
      end;
  end;
end;

procedure TGUIActionRecorder.PerformHighlight;
begin
  if FHighlightControl and (FHwnd <> FHighlightHwnd) then
  begin
    // Remove previous highlight
    if FHighlightHwnd <> 0 then
      ToggleHighlight(FHighlightHwnd);
    // Add new highlight
    if FHwnd <> 0 then
      ToggleHighlight(FHwnd);
    FHighlightHwnd := FHwnd;
  end;
end;

{ TGUIActionEnterTextInto }

constructor TGUIActionEnterTextInto.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string; const AText: string);
begin
  inherited Create(AHwnd, AControl, AControlName);
  FText := AText;
end;

function TGUIActionEnterTextInto.GetCommandName: string;
begin
  if ControlName = '' then
    Result := CEnterTextCommandName
  else
    Result := CEnterTextIntoCommandName;
end;

function TGUIActionEnterTextInto.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
begin
  Result := JoinParams((inherited GetCommandParameters(ACommandFormat)),
      Format('''%s''', [FText]));
end;

{ TGUIActionEnterKey }

constructor TGUIActionEnterKey.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string;
  const AKeyCode: Word; const AKeyState: string; const ACount: Integer);
begin
  inherited Create(AHwnd, AControl, AControlName);
  FKeyCode := AKeyCode;
  FKeyState := AKeyState;
  FCount := ACount;
end;

function TGUIActionEnterKey.GetCommandName: string;
begin
  if ControlName = '' then
    Result := CEnterKeyCommandName
  else
    Result := CEnterKeyIntoCommandName;
end;

function TGUIActionEnterKey.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
var
  LParams: string;
{$IFDEF MSWINDOWS}
  LKey: string;
{$ENDIF}
  LKeyStateParam: string;
  LCountParam: string;
begin
{$IFDEF MSWINDOWS}
  case FKeyCode of
    VK_BACK: LKey := 'VK_BACK';
    VK_TAB: LKey := 'VK_TAB';
    VK_RETURN: LKey := 'VK_RETURN';
    VK_PAUSE: LKey := 'VK_PAUSE';
    VK_ESCAPE: LKey := 'VK_ESCAPE';
    VK_SPACE: LKey := 'VK_SPACE';
    VK_PRIOR: LKey := 'VK_PRIOR';
    VK_NEXT: LKey := 'VK_NEXT';
    VK_END: LKey := 'VK_END';
    VK_HOME: LKey := 'VK_HOME';
    VK_LEFT: LKey := 'VK_LEFT';
    VK_UP: LKey := 'VK_UP';
    VK_RIGHT: LKey := 'VK_RIGHT';
    VK_DOWN: LKey := 'VK_DOWN';
    VK_PRINT: LKey := 'VK_PRINT';
    VK_INSERT: LKey := 'VK_INSERT';
    VK_DELETE: LKey := 'VK_DELETE';
    VK_F1: LKey := 'VK_F1';
    VK_F2: LKey := 'VK_F2';
    VK_F3: LKey := 'VK_F3';
    VK_F4: LKey := 'VK_F4';
    VK_F5: LKey := 'VK_F5';
    VK_F6: LKey := 'VK_F6';
    VK_F7: LKey := 'VK_F7';
    VK_F8: LKey := 'VK_F8';
    VK_F9: LKey := 'VK_F9';
    VK_F10: LKey := 'VK_F10';
    VK_F11: LKey := 'VK_F11';
    VK_F12: LKey := 'VK_F12';
  else
    LKey := IntToStr(FKeyCode);
  end;
{$ELSE}
  LKey := IntToStr(FKeyCode);
{$ENDIF}

  // Count is optional
  if FCount = 1 then
    LCountParam := ''
  else
    LCountParam := Format(', %d', [FCount]);

  // Key state is optional but must be specified if Count is specified
  if (FKeyState = '') and (LCountParam = '') then
    LKeyStateParam := ''
  else
  begin
    if ACommandFormat = acfScript then
      LKeyStateParam := Format(', ''[%s]''', [FKeyState])
    else
      LKeyStateParam := Format(', [%s]', [FKeyState]);
  end;

  LParams := Format('%s%s%s', [LKey, LKeyStateParam, LCountParam]);
  Result := JoinParams((inherited GetCommandParameters(ACommandFormat)), LParams);
end;

{ TGUIActionMouseAbs }

constructor TGUIActionMouseAbs.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string; const AX, AY: Integer);
const
  CMaxPos = 32767;
  CCalculateNegativePos = 65536;
begin
  inherited Create(AHwnd, AControl, AControlName);
  if AX > CMaxPos then
    FX := AX - CCalculateNegativePos
  else
    FX := AX;
  if AY > CMaxPos then
    FY := AY - CCalculateNegativePos
  else
    FY := AY;
end;

function TGUIActionMouseAbs.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
begin
  Result := JoinParams((inherited GetCommandParameters(ACommandFormat)),
      Format('%d, %d', [FX, FY]));
end;

{ TGUIActionMouseMove }

function TGUIActionMouseMove.GetCommandName: string;
begin
  Result := CMouseMoveCommand + CMouseCommand + CClickToCommand;
end;

{ TGUIActionMouseButtonAbs }

constructor TGUIActionMouseButtonAbs.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string; const AX, AY: Integer;
  const AButton: TMouseButton);
begin
  inherited Create(AHwnd, AControl, AControlName, AX, AY);
  FButton := AButton;
end;

function TGUIActionMouseButtonAbs.GetCommandNamePrefix: string;
begin
  case FButton of
    mbLeft: Result := CLeftButtonCommand;
    mbRight: Result := CRightButtonCommand;
//    mbMiddle: Result := CMiddleButtonCommand;
  else
    raise Exception.Create('Unhandled mouse button: ' + IntToStr(Ord(FButton)));
  end;
end;

function TGUIActionMouseButtonAbs.GetCommandNameSuffix: string;
begin
  if ControlName = '' then
    Result := CClickAtCommand
  else
    Result := '';
end;

{ TGUIActionMouseDown }

function TGUIActionMouseDown.GetCommandName: string;
begin
  Result := GetCommandNamePrefix + CMouseCommand + CMouseDownCommand + GetCommandNameSuffix;
end;

{ TGUIActionMouseUp }

function TGUIActionMouseUp.GetCommandName: string;
begin
  Result := GetCommandNamePrefix + CMouseCommand + CMouseUpCommand + GetCommandNameSuffix;
end;

{ TGUIActionClick }

constructor TGUIActionClick.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string; const AX: Integer;
  const AY: Integer; const AButton: TMouseButton;
  const AClickState: TMouseClickState);
begin
  inherited Create(AHwnd, AControl, AControlName, AX, AY, AButton);
  FClickState := AClickState;
end;

function TGUIActionClick.GetCommandName: string;
var
  LDoubleClick: string;
begin
  case FClickState of
    mcsSingle: LDoubleClick := '';
    mcsDouble: LDoubleClick := CDoubleClickCommand;
  else
    raise Exception.Create('Unhandled click state: ' + IntToStr(Ord(FClickState)));
  end;

  Result := GetCommandNamePrefix + LDoubleClick + CClickCommand + GetCommandNameSuffix;
end;

{ TGUIActionSelectMenuItem }

constructor TGUIActionSelectMenuItem.Create(const AHwnd: HWND;
  const AControl: TControl; const AControlName: string; const AID: Integer);
begin
  inherited Create(AHwnd, AControl, AControlName);
  FID := AID;
end;

function TGUIActionSelectMenuItem.GetCommandName: string;
begin
  Result := CSelectMenuItemCommandName;
end;

function TGUIActionSelectMenuItem.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
begin
  Result := JoinParams((inherited GetCommandParameters(ACommandFormat)),
      IntToStr(FID));
end;

initialization
  URecorder := nil;
//  UCallWndProcHook := 0;
  UProcessGetMessageHook := 0;

finalization
  FreeAndNil(URecorder);

end.

