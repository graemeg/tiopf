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
 *   Select main menu items (WM_SYSCOMMAND?)
 *   Copy/paste using keyboard (simulate with keypresses or need WM_COPY,
 *     WM_CUT, WM_PASTE?)
 *   Mouse wheel messages
 *   Context menu key
 *   Drag and drop
 *   TComboBox item selection using keyboard
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
  CHorizontalScrollCommandName = 'HorizontalScroll';
  CVerticalScrollCommandName = 'VerticalScroll';
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
  CChildControlNameToken = '/';
  CChildControlNameWindowToken = '@';

type
  TGUIActionAbs = class;
  TGUIActionMouseButtonAbs = class;
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
    FControlName: string;
    FHighlightHwnd: HWND;
    FMouseButtonAction: TGUIActionMouseButtonAbs;

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
    function GetMouseButton(const AMessage: UINT): TMouseButton;
    procedure SetActive(const AValue: boolean);
    procedure SetControl(const AHwnd: HWND; const AControl: TControl; var AContinue: Boolean); overload;
    procedure SetControl(const AHwnd: HWND; var AContinue: Boolean); overload;
    procedure SetControl(const AHwnd: HWND; var APoint: TPoint; var AContinue: Boolean); overload;
    // Does the control itself have a name?
    function ControlHasName(const AControl: TControl): boolean;
    // Does the control or any parent have a name? We can address using
    // the name hierarchy and child control index. e.g. ParentName/0
    function ControlCanBeAddressedByName(const AControl: TControl): boolean;
    function ControlNameHierarchy(const AControl: TControl;
        out ANamedControl: TControl;
        out AHeirarchicalName: string): boolean; overload;
{$IFDEF MSWINDOWS}
    function ControlNameHierarchy(const AHwnd: HWND;
        out ANamedControl: TControl;
        out AHeirarchicalName: string): boolean; overload;
{$ENDIF}
    function ValidControl: boolean;
    procedure SetHighlightControl(const AValue: Boolean);
    procedure ToggleHighlight(const AHwnd: HWND);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure Finalize;
    procedure ProcessMessage(const AMessage: UINT; const AHwnd: HWND;
        const AWParam: WPARAM; const ALParam: LPARAM; var AContinue: boolean;
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

  TGUIActionScrollAbs = class(TGUIActionAbs)
  private
    FCommand: Word;
    FPosition: Word;
    FCount: Integer;
    FX: Integer;
    FY: Integer;
  protected
    function GetCommandParameters(const ACommandFormat: TGUIActionCommandFormat): string; override;
  public
    constructor Create(
        const AHwnd: HWND; const AControl: TControl; const AControlName: string;
        const ACommand: Word; const APosition: Word; const ACount: Integer);
    property Command: Word read FCommand;
    property Position: Word read FPosition;
    property Count: Integer read FCount write FCount;
  end;

  TGUIActionScrollClass = class of TGUIActionScrollAbs;

  TGUIActionHorizontalScroll = class(TGUIActionScrollAbs)
  protected
    function GetCommandName: string; override;
  end;

  TGUIActionVerticalScroll = class(TGUIActionScrollAbs)
  protected
    function GetCommandName: string; override;
  end;

function GGUIActionRecorder: TGUIActionRecorder;

implementation

uses
   SysUtils
  ,Types
{$IFDEF MSWINDOWS}
  ,Menus
{$ENDIF}
  ,GUIUtils
  ,StrUtils
{
// Debugging
  ,tiLog
}
  ;

var
  URecorder: TGUIActionRecorder;
  UProcessGetMessageHook: HHOOK;
  UCallWndProcHook: HHOOK;

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
  LProcess := (AnCode = HC_ACTION) and (AwParam = PM_REMOVE {Message processed}) and
      Assigned(URecorder) and URecorder.Active;

  LContinue := true;
  try
    if LProcess then
    begin
      LMsg := PMsg(AlParam);
      URecorder.ProcessMessage(LMsg.message, LMsg.hwnd, LMsg.wParam,
          LMsg.lParam, LContinue, LoWord(LMsg.lParam), HiWord(LMsg.lParam));
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

function ProcessCallWndProcHook(AnCode: Integer;
  AwParam: WPARAM; AlParam: LPARAM): LRESULT; stdcall;
var
  LProcess: Boolean;
  LStruct: PCWPStruct;
  LContinue: boolean;
begin
  LProcess := (AnCode = HC_ACTION) and Assigned(URecorder) and URecorder.Active;

  LContinue := true;
  try
    if LProcess then
    begin
      LStruct := PCWPStruct(AlParam);
      URecorder.ProcessMessage(LStruct.message, LStruct.hwnd, LStruct.wParam,
          LStruct.lParam, LContinue, LoWord(LStruct.lParam),
          HiWord(LStruct.lParam));
    end;
  finally
    if LContinue then
      Result := CallNextHookEx(UCallWndProcHook, AnCode, AwParam, AlParam)
    else
      Result := 0;
  end;

  // Highlight after the window has processed the message
  if LProcess and URecorder.HighlightControl then
    URecorder.PerformHighlight;

  // Must only unhook after CallNextHookEx
  if LProcess and (not LContinue) then
    URecorder.StopRecording;

  // Must only unhook after CallNextHookEx
  if Assigned(URecorder) and (not LContinue) then
    URecorder.StopRecording;
end;

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
  FMouseButtonAction := nil;

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
  FMouseButtonAction := nil;
end;

function TGUIActionRecorder.ControlHasName(const AControl: TControl):
  boolean;
begin
  Result := Assigned(AControl) and (AControl.Name <> '');
end;

function TGUIActionRecorder.ControlCanBeAddressedByName(
  const AControl: TControl): boolean;
var
  LControl: TControl;
begin
  result := false;
  LControl := AControl;
  while (not result) and (LControl <> nil) do
  begin
    result := LControl.Name <> '';
    LControl := LControl.Parent;
  end;
end;

// Note: Currently this requires at least one control in the parent hierarchy
// to have a name but this is not strictly required as we could use the index
// even from the top level (first index would be the child control index of
// the active form).
function TGUIActionRecorder.ControlNameHierarchy(
  const AControl: TControl; out ANamedControl: TControl;
  out AHeirarchicalName: string): boolean;
var
  LControl: TControl;

  function _IndexInParent(const AChildControl: TControl): Integer;
  var
    LParent: TWinControl;
    i: Integer;
  begin
    result := -1;
    LParent := AChildControl.Parent;
    if LParent <> nil then
      for i := 0 to LParent.ControlCount - 1 do
        if LParent.Controls[i] = AChildControl then
        begin
          result := i;
          break;
        end;
  end;
  
  procedure _PrependName(const AName: string);
  begin
    if AHeirarchicalName <> '' then
      AHeirarchicalName := CChildControlNameToken + AHeirarchicalName;
    AHeirarchicalName := AName + AHeirarchicalName;
  end;
  
begin
  AHeirarchicalName := '';

  // Work up the parent control hierarchy until we find a control with a name,
  // using the child control index as the name at each level
  LControl := AControl;
  while (LControl <> nil) and (LControl.Name = '') do
  begin
    _PrependName(IntToStr(_IndexInParent(LControl)));
    LControl := LControl.Parent;
  end;

  // If we found a control with a name then start with it, else we can't use
  // the hierarchy
  if LControl <> nil then
  begin
    result := true;
    ANamedControl := LControl;
    _PrependName(LControl.Name);
  end
  else
  begin
    result := false;
    ANamedControl := nil;
    AHeirarchicalName := '';
  end;
end;

{$IFDEF MSWINDOWS}
type
  TIndexOfChildWindowParams = record
    HwndToFind: HWND;
    CurrentIndex: Integer;
    ResultIndex: Integer;
  end;
  PIndexOfChildWindowParams = ^TIndexOfChildWindowParams;
{$ENDIF}

{$IFDEF MSWINDOWS}
function IndexOfChildWindowProc(AHwnd: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  LParams: PIndexOfChildWindowParams;
begin
  LParams := PIndexOfChildWindowParams(ALParam);
  if AHwnd = LParams^.HwndToFind then
  begin
    LParams^.ResultIndex := LParams^.CurrentIndex;
    Result := false; // Stop enumeration
  end
  else
  begin
    Inc(LParams^.CurrentIndex);
    Result := true; // Continue enumeration
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TGUIActionRecorder.ControlNameHierarchy(const AHwnd: HWND;
    out ANamedControl: TControl;
    out AHeirarchicalName: string): boolean;
var
  LHwnd: HWND;
  LParentHwnd: HWND;
  LRootParentHwnd: HWND;
  LControl: TControl;
  LHeirarchicalName: string;

  function _IndexInParent(const AParentHwnd: HWND; const AChildHwnd: HWND): Integer;
  var
    LParams: TIndexOfChildWindowParams;
  begin
    if AParentHwnd <> 0 then
    begin
      LParams.HwndToFind := AChildHwnd;
      LParams.CurrentIndex := 0;
      LParams.ResultIndex := -1;
      EnumChildWindows(AParentHwnd, @IndexOfChildWindowProc, LPARAM(@LParams));
      result := LParams.ResultIndex;
    end
    else
      result := -1;
  end;

  procedure _PrependName(const AName: string);
  begin
    if AHeirarchicalName <> '' then
      AHeirarchicalName := CChildControlNameToken + AHeirarchicalName;
    AHeirarchicalName := AName + AHeirarchicalName;
  end;

begin
  AHeirarchicalName := '';

  // Work up the parent window hierarchy until a) we find a TControl and build
  // the hierarchy from it, or b) get to the root parent window, using the
  // child window index as the name at each level.
  // The further we traverse the greater the number of child windows in the
  // hierarchy and the more likely it is that the index will be wrong in a
  // repeated test as the creation order may be different.
  LRootParentHwnd := GetAncestor(AHwnd, GA_ROOT);
  //LRootParentHwnd := GetTopmostWindow; // Alternative but less likely to be correct
  LHwnd := AHwnd;
  LControl := FindControl(LHwnd);
  while (LHwnd <> 0) and (LHwnd <> LRootParentHwnd {topmost window}) and
        (LHwnd <> Menus.PopupList.Window {popup menu}) and (LControl = nil) do
  begin
    LParentHwnd := GetAncestor(LHwnd, GA_PARENT);
    if LParentHwnd <> 0 then
      _PrependName(CChildControlNameWindowToken +
          IntToStr(_IndexInParent(LParentHwnd, LHwnd)));

    LHwnd := LParentHwnd;
    LControl := FindControl(LHwnd);
  end;

  // If we found a VCL control then continue up the hierarchy using it as
  // hopefully we can find a control with a name or at the least the child
  // index should be more consistent
  if Assigned(LControl) then
  begin
    Result := ControlNameHierarchy(LControl, ANamedControl, LHeirarchicalName);
    if Result then
      _PrependName(LHeirarchicalName);
  end;

  Result := AHeirarchicalName <> '';
end;
{$ENDIF}

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
      // Mouse, keyboard, menu actions
      if UProcessGetMessageHook = 0 then
        UProcessGetMessageHook := SetWindowsHookEx(WH_GETMESSAGE,
            ProcessGetMessageProcHook, 0, GetCurrentThreadId);
      // Scroll
      if UCallWndProcHook = 0 then
        UCallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC,
            ProcessCallWndProcHook, 0, GetCurrentThreadId);
      // The hook WH_JOURNALRECORD might seem perfectly suited at first:
      //   http://msdn.microsoft.com/en-us/library/windows/desktop/ms644983(v=vs.85).aspx
      // The problem is that it can only be used under very strict security
      // conditions:
      //   http://www.wintellect.com/CS/blogs/jrobbins/archive/2008/08/30/so-you-want-to-set-a-windows-journal-recording-hook-on-vista-it-s-not-nearly-as-easy-as-you-think.aspx
    end
    else
    begin
      if FHighlightHwnd <> 0 then
      begin
        ToggleHighlight(FHighlightHwnd);
        FHighlightHwnd := 0;
      end;

      UnHookWindowsHookEx(UCallWndProcHook);
      UCallWndProcHook := 0;
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

procedure TGUIActionRecorder.SetControl(const AHwnd: HWND;
  const AControl: TControl; var AContinue: Boolean);
var
  LControl: TControl;
begin
  // Record the window handle
  if AHwnd <> FHwnd then
  begin
    FlushTextEntry(AContinue);
    FHwnd := AHwnd;
  end;

  // Record the control
  FControl := AControl;

  // Record the control name (if we can get one)
  if ControlHasName(AControl) then
    // Yes, we have a direct name
    FControlName := AControl.Name
  else if Assigned(AControl) and ControlCanBeAddressedByName(AControl) then
    // Try to name from the parent control using child control index
    ControlNameHierarchy(AControl, LControl, FControlName)
  else
{$IFDEF MSWINDOWS}
    // Try to name from the parent window using child window index
    ControlNameHierarchy(AHwnd, LControl, FControlName);
{$ELSE}
    FControlName := '';
{$ENDIF}
end;

procedure TGUIActionRecorder.SetControl(const AHwnd: HWND;
  var AContinue: Boolean);
begin
  // Look for a windowed VCL control with the given window handle
  SetControl(AHWnd, FindControl(AHwnd), AContinue);
end;

procedure TGUIActionRecorder.SetControl(const AHwnd: HWND; var APoint: TPoint;
  var AContinue: Boolean);
var
  LWinControl: TWinControl;
  LHitControl: TControl;
begin
  // Look for a windowed or non-windowed VCL control from the given window
  // handle and position
  LWinControl := FindControl(AHwnd);
  if Assigned(LWinControl) then
  begin
    // If the target control at the given co-ords is not a windowed control but
    // it has a name then we can use it instead of the windowed parent but we
    // need to translate the co-ords
    LHitControl := _ControlAtPos(LWinControl, APoint, true {AllowDisabled});
    if Assigned(LHitControl) and ControlHasName(LHitControl) then
      // Get point relative to child
      APoint := _SourceToTarget(APoint, LWinControl, LHitControl)
    else
      LHitControl := LWinControl;
  end else
    LHitControl := nil;

  SetControl(AHWnd, LHitControl, AContinue);
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
  if (FHwnd <> 0) and (FEnteredText <> '') then
  begin
    AddAction(TGUIActionEnterTextInto.Create(FHwnd, FControl, FControlName,
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

function TGUIActionRecorder.GetMouseButton(const AMessage: UINT): TMouseButton;
begin
  case AMessage of
    WM_LBUTTONDOWN,
    WM_NCLBUTTONDOWN,
    WM_LBUTTONDBLCLK,
    WM_NCLBUTTONDBLCLK,
    WM_LBUTTONUP,
    WM_NCLBUTTONUP:
      Result := mbLeft;
    WM_RBUTTONDOWN,
    WM_NCRBUTTONDOWN,
    WM_RBUTTONDBLCLK,
    WM_NCRBUTTONDBLCLK,
    WM_RBUTTONUP,
    WM_NCRBUTTONUP:
      Result := mbRight;
    WM_MBUTTONDOWN,
    WM_NCMBUTTONDOWN,
    WM_MBUTTONDBLCLK,
    WM_NCMBUTTONDBLCLK,
    WM_MBUTTONUP,
    WM_NCMBUTTONUP:
      Result := mbMiddle;
  else
    raise Exception.Create('Unhandled message: ' + IntToStr(Ord(AMessage)));
  end;
end;

{
// Debugging:
procedure _LogWindowDetails(const AHwnd: HWND; const APrefix: string = '');
var
  LControl: TControl;
  LClassName: string;
  LControlInfo: string;
  LRect: TRect;
begin
  SetLength(LClassName, 200);
  SetLength(LClassName, GetClassName(AHwnd, PChar(LClassName), 200));
  LControl := FindControl(AHwnd);
  if Assigned(LControl) then
    LControlInfo := LControl.ClassName + ' (' + LControl.Name + ')'
  else
    LControlInfo := '';
  LRect := Rect(0, 0, 0, 0);
  GetWindowRect(AHwnd, LRect);
  Log(APrefix + '0x%.8x %s [%s] (%d,%d %d,%d)', [AHwnd, LClassName,
      LControlInfo, LRect.Left, LRect.Top, LRect.Width, LRect.Height], lsUserInfo);
end;

function _LogChildWindowsProc(AHwnd: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  LLevel: Integer;
begin
  LLevel := Integer(ALParam);
  _LogWindowDetails(AHwnd, '=> ' + DupeString('  ', LLevel));
  Result := true; // Continue enumeration
end;

procedure _LogMessage(const AMessage: UINT; const AHwnd: HWND;
  const AWParam: WPARAM; const ALParam: LPARAM; const AX: Integer;
  const AY: Integer);
var
  LHwnd: HWND;
  LLevel: Integer;
begin
  Log('Message: 0x%.4x', [AMessage], lsUserInfo);
  Log('HWND: 0x%.8x', [AHwnd], lsUserInfo);
  Log('wParam: 0x%.8x', [AWParam], lsUserInfo);
  Log('lParam: 0x%.8x', [ALParam], lsUserInfo);
  Log('X: %d', [AX], lsUserInfo);
  Log('Y: %d', [AY], lsUserInfo);

  // Work up the parent window hierarchy until a) we find a TControl and build
  // the hierarchy from it, or b) get to the top window, using the child window
  // index as the name at each level
  LHwnd := AHwnd;
  LLevel := 0;
  while LHwnd <> 0 do
  begin
    _LogWindowDetails(LHwnd, DupeString('  ', LLevel));
    LHwnd := GetAncestor(LHwnd, GA_PARENT);
    Inc(LLevel);
  end;

  _LogWindowDetails(GetTopmostWindow, 'Topmost: ');
  _LogWindowDetails(GetForegroundWindow, 'Foreground: ');
  _LogWindowDetails(GetAncestor(AHwnd, GA_ROOT), 'Root: ');
  EnumChildWindows(GetAncestor(AHwnd, GA_ROOT), @_LogChildWindowsProc, LPARAM(0));
end;
}

procedure TGUIActionRecorder.ProcessMessage(const AMessage: UINT;
  const AHwnd: HWND; const AWParam: WPARAM; const ALParam: LPARAM;
  var AContinue: boolean; const AX: Integer; const AY: Integer);
var
  LKeyState: string;
  LText: string;
  LPoint: TPoint;
  LButton: TMouseButton;
  LScrollClass: TGUIActionScrollClass;
  LScrollCommand: Word;
  LScrollPosition: Word;
  LLastActionAsEnterKey: TGUIActionEnterKey;
  LLastActionAsScroll: TGUIActionScrollAbs;

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
    //LWindowHwnd := GetForegroundWindow;
    LWindowHwnd := GetTopmostWindow; // Works for popup menus
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

        SetControl(AHwnd, AContinue);

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
              if (FActions.Count > 0) and (FActions.Last is TGUIActionEnterKey) then
                LLastActionAsEnterKey := FActions.Last as TGUIActionEnterKey
              else
                LLastActionAsEnterKey := nil;
              if Assigned(LLastActionAsEnterKey) and
                 (AHwnd = LLastActionAsEnterKey.Hwnd) and
                 (AWParam = LLastActionAsEnterKey.KeyCode) and
                 (LKeyState = LLastActionAsEnterKey.KeyState) then
                LLastActionAsEnterKey.Count := LLastActionAsEnterKey.Count + 1
              else
                AddAction(TGUIActionEnterKey.Create(AHwnd, FControl, FControlName,
                    AWParam, LKeyState, 1), AContinue);
            end;
          end;
        end;
      end;
    WM_LBUTTONDOWN,
    WM_RBUTTONDOWN,
    WM_MBUTTONDOWN:
    //WM_NCLBUTTONDOWN
    //WM_NCRBUTTONDOWN
    //WM_NCMBUTTONDOWN
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if FControlName = '' then
            LPoint := _ControlToWindowCoords(LPoint);

          // For single click we record a mouse down but if the mouse up is at the
          // same position the action will be changed to a click for simplicity
          LButton := GetMouseButton(AMessage);
{
// Debugging:
//          if LButton = mbRight then
          _LogMessage(AMessage, AHwnd, AWParam, ALParam, AX, AY);
}
          FMouseButtonAction := TGUIActionMouseDown.Create(AHwnd, FControl,
              FControlName, LPoint.X, LPoint.Y, LButton);
          AddAction(FMouseButtonAction, AContinue);
        end;
      end;
    WM_LBUTTONDBLCLK,
    WM_RBUTTONDBLCLK,
    WM_MBUTTONDBLCLK:
    //WM_NCLBUTTONDBLCLK
    //WM_NCRBUTTONDBLCLK
    //WM_NCMBUTTONDBLCLK
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if FControlName = '' then
            LPoint := _ControlToWindowCoords(LPoint);

          // If same control and position as single mouse click then remove the
          // single click action. Message sequence for a double click is:
          // Mouse down
          // Mouse up
          // Double click down
          // Mouse up
          LButton := GetMouseButton(AMessage);
          if Assigned(FMouseButtonAction) and
             (FMouseButtonAction is TGUIActionClick) and
             ((FMouseButtonAction as TGUIActionClick).ClickState = mcsSingle) and
             (AHwnd = FMouseButtonAction.Hwnd) and
             (LButton = FMouseButtonAction.Button) and
             (LPoint.X = FMouseButtonAction.X) and
             (LPoint.Y = FMouseButtonAction.Y) then
            FActions.Remove(FMouseButtonAction);

          // Remember the double-click so that we skip the mouse up action
          FMouseButtonAction := TGUIActionClick.Create(AHwnd, FControl,
              FControlName, LPoint.X, LPoint.Y, LButton, mcsDouble);
          AddAction(FMouseButtonAction, AContinue);
        end;
      end;
    WM_LBUTTONUP,
    WM_RBUTTONUP,
    WM_MBUTTONUP:
    //WM_NCLBUTTONUP
    //WM_NCRBUTTONUP
    //WM_NCMBUTTONUP
      begin
        // Ignore mouse up after scroll as the scroll message does the work
        // This is OK for the scenarios discovered so far
        if (FActions.Count = 0) or (not (FActions.Last is TGUIActionScrollAbs)) then
        begin
          LPoint := Point(AX, AY);
          SetControl(AHwnd, LPoint, AContinue);
          FlushTextEntry(AContinue);

          if ValidControl then
          begin
            // If control cannot be identified by name then find the control
            // at runtime based on co-ords from foreground window.
            if FControlName = '' then
              LPoint := _ControlToWindowCoords(LPoint);

            // If same control and position as mouse down then:
            // - change mouse down, mouse up to single click action
            // - ignore mouse up for double-click action (it is built in)
            LButton := GetMouseButton(AMessage);
            if Assigned(FMouseButtonAction) and
               (AHwnd = FMouseButtonAction.Hwnd) and
               (LButton = FMouseButtonAction.Button) and
               (LPoint.X = FMouseButtonAction.X) and
               (LPoint.Y = FMouseButtonAction.Y) then
            begin
              if not (FMouseButtonAction is TGUIActionClick) {is double-click} then
              begin
                FActions.Remove(FMouseButtonAction);
                // Remember the new click as double-click will need to remove it
                FMouseButtonAction := TGUIActionClick.Create(AHwnd, FControl,
                    FControlName, LPoint.X, LPoint.Y, LButton, mcsSingle);
                AddAction(FMouseButtonAction, AContinue);
              end else
                FMouseButtonAction := nil;
            end
            else
            begin
              AddAction(TGUIActionMouseUp.Create(AHwnd, FControl, FControlName,
                  LPoint.X, LPoint.Y, LButton), AContinue);
              FMouseButtonAction := nil;
            end;
          end
          else
            FMouseButtonAction := nil;
        end;
      end;
    WM_MOUSEMOVE:
      if FRecordMouseMove then
      begin
        LPoint := Point(AX, AY);
        SetControl(AHwnd, LPoint, AContinue);
        FlushTextEntry(AContinue);

        if ValidControl then
        begin
          // If control cannot be identified by name then find the control
          // at runtime based on co-ords from foreground window.
          if FControlName = '' then
            LPoint := _ControlToWindowCoords(LPoint);

          // Record if position is different to mouse down
          if (not Assigned(FMouseButtonAction)) or
             ((AHwnd <> FMouseButtonAction.Hwnd) or
              (LPoint.X <> FMouseButtonAction.X) or
              (LPoint.Y <> FMouseButtonAction.Y)) then
            AddAction(TGUIActionMouseMove.Create(AHwnd, FControl, FControlName,
                LPoint.X, LPoint.Y), AContinue);
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
          AddAction(TGUIActionSelectMenuItem.Create(AHwnd, FControl,
              FControlName, LoWord(AWParam) {ID_xxxx}), AContinue);
      end;
    WM_HSCROLL,
    WM_VSCROLL:
      begin
        SetControl(AHwnd, AContinue);
        FlushTextEntry(AContinue);

        if AMessage = WM_HSCROLL then
          LScrollClass := TGUIActionHorizontalScroll
        else
          LScrollClass := TGUIActionVerticalScroll;
        LScrollCommand := LoWord(AWParam) {SB_xxxx};
        LScrollPosition := HiWord(AWParam); { For SB_THUMB... }

        if ValidControl
          {$IFDEF MSWINDOWS} and (LScrollCommand <> SB_ENDSCROLL) {$ENDIF} then
        begin
          // Increment existing count if multiple in a row
          if (FActions.Count > 0) and (FActions.Last is TGUIActionScrollAbs) then
            LLastActionAsScroll := FActions.Last as TGUIActionScrollAbs
          else
            LLastActionAsScroll := nil;
          if Assigned(LLastActionAsScroll) and
            (LLastActionAsScroll is LScrollClass) and
            (LLastActionAsScroll.Hwnd = AHwnd) and
            (LLastActionAsScroll.Command = LScrollCommand) and
            (LLastActionAsScroll.Position = LScrollPosition) then
            LLastActionAsScroll.Count := LLastActionAsScroll.Count + 1
          else
            AddAction(LScrollClass.Create(AHwnd, FControl, FControlName,
                LScrollCommand, LScrollPosition, 1), AContinue);
        end;
      end;
{
// Debugging:
    else
      if (AMessage <> WM_MOUSEMOVE) and (AMessage <> WM_NCMOUSEMOVE) and
         (AMessage <> WM_MOUSELEAVE) and (AMessage <> WM_NCMOUSELEAVE) and
         (AMessage <> WM_TIMER) and (AMessage <> WM_PAINT) and
         (AMessage <> WM_DWMNCRENDERINGCHANGED) then
        // See unit: Winapi.Messages
        Log('Message: 0x%.4x 0x%.8x 0x%.8x 0x%.8x', [AMessage, AHwnd, AWParam, ALParam], lsUserInfo);
}
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
    mbMiddle: Result := CMiddleButtonCommand;
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

{ TGUIActionScrollAbs }

constructor TGUIActionScrollAbs.Create(const AHwnd: HWND; const AControl: TControl;
  const AControlName: string; const ACommand: Word; const APosition: Word;
  const ACount: Integer);
var
  LRect: TRect;
begin
  inherited Create(AHwnd, AControl, AControlName);
  FCommand := ACommand;
  FPosition := APosition;
  FCount := ACount;
  // Use X and Y of the centre of the window to find it during playback
  if GetWindowRect(Hwnd, LRect) then
  begin
    FX := LRect.Width div 2;
    FY := LRect.Height div 2;
  end;
end;

function TGUIActionScrollAbs.GetCommandParameters(
  const ACommandFormat: TGUIActionCommandFormat): string;
var
  LCommand: string;
  LCoordParams: string;
  LHasPosition: Boolean;
  LCountParam: string;
  LPositionParam: string;
begin
  if ControlName = '' then
    LCoordParams := Format('%d, %d, ', [FX, FY])
  else
    LCoordParams := '';

{$IFDEF MSWINDOWS}
  case FCommand of
    SB_LINEUP {, SB_LINELEFT}: LCommand := 'SB_LINEBACK';
    SB_LINEDOWN {, SB_LINERIGHT}: LCommand := 'SB_LINEFORWARD';
    SB_PAGEUP {, SB_PAGELEFT}: LCommand := 'SB_PAGEBACK';
    SB_PAGEDOWN {, SB_PAGERIGHT}: LCommand := 'SB_PAGEFORWARD';
    SB_THUMBPOSITION: LCommand := 'SB_THUMBPOSITION';
    SB_THUMBTRACK: LCommand := 'SB_THUMBTRACK';
    SB_TOP {, SB_LEFT}: LCommand := 'SB_HOME';
    SB_BOTTOM {, SB_RIGHT}: LCommand := 'SB_END';
    SB_ENDSCROLL: LCommand := 'SB_ENDSCROLL';
  else
    LCommand := IntToStr(FCommand);
  end;

  LHasPosition := (FCommand = SB_THUMBPOSITION) or (FCommand = SB_THUMBTRACK);
{$ELSE}
  LCommand := IntToStr(FCommand);
  LHasPosition := false;
{$ENDIF}

  // Count is optional if position is not needed
  if (FCount = 1) and (not LHasPosition) then
    LCountParam := ''
  else
    LCountParam := Format(', %d', [FCount]);

  // Position is only specified with thumb tracking
{$IFDEF MSWINDOWS}
  if LHasPosition then
    LPositionParam := Format(', %d', [FPosition])
  else
    LPositionParam := '';
{$ELSE}
  LPositionParam := '';
{$ENDIF}

  Result := JoinParams((inherited GetCommandParameters(ACommandFormat)),
      Format('%s%s%s%s', [LCoordParams, LCommand, LCountParam, LPositionParam]));
end;

{ TGUIActionHorizontalScroll }

function TGUIActionHorizontalScroll.GetCommandName: string;
begin
  Result := CHorizontalScrollCommandName;
end;

{ TGUIActionVerticalScroll }

function TGUIActionVerticalScroll.GetCommandName: string;
begin
  Result := CVerticalScrollCommandName;
end;

initialization
  URecorder := nil;
  UCallWndProcHook := 0;
  UProcessGetMessageHook := 0;

finalization
  FreeAndNil(URecorder);

end.

