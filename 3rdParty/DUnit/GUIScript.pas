{ #(@)$Id$ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision$
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
 *)

//TODO: Add a function to retrieve the value of any property of a control by property name
//TODO: Bridge to model/objects/data presented in the UI. e.g. using RTTI.

{$I DUnit.inc}

unit GUIScript;

interface

uses
  SysUtils, Controls, dwsExprs, dwsCompiler, dwsComp,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  GUIActionRecorder,
  GUIAutomation;

const
  rcs_id: string = '#(@)$Id$';

type
  TGUIScript = class;

  TOnExecutionStateEvent = procedure(AScript: TGUIScript) of object;
  TOnGetControlTextEvent = procedure(const AControl: TControl;
      var AText: string; var AHandled: boolean) of object;
  TCallApplicationEvent = procedure(const AActionName: string;
      var AValue: string) of object;

  TControlClass = class of TControl;
  TControlClasses = array of TControlClass;

  TGUIScript = class(TObject)
  private
    FGUIAutomation: TGUIAutomation;
    FDWScript: TDelphiWebScript;
    FDWExec: IdwsProgramExecution;
    FUnit: TdwsUnit;
    FScriptSource: string;
    FScriptResult: string;
    FTerminated: Boolean;
    FRunSuccessful: Boolean;
    FContinueExecution: Boolean;
    FOnExecutionStarted: TOnExecutionStateEvent;
    FOnExecutionEnded: TOnExecutionStateEvent;
    FOnGetControlText: TOnGetControlTextEvent;
    FOnGetControlSelectedText: TOnGetControlTextEvent;
    FOnCallApplication: TCallApplicationEvent;
    FTextControls: TControlClasses;
    FControlInspectionIndent: string;

    function GetRecordedScript: string;
    procedure RunScript;
    procedure TerminateScript(Info: TProgramInfo);
    procedure AutomationContinueExecution(var AContinueExecution: boolean);
    procedure ScriptExecutionStarted(exec : TdwsProgramExecution);
    procedure ScriptExecutionEnded(exec : TdwsProgramExecution);

    function ControlName(const AInfo: TProgramInfo): string;
    function Control(const AControlName: string): TControl; overload;
    function Control(const AInfo: TProgramInfo): TControl; overload;
    function FormatCheckFailMessage(Info: TProgramInfo; const AMessageSuffix: string): string;
    procedure Check(const ACheckResult: Boolean; const AFailMessage: string);
    procedure StringToFile(const AString: string; const AFileName: string);
    function FileToString(const AFileName: string): string;
    function IsTextControl(const AControl: TControl): Boolean;
    function ShouldContinueExecution: Boolean;
    function FormatScriptCheckCommand(const ACommandName: string;
        const AControlName: string; const AParam: string = ''): string;
    function ActionName(const AInfo: TProgramInfo): string;
    function ActionExists(const AActionName: string): Boolean;
    function ActionVisible(const AActionName: string): Boolean;
    function ActionEnabled(const AActionName: string): Boolean;

    procedure SyncSleepEval(Info: TProgramInfo);
    procedure StringToFileEval(Info: TProgramInfo);
    procedure FileToStringEval(Info: TProgramInfo);
    procedure GetClipboardTextEval(Info: TProgramInfo);
    procedure SetClipboardTextEval(Info: TProgramInfo);
    procedure ReadINIFileStringEval(Info: TProgramInfo);
    procedure WriteINIFileStringEval(Info: TProgramInfo);
    procedure SaveScreenshotEval(Info: TProgramInfo);

    procedure RunScriptEval(Info: TProgramInfo);
    procedure TerminateScriptEval(Info: TProgramInfo);
    procedure ContinueExecutionEval(Info: TProgramInfo);
    procedure CallApplicationEval(Info: TProgramInfo);
    procedure ValueOfConditionalEval(Info: TProgramInfo);

    procedure SetActionDelayEval(Info: TProgramInfo);
    procedure SetMouseMoveDelayEval(Info: TProgramInfo);
    procedure SetKeyDownDelayEval(Info: TProgramInfo);
    procedure SetTextEntryDelayEval(Info: TProgramInfo);
    procedure SetControlWaitPeriodEval(Info: TProgramInfo);
    procedure SetMoveMouseCursorEval(Info: TProgramInfo);

    procedure WaitForControlExistsEval(Info: TProgramInfo);
    procedure WaitForControlVisibleEval(Info: TProgramInfo);
    procedure WaitForControlEnabledEval(Info: TProgramInfo);
    procedure WaitForWindowChangeEval(Info: TProgramInfo);
    procedure WaitForAnyWindowChangeEval(Info: TProgramInfo);

    procedure MoveMouseToEval(Info: TProgramInfo);
    procedure EnterTextIntoEval(Info: TProgramInfo);
    procedure EnterKeyIntoEval(Info: TProgramInfo);
    procedure EnterKeyEval(Info: TProgramInfo);
    procedure LeftMouseDownEval(Info: TProgramInfo);
    procedure LeftMouseDownAtEval(Info: TProgramInfo);
    procedure LeftMouseUpEval(Info: TProgramInfo);
    procedure LeftMouseUpAtEval(Info: TProgramInfo);
    procedure LeftClickEval(Info: TProgramInfo);
    procedure LeftClickAtEval(Info: TProgramInfo);
    procedure LeftDoubleClickEval(Info: TProgramInfo);
    procedure LeftDoubleClickAtEval(Info: TProgramInfo);
    procedure RightMouseDownEval(Info: TProgramInfo);
    procedure RightMouseDownAtEval(Info: TProgramInfo);
    procedure RightMouseUpEval(Info: TProgramInfo);
    procedure RightMouseUpAtEval(Info: TProgramInfo);
    procedure RightClickEval(Info: TProgramInfo);
    procedure RightClickAtEval(Info: TProgramInfo);
    procedure RightDoubleClickEval(Info: TProgramInfo);
    procedure RightDoubleClickAtEval(Info: TProgramInfo);
    procedure SelectMenuItemEval(Info: TProgramInfo);
    procedure HorizontalScrollEval(Info: TProgramInfo);
    procedure VerticalScrollEval(Info: TProgramInfo);
    procedure HorizontalScrollAtEval(Info: TProgramInfo);
    procedure VerticalScrollAtEval(Info: TProgramInfo);
    procedure SetFocusEval(Info: TProgramInfo);
    procedure ControlExistsEval(Info: TProgramInfo);
    procedure ControlVisibleEval(Info: TProgramInfo);
    procedure ControlEnabledEval(Info: TProgramInfo);
    procedure ControlNameEval(Info: TProgramInfo);
    procedure ChildControlCountEval(Info: TProgramInfo);
    procedure ControlTextEval(Info: TProgramInfo);
    procedure ControlSelectedTextEval(Info: TProgramInfo);
    procedure ActionExistsEval(Info: TProgramInfo);
    procedure ActionVisibleEval(Info: TProgramInfo);
    procedure ActionEnabledEval(Info: TProgramInfo);

    procedure FailEval(Info: TProgramInfo);
    procedure CheckTrueEval(Info: TProgramInfo);
    procedure CheckFalseEval(Info: TProgramInfo);
    procedure CheckEqualsEval(Info: TProgramInfo);
    procedure CheckNotEqualsEval(Info: TProgramInfo);
    procedure CheckFilesEqualEval(Info: TProgramInfo);
    procedure CheckFilesNotEqualEval(Info: TProgramInfo);

    procedure CheckControlExistsEval(Info: TProgramInfo);
    procedure CheckControlNotExistsEval(Info: TProgramInfo);
    procedure CheckControlEnabledEval(Info: TProgramInfo);
    procedure CheckControlNotEnabledEval(Info: TProgramInfo);
    procedure CheckControlVisibleEval(Info: TProgramInfo);
    procedure CheckControlNotVisibleEval(Info: TProgramInfo);
    procedure CheckControlFocusedEval(Info: TProgramInfo);
    procedure CheckControlTextEqualEval(Info: TProgramInfo);
    procedure CheckControlSelectedTextEqualEval(Info: TProgramInfo);
    procedure CheckActionExistsEval(Info: TProgramInfo);
    procedure CheckActionNotExistsEval(Info: TProgramInfo);
    procedure CheckActionEnabledEval(Info: TProgramInfo);
    procedure CheckActionNotEnabledEval(Info: TProgramInfo);
    procedure CheckActionVisibleEval(Info: TProgramInfo);
    procedure CheckActionNotVisibleEval(Info: TProgramInfo);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Retrieve script for recorded GUI actions
    property RecordedScript: string read GetRecordedScript;
    // Play back script and get result
    function Execute(const AScript: string): Boolean;
    procedure StopExecution;
    procedure RegisterTextControl(AControlClass: TControlClass);
    function GetControlText(const AHwnd: HWND): string; overload;
    function GetControlText(const AControl: TControl): string; overload;
    function GetControlSelectedText(const AHwnd: HWND): string; overload;
    function GetControlSelectedText(const AControl: TControl): string; overload;
    // Get the name and type of all controls in the hierarchy
    function GetControlList(const AControl: TControl): string;
    // Generate a script to check the state of all controls in the hierarchy
    function GetControlStatesScript(const AControl: TControl): string;
    // Generate a single script command to check the text of a control
    function GenerateCheckControlTextCommand(const AText: string;
        const AControlName: string; const ACheckAllText: Boolean): string; overload;
    function GenerateCheckControlTextCommand(const AControl: TControl;
        const AControlName: string; const ACheckAllText: Boolean): string; overload;
    function GenerateCheckControlTextCommand(const AHwnd: HWND;
        const AControlName: string; const ACheckAllText: Boolean): string; overload;
    // Get the name of all action lists and actions
    function GetActionList: string;
    // Generate a script to check the state of all actions
    function GetActionStatesScript: string;

    property OnExecutionStarted: TOnExecutionStateEvent read FOnExecutionStarted write FOnExecutionStarted;
    property OnExecutionEnded: TOnExecutionStateEvent read FOnExecutionEnded write FOnExecutionEnded;
    property OnGetControlText: TOnGetControlTextEvent read FOnGetControlText write FOnGetControlText;
    property OnGetControlSelectedText: TOnGetControlTextEvent read FOnGetControlSelectedText write FOnGetControlSelectedText;
    property OnCallApplication: TCallApplicationEvent read FOnCallApplication write FOnCallApplication;
    property ScriptResult: string read FScriptResult;
    property Terminated: boolean read FTerminated;
    property DWScript: TDelphiWebScript read FDWScript;
    // Indentation to use in GetControlList and GetControlStatesScript
    property ControlInspectionIndent: string read FControlInspectionIndent write FControlInspectionIndent;
  end;

  EGUIScript = class(Exception);
  EGUIScriptCheckFail = class(EGUIScript);
  EGUIScriptUnhandledControlType = class(Exception);

implementation

{$I dws.inc}

uses
  Classes,
  TypInfo,
  StdCtrls,
  ExtCtrls,
  Buttons,
  CheckLst,
  Grids,
  IniFiles,
  ActnList,
{$IFDEF MSWINDOWS}
  ComCtrls,
  Calendar,
{$ENDIF}
  Forms,
  SyncObjs,
  Clipbrd,
  StrUtils,

  // Make available some built-in functions provided by DWS
{$IFNDEF DWS_NO_BUILTIN_FUNCTIONS}
  dwsFileFunctions,
  dwsDebugFunctions,
  dwsGlobalVarsFunctions,
  dwsRTTIFunctions,
  dwsMathFunctions,
  dwsMath3DFunctions,
  dwsMathComplexFunctions,
  dwsStringFunctions,
  dwsTimeFunctions,
  dwsVariantFunctions,
  dwsVCLGUIFunctions,
{$ENDIF}

  dwsSymbols,
  GUIUtils;

{
  String/set conversion methods from:
  http://tondrej.blogspot.com.au/2007/10/settostring-stringtoset.html
}

function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Result := Byte(SetParam);
    otSWord, otUWord:
      Result := Word(SetParam);
    otSLong, otULong:
      Result := Integer(SetParam);
  end;
end;

procedure SetOrdValue(Info: PTypeInfo; var SetParam; Value: Integer);
begin
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Byte(SetParam) := Value;
    otSWord, otUWord:
      Word(SetParam) := Value;
    otSLong, otULong:
      Integer(SetParam) := Value;
  end;
end;

function SetToString(Info: PTypeInfo; const SetParam; Brackets: Boolean): String;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';

  Integer(S) := GetOrdValue(Info, SetParam);
  TypeInfo := GetTypeData(Info)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: String);
var
  P: PChar;
  EnumInfo: PTypeInfo;
  EnumName: String;
  EnumValue, SetValue: Longint;

  function NextWord(var P: PChar): String;
  var
    I: Integer;
  begin
    I := 0;
    // scan til whitespace
    while not CharInSet(P[I], [',', ' ', #0, ']']) do
      Inc(I);
    SetString(Result, P, I);
    // skip whitespace
    while CharInSet(P[I], [',', ' ', ']']) do
      Inc(I);
    Inc(P, I);
  end;

begin
  SetOrdValue(Info, SetParam, 0);
  if Value = '' then
    Exit;

  SetValue := 0;
  P := PChar(Value);
  // skip leading bracket and whitespace
  while CharInSet(P^, ['[', ' ']) do
    Inc(P);
  EnumInfo := GetTypeData(Info)^.CompType^;
  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
    begin
      SetOrdValue(Info, SetParam, 0);
      Exit;
    end;
    Include(TIntegerSet(SetValue), EnumValue);
    EnumName := NextWord(P);
  end;
  SetOrdValue(Info, SetParam, SetValue);
end;

{ TGUIScript }

constructor TGUIScript.Create;
var
  LFunction: TdwsFunction;

  procedure _AddConstant(const AName: string; const AType: string; const AValue: Variant);
  var
    LConstant: TdwsConstant;
  begin
    LConstant := FUnit.Constants.Add;
    LConstant.Name := AName;
    LConstant.DataType := AType;
    LConstant.Value := AValue;
  end;

  function _AddFunction(const AName: string; const AOnEval: TFuncEvalEvent): TdwsFunction;
  begin
    Result := FUnit.Functions.Add;
    Result.Name := AName;
    Result.OnEval := AOnEval;
  end;

  function _AddParameter(const AFunction: TdwsFunction; const AName: string;
    const AType: string): TdwsParameter; overload;
  begin
    Result := AFunction.Parameters.Add;
    Result.Name := AName;
    Result.DataType := AType;
  end;

  function _AddParameter(const AFunction: TdwsFunction; const AName: string;
    const AType: string; const ADefaultValue: Variant): TdwsParameter; overload;
  begin
    Result := _AddParameter(AFunction, AName, AType);
    Result.HasDefaultValue := true;
    Result.DefaultValue := ADefaultValue;
  end;

begin
  inherited;

  FGUIAutomation := TGUIAutomation.Create;
  FGUIAutomation.OnGetContinueExecution := AutomationContinueExecution;
  FDWScript := TDelphiWebScript.Create(nil);
  FDWScript.OnExecutionStarted := ScriptExecutionStarted;
  FDWScript.OnExecutionEnded := ScriptExecutionEnded;
  FUnit := TdwsUnit.Create(nil);
  FUnit.UnitName := 'UITestUtils';
  FUnit.StaticSymbols := False;
  FUnit.Script := FDWScript;

  RegisterTextControl(TCustomEdit);

  FControlInspectionIndent := '  ';

  // Constants

{$IFDEF MSWINDOWS}
  // Virtual keys
  _AddConstant('VK_BACK', 'Integer', Ord(VK_BACK));
  _AddConstant('VK_TAB', 'Integer', Ord(VK_TAB));
  _AddConstant('VK_RETURN', 'Integer', Ord(VK_RETURN));
  _AddConstant('VK_PAUSE', 'Integer', Ord(VK_PAUSE));
  _AddConstant('VK_ESCAPE', 'Integer', Ord(VK_ESCAPE));
  _AddConstant('VK_SPACE', 'Integer', Ord(VK_SPACE));
  _AddConstant('VK_PRIOR', 'Integer', Ord(VK_PRIOR));
  _AddConstant('VK_NEXT', 'Integer', Ord(VK_NEXT));
  _AddConstant('VK_END', 'Integer', Ord(VK_END));
  _AddConstant('VK_HOME', 'Integer', Ord(VK_HOME));
  _AddConstant('VK_LEFT', 'Integer', Ord(VK_LEFT));
  _AddConstant('VK_UP', 'Integer', Ord(VK_UP));
  _AddConstant('VK_RIGHT', 'Integer', Ord(VK_RIGHT));
  _AddConstant('VK_DOWN', 'Integer', Ord(VK_DOWN));
  _AddConstant('VK_PRINT', 'Integer', Ord(VK_PRINT));
  _AddConstant('VK_INSERT', 'Integer', Ord(VK_INSERT));
  _AddConstant('VK_DELETE', 'Integer', Ord(VK_DELETE));
  _AddConstant('VK_F1', 'Integer', Ord(VK_F1));
  _AddConstant('VK_F2', 'Integer', Ord(VK_F2));
  _AddConstant('VK_F3', 'Integer', Ord(VK_F3));
  _AddConstant('VK_F4', 'Integer', Ord(VK_F4));
  _AddConstant('VK_F5', 'Integer', Ord(VK_F5));
  _AddConstant('VK_F6', 'Integer', Ord(VK_F6));
  _AddConstant('VK_F7', 'Integer', Ord(VK_F7));
  _AddConstant('VK_F8', 'Integer', Ord(VK_F8));
  _AddConstant('VK_F9', 'Integer', Ord(VK_F9));
  _AddConstant('VK_F10', 'Integer', Ord(VK_F10));
  _AddConstant('VK_F11', 'Integer', Ord(VK_F11));
  _AddConstant('VK_F12', 'Integer', Ord(VK_F12));

  // Scrolling
  _AddConstant('SB_LINEBACK', 'Integer', Ord(SB_LINEUP)); // Custom: left or up
  _AddConstant('SB_LINEUP', 'Integer', Ord(SB_LINEUP));
  _AddConstant('SB_LINELEFT', 'Integer', Ord(SB_LINELEFT));
  _AddConstant('SB_LINEFORWARD', 'Integer', Ord(SB_LINEDOWN)); // Custom: right or down
  _AddConstant('SB_LINEDOWN', 'Integer', Ord(SB_LINEDOWN));
  _AddConstant('SB_LINERIGHT', 'Integer', Ord(SB_LINERIGHT));
  _AddConstant('SB_PAGEBACK', 'Integer', Ord(SB_PAGEUP)); // Custom: left or up
  _AddConstant('SB_PAGEUP', 'Integer', Ord(SB_PAGEUP));
  _AddConstant('SB_PAGELEFT', 'Integer', Ord(SB_PAGELEFT));
  _AddConstant('SB_PAGEFORWARD', 'Integer', Ord(SB_PAGEDOWN)); // Custom: right or down
  _AddConstant('SB_PAGEDOWN', 'Integer', Ord(SB_PAGEDOWN));
  _AddConstant('SB_PAGERIGHT', 'Integer', Ord(SB_PAGERIGHT));
  _AddConstant('SB_THUMBPOSITION', 'Integer', Ord(SB_THUMBPOSITION));
  _AddConstant('SB_THUMBTRACK', 'Integer', Ord(SB_THUMBTRACK));
  _AddConstant('SB_HOME', 'Integer', Ord(SB_TOP)); // Custom: left or top
  _AddConstant('SB_TOP', 'Integer', Ord(SB_TOP));
  _AddConstant('SB_LEFT', 'Integer', Ord(SB_LEFT));
  _AddConstant('SB_END', 'Integer', Ord(SB_BOTTOM)); // Custom: right or bottom
  _AddConstant('SB_BOTTOM', 'Integer', Ord(SB_BOTTOM));
  _AddConstant('SB_RIGHT', 'Integer', Ord(SB_RIGHT));
  _AddConstant('SB_ENDSCROLL', 'Integer', Ord(SB_ENDSCROLL));
{$ENDIF}

  // General methods

  LFunction := _AddFunction('SyncSleep', SyncSleepEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('StringToFile', StringToFileEval);
  _AddParameter(LFunction, 'String', 'String');
  _AddParameter(LFunction, 'FileName', 'String');

  LFunction := _AddFunction('FileToString', FileToStringEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'FileName', 'String');

  LFunction := _AddFunction('SetClipboardText', SetClipboardTextEval);
  _AddParameter(LFunction, 'String', 'String');

  LFunction := _AddFunction('GetClipboardText', GetClipboardTextEval);
  LFunction.ResultType := 'String';

  LFunction := _AddFunction('ReadINIFileString', ReadINIFileStringEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'FileName', 'String');
  _AddParameter(LFunction, 'Section', 'String');
  _AddParameter(LFunction, 'Ident', 'String');
  _AddParameter(LFunction, 'Default', 'String', '');

  LFunction := _AddFunction('WriteINIFileString', WriteINIFileStringEval);
  _AddParameter(LFunction, 'FileName', 'String');
  _AddParameter(LFunction, 'Section', 'String');
  _AddParameter(LFunction, 'Ident', 'String');
  _AddParameter(LFunction, 'Value', 'String');

  LFunction := _AddFunction('SaveScreenshot', SaveScreenshotEval);
  _AddParameter(LFunction, 'FileName', 'String');
  _AddParameter(LFunction, 'ActiveWindowOnly', 'Boolean', false);

  // General script control methods

  LFunction := _AddFunction('RunScript', RunScriptEval);
  _AddParameter(LFunction, 'FileName', 'String');

  _AddFunction('TerminateScript', TerminateScriptEval);

  LFunction := _AddFunction('ContinueExecution', ContinueExecutionEval);
  LFunction.ResultType := 'Boolean';

  LFunction := _AddFunction('CallApplication', CallApplicationEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ActionName', 'String', '');
  _AddParameter(LFunction, 'Value', 'String', '');

  LFunction := _AddFunction('ValueOfConditional', ValueOfConditionalEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ConditionalName', 'String');

  // GUI automation methods

  LFunction := _AddFunction('SetActionDelay', SetActionDelayEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('SetMouseMoveDelay', SetMouseMoveDelayEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('SetKeyDownDelay', SetKeyDownDelayEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('SetTextEntryDelay', SetTextEntryDelayEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('SetControlWaitPeriod', SetControlWaitPeriodEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  LFunction := _AddFunction('SetMoveMouseCursor', SetMoveMouseCursorEval);
  _AddParameter(LFunction, 'Move', 'Boolean', true);

  LFunction := _AddFunction('WaitForControlExists', WaitForControlExistsEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'ExistsOrNot', 'Boolean', true);
  _AddParameter(LFunction, 'Milliseconds', 'Integer', CDefaultControlWaitInterval);

  LFunction := _AddFunction('WaitForControlVisible', WaitForControlVisibleEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'VisibleOrNot', 'Boolean', true);
  _AddParameter(LFunction, 'Milliseconds', 'Integer', CDefaultControlWaitInterval);

  LFunction := _AddFunction('WaitForControlEnabled', WaitForControlEnabledEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'EnabledOrNot', 'Boolean', true);
  _AddParameter(LFunction, 'Milliseconds', 'Integer', CDefaultControlWaitInterval);

  LFunction := _AddFunction('WaitForWindowChange', WaitForWindowChangeEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'Caption', 'String');
  _AddParameter(LFunction, 'Milliseconds', 'Integer', CDefaultWindowChangeWaitInterval);

  LFunction := _AddFunction('WaitForAnyWindowChange', WaitForAnyWindowChangeEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'Milliseconds', 'Integer', CDefaultWindowChangeWaitInterval);

  LFunction := _AddFunction('MoveMouseTo', MoveMouseToEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');
  _AddParameter(LFunction, 'PixelInterval', 'Integer', -1);
  _AddParameter(LFunction, 'PixelsPerSecond', 'Integer', CDefaultMouseMovePixelsPerSecond);

  LFunction := _AddFunction('EnterTextInto', EnterTextIntoEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Text', 'String');

  LFunction := _AddFunction('EnterKeyInto', EnterKeyIntoEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Key', 'Integer');
  _AddParameter(LFunction, 'ShiftState', 'String', '');
  _AddParameter(LFunction, 'Count', 'Integer', 1);

  LFunction := _AddFunction('EnterKey', EnterKeyEval);
  _AddParameter(LFunction, 'Key', 'Integer');
  _AddParameter(LFunction, 'ShiftState', 'String', '');

  LFunction := _AddFunction('LeftMouseDown', LeftMouseDownEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('LeftMouseDownAt', LeftMouseDownAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftMouseUp', LeftMouseUpEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('LeftMouseUpAt', LeftMouseUpAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftClick', LeftClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('LeftClickAt', LeftClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftDoubleClick', LeftDoubleClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('LeftDoubleClickAt', LeftDoubleClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightMouseDown', RightMouseDownEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('RightMouseDownAt', RightMouseDownAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightMouseUp', RightMouseUpEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('RightMouseUpAt', RightMouseUpAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightClick', RightClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('RightClickAt', RightClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightDoubleClick', RightDoubleClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer', -1);
  _AddParameter(LFunction, 'Y', 'Integer', -1);

  LFunction := _AddFunction('RightDoubleClickAt', RightDoubleClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('SelectMenuItem', SelectMenuItemEval);
  _AddParameter(LFunction, 'ID', 'Integer');

  LFunction := _AddFunction('HorizontalScroll', HorizontalScrollEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Command', 'Integer');
  _AddParameter(LFunction, 'Count', 'Integer', 1);
  _AddParameter(LFunction, 'Position', 'Integer', 0);

  LFunction := _AddFunction('VerticalScroll', VerticalScrollEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Command', 'Integer');
  _AddParameter(LFunction, 'Count', 'Integer', 1);
  _AddParameter(LFunction, 'Position', 'Integer', 0);

  LFunction := _AddFunction('HorizontalScrollAt', HorizontalScrollAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');
  _AddParameter(LFunction, 'Command', 'Integer');
  _AddParameter(LFunction, 'Count', 'Integer', 1);
  _AddParameter(LFunction, 'Position', 'Integer', 0);

  LFunction := _AddFunction('VerticalScrollAt', VerticalScrollAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');
  _AddParameter(LFunction, 'Command', 'Integer');
  _AddParameter(LFunction, 'Count', 'Integer', 1);
  _AddParameter(LFunction, 'Position', 'Integer', 0);

  LFunction := _AddFunction('SetFocus', SetFocusEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  // GUI inspection

  LFunction := _AddFunction('ControlExists', ControlExistsEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ControlVisible', ControlVisibleEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ControlEnabled', ControlEnabledEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ActionExists', ActionExistsEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ActionName', 'String');

  LFunction := _AddFunction('ActionVisible', ActionVisibleEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ActionName', 'String');

  LFunction := _AddFunction('ActionEnabled', ActionEnabledEval);
  LFunction.ResultType := 'Boolean';
  _AddParameter(LFunction, 'ActionName', 'String');

  LFunction := _AddFunction('ControlName', ControlNameEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ChildControlCount', ChildControlCountEval);
  LFunction.ResultType := 'Integer';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ControlText', ControlTextEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('ControlSelectedText', ControlSelectedTextEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ControlName', 'String');

  // General testing check methods

  LFunction := _AddFunction('Fail', FailEval);
  _AddParameter(LFunction, 'FailMessage', 'String');

  LFunction := _AddFunction('CheckTrue', CheckTrueEval);
  _AddParameter(LFunction, 'Value', 'Boolean');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckFalse', CheckFalseEval);
  _AddParameter(LFunction, 'Value', 'Boolean');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckEquals', CheckEqualsEval);
  _AddParameter(LFunction, 'ExpectedValue', 'String');
  _AddParameter(LFunction, 'ActualValue', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckNotEquals', CheckNotEqualsEval);
  _AddParameter(LFunction, 'NotExpectedValue', 'String');
  _AddParameter(LFunction, 'ActualValue', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckFilesEqual', CheckFilesEqualEval);
  _AddParameter(LFunction, 'FileName1', 'String');
  _AddParameter(LFunction, 'FileName2', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckFilesNotEqual', CheckFilesNotEqualEval);
  _AddParameter(LFunction, 'FileName1', 'String');
  _AddParameter(LFunction, 'FileName2', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  // GUI testing check methods

  LFunction := _AddFunction('CheckControlEnabled', CheckControlEnabledEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlNotEnabled', CheckControlNotEnabledEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlVisible', CheckControlVisibleEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlNotVisible', CheckControlNotVisibleEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlExists', CheckControlExistsEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlNotExists', CheckControlNotExistsEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlFocused', CheckControlFocusedEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlTextEqual', CheckControlTextEqualEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Text', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckControlSelectedTextEqual', CheckControlSelectedTextEqualEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Text', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionExists', CheckActionExistsEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionNotExists', CheckActionNotExistsEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionVisible', CheckActionVisibleEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionNotVisible', CheckActionNotVisibleEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionEnabled', CheckActionEnabledEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');

  LFunction := _AddFunction('CheckActionNotEnabled', CheckActionNotEnabledEval);
  _AddParameter(LFunction, 'ActionName', 'String');
  _AddParameter(LFunction, 'FailMessage', 'String', '');
end;

destructor TGUIScript.Destroy;
begin
  FUnit.Free;
  FDWScript.Free;
  FGUIAutomation.Free;
  inherited;
end;

procedure TGUIScript.RunScript;
var
  LProgram: IdwsProgram;
begin
  FTerminated := false;
  FRunSuccessful := false;
  FScriptResult := '';
  LProgram := FDWScript.Compile(FScriptSource);
  if LProgram.Msgs.HasErrors then
    FScriptResult := LProgram.Msgs.AsInfo
  else
  begin
    try
      // Start with the mouse at the centre of the active form
      FGUIAutomation.MoveMouseTo(Screen.ActiveForm);
      FGUIAutomation.SyncSleep(0);
      FDWExec := LProgram.Execute;
      try
        if FDWExec.Msgs.HasErrors then
          FScriptResult := FDWExec.Msgs.AsInfo
        else
        begin
          FScriptResult := FDWExec.Result.ToString;
          FRunSuccessful := true;
        end;
      finally
        FDWExec := nil;
      end;
    except
      on E: Exception do
        FScriptResult := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

procedure TGUIScript.TerminateScript(Info: TProgramInfo);
begin
  Info.Execution.Stop;
  FTerminated := true;
end;

function TGUIScript.Execute(const AScript: string): Boolean;
begin
  FScriptSource := AScript;
  FGUIAutomation.ThreadedExecute(RunScript);
  Result := FRunSuccessful;
end;

function TGUIScript.ControlName(const AInfo: TProgramInfo): string;
begin
  Result := AInfo.ValueAsString['ControlName'];
end;

function TGUIScript.Control(const AControlName: string): TControl;
begin
  Result := FGUIAutomation.FindControl(AControlName);
end;

function TGUIScript.ShouldContinueExecution: Boolean;
begin
  result := FContinueExecution and
      ((not Assigned(FDWExec)) or (FDWExec.ProgramState = psRunning));
end;

procedure TGUIScript.AutomationContinueExecution(
  var AContinueExecution: boolean);
begin
  AContinueExecution := ShouldContinueExecution;
end;

function TGUIScript.Control(const AInfo: TProgramInfo): TControl;
begin
  Result := Control(ControlName(AInfo));
end;

function TGUIScript.GetRecordedScript: string;
var
  LCommands: TStringList;
  I: Integer;
begin
  LCommands := TStringList.Create;
  try
    LCommands.Text := GGUIActionRecorder.Actions.AsString(acfScript);
    // Indent and separate for script execution
    for I := 0 to LCommands.Count - 1 do
      LCommands.Strings[I] := '  ' + LCommands.Strings[I] + ';';
    Result := LCommands.Text;
  finally
    LCommands.Free;
  end;
end;

procedure TGUIScript.ScriptExecutionEnded(exec: TdwsProgramExecution);
begin
  if Assigned(FOnExecutionEnded) then
    FOnExecutionEnded(Self);
end;

procedure TGUIScript.ScriptExecutionStarted(exec: TdwsProgramExecution);
begin
  // Keep the automation running
  FContinueExecution := true;

  if Assigned(FOnExecutionStarted) then
    FOnExecutionStarted(Self);
end;

procedure TGUIScript.StopExecution;
begin
  FContinueExecution := false;
  FGUIAutomation.WakeUp;
end;

procedure TGUIScript.SyncSleepEval(Info: TProgramInfo);
begin
  FGUIAutomation.SyncSleep(Info.ValueAsInteger['Milliseconds']);
end;

// Note: Unicode not supported
procedure TGUIScript.StringToFile(const AString: string;
  const AFileName: string);
var
  LFileStream: TFileStream;
  LString: AnsiString;
begin
  LFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    LString := AnsiString(AString);
    LFileStream.WriteBuffer(Pointer(LString)^, (Length(LString)));
  finally
    LFileStream.Free;
  end;
end;

// Note: Unicode not supported
function TGUIScript.FileToString(const AFileName: string): string;
var
  LFileStream: TFileStream;
  LString: AnsiString;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(LString, LFileStream.Size);
    LFileStream.ReadBuffer(Pointer(LString)^, LFileStream.Size);
    Result := String(LString);
  finally
    LFileStream.Free;
  end;
end;

procedure TGUIScript.StringToFileEval(Info: TProgramInfo);
begin
  StringToFile(Info.ValueAsString['String'], Info.ValueAsString['FileName']);
end;

procedure TGUIScript.FileToStringEval(Info: TProgramInfo);
begin
  Info.ResultAsString := FileToString(Info.ValueAsString['FileName']);
end;

procedure TGUIScript.GetClipboardTextEval(Info: TProgramInfo);
begin
  Info.ResultAsString := Clipboard.AsText;
end;

procedure TGUIScript.SetClipboardTextEval(Info: TProgramInfo);
begin
  Clipboard.AsText := Info.ValueAsString['String'];
end;

procedure TGUIScript.ReadINIFileStringEval(Info: TProgramInfo);
var
  LINI: TINIFile;
begin
  LINI := TINIFile.Create(Info.ValueAsString['FileName']);
  try
    Info.ResultAsString := LINI.ReadString(Info.ValueAsString['Section'],
        Info.ValueAsString['Ident'], Info.ValueAsString['Default']);
  finally
    LINI.Free;
  end;
end;

procedure TGUIScript.WriteINIFileStringEval(Info: TProgramInfo);
var
  LINI: TINIFile;
begin
  LINI := TINIFile.Create(Info.ValueAsString['FileName']);
  try
    LINI.WriteString(Info.ValueAsString['Section'],
        Info.ValueAsString['Ident'], Info.ValueAsString['Value']);
  finally
    LINI.Free;
  end;
end;

procedure TGUIScript.SaveScreenshotEval(Info: TProgramInfo);
var
  LHwnd: HWND;
begin
  if Info.ValueAsBoolean['ActiveWindowOnly'] then
    LHwnd := GetTopmostWindow
  else
    LHwnd := HWND_DESKTOP;
  SaveScreenshot(LHwnd, Info.ValueAsString['FileName']);
end;

procedure TGUIScript.RunScriptEval(Info: TProgramInfo);
var
  LScript: TGUIScript;
  LScriptSource: TStringList;
begin
  LScript := nil;
  LScriptSource := nil;
  try
    LScript := TGUIScript.Create;
    LScriptSource := TStringList.Create;

    LScriptSource.LoadFromFile(Info.ValueAsString['FileName']);
    if not LScript.Execute(LScriptSource.Text) then
      raise EGUIScript.Create(LScript.ScriptResult);
    if LScript.Terminated then
      TerminateScript(Info);
  finally
    LScriptSource.Free;
    LScript.Free;
  end;
end;

procedure TGUIScript.TerminateScriptEval(Info: TProgramInfo);
begin
  TerminateScript(Info);
end;

procedure TGUIScript.ContinueExecutionEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := ShouldContinueExecution;
end;

procedure TGUIScript.CallApplicationEval(Info: TProgramInfo);
var
  LValue: string;
begin
  LValue := Info.ValueAsString['Value'];
  if Assigned(FOnCallApplication) then
    FOnCallApplication(Info.ValueAsString['ActionName'], LValue);
  Info.ResultAsString := LValue;
end;

procedure TGUIScript.ValueOfConditionalEval(Info: TProgramInfo);
begin
  Info.ResultAsString := FDWScript.Config.Conditionals.Values[
      Info.ValueAsString['ConditionalName']];
end;

procedure TGUIScript.SetActionDelayEval(Info: TProgramInfo);
begin
  FGUIAutomation.ActionDelay := Info.ValueAsInteger['Milliseconds'];
end;

procedure TGUIScript.SetMouseMoveDelayEval(Info: TProgramInfo);
begin
  FGUIAutomation.MouseMoveDelay := Info.ValueAsInteger['Milliseconds'];
end;

procedure TGUIScript.SetKeyDownDelayEval(Info: TProgramInfo);
begin
  FGUIAutomation.KeyDownDelay := Info.ValueAsInteger['Milliseconds'];
end;

procedure TGUIScript.SetTextEntryDelayEval(Info: TProgramInfo);
begin
  FGUIAutomation.TextEntryDelay := Info.ValueAsInteger['Milliseconds'];
end;

procedure TGUIScript.SetControlWaitPeriodEval(Info: TProgramInfo);
begin
  FGUIAutomation.ControlWaitPeriod := Info.ValueAsInteger['Milliseconds'];
end;

procedure TGUIScript.SetMoveMouseCursorEval(Info: TProgramInfo);
begin
  FGUIAutomation.MoveMouseCursor := Info.ValueAsBoolean['Move'];
end;

procedure TGUIScript.WaitForControlExistsEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.WaitForControlExists(
      ControlName(Info),
      Info.ValueAsBoolean['ExistsOrNot'],
      Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.WaitForControlVisibleEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.WaitForControlVisible(
      ControlName(Info),
      Info.ValueAsBoolean['VisibleOrNot'],
      Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.WaitForControlEnabledEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.WaitForControlEnabled(
      ControlName(Info),
      Info.ValueAsBoolean['EnabledOrNot'],
      Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.WaitForWindowChangeEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.WaitForWindowChange(
      Info.ValueAsString['Caption'],
      Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.WaitForAnyWindowChangeEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.WaitForWindowChange(
      Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.MoveMouseToEval(Info: TProgramInfo);
begin
  FGUIAutomation.MoveMouseTo(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y'],
      Info.ValueAsInteger['PixelInterval'],
      Info.ValueAsInteger['PixelsPerSecond']);
end;

procedure TGUIScript.EnterTextIntoEval(Info: TProgramInfo);
begin
  FGUIAutomation.EnterTextInto(
      ControlName(Info),
      Info.ValueAsString['Text']);
end;

procedure TGUIScript.EnterKeyIntoEval(Info: TProgramInfo);
var
  LShiftState: TShiftState;
begin
  StringToSet(TypeInfo(TShiftState), LShiftState, Info.ValueAsString['ShiftState']);
  FGUIAutomation.EnterKeyInto(
      ControlName(Info),
      Char(Info.ValueAsInteger['Key']),
      LShiftState,
      Info.ValueAsInteger['Count']);
end;

procedure TGUIScript.EnterKeyEval(Info: TProgramInfo);
var
  LShiftState: TShiftState;
begin
  StringToSet(TypeInfo(TShiftState), LShiftState, Info.ValueAsString['ShiftState']);
  FGUIAutomation.EnterKey(
      Char(Info.ValueAsInteger['Key']),
      LShiftState);
end;

procedure TGUIScript.LeftMouseDownEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftMouseDown(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftMouseDownAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftMouseDownAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftMouseUpEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftMouseUp(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftMouseUpAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftMouseUpAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftClickEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftClick(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftClickAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftClickAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftDoubleClickEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftDoubleClick(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.LeftDoubleClickAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftDoubleClickAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightMouseDownEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightMouseDown(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightMouseDownAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightMouseDownAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightMouseUpEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightMouseUp(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightMouseUpAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightMouseUp(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightClickEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightClick(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightClickAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightClickAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightDoubleClickEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightDoubleClick(
      ControlName(Info),
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.RightDoubleClickAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightDoubleClickAt(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y']);
end;

procedure TGUIScript.SelectMenuItemEval(Info: TProgramInfo);
begin
  FGUIAutomation.SelectMenuItem(Info.ValueAsInteger['ID']);
end;

procedure TGUIScript.HorizontalScrollEval(Info: TProgramInfo);
begin
  FGUIAutomation.HorizontalScroll(
      ControlName(Info),
      Info.ValueAsInteger['Command'],
      Info.ValueAsInteger['Count'],
      Info.ValueAsInteger['Position']);
end;

procedure TGUIScript.VerticalScrollEval(Info: TProgramInfo);
begin
  FGUIAutomation.VerticalScroll(
      ControlName(Info),
      Info.ValueAsInteger['Command'],
      Info.ValueAsInteger['Count'],
      Info.ValueAsInteger['Position']);
end;

procedure TGUIScript.HorizontalScrollAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.HorizontalScroll(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y'],
      Info.ValueAsInteger['Command'],
      Info.ValueAsInteger['Count'],
      Info.ValueAsInteger['Position']);
end;

procedure TGUIScript.VerticalScrollAtEval(Info: TProgramInfo);
begin
  FGUIAutomation.VerticalScroll(
      Info.ValueAsInteger['X'],
      Info.ValueAsInteger['Y'],
      Info.ValueAsInteger['Command'],
      Info.ValueAsInteger['Count'],
      Info.ValueAsInteger['Position']);
end;

procedure TGUIScript.SetFocusEval(Info: TProgramInfo);
begin
  FGUIAutomation.SetFocus(ControlName(Info));
end;

function TGUIScript.GetControlText(const AHwnd: HWND): string;
begin
  result := WindowText(AHwnd);
end;

function TGUIScript.GetControlText(const AControl: TControl): string;
var
  i, x, y: Integer;
  LHandled: Boolean;

  procedure _AppendLine(var AString: string; const AAppend: string);
  begin
    if AString <> '' then
      AString := AString + #13#10;
    AString := AString + AAppend;
  end;

begin
  Result := '';
  LHandled := true;
  if AControl is TCustomEdit then // TEdit, TMemo, etc
    Result := (AControl as TCustomEdit).Text
  else if AControl is TCheckBox then
    Result := BoolToStr((AControl as TCheckBox).Checked, true {UseBoolStrs})
  else if AControl is TRadioButton then
    Result := BoolToStr((AControl as TRadioButton).Checked, true {UseBoolStrs})
  else if AControl is TButton then
    Result := (AControl as TButton).Caption
  else if AControl is TBitBtn then
    Result := (AControl as TBitBtn).Caption
  else if AControl is TSpeedButton then
    Result := (AControl as TSpeedButton).Caption
  else if AControl is TCustomLabel then
    Result := (AControl as TCustomLabel).Caption // TLabel
  else if AControl is TStaticText then
    Result := (AControl as TStaticText).Caption
  else if AControl is TComboBox then
    Result := (AControl as TComboBox).Items.Text
  else if AControl is TForm then
    Result := (AControl as TForm).Caption
  else if AControl is TPanel then
    Result := (AControl as TPanel).Caption
  else if AControl is TGroupBox then
    Result := (AControl as TGroupBox).Caption
  else if AControl is TSplitter then
    Result := IntToStr((AControl as TSplitter).Left) + ',' + IntToStr((AControl as TSplitter).Top)
  else if AControl is TScrollBar then
    Result := IntToStr((AControl as TScrollBar).Position)
{$IFDEF MSWINDOWS}
  else if AControl is TPageControl then
  begin
    Result := IntToStr((AControl as TPageControl).TabIndex);
    if Assigned((AControl as TPageControl).ActivePage) then
      Result := Result + ': ' + (AControl as TPageControl).ActivePage.Caption;
  end
  else if AControl is TTabSheet then
    Result := (AControl as TTabSheet).Caption
  else if AControl is TRichEdit then
    Result := (AControl as TRichEdit).Text
  else if AControl is TDateTimePicker then
  begin
    if (AControl as TDateTimePicker).Kind = dtkDate then
      Result := DateToStr((AControl as TDateTimePicker).Date)
    else
      Result := TimeToStr((AControl as TDateTimePicker).Time);
  end
  else if AControl is TTrackBar then
    Result := IntToStr((AControl as TTrackBar).Position)
  else if AControl is TProgressBar then
    Result := IntToStr((AControl as TProgressBar).Position)
  else if AControl is TMonthCalendar then
    Result := DateToStr((AControl as TMonthCalendar).Date)
  else if AControl is TCalendar then
    Result := DateToStr((AControl as TCalendar).CalendarDate)
  else if AControl is TCustomTreeView then
    for i := 0 to (AControl as TCustomTreeView).SelectionCount - 1 do
      _AppendLine(Result, (AControl as TCustomTreeView).Selections[i].Text)
  else if AControl is TCustomListView then
  begin
    for i := 0 to (AControl as TCustomListView).Items.Count - 1 do
      if (AControl as TCustomListView).Items[i].Selected then
        _AppendLine(Result, (AControl as TCustomListView).Items[i].Caption);
  end
  else if AControl is TStatusBar then
    for i := 0 to (AControl as TStatusBar).Panels.Count - 1 do
      _AppendLine(Result, (AControl as TStatusBar).Panels.Items[i].Text)
  else if AControl is TPaintBox then
    Result := IntToStr((AControl as TPaintBox).Width) + ',' + IntToStr((AControl as TPaintBox).Height)
{$ENDIF}
  else if AControl is TImage then
    Result := IntToStr((AControl as TImage).Width) + ',' + IntToStr((AControl as TImage).Height)
  else if AControl is TListBox then
    Result := (AControl as TListBox).Items.Text
  else if AControl is TCheckListBox then
    Result := (AControl as TCheckListBox).Items.Text
  else if AControl is TRadioGroup then
    Result := (AControl as TRadioGroup).Items.Text
  else if AControl is TStringGrid then
    for y := 0 to (AControl as TStringGrid).RowCount - 1 do
    begin
      _AppendLine(Result, '');
      for x := 0 to (AControl as TStringGrid).ColCount - 1 do
      begin
        if x > (AControl as TStringGrid).Selection.Left then
          Result := Result + #9;
        Result := Result + (AControl as TStringGrid).Cells[x, y];
      end;
    end
  else
    LHandled := false;

  if Assigned(FOnGetControlText) then
    FOnGetControlText(AControl, Result, LHandled);

  if not LHandled then
    raise EGUIScriptUnhandledControlType.CreateFmt(
        'Unhandled control type for control ''%s''', [AControl.Name]);
end;

function TGUIScript.GetControlSelectedText(const AHwnd: HWND): string;
begin
  //TODO: Implement selected text
  result := GetControlText(AHwnd);
end;

function TGUIScript.GetControlSelectedText(const AControl: TControl): string;
var
  i, x, y: Integer;
  LHandled: Boolean;

  procedure _AppendLine(var AString: string; const AAppend: string);
  begin
    if AString <> '' then
      AString := AString + #13#10;
    AString := AString + AAppend;
  end;

begin
  Result := '';
  LHandled := true;
  if AControl is TCustomEdit then // TEdit, TMemo, etc
    Result := (AControl as TCustomEdit).SelText
  else if AControl is TCheckBox then
    Result := BoolToStr((AControl as TCheckBox).Checked, true {UseBoolStrs})
  else if AControl is TRadioButton then
    Result := BoolToStr((AControl as TRadioButton).Checked, true {UseBoolStrs})
  else if AControl is TButton then
    Result := (AControl as TButton).Caption
  else if AControl is TBitBtn then
    Result := (AControl as TBitBtn).Caption
  else if AControl is TSpeedButton then
    Result := (AControl as TSpeedButton).Caption
  else if AControl is TCustomLabel then
    Result := (AControl as TCustomLabel).Caption // TLabel
  else if AControl is TStaticText then
    Result := (AControl as TStaticText).Caption
  else if AControl is TComboBox then
    Result := (AControl as TComboBox).Text
  else if AControl is TForm then
    Result := (AControl as TForm).Caption
  else if AControl is TPanel then
    Result := (AControl as TPanel).Caption
  else if AControl is TGroupBox then
    Result := (AControl as TGroupBox).Caption
  else if AControl is TSplitter then
    Result := IntToStr((AControl as TSplitter).Left) + ',' + IntToStr((AControl as TSplitter).Top)
  else if AControl is TScrollBar then
    Result := IntToStr((AControl as TScrollBar).Position)
{$IFDEF MSWINDOWS}
  else if AControl is TPageControl then
  begin
    Result := IntToStr((AControl as TPageControl).TabIndex);
    if Assigned((AControl as TPageControl).ActivePage) then
      Result := Result + ': ' + (AControl as TPageControl).ActivePage.Caption;
  end
  else if AControl is TTabSheet then
    Result := (AControl as TTabSheet).Caption
  else if AControl is TRichEdit then
    Result := (AControl as TRichEdit).Text
  else if AControl is TDateTimePicker then
  begin
    if (AControl as TDateTimePicker).Kind = dtkDate then
      Result := DateToStr((AControl as TDateTimePicker).Date)
    else
      Result := TimeToStr((AControl as TDateTimePicker).Time);
  end
  else if AControl is TTrackBar then
    Result := IntToStr((AControl as TTrackBar).Position)
  else if AControl is TProgressBar then
    Result := IntToStr((AControl as TProgressBar).Position)
  else if AControl is TMonthCalendar then
    Result := DateToStr((AControl as TMonthCalendar).Date)
  else if AControl is TCalendar then
    Result := DateToStr((AControl as TCalendar).CalendarDate)
  else if AControl is TCustomTreeView then
    for i := 0 to (AControl as TCustomTreeView).SelectionCount - 1 do
      _AppendLine(Result, (AControl as TCustomTreeView).Selections[i].Text)
  else if AControl is TCustomListView then
  begin
    for i := 0 to (AControl as TCustomListView).Items.Count - 1 do
      if (AControl as TCustomListView).Items[i].Selected then
        _AppendLine(Result, (AControl as TCustomListView).Items[i].Caption);
  end
  else if AControl is TStatusBar then
    for i := 0 to (AControl as TStatusBar).Panels.Count - 1 do
      _AppendLine(Result, (AControl as TStatusBar).Panels.Items[i].Text)
  else if AControl is TPaintBox then
    Result := IntToStr((AControl as TPaintBox).Width) + ',' + IntToStr((AControl as TPaintBox).Height)
{$ENDIF}
  else if AControl is TImage then
    Result := IntToStr((AControl as TImage).Width) + ',' + IntToStr((AControl as TImage).Height)
  else if AControl is TListBox then
  begin
    for i := 0 to (AControl as TListBox).Items.Count - 1 do
      if (AControl as TListBox).Selected[i] then
        _AppendLine(Result, (AControl as TListBox).Items.Strings[i]);
  end
  else if AControl is TCheckListBox then
  begin
    for i := 0 to (AControl as TCheckListBox).Items.Count - 1 do
      if (AControl as TCheckListBox).Checked[i] then
        _AppendLine(Result, (AControl as TCheckListBox).Items.Strings[i]);
  end
  else if AControl is TRadioGroup then
  begin
    if (AControl as TRadioGroup).ItemIndex <> -1 then
      Result := (AControl as TRadioGroup).Items.Strings[
          (AControl as TRadioGroup).ItemIndex];
  end
  else if AControl is TStringGrid then
  begin
    if (AControl as TStringGrid).Selection.Left <> -1 then
      for y := (AControl as TStringGrid).Selection.Top to
          (AControl as TStringGrid).Selection.Bottom do
      begin
        _AppendLine(Result, '');
        for x := (AControl as TStringGrid).Selection.Left to
            (AControl as TStringGrid).Selection.Right do
        begin
          if x > (AControl as TStringGrid).Selection.Left then
            Result := Result + #9;
          Result := Result + (AControl as TStringGrid).Cells[x, y];
        end;
      end;
  end
  else
    LHandled := false;

  if Assigned(FOnGetControlSelectedText) then
    FOnGetControlSelectedText(AControl, Result, LHandled);

  if not LHandled then
    raise EGUIScriptUnhandledControlType.CreateFmt(
        'Unhandled control type for control ''%s''', [AControl.Name]);
end;

function TGUIScript.GetControlList(const AControl: TControl): string;
var
  LControlList: TStringList;

  procedure _GetControlList(const AControl: TControl; AControlList: TStringList;
    const ALevel: Integer; const AChildIndex: Integer);
  var
    i: Integer;
    LWinControl: TWinControl;
    LLine: string;
  begin
    LLine := DupeString(FControlInspectionIndent, ALevel);
    if AControl.Name <> '' then
      LLine := LLine + AControl.Name + ' (' + AControl.ClassName + ')'
    else
    begin
      if AChildIndex <> -1 then
        LLine := LLine + '[' + IntToStr(AChildIndex) + '] ';
      LLine := LLine + '(' + AControl.ClassName + ')';
    end;
    AControlList.Add(LLine);
    // Recurse to get child controls
    if AControl is TWinControl then
    begin
      LWinControl := AControl as TWinControl;
      for i := 0 to LWinControl.ControlCount - 1 do
        if LWinControl.Controls[i] is TControl then
          _GetControlList(LWinControl.Controls[i], AControlList, ALevel + 1, i);
    end;
  end;

begin
  LControlList := TStringList.Create;
  try
    _GetControlList(AControl, LControlList, 0, -1);
    Result := LControlList.Text;
  finally
    LControlList.Free;
  end;
end;

procedure TGUIScript.RegisterTextControl(AControlClass: TControlClass);
begin
  SetLength(FTextControls, Length(FTextControls) + 1);
  FTextControls[Length(FTextControls) - 1] := AControlClass;
end;

function TGUIScript.IsTextControl(const AControl: TControl): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Length(FTextControls) - 1 do
    if AControl is FTextControls[i] then
    begin
      result := true;
      break;
    end;
end;

function TGUIScript.FormatScriptCheckCommand(const ACommandName: string;
  const AControlName: string; const AParam: string): string;
begin
  Result := FControlInspectionIndent + ACommandName + '(''' + AControlName + '''';
  if AParam <> '' then
    Result := Result + ', ' + AParam;
  Result := Result + ');';
end;

function TGUIScript.GenerateCheckControlTextCommand(const AText: string;
  const AControlName: string; const ACheckAllText: Boolean): string;
var
  LText: string;
begin
  if AControlName = '' then
    raise Exception.Create('Error Message');

  LText := AText;
  // Escape single quotes
  LText :=  StringReplace(LText, '''', '''''', [rfReplaceAll]);
  // Encode tabs
  LText := StringReplace(LText, #9, '''#9''', [rfReplaceAll]);
  // Split into nice Pascal multi-line string
  LText := StringReplace(LText, #13#10,
      '''#13#10 +'#13#10 +
      '    ''', [rfReplaceAll]);

  if ACheckAllText then
    Result := FormatScriptCheckCommand('CheckControlTextEqual', AControlName, '''' + LText + '''')
  else
    Result := FormatScriptCheckCommand('CheckControlSelectedTextEqual', AControlName, '''' + LText + '''');
end;

function TGUIScript.GenerateCheckControlTextCommand(const AHwnd: HWND;
  const AControlName: string; const ACheckAllText: Boolean): string;
var
  LText: string;
begin
  Assert(AHwnd <> 0, 'AHwnd must be assigned');
  try
    if ACheckAllText then
      LText := GetControlText(AHwnd)
    else
      LText := GetControlSelectedText(AHwnd);
    Result := GenerateCheckControlTextCommand(LText, AControlName, ACheckAllText);
  except
    on e: EGUIScriptUnhandledControlType do
      Result := '';
  end;
end;

function TGUIScript.GenerateCheckControlTextCommand(const AControl: TControl;
  const AControlName: string; const ACheckAllText: Boolean): string;
var
  LText: string;
begin
  Assert(AControl <> nil, 'AControl must be assigned');
  try
    if ACheckAllText then
      LText := GetControlText(AControl)
    else
      LText := GetControlSelectedText(AControl);
    Result := GenerateCheckControlTextCommand(LText, AControlName, ACheckAllText);
  except
    on e: EGUIScriptUnhandledControlType do
      Result := '';
  end;
end;

function TGUIScript.GetControlStatesScript(const AControl: TControl): string;
var
  LControlStateScript: TStringList;

  function _CheckState(const AState: boolean): string;
  begin
    if AState then
      Result := ''
    else
      Result := 'Not';
  end;

  procedure _GetControlStates(const AControl: TControl; AScript: TStringList);
  var
    i: Integer;
    LWinControl: TWinControl;
    LCheckAllText: Boolean;
    LCommand: string;
  begin
    if AControl.Name <> '' then
    begin
      if AScript.Count > 0 then
        AScript.Add('');

      AScript.Add(FormatScriptCheckCommand('CheckControlExists', AControl.Name));
      AScript.Add(FormatScriptCheckCommand('CheckControl' + _CheckState(AControl.Visible) + 'Visible', AControl.Name));
      AScript.Add(FormatScriptCheckCommand('CheckControl' + _CheckState(AControl.Enabled) + 'Enabled', AControl.Name));
      if (AControl is TWinControl) and (AControl as TWinControl).Focused then
        AScript.Add(FormatScriptCheckCommand('CheckControlFocused', AControl.Name));
      // Control text (all or selected)
      // Generally better to check entire text but the text from some controls
      // like grids, treeviews and list views can be huge so we only get the
      // full text of edit/text controls like edits and memos.
      LCheckAllText := IsTextControl(AControl);
      LCommand := GenerateCheckControlTextCommand(AControl, AControl.Name, LCheckAllText);
      if LCommand <> '' then
        AScript.Add(LCommand);

      // Recurse to get child controls
      if AControl is TWinControl then
      begin
        LWinControl := AControl as TWinControl;
        for i := 0 to LWinControl.ControlCount - 1 do
          if LWinControl.Controls[i] is TControl then
            _GetControlStates(LWinControl.Controls[i], AScript)
      end;
    end;
  end;

begin
  LControlStateScript := TStringList.Create;
  try
    _GetControlStates(AControl, LControlStateScript);
    Result := LControlStateScript.Text;
  finally
    LControlStateScript.Free;
  end;
end;

function TGUIScript.GetActionList: string;
var
  LActionList: TStringList;

  procedure _GetActionList(const AComponent: TComponent;
    AActionList: TStringList);
  var
    i: Integer;
  begin
    // Include named action lists and named actions in named action lists
    if (AComponent is TCustomActionList) and (AComponent.Name <> '') then
      AActionList.Add(AComponent.Name)
    else if (AComponent is TCustomAction) and (AComponent.Name <> '') and
        Assigned(TCustomAction(AComponent).ActionList) and
        (TCustomAction(AComponent).ActionList.Name <> '') then
      AActionList.Add(FControlInspectionIndent + AComponent.Name);

    // Recurse to find all action lists and actions
    for i := 0 to AComponent.ComponentCount - 1 do
      _GetActionList(AComponent.Components[i], AActionList);
  end;

begin
  LActionList := TStringList.Create;
  try
    _GetActionList(Screen.ActiveForm, LActionList);
    Result := LActionList.Text;
  finally
    LActionList.Free;
  end;
end;

function TGUIScript.GetActionStatesScript: string;
var
  LActionStateScript: TStringList;

  function _CheckState(const AState: boolean): string;
  begin
    if AState then
      Result := ''
    else
      Result := 'Not';
  end;

  procedure _GetActionStates(const AComponent: TComponent; AScript: TStringList);
  var
    i: Integer;
  begin
    // Include named actions in named action lists
    if (AComponent is TCustomAction) and (AComponent.Name <> '') and
        Assigned(TCustomAction(AComponent).ActionList) and
        (TCustomAction(AComponent).ActionList.Name <> '') then
    begin
      if AScript.Count > 0 then
        AScript.Add('');
      AScript.Add(FormatScriptCheckCommand('CheckActionExists', AComponent.Name));
      AScript.Add(FormatScriptCheckCommand(
          'CheckAction' + _CheckState(TCustomAction(AComponent).Visible) + 'Visible',
          AComponent.Name));
      AScript.Add(FormatScriptCheckCommand(
          'CheckAction' + _CheckState(TCustomAction(AComponent).Enabled) + 'Enabled',
          AComponent.Name));
    end;

    // Recurse to find all actions
    for i := 0 to AComponent.ComponentCount - 1 do
      _GetActionStates(AComponent.Components[i], AScript);
  end;

begin
  LActionStateScript := TStringList.Create;
  try
    _GetActionStates(Screen.ActiveForm, LActionStateScript);
    Result := LActionStateScript.Text;
  finally
    LActionStateScript.Free;
  end;
end;

procedure TGUIScript.ControlExistsEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.ControlExists(ControlName(Info));
end;

procedure TGUIScript.ControlVisibleEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.ControlVisible(ControlName(Info));
end;

procedure TGUIScript.ControlEnabledEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := FGUIAutomation.ControlEnabled(ControlName(Info));
end;

procedure TGUIScript.ControlNameEval(Info: TProgramInfo);
begin
  Info.ResultAsString := Control(Info).Name;
end;

procedure TGUIScript.ChildControlCountEval(Info: TProgramInfo);
var
  LControl: TControl;
begin
  LControl := Control(Info);
  if LControl is TWinControl then
    Info.ResultAsInteger := (LControl as TWinControl).ControlCount
  else
    Info.ResultAsInteger := 0;
end;

procedure TGUIScript.ControlTextEval(Info: TProgramInfo);
begin
  Info.ResultAsString := GetControlText(Control(Info));
end;

procedure TGUIScript.ControlSelectedTextEval(Info: TProgramInfo);
begin
  Info.ResultAsString := GetControlSelectedText(Control(Info));
end;

function TGUIScript.ActionName(const AInfo: TProgramInfo): string;
begin
  result := AInfo.ValueAsString['ActionName'];
end;

function TGUIScript.ActionExists(const AActionName: string): Boolean;
var
  LComponent: TComponent;
begin
  LComponent := FindComponentNested(Screen.ActiveForm, AActionName);
  Result := Assigned(LComponent) and (LComponent is TCustomAction);
end;

function TGUIScript.ActionVisible(const AActionName: string): Boolean;
var
  LComponent: TComponent;
begin
  LComponent := FindComponentNested(Screen.ActiveForm, AActionName);
  Result := Assigned(LComponent) and (LComponent is TCustomAction) and
      TCustomAction(LComponent).Visible;
end;

function TGUIScript.ActionEnabled(const AActionName: string): Boolean;
var
  LComponent: TComponent;
begin
  LComponent := FindComponentNested(Screen.ActiveForm, AActionName);
  Result := Assigned(LComponent) and (LComponent is TCustomAction) and
      TCustomAction(LComponent).Enabled;
end;

procedure TGUIScript.ActionExistsEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := ActionExists(ActionName(Info));
end;

procedure TGUIScript.ActionVisibleEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := ActionVisible(ActionName(Info));
end;

procedure TGUIScript.ActionEnabledEval(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := ActionEnabled(ActionName(Info));
end;

function TGUIScript.FormatCheckFailMessage(Info: TProgramInfo;
  const AMessageSuffix: string): string;
begin
  Result := Info.ValueAsString['FailMessage'];
  if Result <> '' then
    Result := Result + ': ';
  Result := Result + AMessageSuffix;
end;

procedure TGUIScript.Check(const ACheckResult: Boolean; const AFailMessage: string);
begin
  if not ACheckResult then
    raise EGUIScriptCheckFail.Create(AFailMessage);
end;

procedure TGUIScript.FailEval(Info: TProgramInfo);
begin
  Check(false, Format('Fail: %s', [Info.ValueAsString['FailMessage']]));
end;

procedure TGUIScript.CheckEqualsEval(Info: TProgramInfo);
var
  LExpected: string;
  LActual: string;
begin
  LExpected := Info.ValueAsString['ExpectedValue'];
  LActual := Info.ValueAsString['ActualValue'];
  Check(LActual = LExpected, Format('Expected <%s> actual <%s>', [LExpected, LActual]));
end;

procedure TGUIScript.CheckTrueEval(Info: TProgramInfo);
begin
  Check(Info.ValueAsBoolean['Value'],
      FormatCheckFailMessage(Info, 'Expected true but was false'));
end;

procedure TGUIScript.CheckFalseEval(Info: TProgramInfo);
begin
  Check(not Info.ValueAsBoolean['Value'],
      FormatCheckFailMessage(Info, 'Expected false but was true'));
end;

procedure TGUIScript.CheckNotEqualsEval(Info: TProgramInfo);
var
  LNotExpected: string;
  LActual: string;
begin
  LNotExpected := Info.ValueAsString['NotExpectedValue'];
  LActual := Info.ValueAsString['ActualValue'];
  Check(LActual <> LNotExpected,
      FormatCheckFailMessage(Info, Format('Actual <%s>', [LActual])));
end;

procedure TGUIScript.CheckFilesEqualEval(Info: TProgramInfo);
var
  LFileName1: string;
  LFileName2: string;
  LContents1: string;
  LContents2: string;
begin
  LFileName1 := Info.ValueAsString['FileName1'];
  LFileName2 := Info.ValueAsString['FileName2'];
  LContents1 := FileToString(LFileName1);
  LContents2 := FileToString(LFileName2);
  Check(LContents1 = LContents2,
      FormatCheckFailMessage(Info, Format('File ''%s'' equals file ''%s''',
          [LFileName1, LFileName2])));
end;

procedure TGUIScript.CheckFilesNotEqualEval(Info: TProgramInfo);
var
  LFileName1: string;
  LFileName2: string;
  LContents1: string;
  LContents2: string;
begin
  LFileName1 := Info.ValueAsString['FileName1'];
  LFileName2 := Info.ValueAsString['FileName2'];
  LContents1 := FileToString(LFileName1);
  LContents2 := FileToString(LFileName2);
  Check(LContents1 <> LContents2,
      FormatCheckFailMessage(Info, Format('File ''%s'' not equals file ''%s''',
          [LFileName1, LFileName2])));
end;

procedure TGUIScript.CheckControlEnabledEval(Info: TProgramInfo);
begin
  //TODO: Support non-VCL controls (HWND)
  Check(Control(Info).Enabled,
      FormatCheckFailMessage(Info, Format('Control ''%s'' enabled',
          [ControlName(Info)])));
end;

procedure TGUIScript.CheckControlNotEnabledEval(Info: TProgramInfo);
begin
  //TODO: Support non-VCL controls (HWND)
  Check(not Control(Info).Enabled,
      FormatCheckFailMessage(Info, Format('Control ''%s'' not enabled',
          [ControlName(Info)])));
end;

procedure TGUIScript.CheckControlVisibleEval(Info: TProgramInfo);
begin
  //TODO: Support non-VCL controls (HWND)
  Check(Control(Info).Visible,
      FormatCheckFailMessage(Info, Format('Control ''%s'' visible',
          [ControlName(Info)])));
end;

procedure TGUIScript.CheckControlNotVisibleEval(Info: TProgramInfo);
begin
  //TODO: Support non-VCL controls (HWND)
  Check(not Control(Info).Visible,
      FormatCheckFailMessage(Info, Format('Control ''%s'' not visible',
          [ControlName(Info)])));
end;

procedure TGUIScript.CheckControlExistsEval(Info: TProgramInfo);
begin
  try
    //TODO: Support non-VCL controls (HWND)
    Control(Info);
  except
    on e: EGUIAutomationControlNotFound do
      Check(false,
          FormatCheckFailMessage(Info, Format('Control ''%s'' exists',
              [ControlName(Info)])));
  end;
end;

procedure TGUIScript.CheckControlNotExistsEval(Info: TProgramInfo);
begin
  try
    //TODO: Support non-VCL controls (HWND)
    Control(Info);
    Check(false,
        FormatCheckFailMessage(Info, Format('Control ''%s'' not exists',
            [ControlName(Info)])));
  except
    on e: EGUIAutomationControlNotFound do;
  end;
end;

procedure TGUIScript.CheckControlFocusedEval(Info: TProgramInfo);
var
  LControl: TWinControl;
begin
  //TODO: Support non-VCL controls (HWND)
  LControl := Control(Info) as TWinControl;
  Check(Assigned(LControl) and LControl.Focused,
      FormatCheckFailMessage(Info, Format('Control ''%s'' focused',
          [ControlName(Info)])));
end;

procedure TGUIScript.CheckControlTextEqualEval(Info: TProgramInfo);
var
  LExpected: string;
  LActual: string;
begin
  LExpected := Info.ValueAsString['Text'];
  //TODO: Support non-VCL controls (HWND)
  LActual := GetControlText(Control(Info));
  Check(LExpected = LActual,
      FormatCheckFailMessage(Info, Format('Control ''%s'' text expected <%s> actual <%s>',
          [ControlName(Info), LExpected, LActual])));
end;

procedure TGUIScript.CheckControlSelectedTextEqualEval(Info: TProgramInfo);
var
  LExpected: string;
  LActual: string;
begin
  LExpected := Info.ValueAsString['Text'];
  //TODO: Support non-VCL controls (HWND)
  LActual := GetControlSelectedText(Control(Info));
  Check(LExpected = LActual,
      FormatCheckFailMessage(Info, Format('Control ''%s'' selected text expected <%s> actual <%s>',
          [ControlName(Info), LExpected, LActual])));
end;

procedure TGUIScript.CheckActionExistsEval(Info: TProgramInfo);
begin
  Check(ActionExists(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' exists',
          [ActionName(Info)])));
end;

procedure TGUIScript.CheckActionNotExistsEval(Info: TProgramInfo);
begin
  Check(not ActionExists(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' not exists',
          [ActionName(Info)])));
end;

procedure TGUIScript.CheckActionVisibleEval(Info: TProgramInfo);
begin
  Check(ActionVisible(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' visible',
          [ActionName(Info)])));
end;

procedure TGUIScript.CheckActionNotVisibleEval(Info: TProgramInfo);
begin
  Check(not ActionVisible(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' not visible',
          [ActionName(Info)])));
end;

procedure TGUIScript.CheckActionEnabledEval(Info: TProgramInfo);
begin
  Check(ActionEnabled(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' enabled',
          [ActionName(Info)])));
end;

procedure TGUIScript.CheckActionNotEnabledEval(Info: TProgramInfo);
begin
  Check(not ActionEnabled(ActionName(Info)),
      FormatCheckFailMessage(Info, Format('Action ''%s'' not enabled',
          [ActionName(Info)])));
end;

end.

