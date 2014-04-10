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

//TODO: Add dwScript debugger support
//TODO: Add LoadStringFromFile and SaveStringToFile functions
//TODO: Add a function to retrieve the value of any property of a control by property name
//TODO: Bridge to model/objects/data presented in the UI

{$I DUnit.inc}

unit GUIScript;

interface

uses
  SysUtils, Controls, GUIActionRecorder, GUIAutomation, dwsExprs, dwsCompiler,
  dwsVCLGUIFunctions, dwsComp;

const
  rcs_id: string = '#(@)$Id$';

type
  TGUIScript = class;

  TOnExecutionStateEvent = procedure(AScript: TGUIScript) of object;

  TGUIScript = class(TObject)
  private
    FGUIAutomation: TGUIAutomation;
    FDWScript: TDelphiWebScript;
    FUnit: TdwsUnit;
    FScriptSource: string;
    FScriptResult: string;
    FTerminated: Boolean;
    FRunSuccessful: Boolean;
    FContinueExecution: Boolean;
    FOnExecutionStarted: TOnExecutionStateEvent;
    FOnExecutionEnded: TOnExecutionStateEvent;
    function GetRecordedScript: string;
    procedure RunScript;
    procedure TerminateScript(Info: TProgramInfo);
    procedure AutomationContinueExecution(var AContinueExecution: boolean);
    procedure ScriptExecutionStarted(exec : TdwsProgramExecution);
    procedure ScriptExecutionEnded(exec : TdwsProgramExecution);

    function ControlName(Info: TProgramInfo): string;
    function Control(const AControlName: string): TControl; overload;
    function Control(Info: TProgramInfo): TControl; overload;
    function GetControlText(const AControlName: string): string;
    procedure Check(const ACheckResult: Boolean; const AErrorMessage: string);

    procedure SleepEval(Info: TProgramInfo);
    procedure RunScriptEval(Info: TProgramInfo);
    procedure TerminateScriptEval(Info: TProgramInfo);
    procedure EnterTextIntoEval(Info: TProgramInfo);
    procedure EnterKeyIntoEval(Info: TProgramInfo);
    procedure EnterKeyEval(Info: TProgramInfo);
    procedure LeftClickEval(Info: TProgramInfo);
    procedure LeftClickAtEval(Info: TProgramInfo);
    procedure LeftClickControlEval(Info: TProgramInfo);
    procedure LeftDoubleClickEval(Info: TProgramInfo);
    procedure LeftDoubleClickAtEval(Info: TProgramInfo);
    procedure LeftDoubleClickControlEval(Info: TProgramInfo);
    procedure RightClickEval(Info: TProgramInfo);
    procedure RightClickAtEval(Info: TProgramInfo);
    procedure RightClickControlEval(Info: TProgramInfo);
    procedure RightDoubleClickEval(Info: TProgramInfo);
    procedure RightDoubleClickAtEval(Info: TProgramInfo);
    procedure RightDoubleClickControlEval(Info: TProgramInfo);
    procedure ControlTextEval(Info: TProgramInfo);
    procedure FailEval(Info: TProgramInfo);
    procedure CheckEqualsEval(Info: TProgramInfo);
    procedure CheckNotEqualsEval(Info: TProgramInfo);
    procedure CheckEnabledEval(Info: TProgramInfo);
    procedure CheckNotEnabledEval(Info: TProgramInfo);
    procedure CheckVisibleEval(Info: TProgramInfo);
    procedure CheckNotVisibleEval(Info: TProgramInfo);
    procedure CheckFocusedEval(Info: TProgramInfo);
    procedure CheckControlTextEqualEval(Info: TProgramInfo);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Retrieve script for recorded GUI actions
    property RecordedScript: string read GetRecordedScript;
    // Play back script and get result
    function Execute(const AScript: string): Boolean;
    procedure StopExecution;

    property OnExecutionStarted: TOnExecutionStateEvent read FOnExecutionStarted write FOnExecutionStarted;
    property OnExecutionEnded: TOnExecutionStateEvent read FOnExecutionEnded write FOnExecutionEnded;
    property ScriptResult: string read FScriptResult;
    property Terminated: boolean read FTerminated;
    property DWScript: TDelphiWebScript read FDWScript;
  end;

  EGUIScript = class(Exception);
  EGUIScriptCheckFail = class(EGUIScript);

implementation

uses
  Classes,
  TypInfo,
  StdCtrls,
  Buttons,
  Windows,
  SyncObjs;

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

  procedure _AddParameter(const AFunction: TdwsFunction; const AName: string; const AType: string);
  var
    LParameter: TdwsParameter;
  begin
    LParameter := AFunction.Parameters.Add;
    LParameter.Name := AName;
    LParameter.DataType := AType;
  end;

begin
  inherited;

  FGUIAutomation := TGUIAutomation.Create;
  FGUIAutomation.OnGetContinueExecution := AutomationContinueExecution;
  FDWScript := TDelphiWebScript.Create(nil);
  FDWScript.OnExecutionStarted := ScriptExecutionStarted;
  FDWScript.OnExecutionEnded := ScriptExecutionEnded;
  FUnit := TdwsUnit.Create(nil);
  FUnit.UnitName := 'GUIScriptExecution';
  FUnit.StaticSymbols := False;
  FUnit.Script := FDWScript;

  // Constants

  _AddConstant('VK_TAB', 'Integer', Ord(VK_TAB));
  _AddConstant('VK_BACK', 'Integer', Ord(VK_BACK));
  _AddConstant('VK_HOME', 'Integer', Ord(VK_HOME));
  _AddConstant('VK_END', 'Integer', Ord(VK_END));
  _AddConstant('VK_LEFT', 'Integer', Ord(VK_LEFT));
  _AddConstant('VK_UP', 'Integer', Ord(VK_UP));
  _AddConstant('VK_RIGHT', 'Integer', Ord(VK_RIGHT));
  _AddConstant('VK_DOWN', 'Integer', Ord(VK_DOWN));
  _AddConstant('VK_INSERT', 'Integer', Ord(VK_INSERT));
  _AddConstant('VK_DELETE', 'Integer', Ord(VK_DELETE));
  _AddConstant('VK_ESCAPE', 'Integer', Ord(VK_ESCAPE));

  // General methods

  LFunction := _AddFunction('Sleep', SleepEval);
  _AddParameter(LFunction, 'Milliseconds', 'Integer');

  // General script control methods

  LFunction := _AddFunction('RunScript', RunScriptEval);
  _AddParameter(LFunction, 'FileName', 'String');

  _AddFunction('TerminateScript', TerminateScriptEval);

  // GUI automation methods

  LFunction := _AddFunction('EnterTextInto', EnterTextIntoEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Text', 'String');

  LFunction := _AddFunction('EnterKeyInto', EnterKeyIntoEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Key', 'Integer');
  _AddParameter(LFunction, 'ShiftState', 'String');

  LFunction := _AddFunction('EnterKey', EnterKeyEval);
  _AddParameter(LFunction, 'Key', 'Integer');
  _AddParameter(LFunction, 'ShiftState', 'String');

  LFunction := _AddFunction('LeftClick', LeftClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftClickAt', LeftClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftClickControl', LeftClickControlEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('LeftDoubleClick', LeftDoubleClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftDoubleClickAt', LeftDoubleClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('LeftDoubleClickControl', LeftDoubleClickControlEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('RightClick', RightClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightClickAt', RightClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightClickControl', RightClickControlEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('RightDoubleClick', RightDoubleClickEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightDoubleClickAt', RightDoubleClickAtEval);
  _AddParameter(LFunction, 'X', 'Integer');
  _AddParameter(LFunction, 'Y', 'Integer');

  LFunction := _AddFunction('RightDoubleClickControl', RightDoubleClickControlEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  // GUI inspection

  LFunction := _AddFunction('ControlText', ControlTextEval);
  LFunction.ResultType := 'String';
  _AddParameter(LFunction, 'ControlName', 'String');

  // General testing check methods

  LFunction := _AddFunction('Fail', FailEval);
  _AddParameter(LFunction, 'ErrorMessage', 'String');

  LFunction := _AddFunction('CheckEquals', CheckEqualsEval);
  _AddParameter(LFunction, 'ExpectedValue', 'String');
  _AddParameter(LFunction, 'ActualValue', 'String');

  LFunction := _AddFunction('CheckNotEquals', CheckNotEqualsEval);
  _AddParameter(LFunction, 'NotExpectedValue', 'String');
  _AddParameter(LFunction, 'ActualValue', 'String');

  // GUI testing check methods

  LFunction := _AddFunction('CheckEnabled', CheckEnabledEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('CheckNotEnabled', CheckNotEnabledEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('CheckVisible', CheckVisibleEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('CheckNotVisible', CheckNotVisibleEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('CheckFocused', CheckFocusedEval);
  _AddParameter(LFunction, 'ControlName', 'String');

  LFunction := _AddFunction('CheckControlTextEqual', CheckControlTextEqualEval);
  _AddParameter(LFunction, 'ControlName', 'String');
  _AddParameter(LFunction, 'Text', 'String');
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
  LExec: IdwsProgramExecution;
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
      LExec := LProgram.Execute;
      if LExec.Msgs.HasErrors then
        FScriptResult := LExec.Msgs.AsInfo
      else
      begin
        FScriptResult := LExec.Result.ToString;
        FRunSuccessful := true;
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

function TGUIScript.ControlName(Info: TProgramInfo): string;
begin
  Result := Info.ValueAsString['ControlName'];
end;

function TGUIScript.Control(const AControlName: string): TControl;
begin
  Result := FGUIAutomation.FindControl(AControlName);
end;

procedure TGUIScript.AutomationContinueExecution(
  var AContinueExecution: boolean);
begin
  AContinueExecution := FContinueExecution;
end;

function TGUIScript.Control(Info: TProgramInfo): TControl;
begin
  Result := Control(ControlName(Info));
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

procedure TGUIScript.SleepEval(Info: TProgramInfo);
begin
  FGUIAutomation.SyncSleep(Info.ValueAsInteger['Milliseconds']);
end;

procedure TGUIScript.StopExecution;
begin
  FContinueExecution := false;
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
      LShiftState);
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

procedure TGUIScript.LeftClickControlEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftClick(ControlName(Info));
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

procedure TGUIScript.LeftDoubleClickControlEval(Info: TProgramInfo);
begin
  FGUIAutomation.LeftDoubleClick(ControlName(Info));
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

procedure TGUIScript.RightClickControlEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightClick(ControlName(Info));
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

procedure TGUIScript.RightDoubleClickControlEval(Info: TProgramInfo);
begin
  FGUIAutomation.RightDoubleClick(ControlName(Info));
end;

function TGUIScript.GetControlText(const AControlName: string): string;
var
  LControl: TControl;
begin
  LControl := Control(AControlName);
  if LControl is TCustomEdit then
    Result := (LControl as TCustomEdit).Text
  else if LControl is TCheckBox then
    Result := BoolToStr((LControl as TCheckBox).Checked, true {UseBoolStrs})
  else if LControl is TRadioButton then
    Result := BoolToStr((LControl as TRadioButton).Checked, true {UseBoolStrs})
  else if LControl is TButton then
    Result := (LControl as TButton).Caption
  else if LControl is TBitBtn then
    Result := (LControl as TBitBtn).Caption
  else if LControl is TSpeedButton then
    Result := (LControl as TSpeedButton).Caption
  else if LControl is TLabel then
    Result := (LControl as TLabel).Caption
  else if LControl is TComboBox then
    Result := (LControl as TComboBox).Text
  else if LControl is TListBox then
    Result := (LControl as TListBox).Items.Text
  else
    raise EGUIScript.CreateFmt('Unhandled control type for control ''%s''', [AControlName]);
end;

procedure TGUIScript.ControlTextEval(Info: TProgramInfo);
begin
  Info.ResultAsString := GetControlText(ControlName(Info));
end;

procedure TGUIScript.Check(const ACheckResult: Boolean; const AErrorMessage: string);
begin
  if not ACheckResult then
    raise EGUIScriptCheckFail.Create(AErrorMessage);
end;

procedure TGUIScript.FailEval(Info: TProgramInfo);
begin
  Check(false, Format('Fail: %s', [Info.ValueAsString['ErrorMessage']]));
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

procedure TGUIScript.CheckNotEqualsEval(Info: TProgramInfo);
var
  LNotExpected: string;
  LActual: string;
begin
  LNotExpected := Info.ValueAsString['NotExpectedValue'];
  LActual := Info.ValueAsString['ActualValue'];
  Check(LActual <> LNotExpected, Format('Actual <%s>', [LActual]));
end;

procedure TGUIScript.CheckEnabledEval(Info: TProgramInfo);
begin
  Check(Control(Info).Enabled, Format('Control ''%s'' enabled', [ControlName(Info)]));
end;

procedure TGUIScript.CheckNotEnabledEval(Info: TProgramInfo);
begin
  Check(not Control(Info).Enabled, Format('Control ''%s'' not enabled', [ControlName(Info)]));
end;

procedure TGUIScript.CheckVisibleEval(Info: TProgramInfo);
begin
  Check(Control(Info).Visible, Format('Control ''%s'' visible', [ControlName(Info)]));
end;

procedure TGUIScript.CheckNotVisibleEval(Info: TProgramInfo);
begin
  Check(not Control(Info).Visible, Format('Control ''%s'' not visible', [ControlName(Info)]));
end;

procedure TGUIScript.CheckFocusedEval(Info: TProgramInfo);
var
  LControl: TWinControl;
begin
  LControl := Control(Info) as TWinControl;
  Check(Assigned(LControl) and LControl.Focused, Format('Control ''%s'' focused', [ControlName(Info)]));
end;

procedure TGUIScript.CheckControlTextEqualEval(Info: TProgramInfo);
var
  LExpected: string;
  LActual: string;
begin
  LExpected := Info.ValueAsString['Text'];
  LActual := GetControlText(ControlName(Info));
  Check(LExpected = LActual, Format('ControlText Expected <%s> actual <%s>', [LExpected, LActual]));
end;

end.

