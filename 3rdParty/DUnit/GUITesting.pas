{ #(@)$Id: GUITesting.pas 98 2014-02-07 10:19:25Z jarrodh $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 98 $
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

{$I DUnit.inc}

unit GUITesting;

interface
uses
  TestFramework,
  TestFrameworkIfaces,
  GUIAutomation,

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
  rcs_id: string = '#(@)$Id: GUITesting.pas 98 2014-02-07 10:19:25Z jarrodh $';

type
  TGUITestCase = class(TTestCase, IGUITestCase)
  private
    FAutomation: TGUIAutomation;
    FCursorPos : TPoint;
    FGUI       : TControl; // this is the control we're testing

    function GetRecording: Boolean;
    procedure GetContinueExecution(var AContinueExecution: boolean);
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
{$IFDEF DELPHI2009_UP}
    procedure ThreadedExecute(AProc: TThreadProcedure);
{$ENDIF DELPHI2009_UP}
    function  GetGUI: TControl;
    procedure SetGUI(const AValue: TControl);
    procedure ShowGUI; virtual;
    procedure Show(OnOff :boolean = true); overload;
    procedure Hide; overload;
    function  GetFocused :TControl;
    function  IsFocused(Control : TControl) : boolean;

    procedure CheckTabTo(Control :TControl; Addrs :Pointer = nil); overload;
    procedure CheckTabTo(ControlName :string); overload;
    procedure CheckFocused(Control :TControl; Addrs :Pointer = nil); overload;
    procedure CheckFocused(ControlName :string); overload;
    procedure CheckEnabled(Control :TControl; Addrs :Pointer = nil); overload;
    procedure CheckEnabled(ControlName :string); overload;
    procedure CheckVisible(Control :TControl; Addrs :Pointer = nil);  overload;
    procedure CheckVisible(ControlName :string); overload;
    procedure CheckVisible; overload;

    property GUI :TControl read GetGUI write SetGUI;
    property Recording: Boolean read GetRecording;
    property Automation: TGUIAutomation read FAutomation;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;

{$IFDEF DELPHI2009_UP}
  TGUITestCase<T: TControl> = class(TGUITestCase)
  protected
    function  GetGUI: T;
    procedure SetGUI(const AValue: T);

    property GUI :T read GetGUI write SetGUI;
  end;
{$ENDIF DELPHI2009_UP}


implementation

uses
  GUIActionRecorder
  ,TestUtils
  ;

// assertions are always on so we can check for own consistency
{$ASSERTIONS ON}
// need stack frames to use CallerAddr
{$STACKFRAMES ON}

{ TGUITestCase }

constructor TGUITestCase.Create{(MethodName :string)};
begin
  inherited Create{(MethodName)};
  FAutomation := TGUIAutomation.Create;
  FAutomation.OnGetContinueExecution := GetContinueExecution;
end;

destructor TGUITestCase.Destroy;
begin
  FAutomation.Free;
  FGUI.Free;
  inherited Destroy;
end;

procedure TGUITestCase.SetUpOnce;
begin
  inherited;
  FGUI := nil;
end;

procedure TGUITestCase.SetUp;
begin
  inherited;
  FCursorPos := Mouse.CursorPos;
end;

procedure TGUITestCase.TearDown;
begin
  FreeAndNil(FGUI);
  // Restore cursor position
  Mouse.CursorPos := FCursorPos;
  inherited;
end;

{$IFDEF DELPHI2009_UP}
procedure TGUITestCase.ThreadedExecute(AProc: TThreadProcedure);
begin
  FAutomation.ThreadedExecute(AProc);
end;
{$ENDIF DELPHI2009_UP}

procedure TGUITestCase.GetContinueExecution(var AContinueExecution: boolean);
begin
  AContinueExecution := not FExecControl.HaltExecution;
end;

function TGUITestCase.GetGUI: TControl;
begin
  result := FGUI;
end;

procedure TGUITestCase.SetGUI(const AValue: TControl);
begin
  FGUI := AValue;
end;

function TGUITestCase.GetRecording: Boolean;
begin
  result := GGUIActionRecorder.Active;
end;

procedure TGUITestCase.ShowGUI;
begin
  if Recording and (GUI is TForm) then
    (GUI as TForm).ShowModal
  else
    GUI.Show;
end;

procedure TGUITestCase.Show(OnOff: boolean);
begin
  FAutomation.Show(GUI, OnOff);
end;

procedure TGUITestCase.Hide;
begin
  Screen.ActiveForm.Visible := false;
end;

function TGUITestCase.GetFocused: TControl;
begin
  Result := Screen.ActiveControl;
end;

function TGUITestCase.IsFocused(Control : TControl) : boolean;
begin
  Result := GetFocused = Control;
end;

procedure TGUITestCase.CheckTabTo(Control: TControl; Addrs :Pointer = nil);
var
  i :Integer;
begin
  if Addrs = nil then
    Addrs := CallerAddr;

  if not (Control is TWinControl) then
     Fail(
        Format('Expected a TWinControl, but %s is a %s',
               [Control.Name, Control.ClassName]),
               Addrs
        );
  if not TWinControl(Control).CanFocus then
      Fail(
        Format('Control %s:%s cannot focus', [Control.Name, Control.ClassName]),
        Addrs
        );

  for i := 1 to Screen.ActiveForm.ComponentCount do
  begin
    if FExecControl.HaltExecution then
      Exit; //==>
    if GetFocused = Control then
      Exit; //==>
    FAutomation.Tab;
  end;
  Fail(Format('Could not Tab to control "%s"', [Control.Name]), Addrs);
end;

procedure TGUITestCase.CheckFocused(Control: TControl; Addrs :Pointer);
var
  F :TControl;
begin
  Assert(Control <> nil, 'No control');

  if Addrs = nil then
    Addrs := CallerAddr;

  if not (Control is TWinControl) then
    Fail(
        Format('Expected a TWinControl, but %s is a %s',
               [Control.Name, Control.ClassName]),
        Addrs
        );
  if not TWinControl(Control).CanFocus then
     Fail(
        Format('Control %s cannot focus', [Control.ClassName]),
        Addrs
        );
  if (Control.Owner <> nil) and (Control.Owner is TCustomForm) then
    F := (Control.Owner as TCustomForm).ActiveControl
  else
    F := GetFocused;
  if  F <> Control then
  begin
    if F <> nil then
      Fail(Format('Expected control %s to have focus, but %s had it.', [Control.Name, F.Name]), Addrs)
    else
      Fail(Format('Expected control %s to have focus', [Control.Name]), Addrs);
  end
end;

procedure TGUITestCase.CheckFocused(ControlName: string);
begin
  CheckFocused(FAutomation.FindControl(ControlName, CallerAddr), CallerAddr);
end;

procedure TGUITestCase.CheckTabTo(ControlName: string);
begin
  CheckTabTo(FAutomation.FindControl(ControlName, CallerAddr), CallerAddr);
end;

procedure TGUITestCase.CheckEnabled(Control: TControl; Addrs :Pointer = nil);
begin
  if not Control.Enabled then
    Fail(Format('Expected control %s to be enabled', [Control.Name]), CallerAddr);
end;

procedure TGUITestCase.CheckEnabled(ControlName: string);
begin
  CheckEnabled(FAutomation.FindControl(ControlName, CallerAddr), CallerAddr);
end;

procedure TGUITestCase.CheckVisible(Control: TControl; Addrs: Pointer);
begin
  if not Control.Visible then
    Fail(Format('Expected control %s to be visible', [Control.Name]), CallerAddr);
end;

procedure TGUITestCase.CheckVisible(ControlName: string);
begin
  CheckVisible(FAutomation.FindControl(ControlName, CallerAddr), CallerAddr);
end;

procedure TGUITestCase.CheckVisible;
begin
  CheckVisible(GUI, CallerAddr);
end;

{ TGUITestCase<T> }

{$IFDEF DELPHI2009_UP}
function TGUITestCase<T>.GetGUI: T;
begin
  result := (inherited GetGUI) as T;
end;
{$ENDIF DELPHI2009_UP}

{$IFDEF DELPHI2009_UP}
procedure TGUITestCase<T>.SetGUI(const AValue: T);
begin
  inherited SetGUI(AValue);
end;
{$ENDIF DELPHI2009_UP}

end.

