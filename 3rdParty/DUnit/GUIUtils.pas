{ #(@)$Id: GUIUtils.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $ }
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
 *)

{$I DUnit.inc}

unit GUIUtils;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Controls,
{$ENDIF}
  SysUtils,
  Classes;

const
  rcs_id: string = '#(@)$Id: GUIUtils.pas,v 1.35 2010/05/04 09:55:00 jarrodh Exp $';

  // Control Names
  // -------------
  // If control name is numeric and preceded by the symbol '@':
  // - Return the immediate child <window> with that numeric index (0 based)
  //   (this supports non-VCL controls). If the first control in the hierarchy
  //   (see below) uses this syntax then the parent is the top-most window in
  //   the process
  // If control name is numeric:
  // - If the parent component is a TWinControl then return the immediate child
  //   VCL <control> with that numeric index (0 based)
  // If the control name is not numeric:
  // - Find a VCL <control> with the given name, recursing through child controls
  //   hierarchically in reverse order. This will find the last instance, which
  //   is probably the better default to handle the case where there are multiple
  //   controls with the same name (e.g. multiple instances of the same parented
  //   form). You can control this behavior (e.g. get "find first") if you use
  //   the hierarchical child control name feature below to find the correct
  //   parent before finding within its child controls.
  //
  // Control name can be specified hierarchically by separating the name at
  // each level with a slash (/). Examples:
  // - ControlA/ControlB : find control with name ControlA and then return its
  //   last child (found recursively as above) with name ControlB
  // - ControlA/3 : find control with name ControlA and then return its child
  //   control with index 3
  // - 3/4/1 : find child control with index 3, its child with index 4 (the
  //   grandchild) and return its child with index 1 (the great-grandchild)
  // - 3/ControlB : find child control with index 3 and return the last child
  //   (found recursively) with name ControlB
  // - @3/@1 : from topmost window find the child window with index 3 and return
  //   its child with index 1 (the grandchild)
const
  CChildControlNameToken = '/';
  CChildControlNameWindowToken = '@';

  function FindControlInstance(const AComp: TComponent;
      const AControlName: string): TControl; overload;
{$IFDEF MSWINDOWS}
  function FindControlInstance(const AParentHwnd: HWND;
      const AControlName: string; var AX: Integer; var AY: Integer): HWND; overload;
{$ENDIF}
  function FindParentWinControl(const AControl: TControl): TWinControl;
  function GetWinControl(var AControl: TControl; var AX: Integer;
      var AY: Integer; const AIsTargetControl: Boolean): boolean;
{$IFDEF MSWINDOWS}
  function WindowClassName(const AHwnd: HWND): string;
  // The window highest in the z-order for this process
  function GetTopmostWindow: HWND;
  procedure SaveScreenshot(const AHwnd: HWND; const AFileName: string);
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  Graphics,
  pngimage;
{$ENDIF}

function FindControlInstance(const AComp: TComponent;
  const AControlName: string): TControl;
var
  LControlName: string;
  LWinControl: TWinControl;
  i: Integer;
  LChildControlTokenPos: Integer;
  LChildControlSpecified: boolean;
  LChildControlName: string;
  LControlIndex: Integer;
begin
  Result := nil;

  // Look for child control reference and extract names
  LChildControlTokenPos := Pos(CChildControlNameToken, AControlName);
  LChildControlSpecified := LChildControlTokenPos > 0;
  if LChildControlSpecified then
  begin
    // First is the immediate control to find
    LControlName := Trim(Copy(AControlName, 1, LChildControlTokenPos - 1));
    // Remainder is the child control(s) (skip the token)
    LChildControlName := Trim(Copy(AControlName, LChildControlTokenPos + 1,
        Length(AControlName)));
    LChildControlSpecified := LChildControlName <> '';
  end
  else
  begin
    LControlName := Trim(AControlName);
    LChildControlName := '';
  end;

  // Control by index
  if TryStrToInt(LControlName, LControlIndex) then
  begin
    if AComp is TWinControl then
    begin
      LWinControl := AComp as TWinControl;
      if LControlIndex < LWinControl.ControlCount then
        result := LWinControl.Controls[LControlIndex];
    end;
  end
  else
  begin
    // Control by name, recurse into child controls
    if (AComp is TControl) and (UpperCase(AComp.Name) = UpperCase(LControlName)) then
      Result := AComp as TControl
    else
    begin
      if AComp is TWinControl then
      begin
        LWinControl := AComp as TWinControl;
        // Search in reverse to get last instance
        i := LWinControl.ControlCount - 1;
        while (Result = nil) and (i >= 0) do
        begin
          Result := FindControlInstance(LWinControl.Controls[i], LControlName);
          Dec(i);
        end;
      end;
    end;
  end;

  // Recurse to find the child control if specified
  if Assigned(Result) and LChildControlSpecified then
    Result := FindControlInstance(Result, LChildControlName);
end;

{$IFDEF MSWINDOWS}
type
  TWindowByIndex = record
    IndexToFind: Integer;
    CurrentIndex: Integer;
    ResultHwnd: HWND;
  end;
  PWindowByIndex = ^TWindowByIndex;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ChildWindowByIndexProc(AHwnd: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  LParams: PWindowByIndex;
begin
  LParams := PWindowByIndex(ALParam);
  if LParams^.CurrentIndex = LParams^.IndexToFind then
  begin
    LParams^.ResultHwnd := AHwnd;
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
function FindControlInstance(const AParentHwnd: HWND;
  const AControlName: string; var AX: Integer; var AY: Integer): HWND;
var
  LControlName: string;
  LControl: TControl;
  LWinControl: TWinControl;
  LChildControlTokenPos: Integer;
  LChildControlSpecified: boolean;
  LChildControlName: string;
  LIndex: Integer;

  function _ChildWindowByIndex(const AParentHwnd: HWND;
    const AChildIndex: Integer; var AX: Integer; var AY: Integer;
    const AIsTargetControl: boolean): HWND;
  var
    LParams: TWindowByIndex;
    LRect: TRect;
  begin
    if AParentHwnd <> 0 then
    begin
      LParams.ResultHwnd := 0;
      LParams.CurrentIndex := 0;
      LParams.IndexToFind := AChildIndex;
      EnumChildWindows(AParentHwnd, @ChildWindowByIndexProc, LPARAM(@LParams));
      result := LParams.ResultHwnd;
      // If the default position is specified then use the centre of the control
      if (result <> 0) and AIsTargetControl and (AX = -1) and (AY = -1) then
      begin
        LRect := Rect(0, 0, 0, 0);
        GetWindowRect(result, LRect);
        AX := LRect.Width div 2;
        AY := LRect.Height div 2;
      end;
    end
    else
      result := 0;
  end;

  function _FindByName(const AControl: TControl;
    const AControlName: string; var AX: Integer; var AY: Integer;
    const AIsTargetControl: boolean): HWND;
  var
    LWinControl: TWinControl;
    i: Integer;
    LPoint: TPoint;
  begin
    result := 0;
    if AControl is TWinControl then
      LWinControl := AControl as TWinControl
    else
      LWinControl := nil;

    if UpperCase(AControl.Name) = UpperCase(AControlName) then
    begin
      // If the default position is specified then use the centre of the control
      if AIsTargetControl and (AX = -1) and (AY = -1) then
      begin
        AX := AControl.Width div 2;
        AY := AControl.Height div 2;
      end;

      if LWinControl <> nil then
        result := LWinControl.Handle
      else
      begin
        // Use parent windowed control (translate co-ords)
        LWinControl := AControl.Parent;
        if LWinControl <> nil then
        begin
          result := LWinControl.Handle;
          LPoint := Point(AX, AY);
          LPoint := AControl.ClientToParent(LPoint, LWinControl);
          AX := LPoint.X;
          AY := LPoint.Y;
        end;
      end;
    end
    else if LWinControl <> nil then
    begin
      // Search in reverse to get last instance
      i := LWinControl.ControlCount - 1;
      while (Result = 0) and (i >= 0) do
      begin
        Result := _FindByName(LWinControl.Controls[i], AControlName, AX, AY, AIsTargetControl);
        Dec(i);
      end;
    end;
  end;

begin
  Result := 0;
  LControlName := Trim(AControlName);
  if LControlName = '' then
    Exit; //==>

  // Look for child control reference and extract names
  LChildControlTokenPos := Pos(CChildControlNameToken, LControlName);
  LChildControlSpecified := LChildControlTokenPos > 0;
  if LChildControlSpecified then
  begin
    // Remainder is the child control(s) (skip the token)
    LChildControlName := Trim(Copy(LControlName, LChildControlTokenPos + 1,
        Length(LControlName)));
    // First is the immediate control to find
    LControlName := Trim(Copy(LControlName, 1, LChildControlTokenPos - 1));
    LChildControlSpecified := LChildControlName <> '';
  end
  else
    LChildControlName := '';

  // Find by child window index
  if LControlName[1] = CChildControlNameWindowToken then
  begin
    if TryStrToInt(Copy(LControlName, 2, Length(LControlName)), LIndex) then
      result := _ChildWindowByIndex(AParentHwnd, LIndex, AX, AY, not LChildControlSpecified);
  end
  else
  begin
    LWinControl := Controls.FindControl(AParentHwnd);
    if LWinControl <> nil then
    begin
      // Find by VCL child control index
      if TryStrToInt(LControlName, LIndex) then
      begin
        if LIndex < LWinControl.ControlCount then
        begin
          LControl := LWinControl.Controls[LIndex];
          if GetWinControl(LControl, AX, AY, not LChildControlSpecified) then
            result := TWinControl(LControl).Handle;
        end
        else
          result := 0;
      end
      else
        // Find by name, recurse into child controls
        Result := _FindByName(LWinControl, LControlName, AX, AY, not LChildControlSpecified);
    end;
  end;

  // Recurse to find the child control if specified
  if (Result <> 0) and LChildControlSpecified then
    Result := FindControlInstance(Result, LChildControlName, AX, AY);
end;
{$ENDIF}

function FindParentWinControl(const AControl: TControl): TWinControl;
begin
  if AControl = nil then
    Result := nil
  else if AControl is TWinControl then
    Result := AControl as TWinControl
  else
    Result := AControl.Parent;
end;

function GetWinControl(var AControl: TControl; var AX: Integer;
  var AY: Integer; const AIsTargetControl: Boolean): boolean;
var
  LPoint: TPoint;
begin
  if Assigned(AControl) then
  begin
    // We need an actual position to work with
    // If the default position is specified then use the centre of the control
    if AIsTargetControl and (AX = -1) and (AY = -1) then
    begin
      AX := AControl.Width div 2;
      AY := AControl.Height div 2;
    end;

    // If the control does not have a window handle then find the parent
    // control that does and translate the co-ords to the parent control
    if not (AControl is TWinControl) then
    begin
      // Step 1: Get control co-ords in screen co-ords
      if AIsTargetControl then
      begin
        LPoint := Point(AX, AY);
        LPoint := AControl.ClientToScreen(LPoint);
      end;

      // Target the parent windowed control
      AControl := FindParentWinControl(AControl);

      // Step 2: Now get the parent co-ords from screen co-ords
      if AIsTargetControl and Assigned(AControl) then
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

{$IFDEF MSWINDOWS}
function WindowClassName(const AHwnd: HWND): string;
const
  CMaxClassNameLength = 256;
begin
  SetLength(Result, CMaxClassNameLength);
  SetLength(Result, GetClassName(AHwnd, PChar(Result), CMaxClassNameLength));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function GetTopmostWindowProc(AHwnd: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  LdwProcessId: DWORD;
  LClassName: string;
begin
  // Windows are enumerated in reverse z-order so the topmost window for the
  // process is the first visible and enabled for input
  GetWindowThreadProcessId(AHwnd, LdwProcessId);
  LClassName := WindowClassName(AHwnd);
  // Note: Some higher z-order windows may be present that we need to skip to
  // get to the real window.
  // For example, Windows will create an additional window with class name
  // SysShadow for windows that have class style CS_DROPSHADOW to emphasize
  // their z-order. Tool tips are also implemented in a secondary window with
  // class name tooltips_class32.
  if IsWindowVisible(AHwnd) and IsWindowEnabled(AHwnd) {for input} and
     (LdwProcessId = GetCurrentProcessId) and (LClassName <> 'SysShadow') and
     (LClassName <> 'tooltips_class32') and (LClassName <> 'Shell_TrayWnd') then
  begin
    PINT_PTR(ALParam)^ := AHwnd;
    result := false; // Stop enumeration
  end
  else
    result := true; // Continue enumeration
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function GetTopmostWindow: HWND;
begin
  result := 0;
  EnumWindows(@GetTopmostWindowProc, LPARAM(@result));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function PrintWindow(AHwnd: HWND; AHdcBlt: HDC; AnFlags: DWORD): BOOL; stdcall; external 'user32.dll';
procedure SaveScreenshot(const AHwnd: HWND; const AFileName: string);
var
  LHwnd: HWND;
  LDC: HDC;
  LPalette: TMaxLogPalette;
  LBitmap: TBitmap;
  LRect: TRect;
  LPNGImage: TPNGImage;
begin
  Assert(AFileName <> '', 'Filename not specified');

  LRect := Rect(0, 0, 0, 0);
  if AHwnd = HWND_DESKTOP then
  begin
    LHwnd := GetDesktopWindow;
    // Multi-monitor supported
    LRect.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
    LRect.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
    LRect.Right := LRect.Left + GetSystemMetrics(SM_CXVIRTUALSCREEN);
    LRect.Bottom := LRect.Top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
  end
  else
  begin
    LHwnd := AHwnd;
    GetWindowRect(LHwnd, LRect)
  end;

  LBitmap := nil;
  LPNGImage := nil;
  try
    LBitmap := TBitmap.Create;
    LBitmap.Width := LRect.Width;
    LBitmap.Height := LRect.Height;
    LBitmap.PixelFormat := pf32bit;
    LBitmap.Canvas.Lock;
    try
      LDC := GetWindowDC(LHwnd); // Include non-client area
      try
        if LDC = 0 then
          raise Exception.Create('No DeviceContext for window');

        // Palette-device?
        if (GetDeviceCaps(LDC, RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
        begin
          FillChar(LPalette, SizeOf(TMaxLogPalette), #0);  // fill the structure with zeros
          LPalette.palVersion := $300;                     // fill in the palette version

          // Grab the system palette entries...
          LPalette.palNumEntries := GetSystemPaletteEntries(LDC, 0, 256, LPalette.palPalEntry);
          if LPalette.PalNumEntries <> 0 then
            {$IFDEF FPC}
            LBitmap.Palette := CreatePalette(LPLOGPALETTE(@LPalette)^);
            {$ELSE}
            LBitmap.Palette := CreatePalette(PLogPalette(@LPalette)^);
            {$ENDIF}
        end;

        // Copy the image from the window to the bitmap, including non-client area
        if LHwnd <> GetDesktopWindow then
          PrintWindow(LHwnd, LBitmap.Canvas.Handle, 0)
        else
          BitBlt(LBitmap.Canvas.Handle, 0, 0, LRect.Width, LRect.Height, LDC,
              LRect.Left, LRect.Top, SRCCOPY);
      finally
        ReleaseDC(LHwnd, LDC);
      end;
    finally
      LBitmap.Canvas.UnLock;
    end;

    LPNGImage := TPNGImage.Create;
    LPNGImage.Assign(LBitmap);
    LPNGImage.SaveToFile(AFileName);
  finally
    FreeAndNil(LPNGImage);
    FreeAndNil(LBitmap);
  end;
end;
{$ENDIF}

end.

