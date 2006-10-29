unit tiGUIUtils;

{$I tiDefines.inc}

interface
uses
  tiObject
  ,Graphics      // Canvas
  ,tiDataBuffer_BOM
  ,tiRegINI
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,Forms
 ;

const
  cgsSaveAndClose = 'Do you want to save your changes before exiting?';

{: Don't use tiPerObjAbsAsString. Use TtiObject.AsDebugString}
function  tiPerObjAbsAsString(const AVisited: TtiObject; AIncludeDeleted: boolean = false): string;
procedure tiShowPerObjAbs(const AVisited: TtiObject; AIncludeDeleted: boolean = false);
procedure tiShowPerObjAbsOwnership(const AData: TtiObject);

// Set canvas.font.color to crRed if AData.Dirty, crBlank if not
procedure tiPerObjAbsFormatCanvas(const pCanvas : TCanvas; const AData : TtiObject);
// If AData.Dirty, then prompt user to save
function  tiPerObjAbsSaveAndClose(const AData     : TtiObject;
                                   var   pbCanClose : boolean;
                                   const AMessage : string = cgsSaveAndClose): boolean;
function  tiPerObjAbsConfirmAndDelete(const AData : TtiObject;
                                       const AMessage : string = ''): boolean;
                                       
procedure ShowTIDataSet(const pDataSet: TtiDataBuffer); // For debugging


{$IFDEF MSWINDOWS}
{: Is the Ctrl key down?}
function tiIsCtrlDown : Boolean;
{: Is the Shift key down?}
function tiIsShiftDown : Boolean;
{: Is the Alt key down?}
function tiIsAltDown : Boolean;
{$ENDIF MSWINDOWS}

  // Screen, monitor and desktop
{$IFDEF MSWINDOWS}
  // Get monitor position information for given window.
  function tiGetWindowMonitorPos(WindowHandle: HWND; var r: TRect;
                                 var MonitorToLeft: Boolean; var MonitorToRight: Boolean;
                                 var MonitorAbove: Boolean; var MonitorBelow: Boolean): Boolean;
  // Is the given window off screen (positioned outside visible monitors)
  function tiFormOffScreen(AForm: TForm): Boolean;
{$ENDIF MSWINDOWS}

  procedure ReadFormState(Ini: TtiINIFile; AForm: TForm; AHeight: integer = -1; AWidth: integer = -1); overload;
  procedure ReadFormState(Reg: TtiRegINIFile; AForm: TForm); overload;

implementation
uses
  tiUtils
  ,tiConstants
  ,Controls // mrYes
  {$IFDEF MSWINDOWS}
  ,MultiMon
  {$ENDIF MSWINDOWS}
  ,tiDialogs
  {$IFDEF FPC}
  ,LCLType    // TKeyboardState under FPC is not in Windows unit
  {$ENDIF}
  ,tiDataBuffer_Cli   // used for ShowTIDataset and TIDataSetToString method
 ;


{ Global funcs and procs }


function tiPerObjAbsAsString(const AVisited: TtiObject; AIncludeDeleted: boolean = false): string;
begin
  Assert(AVisited.TestValid, cErrorTIPerObjAbsTestValid);
  Result := AVisited.AsDebugString;
end;


procedure tiShowPerObjAbs(const AVisited: TtiObject; AIncludeDeleted: boolean = false);
var
  ls: string;
begin
  ls := tiPerObjAbsAsString(AVisited, AIncludeDeleted);
  tiShowString(ls);
end;


procedure tiShowPerObjAbsOwnership(const AData: TtiObject);
  function _GetOwnership(const AData: TtiObject; const ps: string): string;
  begin
    if AData.Owner <> nil then
    begin
      result := ps + #13 + AData.Owner.ClassName;
      Result := _GetOwnership(AData.Owner, result);
    end else
      Result := ps;
    end;
var
  ls: string;
begin
  Assert(AData.TestValid(TtiObject), cTIInvalidObjectError);
  ls := _GetOwnership(AData, AData.ClassName);
  tiShowString(ls);
end;


procedure tiPerObjAbsFormatCanvas(const pCanvas: TCanvas; const AData: TtiObject);
begin
  if AData.Dirty then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack;
end;


function  tiPerObjAbsSaveAndClose(const AData     : TtiObject;
                                   var   pbCanClose : boolean;
                                   const AMessage : string = cgsSaveAndClose): boolean;
var
  lResult: integer;
begin

  result    := false;
  pbCanClose := true;

  if not AData.Dirty then
    exit; //==>

  lResult := tiYesNoCancel(AMessage);
  case lResult of
    mrYes   : result := true;
    mrNo    :; // Do nothing
    mrCancel : pbCanClose := false;
  end;

end;


function tiPerObjAbsConfirmAndDelete(const AData: TtiObject;
    const AMessage: string = ''): boolean;
var
  lMessage: string;
begin
  result := false;

  if AData = nil then
    Exit; //==>

  if AMessage = '' then
    lMessage := 'Are you sure you want to delete <' +
                AData.Caption + '>?'
  else
    lMessage := AMessage;

  result := tiAppConfirmation(lMessage);

  if result then
    AData.Deleted := true;
end;


procedure ShowTIDataSet(const pDataSet: TtiDataBuffer);
var
  ls : string;
begin
  ls := TIDataSetToString(pDataSet);
  tiShowString(ls);
end;


{$IFDEF MSWINDOWS}
function tiIsCtrlDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State);
   Result := ((State[vk_Control] And 128) <> 0);
end;


function tiIsShiftDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State);
   Result := ((State[vk_Shift] and 128) <> 0);
end;


function tiIsAltDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State);
   Result := ((State[vk_Menu] and 128) <> 0);
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function tiGetWindowMonitorPos(WindowHandle: HWND; var r: TRect;
                               var MonitorToLeft: Boolean; var MonitorToRight: Boolean;
                               var MonitorAbove: Boolean; var MonitorBelow: Boolean): Boolean;
var
  lHCurrentMon: HMONITOR;
  lOS: OSVERSIONINFO;
  lMI: MONITORINFO;
  I: Integer;
begin
  Result := False;
  MonitorToLeft := False;
  MonitorToRight := False;
  MonitorAbove := False;
  MonitorBelow := False;
  try
    // Only Windows 2000 and later support MonitorFromWindow etc...
    lOS.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
    if GetVersionEx(lOS) and (lOS.dwMajorVersion >= 5 {Windows 2000}) then begin
      // Get the monitor that the window is mostly displayed in.
      lHCurrentMon := MonitorFromWindow(WindowHandle, MONITOR_DEFAULTTONEAREST);
      if lHCurrentMon <> 0 then begin
        FillChar(lMI, sizeof(MONITORINFO), #0);
        lMI.cbSize := sizeof(MONITORINFO);

        // Get the info for the monitor.
        if GetMonitorInfo(lHCurrentMon, @lMI) then begin
          // See if there are any monitors around us. We won't auto-hide at
          // edges adjacent to another monitor.
          for I := 0 to Screen.MonitorCount - 1 do begin
            if Screen.Monitors[I].Handle = lHCurrentMon then
              continue;
            if Screen.Monitors[I].Left < lMI.rcMonitor.left then
              MonitorToLeft := True;
            if Screen.Monitors[I].Left > lMI.rcMonitor.left then
              MonitorToRight := True;
            if Screen.Monitors[I].Top < lMI.rcMonitor.top then
              MonitorAbove := True;
            if Screen.Monitors[I].Top > lMI.rcMonitor.top then
              MonitorBelow := True;
          end;

          // Get the bounds of current monitor excluding any fixed task bar.
          r.left := lMI.rcWork.left;
          r.right := lMI.rcWork.right;
          r.top := lMI.rcWork.top;
          r.bottom := lMI.rcWork.bottom;

          Result := True;
        end;
      end;
    end;

    // Fall-back (e.g. < Windows 2000), simple current monitor size info.
    if not Result then begin
      if SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0) then
        Result := True;
    end;
  except
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function tiFormOffScreen(AForm: TForm): Boolean;
var
  lR: TRect;
  lMonitorToLeft: Boolean;
  lMonitorToRight: Boolean;
  lMonitorAbove: Boolean;
  lMonitorBelow: Boolean;
const
  cOffScreenDistance = 10; // Considered off-screen if this far outside monitors.
begin
  Result := False;
  if tiGetWindowMonitorPos(AForm.Handle, lR, lMonitorToLeft, lMonitorToRight,
                           lMonitorAbove, lMonitorBelow) then begin
    if ((not lMonitorToLeft) and (AForm.Left < (lR.left - cOffScreenDistance))) or
       ((not lMonitorToRight) and ((AForm.Left + AForm.Width) > (lR.right + cOffScreenDistance))) or
       ((not lMonitorAbove) and (AForm.Top < (lR.top - cOffScreenDistance))) or
       ((not lMonitorBelow) and ((AForm.Top + AForm.Height) > (lR.bottom + cOffScreenDistance))) then
      Result := True;
  end;
end;
{$ENDIF MSWINDOWS}

procedure ReadFormState(Ini: TtiINIFile; AForm: TForm; AHeight: integer = -1; AWidth: integer = -1);
begin
  // Default behaviour: retrieve form position, size and window state.
  Ini.ReadFormState(AForm, AHeight, AWidth);

  // If the form is off screen (positioned outside all monitor screens) then
  // center the form on screen.
{$IFDEF MSWINDOWS}
  if (AForm.FormStyle <> fsMDIChild) and tiFormOffScreen(AForm) then begin
    if Assigned(Application.MainForm) and (Application.MainForm <> AForm) then
      AForm.Position := poMainFormCenter
    else
      AForm.Position:= poScreenCenter;
  end;
{$ENDIF MSWINDOWS}
end;

procedure ReadFormState(Reg: TtiRegINIFile; AForm: TForm);
begin
  // Default behaviour: retrieve form position, size and window state.
  Reg.ReadFormState(AForm);

  // If the form is off screen (positioned outside all monitor screens) then
  // center the form on screen.
{$IFDEF MSWINDOWS}
  if (AForm.FormStyle <> fsMDIChild) and tiFormOffScreen(AForm) then begin
    if Assigned(Application.MainForm) and (Application.MainForm <> AForm) then
      AForm.Position := poMainFormCenter
    else
      AForm.Position:= poScreenCenter;
  end;
{$ENDIF MSWINDOWS}
end;

end.


