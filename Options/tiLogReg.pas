{
  Log to a file in the \Log folder and/or to a form, but only if
  the -l (or lv) parameter is passed on the command line.
}
unit tiLogReg;

interface


implementation
uses
   tiLog
//  ,tiConstants    { ToDo: We should move log constant to this unit. }
  ,tiCommandLineParams
  ,tiLogToFile
  ,tiLogToGUI
  {$IFDEF CONSOLE}
  ,tiLogToConsole
  {$ENDIF}
 ;


initialization

  if gCommandLineParams.IsParam(csLog)
  or gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToFile.Create);

  if gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToGUI.Create);

  {$IFDEF CONSOLE}
  if gCommandLineParams.IsParam(csLogConsole) then
    gLog.RegisterLog(TtiLogToConsole.Create);
  {$ENDIF}

end.
