{
  Log to a file in the \Log folder and/or to a form, but only if
  the -l (or lv) parameter is passed on the command line.
}
unit tiLogReg;

{$I tiDefines.inc}

interface

implementation
uses
   tiLog
//  ,tiConstants    { ToDo: We should move log constant to this unit. }
  ,tiCommandLineParams
  ,tiLogToFile
  ,tiLogToGUI
  ,tiLogToConsole
 ;


initialization

  if gCommandLineParams.IsParam(csLog)
  or gCommandLineParams.IsParam(csLogVisual) then
    GLog.RegisterLog(TtiLogToFile.Create);

  if gCommandLineParams.IsParam(csLogVisual) then
    GLog.RegisterLog(TtiLogToGUI.Create);

  if gCommandLineParams.IsParam(csLogConsole) then
    GLog.RegisterLog(TtiLogToConsole.Create);

end.
