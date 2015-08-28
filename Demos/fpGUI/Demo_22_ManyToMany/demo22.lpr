program demo22;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  ,fpg_base
  ,fpg_main
  ,frm_main
  ,tiOPFManager
  ,tiConstants
  ,tiCommandLineParams
  ,tiLog
  ,tiLogToConsole
  ,tiLogToGUI
  ,tiLogToFile
  ,tiLogToDebugSvr
  ,tiQuerySqldbIB
  ,regvisitors
  ;

const
  cPersistenceLayerName = cTIPersistSqldbIB;

procedure MainProc;
var
  frm: TMainForm;
begin
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  gLog.SevToLog :=  [
                    lsNormal
                    ,lsUserInfo
//                    ,lsObjCreation
                    ,lsVisitor
                    ,lsConnectionPool
                    ,lsAcceptVisitor
                    ,lsQueryTiming
                   ,lsDebug
                   ,lsWarning
                   ,lsError
                   ,lsSQL
              ];

  // Logging
  if gCommandLineParams.IsParam(csLogConsole) then
    gLog.RegisterLog(TtiLogToConsole);
  if gCommandLineParams.IsParam(csLog) then
    gLog.RegisterLog(TtiLogToFile.CreateWithFileName('.','demo22.log', True));
  if gCommandLineParams.IsParam(csLogVisual) then
    gLog.RegisterLog(TtiLogToGUI);
  if gCommandLineParams.IsParam(csLogDebugSvr) then
    gLog.RegisterLog(TtiLogToDebugSvr);

  fpgApplication.Initialize;

  // Setup persistence layer and connect to the database
  if GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cPersistenceLayerName) = nil then
    raise Exception.Create('The system failed to find the <' + cPersistenceLayerName + '> persistence layer');
  GTIOPFManager.DefaultPersistenceLayerName := cPersistenceLayerName;
  GTIOPFManager.ConnectDatabase(
      'OrderSystem',          { Alias, same as selection in DB ComboBox }
      'OrderSystem.fdb',      { Database }
      'sysdba',                 { username }
      'masterkey',              { password }
      '',                       { extra db parameters }
      cPersistenceLayerName);   { persistence layer name }


  RegisterVisitors;

  fpgApplication.CreateForm(TMainForm, frm);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    GTIOPFManager.DisconnectDatabase;
    GTIOPFManager.Terminate;
    frm.Free;
  end;
end;

begin
  MainProc;
end.

