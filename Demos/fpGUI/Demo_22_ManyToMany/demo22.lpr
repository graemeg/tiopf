{
    This file is part of the tiOPF project.

    See the file license.txt, included in this distribution,
    for details about redistributing tiOPF.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This demo shows how Many-to-Many relationships can be handled
      with tiOPF. It uses a classic Customer/Order environment as an
      example.
}
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
  ,tiMediators
  ,tiListMediators
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
  RegisterFallBackMediators;
  RegisterFallBackListMediators;

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

