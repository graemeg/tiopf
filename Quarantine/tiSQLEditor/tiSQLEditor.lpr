program tiSQLEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces // this includes the LCL widgetset
  ,Forms
  { add your units here }
  ,FMainSQLEditor
  ,tiLogReg
  ,tiOIDGUID
  ,tiOPFManager
  ,FSQLEditor
  ,FSQLMgrBrowse
  ,tiSQLMgrDataSet_Srv
  ,tiSQLMgrDataSet_BOM
  ,tiCommandLineParams
  ,SysUtils
  ;

var
  lDatabaseName: string;
  lUserName: string;
  lPassword: string;
  lPerLayerName: string;
begin
  Application.Initialize;
  

  lDatabaseName := gCommandLineParams.GetParam('d');
  lUserName     := gCommandLineParams.GetParam('u');
  lPassword     := gCommandLineParams.GetParam('p');
  
  if (lDatabaseName = '') or (lUserName = '') or (lPassword = '') then
    raise Exception.Create('Connect to the appropriate database using -d -u and -p command line params.');

  // Connect to the appropriate database using -d -u and -p command line params.
  // persistence layer linked in via a compiler directive. eg: LINK_FBL = Firebird FBLib
  gTIOPFManager.ConnectDatabase(lDatabaseName, lUserName, lPassword);

  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

  gTIOPFManager.DisconnectDatabase;
  gTIOPFManager.Terminate;
end.

