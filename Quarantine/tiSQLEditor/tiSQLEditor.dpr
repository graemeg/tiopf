program tiSQLEditor;

uses
  Forms,
  FMainSQLEditor in 'FMainSQLEditor.pas' {FMainSQLEditor},
  tiLogReg,
  tiOIDGUID,
  tiOPFManager,
  FSQLEditor in 'FSQLEditor.pas' {FSQLEditor},
  FSQLMgrBrowse in 'FSQLMgrBrowse.pas' {FSQLMgrBrowse},
  tiSQLMgrDataSet_Srv in 'tiSQLMgrDataSet_Srv.pas',
  tiSQLMgrDataSet_BOM in 'tiSQLMgrDataSet_BOM.pas',
  tiCommandLineParams,
  SysUtils,
  FRunAsScript in 'FRunAsScript.pas';

{$R *.res}

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

