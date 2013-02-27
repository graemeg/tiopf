unit tiDeployMgr_Dependencies;

interface
uses
   tiDeployMgr_BOM
  ,tiDeployMgr_Srv
  ,tiDeployMgr_Cli
  ,tiCompressNone
  ,tiCompressZLib
  ,tiPerObjOIDInteger
  {$IFDEF DOA}
  ,tiQueryDOA
  {$ELSE}
  ,tiQueryRemote
  ,tiHTTP
  ,tiHTTPIndy
  ,tiHTTPMSXML
  {$ENDIF}
  ,tiQuery
  ;

procedure Execute ;
procedure ConnectToDatabase;

implementation
uses
   tiLog
  ,tiPersist
  ,cTIPersist
  ,Forms
  ,FAppLaunch
  ,tiRegINI
  ,tiCommandLineParams
  ,tiDialogs
  ,ctiAppLaunch
  ,OPDMSini
  ,tiUtils
  ,MadExcept
  ;

const
  cOPDMSFileNameConnectionWizardEXE = 'OPDMSConnectionWizard.EXE';


{$IFNDEF DOA}
// Warning. This code is cloned in tiAppLaunch.exe tiDeployMgr_Dependencies.pas
function AttemptConnection: Boolean;
var
  lParams : string ;
  ls : string ;
begin
  if gOPDMSConfig.AppServerURL <> '' then
  begin
    lParams := gOPDMSConfig.AppServerParams;
    ls := gtiPerMgr.TestThenConnectToDatabase( gOPDMSConfig.AppServerURL, 'null',
                                         'null', cTIPersistRemote, lParams) ;
    Result := ls <> '' ;
  end else
    Result := False ;
end ;

procedure DoConnect;
begin
  try
    if not AttemptConnection then
    begin
      tiShellExecute(tiAddEXEPath(cOPDMSFileNameConnectionWizardEXE));
      {$IFDEF MadExcept} CloseApplication {$ELSE} Halt {$ENDIF} ;
    end ;
  except
    on e:EtiOPFDBExceptionWrongServerVersion do
    begin
      tiAppError(cTIOPFExcMsgWrongServerVersion);
      {$IFDEF MadExcept} CloseApplication {$ELSE} Halt {$ENDIF} ;
    end;
    on e:EtiOPFDBExceptionCanNotConnect do
    begin
      tiAppError(tiFormatRemoteConnectionErrorString(gOPDMSConfig.AppServerURLs));
      {$IFDEF MadExcept} CloseApplication {$ELSE} Halt {$ENDIF} ;
    end ;
  end ;
end ;
{$ENDIF}

procedure ConnectToDatabase;
{$IFDEF DOA}
var
  lDatabaseName : string ;
  lUserName     : string ;
  lPassword     : string ;
{$ENDIF}
begin

{$IFDEF DOA}
  lDatabaseName := gCommandLineParams.GetParam('d');
  lUserName     := gCommandLineParams.GetParam('u');
  lPassword     := gCommandLineParams.GetParam('p');

  {
    // This is standard tiOPF stuff
    if lDatabaseName = '' then
      lDatabaseName := uINI.ReadString('DBConnection', 'DatabaseName', cLocalHost);

    if lUserName = '' then
      lUserName := uINI.ReadString('DBConnection', 'UserName',     'null');

    if lPassword = '' then
      lPassword := uINI.ReadString('DBConnection', 'Password',     'null');
  }
  gtiPerMgr.LoadDatabaseLayer(cTIPersistDOA,
                              lDatabaseName,
                              lUserName,
                              lPassword);
{$ELSE}
  DoConnect;
{$ENDIF}
end ;

procedure Execute ;
begin
  ConnectToDatabase;
  Application.Initialize;
  Application.CreateForm(TFormAppLaunch, FormAppLaunch);
  FormAppLaunch.DatabaseName := gTIPerMgr.DefaultDBConnectionName ;
  FormAppLaunch.FileGroup := cTIAppLaunchFileGroup;
  Application.MainForm.Caption := cTIAppLaunchApplicationTitle;
  Application.Title := Application.MainForm.Caption ;
  Application.Run;
end;

end.
