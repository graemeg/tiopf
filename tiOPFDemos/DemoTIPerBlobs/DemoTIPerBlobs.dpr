program DemoTIPerBlobs;

uses
  Forms,
  tiPersist,
  tiLog,
  tiPerObjOIDGUID,
  FMain in 'FMain.pas' {FormMain},
  DemoTIPerBlobs_BOM in 'DemoTIPerBlobs_BOM.pas',
  DemoTIPerBlobs_Srv in 'DemoTIPerBlobs_Srv.pas';

{$R *.RES}

begin
  SetupLogForClient ;
  gLog.SevToLog := gLog.SevToLog + [lsConnectionPool];
  Application.Initialize;
  gTIPerMgr.LoadPersistenceFramework ;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
  gTIPerMgr.UnloadPersistenceFramework ;
end.
