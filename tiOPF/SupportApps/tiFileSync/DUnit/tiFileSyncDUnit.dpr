program tiFileSyncDUnit;

uses
  madExcept,
  madLinkDisAsm,
  tiObjAbs,
  tiLog,
  tiPersist,
  tiQueryXMLLight,
  tiUtils,
  Forms,
  TestFrameWork,
  GUITestRunner,
  tiFileName_TST in 'tiFileName_TST.pas',
  tiFileSyncSetup_TST in 'tiFileSyncSetup_TST.pas',
  tiFileSyncMgr_TST in 'tiFileSyncMgr_TST.pas',
  tiFileSyncReader_TST in 'tiFileSyncReader_TST.pas',
  tiFileSyncDependencies in '..\Src\tiFileSyncDependencies.pas';

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  tiFileSyncDependencies.RegisterMappings;
  // Probably don't want this here - should connect as required in the tests
  tiFileSyncDependencies.ConnectToDatabase;
  tiFileName_TST.RegisterTests ;
  tiFileSyncSetup_TST.RegisterTests ;
  tiFileSyncMgr_TST.RegisterTests ;
  tiFileSyncReader_TST.RegisterTests ;
  GUITestRunner.RunRegisteredTests;

end.
