program DemoMultiDBAccess;

uses
  tiLog,
  tiPersist,
  Forms,
  ctiPersist,
  Adrs_Dependencies,
  FMain in 'FMain.pas' {Form2};

{$R *.RES}

begin

  // This application must be built with runtime packages with
  // the package tiPersistCore included in the list.

  // To see the log trace at runtime, run with the -lv parameter.

  // Setup the logging framework for GUI logging
  SetupLogForClient ;

  // Load the persistence layer
  gTIPerMgr.LoadPersistenceLayer( cTIPersistIBX ) ;

  // Run the application
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;

  // Unload the persistence layer
  gTIPerMgr.UnLoadPersistenceFramework( cTIPersistIBX ) ;

end.
