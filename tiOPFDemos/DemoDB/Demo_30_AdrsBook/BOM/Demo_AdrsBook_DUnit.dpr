program Demo_AdrsBook_DUnit;

uses
  tiLog,
  Forms,
  GUITestRunner,
  TestFramework,
  FDUnitTestingWhichPersistenceLayers,
  Adrs_BOM in 'Adrs_BOM.pas',
  Adrs_TST in 'Adrs_tst.pas',
  Adrs_Dependencies in 'Adrs_Dependencies.pas',
  AdrsMetaData_BOM in 'AdrsMetaData_BOM.pas',
  AdrsMetaData_TST in 'AdrsMetaData_TST.pas';

{$R *.res}

begin
  if not TFormWhichPersistenceLayers.Execute then
    Halt ;
  SetupLogForClient ;
  Application.Initialize;
  AdrsMetaData_TST.RegisterTests ;
  Adrs_TST.RegisterTests ;
  GUITestRunner.RunRegisteredTests;
end.
