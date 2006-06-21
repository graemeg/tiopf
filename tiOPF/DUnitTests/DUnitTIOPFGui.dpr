program DUnitTIOPFGui;

uses
  tiLog,
  Forms,
  TestFrameWork,
  GUITestRunner,
  FDUnitTestingWhichPersistenceLayers,
  tiDUnitDependencies in 'tiDUnitDependencies.pas';

{$R *.RES}

begin
  if not TFormWhichPersistenceLayers.Execute then
    Halt ;
  SetupLogForClient ;
  Application.Initialize;
  tiDUnitDependencies.RegisterTests ;
  GUITestRunner.RunRegisteredTests;
end.
