program DUnitTIOPFGui;

uses
  FastMM4,
  tiBaseObject,
  tiLog,
  Forms,
  TestFramework,
  GUITestRunner,
  tiLogToGUI,
  tiDUnitDependencies in '..\Common\tiDUnitDependencies.pas',
  tiPromptWhichPersistenceLayersToTest in '..\Common\tiPromptWhichPersistenceLayersToTest.pas';

{$R *.RES}

begin
  RegisterExpectedTIOPFMemoryLeaks;
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;
  Application.Initialize;
  tiDUnitDependencies.RegisterTests;
  GUITestRunner.RunRegisteredTests;
end.

