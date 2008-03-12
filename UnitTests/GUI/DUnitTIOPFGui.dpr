program DUnitTIOPFGui;

uses
  FastMM4,
  tiBaseObject,
  tiLog,
  Forms,
  TestFramework,
  GUITestRunner,
  tiLogToGUI,
  tiTestDependencies in '..\Common\tiTestDependencies.pas',
  tiPromptWhichPersistenceLayersToTest in '..\Common\tiPromptWhichPersistenceLayersToTest.pas';

{$R *.RES}

begin
  tiRegisterExpectedTIOPFMemoryLeaks;
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;
  Application.Initialize;
  tiTestDependencies.tiRegisterTests;
  GUITestRunner.RunRegisteredTests;
end.



