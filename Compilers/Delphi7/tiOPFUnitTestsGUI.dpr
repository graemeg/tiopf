program tiOPFUnitTestsGUI;

uses
  FastMM4,
  tiBaseObject,
  tiLog,
  Forms,
  TestFramework,
  GUITestRunner,
  tiLogToGUI,
  tiTestDependencies in '..\..\UnitTests\Common\tiTestDependencies.pas',
  tiPromptWhichPersistenceLayersToTest in '..\..\UnitTests\Common\tiPromptWhichPersistenceLayersToTest.pas';

{$R *.RES}

begin
  tiRegisterExpectedTIOPFMemoryLeaks;
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;
  Application.Initialize;
  tiTestDependencies.tiRegisterTests;
  GUITestRunner.RunRegisteredTests;
end.



