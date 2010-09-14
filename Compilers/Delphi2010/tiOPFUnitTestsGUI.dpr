program tiOPFUnitTestsGUI;

{$I DUnit.inc}

uses
{$IFDEF FASTMM}
  FastMM4,
{$ENDIF}
  jclDebug,
  tiBaseObject,
  tiLog,
  Forms,
  TestFramework,
  GUITestRunner,
  tiLogToGUI,
  tiPromptWhichPersistenceLayersToTest in '..\..\UnitTests\Common\tiPromptWhichPersistenceLayersToTest.pas',
  tiTestDependencies in '..\..\UnitTests\Common\tiTestDependencies.pas';

{$R *.res}

begin
  tiRegisterExpectedTIOPFMemoryLeaks;
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;
  Application.Initialize;
  tiTestDependencies.tiRegisterTests;
  GUITestRunner.RunRegisteredTests;

end.
