program tiOPFUnitTestsGUI;

uses
  {$IFNDEF FPC}
    {$IFDEF FASTMM}
    FastMM4,
    {$ENDIF}
    {$IFDEF USE_JEDI_JCL}
    jclDebug,
    {$ENDIF}
  {$ENDIF}
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
