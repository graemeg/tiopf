program DUnitTIOPFGui2010;

uses
  tiBaseObject,
  tiLog,
  Forms,
  TestFramework,
  GUITestRunner,
  tiLogToGUI,
  tiTestDependencies in '..\Common\tiTestDependencies.pas',
  tiPromptWhichPersistenceLayersToTest in '..\Common\tiPromptWhichPersistenceLayersToTest.pas',
  tiGenericFilteredObjectList in '..\..\Options\tiGenericFilteredObjectList.pas',
  tiGenericFilteredObjectList_TST in '..\Tests\tiGenericFilteredObjectList_TST.pas';

{$R *.RES}

begin
  tiRegisterExpectedTIOPFMemoryLeaks;
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;
  Application.Initialize;
  tiTestDependencies.tiRegisterTests;
  GUITestRunner.RunRegisteredTests;
end.



