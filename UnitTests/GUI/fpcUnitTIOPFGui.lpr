program fpcUnitTIOPFGui;

{$I tiDefines.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces
  ,Forms
  ,GuiTestRunner
  ,tiDUnitINI
  ,tiPromptWhichPersistenceLayersToTest
  ,tiTestDependencies
  ;
  

begin
  Application.Initialize;
  
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;

  tiTestDependencies.tiRegisterTests;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

