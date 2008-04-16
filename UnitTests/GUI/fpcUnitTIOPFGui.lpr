program fpcUnitTIOPFGui;

{$I tiDefines.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces
  ,Forms
  ,GuiTestRunner
  ,tiDUnitINI
  ,tiPromptWhichPersistenceLayersToTest
  ,tiTestDependencies
  ;
  

begin
  Application.Title:='fpcUnit TIOPF Gui';
  Application.Initialize;
  
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;

  tiTestDependencies.tiRegisterTests;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

