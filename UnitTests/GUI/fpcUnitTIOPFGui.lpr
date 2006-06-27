program fpcUnitTIOPFGui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces
  ,Forms
  ,GuiTestRunner
  ,tiDUnitDependencies
  ,tiDUnitINI
  ,tiPromptWhichPersistenceLayersToTest
  , tiUtils;
  

begin
  Application.Title:='fpcUnit tiOPF Gui';
  Application.Initialize;
  
  if not TtiPromptWhichPersistenceLayersToTest.Execute then
    Halt;

  tiDUnitDependencies.RegisterTests;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

