program tiOPFUnitTestsFPGui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils
  ,fpg_base, fpg_main
  ,GUITestRunner
  ,tiDUnitINI
//  ,tiPromptWhichPersistenceLayersToTest
  ,tiTestDependencies
  ,tiOPFTestManager
  ;


begin
  { Unit tests are hard-coded to Australian locale }
  FormatSettings.DateSeparator := '/';

//  if not TtiPromptWhichPersistenceLayersToTest.Execute then
//    Halt;
  GTIOPFTestManager.Read;
  GTIOPFTestManager.DeleteDatabaseFiles;
  tiRemoveXMLLightIfNotRegistered;

  tiTestDependencies.tiRegisterTests;
//  fpgApplication.CreateForm(TGUITestRunner, TestRunner);
  RunRegisteredTests;
end.

