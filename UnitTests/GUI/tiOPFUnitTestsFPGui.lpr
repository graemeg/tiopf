program tiOPFUnitTestsFPGui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, clocale,
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
  DateSeparator := '/';
//  DecimalSeparator := '.';
//  CurrencyString := '$';

//  if not TtiPromptWhichPersistenceLayersToTest.Execute then
//    Halt;
  GTIOPFTestManager.Read;
  GTIOPFTestManager.DeleteDatabaseFiles;
  tiRemoveXMLLightIfNotRegistered;

  tiTestDependencies.tiRegisterTests;
//  fpgApplication.CreateForm(TGUITestRunner, TestRunner);
  RunRegisteredTests;
end.

