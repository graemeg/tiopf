program dunit2_fpc_textrunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  TestFramework,
  TextTestRunner,
  tiOPFTestManager,
  tiTestDependencies,
  tiTextTestRunner, textprogressrunner;

var
  LExitBehavior : TRunnerExitBehavior;

begin
  { Unit tests are hard-coded to Australian locale }
  DateSeparator := '/';

  LExitBehavior := rxbContinue;
  GTIOPFTestManager.Read;
  GTIOPFTestManager.DeleteDatabaseFiles;
  tiRemoveXMLLightIfNotRegistered;
  tiTestDependencies.tiRegisterTests;
  tiTextTestRunner.RunRegisteredTests(LExitBehavior);
end.

