program DUnitTIOPFText;
{$APPTYPE CONSOLE}
uses
  FastMM4,
  SysUtils,
  TestFrameWork,
  TextTestRunner,
  tiTestDependencies in '..\Common\tiTestDependencies.pas',
  tiTextTestRunner in '..\Common\tiTextTestRunner.pas';

var
  LExitBehavior : TRunnerExitBehavior;

begin

  // To run with rxbPause, use -p switch
  // To run with rxbHaltOnFailures, use -h switch
  // No switch runs as rxbContinue
  if FindCmdLineSwitch('p', ['-', '/'], true) then
    LExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    LExitBehavior := rxbHaltOnFailures
  else
    LExitBehavior := rxbContinue;

  if not FindCmdLineSwitch(cCommandLineParamsNoTests, ['-', '/'], true) then
  begin
    GTIOPFTestManager.Read;
    RemoveXMLLightIfNotRegistered;
    tiTestDependencies.RegisterTests;
    tiTextTestRunner.RunRegisteredTests(LExitBehavior);
  end else
    WriteEmptyLogs(LExitBehavior);

end.

