program DUnitTIOPFText;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  TestFrameWork,
  TextTestRunner,
  tiDUnitDependencies in 'tiDUnitDependencies.pas',
  tiTextTestRunner in 'tiTextTestRunner.pas';

var
  ExitBehavior : TRunnerExitBehavior;

begin

  tiDUnitDependencies.RemoveUnSelectedPersistenceLayerSetups;
  tiDUnitDependencies.RegisterTests;

  // To run with rxbPause, use -p switch
  // To run with rxbHaltOnFailures, use -h switch
  // No switch runs as rxbContinue

  if FindCmdLineSwitch('p', ['-', '/'], true) then
    ExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    ExitBehavior := rxbHaltOnFailures
  else
    ExitBehavior := rxbContinue;

  tiTextTestRunner.RunRegisteredTests(ExitBehavior);

end.
