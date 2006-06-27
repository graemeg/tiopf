program DUnitTIOPFText;
{$APPTYPE CONSOLE}
uses
  FastMM4,
  SysUtils,
  TestFrameWork,
  TextTestRunner,
  tiDUnitDependencies in '..\Common\tiDUnitDependencies.pas',
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
    tiDUnitDependencies.RegisterTests;
    tiDUnitDependencies.RemoveUnSelectedPersistenceLayerSetups;
    tiTextTestRunner.RunRegisteredTests(LExitBehavior);
  end else
    WriteEmptyLogs(LExitBehavior);

end.
