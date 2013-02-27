program tiObjectLockingFramework_TST;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  tiObjectLocking_TST in 'tiObjectLocking_TST.pas',
  tiObjectLockingService in 'tiObjectLockingService.pas',
  tiXmlRpcObjectLocking in 'tiXmlRpcObjectLocking.pas';

{$R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.
