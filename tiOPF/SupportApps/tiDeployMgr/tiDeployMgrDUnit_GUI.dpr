program tiDeployMgrDUnit_GUI;

uses
  madExcept,
  madLinkDisAsm,
  TestFramework,
  GUITestRunner,
  Forms,
  tiDeployMgr_TST in 'tiDeployMgr_TST.pas';

{$R *.res}

begin
  GUITestRunner.RunRegisteredTests
end.
