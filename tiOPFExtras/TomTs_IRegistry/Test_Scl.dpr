program Test_Scl;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  Test_SclNet in 'Test_SclNet.pas',
  Test_SclRegistry in 'Test_SclRegistry.pas',
  Test_SclRegistryFile in 'Test_SclRegistryFile.pas',
  SclRegistryFile in 'SclRegistryFile.pas';

{$R *.RES}

begin
 Application.Initialize;
 GUITestRunner.RunRegisteredTests;
end.


