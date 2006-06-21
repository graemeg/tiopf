program tiGroupMail_DUnit_GUI;

uses
  tiLog,
  GUITestRunner,
  Forms,
  tiGroupMailDUnitDependencies in 'tiGroupMailDUnitDependencies.pas',
  tiGroupMailDependencies in '..\tiGroupMail\tiGroupMailDependencies.pas';

{$R *.res}

begin
  SetupLogForClient ;
  Application.Initialize;
  tiGroupMailDUnitDependencies.RegisterTests ;
  GUITestRunner.RunRegisteredTests;
end.
