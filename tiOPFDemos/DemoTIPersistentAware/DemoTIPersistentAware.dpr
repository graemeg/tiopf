program DemoTIPersistentAware;

uses
  tiLog,
  Forms,
  FEdit in 'FEdit.pas' {FormEdit},
  FMain in 'FMain.pas' {FormMain},
  uAnimal in 'uAnimal.pas';

{$R *.RES}

begin
SetupLogForClient;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
