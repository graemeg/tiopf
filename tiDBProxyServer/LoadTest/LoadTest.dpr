program LoadTest;

uses
  madExcept,
  madLinkDisAsm,
  tiLog,
  Forms,
  FMainLoadTest in 'FMainLoadTest.pas' {Form2};

{$R *.res}

begin
  SetupLogForClient;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
