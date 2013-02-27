program DemoTILog;

uses
  tiLog,
  Forms,
  FMain in 'FMain.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  SetupLogForClient ;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
