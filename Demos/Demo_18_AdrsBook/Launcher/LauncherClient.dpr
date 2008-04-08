program LauncherClient;

uses
  Forms,
  tiCGIExtensionRequestDBProxyServer,
  FMain in 'FMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm := False ;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
