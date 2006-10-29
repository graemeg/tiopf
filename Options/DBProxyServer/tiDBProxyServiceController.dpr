program tiDBProxyServiceController;

uses
  madExcept,
  Forms,
  tiWebServerConstants,
  Windows,
  FMainTIDBProxyServiceController in 'FMainTIDBProxyServiceController.pas' {Form1};

{$R *.res}

begin
  CreateMutex(nil, False, cTIDBProxyServerMutex );
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
