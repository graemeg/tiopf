program tiDBProxyServiceTest;

uses
  FastMM4,
  madExcept,
  Forms,
  tiLogToGUI,
  FMainTIDBProxy in 'FMainTIDBProxy.pas' {FormMainTIDBProxyServer},
  tiDBProxyServerDependencies in '..\..\Source\Options\tiDBProxyServerDependencies.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainTIDBProxyServer, FormMainTIDBProxyServer);
  Application.Run;
end.
