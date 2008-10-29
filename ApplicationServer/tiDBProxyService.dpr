program tiDBProxyService;

uses
  FastMM4,
  tiLog,
  tiLogToFile,
  SvcMgr,
  FMainTIDBProxyService in 'FMainTIDBProxyService.pas' {tiDBProxyServer: TService},
  tiDBProxyServerDependencies in '..\Options\tiDBProxyServerDependencies.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TtiDBProxyServer, tiDBProxyServer);
  Application.Run;
end.
