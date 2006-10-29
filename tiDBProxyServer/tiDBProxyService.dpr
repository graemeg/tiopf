program tiDBProxyService;

uses
  FastMM4,
  madExcept,
  tiLog,
  tiLogToFile,
  SvcMgr,
  FMainTIDBProxyService in 'FMainTIDBProxyService.pas' {tiDBProxyServer: TService},
  tiDBProxyServerDependencies in '..\..\Source\Options\tiDBProxyServerDependencies.pas';

{$R *.RES}

begin
  gLog.RegisterLog(TtiLogToFile.CreateWithDateInFileName);
  Application.Initialize;
  Application.CreateForm(TtiDBProxyServer, tiDBProxyServer);
  Application.Run;
end.
