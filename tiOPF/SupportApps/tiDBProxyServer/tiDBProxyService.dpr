program tiDBProxyService;

uses
  SvcMgr,
  tiLog,
  tiDBProxyServerIndyHTTP in 'tiDBProxyServerIndyHTTP.pas',
  FMainTIDBProxyService in 'FMainTIDBProxyService.pas' {tiDBProxyServer: TService},
  tiQueryRemote_Svr in 'tiQueryRemote_Svr.pas';

{$R *.RES}

begin
  SetupLogForServer;
  Application.Initialize;
  Application.CreateForm(TtiDBProxyServer, tiDBProxyServer);
  Application.Run;
end.
