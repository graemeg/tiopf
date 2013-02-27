program tiDBProxyServer;

uses
  tiLog,
  Forms,
  FMainTIDBProxy in 'FMainTIDBProxy.pas' {FormMainTIDBProxyServer},
  tiQueryRemote_Svr in 'tiQueryRemote_Svr.pas',
  tiDBProxyServerIndyHTTP in 'tiDBProxyServerIndyHTTP.pas',
  tiDBProxyServerStats in 'tiDBProxyServerStats.pas';

{$R *.RES}

begin
  SetupLogForClient(false)  ;

  Application.Initialize;
  Application.CreateForm(TFormMainTIDBProxyServer, FormMainTIDBProxyServer);
  Application.Run;
end.
