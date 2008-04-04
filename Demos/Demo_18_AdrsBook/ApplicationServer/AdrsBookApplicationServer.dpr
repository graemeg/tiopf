program AdrsBookApplicationServer;

uses
  FastMM4,
  tiLog,
  tiLogToFile,
  SvcMgr,
  FAdrsBookApplicationServerService in 'FAdrsBookApplicationServerService.pas' {FormAdrsBookApplicationServer: TService},
  FMainTIDBProxyService in '..\..\..\ApplicationServer\FMainTIDBProxyService.pas' {tiDBProxyServer: TService};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormAdrsBookApplicationServer, FormAdrsBookApplicationServer);
  Application.CreateForm(TtiDBProxyServer, tiDBProxyServer);
  Application.Run;
end.
