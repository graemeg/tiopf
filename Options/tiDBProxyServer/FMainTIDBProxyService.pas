unit FMainTIDBProxyService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  ExtCtrls;

const
  cINISection         = 'System';
  cINIIdent           = 'ApplicationTitle';
  cDefaultDisplayName = 'TechInsite Database Proxy Service';

type
  TtiDBProxyServer = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  tiDBProxyServer: TtiDBProxyServer;

implementation
uses
  tiConstants
  ,tiUtils
  ,tiOPFManager
  ,tiDBConnectionPool
  ,tiQueryRemote_Svr
  ,tiWebServerConstants
  {$IFDEF madexcept} ,madexcept {$ENDIF}
  ,tiDBProxyServerDependencies
  ,ServicesUtil
  ,tiLog

  ;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  tiDBProxyServer.Controller(CtrlCode);
end;

function TtiDBProxyServer.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TtiDBProxyServer.ServiceCreate(Sender: TObject);
begin
  Self.Name := cTIDBProxyServiceName ;
  Self.DisplayName := ProxyServerINI.ReadString(cINISection, cINIIdent, cDefaultDisplayName);
end;

procedure TtiDBProxyServer.ServiceAfterInstall(Sender: TService);
begin
  SetServiceDescription(Self,
    'Manages remote database connections through the TechInsite Object Persistence Framework''s' +
    ' (tiOPF) remote persistence layer');
end;

procedure TtiDBProxyServer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Log(Format('%s (%s) service starting...', [Name, DisplayName]));
  Started := False;
  ConnectToDatabase;
  gTIDBProxy.Start;
  Started := True ;
  Log(Format('%s (%s) service started.', [Name, DisplayName]));
end;

procedure TtiDBProxyServer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Log(Format('%s (%s) service stopping...', [Name, DisplayName]));
  // ToDo: Will have to do better than this. Should wait for database activity to end. Then shut down.
  Stopped := False;
  gTIDBProxy.Stop;
  gTIOPFManager.Terminate;
  FreeAndNilTIPerMgr;
  Stopped := True;
  Log(Format('%s (%s) service stopped.', [Name, DisplayName]));
end;

end.
