unit FMainTIDBProxyService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  ExtCtrls;

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
  ,tiWebServerConfig
  {$IFDEF madexcept} ,madexcept {$ENDIF}
  ,tiDBProxyServerDependencies
  ,ServicesUtil
  ,tiLog
  ,tiLogToFile

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
var
  LConfig: TtiWebServerConfig;
begin
  LConfig:= TtiWebServerConfig.Create;
  try
    LConfig.RegisterLog;
    Self.Name:= LConfig.WebServiceShortName;
    Self.DisplayName:= LConfig.WebServiceDisplayName;
  finally
    LConfig.Free;
  end;
end;

procedure TtiDBProxyServer.ServiceAfterInstall(Sender: TService);
begin
  SetServiceDescription(Self,
    'Manages remote database connections through the TechInsite Object Persistence Framework''s' +
    ' (tiOPF) remote persistence layer');
end;

procedure TtiDBProxyServer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  try
    Log(Format('%s (%s) service starting...', [Name, DisplayName]));
    Started := False;
    ConnectToDatabase;
    gTIDBProxy.Start;
    Started := True ;
    Log(Format('%s (%s) service started.', [Name, DisplayName]));
  except
    on e: exception do
    begin
      Log(e.message, lsError);
      Log(Format('%s (%s) service failed to start.', [Name, DisplayName]), lsError);
      raise;
    end;
  end;
end;

procedure TtiDBProxyServer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Log(Format('%s (%s) service stopping...', [Name, DisplayName]));
  // ToDo: Will have to do better than this. Should wait for database activity to end. Then shut down.
  Stopped := False;
  GTIOPFManager.Terminate;
  gTIDBProxy.Stop;
  FreeAndNilTIPerMgr;
  Stopped := True;
  Log(Format('%s (%s) service stopped.', [Name, DisplayName]));
end;

end.
