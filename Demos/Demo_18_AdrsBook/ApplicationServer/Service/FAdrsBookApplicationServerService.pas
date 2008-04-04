unit FAdrsBookApplicationServerService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  tiDBProxyServer;

type
  TAddressBookApplicationServer = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
  private
    FAppServer: TtiDBProxyServer;
    public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  AddressBookApplicationServer: TAddressBookApplicationServer;

implementation
uses
  tiOPFManager,
  tiUtils,
  tiLog,
  tiLogToFile,
  tiQueryIBX,  // Link IBX
//  tiHTTPIndy,  // Link Indy
  tiConstants,
  ServicesUtil;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  AddressBookApplicationServer.Controller(CtrlCode);
end;

function TAddressBookApplicationServer.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TAddressBookApplicationServer.ServiceAfterInstall(Sender: TService);
begin
  Log('Service "%s" (%s) installed', [Name, DisplayName]);
  SetServiceDescription(Self,
    'Manages remote database connections to Adrs.fdb through the TechInsite ' +
    'Object Persistence Framework''s (tiOPF) remote persistence layer');
end;

procedure TAddressBookApplicationServer.ServiceAfterUninstall(Sender: TService);
begin
  Log('Service "%s" (%s) uninstalled', [Name, DisplayName]);
end;

procedure TAddressBookApplicationServer.ServiceCreate(Sender: TObject);
begin
  GLog.RegisterLog(TtiLogToFile.CreateWithDateInFileName);
  FAppServer:= TtiDBProxyServer.Create(80);
  FAppServer.ReadPageLocationAtStartup:= False;
  FAppServer.StaticPageLocation:= tiGetEXEPath + '\StaticPages\';
  FAppServer.CGIBinLocation:= tiGetEXEPath + '\CGI-Bin\';
end;

procedure TAddressBookApplicationServer.ServiceDestroy(Sender: TObject);
begin
  FAppServer.Free;
end;

procedure TAddressBookApplicationServer.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  try
    Log(Format('"%s" (%s) service starting...', [Name, DisplayName]));
    Started := False;
    GTIOPFManager.DefaultPersistenceLayerName:= CTIPersistIBX;
    // ToDo: You would want to parameterise this.
    //       Use TtiWebServerConfig as a starting point
    GTIOPFManager.ConnectDatabase(
      'adrs', tiGetEXEPath + '\adrs.fdb', 'SYSDBA', 'masterkey', '', '');
    FAppServer.Start;
    Started := True ;
    Log(Format('"%s" (%s) service started.', [Name, DisplayName]));
  except
    on e: exception do
    begin
      Log(e.message, lsError);
      Log(Format('"%s" (%s) service failed to start.', [Name, DisplayName]), lsError);
      raise;
    end;
  end;
end;

procedure TAddressBookApplicationServer.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  Log(Format('%s (%s) service stopping...', [Name, DisplayName]));
  Stopped := False;
  GTIOPFManager.DisconnectDatabase;
  FAppServer.Stop;
  Stopped := True;
  Log(Format('%s (%s) service stopped.', [Name, DisplayName]));
end;

end.
