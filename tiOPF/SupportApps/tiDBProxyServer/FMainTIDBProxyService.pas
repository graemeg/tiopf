unit FMainTIDBProxyService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  ExtCtrls, tiDBProxyServerIndyHTTP, tiRegINI;

type
  TtiDBProxyServer = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
  private
    FINI          : TtiINIFile ;
    FtiDBProxy : TtiDBProxyServerIndyHTTP ;
    FPerLayerName : string ;
    FDatabaseName : string ;
    FUsername     : string ;
    FPassword     : string ;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  tiDBProxyServer: TtiDBProxyServer;

implementation
uses
  cTIPersist
  ,tiUtils
  ,tiPersist
  ,tiDBConnectionPool
  ,tiQueryRemote_Svr
  ,cTIDBProxyServerConstants
  {$IFDEF madexcept} ,madexcept {$ENDIF}
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
  lStaticPageLocation  : string ;
  lTimeOut      : integer ;
begin
  Self.Name := cTIDBProxyServiceName ;

  // Move all this to an object as it's shared between two aps
  FINI          := TtiINIFile.Create( tiAddTrailingSlash(tiGetEXEPath) + 'tiDBProxyServer.ini') ;

  CreateMutex(nil, False, cTIDBProxyServerMutex );
  DisplayName := FINI.ReadString('System', 'ApplicationTitle', cRemoteServerMainFormCaption ) ;

  FPerLayerName        := FINI.ReadString('DatabaseConnection', 'PerLayerName', cTIPersistIBX ) ;
  FDatabaseName        := FINI.ReadString('DatabaseConnection', 'DatabaseName', ExpandFileName('..\Data\Demo.gdb')) ;
  FUserName            := FINI.ReadString('DatabaseConnection', 'UserName', 'SYSDBA' ) ;
  FPassword            := FINI.ReadString('DatabaseConnection', 'Password', 'masterkey' ) ;
  lStaticPageLocation  := FINI.ReadString('System',     'StaticPageLocation', tiAddTrailingSlash( tiGetEXEPath ) + 'StaticPages' ) ;
  lTimeOut             := FINI.ReadInteger('DatabaseConnection', 'TimeOut', 1 ) ;

  gTIPerMgr.TerminateOnFailedDBConnection := false ;
  gTIPerMgr.LoadPersistenceLayer( FPerLayerName ) ;
  gTIPerMgr.DefaultPerLayerName := FPerLayerName ;
  gTIPerMgr.LoadDatabaseLayer( FPerLayerName, FDatabaseName, FUserName, FPassword ) ;

  FtiDBProxy := TtiDBProxyServerIndyHTTP.Create ;
  FtiDBProxy.StaticPageLocation := lStaticPageLocation ;

  gStatefulDBConnectionPool.TimeOut := lTimeOut ;

end;

procedure TtiDBProxyServer.ServiceDestroy(Sender: TObject);
begin
  FtiDBProxy.Free;
  FINI.Free;
end;

end.
