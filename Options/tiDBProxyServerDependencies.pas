unit tiDBProxyServerDependencies;

{$I tiDefines.Inc}

interface
uses
   tiDBProxyServer
  ,tiRegINI
  ;

procedure ConnectToDatabase;
function  GetDBConnectionMessage:string ;
function  GetDBProxyServerApplicationTitle: string;
function  ProxyServerINI: TtiINIFile;
function  gTIDBProxy: TtiDBProxyServer ;

implementation
uses
   SysUtils
  ,Windows
  ,tiUtils
  ,tiWebServerConstants
  ,tiConstants
  ,tiLog
  ,tiOPFManager
  ,tiQueryRemote_Svr
  ,tiDBProxyServerStats
  ,tiOIDGUID
  {$IFDEF madExcept}
  ,madExcept
  ,madLinkDisAsm
  {$ENDIF}
  ;

var
  uINI          : TtiINIFile ;
  utiDBProxy : TtiDBProxyServer ;

function  ProxyServerINI: TtiINIFile;
begin
  Result := uINI;
end;

procedure ConnectToDatabase;
var
  lDatabaseName : string ;
  lUsername     : string ;
  lPassword     : string ;
  lTimeOut      : integer ;
begin
  lDatabaseName        := uINI.ReadString('DatabaseConnection', 'DatabaseName', ExpandFileName('..\_Data\Demo.gdb')) ;
  lUserName            := uINI.ReadString('DatabaseConnection', 'UserName', 'SYSDBA' ) ;
  lPassword            := uINI.ReadString('DatabaseConnection', 'Password', 'masterkey' ) ;

  gTIOPFManager.TerminateOnFailedDBConnection := false ;
  gTIOPFManager.ConnectDatabase( lDatabaseName, lUserName, lPassword) ;

  lTimeOut := uINI.ReadInteger('DatabaseConnection', 'TimeOut', 1 ) ;
  gStatefulDBConnectionPool.TimeOut := lTimeOut ;

  Log(GetDBConnectionMessage);

end ;

function GetDBConnectionMessage: string;
begin
  result :=
    gTIOPFManager.DefaultDBConnectionPool.DetailsAsString + Cr(2) +
    'Transaction timeout:     ' + FloatToStr( gStatefulDBConnectionPool.TimeOut ) + ' min';
  Result := Result + Cr(2) + 'Static pages read from: ' + uTIDBProxy.StaticPageLocation;
end;

function GetDBProxyServerApplicationTitle: string;
begin
  uINI.ReadString('System', 'ApplicationTitle', cRemoteServerMainFormCaption )
end;

function  gTIDBProxy : TtiDBProxyServer ;
begin
  Result := utiDBProxy;
end;

initialization
  uINI          := TtiINIFile.Create( tiAddTrailingSlash(tiGetEXEPath) + cTIDBProxyServerININame) ;
  utiDBProxy := TtiDBProxyServer.Create(80);

finalization
  uINI.Free;
  utiDBProxy.Free;


end.

