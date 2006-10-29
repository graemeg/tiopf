unit tiDBProxyServerDependencies;

{$I tiDefines.Inc}

interface
uses
   tiDBProxyServer
 ;

procedure ConnectToDatabase;
function  GetDBConnectionMessage:string;
function  gTIDBProxy: TtiDBProxyServer;

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
  ,tiDBProxyServerConfig
  ,tiOIDGUID
  {$IFDEF madExcept}
  ,madExcept
  ,madLinkDisAsm
  {$ENDIF}
 ;

var
  utiDBProxy : TtiDBProxyServer;

procedure ConnectToDatabase;
var
  LINI: TtiDBProxyServerConfig;
  lDatabaseName : string;
  lUsername    : string;
  lPassword    : string;
  lTimeOut     : integer;
begin
  LINI:= TtiDBProxyServerConfig.Create;
  try
    lDatabaseName       := LINI.DatabaseName;
    lUserName           := LINI.UserName;
    lPassword           := LINI.Password;
    lTimeOut            := LINI.TransactionTimeout;
  finally
    LINI.Free;
  end;

  gTIOPFManager.TerminateOnFailedDBConnection := false;
  gTIOPFManager.ConnectDatabase(lDatabaseName, lUserName, lPassword);

  gStatefulDBConnectionPool.TimeOut := lTimeOut;

  Log(GetDBConnectionMessage);

end;

function GetDBConnectionMessage: string;
begin
  result :=
    gTIOPFManager.DefaultDBConnectionPool.DetailsAsString + Cr(2) +
    'Transaction timeout:     ' + FloatToStr(gStatefulDBConnectionPool.TimeOut) + ' min';
  Result := Result + Cr(2) + 'Static pages read from: ' + uTIDBProxy.StaticPageLocation;
end;

function  gTIDBProxy : TtiDBProxyServer;
begin
  Result := utiDBProxy;
end;

initialization
  utiDBProxy := TtiDBProxyServer.Create(80);

finalization
  utiDBProxy.Free;


end.

