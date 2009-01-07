unit tiDBProxyServerDependencies;

{$I tiDefines.Inc}

interface
uses
   tiDBProxyServer
 ;

procedure ConnectToDatabase;
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
  ,tiExcept
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
  LDatabaseName: string;
  LUserName: string;
  LPassword: string;
  LRetryCount: Word;
  LRetryInterval: Word;
  LTransactionTimeout: Integer;
begin
  LINI:= TtiDBProxyServerConfig.Create;
  try
    if LINI.DatabaseConnectionEnabled then
    begin
      LDatabaseName:= LINI.DatabaseName;
      LUserName:= LINI.UserName;
      LPassword:= LINI.Password;
      LRetryCount:= LINI.RetryCount;
      LRetryInterval:= LINI.RetryInterval;
      LTransactionTimeout:= LINI.TransactionTimeout;
      gStatefulDBConnectionPool.TimeOut := LTransactionTimeout;
      GTIOPFManager.ConnectDatabaseWithRetry(
        LDatabaseName, LUserName, LPassword, LRetryCount, LRetryInterval);
      Log('Transaction timeout: ' + FloatToStr(gStatefulDBConnectionPool.TimeOut) + ' min');
    end;
  finally
    LINI.Free;
  end;
end;

function  gTIDBProxy : TtiDBProxyServer;
begin
  Result := utiDBProxy;
end;

function GetAppServerPort: integer;
var
  LCfg: TtiDBProxyServerConfig;
begin
  LCfg:= TtiDBProxyServerConfig.Create;
  try
    result:= LCfg.Port;
  finally
    LCfg.Free;
  end;
end;

initialization
  utiDBProxy := TtiDBProxyServer.Create(GetAppServerPort);

finalization
  utiDBProxy.Free;


end.

