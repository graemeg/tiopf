unit tiWebServerClientConnectionDetails;

interface
uses
   tiBaseObject
 ;

type

  TtiWebServerClientConnectionDetails = class;

  TtiWebServerClientConnectionDetails = class(TtiBaseObject)
  private
    FConnectWith: string;
    FProxyServerPort: Integer;
    FProxyServerActive: Boolean;
    FAppServerURL: string;
    FProxyServerName: string;
  public
    property AppServerURL: string read FAppServerURL write FAppServerURL;
    property ConnectWith: string read FConnectWith write FConnectWith;
    property ProxyServerActive: Boolean read FProxyServerActive write FProxyServerActive;
    property ProxyServerName: string read FProxyServerName write FProxyServerName;
    property ProxyServerPort: Integer read FProxyServerPort write FProxyServerPort;

    procedure Assign(const ASource: TtiWebServerClientConnectionDetails);
    function  Equals(const ACompareWith: TtiWebServerClientConnectionDetails): boolean;

  end;

implementation
uses
  tiConstants
  ;

{ TtiWebServerClientConnectionDetails }

procedure TtiWebServerClientConnectionDetails.Assign(
  const ASource: TtiWebServerClientConnectionDetails);
begin
  Assert(ASource.TestValid, cTIInvalidObjectError);
  AppServerURL:=      ASource.AppServerURL;
  ConnectWith:=       ASource.ConnectWith;
  ProxyServerActive:= ASource.ProxyServerActive;
  ProxyServerName:=   ASource.ProxyServerName;
  ProxyServerPort:=   ASource.ProxyServerPort;
end;

function TtiWebServerClientConnectionDetails.Equals(
  const ACompareWith: TtiWebServerClientConnectionDetails): boolean;
begin
  result:=
    (AppServerURL = ACompareWith.AppServerURL) and
    (ConnectWith = ACompareWith.ConnectWith) and
    (ProxyServerActive = ACompareWith.ProxyServerActive) and
    (ProxyServerName = ACompareWith.ProxyServerName) and
    (ProxyServerPort = ACompareWith.ProxyServerPort);
end;

end.

