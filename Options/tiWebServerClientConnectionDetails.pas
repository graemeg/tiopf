{
    This file is part of the tiOPF project.

    See the file license.txt, included in this distribution,
    for details about redistributing tiOPF.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines a class that keeps client connection details
      to a web server.
}
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
    FLongword: Longword;
    FRetryLimit: byte;
  public
    constructor Create;
    property AppServerURL: string read FAppServerURL write FAppServerURL;
    property ConnectWith: string read FConnectWith write FConnectWith;
    property ProxyServerActive: Boolean read FProxyServerActive write FProxyServerActive;
    property ProxyServerName: string read FProxyServerName write FProxyServerName;
    property ProxyServerPort: Integer read FProxyServerPort write FProxyServerPort;
    property BlockSize: Longword read FLongword write FLongword;
    property RetryLimit: byte read FRetryLimit write FRetryLimit;

    procedure Assign(const ASource: TtiWebServerClientConnectionDetails);
    function  Equals(const ACompareWith: TtiWebServerClientConnectionDetails): boolean; reintroduce;

  end;

implementation
uses
  tiConstants
  ;

{ TtiWebServerClientConnectionDetails }

procedure TtiWebServerClientConnectionDetails.Assign(
  const ASource: TtiWebServerClientConnectionDetails);
begin
  Assert(ASource.TestValid, CTIErrorInvalidObject);
  AppServerURL:=      ASource.AppServerURL;
  ConnectWith:=       ASource.ConnectWith;
  ProxyServerActive:= ASource.ProxyServerActive;
  ProxyServerName:=   ASource.ProxyServerName;
  ProxyServerPort:=   ASource.ProxyServerPort;
  BlockSize:=         ASource.BlockSize;
  RetryLimit:=        ASource.RetryLimit;
end;

constructor TtiWebServerClientConnectionDetails.Create;
begin
  inherited Create;
  FRetryLimit:= 1;
end;

function TtiWebServerClientConnectionDetails.Equals(
  const ACompareWith: TtiWebServerClientConnectionDetails): boolean;
begin
  result:=
    (AppServerURL = ACompareWith.AppServerURL) and
    (ConnectWith = ACompareWith.ConnectWith) and
    (ProxyServerActive = ACompareWith.ProxyServerActive) and
    (ProxyServerName = ACompareWith.ProxyServerName) and
    (ProxyServerPort = ACompareWith.ProxyServerPort) and
    (BlockSize = ACompareWith.BlockSize) and
    (RetryLimit = ACompareWith.RetryLimit);
end;

end.

