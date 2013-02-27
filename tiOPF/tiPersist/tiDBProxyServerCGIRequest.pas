unit tiDBProxyServerCGIRequest;

interface
uses
  tiObjAbs
  ;

type
  TtiDBProxyServerCGIRequest = class( TtiObjAbs )
  private
  public
    function Execute(const pURL : string;
                     const pCGIExeName: string ;
                     const pParams : string;
                     const pConnectWith: string;
                     const pProxyServerActive: Boolean;
                     const pProxyServerName: string;
                     const pProxyServerPort: integer ) : string ;
  end ;

implementation
uses
   tiHTTP
  ,SysUtils
  ,tiQueryRemote
  ,cTIPersist
  ;

{ TtiDBProxyServerCGIRequest }

function TtiDBProxyServerCGIRequest.Execute(const pURL : string;
                     const pCGIExeName: string ;
                     const pParams : string;
                     const pConnectWith: string;
                     const pProxyServerActive: Boolean;
                     const pProxyServerName: string;
                     const pProxyServerPort: integer): string;
var
  lHTTP: TtiHTTPAbs;
begin
  Assert(pURL<>'', 'pURL not assigned');
  Assert(pCGIExeName<>'', 'pCGIExeName not assigned');
  Assert(pConnectWith<>'', 'pConnectWith not assigned');
  lHTTP:= gTIHTTPFactory.CreateInstance(pConnectWith);
  try
    if pProxyServerActive then
    begin
      lHTTP.ProxyServer := pProxyServerName;
      lHTTP.ProxyPort := pProxyServerPort;
    end ;
    lHTTP.FormatExceptions := False ;
    lHTTP.Input.WriteString(pParams);
    try
      lHTTP.Post(pURL + '/' + pCGIExeName);
      Result := Trim(lHTTP.Output.DataString);
    except
      on e:Exception do
        raise Exception.Create(lHTTP.ResponseText);
    end;
  finally
    lHTTP.Free;
  end;
end;

end.
