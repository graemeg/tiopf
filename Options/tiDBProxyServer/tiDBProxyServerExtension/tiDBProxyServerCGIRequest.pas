unit tiDBProxyServerCGIRequest;

interface
uses
  tiBaseObject
  ;

type
  TtiDBProxyServerCGIRequest = class( TtiBaseObject )
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
  ,tiHTTPMSXML // Remove this when the FlushParams problem is fixed. 
  ,SysUtils
  ,tiQueryRemote
  ,tiConstants
  ,tiLog
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
//  Log(ClassName + '.Execute');
//  Log('    CGIExeFileName ' +     pCGIExeName );
//  Log('    Params ' +             pParams );
//  Log('    ConnectWith ' +       pConnectWith );
//  Log('    ProxyServerActive ' + IntToStr(Ord(pProxyServerActive)));
//  Log('    ProxyServerName ' +    (pProxyServerName));
//  Log('    ProxyServerPort ' +    IntToStr(pProxyServerPort));

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
      if lHTTP is TtiHTTPMSXML then
        (lHTTP as TtiHTTPMSXML).AutoFlushCache:= False;
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
