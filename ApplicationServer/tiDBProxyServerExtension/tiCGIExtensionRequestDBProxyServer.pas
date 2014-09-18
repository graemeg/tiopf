unit tiCGIExtensionRequestDBProxyServer;

interface
uses
  tiCGIExtensionRequest
  ,tiWebServerClientConnectionDetails
  ,tiConstants
  ;

const
  cErrorTIOPFErrorCode = 'Error executing command on application server: "%s"' + cLineEnding + 'Message: "%s"';
  cErrorExecutingHTTPPost = 'Error calling %s' + #13#10 + 'Message: %s' + cLineEnding + 'Response: %s';

type

  {: Concrete TtiCGIExtensionRequest for shelling out to a CGI Extension.
     Makes calls to the CGI.exe via a tiDBProxyServer - used for deployed apps.}
  TtiCGIExtensionRequestDBProxyServer = class(TtiCGIExtensionRequest)
  public
    function Execute(const ACGIExeName: string;
                     const AParams: string;
                     const AConnectionDetails: TtiWebServerClientConnectionDetails): string; override;
  end ;

implementation
uses
   tiHTTP
  ,tiExcept
  ,SysUtils
  ,tiQueryRemote
  ,tiLog
  ,tiWebServerConstants
  ;

{ TtiCGIExtensionRequestDBProxyServer }

function TtiCGIExtensionRequestDBProxyServer.Execute(
  const ACGIExeName: string;
  const AParams: string;
  const AConnectionDetails: TtiWebServerClientConnectionDetails): string;
var
  LHTTP: TtiHTTPAbs;
  LURL: string;
  LErrorCode: Byte;
begin
  Assert(AConnectionDetails.TestValid, CTIErrorInvalidObject);
  Assert(ACGIExeName <> '', 'CGIExeName not assigned');

  LURL:= AConnectionDetails.AppServerURL + '/' + ACGIExeName;
  LHTTP:= gTIHTTPFactory.CreateInstance(AConnectionDetails);
  try
    LHTTP.FormatExceptions := False;
    LHTTP.Input.WriteString(AParams);
    LHTTP.AutoFlushCache := False;
    LHTTP.ExpectResponseBlockHeader := true;
    Log('CGI extension request: [%s]', [LURL], lsDebug);
    try
      LHTTP.Post(LURL);
      if LHTTP.ResponseCode <> cHTTPResponseCodeOK then
        raise Exception.CreateFmt(CErrorHTTPServer, [LHTTP.ResponseCode]);
    except
      on e:exception do
      begin
        Log('CGI extension request: ERROR [%s]', [e.message], lsWarning);
        raise EtiOPFHTTPException.Create(e.message);
      end;
    end;
    LErrorCode := LHTTP.ResponseTIOPFErrorCode;
    Result := Trim(LHTTP.Output.DataString);
    Log('CGI extension response: response code=%d, error code=%d, expected length=%d, actual length=%d',
        [LHTTP.ResponseCode, LErrorCode,
         StrToIntDef(LHTTP.ResponseHeader['Content-Length'], 0),
         LHTTP.Output.Size], lsDebug);
    if LErrorCode > 0 then
      raise EtiOPFDataException.CreateFmt(cErrorTIOPFErrorCode, [LURL, Result]);
  finally
    LHTTP.Free;
  end;
end;

initialization
  Assert(gCGIExtensionRequestClass=nil, 'gCGIExtensionRequestClass already assigned');
  gCGIExtensionRequestClass:= TtiCGIExtensionRequestDBProxyServer;

end.
