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
    function Execute(const ACGIExeName: string ;
                     const AParams : string;
                     const AConnectionDetails: TtiWebServerClientConnectionDetails): string; override;
  end ;

implementation
uses
   tiHTTP
  ,tiHTTPMSXML // Remove this when the FlushParams problem is fixed.
  ,tiExcept
  ,SysUtils
  ,tiQueryRemote
  ,tiLog
  ;

{ TtiCGIExtensionRequestDBProxyServer }

function TtiCGIExtensionRequestDBProxyServer.Execute(
  const ACGIExeName: string ;
  const AParams : string;
  const AConnectionDetails: TtiWebServerClientConnectionDetails): string;
var
  LHTTP: TtiHTTPAbs;
  LURL: string;
  LErrorCode: Byte;
begin
  Assert(AConnectionDetails.TestValid, CTIErrorInvalidObject);
  Assert(ACGIExeName<>'', 'pCGIExeName not assigned');

  LURL:= AConnectionDetails.AppServerURL + '/' + ACGIExeName;
  LHTTP:= gTIHTTPFactory.CreateInstance(AConnectionDetails);
  try
    LHTTP.FormatExceptions := False ;
    LHTTP.Input.WriteString(AParams);
    if LHTTP is TtiHTTPMSXML then
      (LHTTP as TtiHTTPMSXML).AutoFlushCache:= False;
    LHTTP.Post(LURL);
    LErrorCode:= LHTTP.ResponseTIOPFErrorCode;
    Result := Trim(LHTTP.Output.DataString);
  finally
    LHTTP.Free;
  end;
  if LErrorCode > 0 then
    raise EtiOPFDataException.CreateFmt(cErrorTIOPFErrorCode, [LURL, Result]);
end;

initialization
  Assert(gCGIExtensionRequestClass=nil, 'gCGIExtensionRequestClass already assigned');
  gCGIExtensionRequestClass:= TtiCGIExtensionRequestDBProxyServer;

end.
