unit tiWebServerConstants;

interface
uses
  tiConstants
  ;

const
  cDefaultPageName = 'default.htm';
  cDocumentToIgnore = ' HTTP/1.1';
  cDefaultPageText = '<html><h2>TechInsite Web Server - Default Page</h2></html>';
  cErrorInServerExtension = 'Error in server extension: %s '+cLineEnding+'%s';
  cPassThroughContentFilePrefix = 'PassThroughContentFile:';

  cTIDBProxyServerMutex          = 'tiDBProxyServerMutex'; // So INNO setup asks the user to shutdown first
  cErrorOnServer                 = '<html><body>Error on server: %s</body></html>';
  cErrorCanNotFindPage           = '<html><body>Error on server. Can not find page %s</body></html>';
  cErrorCanNotFindLogFile        = '<body><html>Can not find log file: s</body></html>';
  cErrorOnCallingServerExtension = '<html><body>Error calling server extension: %s<p>Error message: %s</body></html>';
  cStaticPageDir                 = 'StaticPages';
  cCGIBinDir                     = 'cgi-bin'; // DO NOT CHANGE THE CASE

  cHTTPResponseCodeOK            = 200;                       
  cHTTPResponseCodePageNotFound  = 404;
  cHTTPResponseCodeInternalError = 500;

  cErrorHTTPCGIExtension         = 'Error in %s.'+cLineEnding+cLineEnding+
      'Error code #%d.'+cLineEnding+cLineEnding+'Response text: %s'+cLineEnding;

  cgTIDBProxyGetLog          = 'log';
  CTIDBProxyForceException   = 'ForceException';
  CTIDBProxyForceExceptionThread   = 'ForceExceptionThread';

  cHTTPContentTypeTextHTML = 'text/html';
  cHTTPContentTypeApplicationZip = 'application/x-zip-compressed';

  cTICGIExitCodeOK                                   = 0;
  cTICGIExitCodeUnknownException                     = 2000 ;
  cTICGIExitCodeCanNotCreateCacheDirectory           = 2001 ;
  cTICGIExitCodeNoDataReturnedFromGetLatestDateQuery = 2002 ;
  cTICGIExitCodeInvalidParameterToCGIExtension       = 2003 ;


implementation

end.
