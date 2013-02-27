unit ctiDBProxyServerConstants;

interface

const
  cTIDBProxyServerMutex          = 'tiDBProxyServerMutex' ; // So INNO setup asks the user to shutdown first
  cErrorOnServer                 = '<html><body>Error on server: %s</body></html>' ;
  cErrorCanNotFindPage           = '<html><body>Error on server. Can not find page %s</body></html>' ;
  cErrorCanNotFindLogFile        = '<body><html>Can not find log file: s</body></html>' ;
  cErrorOnCallingServerExtension = '<html><body>Error calling server extension: %s<p>Error message: %s</body></html>' ;
  cStaticPageDir                 = 'StaticPages';
  cCGIBinDir                     = 'CGI-Bin';

  cHTTPResponseCodeOK            = 200 ;
  cHTTPResponseCodePageNotFound  = 404 ;
  cHTTPResponseCodeInternalError = 500 ;

  cErrorHTTPCGIExtension         = 'Error in %s. Error code #%d';

implementation

end.
