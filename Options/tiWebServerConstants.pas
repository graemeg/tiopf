unit tiWebServerConstants;

interface

const
  cDefaultPageName = 'default.htm';
  cDocumentToIgnore = ' HTTP/1.1';
  cDefaultPageText = '<html><h2>TechInsite Web Server - Default Page</h2></html>';
  cErrorInServerExtension = 'Error in server extension: %s '#13#13'%s';

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

  cErrorHTTPCGIExtension         = 'Error in %s.'#13#10#13#10'Error code #%d.'#13#10#13#10'Response text: %s'#13#10;

  cTIDBProxyServerININame             = 'tiDBProxyServer.ini';
  cTIDBProxyServerINIDBSection        = 'DatabaseConnection';
  cTIDBProxyServerINIDBNameIndent     = 'DatabaseName';
  cTIDBProxyServerINIDBUserIndent     = 'UserName';
  cTIDBProxyServerINIDBPasswordIndent = 'Password';

  cgTIDBProxyGetLog          = 'log' ;

  cHTTPContentTypeTextHTML = 'text/html';
  


implementation

end.
