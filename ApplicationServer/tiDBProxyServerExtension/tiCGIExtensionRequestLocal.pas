unit tiCGIExtensionRequestLocal;

interface
uses
  tiCGIExtensionRequest
  ;

const
  cErrorCanNotFindCGIExtension = 'Can not find CGI extension "%s"';

type

  {: Concrete TtiCGIExtensionRequest for shelling out to a CGI Extension.
     Makes direct calls to the CGI.exe - used for unit testing.}
  TtiCGIExtensionRequestLocal = class(TtiCGIExtensionRequest)
  private
    function AddDatabaseConnectionDetailsToParams(const AParams: string): string;

  public
    function Execute(const AULR : string;
                     const ACGIExeName: string ;
                     const AParams : string;
                     const AConnectWith: string;
                     const AProxyServerActive: Boolean;
                     const AProxyServerName: string;
                     const AProxyServerPort: integer ): string; override;
  end ;

var
  gPathToCGIExtensions: string;

implementation
uses
  tiConsoleApp
  ,tiExcept
  ,tiUtils
  ,tiCGIParams
  ,tiConstants
  ,tiOPFManager
  ,SysUtils
  , tiDBConnectionPool;

{ TtiCGIExtensionRequestLocal }

{: Add details of the locally connected database to the CGI params}
function TtiCGIExtensionRequestLocal.AddDatabaseConnectionDetailsToParams(const AParams: string): string;
var
  LParams:TtiCGIParams;
begin
  LParams:= TtiCGIParams.Create;
  try
    LParams.AsCompressedEncodedString:= AParams;
    LParams.Values[cINIIdentDatabaseName]:= gTIOPFManager.DefaultDBConnectionPool.DBConnectParams.DatabaseName;
    LParams.Values[cINIIdentUserName]:= gTIOPFManager.DefaultDBConnectionPool.DBConnectParams.UserName;
    LParams.Values[cINIIdentPassword]:= gTIOPFManager.DefaultDBConnectionPool.DBConnectParams.Password;
    Result:= LParams.AsCompressedEncodedString;
  finally
    LParams.Free;
  end;
end;

function TtiCGIExtensionRequestLocal.Execute(
  const AULR : string;
  const ACGIExeName: string ;
  const AParams : string;
  const AConnectWith: string;
  const AProxyServerActive: Boolean;
  const AProxyServerName: string;
  const AProxyServerPort: integer): string;
var
  LResult: string;
  LCGIEXEName: string;
  LParams: string;
begin
  if gPathToCGIExtensions <> '' then
    LCGIEXEName:= tiAddTrailingSlash(gPathToCGIExtensions) + ACGIEXEName
  else
    LCGIEXEName:= ACGIEXEName;
  LParams:= AddDatabaseConnectionDetailsToParams(AParams);
  if not FileExists(LCGIExeName) then
    raise EtiOPFProgrammerException.CreateFmt(cErrorCanNotFindCGIExtension, [LCGIExeName]);
  LResult:= '';
  tiExecConsoleApp(LCGIExeName, LParams, LResult, nil, false);
  Result:= LResult;
end;

initialization
  Assert(gCGIExtensionRequestClass=nil, 'gCGIExtensionRequestClass already assigned');
  gCGIExtensionRequestClass:= TtiCGIExtensionRequestLocal;
  gPathToCGIExtensions:= tiGetEXEPath;

end.
