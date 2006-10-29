unit tiCGIObjectCacheServer;

interface
uses
  // tiOPF
  tiCGIObjectCacheAbs
  ,tiDBConnectionPool
  ,tiQuery
  // Delphi
  ,SysUtils
  ,Classes
  ;

type

  TtiCGIOjectCacheServer = class( TtiCGIOjectCacheAbs )
  private
    procedure   ReturnDataFromCache;
  protected
    function    GetDataAsXML: string ; virtual ; abstract ;
    procedure   Init; override ;
    procedure   RefreshCacheFromDB; override;

    // You MAY override these
    function    GetDBFileDataSQL: string ; override ;
    function    CacheDirectory: string; override;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute ;
  end ;

implementation
uses
  // tiOPF
  tiUtils
  ,tiCGIDBConnection
  ,tiOPFManager
  ,tiConstants
  ,tiXML
  ,tiCommandLineParams
  ,tiRegINI
  ,tiLog
  // Delphi
  ;


{ TtiCGIOjectCacheServer }

constructor TtiCGIOjectCacheServer.Create;
begin
  gTIOPFManager.VisMgr.BreakOnException := True ;
  inherited ;
  Params.AsCompressedEncodedString := gCommandLineParams.AsString ;
end;

destructor TtiCGIOjectCacheServer.Destroy;
begin
  inherited;
end;

procedure TtiCGIOjectCacheServer.Execute;
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
begin
  try
    Init ;
    tiCGIConnectToDB;
    gTIOPFManager.DefaultDBConnectionPool.MinPoolSize := 1 ;
    lCachedFileDate := GetCachedFileDate;
    lDatabaseFileDate := GetDBFileDate ;
    if ( lCachedFileDate <> lDatabaseFileDate ) or
       ( not FileExists( GetCachedFileDirAndName )) then
    begin
      RefreshCacheFromDB;
      SetCachedFileDate(lDatabaseFileDate);
    end ;
    ReturnDataFromCache;
  except
    on e:EtiCGIException do
      ExitCode := e.ExitCode;
    on E:Exception do
      raise;
  end ;
end;

procedure TtiCGIOjectCacheServer.ReturnDataFromCache;
var
  lFileName : string ;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
  Assert(FileExists(lFileName), 'File not found in cache: ' + lFileName );
  Write(tiFileToString(lFileName))
end;

procedure TtiCGIOjectCacheServer.RefreshCacheFromDB;
var
  lResult: string ;
  lResultEncode: string;
  lFileName: string;
begin
  Assert(gTIOPFManager.DefaultDBConnectionName <> '', 'No database connection');
  lFileName := GetCachedFileDirAndName;
  lResult := GetDataAsXML;
  lResultEncode := tiCompressEncode(lResult);
  tiStringToFile(lResultEncode,lFileName);
end;

function TtiCGIOjectCacheServer.GetDBFileDataSQL: string;
begin
  Result := 'select max(update_date) as file_date from update_date ' ;
end;

procedure TtiCGIOjectCacheServer.Init;
begin
  if not DirectoryExists(CacheDirectory) then
    ForceDirectories(CacheDirectory);
  if not DirectoryExists(CacheDirectory) then
    raise EtiCGIException.Create(cTICGIExitCodeCanNotCreateCacheDirectory);
  inherited;
end;

function TtiCGIOjectCacheServer.CacheDirectory: string;
begin
  Result:= ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + '\..\CachedData' );
end;

initialization
  // Not a good use of Assert in an initialization, better as a ShowMessage followed
  // by Abort, but that would link in Dialogs, which we want to avoid in the
  // long term.
  Assert( IsConsole, 'tiCGIObjectCache.pas has been included in an Application that is not a Console App' );

end.
