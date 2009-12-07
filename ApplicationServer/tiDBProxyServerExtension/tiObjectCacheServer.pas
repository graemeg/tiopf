unit tiObjectCacheServer;

interface
uses
  // tiOPF
  tiObjectCacheAbs
  ,tiCGIParams
  ,tiDBConnectionPool
  ,tiQuery
  // Delphi
  ,SysUtils
  ,Classes
  ;

type

  TtiObjectCacheServer = class( TtiObjectCacheAbs )
  private
    FParams: TtiCGIParams ;
  protected
    property    Params: TtiCGIParams read FParams;
    // ToDo: Rename GetDataAsXML -> GetDataAsString
    function    GetDataAsXML: string ; virtual ; abstract ;
    procedure   Init; override ;
    procedure   RefreshCacheFromDB(const ACacheFileDate: TDateTime); override;
    function    ReturnDataFromCache: string; virtual;
    function    DoExecute: string; virtual;

    // You MAY override these
    //function    GetDBFileDataSQL: string ; override ;
    //Result := 'select max(Field_Name) as file_date from Table_Name ' ;
    //function    CacheDirectory: string; override;

  public
    constructor Create(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string); reintroduce; virtual;
    destructor  Destroy ; override ;
    class function Execute(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string): string;
  end ;

function  tiGetCGIAppINIFileName: string;

implementation
uses
  // tiOPF
  tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiXML
  ,tiCommandLineParams
  ,tiINI
  ,tiLog
  ,tiCGIExcept
  ,tiWebServerConstants
  ,tiDBProxyServerConfig
  ,tiSyncObjs
  ,Windows
  // Delphi
  ;

const
  cErrorReadingDBConnectionFromINI = 'Database connection details not found in <%s> Looking for [%s] %s, %s, %s';

function tiGetCGIAppINIFileName: string;
var
  LEXEName: string;
begin
  LEXEName:= tiSwapExt(ParamStr(0), 'ini');
  Result :=
    ExpandFileName(
      tiAddTrailingSlash(ExtractFilePath(LEXEName)) +
      '..\' +
      ExtractFileName(LEXEName));
end;

{ TtiCGIOjectCacheServer }

constructor TtiObjectCacheServer.Create(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string);
var
  LCacheDirectoryRoot: string;
begin
  if ACacheDirectoryRoot <> '' then
    LCacheDirectoryRoot:= ACacheDirectoryRoot
  else
    LCacheDirectoryRoot:= ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + '\..\' + CPathToCachedDataRoot);
  inherited Create(LCacheDirectoryRoot);
  FParams := TtiCGIParams.Create;
  if Assigned(AParams) then
    FParams.Assign(AParams);
end;

destructor TtiObjectCacheServer.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TtiObjectCacheServer.DoExecute: string;
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
  LStart: DWord;
begin
  LStart:= GetTickCount;
  if tiWaitForMutex(CachedFileName, word(INFINITE)) then
  try
    Init ;
    lCachedFileDate := GetCachedFileDate;
    lDatabaseFileDate := GetDBFileDate ;
    Log('Reading "' + CachedFileName + '"');
    Log('  File directory "' + CacheDirectory);
    Log('  File name "' + CachedFileName);
    Log('  Database date "' + tiDateTimeToStr(LDatabaseFileDate) + '"');
    Log('  Cache date "' + tiDateTimeToStr(LCachedFileDate) + '"');
    if MustUpdateCacheFile(lCachedFileDate, lDatabaseFileDate) then
    begin
      Log('  File WILL be refreshed (' + tiIntToCommaStr(GetTickCount - LStart) + 'ms)');
      LStart:= GetTickCount;
      RefreshCacheFromDB(lDatabaseFileDate);
    end else
      Log('  File WILL NOT be refreshed (' + tiIntToCommaStr(GetTickCount - LStart) + 'ms)');
    result:= ReturnDataFromCache;
    Log('Done reading "' + CachedFileName + '" (' + tiIntToCommaStr(GetTickCount - LStart) + 'ms)');
  finally
    tiReleaseMutex(CachedFileName);
  end;
end;

class function TtiObjectCacheServer.Execute(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string): string;
var
  L: TtiObjectCacheServer;
begin
  Assert(AParams.TestValid(TtiCGIParams, True), CTIErrorInvalidObject);
  L:= Create(AParams, ACacheDirectoryRoot);
  try
    result:= L.DoExecute;
  finally
    L.Free;
  end;
end;

function TtiObjectCacheServer.ReturnDataFromCache: string;
var
  lFileName : string ;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
  Assert(FileExists(lFileName), 'File not found in cache: ' + lFileName );
  result:= tiFileToString(lFileName);
end;

procedure TtiObjectCacheServer.RefreshCacheFromDB(const ACacheFileDate: TDateTime);
var
  LResult: string ;
  LResultEncode: string;
  LFileName: string;
begin
  Assert(GTIOPFManager.DefaultDBConnectionName <> '', 'No database connection');
  LFileName := GetCachedFileDirAndName;
  LResult := GetDataAsXML;
  LResultEncode := tiCompressEncode(LResult);
  tiStringToFile(LResultEncode,LFileName);
  SetCachedFileDate(ACacheFileDate);
end;

procedure TtiObjectCacheServer.Init;
var
  LPath: string;
begin
  LPath:= CacheDirectory;
  if not DirectoryExists(LPath) then
    ForceDirectories(LPath);
  if not DirectoryExists(LPath) then
    raise EtiCGIException.Create(cTICGIExitCodeCanNotCreateCacheDirectory);
  inherited;
end;

end.
