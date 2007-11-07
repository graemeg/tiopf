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

  TtiOjectCacheServer = class( TtiOjectCacheAbs )
  private
    FParams: TtiCGIParams ;
    function    ReturnDataFromCache: string;
  protected
    property    Params: TtiCGIParams read FParams;
    function    GetDataAsXML: string ; virtual ; abstract ;
    procedure   Init; override ;
    procedure   RefreshCacheFromDB; override;
    function    DoExecute: string;

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

constructor TtiOjectCacheServer.Create(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string);
var
  LCacheDirectoryRoot: string;
begin
  if ACacheDirectoryRoot <> '' then
    LCacheDirectoryRoot:= ACacheDirectoryRoot
  else
    LCacheDirectoryRoot:= ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + '\..\' + cPathToCachedData);
  inherited Create(LCacheDirectoryRoot);
  FParams := TtiCGIParams.Create;
  if Assigned(AParams) then
    FParams.Assign(AParams);
end;

destructor TtiOjectCacheServer.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TtiOjectCacheServer.DoExecute: string;
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
begin
  if tiWaitForMutex(CachedFileName, word(INFINITE)) then
  try
    Init ;
    gTIOPFManager.DefaultDBConnectionPool.MinPoolSize := 1 ;
    lCachedFileDate := GetCachedFileDate;
    lDatabaseFileDate := GetDBFileDate ;
    if ( lCachedFileDate <> lDatabaseFileDate ) or
       ( not FileExists( GetCachedFileDirAndName )) then
    begin
      RefreshCacheFromDB;
      SetCachedFileDate(lDatabaseFileDate);
    end ;
    result:= ReturnDataFromCache;
  finally
    tiReleaseMutex(CachedFileName);
  end;
end;

class function TtiOjectCacheServer.Execute(const AParams: TtiCGIParams; const ACacheDirectoryRoot: string): string;
var
  L: TtiOjectCacheServer;
begin
  Assert(AParams.TestValid(TtiCGIParams, True), cTIInvalidObjectError);
  L:= Create(AParams, ACacheDirectoryRoot);
  try
    result:= L.DoExecute;
  finally
    L.Free;
  end;
end;

function TtiOjectCacheServer.ReturnDataFromCache: string;
var
  lFileName : string ;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
  Assert(FileExists(lFileName), 'File not found in cache: ' + lFileName );
  result:= tiFileToString(lFileName);
end;

procedure TtiOjectCacheServer.RefreshCacheFromDB;
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

procedure TtiOjectCacheServer.Init;
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

