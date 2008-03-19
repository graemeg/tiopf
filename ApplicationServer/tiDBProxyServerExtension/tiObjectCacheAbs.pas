unit tiObjectCacheAbs;

interface
uses
  // tiOPF
  tiBaseObject
  ,tiDBConnectionPool
  ,tiQuery
  ,tiINI
  ,tiCGIParams
  // Delphi
  ,SysUtils
  ;

const
  cDBFileDateFieldName  = 'file_date' ;
  cCacheIndexFileName   = 'CacheIndex.ini' ;
  cCacheIndexINISection = 'CacheIndex';

type

  TtiOjectCacheAbs = class( TtiBaseObject )
  private
    FCacheIndex: TtiINIFile ;
    FCacheDirectoryRoot: String;
  protected
    property    CacheDirectoryRoot: string read FCacheDirectoryRoot;
    // ToDo: Rename to CacheIndexINIFile
    property    CacheIndex   : TtiINIFile read FCacheIndex;
    function    GetCachedFileDirAndName: string; virtual;
    function    MustUpdateCacheFile(const ADatabaseDate, AFileDate: TDateTime): Boolean;

    procedure   Init; virtual;
    function    GetDBFileDate: TDateTime ; virtual ;

    function    GetCachedFileDate: TDateTime ; virtual ;
    procedure   SetCachedFileDate(const AData: TDateTime); virtual;
    function    LockDBConnectionCreateQuery(var ADatabase: TtiDatabase): TtiQuery;

    // You MAY override these
    function    CacheDirectory: string ; virtual ;

    // You MUST override these
    procedure   RefreshCacheFromDB(const ACacheFileDate: TDateTime); virtual ; abstract ;
    function    CachedFileName: string ; virtual ;
    function    GetDBFileDataSQL: string ; virtual ;

  public
    constructor Create(const ACacheDirectoryRoot: string) ; virtual ;
    destructor  Destroy ; override ;
  end ;

implementation
uses
  //tiOPF
   tiOPFManager
  ,tiUtils
  ,tiExcept
  ,tiCGIExcept
  ,tiWebServerConstants
  ,tiConstants
  ;

{ TtiCGIOjectCacheAbs }

constructor TtiOjectCacheAbs.Create(const ACacheDirectoryRoot: string);
begin
  inherited Create;
  if ACacheDirectoryRoot <> '' then
    FCacheDirectoryRoot:= ACacheDirectoryRoot
  else
    FCacheDirectoryRoot:= ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) + PathDelim + CPathToCachedData);
end;

destructor TtiOjectCacheAbs.Destroy;
begin
  FCacheIndex.Free ;
  inherited;
end;

function TtiOjectCacheAbs.GetCachedFileDate: TDateTime;
var
  lFileName: string;
begin
  Assert( FCacheIndex <> nil, 'FCacheIndex not assigned');
  lFileName := CachedFileName;
  Assert( lFileName <> '', 'CachedFileName not assigned');
  lFileName := ChangeFileExt(ExtractFileName(lFileName),'');
  Result := FCacheIndex.ReadDateTime(cCacheIndexINISection, lFileName, 0);
end;

procedure TtiOjectCacheAbs.SetCachedFileDate(const AData: TDateTime);
var
  lFileName: string;
begin
  Assert( FCacheIndex <> nil, 'FCacheIndex not assigned');
  lFileName := CachedFileName;
  Assert( lFileName <> '', 'CachedFileName not assigned');
  lFileName := ChangeFileExt(ExtractFileName(lFileName),'');
  FCacheIndex.WriteDateTime(cCacheIndexINISection, lFileName, AData);
  tiSetFileDate(GetCachedFileDirAndName, AData);
end;

function TtiOjectCacheAbs.CachedFileName: string;
begin
  Assert(False, 'CachedFileName not overridden in ' + ClassName);
end;

function TtiOjectCacheAbs.CacheDirectory: string;
begin
  Result:= FCacheDirectoryRoot;
end;

function TtiOjectCacheAbs.LockDBConnectionCreateQuery(
  var ADatabase: TtiDatabase ): TtiQuery;
begin
  ADatabase := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock ;
  result := ADatabase.CreateTIQuery;
  result.AttachDatabase(ADatabase);
end;

function TtiOjectCacheAbs.MustUpdateCacheFile(const ADatabaseDate,
  AFileDate: TDateTime): Boolean;
begin
  result:=
    (ADatabaseDate <> AFileDate) or
    (not FileExists(GetCachedFileDirAndName));
end;

function TtiOjectCacheAbs.GetDBFileDataSQL: string;
begin
  Assert(False, 'GetDBFileDataSQL not overridden in ' + ClassName);
end;

function TtiOjectCacheAbs.GetCachedFileDirAndName: string;
begin
  Assert( tiExtractExtension( CachedFileName ) <> '', 'CachedFileName missing a file extension');
  Result := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
end;

function TtiOjectCacheAbs.GetDBFileDate: TDateTime;
var
  LQuery    : TtiQuery;
  LDatabase : TtiDatabase;
begin
  LQuery := LockDBConnectionCreateQuery(LDatabase);
  try
    LQuery.SQLText := GetDBFileDataSQL ;
    LDatabase.StartTransaction;
    try
      LQuery.Open;
      try
        if LQuery.EOF then
        begin
          Result := 0 ;
          raise EtiCGIException.Create(cTICGIExitCodeNoDataReturnedFromGetLatestDateQuery);
        end;
        Result := LQuery.FieldAsDateTime[cDBFileDateFieldName];
      finally
        LQuery.Close ;
      end;
    finally
      LDatabase.Commit;
    end;
  finally
    LQuery.Free;
    GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(LDatabase) ;
  end;
end;

procedure TtiOjectCacheAbs.Init;
var
  lFileName : string;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + cCacheIndexFileName;
  FCacheIndex := TtiINIFile.Create(lFileName);
end;

end.

