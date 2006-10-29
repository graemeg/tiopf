unit tiCGIObjectCacheAbs;

interface
uses
  // tiOPF
  tiBaseObject
  ,tiDBConnectionPool
  ,tiQuery
  ,tiRegINI
  ,tiCGIParams
  // Delphi
  ,SysUtils
  ;

const
  cTICGIExitCodeCanNotCreateCacheDirectory           = 2001 ;
  cTICGIExitCodeNoDataReturnedFromGetLatestDateQuery = 2002 ;
  cTICGIExitCodeInvalidParameterToCGIExtension       = 2003 ;
  cDBFileDateFieldName  = 'file_date' ;
  cCacheIndexFileName   = 'CacheIndex.ini' ;
  cCacheIndexINISection = 'CacheIndex';

type

  EtiCGIException = class( Exception )
  private
    FExitCode: Integer;
  public
    constructor Create(pExitCode: Integer; const pMessage: string = '');
    property    ExitCode: Integer read FExitCode;
  end ;

  TtiCGIOjectCacheAbs = class( TtiBaseObject )
  private
    FCacheIndex   : TtiINIFile ;
    FParams       : TtiCGIParams ;

  protected
    property    Params: TtiCGIParams read FParams;
    property    CacheIndex   : TtiINIFile read FCacheIndex;
    function    GetCachedFileDirAndName: string;

    procedure   Init; virtual;
    function    GetDBFileDate: TDateTime ; virtual ;

    function    GetCachedFileDate: TDateTime ; virtual ;
    procedure   SetCachedFileDate(const pData: TDateTime); virtual;
    function    LockDBConnectionCreateQuery( var pPooledDB : TPooledDB; var pDatabase: TtiDatabase): TtiQuery;

    // You MAY override these
    function    CacheDirectory: string ; virtual ;

    // You MUST override these
    procedure   RefreshCacheFromDB; virtual ; abstract ;
    function    CachedFileName: string ; virtual ;
    function    GetDBFileDataSQL: string ; virtual ;

  public
    constructor Create ; virtual ;
    destructor  Destroy ; override ;
  end ;

implementation
uses
  //tiOPF
  tiOPFManager
  ,tiUtils
  // Delphi
  ,tiExcept
  ;

{ TtiCGIOjectCacheAbs }

constructor TtiCGIOjectCacheAbs.Create;
begin
  inherited ;
  FParams := TtiCGIParams.Create;
end;

destructor TtiCGIOjectCacheAbs.Destroy;
begin
  FCacheIndex.Free ;
  FParams.Free;
  inherited;
end;

function TtiCGIOjectCacheAbs.GetCachedFileDate: TDateTime;
var
  lFileName: string;
begin
  Assert( FCacheIndex <> nil, 'FCacheIndex not assigned');
  lFileName := CachedFileName;
  Assert( lFileName <> '', 'CachedFileName not assigned');
  lFileName := ChangeFileExt(ExtractFileName(lFileName),'');
  Result := FCacheIndex.ReadDateTime(cCacheIndexINISection, lFileName, 0);
end;

procedure TtiCGIOjectCacheAbs.SetCachedFileDate(const pData: TDateTime);
var
  lFileName: string;
begin
  Assert( FCacheIndex <> nil, 'FCacheIndex not assigned');
  lFileName := CachedFileName;
  Assert( lFileName <> '', 'CachedFileName not assigned');
  lFileName := ChangeFileExt(ExtractFileName(lFileName),'');
  FCacheIndex.WriteDateTime(cCacheIndexINISection, lFileName, pData);
  tiSetFileDate(GetCachedFileDirAndName, pData);
end;

function TtiCGIOjectCacheAbs.CachedFileName: string;
begin
  Assert(False, 'CachedFileName not overridden in ' + ClassName);
end;

function TtiCGIOjectCacheAbs.CacheDirectory: string;
begin
  // Move the suffix to a constant
  Result:= ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) + '\CachedData');
end;

function TtiCGIOjectCacheAbs.LockDBConnectionCreateQuery(
            var pPooledDB : TPooledDB ; var pDatabase: TtiDatabase ): TtiQuery;
begin
  pPooledDB := gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock ;
  pDatabase := pPooledDB.Database ;
  result := pDatabase.CreateTIQuery;
  result.AttachDatabase(pDatabase);
end;

function TtiCGIOjectCacheAbs.GetDBFileDataSQL: string;
begin
  Assert(False, 'GetDBFileDataSQL not overridden in ' + ClassName);
end;

function TtiCGIOjectCacheAbs.GetCachedFileDirAndName: string;
begin
  Assert( tiExtractExtension( CachedFileName ) <> '', 'CachedFileName missing a file extension');
  Result := tiAddTrailingSlash(CacheDirectory) + CachedFileName ;
end;

function TtiCGIOjectCacheAbs.GetDBFileDate: TDateTime;
var
  lPooledDB : TPooledDB ;
  lQuery    : TtiQuery;
  lDatabase : TtiDatabase;
begin
  lQuery := LockDBConnectionCreateQuery(lPooledDB, lDatabase);
  try
    lQuery.SQLText := GetDBFileDataSQL ;
    lDatabase.StartTransaction;
    try
      lQuery.Open;
      try
        if lQuery.EOF then
        begin
          Result := 0 ;
          raise EtiCGIException.Create(cTICGIExitCodeNoDataReturnedFromGetLatestDateQuery);
        end;
        Result := lQuery.FieldAsDateTime[cDBFileDateFieldName];
      finally
        lQuery.Close ;
      end;
    finally
      lDatabase.Commit;
    end;
  finally
    lQuery.Free;
    gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(lPooledDB) ;
  end;
end;

procedure TtiCGIOjectCacheAbs.Init;
var
  lFileName : string;
begin
  lFileName := tiAddTrailingSlash(CacheDirectory) + cCacheIndexFileName;
  FCacheIndex := TtiINIFile.Create(lFileName);
end;

{ EtiCGIException }

constructor EtiCGIException.Create(pExitCode: Integer; const pMessage: string);
var
  lMessage: string;
begin
  if pMessage = '' then
    lMessage := 'CGI Exception ' + IntToStr(pExitCode)
  else
    lMessage := 'CGI Exception ' + IntToStr(pExitCode) + ' "' + lMessage + '"';
  inherited Create(lMessage);
  FExitCode := pExitCode ;
end;

end.

