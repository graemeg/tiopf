unit tiCGIObjectCacheClient;

interface
uses
  // tiOPF
   tiObject
  ,tiCGIObjectCacheAbs
  // Delphi
  ,SyncObjs
  ;

type

  TtiCGIObjectCacheClient = class( TtiCGIOjectCacheAbs )
  private
    procedure   ResponseToFile(const pResponse: string);
  protected
    procedure   RefreshCacheFromDB; override ;
    procedure   Init; override ;
    function    CGIEXEName: string; virtual ; abstract ;
    procedure   CacheToBOM(const pData: TtiObject);virtual; abstract;
  public
    constructor Create; override ;
    procedure   Execute(const pData: TtiObject); virtual ;
  end ;

  TtiCGIObjectCacheClientVisitor = class( TtiCGIObjectCacheClient )
  protected
    procedure   CacheToBOM(const pData: TtiObject); override;
    function    VisitorGroupName: string; virtual ; abstract ;
  end ;

implementation
uses
  // tiOPF
  tiDBProxyServerCGIRequest
  ,tiUtils
  ,tiStreams
  ,tiXML
  ,tiLog
  ,tiConstants
  ,tiQueryXMLLight
  ,tiXMLToTIDataSet
  ,tiOPFManager
  // Delphi
  ,SysUtils
  ,Classes
  ,Windows
  // OPDMS
  ,OPDMSINI
  ;

{ TtiCGIObjectCacheClientVisitor }

procedure TtiCGIObjectCacheClient.RefreshCacheFromDB;
var
  lCGIRequest: TtiDBProxyServerCGIRequest ;
  lResponse:   string ;
begin
  lCGIRequest:= TtiDBProxyServerCGIRequest.Create ;
  try
    lResponse := lCGIRequest.Execute( gOPDMSConfig.AppServerURL,
                                      CGIEXEName,
                                      Params.AsCompressedEncodedString,
                                      gOPDMSConfig.ConnectWith,
                                      gOPDMSConfig.ProxyServerActive,
                                      gOPDMSConfig.ProxyServerName,
                                      gOPDMSConfig.ProxyServerPort ) ;
    ResponseToFile(lResponse);
  finally
    lCGIRequest.Free;
  end ;
end;

procedure TtiCGIObjectCacheClient.Init;
begin
  if not DirectoryExists(CacheDirectory) then
    ForceDirectories(CacheDirectory);
  if not DirectoryExists(CacheDirectory) then
    raise EtiCGIException.Create(cTICGIExitCodeCanNotCreateCacheDirectory);
  inherited;
end;

constructor TtiCGIObjectCacheClient.Create;
begin
  inherited;
  Init;
end;

procedure TtiCGIObjectCacheClient.ResponseToFile(const pResponse: string);
var
  lStreamFrom: TMemoryStream;
  lStreamTo: TMemoryStream;
begin
  lStreamFrom:= TMemoryStream.Create;
  try
    lStreamTo:= TMemoryStream.Create;
    try
      tiStringToStream( pResponse, lStreamFrom);
      MimeDecodeStream(lStreamFrom, lStreamTo);
      lStreamTo.SaveToFile(GetCachedFileDirAndName);
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

procedure TtiCGIObjectCacheClientVisitor.CacheToBOM(const pData: TtiObject);
var
  lStart: DWord ;
  lParams: string ;
  lFileName: string;
begin
  Assert( pData.TestValid(TtiObject), cTIInvalidObjectError );
  lStart := GetTickCount;
  lParams := tiMakeXMLLightParams( True, cgsCompressZLib, optDBSizeOn, xfnsInteger );
  lFileName := GetCachedFileDirAndName;
  gTIOPFManager.ConnectDatabase( lFileName, 'null', 'null', lParams, cTIPersistXMLLight);
  try
    gTIOPFManager.VisMgr.Execute( VisitorGroupName, pData, lFileName, cTIPersistXMLLight ) ;
  finally
    gTIOPFManager.DisconnectDatabase(lFileName, cTIPersistXMLLight);
  end;
  Log('  :) Finished loading ' + pData.ClassName +
      '. (' + IntToStr(GetTickCount - lStart) + 'ms)', lsQueryTiming);

end;

procedure TtiCGIObjectCacheClient.Execute(const pData: TtiObject);
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
begin
  lCachedFileDate := GetCachedFileDate;
  lDatabaseFileDate := GetDBFileDate ;
  Log([ClassName, 'Execute']);
  Log(['  FileName', GetCachedFileDirAndName]);
  Log(['  Database date', tiDateTimeToStr(lDatabaseFileDate)]);
  Log(['  Cache date', tiDateTimeToStr(lCachedFileDate)]);
  if ( lCachedFileDate <> lDatabaseFileDate ) or
     ( not FileExists( GetCachedFileDirAndName )) then
  begin
    Log('  File WILL be refreshed');
    RefreshCacheFromDB;
    SetCachedFileDate(lDatabaseFileDate);
  end else
    Log('  File WILL NOT be refreshed');
  CacheToBOM(pData);
end;

end.
