unit tiCGIObjectCacheClient;

interface
uses
  // tiOPF
   tiObject
  ,tiObjectCacheAbs
  ,tiCGIParams
  ,tiWebServerClientConnectionDetails
  // Delphi
  ,SyncObjs
  ;

type

  TtiCGIObjectCacheClient = class( TtiOjectCacheAbs )
  private
    FParams: TtiCGIParams;
    FConnectionDetails: TtiWebServerClientConnectionDetails;
  protected
    property    Params: TtiCGIParams read FParams;
    property    ConnectionDetails: TtiWebServerClientConnectionDetails read FConnectionDetails;

    procedure   RefreshCacheFromDB; override ;
    procedure   Init; override ;

    function    CGIEXEName: string; virtual ; abstract ;
    procedure   CacheToBOM(const pData: TtiObject);virtual; abstract;

    procedure   ResponseToFile(const pResponse: string); virtual;

  public
    constructor Create(const ACacheDirectoryRoot: string;
                       const AConnectionDetails: TtiWebServerClientConnectionDetails); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   Execute(const pData: TtiObject); virtual ;
  end ;

  TtiCGIObjectCacheClientVisitor = class( TtiCGIObjectCacheClient )
  protected
    procedure   CacheToBOM(const pData: TtiObject); override;
    function    VisitorGroupName: string; virtual ; abstract ;
    procedure   RegisterVisitors; virtual;
  public
    procedure   Execute(const pData: TtiObject); override;
//    {:For unit testing}
//    class procedure   StringToBOM(const AStr: string; const AData: TtiObject);
  end ;

implementation
uses
  // tiOPF
   tiCGIExtensionRequest
  ,tiUtils
  ,tiStreams
  ,tiXML
  ,tiLog
  ,tiConstants
  ,tiQueryXMLLight
  ,tiXMLToTIDataSet
  ,tiOPFManager
  ,tiCGIExcept
  ,tiWebServerConstants
  // Delphi
  ,SysUtils
  ,Classes
  ,Windows
  ,tiSyncObjs
  ;

{ TtiCGIObjectCacheClientVisitor }

procedure TtiCGIObjectCacheClient.RefreshCacheFromDB;
var
  lCGIRequest: TtiCGIExtensionRequest;
  lResponse:   string ;
begin
  lCGIRequest:= TtiCGIExtensionRequest.CreateInstance;
  try
    lResponse := lCGIRequest.Execute( CGIEXEName,
                                      Params.AsCompressedEncodedString,
                                      ConnectionDetails) ;
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

constructor TtiCGIObjectCacheClient.Create(const ACacheDirectoryRoot: string;
  const AConnectionDetails: TtiWebServerClientConnectionDetails);
begin
  Assert(AConnectionDetails.TestValid, CTIErrorInvalidObject);
  inherited Create(ACacheDirectoryRoot);
  FParams:= TtiCGIParams.Create;
  FConnectionDetails:= TtiWebServerClientConnectionDetails.Create;
  FConnectionDetails.Assign(AConnectionDetails);
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

procedure TtiCGIObjectCacheClientVisitor.Execute(const pData: TtiObject);
begin
  RegisterVisitors;
  inherited;
end;

procedure TtiCGIObjectCacheClientVisitor.RegisterVisitors;
begin
  // Do nothing
end;

//class procedure TtiCGIObjectCacheClientVisitor.StringToBOM(const AStr: string; const AData: TtiObject);
//var
//  LO: TtiCGIObjectCacheClientVisitor;
//  lParams: string ;
//  LFileName: string;
//begin
//  LO:= Create;
//  try
//    LO.RegisterVisitors;
//    lParams := tiMakeXMLLightParams( True, cgsCompressNone, optDBSizeOn, xfnsInteger );
//    LFileName := tiGetTempFile('xml');
//    tiStringToFile(AStr, LFileName);
//    try
//      gTIOPFManager.ConnectDatabase( LFileName, 'null', 'null', lParams, cTIPersistXMLLight);
//      try
//        gTIOPFManager.VisitorManager.Execute(LO.VisitorGroupName, AData, LFileName, cTIPersistXMLLight ) ;
//      finally
//        gTIOPFManager.DisconnectDatabase(LFileName, cTIPersistXMLLight);
//      end;
//    finally
//      SysUtils.DeleteFile(LFileName);
//    end;
//  finally
//    LO.Free;
//  end;
//end;

procedure TtiCGIObjectCacheClientVisitor.CacheToBOM(const pData: TtiObject);
var
  lStart: DWord ;
  lParams: string ;
  lFileName: string;
begin
  Assert( pData.TestValid(TtiObject), CTIErrorInvalidObject );
  lStart := GetTickCount;
  lParams := tiMakeXMLLightParams( True, cgsCompressZLib, optDBSizeOn, xfnsInteger );
  lFileName := GetCachedFileDirAndName;
  gTIOPFManager.ConnectDatabase( lFileName, 'null', 'null', lParams, cTIPersistXMLLight);
  try
    gTIOPFManager.VisitorManager.Execute( VisitorGroupName, pData, lFileName, cTIPersistXMLLight ) ;
  finally
    gTIOPFManager.DisconnectDatabase(lFileName, cTIPersistXMLLight);
  end;
  Log('  :) Finished loading ' + pData.ClassName +
      '. (' + IntToStr(GetTickCount - lStart) + 'ms)', lsQueryTiming);

end;

destructor TtiCGIObjectCacheClient.Destroy;
begin
  FParams.Free;
  FConnectionDetails.Free;
  inherited;
end;

procedure TtiCGIObjectCacheClient.Execute(const pData: TtiObject);
var
  lCachedFileDate : TDateTime ;
  lDatabaseFileDate : TDateTime ;
begin
  if tiWaitForMutex(CachedFileName, word(INFINITE)) then
  try
    Init;
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
    end;
    CacheToBOM(pData);
  finally
    tiReleaseMutex(CachedFileName);
  end;
end;

end.
