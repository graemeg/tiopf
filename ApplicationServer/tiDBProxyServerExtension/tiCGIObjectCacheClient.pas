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

    procedure   RefreshCacheFromDB(const ACacheFileDate: TDateTime); override ;
    procedure   Init; override ;

    function    CGIEXEName: string; virtual ; abstract ;
    procedure   CacheToBOM(const AData: TtiObject);virtual; abstract;

    procedure   ResponseToFile(const AResponse: string); virtual;
    procedure   DoExecute(const AData: TtiObject); virtual ;

  public
    constructor Create(const ACacheDirectoryRoot: string;
                       const AConnectionDetails: TtiWebServerClientConnectionDetails); reintroduce; virtual;
    destructor  Destroy; override;
    class procedure Execute(const AData: TtiObject;
      const ACacheDirectoryRoot: string;
      const AConnectionDetails: TtiWebServerClientConnectionDetails);

  end ;

  TtiCGIObjectCacheClientVisitor = class( TtiCGIObjectCacheClient )
  protected
    procedure   CacheToBOM(const AData: TtiObject); override;
    function    VisitorGroupName: string; virtual ; abstract ;
    procedure   RegisterVisitors; virtual;
    procedure   DoExecute(const AData: TtiObject); override;
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

procedure TtiCGIObjectCacheClient.RefreshCacheFromDB(const ACacheFileDate: TDateTime);
var
  LCGIRequest: TtiCGIExtensionRequest;
  LResponse:   string ;
begin
  LCGIRequest:= TtiCGIExtensionRequest.CreateInstance;
  try
    LResponse := LCGIRequest.Execute( CGIEXEName,
                                      Params.AsCompressedEncodedString,
                                      ConnectionDetails) ;
    ResponseToFile(LResponse);
    SetCachedFileDate(ACacheFileDate);
  finally
    LCGIRequest.Free;
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

procedure TtiCGIObjectCacheClient.ResponseToFile(const AResponse: string);
var
  lStreamFrom: TMemoryStream;
  lStreamTo: TMemoryStream;
begin
  lStreamFrom:= TMemoryStream.Create;
  try
    lStreamTo:= TMemoryStream.Create;
    try
      tiStringToStream( AResponse, lStreamFrom);
      MimeDecodeStream(lStreamFrom, lStreamTo);
      lStreamTo.SaveToFile(GetCachedFileDirAndName);
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

procedure TtiCGIObjectCacheClientVisitor.DoExecute(const AData: TtiObject);
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

procedure TtiCGIObjectCacheClientVisitor.CacheToBOM(const AData: TtiObject);
var
  lStart: DWord ;
  lParams: string ;
  lFileName: string;
begin
  Assert( AData.TestValid(TtiObject), CTIErrorInvalidObject );
  lStart := GetTickCount;
  lParams := tiMakeXMLLightParams( True, cgsCompressZLib, optDBSizeOn, xfnsInteger );
  lFileName := GetCachedFileDirAndName;
  gTIOPFManager.ConnectDatabase( lFileName, 'null', 'null', lParams, cTIPersistXMLLight);
  try
    gTIOPFManager.VisitorManager.Execute( VisitorGroupName, AData, lFileName, cTIPersistXMLLight ) ;
  finally
    gTIOPFManager.DisconnectDatabase(lFileName, cTIPersistXMLLight);
  end;
  Log('  :) Finished loading ' + AData.ClassName +
      '. (' + IntToStr(GetTickCount - lStart) + 'ms)', lsQueryTiming);

end;

destructor TtiCGIObjectCacheClient.Destroy;
begin
  FParams.Free;
  FConnectionDetails.Free;
  inherited;
end;

procedure TtiCGIObjectCacheClient.DoExecute(const AData: TtiObject);
var
  LCachedFileDate : TDateTime ;
  LDatabaseFileDate : TDateTime ;
begin
  if tiWaitForMutex(CachedFileName, word(INFINITE)) then
  try
    Init;
    LCachedFileDate := GetCachedFileDate;
    LDatabaseFileDate := GetDBFileDate ;
    Log([ClassName, 'Execute']);
    Log(['  FileName', GetCachedFileDirAndName]);
    Log(['  Database date', tiDateTimeToStr(LDatabaseFileDate)]);
    Log(['  Cache date', tiDateTimeToStr(LCachedFileDate)]);
    if MustUpdateCacheFile(LCachedFileDate, LDatabaseFileDate) then
    begin
      Log('  File WILL be refreshed');
      RefreshCacheFromDB(LDatabaseFileDate);
    end;
    CacheToBOM(AData);
  finally
    tiReleaseMutex(CachedFileName);
  end;
end;

class procedure TtiCGIObjectCacheClient.Execute(const AData: TtiObject;
  const ACacheDirectoryRoot: string;
  const AConnectionDetails: TtiWebServerClientConnectionDetails);
var
  L: TtiCGIObjectCacheClient;
begin
  L:= Create(ACacheDirectoryRoot, AConnectionDetails);
  try
    L.DoExecute(AData);
  finally
    L.Free;
  end;
end;

end.
