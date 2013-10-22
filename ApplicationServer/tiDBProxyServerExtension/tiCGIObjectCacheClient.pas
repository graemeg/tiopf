unit tiCGIObjectCacheClient;

{$I tiDefines.inc}

interface
uses
  // tiOPF
   tiObject
  ,tiObjectCacheAbs
  ,tiCGIParams
  ,tiStructuredCSVReader
  ,tiWebServerClientConnectionDetails
  // Delphi
  ,SyncObjs
  ;

const
  CErrorCanNotCreateCacheDataDirectory = 'Can not create cache data directory "%s"';

type

  TtiCGIObjectCacheClient = class( TtiObjectCacheAbs )
  private
    FParams: TtiCGIParams;
    FConnectionDetails: TtiWebServerClientConnectionDetails;
  protected
    property    Params: TtiCGIParams read FParams;
    property    ConnectionDetails: TtiWebServerClientConnectionDetails read FConnectionDetails;

    procedure   RefreshCacheFromDB(const ACacheFileDate: TDateTime); override ;
    procedure   Init; override ;

    function    CGIEXEName: string; virtual ; abstract ;
    procedure   CacheToBOM(const AData: TtiObject); virtual; abstract;

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

  // PH: October 2013.
  // TtiCGIObjectCacheClient1 has the code in CacheToBOM that is often cloned
  // between concreate classes abstracted up. Better to use
  // TtiCGIObjectCacheClient1 for new work, and to replace classes that use
  // TtiCGIObjectCacheClient as their parent over time.
  TtiCGIObjectCacheClient1 = class(TtiCGIObjectCacheClient)
  protected
    procedure   CacheToBOM(const AData: TtiObject); override;
    function    ParserClass: TtiStructuredCSVtoBOMFileParserClass; virtual; abstract;
  end;

  // PH: October 2013.
  // Do not use TtiCGIObjectCacheClientVisitor as the parent in new work, use
  // TtiCGIObjectCacheClient1 instead. Replace code that uses
  // TtiCGIObjectCacheClient1 over time.
  TtiCGIObjectCacheClientVisitor = class( TtiCGIObjectCacheClient )
  protected
    procedure   CacheToBOM(const AData: TtiObject); override;
    function    VisitorGroupName: string; virtual ; abstract ;
    procedure   RegisterVisitors; virtual;
    procedure   DoExecute(const AData: TtiObject); override;
  end ;

implementation
uses
   tiXML
  ,tiLog
  ,tiUtils
  ,tiStreams
  ,tiCompress
  ,tiConstants
  ,tiCGIExcept
  ,tiOPFManager
  ,tiQueryXMLLight
  ,tiXMLToTIDataSet
  ,tiWebServerConstants
  ,tiCGIExtensionRequest
  // Delphi
  ,SysUtils
  ,Classes
  ,Windows
  ,tiSyncObjs
  ;

{ TtiCGIObjectCacheClient }

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
    raise EtiCGIException.CreateFmt(CErrorCanNotCreateCacheDataDirectory, [CacheDirectory]);
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
    Log(['Reading', CachedFileName]);
    Log(['  FileName', GetCachedFileDirAndName]);
    Log(['  Database date', tiDateTimeToStr(LDatabaseFileDate)]);
    Log(['  Cache date', tiDateTimeToStr(LCachedFileDate)]);
    if MustUpdateCacheFile(LCachedFileDate, LDatabaseFileDate) then
    begin
      Log('  File WILL be refreshed');
      RefreshCacheFromDB(LDatabaseFileDate);
    end else
      Log('  File WILL NOT be refreshed');
    CacheToBOM(AData);
    Log(['Done reading', CachedFileName]);
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

{ TtiCGIObjectCacheClientVisitor }

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
//      GTIOPFManager.ConnectDatabase( LFileName, 'null', 'null', lParams, cTIPersistXMLLight);
//      try
//        GTIOPFManager.VisitorManager.Execute(LO.VisitorGroupName, AData, LFileName, cTIPersistXMLLight ) ;
//      finally
//        GTIOPFManager.DisconnectDatabase(LFileName, cTIPersistXMLLight);
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
  GTIOPFManager.ConnectDatabase( lFileName, 'null', 'null', lParams, cTIPersistXMLLight);
  try
    GTIOPFManager.VisitorManager.Execute( VisitorGroupName, AData, lFileName, cTIPersistXMLLight ) ;
  finally
    GTIOPFManager.DisconnectDatabase(lFileName, cTIPersistXMLLight);
  end;
  Log('  :) Finished loading ' + AData.ClassName +
      '. (' + IntToStr(GetTickCount - lStart) + 'ms)', lsQueryTiming);

end;

{ TtiCGIObjectCacheClient1 }

procedure TtiCGIObjectCacheClient1.CacheToBOM(const AData: TtiObject);
 var
  lFileName: string;
  LFileParser: TtiStructuredCSVtoBOMFileParser;
  LCSVStream: TMemoryStream;
begin
  Assert(AData.TestValid, CTIErrorInvalidObject );
  lFileName := GetCachedFileDirAndName;
  LFileParser := nil;
  LCSVStream := nil;
  try
    LFileParser := ParserClass.Create(AData);
    LCSVStream := TMemoryStream.Create;
    tiDecompressFileToStream(lFileName, LCSVStream);
    LFileParser.ParseStream(LCSVStream);
  finally
    LFileParser.Free;
    LCSVStream.Free;
  end;
end;

end.
