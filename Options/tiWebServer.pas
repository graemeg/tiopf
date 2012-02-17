unit tiWebServer;

{$I tiDefines.inc}

interface
uses
  tiBaseObject,
  tiStreams,
  tiThread,
  SysUtils,
  SyncObjs,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdContext,
  Contnrs,
  Classes;

const
  cErrorInvalidCachedBlockStreamTransID = 'Invalid cached block TransID "%d"';
  cDefaultBlockStreamCacheTimeout= 120;
  cDefaultBlockStreamCacheSweepEvery= 120;
  cDefaultSleepSec= 10;

type

  TtiWebServerExceptionEvent = function(const E: Exception): String of object;

  TtiWebServer = class;

  TtiWebServerAction = class(TtiBaseObject)
  private
    FOwner: TtiWebServer;
    FSortOrder: Byte;
  protected
    procedure GetReturnPage(const ADocument: string; AResponse: TStream; var AContentType: string);
    function  StaticPageLocation : string;
    function  CGIBinLocation : string;
    function  PassThroughLocation: string;
  public
    constructor Create(const AOwner : TtiWebServer; ASortOrder: Byte);
    function    CanExecute(const ADocument: string): boolean; virtual; abstract;
    procedure   Execute(const ADocument: string;
                        const ARequestInfo: TIdHTTPRequestInfo;
                        const ARequestParams: string;
                        const AResponse: TStream; var AContentType: string;
                        var   AResponseCode: Integer;
                        const AResponseInfo: TIdHTTPResponseInfo); virtual; abstract;

    property    Owner : TtiWebServer read FOwner write FOwner;
    property    SortOrder: Byte Read FSortOrder;

  end;

  TtiWebServerAction_Default = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  // A hack to ignore a single request currently being used by a web switch
  // to determine which of several servers to direct requests to
  TtiWebServerAction_Ignore = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_CanNotFindPage = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_GetLogFile = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_CanFindPage = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_ForceException = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_ForceExceptionThread = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  // ToDo: Not a true CGI interface, but a hack. Change this to support true CGI
  TtiWebServerAction_RunCGIExtension = class(TtiWebServerAction)
  private
    function ExecuteCGIApp(const ACGIApp, ARequestParams: string;
      out AResponse: string): Cardinal;
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_PassThrough = class(TtiWebServerAction)
  private
    function ExecutePassThrough(const APassThroughApp, ARequestParams: string;
      out AResponse: string): Cardinal;
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiCachedBlockStream = class(TtiBlockStream)
  private
    FTransID: Longword;
    FLastAccessed: TDateTime;
    function GetSecInUse: Word;
  public
    property TransID: Longword Read FTransID Write FTransID;
    property LastAccessed: TDateTime Read FLastAccessed Write FLastAccessed;
    property SecInUse: Word Read GetSecInUse;
  end;

  TtiBlockStreamCache = class;

  TtiThreadBlockStreamCacheSweepForTimeouts = class(TtiThread)
  private
    FBlockStreamCache: TtiBlockStreamCache;
  public
    constructor Create(ABlockStreamCache: TtiBlockStreamCache); reintroduce;
    procedure   Execute; override;
  end;

  // ToDo: We depend on SweepForTimeouts to clean up items that have been added
  //       to the cache, used and no longer required. We could add a command
  //       that's sent from the client to delete an entry from the cache, based
  //       on TransID, but this will require another network round trip. Have
  //       decided (for the time being) to depend on SweepForTimeout to clean
  //       up the cahce. We may need to review this decision if memory use on
  //       the server becomes an issue.
  TtiBlockStreamCache = class(TtiBaseObject)
  private
    FList: TObjectList;
    FCritSect: TCriticalSection;
    FLastTransID: Longword;
    FTimeOutSec: Longword;
    FSweeper: TtiThreadBlockStreamCacheSweepForTimeouts;
    FSweepEverySec: Longword;
    FSleepSec: Longword;
    FStarted: Boolean;
    function    FindByTransID(ATransID: Longword): TtiCachedBlockStream;
    function    GetCount: Longword;
  protected
    procedure   SweepForTimeOuts;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddBlockStream(const AData: string; const ABlockSize: Longword;
                               var   AFirstBlockAsString: string; var ABlockCount, ATransID: Longword);
    procedure   ReadBlock(ATransID: Longword; ABlockIndex: Longword; var ABlockAsString: string);
    property    TimeOutSec: Longword Read FTimeOutSec Write FTimeOutSec;
    property    SweepEverySec: Longword Read FSweepEverySec Write FSweepEverySec;
    property    SleepSec: Longword Read FSleepSec Write FSleepSec;
    property    Count: Longword Read GetCount;
    procedure   Start;
  end;

  TtiWebServer = class(TtiBaseObject)
  private
    FIdHTTPServer: TIdHTTPServer;
    FServerActions: TObjectList;
    FStaticPageLocation: string;
    FCGIBinLocation: string;
    FPassThroughLocation: string;
    FBlockStreamCache: TtiBlockStreamCache;
    FOnServerException: TtiWebServerExceptionEvent;
    FReadPageLocationAtStartup: Boolean;
    FLogFullHTTPRequest: boolean;
    function GetActive: Boolean;
    function  GetPort: Integer;
    procedure SetPort(const AValue: Integer);
  protected
    procedure SetStaticPageLocation(const AValue: string); virtual;
    procedure SetCGIBinLocation(const AValue: string); virtual;
    procedure SetLogFullHTTPRequest(const AValue: boolean);

    property  BlockStreamCache: TtiBlockStreamCache read FBlockStreamCache;
    property  ServerActions: TObjectList Read FServerActions;

    procedure DoIDHTTPServerCommandGet(AContext:TIdContext;
                                       ARequestInfo: TIdHTTPRequestInfo;
                                       AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure ProcessHTTPGet(const ADocument: string;
                             const ARequestInfo: TIdHTTPRequestInfo;
                             const AParams: string;
                             const AResponse: TStream; var AContentType: string;
                             var   AResponseCode: Integer;
                             const AResponseInfo: TIdHTTPResponseInfo);
    procedure ApplyResponseStreamToHTTPResponse(
                             AResponseInfo: TIdHTTPResponseInfo; AResponse: TStream;
                             const AResponseType: string; AResponseCode: Integer;
                             const AContentEncoding: string);
    function  DefaultFileName(const ADir: string; out AFilePathAndName: string): boolean;
    function  DefaultFileNameExists(const ADir: string): boolean;
    procedure CreateDefaultPage;
    procedure Sort;
    procedure ReadPageLocation; virtual;

  public
    constructor Create(APort: integer); virtual;
    destructor  Destroy; override;

    property    Port: Integer read GetPort write SetPort;
    property    StaticPageLocation : string read FStaticPageLocation;
    property    ReadPageLocationAtStartUp: Boolean read FReadPageLocationAtStartup write FReadPageLocationAtStartup;
    property    CGIBinLocation    : string read FCGIBinLocation;
    property    PassThroughLocation : string read FPassThroughLocation;
    property    OnServerException: TtiWebServerExceptionEvent read FOnServerException write FOnServerException;


    procedure   Start;
    procedure   Stop;
    property    Active: Boolean read GetActive;

  end;

function tiHTTPRequestInfoToParams(const ARequestInfo: TidHTTPRequestInfo): string;

implementation
uses
  tiConstants,
  tiExcept,
  tiWebServerConfig,
  tiWebServerUtils,
  tiWebServerConstants,
  tiUtils,
  tiLog,
  tiConsoleApp,
  tiHTTP,
  tiCRC32,
  Math;

function tiHTTPRequestInfoToParams(const ARequestInfo: TidHTTPRequestInfo): string;
begin
  // ToDo: May want to work towards a case in the future:
  //   case ARequestInfo.CommandType of ...
  //   (hcUnknown, hcHEAD, hcGET, hcPOST, hcDELETE, hcPUT, hcTRACE, hcOPTION);
  if Assigned(ARequestInfo.PostStream) then
    Result:= tiStreamToString(ARequestInfo.PostStream)
  else
    Result:= ARequestInfo.UnparsedParams;
end;

{ TtiWebServer }

constructor TtiWebServer.Create(APort: Integer);
begin
  inherited Create;
  FReadPageLocationAtStartup:= True;

  FServerActions:= TObjectList.Create(true);
  FServerActions.Add(TtiWebServerAction_Ignore.Create(       Self,  1));
  FServerActions.Add(TtiWebServerAction_Default.Create(      Self,  2));
  FServerActions.Add(TtiWebServerAction_CanFindPage.Create(  Self,  3));
  FServerActions.Add(TtiWebServerAction_GetLogFile.Create(   Self,  4));
  FServerActions.Add(TtiWebServerAction_RunCGIExtension.Create(Self, 5));
  FServerActions.Add(TtiWebServerAction_ForceException.Create(Self, 6));
  FServerActions.Add(TtiWebServerAction_ForceExceptionThread.Create(Self, 7));
  FServerActions.Add(TtiWebServerAction_PassThrough.Create(Self, 8));
  FServerActions.Add(TtiWebServerAction_CanNotFindPage.Create(Self, High(Byte)));

  FIdHTTPServer := TIdHTTPServer.Create(Nil);
  FIdHTTPServer.OnCommandGet := DoIDHTTPServerCommandGet;
  FIdHTTPServer.KeepAlive := False;
  Port:= APort;

  FBlockStreamCache:= TtiBlockStreamCache.Create;
end;

function TtiWebServer.DefaultFileName(const ADir: string;
  out AFilePathAndName: string): boolean;
var
  LDir: string;
begin
  LDir:= tiStrTran(ADir, '/', '\');
  LDir:= tiAddTrailingSlash(StaticPageLocation + LDir);
  if FileExists(LDir + 'default.htm') then
    AFilePathAndName:= LDir + 'default.htm'
  else if FileExists(LDir + 'default.html') then
    AFilePathAndName:= LDir + 'default.html'
  else if FileExists(LDir + 'index.htm') then
    AFilePathAndName:= LDir + 'index.htm'
  else if FileExists(LDir + 'index.html') then
    AFilePathAndName:= LDir + 'index.html'
  else
    AFilePathAndName:= '';
  result:= AFilePathAndName <> '';
end;

function TtiWebServer.DefaultFileNameExists(const ADir: string): boolean;
var
  LDummy: string;
begin
  result:= DefaultFileName(ADir, LDummy);
end;

destructor TtiWebServer.Destroy;
begin
  FIdHTTPServer.Free;
  FServerActions.Free;
  FBlockStreamCache.Free;
  inherited;
end;

procedure TtiWebServer.DoIDHTTPServerCommandGet(
  AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  LDocument: string;
  LParams: string;
  LResponse: TMemoryStream;
  LContentType: string;
  LResponseCode: Integer;
  LPassThroughValue: string;

  LRequestTIOPFBlockHeader: string;
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  LongWord;
  LTransID:    Longword;
  LBlockCRC:   Longword;
  LResponseTIOPFBlockHeader: string;
  LTemp: string; // Change to a stream - might be a little faster

begin
  if FLogFullHTTPRequest then
  begin
    Log('* * * * * * * * * *');
    Log('--- RawHTTPCommand');
    Log(ARequestInfo.RawHTTPCommand);
    Log('--- FormParams');
    Log(ARequestInfo.FormParams);
    Log('--- Params.Text');
    Log(ARequestInfo.Params.Text);
//    if Assigned(ARequestInfo.PostStream) then
//    begin
//      Log('--- PostStream');
//      Log(tiStreamToString(ARequestInfo.PostStream));
//    end;
  end;

  LDocument := ARequestInfo.Document;
  if LDocument[1] = '/' then
    LDocument := Copy(LDocument, 2, Length(LDocument) - 1);

  LParams:= tiHTTPRequestInfoToParams(ARequestInfo);

  LRequestTIOPFBlockHeader:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LRequestTIOPFBlockHeader, LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);

  LResponseCode:= cHTTPResponseCodeOK;
  LContentType:= cHTTPContentTypeTextHTML;
  LResponse:= TMemoryStream.Create;
  try

    // BlockSize = 0, so don't block result
    if (LBlockSize = 0) then
    begin
      ProcessHTTPGet(LDocument, ARequestInfo, LParams, LResponse, LContentType, LResponseCode, AResponseInfo);
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(0, 0, 0, 0, 0);
    end
    // BlockSize <> 0 and TransID = 0, a new blocked request
    else if (LBlockSize <> 0) and (LTransID = 0) then
    begin
      ProcessHTTPGet(LDocument, ARequestInfo, LParams, LResponse, LContentType, LResponseCode, AResponseInfo);
      // To fix http://meldeopd1:8080/browse/BR-242:
      //   "Error reading response from remote server: Transaction ID#39096 timed out.",
      // we need to reset the corresponding transaction in the StatelessDBConnection's
      // LastUsed value to Now. This will involve passing the transaction ID back
      // and hitting the StatelessDBConnectionPool from  here. Hard to do.
      FBlockStreamCache.AddBlockStream(tiStreamToString(LResponse), LBlockSize, LTemp, LBlockCount, LTransID);
      tiStringToStream(LTemp, LResponse);
      LBlockCRC:= tiCRC32FromStream(LResponse);
      if LBlockCount > 1 then
        Log('Returning block 0 of ' + IntToStr(LBlockCount) + ' in TransID ' + IntToStr(LTransID));
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(0, LBlockCount, LBlockSize, LTransID, LBlockCRC);
    end
    // BlockSize <> 0 and TransID <> 0, Retrun and existing block from the cache
    else if (LBlockSize <> 0) and (LTransID <> 0) then
    begin
      FBlockStreamCache.ReadBlock(LTransID, LBlockIndex, LTemp);
      tiStringToStream(LTemp, LResponse);
      LBlockCRC:= tiCRC32FromStream(LResponse);
      Log('Returning block ' + IntToStr(LBlockIndex) + ' of ' + IntToStr(LBlockCount) + ' in TransID ' + IntToStr(LTransID));
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
    end;

    LPassThroughValue := AResponseInfo.CustomHeaders.Values[ctiOPFHTTPPassThroughHeader];
    if SameText(LPassThroughValue, ctiOPFHTTPIsPassThroughContent) then
    begin
      // _Entire_ response is PassThrough response content
      AResponseInfo.HeaderHasBeenWritten := true; // suppress HTTP headers from response
      ApplyResponseStreamToHTTPResponse(AResponseInfo, LResponse,
          '' {AContentType}, LResponseCode, '' {AContentEncoding});
    end
    else
    begin
      ApplyResponseStreamToHTTPResponse(AResponseInfo, LResponse,
          LContentType, LResponseCode, 'MIME');
      AResponseInfo.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= LResponseTIOPFBlockHeader;
    end;

  finally
    LResponse.Free;
  end;

end;

function TtiWebServer.GetPort: Integer;
begin
  result:= FidHTTPServer.DefaultPort;
end;

procedure TtiWebServer.ApplyResponseStreamToHTTPResponse(
  AResponseInfo: TIdHTTPResponseInfo;
  AResponse: TStream;
  const AResponseType: string;
  AResponseCode: Integer;
  const AContentEncoding: string);
var
  LTempResponse: TMemoryStream;
begin
  if AResponseType = cHTTPContentTypeTextHTML then
  begin
    AResponseInfo.ContentText:= tiStreamToString(AResponse);
    if AResponseCode <> cHTTPResponseCodeOK then
      AResponseInfo.ResponseNo:= AResponseCode;
  end
  else begin
    LTempResponse:= TMemoryStream.Create;
    tiCopyStream(AResponse, LTempResponse);
    AResponseInfo.ContentStream:= LTempResponse;
    AResponseInfo.FreeContentStream:= True;
    AResponseInfo.ContentEncoding:= AContentEncoding;
  end;
  AResponseInfo.ContentType:= AResponseType;
end;

procedure TtiWebServer.ProcessHTTPGet(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const AParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  i : integer;
  LServerAction : TtiWebServerAction;
  LErrorMessage: string;
begin
  Assert(AResponse<>nil, 'AResponse not assigned');
  try
    for i := 0 to FServerActions.Count - 1 do
      if (FServerActions.Items[i] as TtiWebServerAction).CanExecute(ADocument) then
      begin
        LServerAction := (FServerActions.Items[i] as TtiWebServerAction);
        LServerAction.Execute(ADocument, ARequestInfo, AParams, AResponse,
                              AContentType, AResponseCode, AResponseInfo);
        Exit; //==>
      end;
    // Should not get here - unless can not find page.
    tiStringToStream(Format(cErrorCanNotFindPage, [ADocument]), AResponse);
  except
    on e:exception do
    begin
      if Assigned(FOnServerException) then
        LErrorMessage:= FOnServerException(E)
      else begin
        LErrorMessage:= e.message;
        LogError(LErrorMessage);
      end;
      // ToDo: Must inject error info into the response here, that can be
      //       decoded by the app server and returned as something other than a
      //       StreamDecompress error
      tiStringToStream(Format(cErrorOnServer,[LErrorMessage]), AResponse);
    end;
  end;
end;

procedure TtiWebServer.ReadPageLocation;
var
  LConfig: TtiWebServerConfig;
begin
  if ReadPageLocationAtStartup then
  begin
    LConfig:= TtiWebServerConfig.Create;
    try
      FStaticPageLocation:= tiAddTrailingSlash(LConfig.PathToStaticPages);
      FCGIBinLocation:= tiAddTrailingSlash(LConfig.PathToCGIBin);
      FPassThroughLocation:= tiAddTrailingSlash(LConfig.PathToPassThrough);
      FLogFullHTTPRequest:= LConfig.LogFullHTTPRequest;
    finally
      LConfig.Free;
    end;
  end;
end;

procedure TtiWebServer.Start;
var
  LDefaultFileName: string;
begin
  ReadPageLocation;

  if not DirectoryExists(StaticPageLocation) and (StaticPageLocation <> '') then
    ForceDirectories(StaticPageLocation);
  if not DirectoryExists(StaticPageLocation) and (StaticPageLocation <>'')then
    raise exception.create('Unable to locate or create directory for static pages <' + StaticPageLocation + '>');

  if not DefaultFileNameExists('') then
  begin
    Log('No default file found in ' + StaticPageLocation);
    Log('Default page being created...');
    CreateDefaultPage;
  end;

  DefaultFileName('', LDefaultFileName);
  Log('Attempting to start HTTP server on port ' + IntToStr(FidHTTPServer.DefaultPort));
  FIdHTTPServer.Active := true;
  Log('HTTP server started');
  Log('Static web pages location "' + StaticPageLocation + '"');
  Log('Default page location "' + LDefaultFileName + '"');
  Log('CGI-Bin location "' + FCGIBinLocation + '"');
  Log('Pass through location "' + FPassThroughLocation + '"');
  Log('Log full HTTP request "' + tiBoolToStr(FLogFullHTTPRequest) + '"');

  BlockStreamCache.Start;

end;

procedure TtiWebServer.Stop;
begin
  FIdHTTPServer.Active := False;
  //ToDo: Implement BlockStreamCache.Stop;
end;

function TtiWebServer.GetActive: Boolean;
begin
  result:= FIdHTTPServer.Active;
end;

function _CompareWebServerActions(AItem1, AItem2: Pointer): Integer;
var
  LItem1: TtiWebServerAction;
  LItem2: TtiWebServerAction;
begin
  Assert(TtiBaseObject(AItem1).TestValid(TtiWebServerAction), CTIErrorInvalidObject);
  Assert(TtiBaseObject(AItem2).TestValid(TtiWebServerAction), CTIErrorInvalidObject);
  LItem1:= TtiWebServerAction(AItem1);
  LItem2:= TtiWebServerAction(AItem2);
  Result:= CompareValue(LItem1.SortOrder, LItem2.SortOrder);
end;

procedure TtiWebServer.SetCGIBinLocation(const AValue: string);
begin
  if AValue <> '' then
    FCGIBinLocation:= tiAddtrailingSlash(AValue)
  else
    FCGIBinLocation:= AValue;
end;

procedure TtiWebServer.SetLogFullHTTPRequest(const AValue: boolean);
begin
  FLogFullHTTPRequest:= AValue;
end;

procedure TtiWebServer.SetPort(const AValue: Integer);
begin
  FidHTTPServer.DefaultPort:= AValue;
end;

procedure TtiWebServer.SetStaticPageLocation(const AValue: string);
begin
  if AValue <> '' then
    FStaticPageLocation:= tiAddtrailingSlash(AValue)
  else
    FStaticPageLocation:= AValue;
end;

procedure TtiWebServer.Sort;
begin
  FServerActions.Sort(_CompareWebServerActions);
end;

{ TtiWebServerAction_Default }

function TtiWebServerAction_Default.CanExecute(const ADocument: string): boolean;
begin
  result := Owner.DefaultFileNameExists(ADocument);
end;

procedure TtiWebServerAction_Default.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream; var
        AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  LFileName: string;
begin
  Owner.DefaultFileName(ADocument, LFileName);
  Log('Processing document <' + LFileName + '> in <' + ClassName + '>');
  GetReturnPage(LFileName, AResponse, AContentType);
end;

{ TtiWebServerAction }

function TtiWebServerAction.CGIBinLocation: string;
begin
  Assert(Owner <> nil, 'Owner not assigned');
  result := Owner.CGIBinLocation;
end;

constructor TtiWebServerAction.Create(const AOwner: TtiWebServer; ASortOrder: Byte);
begin
  inherited Create;
  FOwner := AOwner;
  FSortOrder:= ASortOrder;
end;

procedure TtiWebServerAction.GetReturnPage(const ADocument: string; AResponse: TStream; var AContentType: string);
  function _ExtToMIMEContentType(const pExt: string): string;
  begin
    // MIME types found at: http://www.utoronto.ca/webdocs/HTMLdocs/Book/Book-3ed/appb/mimetype.html
    if      pExt = 'gif' then
      Result := 'image/gif'
    else if pExt = 'exe' then
      Result := 'application/octet-stream'
    else if pExt = 'zip' then
      Result := 'application/zip'
    else if pExt = 'pdf' then
      Result := 'application/pdf'
    else if pExt = 'jpg' then
      Result := '	image/jpeg'
    else if pExt = 'ico' then
      Result := '	image/ico'
    else
      raise ETIOPFDataException('Invalid MIME type ' + pExt);
  end;

var
  lExt : string;
begin
  lExt := LowerCase(tiExtractExtension(ADocument));
  // ToDo: TtiWebServer.GetReturnPage will only return text, gif,
  //       jpg, exe or zip pages. Extend this functionality
  if (lExt = 'gif') or
     (lExt = 'jpg') or
     (lExt = 'ico') or
     (lExt = 'exe') or
     (lExt = 'pdf') or
     (lExt = 'zip') then
  begin
    tiFileToStream(ADocument, AResponse);
    AContentType:= _ExtToMIMEContentType(lExt)
  end else if LExt = 'css' then
  begin
    tiFileToStream(ADocument, AResponse);
    AContentType := 'text/css';
  end else if LExt = 'js' then
  begin
    tiFileToStream(ADocument, AResponse);
    AContentType := 'text/javascipt';
  end else if LExt= '' then
  begin
    if FileExists(tiSwapExt(ADocument, 'htm')) then
      tiFileToStream(tiSwapExt(ADocument, 'htm'), AResponse)
    else if FileExists(tiSwapExt(ADocument, 'html')) then
      tiFileToStream(tiSwapExt(ADocument, 'html'), AResponse)
    else
      raise EtiOPFProgrammerException.CreateFmt(cErrorCanNotFindPage, [ADocument]);
    AContentType:= cHTTPContentTypeTextHTML;
  end else
  begin
    tiFileToStream(ADocument, AResponse);
    AContentType:= cHTTPContentTypeTextHTML;
  end;
end;

function TtiWebServerAction.PassThroughLocation: string;
begin
  Assert(Owner <> nil, 'Owner not assigned');
  result := Owner.PassThroughLocation;
end;

function TtiWebServerAction.StaticPageLocation: string;
begin
  Assert(Owner <> nil, 'Owner not assigned');
  result := Owner.StaticPageLocation;
end;

{ TtiWebServerAction_CanNotFindPage }

function TtiWebServerAction_CanNotFindPage.CanExecute(const ADocument: string): boolean;
var
  lFileNameStatic : string;
  lFileNameCGI : string;
begin

  if (ADocument = '') and (not FileExists(StaticPageLocation + cDefaultPageName)) then
  begin
    result := true;
    Exit; //==>
  end;

  if (ADocument = '') and (FileExists(StaticPageLocation + cDefaultPageName)) then
  begin
    result := false;
    Exit; //==>
  end;

  lFileNameStatic := StaticPageLocation + ADocument;
  lFileNameCGI := CGIBinLocation + ADocument;

  if (ADocument <> '') and
     (((not FileExists(lFileNameStatic))) and
      ((not FileExists(lFileNameCGI)))) then
  begin
    result := true;
    Exit; //==>
  end;

  result := false;

end;

procedure TtiWebServerAction_CanNotFindPage.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  tiStringToStream(Format(cErrorCanNotFindPage, [ADocument]), AResponse);
  AResponseCode := cHTTPResponseCodePageNotFound;
end;

{ TtiWebServerAction_CanFindPage }

function TtiWebServerAction_CanFindPage.CanExecute(const ADocument: string): boolean;
var
  LFileName: string;
begin
  if ADocument = '' then
    Result := False
  else begin
    LFileName:= StaticPageLocation + ADocument;
    Result := FileExists(LFileName);
    if (not Result) and (tiExtractExtension(LFileName)='') then
      Result:= FileExists(tiSwapExt(LFileName, 'htm')) or
               FileExists(tiSwapExt(LFileName, 'html'));
  end;
end;

procedure TtiWebServerAction_CanFindPage.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  GetReturnPage(StaticPageLocation + ADocument, AResponse, AContentType);
end;

{ TtiWebServerAction_GetLogFile }

function TtiWebServerAction_GetLogFile.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyGetLog);
end;

procedure TtiWebServerAction_GetLogFile.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  lFileName: string;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  lFileName := gLog.LogToFileName;
  if FileExists(lFileName) then
    tiStringToStream(
      '<HTML><PRE>' + tiFileToString(lFileName) + '</PRE></HTML>',
      AResponse)
  else
    tiStringToStream(
      Format(cErrorCanNotFindLogFile, [lFileName]),
      AResponse);
  AContentType:= cHTTPContentTypeTextHTML;
end;

{ TtiWebServerAction_RunCGIExtension }

function TtiWebServerAction_RunCGIExtension.CanExecute(const ADocument: string): boolean;
var
  LCGI: string;
begin
  LCGI:= CGIBinLocation + ADocument;
  // ToDo: RunCGIExtension to execute application types other than exe
  result := (ADocument <> '') and
            (FileExists(LCGI) and
            (SameText(ExtractFileExt(ADocument), '.exe')));
end;

procedure TtiWebServerAction_RunCGIExtension.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  LCGIApp : string;
  LResponse : string;
  LExitCode : Integer;
  LConfig: TtiWebServerConfig;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  try
    LCGIApp := CGIBinLocation + ADocument;
    LResponse := '';
    Log('About to call ' + LCGIApp + ' ' + ARequestParams);

    LExitCode := ExecuteCGIApp(LCGIApp, ARequestParams, LResponse);
    if LExitCode = 0 then
      tiStringToStream(LResponse, AResponse)
    else
    begin
      LResponse := Format(cErrorHTTPCGIExtension, [ADocument, LExitCode, LResponse]);
      LogError(LResponse, false);
      LConfig:= TtiWebServerConfig.Create;
      try
        if LConfig.SendBugReportEmailOnCGIFailure then
          tiMailBugReport(LResponse);
      finally
        LConfig.Free;
      end;
      // This will cause a browser to fail which is a problem with OPDMSMonitor.exe
      // Req an exit code in the returned XML
      //pResponseInfo.ResponseNo := cHTTPResponseCodeInternalError;
      tiStringToStream(LResponse, AResponse);
      AResponseInfo.CustomHeaders.Values[ctiOPFHTTPErrorCode]:= IntToStr(LExitCode);
    end;
  except
    on e:exception do
    begin
      LogError(e.message, false);
      AResponseCode := cHTTPResponseCodeInternalError;
      tiStringToStream(Format(cErrorInServerExtension, [ADocument, e.message]), AResponse);
    end;
  end;
end;

function TtiWebServerAction_RunCGIExtension.ExecuteCGIApp(const ACGIApp,
  ARequestParams: string; out AResponse: string): Cardinal;
begin
  tiWebServerExecuteCGIApp(ARequestParams, AResponse, ACGIApp, Result);
end;

procedure TtiWebServer.CreateDefaultPage;
begin
  Log('Createing default page: ' + tiAddTrailingSlash(StaticPageLocation) + cDefaultPageName);
  tiStringToFile(cDefaultPageText,
                 tiAddTrailingSlash(StaticPageLocation) + cDefaultPageName);
end;

{ TtiWebServerAction_TestAlive }

{ TtiWebServerAction_Ignore }

function TtiWebServerAction_Ignore.CanExecute(const ADocument: string): boolean;
begin
  result := (ADocument = cDocumentToIgnore);
end;

procedure TtiWebServerAction_Ignore.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponse.Size:= 0;
  AContentType:= cHTTPContentTypeTextHTML;
end;

{ TtiBlockStreamCache }

procedure TtiBlockStreamCache.AddBlockStream(
  const AData: string; const ABlockSize: Longword;
  var   AFirstBlockAsString: string; var ABlockCount, ATransID: Longword);
var
  L: TtiCachedBlockStream;
begin
  FCritSect.Enter;
  try
    Inc(FLastTransID);
    L:= TtiCachedBlockStream.Create(AData, ABlockSize);
    FList.Add(L);
    L.TransID:= FLastTransID;
    L.LastAccessed:= Now;
    ATransID:= FLastTransID;
    ABlockCount:= L.BlockCount;
    if ABlockCount > 0 then
      AFirstBlockAsString:= L.BlockAsString[0];
  finally
    FCritSect.Leave;
  end;
end;

constructor TtiBlockStreamCache.Create;
begin
  inherited;
  FList:=TObjectList.Create(True);
  FCritSect:= TCriticalSection.Create;
  FTimeOutSec:= cDefaultBlockStreamCacheTimeout;
  FSweepEverySec:= cDefaultBlockStreamCacheSweepEvery;
  FSleepSec:= cDefaultSleepSec;
  FSweeper:= TtiThreadBlockStreamCacheSweepForTimeouts.Create(Self);
  FStarted:= False;
end;

destructor TtiBlockStreamCache.Destroy;
begin
  if FStarted then
  begin
    FSweeper.Terminate;
    FSweeper.WaitFor;
  end;
  FSweeper.Free;
  FList.Free;
  FCritSect.Free;
  inherited;
end;

function TtiBlockStreamCache.FindByTransID(ATransID: Longword): TtiCachedBlockStream;
var
  i: Integer;
begin
  for i:= 0 to FList.Count - 1 do
    if (FList.Items[i] as TtiCachedBlockStream).TransID = ATransID then
    begin
      Result:= (FList.Items[i] as TtiCachedBlockStream);
      Exit; //==>
    end;
  Result:= nil;
end;

function TtiBlockStreamCache.GetCount: Longword;
begin
  FCritSect.Enter;
  try
    Result:= Longword(FList.Count);
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiBlockStreamCache.ReadBlock(ATransID, ABlockIndex: Longword; var ABlockAsString: string);
var
  L: TtiCachedBlockStream;
begin
  FCritSect.Enter;
  try
    L:= FindByTransID(ATransID);
    if L = nil then
      raise EtiOPFDataException.CreateFmt(cErrorInvalidCachedBlockStreamTransID, [ATransID]);
    ABlockAsString:= L.BlockAsString[ABlockIndex];
    L.LastAccessed:= Now;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiBlockStreamCache.Start;
begin
  FStarted:= True;
  FSweeper.Start;
end;

procedure TtiBlockStreamCache.SweepForTimeOuts;
var
  i: Integer;
begin
  FCritSect.Enter;
  try
    for i:= FList.Count-1 downto 0 do
      if (FList.Items[i] as TtiCachedBlockStream).SecInUse > FTimeOutSec then
        FList.Delete(i);
  finally
    FCritSect.Leave;
  end;
end;

{ TtiCachedBlockStream }

function TtiCachedBlockStream.GetSecInUse: Word;
begin
  result := Trunc((Now - LastAccessed) * 24 * 60 * 60 );
end;

{ TtiThreadBlockStreamCacheSweepForTimeouts }

constructor TtiThreadBlockStreamCacheSweepForTimeouts.Create(ABlockStreamCache: TtiBlockStreamCache);
begin
  inherited Create(True);
  FBlockStreamCache:= ABlockStreamCache;
  FreeOnTerminate:= False;
  Priority:= tpLower;
end;

procedure TtiThreadBlockStreamCacheSweepForTimeouts.Execute;
var
  LStart: TDateTime;
begin
  while not Terminated do
  begin
    LStart:= Now;
    while (not Terminated) and
          ((Now - LStart) < (cdtOneSecond * FBlockStreamCache.SweepEverySec)) do
      Sleep(FBlockStreamCache.SleepSec*1000); // Higher value uses less resouce, but will take longer to shut down.
      FBlockStreamCache.SweepForTimeOuts;
  end;
end;

{ TtiWebServerAction_ForceException }

function TtiWebServerAction_ForceExceptionThread.CanExecute(
  const ADocument: string): boolean;
begin
  result := SameText(ADocument, CTIDBProxyForceExceptionThread);
end;

type
  ETIWebServerTestException = class(Exception)
  end;

type
  TTestExceptionThread = class(TThread)
  public
    constructor Create(ASuspended: boolean);
    procedure   Execute; override;
  end;

  constructor TTestExceptionThread.Create(ASuspended: boolean);
  begin
    inherited;
    FreeOnTerminate:= True;
  end;

  procedure TTestExceptionThread.Execute;
  begin
    Sleep(100);
    Log('About to raise a test exception');
    raise ETIWebServerTestException.Create('A test exception has been raised on the server at your request at ' + tiDateTimeToStr(now));
  end;

procedure TtiWebServerAction_ForceExceptionThread.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var AContentType: string;
  var AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  TTestExceptionThread.Create(false);
  tiStringToStream('<html>A test exception was raised on the server at your request at ' +
    tiDateTimeToStr(now) + '</html>', AResponse);
end;

{ TtiWebServerAction_ForceException }

function TtiWebServerAction_ForceException.CanExecute(
  const ADocument: string): boolean;
begin
  result := SameText(ADocument, CTIDBProxyForceException);
end;

procedure TtiWebServerAction_ForceException.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  raise ETIWebServerTestException.Create('A test exception has been raised on the server at your request at ' + tiDateTimeToStr(now));
end;

{ TtiWebServerAction_PassThrough }

function TtiWebServerAction_PassThrough.CanExecute(
  const ADocument: string): boolean;
var
  LPassThrough: string;
  LRequestArgs: TStrings;
begin
  LRequestArgs := TStringList.Create;
  try
    LRequestArgs := TStringList.Create;
    LRequestArgs.Delimiter := '/';
    LRequestArgs.StrictDelimiter := true;
    LRequestArgs.DelimitedText := ADocument;
    LPassThrough := PassThroughLocation + LRequestArgs[0];
    Log('Processing TtiWebServerAction_PassThrough.CanExecute <' + LPassThrough  + '>');
    result := (ADocument <> '') and FileExists(LPassThrough);
  finally
    LRequestArgs.Free;
  end;

end;

procedure TtiWebServerAction_PassThrough.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  LPassThroughApp : string;
  LPassThroughTail: string;
  LResponse : string;
  LExitCode : Integer;
  LConfig: TtiWebServerConfig;
  LRequestArgs: TStrings;
  LContentFileName: string;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  try
    LRequestArgs := TStringList.Create;
    try
      LRequestArgs.Delimiter := '/';
      LRequestArgs.StrictDelimiter := true;
      LRequestArgs.DelimitedText := ADocument;
      LPassThroughApp := PassThroughLocation + LRequestArgs[0];
      LPassThroughTail := ADocument;
      Delete(LPassThroughTail, 1, Length(LRequestArgs[0]));
      LResponse := '';
      Log('About to call ' + LPassThroughApp + ' ' + LPassThroughTail +  ' ' + ARequestParams);
    finally
      LRequestArgs.Free;
    end;

    LExitCode := ExecutePassThrough(LPassThroughApp, LPassThroughTail + ' ' + ARequestParams, LResponse);
    if LExitCode = 0 then
    begin
      AResponseInfo.CustomHeaders.Values[ctiOPFHTTPPassThroughHeader] :=
        ctiOPFHTTPIsPassThroughContent;
      // If returned text is the passthrough content file name then load
      // response from the file, else response is returned text
      if Copy(LResponse, 1, Length(cPassThroughContentFilePrefix)) = cPassThroughContentFilePrefix then
      begin
        LContentFileName := Trim(Copy(LResponse,
            Length(cPassThroughContentFilePrefix) + 1, Length(LResponse)));
        if (LContentFileName <> '') and FileExists(LContentFileName) then
        begin
          tiFileToStream(LContentFileName, AResponse);
          tiDeleteFile(LContentFileName);
        end
        // else empty response
      end
      else
        tiStringToStream(LResponse, AResponse);
    end
    else
    begin
      LResponse := Format(cErrorHTTPCGIExtension, [ADocument, LExitCode, LResponse]);
      LogError(LResponse, false);
      LConfig:= TtiWebServerConfig.Create;
      try
        //TODO: Add separate setting in config for PassThrough?
        if LConfig.SendBugReportEmailOnCGIFailure then
          tiMailBugReport(LResponse);
      finally
        LConfig.Free;
      end;
      tiStringToStream(LResponse, AResponse);
      AResponseInfo.CustomHeaders.Values[ctiOPFHTTPErrorCode]:= IntToStr(LExitCode);
    end;
  except
    on e:exception do
    begin
      LogError(e.message, false);
      AResponseCode := cHTTPResponseCodeInternalError;
      tiStringToStream(Format(cErrorInServerExtension, [ADocument, e.message]), AResponse);
    end;
  end;
end;


type
  ETIWebServerPassThroughException = class(Exception)
  end;

function TtiWebServerAction_PassThrough.ExecutePassThrough(const APassThroughApp,
  ARequestParams: string; out AResponse: string): Cardinal;
begin
  if Length(APassThroughApp) + Length(ARequestParams) <= CMaximumCommandLineLength then
    Result:= tiExecConsoleApp(APassThroughApp, ARequestParams, AResponse, nil, false)
  else
    raise ETIWebServerPassThroughException.CreateFmt(
      'PassThrough error: command-line too long: <%s>',
      [APassThroughApp + ARequestParams]);
end;


end.
