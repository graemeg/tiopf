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
  CFavIconFileName = 'favicon.ico';
  cErrorInvalidCachedBlockStreamTransID = 'Invalid cached block TransID "%s"';
  CProxyClientServersHeaderField = 'X-Forwarded-For';

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
    FTransID: string;
    FLastAccessed: TDateTime;
    function GetIdleMS: LongWord;
  public
    property TransID: string Read FTransID Write FTransID;
    property LastAccessed: TDateTime Read FLastAccessed Write FLastAccessed;
    property IdleMS: LongWord Read GetIdleMS;
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
    FEntryIdleLimitMS: Longword;
    FSweeper: TtiThreadBlockStreamCacheSweepForTimeouts;
    FSweepIntervalMS: Longword;
    FStarted: Boolean;
    FCurrentSize: LongWord;
    FMaxSize: LongWord;
    FMaxSizeTime: TDateTime;
    function    FindByTransID(ATransID: string): TtiCachedBlockStream;
    function    GetCount: Longword;
    procedure   Clear;
  protected
    procedure   SweepForTimeOuts;
  public
    constructor Create;
    destructor  Destroy; override;
    function    AddBlockStream(const ATransID: string; const AData: string; const ABlockSize: Longword;
                               out ABlockCount: LongWord): boolean;
    function    ReadBlock(const ATransID: string; const ABlockIndex: Longword;
                          out ABlockCount: LongWord; out ABlockContent: string): boolean;
    property    EntryIdleLimitMS: Longword Read FEntryIdleLimitMS Write FEntryIdleLimitMS;
    property    SweepIntervalMS: Longword Read FSweepIntervalMS Write FSweepIntervalMS;
    property    CurrentSize: LongWord read FCurrentSize;
    property    MaxSize: LongWord read FMaxSize;
    property    MaxSizeTime: TDateTime read FMaxSizeTime;
    property    Count: Longword Read GetCount;
    procedure   Start;
  end;

  TtiWebServer = class(TtiBaseObject)
  private
    FIdHTTPServer: TIdHTTPServer;
    // HTTPS/SSL Support:
    //FServerIOHandler: TIdServerIOHandlerSSLOpenSSL;
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
    function GetCacheCurrentSize: LongWord;
    function GetCacheMaxSize: LongWord;
    function GetCacheMaxSizeTime: TDateTime;
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

    property    CacheCurrentSize: LongWord read GetCacheCurrentSize;
    property    CacheMaxSize: LongWord read GetCacheMaxSize;
    property    CacheMaxSizeTime: TDateTime read GetCacheMaxSizeTime;

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
  LTransID:    string;
  LBlockCRC:   Longword;
  LResponseTIOPFBlockHeader: string;
  LBlockContent: string; // Change to a stream - might be a little faster
  LCreatedBlockStream: boolean;
  LIsTIOPFClientRequest: boolean;
  LUnCachedRequest: boolean;

begin
  Log('Request from %s:%d (proxy client and servers: %s)',
      [AContext.Binding.PeerIP, AContext.Binding.PeerPort,
       ARequestInfo.RawHeaders.Values[CProxyClientServersHeaderField]]);
  if FLogFullHTTPRequest then
  begin
    Log('* * * * * * * * * *');
    Log('--- RawHTTPCommand');
    Log(ARequestInfo.RawHTTPCommand);
    Log('--- FormParams');
    Log(ARequestInfo.FormParams);
    Log('--- Params.Text');
    Log(ARequestInfo.Params.Text);
    Log('--- Document');
    Log(ARequestInfo.Document);
    Log('--- Request Headers');
    Log(ARequestInfo.RawHeaders.Text);
  end;

  try
    LDocument := ARequestInfo.Document;

    if (Length(LDocument) > 0) and (LDocument[1] = '/') then
      LDocument := Copy(LDocument, 2, Length(LDocument) - 1);

    LParams:= tiHTTPRequestInfoToParams(ARequestInfo);

    LRequestTIOPFBlockHeader:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
    // The tiOPF custom HTTP Request header, ctiOPFHTTPBlockHeader, will be missing
    // from web browser requests, and STNetExport requests - we only block responses to
    // tiOPF client requests
    LIsTIOPFClientRequest := (Length(Trim(LRequestTIOPFBlockHeader)) <> 0);

    // If tiOPF custom HTTP Request header is missing, render all values as
    // empty, ie empty strings, or zero for numerical values
    tiHTTP.tiParseTIOPFHTTPBlockHeader(LRequestTIOPFBlockHeader, LBlockIndex,
      LBlockCount, LBlockSize, LTransID, LBlockCRC);

    LResponseCode:= cHTTPResponseCodeOK;
    LContentType:= cHTTPContentTypeTextHTML;
    LResponse:= TMemoryStream.Create;
    try

      if not FBlockStreamCache.ReadBlock(LTransID, LBlockIndex, LBlockCount,
        LBlockContent) then
      begin
        // A ReadBlock failure with (LBlockCount = 0) indicates an uncached request
        LUnCachedRequest := (LBlockCount = 0);

        if LUnCachedRequest then
        begin
          // Cache miss on LTransID, pass new request through
          ProcessHTTPGet(LDocument, ARequestInfo, LParams, LResponse, LContentType,
            LResponseCode, AResponseInfo);

          if (not LIsTIOPFClientRequest) or (LBlockSize = 0) then
          begin
            // Non-tiOPF HTTP request, or BlockSize = 0: => don't block result
            LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(
              0, 0, 0, LTransID, 0);
          end
          else
          begin
            // tiOPF HTTP request
            LCreatedBlockStream := FBlockStreamCache.AddBlockStream(LTransID,
              tiStreamToString(LResponse), LBlockSize, LBlockCount);

            if not LCreatedBlockStream then
              Log('HTTP Trans ID: ' + LTransID + '. Failed to create new BlockStream ' +
                '(BlockStream already exists or invalid new TransID)', lsError);

            if FBlockStreamCache.ReadBlock(LTransID, LBlockIndex, LBlockCount,
              LBlockContent) then
            begin
              tiStringToStream(LBlockContent, LResponse);
              LBlockCRC:= tiCRC32FromStream(LResponse);
              LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(
                LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);

              if LBlockCount > 1 then
                Log('HTTP Trans ID: ' + LTransID + '. Returning block '+
                  IntToStr(LBlockIndex) + '  of ' + IntToStr(LBlockCount), lsQueryTiming);
            end
            else
            begin
              // LBlockCount <> 0 - range error for arg LBlockIndex on new request
              Log('HTTP Trans ID (new request): ' + LTransID + '. Requested block ' +
                IntToStr(LBlockIndex) + ' of ' + IntToStr(LBlockCount), lsError);
            end;

          end;

        end
        else
        begin
          // Request is cached, but range error for arg LBlockIndex in cache read
          Log('HTTP Trans ID (cached response): ' + LTransID +
            '. Requested block '+ IntToStr(LBlockIndex) + ' of ' + IntToStr(LBlockCount),
            lsError);
        end;
      end
      else
      begin
        // Return an existing block from the cache
        tiStringToStream(LBlockContent, LResponse);
        LBlockCRC:= tiCRC32FromStream(LResponse);
        LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(
          LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
        Log('HTTP Trans ID: ' + LTransID + '. Returning block ' +
          IntToStr(LBlockIndex) + ' of ' + IntToStr(LBlockCount), lsQueryTiming);
      end;

      // The HTTP Request has been processed or discarded -
      // now deal with the HTTP Response...

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
  except
    on E: Exception do
    begin
      Log('Server exception: %s', [E.Message], lsError);
      raise;
    end;
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
  Log('ProcessHTTPGet: Request Document: %s', [ADocument], lsDebug);
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
  LConfig: TtiWebServerConfig;
begin
  LConfig:= TtiWebServerConfig.Create;
  try
    FBlockStreamCache.EntryIdleLimitMS := LConfig.CacheEntryIdleLimitMS;
    FBlockStreamCache.SweepIntervalMS := LConfig.CacheSweepIntervalMS;
  finally
    LConfig.Free;
  end;

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

function TtiWebServer.GetCacheCurrentSize: LongWord;
begin
  Result := FBlockStreamCache.CurrentSize;
end;

function TtiWebServer.GetCacheMaxSize: LongWord;
begin
  Result := FBlockStreamCache.MaxSize;
end;

function TtiWebServer.GetCacheMaxSizeTime: TDateTime;
begin
  Result := FBlockStreamCache.MaxSizeTime;
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
  // HTTPS/SSL Support:
  // Replace DefaultPort assignment with:
  //FIdHTTPServer.Bindings.Add.Port := AValue;
  //FIdHTTPServer.Bindings.Add.Port := 443; // Move to SetSSLPort
  // Should add SSLPort to TtiWebServerConfig as well as key and cert filenames
  // with setters for the latter.
  // NOTE: To facilitate re-assignment of the ports we would need to store both
  // port numbers in private fields and remove the previous port from the
  // bindings list before adding the new one
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
    Result := tiIsFileNameValid(LFileName) and FileExists(LFileName);
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
  if not SameText(CFavIconFileName, ADocument) then
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
    Log('About to call ' + LCGIApp);
    Log('Parameters: ' + ARequestParams, lsDebug);

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
    Log(LCGIApp + ' completed');
    Log('Response: ' + LResponse, lsDebug);
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

function TtiBlockStreamCache.AddBlockStream(
  const ATransID: string; const AData: string; const ABlockSize: Longword;
  out ABlockCount: LongWord): boolean;
var
  L: TtiCachedBlockStream;
  LTransID: string;
begin
  ABlockCount := 0;
  FCritSect.Enter;
  try
    LTransID := Trim(ATransID);
    Result := (Length(LTransID) > 0) and (ABlockSize > 0) and
      (FindByTransID(LTransID) = nil);
    if Result then
    begin
      L:= TtiCachedBlockStream.Create(AData, ABlockSize);
      Inc(FCurrentSize, Length(AData));
      if FCurrentSize > FMaxSize then
        FMaxSizeTime := Now;
      FMaxSize := Max(FMaxSize, FCurrentSize);
      FList.Add(L);
      L.TransID := LTransID;
      L.LastAccessed:= Now;
      ABlockCount:= L.BlockCount;
    end;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiBlockStreamCache.Clear;
var
  i: Integer;
begin
  FCritSect.Enter;
  try
    i := FList.Count;
    while i > 0 do
    begin
      Dec(i);
      FList.Delete(i);
    end;
    FCurrentSize := 0;
    FMaxSize := 0;
    FMaxSizeTime := Now;
  finally
    FCritSect.Leave;
  end;
  // Logging operations too long/expensive to be in critical section
  Log('tiWebServer cache cleared.', lsDebug);
end;

constructor TtiBlockStreamCache.Create;
begin
  inherited;
  FList:=TObjectList.Create(True);
  FCritSect:= TCriticalSection.Create;
  FSweeper:= TtiThreadBlockStreamCacheSweepForTimeouts.Create(Self);
  FStarted:= False;
end;

destructor TtiBlockStreamCache.Destroy;
begin
  if FStarted then
  begin
    FSweeper.Terminate;
    FSweeper.WakeUp;
    FSweeper.WaitFor;
  end;
  FSweeper.Free;
  FList.Free;
  FCritSect.Free;
  inherited;
end;

function TtiBlockStreamCache.FindByTransID(ATransID: string): TtiCachedBlockStream;
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

function TtiBlockStreamCache.ReadBlock(const ATransID: string; const ABlockIndex: Longword;
  out ABlockCount: LongWord; out ABlockContent: string): boolean;
var
  L: TtiCachedBlockStream;
  LTransID: string;
begin
  FCritSect.Enter;
  try
    LTransID := Trim(ATransID);
    Result := false;
    ABlockContent := '';
    L:= FindByTransID(LTransID);
    if (L <> nil) then
    begin
      ABlockCount := L.BlockCount;
      if (ABlockIndex < ABlockCount) then
      begin
         ABlockContent := L.BlockAsString[ABlockIndex];
         Result := true;
      end;
      L.LastAccessed := Now;
    end
    else
    begin
      // Return ABlockCount = 0 for miss on stream TransID
      ABlockCount := 0;
    end;

  finally
    FCritSect.Leave;
  end;
end;

procedure TtiBlockStreamCache.Start;
begin
  Clear;
  FStarted:= True;
  FSweeper.Start;
end;

procedure TtiBlockStreamCache.SweepForTimeOuts;
var
  i: Integer;
  LStream: TtiCachedBlockStream;
begin
  FCritSect.Enter;
  try
    Log('SweepForTimeOuts: BlockStreamCache count=%d', [FList.Count], lsDebug);
    for i:= FList.Count-1 downto 0 do
    begin
      LStream := FList.Items[i] as TtiCachedBlockStream;
      if LStream.IdleMS > EntryIdleLimitMS then
      begin
        Dec(FCurrentSize, Length(LStream.AsString));
        FList.Delete(i);
      end;
    end;
  finally
    FCritSect.Leave;
  end;
  // Logging operations too long/expensive to be in critical section
  Log('tiWebServer current cache size: %d MB.   Max cache size: %d MB, occurred at: %s',
      [FCurrentSize div 1000000, FMaxSize div 1000000, tiDateTimeToStr(FMaxSizeTime)], lsDebug);
end;

{ TtiCachedBlockStream }

function TtiCachedBlockStream.GetIdleMS: LongWord;
begin
  result := Trunc((Now - LastAccessed) * 24 * 60 * 60 * 1000);
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
begin
  while SleepAndCheckTerminated(FBlockStreamCache.SweepIntervalMS) do
    FBlockStreamCache.SweepForTimeOuts;
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
  if Trim(ADocument) = '' then
    Exit(false); //==>

  LRequestArgs := TStringList.Create;
  try
    LRequestArgs.Delimiter := '/';
    LRequestArgs.StrictDelimiter := true;
    LRequestArgs.DelimitedText := ADocument;
    if LRequestArgs.Count = 0 then
      Exit(false); //==>

    LPassThrough := PassThroughLocation + LRequestArgs[0];
    result := FileExists(LPassThrough);
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
