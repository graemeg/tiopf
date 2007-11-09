unit tiWebServer;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiStreams
  ,tiThread
  ,SyncObjs
  ,IdHTTPServer
  ,IdCustomHTTPServer
  ,IdContext
  ,Contnrs
  ,Classes
;

const
  cErrorInvalidCachedBlockStreamTransID = 'Invalid cached block TransID "%d"';
  cDefaultBlockStreamCacheTimeout= 120;
  cDefaultBlockStreamCacheSweepEvery= 120;
  cDefaultSleepSec= 10;

type

  TtiWebServer = class;

  TtiWebServerAction = class(TtiBaseObject)
  private
    FOwner: TtiWebServer;
    FSortOrder: Byte;
  protected
    procedure GetReturnPage(const ADocument: string; AResponse: TStream; var AContentType: string);
    function  StaticPageLocation : string;
    function  CGIBinLocation : string;
  public
    constructor Create(const AOwner : TtiWebServer; ASortOrder: Byte);
    function    CanExecute(const ADocument: string): boolean; virtual; abstract;
    procedure   Execute(const ADocument: string; const ARequestParams: string;
                        const AResponse: TStream; var AContentType: string;
                        var   AResponseCode: Integer;
                        const AResponseInfo: TIdHTTPResponseInfo); virtual; abstract;

    property    Owner : TtiWebServer read FOwner write FOwner;
    property    SortOrder: Byte Read FSortOrder;

  end;

  TtiWebServerAction_Default = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  // A hack to ignore a single request currently being used by a web switch
  // to determine which of several servers to direct requests to
  TtiWebServerAction_Ignore = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_CanNotFindPage = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_GetLogFile = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiWebServerAction_CanFindPage = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  // ToDo: Not a true CGI interface, but a hack. Change this to support true CGI
  TtiWebServerAction_RunCGIExtension = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
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

  TtiBlockStreamCache = class(TtiBaseObject)
  private
    FList: TObjectList;
    FCritSect: TCriticalSection;
    FLastTransID: Longword;
    FTimeOutSec: Longword;
    FSweeper: TtiThreadBlockStreamCacheSweepForTimeouts;
    FSweepEverySec: Longword;
    FSleepSec: Longword;
    function    FindByTransID(ATransID: Longword): TtiCachedBlockStream;
    function    GetCount: Longword;
  protected
    property    Count: Longword Read GetCount;
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
  end;

  TtiWebServer = class(TtiBaseObject)
  private
    FIdHTTPServer: TIdHTTPServer;
    FServerActions: TObjectList;
    FStaticPageLocation: string;
    FCGIBinLocation: string;
    FBlockStreamCache: TtiBlockStreamCache;
  protected
    property  ServerActions: TObjectList Read FServerActions;

    procedure DoIDHTTPServerCommandGet(AContext:TIdContext;
                                       ARequestInfo: TIdHTTPRequestInfo;
                                       AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure ProcessHTTPGet(const ADocument: string; const AParams: string;
                             const AResponse: TStream; var AContentType: string;
                             var   AResponseCode: Integer;
                             const AResponseInfo: TIdHTTPResponseInfo);
    procedure ApplyResponseStreamToHTTPResponse(
                             AResponseInfo: TIdHTTPResponseInfo; AResponse: TStream;
                             const AResponseType: string; AResponseCode: Integer);
    procedure CreateDefaultPage;
    procedure Sort;

  public
    constructor Create(APort: integer); virtual;
    destructor  Destroy; override;

    property    StaticPageLocation : string read FStaticPageLocation;
    property    CGIBinLocation    : string read FCGIBinLocation;

    procedure   Start;
    procedure   Stop;

  end;


implementation
uses
   tiObject
  ,tiConstants
  ,tiExcept
  ,tiWebServerConstants
  ,tiUtils
  ,tiLog
  ,tiConsoleApp
  ,tiHTTP
  ,tiWebServerConfig
  ,Math
  ,SysUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
;

{ TtiWebServer }

constructor TtiWebServer.Create(APort: Integer);
begin
  inherited Create;

  FServerActions:= TObjectList.Create(true);
  FServerActions.Add(TtiWebServerAction_Ignore.Create(       Self,  1));
  FServerActions.Add(TtiWebServerAction_Default.Create(      Self,  2));
  FServerActions.Add(TtiWebServerAction_CanFindPage.Create(  Self,  3));
  FServerActions.Add(TtiWebServerAction_GetLogFile.Create(   Self,  4));
  FServerActions.Add(TtiWebServerAction_RunCGIExtension.Create(Self, 5));
  FServerActions.Add(TtiWebServerAction_CanNotFindPage.Create(Self, High(Byte)));

  FIdHTTPServer := TIdHTTPServer.Create(Nil);
  FIdHTTPServer.OnCommandGet := DoIDHTTPServerCommandGet;
  FIdHTTPServer.KeepAlive := False;
  FidHTTPServer.DefaultPort:= APort;

  FBlockStreamCache:= TtiBlockStreamCache.Create;
end;

destructor TtiWebServer.Destroy;
begin
  FIdHTTPServer.Free;
  FServerActions.Free;
  FBlockStreamCache.Free;
  inherited;
end;

procedure TtiWebServer.DoIDHTTPServerCommandGet(
  AContext:TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  LDocument: string;
  LParams: string;
  LResponse: TMemoryStream;
  LContentType: string;
  LResponseCode: Integer;

  LRequestTIOPFBlockHeader: string;
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  LongWord;
  LTransID:    Longword;
  LResponseTIOPFBlockHeader: string;

  LTemp: string; // Change to a stream

begin
  LDocument := ARequestInfo.Document;
  if LDocument[1] = '/' then
    LDocument := Copy(LDocument, 2, Length(LDocument) - 1);

  LParams:= ARequestInfo.UnparsedParams;
  LRequestTIOPFBlockHeader:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LRequestTIOPFBlockHeader, LBlockIndex, LBlockCount, LBlockSize, LTransID);

  LResponseCode:= cHTTPResponseCodeOK;
  LContentType:= cHTTPContentTypeTextHTML;
  LResponse:= TMemoryStream.Create;
  try

    // BlockSize = 0, so don't block result
    if (LBlockSize = 0) then
    begin
      ProcessHTTPGet(LDocument, LParams, LResponse, LContentType, LResponseCode, AResponseInfo);
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(0, 0, 0, 0);
    end
    // BlockSize <> 0 and TransID = 0, a new blocked request
    else if (LBlockSize <> 0) and (LTransID = 0) then
    begin
      ProcessHTTPGet(LDocument, LParams, LResponse, LContentType, LResponseCode, AResponseInfo);
      FBlockStreamCache.AddBlockStream(tiStreamToString(LResponse), LBlockSize, LTemp, LBlockCount, LTransID);
      tiStringToStream(LTemp, LResponse);
      if LBlockCount > 1 then
        Log('Returning block 0 of ' + IntToStr(LBlockCount) + ' in TransID ' + IntToStr(LTransID));
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(0, LBlockCount, LBlockSize, LTransID);
    end
    // BlockSize <> 0 and TransID <> 0, Retrun and existing block from the cache
    else if (LBlockSize <> 0) and (LTransID <> 0) then
    begin
      FBlockStreamCache.ReadBlock(LTransID, LBlockIndex, LTemp);
      tiStringToStream(LTemp, LResponse);
      Log('Returning block ' + IntToStr(LBlockIndex) + ' of ' + IntToStr(LBlockCount) + ' in TransID ' + IntToStr(LTransID));
      LResponseTIOPFBlockHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID);
    end;

    ApplyResponseStreamToHTTPResponse(AResponseInfo, LResponse, LContentType, LResponseCode);
    AResponseInfo.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= LResponseTIOPFBlockHeader;

  finally
    LResponse.Free;
  end;

end;

procedure TtiWebServer.ApplyResponseStreamToHTTPResponse(
  AResponseInfo: TIdHTTPResponseInfo;
  AResponse: TStream;
  const AResponseType: string;
  AResponseCode: Integer);
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
    AResponseInfo.ContentType:= AResponseType;
    AResponseInfo.FreeContentStream:= True;
    AResponseInfo.ContentEncoding:= 'MIME';
  end;
end;

procedure TtiWebServer.ProcessHTTPGet(
  const ADocument: string; const AParams: string;
  const AResponse: TStream; var AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  i : integer;
  LServerAction : TtiWebServerAction;
begin
  Assert(AResponse<>nil, 'AResponse not assigned');
  try
    for i := 0 to FServerActions.Count - 1 do
      if (FServerActions.Items[i] as TtiWebServerAction).CanExecute(ADocument) then
      begin
        LServerAction := (FServerActions.Items[i] as TtiWebServerAction);
        LServerAction.Execute(ADocument, AParams, AResponse,
                              AContentType, AResponseCode, AResponseInfo);
        Exit; //==>
      end;
    // Should not get here - unless can not find page.
    tiStringToStream(Format(cErrorCanNotFindPage, [ADocument]), AResponse);
  except
    on e:exception do
    begin
      LogError(e.message);
      tiStringToStream(Format(cErrorOnServer,[e.Message]), AResponse);
    end;
  end;
end;

{ TtiWebServerAction_ExecuteRemoteXML }

procedure TtiWebServer.Start;
var
  LConfig: TtiWebServerConfig;
begin
  LConfig:= TtiWebServerConfig.Create;
  try
    FStaticPageLocation:= tiAddTrailingSlash(LConfig.PathToStaticPages);
    FCGIBinLocation:= tiAddTrailingSlash(LConfig.PathToCGIBin);
  finally
    LConfig.Free;
  end;

  if not DirectoryExists(StaticPageLocation) then
    ForceDirectories(StaticPageLocation);
  if not DirectoryExists(StaticPageLocation) then
    raise exception.create('Unable to locate or create directory for static pages <' + StaticPageLocation + '>');
  if not FileExists(StaticPageLocation + 'default.htm') then
    CreateDefaultPage;

  Log('Attempting to start HTTP server on port ' + IntToStr(FidHTTPServer.DefaultPort));
  FIdHTTPServer.Active := true;
  Log('HTTP server started');
  Log('Static web pages location <' + StaticPageLocation + '>');
  Log('CGI-Bin location <' + FCGIBinLocation + '>');
end;

procedure TtiWebServer.Stop;
begin
  FIdHTTPServer.Active := False;
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

procedure TtiWebServer.Sort;
begin
  FServerActions.Sort(_CompareWebServerActions);
end;

{ TtiWebServerAction_Default }

function TtiWebServerAction_Default.CanExecute(const ADocument: string): boolean;
begin
  result := ((ADocument = '') and
             (FileExists(StaticPageLocation + cDefaultPageName))) or
            (SameText(ADocument, cDefaultPageName) and
             (FileExists(StaticPageLocation + cDefaultPageName)));
end;

procedure TtiWebServerAction_Default.Execute(
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var
        AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  GetReturnPage(StaticPageLocation + cDefaultPageName, AResponse, AContentType);
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
     (lExt = 'zip') then
  begin
    tiFileToStream(ADocument, AResponse);
    AContentType:= _ExtToMIMEContentType(lExt)
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
  const ADocument: string; const ARequestParams: string;
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
  const ADocument: string; const ARequestParams: string;
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
  const ADocument: string; const ARequestParams: string;
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
begin
  result := (ADocument <> '') and
            (FileExists(CGIBinLocation + ADocument) and
            (SameText(ExtractFileExt(ADocument), '.exe')));
end;

procedure TtiWebServerAction_RunCGIExtension.Execute(
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  lCGI : string;
  ls : string;
  lExitCode : Integer;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  try
    lCGI := CGIBinLocation + ADocument;
    ls := '';
    Log('About to call ' + lCGI + ' ' + ARequestParams);
    lExitCode := tiExecConsoleApp(lCGI, ARequestParams, ls, nil, false);
    if lExitCode = 0 then
      tiStringToStream(ls, AResponse)
    else
    begin
      ls := Format(cErrorHTTPCGIExtension, [ADocument, lExitCode, ls]);
      LogError(ls, false);
      tiMailBugReport(ls);
      // This will cause a browser to fail which is a problem with OPDMSMonitor.exe
      // Req an exit code in the returned XML
      //pResponseInfo.ResponseNo := cHTTPResponseCodeInternalError;
      tiStringToStream(ls, AResponse);
      AResponseInfo.CustomHeaders.Values[ctiOPFHTTPErrorCode]:= IntToStr(lExitCode);
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

procedure TtiWebServer.CreateDefaultPage;
begin
  tiStringToFile(cDefaultPageText,
                  StaticPageLocation + 'default.htm');
end;

{ TtiWebServerAction_TestAlive }

{ TtiWebServerAction_Ignore }

function TtiWebServerAction_Ignore.CanExecute(const ADocument: string): boolean;
begin
  result := (ADocument = cDocumentToIgnore);
end;

procedure TtiWebServerAction_Ignore.Execute(
  const ADocument: string; const ARequestParams: string;
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
end;

destructor TtiBlockStreamCache.Destroy;
begin
  FSweeper.Terminate;
  FSweeper.WaitFor;
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
    // ToDo: Will have to remove this when we start using multi threaded access to the block stream cache
    if ABlockIndex = L.BlockCount-1 then
      FList.Remove(L);
  finally
    FCritSect.Leave;
  end;
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
  Resume;
end;

procedure TtiThreadBlockStreamCacheSweepForTimeouts.Execute;
var
  LStart: TDateTime;
begin
  while not Terminated do
  begin
    LStart:= Now;
    while (not Terminated) and
          ((Now - LStart) <= (cdtOneSecond * FBlockStreamCache.SweepEverySec)) do
      Sleep(FBlockStreamCache.SleepSec*1000); // Higher value uses less resouce, but will take longer to shut down.
    FBlockStreamCache.SweepForTimeOuts;
  end;
end;

end.


