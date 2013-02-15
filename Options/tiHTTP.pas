unit tiHTTP;

{$I tiDefines.inc}

interface
uses
  Classes
  ,Contnrs
  ,tiBaseObject
  ,tiConstants
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPConnection
  ,IdTCPClient
  ,IdHTTP
  ,tiWebServerClientConnectionDetails
 ;

const
  cErrorDuplicateHTTPClassMapping    = 'Attempt to register duplicate TtiHTTP class mapping <%s>';
  cErrorUnRegisteredHTTPClassMapping = 'Unregistered TtiHTTP class mapping <%s>';
  cErrorAccessingHTTPServer =
    'Error accessing HTTP server.' + cLineEnding +
    'Exception message: %s ' + cLineEnding +
    'Request type: %s ' + cLineEnding +
    'URL: %s ';

  ctiOPFHTTPBlockHeader= 'tiOPFBlockID';
  ctiOPFHTTPBlockDelim = '/';
  ctiOPDHTTPNullBlockSize = 0;
  ctiOPFHTTPErrorCode= 'tiOPFErrorCode';
  ctiOPFHTTPPassThroughHeader = 'tiOPFPassThrough';
  ctiOPFHTTPIsPassThroughContent = 'true';

  CErrorHTTPRetryLimiteExceded = '%s (After %d attempts)';

type

  TtiHTTPProgressEvent = procedure(ABlockIndex, ABlockCount, ABlockSize: Longword) of object;
  TtiHTTPCheckTerminatedEvent = procedure(var ATerminated: Boolean) of object;

  TtiHTTPGetOrPostMethod = procedure (const AURL : string;
                                      AInput, AOutput: TStringStream) of object;

  {: An abstract HTTP class, used as a wrapper around TidHTTP and MSHTTPXML giving the same interface.
     Adds support for returning large quantities of data in blocks (to avoid proxy server timeout).
     This blocking functionality is supported by TtiDBProxyServer}
  TtiHTTPAbs = class(TtiBaseObject)
  private
    FInput : TStringStream;
    FOutput : TStringStream;
    FFormatExceptions: Boolean;
    FHeaders: TStringList;
    FRequestTIOPFBlockHeader: string;
    FDeriveRequestTIOPFBlockHeader: Boolean;
    FOnProgress: TtiHTTPProgressEvent;
    FOnCheckTerminated: TtiHTTPCheckTerminatedEvent;
    FRetryLimit: Byte;
    FBlockSize: Longword;
    FResolveTimeout: Longword;
    FConnectTimeout: Longword;
    FSendTimeout: Longword;
    FReceiveTimeout: Longword;
    FSSLLibraryPath: string;
    function    GetResponseHeader(const AName: string): string;
    procedure   SetResponseHeader(const AName, AValue: string);
    function    GetResponseTIOPFBlockHeader: string;
    function    GetResponseTIOPFErrorCode: Byte;
    procedure   DoProgressEvent(ABlockIndex, ABlockCount, ABlockSize: Longword);
    function    IsTerminated: Boolean;

  protected
    procedure   DoGetOrPostBlock(
      const AURL: string;
      const AGetOrPostMethod: TtiHTTPGetOrPostMethod;
      const AInput: TStringStream;
      const AOutput: TStringStream;
      const ABlockIndex: LongWord;
      var   ATransID: LongWord;
      out   ABlockCRC: LongWord;
      out   ABlockCount: LongWord); virtual;
    procedure   DoGetOrPostBlockWithRetry(
      const AURL: string;
      const AGetOrPostMethod: TtiHTTPGetOrPostMethod;
      const AInput: TStringStream;
      const AOutput: TStringStream;
      const ABlockIndex: LongWord;
      var   ATransID: LongWord;
      out   ABlockCRC: LongWord;
      out   ABlockCount: LongWord); virtual;
    procedure   DoGetOrPost(const AURL : string; AGetOrPostMethod: TtiHTTPGetOrPostMethod);
    procedure   DoGet(const AURL : string; AInput, AOutput: TStringStream); virtual; abstract;
    procedure   DoPost(const AURL : string; AInput, AOutput: TStringStream); virtual; abstract;

    function    GetProxyPort: integer; virtual; abstract;
    function    GetProxyServer: string; virtual; abstract;
    procedure   SetProxyPort(const AValue: integer); virtual; abstract;
    procedure   SetProxyServer(const AValue: string); virtual; abstract;
    function    GetResponseCode: Integer; virtual; abstract;
    function    GetResponseText: string; virtual; abstract;
    function    GetResponseHeaders: TStringList; virtual;

  public
    class function MappingName: string; virtual; abstract;

    Constructor Create; virtual;
    Destructor  Destroy; override;

    procedure   Clear;
    procedure   Post(const AURL : string);
    procedure   Get(const AURL : string);

    function    CorrectURL(const pURL: string): string;
    function    AddURLParams(const AURL: string; const AParams: string): string;
    function    GetMappingName: string;

    property    Input : TStringStream read FInput;
    property    Output : TStringStream read FOutput;

    property    ProxyServer : string read GetProxyServer write SetProxyServer;
    property    ProxyPort  : integer read GetProxyPort  write SetProxyPort;
    property    ResponseCode: Integer read GetResponseCode;
    property    ResponseText: string  read GetResponseText;
    property    FormatExceptions: Boolean read FFormatExceptions Write FFormatExceptions;
    property    RetryLimit: Byte read FRetryLimit write FRetryLimit;
    property    BlockSize: Longword read FBlockSize write FBlockSize;
    property    ResolveTimeout: Longword read FResolveTimeout write FResolveTimeout;
    property    ConnectTimeout: Longword read FConnectTimeout write FConnectTimeout;
    property    SendTimeout: Longword read FSendTimeout write FSendTimeout;
    property    ReceiveTimeout: Longword read FReceiveTimeout write FReceiveTimeout;
    property    SSLLibraryPath: string read FSSLLibraryPath write FSSLLibraryPath;

    property    ResponseHeaders: TStringList Read GetResponseHeaders;
    property    ResponseHeader[const AName: string]: string Read GetResponseHeader Write SetResponseHeader;

    {: True by default. Can set to False for testing passing of custom headers between client & server}
    property    DeriveRequestTIOPFBlockHeader: Boolean Read FDeriveRequestTIOPFBlockHeader Write FDeriveRequestTIOPFBlockHeader;
    property    RequestTIOPFBlockHeader: string Read FRequestTIOPFBlockHeader Write FRequestTIOPFBlockHeader;
    property    ResponseTIOPFBlockHeader: string Read GetResponseTIOPFBlockHeader;
    property    ResponseTIOPFErrorCode: Byte read GetResponseTIOPFErrorCode;

    property    OnProgress: TtiHTTPProgressEvent Read FOnProgress Write FOnProgress;
    property    OnCheckTerminated: TtiHTTPCheckTerminatedEvent Read FOnCheckTerminated Write FOnCheckTerminated;

  end;

  TtiHTTPClass = class of TtiHTTPAbs;

  TtiHTTPFactory = class(TtiBaseObject)
  private
    FList: TClassList;
    function FindMapping(const pMappingName: string): Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   RegisterMapping(const AMappingName: string; const AMappingClass: TtiHTTPClass);
    function    CreateInstance(const AMappingName: string): TtiHTTPAbs; overload;
    function    CreateInstance(const AConnectionDetails: TtiWebServerClientConnectionDetails): TtiHTTPAbs; overload;
    function    IsInstanceOfType(const AFieldMetaData: TtiHTTPAbs; const AMappingName: string): Boolean;
  end;

function  GTIHTTPFactory: TtiHTTPFactory;
procedure SetDefaultHTTPClass(const pMappingName: string);
function  tiMakeTIOPFHTTPBlockHeader(
  const ABlockIndex, ABlockCount, ABlockSize, ATransID, ABlockCRC: LongWord): string;
procedure tiParseTIOPFHTTPBlockHeader(
  const AValue: string;
  var ABlockIndex, ABlockCount, ABlockSize, ATransID, ABlockCRC: LongWord);
function  tiGetTIOPFHTTPBlockIndex(const AValue: string): LongWord;
{$IFDEF DELPHIXEORABOVE}
function  tiPortFromURL(const AURL: string; const ADef: Word): Word;
{$ENDIF}

const
  cLocalHost = 'http://localhost';

var
  GTIHTTPClass : TtiHTTPClass;

implementation
uses
  Windows
  ,SysUtils
  ,tiUtils
  ,tiCRC32
  ,tiExcept
  ,tiLog
  {$IFDEF DELPHIXEORABOVE}
  ,RegularExpressions
  {$ENDIF}
 ;

var
  UTIHTTPFactory : TtiHTTPFactory;

function GTIHTTPFactory: TtiHTTPFactory;
begin
  if UTIHTTPFactory = nil then
    UTIHTTPFactory := TtiHTTPFactory.Create;
  Result := UTIHTTPFactory;
end;

procedure SetDefaultHTTPClass(const pMappingName: string);
var
  lHTTP : TtiHTTPAbs;
begin
  lHTTP := GTIHTTPFactory.CreateInstance(pMappingName);
  try
    GTIHTTPClass := TtiHTTPClass(lHTTP.ClassType);
  finally
    lHTTP.Free;
  end;
end;

function  tiMakeTIOPFHTTPBlockHeader(
  const ABlockIndex, ABlockCount, ABlockSize, ATransID, ABlockCRC: LongWord): string;
begin
  Result:=
    IntToStr(ABlockIndex) + ctiOPFHTTPBlockDelim +
    IntToStr(ABlockCount) + cTIOPFHTTPBlockDelim +
    IntToStr(ABlockSize)  + cTIOPFHTTPBlockDelim +
    IntToStr(ATransID)    + cTIOPFHTTPBlockDelim +
    IntToStr(ABlockCRC);
end;

procedure tiParseTIOPFHTTPBlockHeader(
  const AValue: string;
  var ABlockIndex, ABlockCount, ABlockSize, ATransID, ABlockCRC: LongWord);
var
  LBlockCount: string;
  LBlockIndex: string;
  LBlockSize:  string;
  LTransID:    string;
  LBlockCRC:   string;
begin
  LBlockIndex:=  tiToken(AValue, ctiOPFHTTPBlockDelim, 1);
  LBlockCount:=  tiToken(AValue, ctiOPFHTTPBlockDelim, 2);
  LBlockSize:=   tiToken(AValue, ctiOPFHTTPBlockDelim, 3);
  LTransID:=     tiToken(AValue, ctiOPFHTTPBlockDelim, 4);
  LBlockCRC:=    tiToken(AValue, ctiOPFHTTPBlockDelim, 5);
  ABlockIndex:=  StrToInt64Def(LBlockIndex,  0);
  ABlockCount:=  StrToInt64Def(LBlockCount,  1);
  ABlockSize:=   StrToInt64Def(LBlockSize,   ctiOPDHTTPNullBlockSize);
  ATransID:=     StrToInt64Def(LTransID,     0);
  ABlockCRC:=    StrToInt64Def(LBlockCRC,    0);
end;

{$IFDEF DELPHIXEORABOVE}
function tiPortFromURL(const AURL: string; const ADef: Word) : Word;
var
  LMatcher : TRegEx;
  LPortStr : string;
  LTempResult : integer;
const
  //Maximum port is 65535 hence only 5 digits------------V
  cPortExtract = '\w+://[-A-Za-z0-9+&@#%?=~_|!,.;]+:(\d{1,5})([/?]+|$).*';
begin
  Result := ADef;
  LMatcher := TRegEx.Create(cPortExtract);
  if LMatcher.IsMatch(AURL) then
  begin
    try
      LPortStr := LMatcher.Replace(AURL,'\1');
      LTempResult := StrToInt(LPortStr);
      if (LTempResult > High(Word)) or (LTempResult <= Low(Word)) then
        Result := ADef
      else
        Result := LTempResult;
    except
      Result := ADef;
    end;
  end;
end;
{$ENDIF}

function  tiGetTIOPFHTTPBlockIndex(const AValue: string): LongWord;
var
  LBlockIndex: string;
begin
  LBlockIndex:=  tiToken(AValue, ctiOPFHTTPBlockDelim, 1);
  Result:=  StrToIntDef(LBlockIndex, 0);
end;

constructor TtiHTTPAbs.Create;
begin
  Inherited;
  FInput           := TStringStream.Create('');
  FOutput          := TStringStream.Create('');
  FFormatExceptions := True;
  FDeriveRequestTIOPFBlockHeader:= True;
  FRetryLimit:= 1;
  FBlockSize:= 0;
end;

destructor TtiHTTPAbs.Destroy;
begin
  FInput.Free;
  FOutput.Free;
  FreeAndNil(FHeaders);
  inherited;
end;

function TtiHTTPAbs.AddURLParams(const AURL, AParams: string): string;
begin
  if AParams = '' then
    Result := AURL
  else
  begin
    Result := AURL;
    if Pos('?', AURL) = 0 then
      Result := Result + '?'
    else
      Result := Result + '&';
    Result := Result + AParams;
  end;
end;

procedure TtiHTTPAbs.Clear;
begin
  FInput.Size := 0;
  FOutput.Size := 0;
  FreeAndNil(FHeaders);
end;

{ TtiHTTPFactory }

constructor TtiHTTPFactory.Create;
begin
  inherited;
  FList:= TClassList.Create;
end;

function TtiHTTPFactory.CreateInstance(const AMappingName: string): TtiHTTPAbs;
var
  lIndex: Integer;
begin
  lIndex := FindMapping(AMappingName);
  if lIndex = -1 then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnRegisteredHTTPClassMapping, [AMappingName]);
  Result := TtiHTTPClass(FList.Items[lIndex]).Create as TtiHTTPAbs;
end;

function TtiHTTPFactory.CreateInstance(const AConnectionDetails: TtiWebServerClientConnectionDetails): TtiHTTPAbs;
begin
  Assert(AConnectionDetails.TestValid, CTIErrorInvalidObject);
  result:= CreateInstance(AConnectionDetails.ConnectWith);
  result.BlockSize:= AConnectionDetails.BlockSize;
  result.RetryLimit:= AConnectionDetails.RetryLimit;
  result.ResolveTimeout:= AConnectionDetails.ResolveTimeout;
  result.ConnectTimeout:= AConnectionDetails.ConnectTimeout;
  result.SendTimeout:= AConnectionDetails.SendTimeout;
  result.ReceiveTimeout:= AConnectionDetails.ReceiveTimeout;
  result.SSLLibraryPath:= AConnectionDetails.SSLLibraryPath;
  if AConnectionDetails.ProxyServerActive then
  begin
    result.ProxyServer := AConnectionDetails.ProxyServerName;
    result.ProxyPort := AConnectionDetails.ProxyServerPort;
  end;
end;

destructor TtiHTTPFactory.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiHTTPFactory.FindMapping(const pMappingName: string): integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
    if SameText(TtiHTTPClass(FList.Items[i]).MappingName, pMappingName) then
    begin
      Result := i;
      Exit; //==>
    end;
end;

function TtiHTTPFactory.IsInstanceOfType(const AFieldMetaData: TtiHTTPAbs; const AMappingName: string): Boolean;
begin
  Assert(AFieldMetaData<> nil, 'AFieldMetaData not assigned');
  Result := SameText(AFieldMetaData.MappingName, AMappingName);
end;

procedure TtiHTTPFactory.RegisterMapping(const AMappingName: string;const AMappingClass: TtiHTTPClass);
begin
  if FindMapping(AMappingName) <> -1 then
    raise EtiOPFProgrammerException.CreateFmt(cErrorDuplicateHTTPClassMapping, [AMappingName]);
  Assert(SameText(AMappingClass.MappingName, AMappingName), 'MappingName <> pMappingClass.MappingName');
  FList.Add(AMappingClass);
end;

function TtiHTTPAbs.GetMappingName: string;
begin
  Result := TtiHTTPAbs(ClassType).MappingName;
end;

function TtiHTTPAbs.CorrectURL(const pURL: string): string;
var
  lPosSlash: Integer;
  lPosParams: Integer;
  lLHS: string;
  lRHS: string;
begin
  lPosSlash := Pos('\', pURL);
  if lPosSlash = 0 then
  begin
    Result := pURL;
    Exit; //==>
  end;

  lPosParams := Pos('?', pURL);
  if lPosParams = 0 then
    Result := tiStrTran(pURL, '\', '/')
  else begin
    lLHS := Copy(pURL, 1, lPosParams-1);
    lRHS := Copy(pURL, lPosParams, Length(pURL));
    Result := tiStrTran(lLHS, '\', '/') + lRHS;
  end;
end;

function TtiHTTPAbs.GetResponseHeaders: TStringList;
begin
  if FHeaders = nil then
  begin
    FHeaders:= TStringList.Create;
    FHeaders.NameValueSeparator:= ':';
  end;
  Result:= FHeaders;
end;

function TtiHTTPAbs.GetResponseHeader(const AName: string): string;
begin
  Result:= Trim(ResponseHeaders.Values[AName]);
end;

procedure TtiHTTPAbs.SetResponseHeader(const AName, AValue: string);
begin
  ResponseHeaders.Values[AName]:= ' ' + AValue;
end;

procedure TtiHTTPAbs.Get(const AURL: string);
begin
  DoGetOrPost(AURL, DoGet);
end;

procedure TtiHTTPAbs.Post(const AURL: string);
begin
  DoGetOrPost(AURL, DoPost);
end;

function TtiHTTPAbs.GetResponseTIOPFBlockHeader: string;
begin
  Result:= ResponseHeader[ctiOPFHTTPBlockHeader];
end;

function TtiHTTPAbs.GetResponseTIOPFErrorCode: Byte;
begin
  Result:= StrToIntDef(ResponseHeader[ctiOPFHTTPErrorCode], 0);
end;

procedure TtiHTTPAbs.DoGetOrPost(const AURL: string; AGetOrPostMethod: TtiHTTPGetOrPostMethod);
var
  LBlockCount: Longword;
  LTransID: Longword;
  LBlockCRC: LongWord;
  i: Integer;
  LInput: TStringStream;
  LOutput: TStringStream;
begin
  Input.Position := 0;
  Output.Size := 0;
  // ToDo: Add code to make this ASync in multiple threads
  //       This will involve storing a TtiBlockedStream and managing the variable order the blocks
  //       may be returned.

  LTransID:= 0;
  LBlockCRC:= 0;
  LBlockCount:= 0;

  DoGetOrPostBlock(AURL, AGetOrPostMethod, Input, Output, 0, LTransID, LBlockCRC, LBlockCount);

  if (LBlockCount > 1) and not IsTerminated then
  begin
    LInput:= TStringStream.Create('');
    try
      LOutput:= TStringStream.Create('');
      try
        for i:= 1 to LBlockCount-1 do // It's zero indexed, but the first call has already been made
        begin
          DoGetOrPostBlockWithRetry(AURL, AGetOrPostMethod, LInput, LOutput,
            i, LTransID, LBlockCRC, LBlockCount);
          Output.CopyFrom(LOutput, 0);
          if IsTerminated then
            Break; //==>
        end;
      finally
        LOutput.Free;
      end;
    finally
      LInput.Free;
    end;
  end;
end;

procedure TtiHTTPAbs.DoGetOrPostBlock(
  const AURL: string;
  const AGetOrPostMethod: TtiHTTPGetOrPostMethod;
  const AInput: TStringStream;
  const AOutput: TStringStream;
  const ABlockIndex: LongWord;
  var   ATransID: LongWord;
  out   ABlockCRC: LongWord;
  out   ABlockCount: LongWord);
var
  LReturnBlockIndex: LongWord;
  LReturnBlockSize: LongWord;
begin
Log('DoGetOrPostBlock block %d', [ABlockIndex]);
  if FDeriveRequestTIOPFBlockHeader then
    RequestTIOPFBlockHeader:= tiMakeTIOPFHTTPBlockHeader(ABlockIndex, ABlockCount, FBlockSize, ATransID, 0);
  AGetOrPostMethod(AURL, AInput, AOutput);
  tiParseTIOPFHTTPBlockHeader(ResponseTIOPFBlockHeader, LReturnBlockIndex, ABlockCount,
    LReturnBlockSize, ATransID, ABlockCRC);
  DoProgressEvent(ABlockIndex, ABlockCount, FBlockSize);
end;

procedure TtiHTTPAbs.DoGetOrPostBlockWithRetry(const AURL: string;
  const AGetOrPostMethod: TtiHTTPGetOrPostMethod; const AInput,
  AOutput: TStringStream; const ABlockIndex: LongWord;
  var ATransID: LongWord; out ABlockCRC, ABlockCount: LongWord);
var
  i: integer;
  LSuccess: Boolean;
begin
  i:= 0;
  LSuccess:= False;
  while (not LSuccess) and
    (i< FRetryLimit) do
  begin
Log('DoGetOrPostBlockWithRetry attempt %d', [i+1]);
    // Within the retry range
    if i < FRetryLimit - 1 then
    begin
      try
        DoGetOrPostBlock(
          AURL, AGetOrPostMethod, AInput,
          AOutput, ABlockIndex, ATransID,
          ABlockCRC, ABlockCount);
        LSuccess:= (ABlockCRC = 0) or (tiCRC32FromStream(AOutput) = ABlockCRC);
      except
        on e:exception do
        begin
Log('DoGetOrPostBlockWithRetry fail: %s', [e.Message]);
          Sleep(1000); // ToDo: Parameterise the retry wait period
          LSuccess:= false;
        end;
      end;
      Inc(i);
    end
    else // The last try, so let exception be raised if there is one
    begin
      try
        DoGetOrPostBlock(
          AURL, AGetOrPostMethod, AInput,
          AOutput, ABlockIndex, ATransID,
          ABlockCRC, ABlockCount);
        LSuccess:= (ABlockCRC = 0) or (tiCRC32FromStream(AOutput) = ABlockCRC);
      except
        on e:exception do
        begin
Log('DoGetOrPostBlockWithRetry fail final: %s', [e.Message]);
          raise EtiOPFHTTPException.CreateFmt(CErrorHTTPRetryLimiteExceded, [e.message, i+1])
        end;
      end;
    end;
  end;
end;

procedure TtiHTTPAbs.DoProgressEvent(ABlockIndex, ABlockCount, ABlockSize: Longword);
begin
  if Assigned(FOnProgress) then
    FOnProgress(ABlockIndex, ABlockCount, ABlockSize);
end;

function TtiHTTPAbs.IsTerminated: Boolean;
begin
  if Assigned(OnCheckTerminated) then
    OnCheckTerminated(Result)
  else
    Result:= False;
end;

initialization

finalization
  FreeAndNil(UTIHTTPFactory);

end.
