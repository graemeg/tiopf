unit tiHTTP;

{$I tiDefines.inc}

interface
uses
  Classes
  ,Contnrs
  ,tiBaseObject
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
    'Error accessing HTTP server.' + #13#10 +
    'Exception message: %s ' + #13#10 +
    'Request type: %s ' + #13#10 +
    'URL: %s ' + #13#10 +
    'Data: %s';

  ctiOPFHTTPBlockHeader= 'tiOPFBlockID';
  ctiOPFHTTPBlockDelim = '/';
  ctiOPDHTTPNullBlockSize = 0;
  ctiOPFHTTPErrorCode= 'tiOPFErrorCode';

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
    function    GetResponseHeader(const AName: string): string;
    procedure   SetResponseHeader(const AName, AValue: string);
    function    GetResponseTIOPFBlockHeader: string;
    function    GetResponseTIOPFErrorCode: Byte;
    procedure   DoProgressEvent(ABlockIndex, ABlockCount, ABlockSize: Longword);
    function    IsTerminated: Boolean;

  protected
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
    function    GetMappingName: string;

    property    Input : TStringStream read FInput;
    property    Output : TStringStream read FOutput;

    property    ProxyServer : string read GetProxyServer write SetProxyServer;
    property    ProxyPort  : integer read GetProxyPort  write SetProxyPort;
    property    ResponseCode: Integer read GetResponseCode;
    property    ResponseText: string  read GetResponseText;
    property    FormatExceptions: Boolean read FFormatExceptions Write FFormatExceptions;

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

function  gTIHTTPFactory: TtiHTTPFactory;
procedure SetDefaultHTTPClass(const pMappingName: string);
function  tiMakeTIOPFHTTPBlockHeader(ABlockIndex, ABlockCount, ABlockSize, ATransID: Integer): string;
procedure tiParseTIOPFHTTPBlockHeader(const AValue: string; var ABlockIndex, ABlockCount, ABlockSize, ATransID: LongWord);
function  tiGetTIOPFHTTPBlockIndex(const AValue: string): LongWord;

const
  cLocalHost = 'http://localhost';

var
  gTIHTTPClass : TtiHTTPClass;
  gTIOPFHTTPDefaultBlockSize: Longword;

implementation
uses
  Windows
  ,tiConstants
  ,SysUtils
  ,tiUtils
  ,tiXML
 ;

var
  uTIHTTPFactory : TtiHTTPFactory;

function gTIHTTPFactory: TtiHTTPFactory;
begin
  if uTIHTTPFactory = nil then
    uTIHTTPFactory := TtiHTTPFactory.Create;
  Result := uTIHTTPFactory;
end;

procedure SetDefaultHTTPClass(const pMappingName: string);
var
  lHTTP : TtiHTTPAbs;
begin
  lHTTP := gTIHTTPFactory.CreateInstance(pMappingName);
  try
    gTIHTTPClass := TtiHTTPClass(lHTTP.ClassType);
  finally
    lHTTP.Free;
  end;
end;

function  tiMakeTIOPFHTTPBlockHeader(ABlockIndex, ABlockCount, ABlockSize, ATransID: Integer): string;
begin
  Result:=
    IntToStr(ABlockIndex) + ctiOPFHTTPBlockDelim +
    IntToStr(ABlockCount) + cTIOPFHTTPBlockDelim +
    IntToStr(ABlockSize)  + cTIOPFHTTPBlockDelim +
    IntToStr(ATransID);
end;

procedure tiParseTIOPFHTTPBlockHeader(const AValue: string;
  var ABlockIndex, ABlockCount, ABlockSize, ATransID: LongWord);
var
  LBlockCount: string;
  LBlockIndex: string;
  LBlockSize:  string;
  LTransID:    string;
begin
  LBlockIndex:=  tiToken(AValue, ctiOPFHTTPBlockDelim, 1);
  LBlockCount:=  tiToken(AValue, ctiOPFHTTPBlockDelim, 2);
  LBlockSize:=   tiToken(AValue, ctiOPFHTTPBlockDelim, 3);
  LTransID:=     tiToken(AValue, ctiOPFHTTPBlockDelim, 4);
  ABlockIndex:=  StrToIntDef(LBlockIndex,  0);
  ABlockCount:=  StrToIntDef(LBlockCount,  1);
  ABlockSize:=   StrToIntDef(LBlockSize,   ctiOPDHTTPNullBlockSize);
  ATransID:=     StrToIntDef(LTransID,     0);
end;

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
end;

destructor TtiHTTPAbs.Destroy;
begin
  FInput.Free;
  FOutput.Free;
  FreeAndNil(FHeaders);
  inherited;
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
    raise Exception.CreateFmt(cErrorUnRegisteredHTTPClassMapping, [AMappingName]);
  Result := TtiHTTPClass(FList.Items[lIndex]).Create as TtiHTTPAbs;
end;

function TtiHTTPFactory.CreateInstance(const AConnectionDetails: TtiWebServerClientConnectionDetails): TtiHTTPAbs;
begin
  Assert(AConnectionDetails.TestValid, CTIErrorInvalidObject);
  result:= CreateInstance(AConnectionDetails.ConnectWith);
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
    raise Exception.CreateFmt(cErrorDuplicateHTTPClassMapping, [AMappingName]);
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
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  LongWord;
  LTransID: Longword;
  i: Integer;
  LInput: TStringStream;
  LOutput: TStringStream;
begin
  Input.Position := 0;
  Output.Size := 0;
  // ToDo: Add code to make this ASync in multiple threads
  //       This will involve storing a TtiBlockedStream and managing the variable order the blocks
  //       may be returned.
  if FDeriveRequestTIOPFBlockHeader then
    RequestTIOPFBlockHeader:= tiMakeTIOPFHTTPBlockHeader(0, 0, gTIOPFHTTPDefaultBlockSize, 0);
  AGetOrPostMethod(AURL, Input, Output);
  DoProgressEvent(0, 0, 0);
  tiParseTIOPFHTTPBlockHeader(ResponseTIOPFBlockHeader, LBlockIndex, LBlockCount, LBlockSize, LTransID);
  if (LBlockCount > 1) and not IsTerminated then
  begin
    LInput:= TStringStream.Create('');
    try
      LOutput:= TStringStream.Create('');
      try
        for i:= 1 to LBlockCount-1 do // It's zero indexed, but the first call has already been made
        begin
          DoProgressEvent(i, LBlockCount, gTIOPFHTTPDefaultBlockSize);
          RequestTIOPFBlockHeader:= tiMakeTIOPFHTTPBlockHeader(i, LBlockCount, gTIOPFHTTPDefaultBlockSize, LTransID);
          AGetOrPostMethod(AURL, LInput, LOutput);
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
  gTIOPFHTTPDefaultBlockSize:= 0;

finalization
  FreeAndNil(uTIHTTPFactory);

end.


