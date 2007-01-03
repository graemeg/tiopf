unit tiHTTPMSXml;

{$I tiDefines.inc}

interface
uses
   Classes
  ,tiBaseObject
  ,tiHTTP
  ,Windows
  ,MSXML_TLB
 ;

const
  cErrorHTTPServer = 'HTTP/1.1 %d Internal Server Error';
  cSleepPeriod = 10;

type

  {:Uses the Adapter pattern to wrapper a IXMLHttpRequest giving a starndard interface.}
  TtiHTTPMSXML = class(TtiHTTPAbs)
  private
    FHTTP : IXMLHttpRequest;
    FAutoFlushCache: boolean;
    FLastCallTime: DWord;
  protected
    procedure   DoGet(const AURL : string; AInput, AOutput: TStringStream); override;
    procedure   DoPost(const AURL : string; AInput, AOutput: TStringStream); override;

    function    GetProxyPort: integer; override;
    function    GetProxyServer: string; override;
    procedure   SetProxyPort(const AValue: integer); override;
    procedure   SetProxyServer(const AValue: string); override;
    function    GetResponseCode: Integer; override;
    function    GetResponseText: string; override;
    function    GetResponseHeaders: TStringList; override;
  public
    Constructor Create; override;
    Destructor  Destroy; override;
    class function MappingName: string; override;
    property    AutoFlushCache: boolean read FAutoFlushCache write FAutoFlushCache;
  end;

implementation
uses
   tiUtils
  ,tiConstants
  ,tiWin32
  ,tiExcept
  ,SysUtils
  ,Math
  ,ComObj

 ;

var
  FCacheFlushParam: DWord;

constructor TtiHTTPMSXML.Create;
begin
  inherited;
  FAutoFlushCache:= True;
  FLastCallTime:= GetTickCount;
  tiWin32CoInitialize;
  try
    FHTTP := CoXMLHTTPRequest.Create;
  except
    // 03/01/2007. PH. In rare cases, tiWin32CoInitialize will wrongly
    // detect that CoInitialize has been called in this thread. Not
    // sure why this is happening, but the code below is an attempt
    // to work around this problem.
    on e:EOleSysError do
    begin
      FHTTP:= nil;
      tiWin32ForceCoInitialize;
      FHTTP := CoXMLHTTPRequest.Create;
    end;
  end;
end;

destructor TtiHTTPMSXML.Destroy;
begin
  FHTTP:= nil;
  tiWin32CoUnInitialize;
  inherited;
end;

procedure TtiHTTPMSXML.DoGet(const AURL : string; AInput, AOutput: TStringStream);
var
  lURL: string;
begin
  lURL := CorrectURL(AURL);
  // Hack around the MSXML 'feature' of caching pages, which may not be what
  // we want
  if FAutoFlushCache then
  begin
    LURL:= LURL + '?CacheFlushParam=' + IntToStr(FCacheFlushParam);
    Inc(FCacheFlushParam);
  end;

  try
    FHTTP.open('GET', lURL, False, '', '');
    if RequestTIOPFBlockHeader <> '' then
      FHTTP.setRequestHeader(ctiOPFHTTPBlockHeader, RequestTIOPFBlockHeader);
    FHTTP.Send(AInput.DataString);
    if FHTTP.Get_Status <> 200 then
      raise Exception.CreateFmt(cErrorHTTPServer, [FHTTP.Get_Status]);
    AOutput.Size := 0;
    AOutput.WriteString(FHTTP.responseText);
  except
    on e:exception do
    begin
      if FormatExceptions then
        raise EtiOPFProgrammerException.CreateFmt(cErrorAccessingHTTPServer,
          [e.message, 'Get', AURL, Input.DataString])
      else
        raise;
    end;
  end;
end;

procedure TtiHTTPMSXML.DoPost(const AURL : string; AInput, AOutput: TStringStream);
var
  lURL: string;
  LTimeSinceLastCall: DWord;
begin
  lURL := CorrectURL(AURL);
  // Hack around the MSXML 'feature' of caching pages, which may not be what
  // we want
  if FAutoFlushCache then
  begin
    LURL:= LURL + '?CacheFlushParam=' + IntToStr(FCacheFlushParam);
    Inc(FCacheFlushParam);
  end;

  //This fixes the error when thrashing MSXML (sometimes)
  LTimeSinceLastCall:= GetTickCount - FLastCallTime;
  if LTimeSinceLastCall < cSleepPeriod then
    Sleep(Max(cSleepPeriod-LTimeSinceLastCall, cSleepPeriod));
  FLastCallTime:= GetTickCount;

  try
    FHTTP.open('POST', lURL, False, '', '');
    if RequestTIOPFBlockHeader <> '' then
      FHTTP.setRequestHeader(ctiOPFHTTPBlockHeader, RequestTIOPFBlockHeader);
    FHTTP.Send(AInput.DataString);
    if FHTTP.Get_Status <> 200 then
      raise Exception.CreateFmt(cErrorHTTPServer, [FHTTP.Get_Status]);
    AOutput.Size := 0;
    AOutput.WriteString(FHTTP.responseText);

  except
    on e:exception do
    begin
      if FormatExceptions then
        raise EtiOPFProgrammerException.CreateFmt(cErrorAccessingHTTPServer,
          [e.message, 'Post', AURL, Input.DataString])
      else
        raise;
    end;
  end;
end;

function TtiHTTPMSXML.GetResponseHeaders: TStringList;
begin
  Result:= inherited GetResponseHeaders;
  Result.Text:= FHTTP.getAllResponseHeaders;
end;

function TtiHTTPMSXML.GetProxyPort: integer;
begin
  Assert(False, 'GetProxyPort not available in ' + ClassName);
  Result := 0;
end;

function TtiHTTPMSXML.GetProxyServer: string;
begin
  Assert(False, 'GetProxyServer not available in ' + ClassName);
  result := '';
end;

function TtiHTTPMSXML.GetResponseCode: Integer;
begin
  result := FHTTP.Get_Status;
end;

function TtiHTTPMSXML.GetResponseText: string;
var
  lResponseCode: Integer;
begin
  lResponseCode := ResponseCode;
  if lResponseCode = 200 then
    Result := 'HTTP/1.1 200 OK'
  else
    Result := Format(cErrorHTTPServer, [lResponseCode])
end;

class function TtiHTTPMSXML.MappingName: string;
begin
  Result := cHTTPMsXml;
end;

procedure TtiHTTPMSXML.SetProxyPort(const AValue: integer);
begin
  Assert(False, 'SetProxyPort not available in ' + ClassName);
end;

procedure TtiHTTPMSXML.SetProxyServer(const AValue: string);
begin
  Assert(False, 'SetProxyServer not available in ' + ClassName);
end;

initialization
  gTIHTTPClass := TtiHTTPMSXML;
  gTIHTTPFactory.RegisterMapping(cHTTPMsXml, TtiHTTPMSXML);
  FCacheFlushParam:= 0;

end.
