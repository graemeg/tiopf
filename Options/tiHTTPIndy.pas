unit tiHTTPIndy;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiHTTP
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPConnection
  ,IdTCPClient
  ,IdHTTP
 ;

type

  {:Uses the Adapter pattern to wrapper a TidHTTP giving a standard interface.}
  TtiHTTPIndy = class(TtiHTTPAbs)
  private
    FHTTP : TidHTTP;
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
  end;

implementation
uses
  tiConstants
  ,SysUtils
  ,tiExcept
 ;

constructor TtiHTTPIndy.Create;
begin
  inherited;
  FHTTP := TidHTTP.Create(nil);
  FHTTP.ProtocolVersion := pv1_0;
end;

destructor TtiHTTPIndy.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

procedure TtiHTTPIndy.DoGet(const AURL : string; AInput, AOutput: TStringStream);
begin
  Assert(AURL<>'', 'AURL not assigned');
  Assert(AInput<>nil, 'AInput not assigned');
  Assert(AOutput<>nil, 'AInput not assigned');
  AOutput.Size:= 0;
  try
    FHTTP.Response.KeepAlive:= False;
    FHTTP.Request.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= RequestTIOPFBlockHeader;
    AOutput.Size:= 0;
    FHTTP.Get(AURL + '?' + AInput.DataString, AOutput);
  except
    on e:exception do
      raise EtiOPFHTTPException.Create(e.message);
  end;
end;

procedure TtiHTTPIndy.DoPost(const AURL : string; AInput, AOutput: TStringStream);
begin
  Assert(AURL<>'', 'AURL not assigned');
  Assert(AInput<>nil, 'AInput not assigned');
  Assert(AOutput<>nil, 'AInput not assigned');
  AOutput.Size:= 0;
  try
    // Had problem with this error after the app being idle for a period:
    // "Socket Error # 10054 Connection reset by peer"
    // A quick look on Google Groups took me to this discussion:
    // http://groups.google.com.au/group/borland.public.delphi.internet.winsock/browse_thread/thread/21285265e0ab0f69/a6d9c0608aeb691e?lnk=st&q=TidHTTP+%22Socket+Error+%23+10054+Connection+reset+by+peer%22&rnum=1&hl=en#a6d9c0608aeb691e
    FHTTP.Response.KeepAlive:= False;
    FHTTP.Request.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= RequestTIOPFBlockHeader;
    FHTTP.Post(AURL, AInput, AOutput);
  except
    on e:exception do
      raise EtiOPFHTTPException.Create(e.message);
  end;
end;

function TtiHTTPIndy.GetResponseHeaders: TStringList;
var
  i: Integer;
begin
  Result:= inherited GetResponseHeaders;
  Result.Clear;
  for i:= 0 to FHTTP.Response.RawHeaders.Count-1 do
    Result.Add(FHTTP.Response.RawHeaders.Strings[i]);
end;

function TtiHTTPIndy.GetProxyPort: integer;
begin
  result := FHTTP.ProxyParams.ProxyPort;
end;

function TtiHTTPIndy.GetProxyServer: string;
begin
  result := FHTTP.ProxyParams.ProxyServer;
end;

function TtiHTTPIndy.GetResponseCode: Integer;
begin
  Result := FHTTP.ResponseCode;
end;

function TtiHTTPIndy.GetResponseText: string;
begin
  Result := FHTTP.ResponseText;
end;

class function TtiHTTPIndy.MappingName: string;
begin
  Result := cHTTPIndy;
end;

procedure TtiHTTPIndy.SetProxyPort(const AValue: integer);
begin
  FHTTP.ProxyParams.ProxyPort := AValue;
end;

procedure TtiHTTPIndy.SetProxyServer(const AValue: string);
begin
  FHTTP.ProxyParams.ProxyServer := AValue;
end;

initialization
  gTIHTTPClass := TtiHTTPIndy;
  gTIHTTPFactory.RegisterMapping(cHTTPIndy, TtiHTTPIndy);

end.
