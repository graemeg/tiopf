{$I tiDefines.inc}

unit tiHTTPIndy;

interface
uses
  Classes
  ,tiHTTP
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPConnection
  ,IdTCPClient
  ,IdHTTP
  ,IdSSL
  ,IdSSLOpenSSL
 ;

type

  {:Uses the Adapter pattern to wrapper a TidHTTP giving a starndard interface.}
  TtiHTTPIndy = class(TtiHTTPAbs)
  private
    FHTTP : TidHTTP;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FProxyPort: integer;
    FProxyServer: string;
    procedure CreateHTTP;
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
    Destructor  Destroy; override;
    class function MappingName: string; override;
  end;

implementation
uses
  tiConstants
  ,tiLog
  ,SysUtils
{$IFDEF MSWINDOWS}
  ,Windows
{$ENDIF}
  ,tiExcept
 ;

var
  URequestID: Integer;

procedure TtiHTTPIndy.CreateHTTP;
begin
  FreeAndNil(FSSLHandler);
  FreeAndNil(FHTTP);
  FHTTP := TidHTTP.Create(nil);
  FHTTP.ProtocolVersion := pv1_0;
  FHTTP.ProxyParams.ProxyPort:= FProxyPort;
  FHTTP.ProxyParams.ProxyServer:= FProxyServer;
  FHTTP.ConnectTimeout := ConnectTimeout;
  FHTTP.ReadTimeout := ReceiveTimeout;

  // HTTPS/SSL support
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FSSLHandler.SSLOptions.SSLVersions := [sslvSSLv23];
  FHTTP.IOHandler := FSSLHandler;
  // Allow for the libraries to be located outside the application path and
  // OS PATH directories. It would be better to use the
  // IdSSLOpenSSLHeaders.IdOpenSSLSetLibPath() added to Indy 10 in May 2012
  // because SetDllDirectory overwrites the previous setting.
{$IFDEF MSWINDOWS}
  if SSLLibraryPath <> '' then
    SetDllDirectory(PWideChar(SSLLibraryPath));
{$ENDIF}
end;

destructor TtiHTTPIndy.Destroy;
begin
  FreeAndNil(FSSLHandler);
  FreeAndNil(FHTTP);
  inherited;
end;

procedure TtiHTTPIndy.DoGet(const AURL : string; AInput, AOutput: TStringStream);
var
  LURL: string;
  LRequestID: Integer;
begin
  Assert(AURL<>'', 'AURL not assigned');
  Assert(AInput<>nil, 'AInput not assigned');
  Assert(AOutput<>nil, 'AInput not assigned');
  CreateHTTP;
  AOutput.Size:= 0;
  LRequestID := URequestID;
  Inc(URequestID);
  try
    FHTTP.Response.KeepAlive:= False;
    FHTTP.Request.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= RequestTIOPFBlockHeader;
    AOutput.Size:= 0;
    LURL := CorrectURL(AddURLParams(AURL, AInput.DataString));
    Log('Request INDY %d: GET START [%s]', [LRequestID, LURL]);
    try
      FHTTP.Get(LURL, AOutput);
      Log('Response INDY %d: GET [%s]', [LRequestID, FHTTP.Response.ResponseText]);
    finally
      Log('Request INDY %d: GET END', [LRequestID]);
    end;
  except
    on e:exception do
    begin
      Log('Request INDY %d: GET ERROR [%s]', [LRequestID, e.message]);
      raise EtiOPFHTTPException.Create(e.message);
    end;
  end;
end;

procedure TtiHTTPIndy.DoPost(const AURL : string; AInput, AOutput: TStringStream);
var
  LRequestID: Integer;
begin
  Assert(AURL<>'', 'AURL not assigned');
  Assert(AInput<>nil, 'AInput not assigned');
  Assert(AOutput<>nil, 'AInput not assigned');
  CreateHTTP;
  AOutput.Size:= 0;
  LRequestID := URequestID;
  Inc(URequestID);
  try
    // Had problem with this error after the app being idle for a period:
    // "Socket Error # 10054 Connection reset by peer"
    // A quick look on Google Groups took me to this discussion:
    // http://groups.google.com.au/group/borland.public.delphi.internet.winsock/browse_thread/thread/21285265e0ab0f69/a6d9c0608aeb691e?lnk=st&q=TidHTTP+%22Socket+Error+%23+10054+Connection+reset+by+peer%22&rnum=1&hl=en#a6d9c0608aeb691e
    FHTTP.Response.KeepAlive:= False;
    FHTTP.Request.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= RequestTIOPFBlockHeader;
    Log('Request INDY %d: POST START [%s]', [LRequestID, AURL]);
    try
      FHTTP.Post(AURL, AInput, AOutput);
      Log('Response INDY %d: POST [%s]', [LRequestID, FHTTP.Response.ResponseText]);
    finally
      Log('Request INDY %d: POST END', [LRequestID]);
    end;
  except
    on e:exception do
    begin
      Log('Request INDY %d: POST ERROR [%s]', [LRequestID, e.message]);
      raise EtiOPFHTTPException.Create(e.message);
    end;
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
  result:= FProxyPort;
end;

function TtiHTTPIndy.GetProxyServer: string;
begin
  result:= FProxyServer;
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
  FProxyPort:= AValue;
end;

procedure TtiHTTPIndy.SetProxyServer(const AValue: string);
begin
  FProxyServer:= AValue;
end;

initialization
  gTIHTTPClass := TtiHTTPIndy;
  gTIHTTPFactory.RegisterMapping(cHTTPIndy, TtiHTTPIndy);
  URequestID := 1;

end.
