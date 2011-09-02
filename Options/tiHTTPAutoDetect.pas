unit tiHTTPAutoDetect;

{$I tiDefines.inc}

interface
uses
  Classes
  ,Contnrs
  ,tiBaseObject
  ,tiHTTP
  ,tiXML
  ,tiConstants
  ,tiWebServerClientConnectionDetails
 ;

const

  cLogMessageCheckingURL            = 'Checking URL %s';
  cLogMessageCheckingWithIndy       = '  Checking with Indy HTTP communication library';
  cLogMessageCheckingWithIndyDirect = '    Checking direct connection with Indy';
  cLogMessageDetectingProxySettings = '    Detecting proxy settings';
  cLogMessageCheckingWithIndyProxy  = '    Checking direct connection via proxy server';
  cLogMessageCheckingWithMSXMLHTTP  = '  Checking with Microsoft XML-HTTP library';

  cLogMessageConnectedWithIndyDirect= '      Connected with Indy :)';
  cLogMessageConnectedWithIndyProxy = '      Connected with Indy via proxy server :)';
  cLogMessageConnectedWithMSXMLHTTP = '      Connected with Microsoft XML-HTTP library:)';

  cLogMessageFailedWithIndyDirect   = '      Failed connecting with Indy';
  cLogMessageFailedWithIndyProxy    = '      Failed connecting with Indy via proxy server';
  cLogMessageFailedWithMSXMLHTTP    = '      Failed connecting with Microsoft XML-HTTP library';

  cLogMessageConnectionSummary      = 'The following connection parameters will be used:';
  cLogMessageCSURL                  = '   URL:           %s';
  cLogMessageCSConnectWith          = '   Connect with:  %s';
  cLogMessageCSProxyActive          = '   Proxy active:  %s';
  cLogMessageCSProxyName            = '   Proxy address: %s';
  cLogMessagecsProxyPort            = '   Proxy port:    %d';
  cLogMessageFailedToDetectConnection = 'Unable to detect a connection';


type

  TtiHTTPAutoDetectLogEvent = procedure(const AMessage: string) of object;

  TtiHTTPAutoDetectServer = class(TtiBaseObject)
  private
    FXMLTags: TtiXMLTags;
    FURLList: TStringList;
    FOnLog: TtiHTTPAutoDetectLogEvent;
    FConnectionDetails: TtiWebServerClientConnectionDetails;
    function    TestOne(const pURL: string): Boolean;
    function    TestOneWithIndy(const AURL: string): Boolean;
    function    TestOneWithIndyAndProxy(const AURL: string): Boolean;
    function    TestOneWithMSXMLHTTP(const AURL: string): Boolean;
    procedure   DoLog(const AMessage: string; const pArgs: array of const); overload;
    procedure   DoLog(const AMessage: string); overload;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Execute: Boolean;

    // Input
    property    URLList: TStringList read FURLList;
    property    OnLog: TtiHTTPAutoDetectLogEvent read FOnLog Write FOnLog;

    // Outputs
    property    ConnectionDetails: TtiWebServerClientConnectionDetails read FConnectionDetails;

  end;

  TtiIEProxySettings = class(TtiBaseObject)
  private
    FProxyServerActive: Boolean;
    FProxyServerName: string;
    FProxyServerPort:Integer;
    function  ParseIeProxyStr(const ProxyStr: string): Boolean;
    procedure ParseIeProxyBypassStr(const BypassStr: string);
  public
    procedure Execute;
    property  ProxyServerActive: Boolean read FProxyServerActive;
    property  ProxyServerName: string read FProxyServerName;
    property  ProxyServerPort: Integer read FProxyServerPort;
  end;

function tiDetectProxySettings(var pProxyServer : string; var pProxyPort : Integer): boolean;
function tiHTTPTestConnection(const AConnectionDetails: TtiWebServerClientConnectionDetails): Boolean;


implementation
uses
  Windows
  ,SysUtils
  ,tiUtils
  ,Registry
  ,WinINet
  ,tiWebServerVersion
 ;

function tiDetectProxySettings(var pProxyServer : string; var pProxyPort : Integer): boolean;
var
  lIEProxySettings : TtiIEProxySettings;
begin
  lIEProxySettings := TtiIEProxySettings.Create;
  try
    lIEProxySettings.Execute;
    Result:= lIEProxySettings.ProxyServerActive;
    if Result then
    begin
      pProxyServer := lIEProxySettings.ProxyServerName;
      pProxyPort  := lIEProxySettings.ProxyServerPort;
    end;
  finally
    lIEProxySettings.Free;
  end;
end;

function tiHTTPTestConnection(const AConnectionDetails: TtiWebServerClientConnectionDetails): Boolean;
var
  LHTTP: TtiHTTPAbs;
  LS : string;
begin
  LHTTP:= gtiHTTPFactory.CreateInstance(AConnectionDetails);
  try
    try
      LHTTP.Post(AConnectionDetails.AppServerURL + '/' + cgTIDBProxyTestAlive1);
      LS := LHTTP.Output.DataString;
      result:= LS <> '';
    except
      on e:Exception do
        Result := False;
    end;
  finally
    LHTTP.Free;
  end;
end;

procedure TtiIEProxySettings.Execute;
var
   ProxyInfo: PInternetProxyInfo;
   Len: LongWord;
   ProxyStr: string;
   ProxyBypassStr: string;
begin
   Len := 4096;
   GetMem(ProxyInfo, Len);
   try
     if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len) then
     begin
       if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
       begin
         ProxyStr := ProxyInfo^.lpszProxy;
         if not ParseIeProxyStr(ProxyStr) then
           FProxyServerActive := False
         else
         begin
           FProxyServerActive := True;
           ProxyBypassStr := ProxyInfo^.lpszProxyBypass;
           ParseIeProxyBypassStr(ProxyBypassStr);
         end;
       end;
     end;
   finally
     FreeMem(ProxyInfo);
   end;
end;

function TtiIEProxySettings.ParseIeProxyStr(const ProxyStr: string): Boolean;
var
   I: Integer;
   ServerPortStr: string;
begin
   if Pos('=', ProxyStr) = 0 then
     ServerPortStr := ProxyStr
   else
   begin
     I := Pos('http=', ProxyStr);
     if I = 0 then
       ServerPortStr := ''
     else
     begin
       ServerPortStr := Copy(ProxyStr, I+5, MaxInt);
       I := Pos(' ', ServerPortStr);
       if I > 0 then
         ServerPortStr := Copy(ServerPortStr, 1, I-1)
       else
       begin
         I := Pos(';', ServerPortStr);
         if I > 0 then
         begin
           ServerPortStr := Copy(ServerPortStr, 1, I-1);
         end;
       end;
     end;
   end;

   Result := ServerPortStr <> '';

   if Result then
   begin
     I := Pos(':', ServerPortStr);
     if I <= 0 then
       Result := False
     else
     begin
       FProxyServerName := Copy(ServerPortStr, 1, I-1);
       FProxyServerPort := StrToIntDef(Copy(ServerPortStr, I+1, MaxInt), 0);
       if FProxyServerPort = 0 then
       begin
         Result := False;
       end;
     end;
   end;
end;

procedure TtiIEProxySettings.ParseIeProxyBypassStr(const BypassStr: string);
begin
//   FIeProxyBypasses.CommaText := BypassStr;
end;

{ TtiHTTPAutoDetectServer }

constructor TtiHTTPAutoDetectServer.Create;
begin
  inherited Create;
  FXMLTags:= TtiXMLTags.Create;
  FXMLTags.OptXMLDBSize:= optDBSizeOn;
  FURLList:= TStringList.Create;
  FConnectionDetails:= TtiWebServerClientConnectionDetails.Create;
end;

destructor TtiHTTPAutoDetectServer.Destroy;
begin
  FXMLTags.Free;
  FURLList.Free;
  FConnectionDetails.Free;
  inherited;
end;

procedure TtiHTTPAutoDetectServer.DoLog(const AMessage: string; const pArgs: array of const);
begin
  if Assigned(FOnLog) then
    FOnLog(Format(AMessage, pArgs));
end;

procedure TtiHTTPAutoDetectServer.DoLog(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

function TtiHTTPAutoDetectServer.Execute: Boolean;
var
  i : Integer;
begin
  Assert(FURLList.Count > 0, 'No URLs to test');
  Result := False;
  for i := 0 to FURLList.Count - 1 do
    if TestOne(FURLList.Strings[i]) then
    begin
      Result := True;
      Break; //==>
    end;
  if Result then
  begin
    DoLog('');
    DoLog(cLogMessageConnectionSummary);
    DoLog(cLogMessageCSURL, [ConnectionDetails.AppServerURL]);
    DoLog(cLogMessageCSConnectWith, [ConnectionDetails.ConnectWith]);
    DoLog(cLogMessageCSProxyActive, [tiBoolToStr(ConnectionDetails.ProxyServerActive)]);
    DoLog(cLogMessageCSProxyName,   [ConnectionDetails.ProxyServerName]);
    DoLog(cLogMessagecsProxyPort,   [ConnectionDetails.ProxyServerPort]);
  end else
    DoLog(cLogMessageFailedToDetectConnection);
end;

function TtiHTTPAutoDetectServer.TestOne(const pURL: string): Boolean;
begin
  Result := True;
  DoLog(cLogMessageCheckingURL, [pURL]);
  if TestOneWithIndy(pURL)then
    Exit; //==>
  DoLog('');
  if TestOneWithIndyAndProxy(pURL) then
    Exit; //==>
  DoLog('');
  if TestOneWithMSXMLHTTP(pURL) then
    Exit; //==>
  DoLog('');
  Result := False;
end;

function TtiHTTPAutoDetectServer.TestOneWithIndy(const AURL: string): Boolean;
var
  LConnectionDetails: TtiWebServerClientConnectionDetails;
begin
  DoLog(cLogMessageCheckingWithIndy, []);
  DoLog(cLogMessageCheckingWithIndyDirect, []);
  LConnectionDetails:= TtiWebServerClientConnectionDetails.Create;
  try
    LConnectionDetails.AppServerURL:= AURL;
    LConnectionDetails.ConnectWith:= cHTTPIndy;
    LConnectionDetails.ProxyServerActive:= False;
    LConnectionDetails.ProxyServerPort  := 0;
    LConnectionDetails.ProxyServerName  := '';
    Result := tiHTTPTestConnection(LConnectionDetails);
    if Result then
    begin
      ConnectionDetails.Assign(LConnectionDetails);
      DoLog(cLogMessageConnectedWithIndyDirect);
    end else
      DoLog(cLogMessageFailedWithIndyDirect);
  finally
    LConnectionDetails.Free;
  end;
end;

function TtiHTTPAutoDetectServer.TestOneWithIndyAndProxy(const AURL: string): Boolean;
var
  lProxyAddress: string;
  lProxyPort: Integer;
  LConnectionDetails: TtiWebServerClientConnectionDetails;
begin
  Result:= False;
  DoLog(cLogMessageDetectingProxySettings, []);
  DoLog(cLogMessageCheckingWithIndyProxy, []);
  LConnectionDetails:= TtiWebServerClientConnectionDetails.Create;
  try
    if tiDetectProxySettings(lProxyAddress, lProxyPort) then
    begin
      LConnectionDetails.AppServerURL:= AURL;
      LConnectionDetails.ConnectWith:= cHTTPIndy;
      LConnectionDetails.ProxyServerActive:= True;
      LConnectionDetails.ProxyServerPort  := lProxyPort;
      LConnectionDetails.ProxyServerName  := lProxyAddress;
      Result := tiHTTPTestConnection(LConnectionDetails);
    end;

    if Result then
    begin
      ConnectionDetails.Assign(LConnectionDetails);
      DoLog(cLogMessageConnectedWithIndyProxy);
    end else
      DoLog(cLogMessageFailedWithIndyProxy);
  finally
    LConnectionDetails.Free;
  end;
end;

function TtiHTTPAutoDetectServer.TestOneWithMSXMLHTTP(const AURL: string): Boolean;
var
  LConnectionDetails: TtiWebServerClientConnectionDetails;
begin
  DoLog(cLogMessageCheckingWithMSXMLHTTP);
  LConnectionDetails:= TtiWebServerClientConnectionDetails.Create;
  try
    LConnectionDetails.AppServerURL:= AURL;
    LConnectionDetails.ConnectWith:= cHTTPMSXML;
    LConnectionDetails.ProxyServerActive:= False;
    LConnectionDetails.ProxyServerPort  := 0;
    LConnectionDetails.ProxyServerName  := '';
    Result := tiHTTPTestConnection(LConnectionDetails);
    if Result then
    begin
      ConnectionDetails.Assign(LConnectionDetails);
      DoLog(cLogMessageConnectedWithMSXMLHTTP);
    end else
      DoLog(cLogMessageFailedWithMSXMLHTTP);
  finally
    LConnectionDetails.Free;
  end;
end;

end.

