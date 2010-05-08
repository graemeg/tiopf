unit tiDBProxyServer;

{$I tiDefines.inc}

interface
uses
   tiXML
  ,IdCustomHTTPServer
  ,tiWebServer
  ,Classes
;

type

  TtiDBProxyServer = class;

  TtiDBPS_ServerVersion = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBPS_ExecuteRemoteXML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBPS_ForceException = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  // For a legacy system. Use TtiDBPS_TestAlive1 in new applications
  TtiDBPS_TestAlive = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBPS_TestAlive1 = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBPS_TestHTML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBPS_TestXML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string;
                      var   AResponseCode: Integer;
                      const AResponseInfo: TIdHTTPResponseInfo); override;
  end;

  TtiDBProxyServer = class(TtiWebServer)
  private
    FXMLTags: TtiXMLTags;
  public
    constructor Create(APort: integer); override;
    destructor  Destroy; override;
    property    XMLTags           : TtiXMLTags read FXMLTags;
  end;


implementation
uses
   tiBaseObject
  ,tiObject
  ,tiUtils
  ,tiLog
  ,tiQuery
  ,tiExcept
  ,tiWebServerConstants
  ,tiConstants
  ,tiQueryRemote_Svr
  ,tiDBProxyServerStats
  ,tiDBConnectionPool
  ,tiOPFManager
  ,tiWebServerVersion
  ,SysUtils
  ,Math

;

{ TtiDBProxyServer }

constructor TtiDBProxyServer.Create(APort: integer);
begin
  inherited Create(APort);
  FXMLTags := TtiXMLTags.Create;
  FXMLTags.OptXMLDBSize := optDBSizeOn;

  ServerActions.Add(TtiDBPS_ExecuteRemoteXML.Create(Self, 10));
  ServerActions.Add(TtiDBPS_ServerVersion.Create(   Self, 11));
  ServerActions.Add(TtiDBPS_TestAlive1.Create(      Self, 12));
  ServerActions.Add(TtiDBPS_TestHTML.Create(        Self, 13));
  ServerActions.Add(TtiDBPS_TestXML.Create(         Self, 14));
  ServerActions.Add(TtiDBPS_TestAlive.Create(       Self, 15));
  ServerActions.Add(TtiDBPS_ForceException.Create(  Self, 16));
  Sort;

end;

destructor TtiDBProxyServer.Destroy;
begin
  FXMLTags.Free;
  inherited;
end;

{ TtiDBPS_ExecuteRemoteXML }

function TtiDBPS_ExecuteRemoteXML.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxy);
end;

procedure TtiDBPS_ExecuteRemoteXML.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  LS: string;
begin
  LS:=ExecuteRemoteXML(ARequestParams);
  tiStringToStream(LS, AResponse);
end;

{ TtiDBPS_TestAlive1 }

function TtiDBPS_TestAlive1.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestAlive1);
end;

procedure TtiDBPS_TestAlive1.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  lResult  : string;
  lDBConnectionName : string;
  LAppServerVersion: TtiAppServerVersionAbs;
  LDatabase: TtiDatabase;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  LAppServerVersion:= gAppServerVersionFactory.CreateInstance;
  try
    LAppServerVersion.LoadDefaultValues;
    lDBConnectionName := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionName;
    try
      LDatabase := GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName);
      try
        LAppServerVersion.SetConnectionStatus(LDatabase.Test);
      finally
        GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, LDatabase);
      end;
    except
      on e:exception do
        LAppServerVersion.SetConnectionStatus(False);
    end;
    LResult:= LAppServerVersion.AsString;
  finally
   LAppServerVersion.Free;
  end;
  tiStringToStream(LResult, AResponse);
end;

{ TtiDBPS_TestHTML }

function TtiDBPS_TestHTML.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestHTML);
end;

procedure TtiDBPS_TestHTML.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  lDBProxyServerStats : TtiDBProxyServerStats;
begin
  // Don't log this. AutoRefresh will cause the log to become full
  //Log('Processing document <' + ADocument +
  //    ' in <' + ClassName + '>');
  lDBProxyServerStats := TtiDBProxyServerStats.Create;
  try
    // ToDo: Read RefreshRate from RequestInfo
    lDBProxyServerStats.TestRefreshRate := 1;
    lDBProxyServerStats.Execute;
    tiStringToStream(lDBProxyServerStats.AsHTML, AResponse);
  finally
    lDBProxyServerStats.Free;
  end;
end;

{ TtiDBPS_TestXML }

function TtiDBPS_TestXML.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestXML);
end;

procedure TtiDBPS_TestXML.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  lDBProxyServerStats : TtiDBProxyServerStats;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  lDBProxyServerStats := TtiDBProxyServerStats.Create;
  try
    // ToDo: Read RefreshRate from RequestInfo
    lDBProxyServerStats.TestRefreshRate := 1;
    lDBProxyServerStats.Execute;
    tiStringToStream(lDBProxyServerStats.AsXML, AResponse);
  finally
    lDBProxyServerStats.Free;
  end;
end;

function TtiDBPS_TestAlive.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestAlive);
end;

procedure TtiDBPS_TestAlive.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
var
  LDatabase : TtiDatabase;
  lResult  : string;
  lDBConnectionName : string;
const
  cPassed =
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="passed" />' +
          '</a>';

  cFailed =
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="failed" />' +
          '</a>';

begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  lDBConnectionName := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionName;
  try
    LDatabase := GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName);
    try
      if LDatabase.Test then
        lResult := cPassed
      else
        lResult := cFailed;
    finally
      GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, LDatabase);
    end;
  except
    on e:exception do
      lResult := cFailed;
  end;
  tiStringToStream(lResult, AResponse);
end;

{ TtiDBPS_ServerVersion }

function TtiDBPS_ServerVersion.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyServerVersion);
end;

procedure TtiDBPS_ServerVersion.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var   AContentType: string;
  var   AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  tiStringToStream((Owner as TtiDBProxyServer).XMLTags.XMLVersion, AResponse);
end;

function _CompareServerActions(AItem1, AItem2: Pointer): Integer;
var
  LItem1, LItem2: TtiWebServerAction;
begin
  Assert(TtiBaseObject(AItem1).TestValid(TtiWebServerAction), CTIErrorInvalidObject);
  Assert(TtiBaseObject(AItem2).TestValid(TtiWebServerAction), CTIErrorInvalidObject);
  LItem1:= TtiWebServerAction(AItem1);
  LItem2:= TtiWebServerAction(AItem2);
  result:= CompareValue(LItem1.SortOrder, LItem2.SortOrder);
end;

{ TtiDBPS_ForceException }

function TtiDBPS_ForceException.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyServerException);
end;

procedure TtiDBPS_ForceException.Execute(
  const ADocument: string;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ARequestParams: string;
  const AResponse: TStream;
  var AContentType: string;
  var AResponseCode: Integer;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  raise EtiOPFProgrammerException.CreateFmt(
    'Test exception raised at the request of the user at %s',
    [DateTimeToStr(now)]);
end;

end.
