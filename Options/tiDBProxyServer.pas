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
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  TtiDBPS_ExecuteRemoteXML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  // For a legacy system. Use TtiDBPS_TestAlive1 in new applications
  TtiDBPS_TestAlive = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  TtiDBPS_TestAlive1 = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  TtiDBPS_TestHTML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  TtiDBPS_TestXML = class(TtiWebServerAction)
  public
    function  CanExecute(const ADocument: string): boolean; override;
    procedure Execute(const ADocument: string; const ARequestParams: string;
                      const AResponse: TStream; var AContentType: string; var AResponseCode: Integer); override;
  end;

  TtiDBProxyServer = class(TtiWebServer)
  private
    FXMLTags: TtiXMLTags;
  public
    constructor Create(APort: integer); override;
    destructor  Destroy; override;
    property    XMLTags            : TtiXMLTags read FXMLTags;
  end;


implementation
uses
   tiBaseObject
  ,tiObject
  ,tiUtils
  ,tiLog
  ,tiExcept
  ,tiWebServerConstants
  ,tiConstants
  ,tiQueryRemote_Svr
  ,tiDBProxyServerStats
  ,tiDBConnectionPool
  ,tiOPFManager

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
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
begin
  tiStringToStream(ExecuteRemoteXML(ARequestParams), AResponse);
end;

{ TtiDBPS_TestAlive1 }

function TtiDBPS_TestAlive1.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestAlive1);
end;

procedure TtiDBPS_TestAlive1.Execute(
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
var
  lPooledDB : TPooledDB;
  lResult   : string;
  lDBConnectionName : string;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  lDBConnectionName := gTIOPFManager.DefaultPerLayer.DefaultDBConnectionName;
  try
    lPooledDB := gTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName);
    try
      if lPooledDB.Database.Test then
        lResult := (Owner as TtiDBProxyServer).XMLTags.ProxyTestPassed
      else
        lResult := (Owner as TtiDBProxyServer).XMLTags.ProxyTestFailed;
    finally
      gTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, lPooledDB);
    end;
  except
    on e:exception do
      lResult := (Owner as TtiDBProxyServer).XMLTags.ProxyTestFailed;
  end;
  tiStringToStream(LResult, AResponse);
end;

{ TtiDBPS_TestHTML }

function TtiDBPS_TestHTML.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyTestHTML);
end;

procedure TtiDBPS_TestHTML.Execute(
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
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
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
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
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
var
  lPooledDB : TPooledDB;
  lResult   : string;
  lDBConnectionName : string;
begin
  Log('Processing document <' + ADocument + '> in <' + ClassName + '>');
  lDBConnectionName := gTIOPFManager.DefaultPerLayer.DefaultDBConnectionName;
  try
    lPooledDB := gTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName);
    try
      if lPooledDB.Database.Test then
        lResult :=
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="passed" />' +
          '</a>'
      else
        lResult :=
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="failed" />' +
          '</a>'
    finally
      gTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, lPooledDB);
    end;
  except
    on e:exception do
      lResult := (Owner as TtiDBProxyServer).XMLTags.ProxyTestFailed;
  end;
  tiStringToStream(lResult, AResponse);
end;

{ TtiDBPS_ServerVersion }

function TtiDBPS_ServerVersion.CanExecute(const ADocument: string): boolean;
begin
  result := SameText(ADocument, cgTIDBProxyServerVersion); 
end;

procedure TtiDBPS_ServerVersion.Execute(
  const ADocument: string; const ARequestParams: string;
  const AResponse: TStream; var AContentType: string; var AResponseCode: Integer);
begin
  tiStringToStream((Owner as TtiDBProxyServer).XMLTags.XMLVersion, AResponse);
end;

function _CompareServerActions(AItem1, AItem2: Pointer): Integer;
var
  LItem1, LItem2: TtiWebServerAction;
begin
  Assert(TtiBaseObject(AItem1).TestValid(TtiWebServerAction), cErrorTIPerObjAbsTestValid);
  Assert(TtiBaseObject(AItem2).TestValid(TtiWebServerAction), cErrorTIPerObjAbsTestValid);
  LItem1:= TtiWebServerAction(AItem1);
  LItem2:= TtiWebServerAction(AItem2);
  result:= CompareValue(LItem1.SortOrder, LItem2.SortOrder);
end;

end.
