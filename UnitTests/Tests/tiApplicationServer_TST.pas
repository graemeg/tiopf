unit tiApplicationServer_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFrameWork,
  tiWebServer_tst,
  tiWebServer,
  Classes,
  tiOPFTestCase,
  tiOPFTestManager;

type

  TtiApplicationServerTestCase = class(TtiTestCase)
  private
    OrigDefaultPersistenceLayer: string;
    DefaultPersistenceLayerWasEmpty: boolean;
    DummySetup: TtiOPFTestSetupData;
  protected
    procedure PrepareExecute(AAction: TtiWebServerAction; AResult: TStream); overload;
    procedure PrepareExecute
        (ARequestParams: string; AAction: TtiWebServerAction; AResult: TStream); overload;
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    function StringToCompressedMIMEEncoded(AValue: string): string;
    function CompressedMIMEEncodedtoString(AValue: string): string;
    function RemoveAllMatches(const AInputString: string; const ARegex: string; const AReplacement: string): string;
    class function PersistenceLayerName: string;
    procedure SetDummyDBIsConnected(AValue: boolean);
  published
    procedure tiApplicationServer_Create;
    procedure tiApplicationServer_ServerVersion;
    procedure tiApplicationServer_ExecuteRemoteXML;
    procedure tiApplicationServer_TestAlive1;
    procedure tiApplicationServer_TestHTML;
    procedure tiApplicationServer_TestXML;
    procedure tiApplicationServer_TestAlive;
    procedure tiApplicationServer_ForceException;
  end;

procedure RegisterTests;

implementation

uses
  SysUtils,
  tiDBProxyServer,
  tiUtils,
  tiTestDependencies,
  tiXML,
  tiOPFManager,
  tiPersistenceLayers,
  tiWebServerVersion,
  tiConstants,
  tiDBProxyServerStats,
  tiCompress,
  tiStreams,
  tiQueryDummy,
  tiMime,
  RegularExpressions,
  tiExcept;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiApplicationServerTestCase);
end;

const
  cPort = 81;

{ TTestTIWebServer }

procedure TtiApplicationServerTestCase.SetUpOnce;
var
  LSetup: TtiOPFTestSetupData;
begin
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(TtiPersistenceLayerDummy);
  inherited;
  LSetup := GTIOPFTestManager.FindByPersistenceLayerName(GTIOPFManager.DefaultPersistenceLayerName);
  DefaultPersistenceLayerWasEmpty := True;
  if LSetup <> nil then
  begin
    OrigDefaultPersistenceLayer     := GTIOPFManager.DefaultPersistenceLayerName;
    DefaultPersistenceLayerWasEmpty := False;
  end;
  GTIOPFManager.DefaultPersistenceLayerName := PersistenceLayerName;
  GTIOPFManager.LoadPersistenceLayer(GTIOPFManager.DefaultPersistenceLayerName);
  DummySetup := TtiOPFTestSetupData.Create(GTIOPFManager.DefaultPerLayer);
  GTIOPFTestManager.Add(DummySetup);
  LSetup     := GTIOPFTestManager.FindByPersistenceLayerName(GTIOPFManager.DefaultPersistenceLayerName);

  GTIOPFManager.DefaultPerLayer.DBConnectionPools.AddInstance(
    LSetup.PersistenceLayerName,
    LSetup.DBName,
    LSetup.Username,
    LSetup.Password,
    '');
end;

procedure TtiApplicationServerTestCase.TearDownOnce;
begin
  GTIOPFManager.DefaultPerLayer.DBConnectionPools.Disconnect(PersistenceLayerName);
  GTIOPFManager.UnLoadPersistenceLayer(TtiPersistenceLayerDummy.CTIPersistDummy);
  GTIOPFTestManager.Remove(DummySetup);
  if not DefaultPersistenceLayerWasEmpty then
    GTIOPFManager.DefaultPersistenceLayerName := OrigDefaultPersistenceLayer
  else
    GTIOPFManager.DefaultPersistenceLayerName := '';
  inherited;
  GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(TtiPersistenceLayerDummy.CTIPersistDummy);
end;

class function TtiApplicationServerTestCase.PersistenceLayerName: string;
begin
  Result := TtiPersistenceLayerDummy.CTIPersistDummy;
end;

function TtiApplicationServerTestCase.StringToCompressedMIMEEncoded(AValue: string): string;
var
  LCompress: TtiCompressAbs;
  LCompStream, LUnCompStream, LCompMimeEncStream: TStream;
begin
  LCompStream        := nil;
  LUnCompStream      := nil;
  LCompMimeEncStream := nil;
  LCompress          := nil;
  try
    LCompStream := TMemoryStream.Create;
    LUnCompStream := TMemoryStream.Create;
    LCompMimeEncStream := TMemoryStream.Create;
    LCompress          := gCompressFactory.CreateInstance(cgsCompressZLib);
    tiStringToStream(AValue, LUnCompStream);
    LCompress.CompressStream(LUnCompStream, LCompStream);
    MimeEncodeStream(LCompStream, LCompMimeEncStream);

    Result := tiStreamToString(LCompMimeEncStream);
  finally
    LCompress.Free;
    LCompMimeEncStream.Free;
    LUnCompStream.Free;
    LCompStream.Free;
  end;
end;

function TtiApplicationServerTestCase.CompressedMIMEEncodedtoString(AValue: string): string;
var
  LCompress: TtiCompressAbs;
  LCompStream, LUnCompStream, LCompMimeEncStream: TStream;
begin
  LCompStream        := nil;
  LUnCompStream      := nil;
  LCompMimeEncStream := nil;
  LCompress          := nil;
  try
    LCompStream := TMemoryStream.Create;
    LUnCompStream := TMemoryStream.Create;
    LCompMimeEncStream := TMemoryStream.Create;
    LCompress          := gCompressFactory.CreateInstance(cgsCompressZLib);
    tiStringToStream(AValue, LCompMimeEncStream);
    MimeDecodeStream(LCompMimeEncStream, LCompStream);
    LCompress.DecompressStream(LCompStream, LUnCompStream);

    Result := tiStreamToString(LUnCompStream);
  finally
    LCompress.Free;
    LCompMimeEncStream.Free;
    LUnCompStream.Free;
    LCompStream.Free;
  end;
end;

procedure TtiApplicationServerTestCase.SetDummyDBIsConnected(AValue: boolean);
var
  LDatabase: TtiDatabaseDummy;
  lDBConnectionName: string;
begin
  lDBConnectionName := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionName;
  LDatabase         := TtiDatabaseDummy(GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName));
  LDatabase.setConnected(AValue);
  GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, LDatabase);
end;


function TtiApplicationServerTestCase.RemoveAllMatches(const AInputString: string;
  const ARegex: string; const AReplacement: string): string;
var
  LMatcher: TRegEx;
begin
  Result   := AInputString;
  LMatcher := TRegEx.Create(ARegex, [roMultiLine]);
  while LMatcher.IsMatch(Result) do
    Result := LMatcher.Replace(Result, AReplacement);
end;


procedure TtiApplicationServerTestCase.PrepareExecute(AAction: TtiWebServerAction; AResult: TStream);
begin
  PrepareExecute('none', AAction, AResult);
end;

procedure TtiApplicationServerTestCase.PrepareExecute
  (ARequestParams: string; AAction: TtiWebServerAction; AResult: TStream);
var
  LContentType: string;
  LResponseCode: integer;
begin
  try
    AAction.Execute('document in',
      nil,
      ARequestParams,
      AResult,
      LContentType,
      LResponseCode,
      nil);
  except
    raise;
  end;
end;


procedure TtiApplicationServerTestCase.tiApplicationServer_Create;
var
  LO: TtiDBProxyServer;
begin
  LO := TtiDBProxyServer.Create(cPort);
  try
    Check(True);
    Sleep(1000);
  finally
    LO.Free;
  end;
end;


procedure TtiApplicationServerTestCase.tiApplicationServer_ExecuteRemoteXML;
var
  LO: TtiWebServer;
  LExecuteRemoteXML: TtiDBPS_ExecuteRemoteXML;
  LResultStream: TStream;
  LStrippedResult: string;
const
  CTestString   = '<?xml version="1.0"?><a><tiopf version="' + cTIOPFXMLVersion
    + '"/><b><c d="m"><e><f g="n" h="string" i="9999"/><f g="o" h="string" i="10"/><f g="p" '
    + 'h="string" i="9999"/><f g="r" h="string" i="9999"/><f g="q" h="string" i="9999"/></e><j><k A=""  B=""  C="START_TRANSACTION" '
    + 'D="aaa"  E="aaaa"/></j></c><c d="s"><e><f g="u" h="string" i="9999"/><f g="v" h="string" i="9999"/><f g="w" h="string" '
    + 'i="9999"/><f g="x" h="string" i="9999"/></e><j></j></c></b></a>';
  CTestResult   = '<?xml version="1.0"?><a><tiopf version="' + cTIOPFXMLVersion
    + '"/><b><c d="y"><e><f g="z" h="string" i="9999"/><f g="o" h="string" i="9999"/><f g="aa" h="integer" i="0"/></e><j><k A="" B="" C="0"/></j></c></b></a>';
  CTranIDRemove = '(.*)B="\d+"(.*)';
begin
  LO := nil;
  LExecuteRemoteXML := nil;
  LResultStream := nil;
  try
    LO := TtiWebServer.Create(cPort);
    LExecuteRemoteXML := TtiDBPS_ExecuteRemoteXML.Create(LO, 10);
    LResultStream := TMemoryStream.Create;
    Check(LExecuteRemoteXML.CanExecute(tiXML.cgTIDBProxy));
    Check(not LExecuteRemoteXML.CanExecute(tiXML.cgTIDBProxy + 'b'));
    Check(not LExecuteRemoteXML.CanExecute('c' + tiXML.cgTIDBProxy));

    PrepareExecute(StringToCompressedMIMEEncoded(CTestString), LExecuteRemoteXML, LResultStream);
    LStrippedResult := RemoveAllMatches(CompressedMIMEEncodedtoString(tiStreamToString(LResultStream)),
      CTranIDRemove,
      '\1B=""\2');
    CheckEquals(LStrippedResult, CTestResult);
  finally
    LO.Free;
    LExecuteRemoteXML.Free;
    LResultStream.Free;
  end;
end;

procedure TtiApplicationServerTestCase.tiApplicationServer_ServerVersion;
var
  LO: TtiDBProxyServer;
  LServerVersion: TtiDBPS_ServerVersion;
  LResultStream : TStream;
begin
  LO := nil;
  LServerVersion := nil;
  LResultStream := nil;
  try
    LResultStream := TStringStream.Create;
    LO := TtiDBProxyServer.Create(cPort);
    LServerVersion := TtiDBPS_ServerVersion.Create(LO, 10);
    Check(LServerVersion.CanExecute(tiXML.cgTIDBProxyServerVersion));
    CheckFalse(LServerVersion.CanExecute(tiXML.cgTIDBProxyServerVersion + ' '));
    CheckFalse(LServerVersion.CanExecute('abcd' + tiXML.cgTIDBProxyServerVersion));
    PrepareExecute(LServerVersion,LResultStream);
    CheckEquals(LO.XMLTags.XMLVersion, tiStreamToString(LResultStream));
  finally
    LResultStream.Free;
    LO.Free;
    LServerVersion.Free;
  end;
end;


procedure TtiApplicationServerTestCase.tiApplicationServer_TestAlive;
var
  LO: TtiWebServer;
  LTestAlive: TtiDBPS_TestAlive;
  LResultStream : TStream;
begin
  LO         := nil;
  LTestAlive := nil;
  LResultStream := nil;
  try
    LResultStream := TStringStream.Create;
    LO := TtiWebServer.Create(cPort);
    LTestAlive := TtiDBPS_TestAlive.Create(LO, 10);
    Check(LTestAlive.CanExecute(tiXML.cgTIDBProxyTestAlive));
    CheckFalse(LTestAlive.CanExecute(tiXML.cgTIDBProxyTestAlive + ' '));
    CheckFalse(LTestAlive.CanExecute(' ' + tiXML.cgTIDBProxyTestAlive));

    PrepareExecute(LTestAlive,LResultStream);
    CheckEquals(tiStreamToString(LResultStream), LTestAlive.cPassed);

    SetDummyDBIsConnected(False);
    PrepareExecute(LTestAlive,LResultStream);
    CheckEquals(tiStreamToString(LResultStream), LTestAlive.cFailed);
    SetDummyDBIsConnected(True);
  finally
    LResultStream.Free;
    LO.Free;
    LTestAlive.Free;
  end;
end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestAlive1;
var
  LO: TtiWebServer;
  LTestAlive1: TtiDBPS_TestAlive1;
  LAppServerVersion: TtiAppServerVersionAbs;
  LResultStream : TStream;
begin
  LO          := nil;
  LTestAlive1 := nil;
  LAppServerVersion := nil;
  LResultStream := nil;
  try
    LResultStream := TStringStream.Create;
    LO := TtiWebServer.Create(cPort);
    LTestAlive1 := TtiDBPS_TestAlive1.Create(LO, 10);
    LAppServerVersion := gAppServerVersionFactory.CreateInstance;

    Check(LTestAlive1.CanExecute(tiXML.cgTIDBProxyTestAlive1));
    CheckFalse(LTestAlive1.CanExecute(tiXML.cgTIDBProxyTestAlive1 + #10));
    CheckFalse(LTestAlive1.CanExecute(#13 + tiXML.cgTIDBProxyTestAlive1));

    LAppServerVersion.LoadDefaultValues;
    LAppServerVersion.SetConnectionStatus(True);
    PrepareExecute(LTestAlive1, LResultStream);
    CheckEquals(tiStreamToString(LResultStream), LAppServerVersion.AsString);

    SetDummyDBIsConnected(False);
    LAppServerVersion.SetConnectionStatus(False);
    PrepareExecute(LTestAlive1, LResultStream);
    CheckEquals(tiStreamToString(LResultStream), LAppServerVersion.AsString);
    SetDummyDBIsConnected(True);

  finally
    LResultStream.Free;
    LO.Free;
    LTestAlive1.Free;
    LAppServerVersion.Free;
  end;
end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestHTML;
var
  LO: TtiWebServer;
  LTestHTML: TtiDBPS_TestHTML;
  LStrippedResult, LStrippedExpected: string;
  lDBProxyServerStats: TtiDBProxyServerStats;
  LResultStream : TStream;
const
  CDateReplace   = '(.*)\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}:\d{3}(.*)';
  CUpTimeReplace = '(.*)\d days .* hours(.*)';
begin
  LO        := nil;
  LTestHtml := nil;
  lDBProxyServerStats := nil;
  LResultStream := nil;
  try
    LResultStream := TStringStream.Create;
    LO := TtiWebServer.Create(cPort);
    LTestHTML := TtiDBPS_TestHTML.Create(LO, 10);

    Check(LTestHTML.CanExecute(tiXML.CTIViewAppServerStatus));
    CheckFalse(LTestHTML.CanExecute(tiXML.CTIViewAppServerStatus + ' '));
    CheckFalse(LTestHTML.CanExecute('XYZ' + tiXML.CTIViewAppServerStatus));
    //Remove date/time
    PrepareExecute(LTestHTML,LResultStream);
    LStrippedResult := RemoveAllMatches(tiStreamToString(LResultStream),
      CDateReplace,
      '\1\2');
    //Remove uptime
    LStrippedResult := RemoveAllMatches(LStrippedResult,
      CUpTimeReplace,
      '\1\2');

    lDBProxyServerStats := TtiDBProxyServerStats.Create;

    lDBProxyServerStats.TestRefreshRate := 1;
    lDBProxyServerStats.Execute;
    //Remove date/time
    LStrippedExpected := RemoveAllMatches(lDBProxyServerStats.AsHTML,
      CDateReplace,
      '\1\2');
    //Remove uptime
    LStrippedExpected := RemoveAllMatches(LStrippedExpected,
      CUpTimeReplace,
      '\1\2');
    CheckEquals(LStrippedResult, LStrippedExpected);
  finally
    LResultStream.Free;
    lDBProxyServerStats.Free;
    LO.Free;
    LTestHTML.Free;
  end;
end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestXML;
var
  LO: TtiWebServer;
  LTestXML: TtiDBPS_TestXML;
  LStrippedResult, LStrippedExpected: string;
  lDBProxyServerStats: TtiDBProxyServerStats;
  LResultStream: TStream;
const
  CDateReplace   = '(.*)\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}:\d{3}(.*)';
  CUpTimeReplace = '(.*)\d days .* hours(.*)';
begin
  LO       := nil;
  LTestXML := nil;
  lDBProxyServerStats := nil;
  LResultStream := nil;
  try
    LResultStream := TStringStream.Create;
    LO := TtiWebServer.Create(cPort);
    LTestXML := TtiDBPS_TestXML.Create(LO, 10);

    Check(LTestXML.CanExecute(tiXML.cgTIDBProxyTestXML));
    CheckFalse(LTestXML.CanExecute(tiXML.cgTIDBProxyTestXML + ' '));
    CheckFalse(LTestXML.CanExecute('XYZ' + tiXML.cgTIDBProxyTestXML));

    PrepareExecute(LTestXML,LResultStream);
    //Remove date/time
    LStrippedResult := RemoveAllMatches(tiStreamToString(LResultStream),
      CDateReplace,
      '\1\2');
    //Remove uptime
    LStrippedResult := RemoveAllMatches(LStrippedResult,
      CUpTimeReplace,
      '\1\2');

    lDBProxyServerStats := TtiDBProxyServerStats.Create;

    lDBProxyServerStats.TestRefreshRate := 1;
    lDBProxyServerStats.Execute;
    //Remove date/time
    LStrippedExpected := RemoveAllMatches(lDBProxyServerStats.AsXML,
      CDateReplace,
      '\1\2');
    //Remove uptime
    LStrippedExpected := RemoveAllMatches(LStrippedExpected,
      CUpTimeReplace,
      '\1\2');
    CheckEquals(LStrippedResult, LStrippedExpected);

  finally
    LResultStream.Free;
    lDBProxyServerStats.Free;
    LO.Free;
    LTestXML.Free;
  end;
end;

procedure TtiApplicationServerTestCase.tiApplicationServer_ForceException;
var
  LForceException: TtiDBPS_ForceException;
begin
  LForceException := TtiDBPS_ForceException.Create(nil, 10);
  try
    Check(LForceException.CanExecute(tiXML.cgTIDBProxyServerException));
    CheckFalse(LForceException.CanExecute(tiXML.cgTIDBProxyServerException + ' '));
    CheckFalse(LForceException.CanExecute('aaa' + tiXML.cgTIDBProxyServerException));
    try
      PrepareExecute(LForceException,nil);
      Check(False,'An exception wasn''t raised when it should have been');
    except on e:ETIOPFException do
      //tiOPF Exception must be raised to pass, allow DUnit exception to fail
      Check(True);
    end;
  finally
    LForceException.Free;
  end;
end;

end.

