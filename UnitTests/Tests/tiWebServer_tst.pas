unit tiWebServer_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
 ;

type

  TTestTIWebServerClientConnectionDetails = class(TtiTestCase)
  published
    procedure Equals;
    procedure Assign;
  end;

  TTestTIWebServer = class(TtiTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function  TestHTTPRequest(const ADocument: string; AFormatException: boolean = True): string;
    function  TestHTTPRequestInBlocks(const ADocument: string;
                                      var   ABlockIndex, ABlockCount, ABlockSize, ATransID: Longword): string;
  published
    procedure tiBlockStreamCache_AddRead;
    procedure tiBlockStreamCache_SweepForTimeOuts;

    procedure tiWebServer_Create;
    procedure tiWebServer_Ignore;
    procedure tiWebServer_Default;
    procedure tiWebServer_CanNotFindPage;
    procedure tiWebServer_CanFindPage;
    procedure tiWebServer_GetLogFile;
    procedure tiWebServer_RunCGIExtension;
    procedure tiWebServer_PageInBlocks;

// Add an IfDef so these are only tested if the remote persistence layer is tested
    procedure tiDBProxyServer_Create;
    procedure tiDBProxyServer_ServerVersion;
    procedure tiDBProxyServer_ExecuteRemoteXML;
    procedure tiDBProxyServer_TestAlive1;
    procedure tiDBProxyServer_TestHTML;
    procedure tiDBProxyServer_TestXML;
    procedure tiDBProxyServer_TestAlive;

    procedure tiWebServerVersion;

  end;

  TTestTICGIParams = class(TtiTestCase)
  published
    procedure Values;
    procedure Assign;
  end;

procedure RegisterTests;

implementation
uses
   tiUtils

  ,tiTestDependencies
  ,tiWebServerConfig
  ,tiWebServer

// Add an IfDef so these are only linked if the Remote persistence layer is tested
//  ,tiDBProxyServer
//  ,tiDBProxyServerStats
//  ,tiDBProxyServerDependencies

  ,tiWebServerClientConnectionDetails
  ,tiWebServerConstants
  ,tiWebServerVersion
  ,tiHTTPIndy
  ,tiLog
  ,tiHTTP
  ,tiCGIParams

  ,SysUtils
  ,Classes
  ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIWebServerClientConnectionDetails);
  RegisterNonPersistentTest(TTestTIWebServer);
  RegisterNonPersistentTest(TtestTICGIParams);
end;

const
  cPort= 81;

{ TTestTIWebServer }

procedure TTestTIWebServer.tiDBProxyServer_Create;
//var
//  LO: TtiDBProxyServer;
begin
//  LO:= TtiDBProxyServer.Create(cPort);
//  try
//
//  finally
//    LO.Free;
//  end;
end;

procedure TTestTIWebServer.SetUp;
begin
  inherited;

end;

procedure TTestTIWebServer.TearDown;
begin
  inherited;

end;

type
  TtiWebServerForTesting = class(TtiWebServer)
  public
    procedure SetStaticPageLocation(const AValue: string); override;
    procedure SetCGIBinLocation(const AValue: string); override;
    property  BlockStreamCache;
  end;

procedure TTestTIWebServer.tiWebServer_Create;
var
  LConfig: TtiWebServerConfig;
  LO: TtiWebServerForTesting;
  LExpectedStaticPageDir: string;
  LExpectedCGIBinDir: string;
begin
  LConfig:= nil;
  LO:= nil;
  try
    LConfig:= TtiWebServerConfig.Create;
    LO:= TtiWebServerForTesting.Create(cPort);
    LO.BlockStreamCache.SleepSec:= 0;

    LExpectedStaticPageDir:= tiAddTrailingSlash(LConfig.PathToStaticPages);
    LExpectedCGIBinDir:= tiAddTrailingSlash(LConfig.PathToCGIBin);

    LO.Start;
    CheckEquals(LExpectedStaticPageDir, LO.StaticPageLocation);
    Check(DirectoryExists(LO.StaticPageLocation));
    CheckEquals(LExpectedCGIBinDir, LO.CGIBinLocation);
    LO.Stop;
  finally
    LO.Free;
    LConfig.Free;
  end;
end;

procedure TTestTIWebServer.tiDBProxyServer_ExecuteRemoteXML;
begin

end;

procedure TTestTIWebServer.tiDBProxyServer_TestAlive;
begin

end;

procedure TTestTIWebServer.tiDBProxyServer_TestAlive1;
begin

end;

procedure TTestTIWebServer.tiDBProxyServer_TestHTML;
begin

end;

procedure TTestTIWebServer.tiDBProxyServer_TestXML;
begin

end;

procedure TTestTIWebServer.tiWebServerVersion;
var
  L: TtiAppServerVersion;
  LS: string;
begin

  L:= TtiAppServerVersion.Create;
  try
    CheckEquals(cWebServerStatus_unknown, L.ConnectionStatus);
    CheckEquals('', L.XMLVersion);
    CheckEquals('', L.FileSyncVersion);

    L.LoadDefaultValues;
    CheckEquals(cWebServerStatus_unknown, L.ConnectionStatus);
    CheckEquals(cXMLVersion,              L.XMLVersion);
    CheckEquals(cFileSyncVersion,         L.FileSyncVersion);

    L.SetConnectionStatus(True);
    CheckEquals(cWebServerStatus_passed, L.ConnectionStatus);

    L.SetConnectionStatus(False);
    CheckEquals(cWebServerStatus_failed, L.ConnectionStatus);

  finally
    L.free;
  end;

  L:= TtiAppServerVersion.Create;
  try
    L.ConnectionStatus:= 'test1';
    L.XMLVersion:= 'test2';
    L.FileSyncVersion:= 'test3';
    LS:= L.AsString;
  finally
    L.free;
  end;

  L:= TtiAppServerVersion.Create;
  try
    L.AsString:= LS;
    CheckEquals('test1', L.ConnectionStatus);
    CheckEquals('test2', L.XMLVersion);
    CheckEquals('test3', L.FileSyncVersion);
  finally
    L.free;
  end;

end;
procedure TTestTIWebServer.tiWebServer_CanFindPage;
var
  LO: TtiWebServerForTesting;
  LResult: string;
  LFileName: string;
  LPage: string;
  LDir: string;
begin
  LPage:= '<html>test page</html>';
  LFileName:= TempFileName('testpage.htm');
  LDir:= ExtractFilePath(LFileName);
  tiForceDirectories(LDir);
  tiStringToFile(LPage, LFileName);

  LO:= TtiWebServerForTesting.Create(cPort);
  try
    // ToDo: This is too fragile. SleepSec must be set before the web server is
    //       started but StaticPageLocation must be set after web server is started
    LO.BlockStreamCache.SleepSec:= 0;
    LO.Start;
    LO.SetStaticPageLocation(LDir);

    LResult:= TestHTTPRequest('testpage.htm');
    CheckEquals(LPage, LResult);

    LResult:= TestHTTPRequest('testpage');
    CheckEquals(LPage, LResult);

    tiDeleteFile(LFileName);
    LFileName:= tiSwapExt(LFileName, 'html');
    tiStringToFile(LPage, LFileName);

    LResult:= TestHTTPRequest('testpage');
    CheckEquals(LPage, LResult);

  finally
    LO.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_CanNotFindPage;
var
  LO: TtiWebServer;
  LResult: string;
begin
  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    try
      LResult:= TestHTTPRequest('pagethatsnotthere.htm', False);
      fail('Exception not raised');
    except
      on e:exception do
      begin
        CheckIs(e, Exception);
        CheckEquals('HTTP/1.1 404 Not Found', e.message);
      end;
    end;
    CheckEquals('', LResult);

    try
      LResult:= TestHTTPRequest('pagethatsnotthere.htm', True);
      fail('Exception not raised');
    except
      on e:exception do
      begin
        CheckIs(e, Exception);
        CheckEquals(
          Format(cErrorAccessingHTTPServer,
                 ['HTTP/1.1 404 Not Found',
                  'Post',
                  'http://localhost:81/pagethatsnotthere.htm',
                  '']),
        e.message);
      end;
    end;
    CheckEquals('', LResult);
  finally
    LO.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_Default;
var
  LO: TtiWebServer;
  LResult: string;
begin
  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    LResult:= TestHTTPRequest('');
    CheckEquals(cDefaultPageText, LResult);
  finally
    LO.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_GetLogFile;
  function _WaitForLogFile(const AFileName: string): boolean;
  var
    LStream: TFileStream;
  begin
    if not FileExists(AFileName) then
      result:= True
    else
      try
        LStream:= TFileStream.Create(AFileName, fmShareExclusive or fmOpenRead);
        LStream.Free;
        result:= true;
      except
        on e: exception do
          result:= false;
      end;
  end;
var
  LO: TtiWebServerForTesting;
  LResult: string;
  LPage: string;
  LFileName: string;
  LTryCount: integer;
begin
  LFileName:= gLog.LogToFileName;
  LPage:= 'test log file';
  LO:= TtiWebServerForTesting.Create(cPort);
  try
    LO.BlockStreamCache.SleepSec:= 0;
    LO.Start;
    LTryCount:= 0 ;
    while not _WaitForLogFile(LFileName) and (LTryCount <= 10) do
      Sleep(100);
    tiDeleteFile(LFileName);
    tiStringToFile(LPage, LFileName);
    LResult:= TestHTTPRequest(cgTIDBProxyGetLog);
    CheckEquals('<HTML><PRE>'+LPage+'</PRE></HTML>', LResult);
  finally
    LO.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_Ignore;
var
  LO: TtiWebServer;
  LResult: string;
begin
  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    LResult:= TestHTTPRequest(cDocumentToIgnore);
    CheckEquals('', LResult);
  finally
    LO.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_RunCGIExtension;
begin

end;

procedure TTestTIWebServer.tiDBProxyServer_ServerVersion;
begin

end;

function TTestTIWebServer.TestHTTPRequest(const ADocument: string; AFormatException: boolean = True): string;
var
  LHTTP: TtiHTTPIndy;
begin
  LHTTP:= TtiHTTPIndy.Create;
  try
    LHTTP.FormatExceptions:= AFormatException;
    LHTTP.Post('http://localhost:' + IntToStr(cPort) + '/' + ADocument);
    Result:= LHTTP.Output.DataString;
  finally
    LHTTP.Free;
  end;
end;

procedure TTestTIWebServer.tiWebServer_PageInBlocks;
var
  LO: TtiWebServerForTesting;
  LResult: string;
  LFileName: string;
  LPage: string;
  LBlockCount, LBlockIndex, LBlockSize, LTransID: Longword;
  LDir: string;
begin
  LFileName:= TempFileName('testpage.htm');
  LDir:= ExtractFilePath(LFileName);
  tiForceDirectories(LDir);
  LPage:= 'abcDEFghiJKLmn';
  tiStringToFile(LPage, LFileName);

  LO:= TtiWebServerForTesting.Create(cPort);
  try
    LO.BlockStreamCache.SleepSec:= 0;
    LO.Start;
    LO.SetStaticPageLocation(LDir);

    LBlockCount:= 0;
    LBlockIndex:= 0;
    LBlockSize:=  3;
    LTransID:=    0;

    LResult:= TestHTTPRequestInBlocks('testpage.htm', LBlockIndex, LBlockCount, LBlockSize, LTransID);
    CheckEquals(5, LBlockCount, 'BlockCount #1');
    CheckEquals(4, LBlockIndex, 'BlockIndex #1');
    CheckEquals(3, LBlockSize,  'BlockSize #1');
    CheckEquals(1, LTransID,    'TransID #1');

  finally
    LO.Free;
  end;
end;

function TTestTIWebServer.TestHTTPRequestInBlocks(const ADocument: string;
  var ABlockIndex, ABlockCount, ABlockSize, ATransID: Longword): string;
var
  LHTTP: TtiHTTPIndy;
  LHeader: string;
begin
  LHTTP:= TtiHTTPIndy.Create;
  try
    tiHTTP.gTIOPFHTTPDefaultBlockSize:= ABlockSize;
    LHTTP.Post('http://localhost:' + IntToStr(cPort) + '/' + ADocument);
    Result:= LHTTP.Output.DataString;
    LHeader:= LHTTP.ResponseTIOPFBlockHeader;
    tiHTTP.tiParseTIOPFHTTPBlockHeader(LHeader, ABlockIndex, ABlockCount, ABlockSize, ATransID);
  finally
    LHTTP.Free;
  end;
end;

type
  TtiBlockStreamCacheForTesting = class(TtiBlockStreamCache)
  public
    property Count;
  end;

procedure TTestTIWebServer.tiBlockStreamCache_AddRead;
var
  L: TtiBlockStreamCacheForTesting;
  LBlockText: string;
  LBlockCount: Longword;
  LTransID: Longword;
begin
  L:= TtiBlockStreamCacheForTesting.Create;
  try
    L.AddBlockStream('abcDEFgh', 3, LBlockText, LBlockCount, LTransID);
    CheckEquals(1, L.Count);
    CheckEquals('abc', LBlockText);
    CheckEquals(3, LBlockCount);
    CheckEquals(1, LTransID);

    L.AddBlockStream('jklMNOpq', 3, LBlockText, LBlockCount, LTransID);
    CheckEquals(2, L.Count);
    CheckEquals('jkl', LBlockText);
    CheckEquals(3, LBlockCount);
    CheckEquals(2, LTransID);

    L.ReadBlock(2, 0, LBlockText);
    CheckEquals('jkl', LBlockText);
    L.ReadBlock(2, 1, LBlockText);
    CheckEquals('MNO', LBlockText);
    L.ReadBlock(2, 2, LBlockText);
    CheckEquals('pq', LBlockText);
    CheckEquals(1, L.Count);

    L.ReadBlock(1, 0, LBlockText);
    CheckEquals('abc', LBlockText);
    L.ReadBlock(1, 1, LBlockText);
    CheckEquals('DEF', LBlockText);
    L.ReadBlock(1, 2, LBlockText);
    CheckEquals('gh', LBlockText);
    CheckEquals(0, L.Count);

  finally
    L.Free;
  end;
end;

procedure TTestTIWebServer.tiBlockStreamCache_SweepForTimeOuts;
var
  L: TtiBlockStreamCacheForTesting;
  LBlockText: string;
  LBlockCount: Longword;
  LTransID: Longword;
begin
  L:= TtiBlockStreamCacheForTesting.Create;
  try
    L.SleepSec:= 1;
    L.SweepEverySec:= 1;
    L.Start;
    L.AddBlockStream('abcDEFgh', 3, LBlockText, LBlockCount, LTransID);
    L.AddBlockStream('jklMNOpq', 3, LBlockText, LBlockCount, LTransID);
    Sleep(2000);
    CheckEquals(2, L.Count);
    L.TimeOutSec:= 1;
    Sleep(2000);
    CheckEquals(0, L.Count);
    
  finally
    L.Free;
  end;
end;

{ TTestTIWebServerConnectionDetails }

procedure TTestTIWebServerClientConnectionDetails.Assign;
var
  LA: TtiWebServerClientConnectionDetails;
  LB: TtiWebServerClientConnectionDetails;
const
  CAppServerURL= '1';
  CConnectWith= '2';
  CProxyServerActive= True;
  CProxyServerName= '3';
  CProxyServerPort= 4;

begin
  LA:= nil;
  LB:= nil;
  try
    LA:= TtiWebServerClientConnectionDetails.Create;
    LB:= TtiWebServerClientConnectionDetails.Create;

    LA.AppServerURL:= CAppServerURL;
    LA.ConnectWith:= CConnectWith;
    LA.ProxyServerActive:= CProxyServerActive;
    LA.ProxyServerName:= CProxyServerName;
    LA.ProxyServerPort:= CProxyServerPort;

    LB.Assign(LA);

    CheckEquals(CAppServerURL, LA.AppServerURL);
    CheckEquals(CConnectWith, LA.ConnectWith);
    CheckEquals(CProxyServerActive, LA.ProxyServerActive);
    CheckEquals(CProxyServerName, LA.ProxyServerName);
    CheckEquals(CProxyServerPort, LA.ProxyServerPort);

  finally
    LA.Free;
    LB.Free;
  end;
end;

procedure TTestTIWebServerClientConnectionDetails.Equals;
var
  LA: TtiWebServerClientConnectionDetails;
  LB: TtiWebServerClientConnectionDetails;
const
  CAppServerURL= '1';
  CConnectWith= '2';
  CProxyServerActive= True;
  CProxyServerName= '3';
  CProxyServerPort= 4;

begin
  LA:= nil;
  LB:= nil;
  try
    LA:= TtiWebServerClientConnectionDetails.Create;
    LB:= TtiWebServerClientConnectionDetails.Create;

    LA.AppServerURL:= CAppServerURL;
    LA.ConnectWith:= CConnectWith;
    LA.ProxyServerActive:= CProxyServerActive;
    LA.ProxyServerName:= CProxyServerName;
    LA.ProxyServerPort:= CProxyServerPort;

    LB.AppServerURL:= CAppServerURL;
    LB.ConnectWith:= CConnectWith;
    LB.ProxyServerActive:= CProxyServerActive;
    LB.ProxyServerName:= CProxyServerName;
    LB.ProxyServerPort:= CProxyServerPort;

    Check(LA.Equals(LB));
    LB.AppServerURL:= 'test';
    Check(not LA.Equals(LB));
    LB.AppServerURL:= CAppServerURL;

    Check(LA.Equals(LB));
    LB.ConnectWith:= 'test';
    Check(not LA.Equals(LB));
    LB.ConnectWith:= CConnectWith;

    Check(LA.Equals(LB));
    LB.ProxyServerActive:= not LB.ProxyServerActive;
    Check(not LA.Equals(LB));
    LB.ProxyServerActive:= CProxyServerActive;

    Check(LA.Equals(LB));
    LB.ProxyServerName:= 'test';
    Check(not LA.Equals(LB));
    LB.ProxyServerName:= CProxyServerName;

    Check(LA.Equals(LB));
    LB.ProxyServerPort:= LB.ProxyServerPort+1;
    Check(not LA.Equals(LB));
    LB.ProxyServerPort:= CProxyServerPort;

  finally
    LA.Free;
    LB.Free;
  end;
end;

{ TTestTICGIParams }

const
  CParam1 = 'param1';
  CParam2 = 'param2';
  CValue1 = 'value1';
  CValue2 = 'value2';

procedure TTestTICGIParams.Assign;
var
  LA: TtiCGIParams;
  LB: TtiCGIParams;
begin
  LA:= nil;
  LB:= nil;
  try
    LA:= TtiCGIParams.Create;
    LB:= TtiCGIParams.Create;
    LA.Values[CParam1]:= CValue1;
    LA.Values[CParam2]:= CValue2;
    LB.Assign(LA);
    CheckEquals(2, LB.Count);
    CheckEquals(CValue1, LB.Values[CParam1]);
    CheckEquals(CValue2, LB.Values[CParam2]);
  finally
    LA.Free;
    LB.Free;
  end;
end;

procedure TTestTICGIParams.Values;
var
  L: TtiCGIParams;
  LAsString: string;
  LCompressEncode: string;
begin

  L:= TtiCGIParams.Create;
  try
    L.Values[CParam1]:= CValue1;
    CheckEquals(1, L.Count);
    L.Values[CParam2]:= CValue2;
    CheckEquals(2, L.Count);
    CheckEquals(CValue1, L.Values[CParam1]);
    CheckEquals(CValue2, L.Values[CParam2]);

    LAsString:= L.AsString;
    LCompressEncode:= L.AsCompressedEncodedString;

  finally
    L.Free;
  end;

  L:= TtiCGIParams.Create;
  try
    L.AsString:= LAsString;
    CheckEquals(2, L.Count);
    CheckEquals(CValue1, L.Values[CParam1]);
    CheckEquals(CValue2, L.Values[CParam2]);
  finally
    L.Free;
  end;

  L:= TtiCGIParams.Create;
  try
    L.AsCompressedEncodedString:= LCompressEncode;
    CheckEquals(2, L.Count);
    CheckEquals(CValue1, L.Values[CParam1]);
    CheckEquals(CValue2, L.Values[CParam2]);
  finally
    L.Free;
  end;

end;

{ TtiWebServerForTesting }

procedure TtiWebServerForTesting.SetCGIBinLocation(const AValue: string);
begin
  inherited;
end;

procedure TtiWebServerForTesting.SetStaticPageLocation(const AValue: string);
begin
  inherited;
end;

end.
