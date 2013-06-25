unit tiWebServer_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
 ;

type

  TTestTIWebServerClientConnectionDetails = class(TtiTestCase)
  published
    procedure tiWebServerClientConnectionDetails_Equals;
    procedure Assign;
  end;

  TtiWebServerTestCase = class(TtiTestCase)
  private
    procedure DotiWebServer_Default(const APageName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function  TestHTTPRequest(const ADocument: string;
      const AFormatException: boolean = True;
      const AParams: string = ''): string;
    function  TestHTTPRequestInBlocks(
      const ADocument: string;
      const ABlockSize: Longword;
      const ABlockIndex: Longword;
      var   ABlockCount, ATransID, ABlockCRC: Longword): string;
    procedure TestRunCGIExtension(const AParam: string);
  published
    procedure tiBlockStreamCache_AddRead;
    procedure tiBlockStreamCache_SweepForTimeOuts;

    procedure tiWebServer_Create;
    procedure tiWebServer_CreateStartAndStop;
    procedure tiWebServer_Ignore;
    procedure tiWebServer_Default_NoPageAvailable;
    procedure tiWebServer_Default_DefaultHTMRootFolder;
    procedure tiWebServer_Default_DefaultHTMRootSubFolder;
    procedure tiWebServer_Default_DefaultHTMLRootFolder;
    procedure tiWebServer_Default_DefaultHTMLRootSubFolder;
    procedure tiWebServer_Default_IndexHTMRootFolder;
    procedure tiWebServer_Default_IndexHTMRootSubFolder;
    procedure tiWebServer_Default_IndexHTMLRootFolder;
    procedure tiWebServer_Default_IndexHTMLRootSubFolder;
    procedure tiWebServer_CanNotFindPage;
    procedure tiWebServer_CanFindPage;
    procedure tiWebServer_GetLogFile;
    procedure tiWebServer_TestWebServerCGIForTestingEXE;
    procedure tiWebServer_RunCGIExtensionSmallParameter;
    procedure tiWebServer_RunCGIExtensionLargeParameter;
    procedure tiWebServer_PageInBlocks;
    procedure tiWebServerVersion;

  end;

  TTestTICGIParams = class(TtiTestCase)
  published
    procedure Values;
    procedure Assign;
    procedure Clear;
  end;

procedure RegisterTests;

implementation
uses
   tiUtils

  ,tiTestDependencies
  ,tiWebServerConfig
  ,tiWebServer

  ,tiWebServerClientConnectionDetails
  ,tiWebServerConstants
  ,tiWebServerVersion
  ,tiHTTPIndy
  ,tiLog
  ,tiHTTP
  ,tiCGIParams
  ,tiStreams
  ,tiConsoleApp
  ,tiConstants
  ,tiCRC32
  ,tiXML

  ,SysUtils
  ,Classes
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIWebServerClientConnectionDetails);
  tiRegisterNonPersistentTest(TtiWebServerTestCase);
  tiRegisterNonPersistentTest(TtestTICGIParams);
end;

const
  cPort= 81;
  cExpectedResponseErrorTextCountAttempts = 'HTTP/1.1 404 Not Found (After 1 attempts)';

{ TTestTIWebServer }

procedure TtiWebServerTestCase.SetUp;
begin
  inherited;

end;

procedure TtiWebServerTestCase.TearDown;
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

  procedure TtiWebServerForTesting.SetCGIBinLocation(const AValue: string);
  begin
    inherited;
  end;

  procedure TtiWebServerForTesting.SetStaticPageLocation(const AValue: string);
  begin
    inherited;
  end;

procedure TtiWebServerTestCase.tiWebServer_Create;
var
  LO: TtiWebServerForTesting;
begin
  LO:= TtiWebServerForTesting.Create(cPort);
  try
    Check(True);
    Sleep(1000);
  finally
    LO.Free;
  end;
end;

procedure TtiWebServerTestCase.tiWebServer_CreateStartAndStop;
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

type
  _TtiAppServerVersionTest = class(TtiAppServerVersionAbs)
  protected
    function GetCurrentFileSyncVersion: string; override;
  end;

  function _TtiAppServerVersionTest.GetCurrentFileSyncVersion: string;
  begin
    result := 'abcd';
  end;

procedure TtiWebServerTestCase.tiWebServerVersion;
var
  L: TtiAppServerVersionAbs;
  LS: string;
begin

  L:= gAppServerVersionFactory.CreateInstance;
  try
    CheckEquals(cWebServerStatus_unknown, L.ConnectionStatus);
    CheckEquals('', L.XMLVersion);
    CheckEquals('', L.FileSyncVersion);

    L.LoadDefaultValues;
    CheckEquals(cWebServerStatus_unknown, L.ConnectionStatus);
    CheckEquals(ctiOPFXMLVersion,         L.XMLVersion);
    CheckEquals(ctiOPFOldFileSyncVersion, L.FileSyncVersion);

    L.SetConnectionStatus(True);
    CheckEquals(cWebServerStatus_passed, L.ConnectionStatus);

    L.SetConnectionStatus(False);
    CheckEquals(cWebServerStatus_failed, L.ConnectionStatus);
  finally
    L.free;
  end;

  gAppServerVersionFactory.RegisterClass(_TtiAppServerVersionTest);
  L:= gAppServerVersionFactory.CreateInstance;
  try
    CheckFalse(L.IsVersionValid, 'IsVersionValid before LoadDefaultValues');
    L.LoadDefaultValues;
    CheckEquals('abcd', L.FileSyncVersion);
    CheckTrue(L.IsVersionValid, 'IsVersionValid after LoadDefaultValues');
    L.FileSyncVersion := ctiOPFOldFileSyncVersion;
    CheckFalse(L.IsVersionValid, 'IsVersionValid with default FileSyncVersion');
  finally
    L.free;
  end;

  gAppServerVersionFactory.UnRegisterClass;
  L:= gAppServerVersionFactory.CreateInstance;
  try
    L.LoadDefaultValues;
    CheckEquals(ctiOPFOldFileSyncVersion, L.FileSyncVersion);
    CheckTrue(L.IsVersionValid, 'IsVersionValid after LoadDefaultValues');
    L.FileSyncVersion := 'abcd';
    CheckFalse(L.IsVersionValid, 'IsVersionValid with different FileSyncVersion');
  finally
    L.free;
  end;

  L:= gAppServerVersionFactory.CreateInstance;
  try
    L.ConnectionStatus:= 'test1';
    L.XMLVersion:= 'test2';
    L.FileSyncVersion:= 'test3';
    LS:= L.AsString;
  finally
    L.free;
  end;

  L:= gAppServerVersionFactory.CreateInstance;
  try
    L.AsString:= LS;
    CheckEquals('test1', L.ConnectionStatus);
    CheckEquals('test2', L.XMLVersion);
    CheckEquals('test3', L.FileSyncVersion);
  finally
    L.free;
  end;

end;
procedure TtiWebServerTestCase.tiWebServer_CanFindPage;
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

procedure TtiWebServerTestCase.tiWebServer_CanNotFindPage;
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
        CheckEquals(cExpectedResponseErrorTextCountAttempts, e.message);
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
          cExpectedResponseErrorTextCountAttempts,
          e.message);
      end;
    end;
    CheckEquals('', LResult);
  finally
    LO.Free;
  end;
end;

procedure TtiWebServerTestCase.DotiWebServer_Default(const APageName: string);
var
  LO: TtiWebServerForTesting;
  LResult: string;
  LFileName: string;
  LPage: string;
  LDir: string;
begin
  LPage:= '<html>test page</html>';
  LFileName:= TempFileName(APageName);
  LDir:= ExtractFilePath(LFileName);
  tiForceDirectories(LDir);
  try
    tiStringToFile(LPage, LFileName);
    LO:= TtiWebServerForTesting.Create(cPort);
    try
      // ToDo: This is too fragile. SleepSec must be set before the web server is
      //       started but StaticPageLocation must be set after web server is started
      LO.BlockStreamCache.SleepSec:= 0;
      LO.Start;
      LO.SetStaticPageLocation(TempDirectory);

      LResult:= TestHTTPRequest(ExtractFilePath(APageName));
      CheckEquals(LPage, LResult);

    finally
      LO.Free;
    end;
  finally
    tiForceRemoveDir(LDir);
  end ;
end;

procedure TtiWebServerTestCase.tiWebServer_Default_DefaultHTMRootFolder;
begin
  DotiWebServer_Default('default.htm');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_DefaultHTMRootSubFolder;
begin
  DotiWebServer_Default('subfolder\default.htm');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_DefaultHTMLRootFolder;
begin
  DotiWebServer_Default('default.html');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_DefaultHTMLRootSubFolder;
begin
  DotiWebServer_Default('subfolder\default.html');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_IndexHTMLRootFolder;
begin
  DotiWebServer_Default('index.html');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_IndexHTMLRootSubFolder;
begin
  DotiWebServer_Default('subfolder\index.html');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_IndexHTMRootFolder;
begin
  DotiWebServer_Default('index.htm');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_IndexHTMRootSubFolder;
begin
  DotiWebServer_Default('subfolder\index.htm');
end;

procedure TtiWebServerTestCase.tiWebServer_Default_NoPageAvailable;
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

procedure TtiWebServerTestCase.tiWebServer_GetLogFile;
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
  LSavedSevToLog: TtiSevToLog;
begin
  LSavedSevToLog:= GLog.SevToLog;
  try
    GLog.SevToLog:= [];
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
  finally
    GLog.SevToLog:= LSavedSevToLog;
  end;
end;

procedure TtiWebServerTestCase.tiWebServer_Ignore;
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

procedure TtiWebServerTestCase.tiWebServer_RunCGIExtensionLargeParameter;
var
  LWebServer: TtiWebServerForTesting;
  LHTTP: TtiHTTPIndy;
  LEncoded: string;
  LExpected: string;
  LActual: string;
  LSize: Cardinal;
begin
  LSize:= CMaximumCommandLineLength + 1;
  LExpected:= tiCreateStringOfSize(LSize);
  // The actual size of the string passed will be larger than
  // CMaximumCommandLineLength because MIME encoding inflates the string
  LEncoded:= MimeEncodeStringNoCRLF(LExpected);
  LWebServer:= nil;
  LHTTP:= nil;
  try
    LWebServer:= TtiWebServerForTesting.Create(cPort);
    LHTTP:= TtiHTTPIndy.Create;
    LWebServer.Start;
    LWebServer.SetCGIBinLocation(tiAddTrailingSlash(tiGetEXEPath) + 'CGI-Bin');
    LHTTP.FormatExceptions:= False;
    LHTTP.Input.WriteString(LEncoded);
    LHTTP.Post('http://localhost:' + IntToStr(cPort) + '/tiWebServerCGIForTesting.exe');
    LEncoded:= LHTTP.Output.DataString;
    LActual:= MimeDecodeString(LEncoded);
    Check(Trim(LExpected) = Trim(LActual));
  finally
    LHTTP.Free;
    LWebServer.Free;
  end;
end;

procedure TtiWebServerTestCase.tiWebServer_RunCGIExtensionSmallParameter;
begin
  TestRunCGIExtension('teststring');
end;

procedure TtiWebServerTestCase.tiWebServer_TestWebServerCGIForTestingEXE;
var
  LExpected: string;
  LActual: string;
  LPath: string;
  LEncode: string;
  LMaxCommandLineLength: Cardinal;
begin
  // tiExecConsoleApp will inject CrLf every 255 characters, so comparing strings
  // of less than this length will be OK, but longer strings will be mangled
  // so must be encoded first.
  LPath:= tiAddTrailingSlash(tiGetEXEPath) + 'CGI-Bin\tiWebServerCGIForTesting.exe';
  Check(FileExists(LPath), 'Can not find "%s"', [LPath]);

  // Test a short string
  LActual:= '';
  LExpected:= 'abcd';
  tiExecConsoleApp(LPath, LExpected, LActual, nil, False);
  CheckEquals(LExpected, LActual);

  // Test a long string
  LActual:= '';
  LExpected:= tiCreateStringOfSize(20*1024);
  tiExecConsoleApp(LPath, MimeEncodeStringNoCRLF(LExpected), LActual, nil, False);
  CheckEquals(Trim(LExpected), Trim(MimeDecodeString(LActual)));

  // Test a string string on the limit
  LMaxCommandLineLength:=
    CMaximumCommandLineLength - Length(LPath);
  LActual:= '';
  LEncode:= tiReplicate('X', LMaxCommandLineLength);
  tiExecConsoleApp(LPath, LEncode, LActual, nil, False);

  // Test a string string 1 byte above the limit
  try
    LActual:= '';
    LEncode:= tiReplicate('X', LMaxCommandLineLength + 1);
    tiExecConsoleApp(LPath, LEncode, LActual, nil, False);
    Fail('Exception not raised');
  except
    on e:exception do
      Check(Pos('Maximum command line length', e.message) <> 0);
  end;
end;

function TtiWebServerTestCase.TestHTTPRequest(const ADocument: string;
  const AFormatException: boolean = True;
  const AParams: string = ''): string;
var
  LHTTP: TtiHTTPIndy;
begin
  LHTTP:= TtiHTTPIndy.Create;
  try
    LHTTP.FormatExceptions:= AFormatException;
    LHTTP.Input.WriteString(AParams);
    LHTTP.Post('http://localhost:' + IntToStr(cPort) + '/' + ADocument);
    Result:= LHTTP.Output.DataString;
  finally
    LHTTP.Free;
  end;
end;

procedure TtiWebServerTestCase.tiWebServer_PageInBlocks;
var
  LO: TtiWebServerForTesting;
  LBlockContent: string;
  LFileName: string;
  LBlockCount, LTransID, LBlockCRC: Longword;
  LDir: string;
const
  CBlockSize = 3;
  CBlock0 = 'abc';
  CBlock1 = 'DEF';
  CBlock2 = 'ghi';
  CBlock3 = 'JKL';
  CBlock4 = 'mn';
  CPageContent = CBlock0 + CBlock1 + CBlock2 + CBlock3 + CBlock4;

begin

  LFileName:= TempFileName('testpage.htm');
  LDir:= ExtractFilePath(LFileName);
  tiForceDirectories(LDir);
  tiStringToFile(CPageContent, LFileName);

  LO:= TtiWebServerForTesting.Create(cPort);
  try
    LO.BlockStreamCache.SleepSec:= 0;
    LO.Start;
    LO.SetStaticPageLocation(LDir);

    LBlockCount:= 0;
    LTransID:=    0;

    CheckEquals(0, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');
    LBlockContent:= TestHTTPRequestInBlocks('testpage.htm', CBlockSize, 0, LBlockCount, LTransID, LBlockCRC);
    CheckEquals(CBlock0, LBlockContent, 'BlockContent #0');
    CheckEquals(5, LBlockCount, 'BlockCount #0');
    CheckEquals(1, LTransID,    'TransID #0');
    CheckEquals(tiCRC32FromString(CBlock0), LBlockCRC,   'BlockCRC #0');
    CheckEquals(1, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');

    LBlockContent:= TestHTTPRequestInBlocks('testpage.htm', CBlockSize, 1, LBlockCount, LTransID, LBlockCRC);
    CheckEquals(CBlock1, LBlockContent, 'BlockContent #1');
    CheckEquals(5, LBlockCount, 'BlockCount #1');
    CheckEquals(1, LTransID,    'TransID #1');
    CheckEquals(tiCRC32FromString(CBlock1), LBlockCRC,   'BlockCRC #1');
    CheckEquals(1, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');

    LBlockContent:= TestHTTPRequestInBlocks('testpage.htm', CBlockSize, 2, LBlockCount, LTransID, LBlockCRC);
    CheckEquals(CBlock2, LBlockContent, 'BlockContent #2');
    CheckEquals(5, LBlockCount, 'BlockCount #2');
    CheckEquals(1, LTransID,    'TransID #2');
    CheckEquals(tiCRC32FromString(CBlock2), LBlockCRC,   'BlockCRC #2');
    CheckEquals(1, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');

    LBlockContent:= TestHTTPRequestInBlocks('testpage.htm', CBlockSize, 3, LBlockCount, LTransID, LBlockCRC);
    CheckEquals(CBlock3, LBlockContent, 'BlockContent #3');
    CheckEquals(5, LBlockCount, 'BlockCount #3');
    CheckEquals(1, LTransID,    'TransID #3');
    CheckEquals(tiCRC32FromString(CBlock3), LBlockCRC,   'BlockCRC #3');
    CheckEquals(1, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');

    LBlockContent:= TestHTTPRequestInBlocks('testpage.htm', CBlockSize, 4, LBlockCount, LTransID, LBlockCRC);
    CheckEquals(CBlock4, LBlockContent, 'BlockContent #4');
    CheckEquals(5, LBlockCount, 'BlockCount #4');
    CheckEquals(1, LTransID,    'TransID #4');
    CheckEquals(tiCRC32FromString(CBlock4), LBlockCRC,   'BlockCRC #4');
    CheckEquals(1, LO.BlockStreamCache.Count, 'LO.BlockStreamCache.Count');

  finally
    LO.Free;
  end;
end;

type
  TtiHTTPIndyForTesting = class(TtiHTTPIndy)
  public
   procedure   DoGetOrPostBlockWithRetry(
      const AURL: string;
      const AGetOrPostMethod: TtiHTTPGetOrPostMethod;
      const AInput: TStringStream;
      const AOutput: TStringStream;
      const ABlockIndex: LongWord;
      var   ATransID: LongWord;
      out   ABlockCRC: LongWord;
      out   ABlockCount: LongWord); override;
    procedure   DoPost(const AURL : string; AInput, AOutput: TStringStream); override;
  end;

  procedure TtiHTTPIndyForTesting.DoGetOrPostBlockWithRetry(const AURL: string;
  const AGetOrPostMethod: TtiHTTPGetOrPostMethod; const AInput,
  AOutput: TStringStream; const ABlockIndex: LongWord;
  var ATransID: LongWord; out ABlockCRC, ABlockCount: LongWord);
  begin
    inherited;
  end;

  procedure TtiHTTPIndyForTesting.DoPost(const AURL: string; AInput,
    AOutput: TStringStream);
  begin
    inherited;
  end;

function TtiWebServerTestCase.TestHTTPRequestInBlocks(
  const ADocument: string;
  const ABlockSize: Longword;
  const ABlockIndex: Longword;
  var   ABlockCount, ATransID, ABlockCRC: Longword): string;
var
  LInput: TStringStream;
  LOutput: TStringStream;
  LHTTP: TtiHTTPIndyForTesting;
begin

  LHTTP:= nil;
  LInput:= nil;
  LOutput:= nil;
  try
    LHTTP:= TtiHTTPIndyForTesting.Create;
    
    LInput:= TStringStream.Create('');
    LOutput:= TStringStream.Create('');

    LHTTP.BlockSize:= ABlockSize;
    LHTTP.DoGetOrPostBlockWithRetry(
      'http://localhost:' + IntToStr(cPort) + '/' + ADocument,
      LHTTP.DoPost,
      LInput,
      LOutput,
      ABlockIndex,
      ATransID,
      ABlockCRC,
      ABlockCount);

    Result:= LOutput.DataString;

  finally
    LHTTP.Free;
    LInput.Free;
    LOutput.Free;
  end;
end;

procedure TtiWebServerTestCase.TestRunCGIExtension(const AParam: string);
var
  LO: TtiWebServerForTesting;
  LResult: string;
begin
  LO:= TtiWebServerForTesting.Create(cPort);
  try
    LO.Start;
    LO.SetCGIBinLocation(tiAddTrailingSlash(tiGetEXEPath) + 'CGI-Bin');
    LResult:= TestHTTPRequest('tiWebServerCGIForTesting.exe', True, AParam);
    // ToDo: Tidy up the white space padding before and after the result string
    CheckEquals(Trim(AParam), Trim(LResult));
  finally
    LO.Free;
  end;
end;

type
  TtiBlockStreamCacheForTesting = class(TtiBlockStreamCache)
  public
    property Count;
  end;

procedure TtiWebServerTestCase.tiBlockStreamCache_AddRead;
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
    CheckEquals(1, L.Count);

    L.AddBlockStream('jklMNOpq', 3, LBlockText, LBlockCount, LTransID);
    CheckEquals(2, L.Count);
    CheckEquals('jkl', LBlockText);
    CheckEquals(3, LBlockCount);
    CheckEquals(2, LTransID);
    CheckEquals(2, L.Count);

    L.ReadBlock(2, 0, LBlockText);
    CheckEquals('jkl', LBlockText);
    L.ReadBlock(2, 1, LBlockText);
    CheckEquals('MNO', LBlockText);
    L.ReadBlock(2, 2, LBlockText);
    CheckEquals('pq', LBlockText);
    CheckEquals(2, L.Count);

    L.ReadBlock(1, 0, LBlockText);
    CheckEquals('abc', LBlockText);
    L.ReadBlock(1, 1, LBlockText);
    CheckEquals('DEF', LBlockText);
    L.ReadBlock(1, 2, LBlockText);
    CheckEquals('gh', LBlockText);
    CheckEquals(2, L.Count);

  finally
    L.Free;
  end;
end;

procedure TtiWebServerTestCase.tiBlockStreamCache_SweepForTimeOuts;
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
  CBlockSize = 5;
  CRetryLimit = 6;
  CResolveTimeout = 7;
  CConnectTimeout = 8;
  CSendTimeout = 9;
  CReceiveTimeout = 10;

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
    LA.BlockSize:= CBlockSize;
    LA.RetryLimit:= CRetryLimit;
    LA.ResolveTimeout := CResolveTimeout;
    LA.ConnectTimeout := CConnectTimeout;
    LA.SendTimeout := CSendTimeout;
    LA.ReceiveTimeout := CReceiveTimeout;

    LB.Assign(LA);

    CheckEquals(CAppServerURL, LA.AppServerURL);
    CheckEquals(CConnectWith, LA.ConnectWith);
    CheckEquals(CProxyServerActive, LA.ProxyServerActive);
    CheckEquals(CProxyServerName, LA.ProxyServerName);
    CheckEquals(CProxyServerPort, LA.ProxyServerPort);
    CheckEquals(CBlockSize, LA.BlockSize);
    CheckEquals(CRetryLimit, LA.RetryLimit);
    CheckEquals(CResolveTimeout, LA.ResolveTimeout);
    CheckEquals(CConnectTimeout, LA.ConnectTimeout);
    CheckEquals(CSendTimeout, LA.SendTimeout);
    CheckEquals(CReceiveTimeout, LA.ReceiveTimeout);
  finally
    LA.Free;
    LB.Free;
  end;
end;

procedure TTestTIWebServerClientConnectionDetails.tiWebServerClientConnectionDetails_Equals;
var
  LA: TtiWebServerClientConnectionDetails;
  LB: TtiWebServerClientConnectionDetails;
const
  CAppServerURL= '1';
  CConnectWith= '2';
  CProxyServerActive= True;
  CProxyServerName= '3';
  CProxyServerPort= 4;
  CBlockSize = 5;
  CRetryLimit = 6;
  CResolveTimeout = 7;
  CConnectTimeout = 8;
  CSendTimeout = 9;
  CReceiveTimeout = 10;

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
    LA.BlockSize:= CBlockSize;
    LA.RetryLimit:= CRetryLimit;
    LA.ResolveTimeout := CResolveTimeout;
    LA.ConnectTimeout := CConnectTimeout;
    LA.SendTimeout := CSendTimeout;
    LA.ReceiveTimeout := CReceiveTimeout;

    LB.AppServerURL:= CAppServerURL;
    LB.ConnectWith:= CConnectWith;
    LB.ProxyServerActive:= CProxyServerActive;
    LB.ProxyServerName:= CProxyServerName;
    LB.ProxyServerPort:= CProxyServerPort;
    LB.BlockSize:= CBlockSize;
    LB.RetryLimit:= CRetryLimit;
    LB.ResolveTimeout := CResolveTimeout;
    LB.ConnectTimeout := CConnectTimeout;
    LB.SendTimeout := CSendTimeout;
    LB.ReceiveTimeout := CReceiveTimeout;

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

    Check(LA.Equals(LB));
    LB.BlockSize:= LB.BlockSize+1;
    Check(not LA.Equals(LB));
    LB.BlockSize:= CBlockSize;

    Check(LA.Equals(LB));
    LB.RetryLimit:= LB.RetryLimit+1;
    Check(not LA.Equals(LB));
    LB.RetryLimit:= CRetryLimit;

    Check(LA.Equals(LB));
    LB.ResolveTimeout:= LB.ResolveTimeout+1;
    Check(not LA.Equals(LB));
    LB.ResolveTimeout:= CResolveTimeout;

    Check(LA.Equals(LB));
    LB.ConnectTimeout:= LB.ConnectTimeout+1;
    Check(not LA.Equals(LB));
    LB.ConnectTimeout:= CConnectTimeout;

    Check(LA.Equals(LB));
    LB.SendTimeout:= LB.SendTimeout+1;
    Check(not LA.Equals(LB));
    LB.SendTimeout:= CSendTimeout;

    Check(LA.Equals(LB));
    LB.ReceiveTimeout:= LB.ReceiveTimeout+1;
    Check(not LA.Equals(LB));
    LB.ReceiveTimeout:= CReceiveTimeout;
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

procedure TTestTICGIParams.Clear;
var
  L: TtiCGIParams;
begin
  L:= TtiCGIParams.Create;
  try
    L.Values[CParam1]:= CValue1;
    L.Values[CParam2]:= CValue2;
    CheckEquals(2, L.Count);
    L.Clear;
    CheckEquals(0, L.Count);
  finally
    L.Free;
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

end.
