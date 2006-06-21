unit tiWebServer_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
  ;

type

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

  end ;

procedure RegisterTests ;

implementation
uses
   tiUtils

  ,tiDUnitDependencies
  ,tiWebServer

// Add an IfDef so these are only linked if the Remote persistence layer is tested
//  ,tiDBProxyServer
//  ,tiDBProxyServerStats
//  ,tiDBProxyServerDependencies

  ,tiWebServerConstants
  ,tiHTTPIndy
  ,tiLog

  ,SysUtils

  , tiHTTP;

procedure RegisterTests ;
begin
  RegisterNonPersistentTest(TtestTIWebServer);
end;

const
  cPort= 81;

{ TtestTIWebServer }

procedure TtestTIWebServer.tiDBProxyServer_Create;
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

procedure TtestTIWebServer.SetUp;
begin
  inherited;

end;

procedure TtestTIWebServer.TearDown;
begin
  inherited;

end;

procedure TtestTIWebServer.tiWebServer_Create;
var
  LO: TtiWebServer;
begin
  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    CheckEquals(tiGetEXEPath + PathDelim + cStaticPageDir + PathDelim,
                LO.StaticPageLocation);
    Check(DirectoryExists(LO.StaticPageLocation));
    CheckEquals(tiGetEXEPath + PathDelim + cCGIBinDir + PathDelim,
                LO.CGIBinLocation);
    LO.Stop;
  finally
    LO.Free;
  end;
end;

procedure TtestTIWebServer.tiDBProxyServer_ExecuteRemoteXML;
begin

end;

procedure TtestTIWebServer.tiDBProxyServer_TestAlive;
begin

end;

procedure TtestTIWebServer.tiDBProxyServer_TestAlive1;
begin

end;

procedure TtestTIWebServer.tiDBProxyServer_TestHTML;
begin

end;

procedure TtestTIWebServer.tiDBProxyServer_TestXML;
begin

end;

procedure TtestTIWebServer.tiWebServer_CanFindPage;
var
  LO: TtiWebServer;
  LResult: string;
  LFileName: string;
  LPage: string;
begin
  LFileName:= tiGetEXEPath + PathDelim + cStaticPageDir + PathDelim + 'testpage.htm';
  LPage:= '<html>test page</html>';
  tiStringToFile(LPage, LFileName);

  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    LResult:= TestHTTPRequest('testpage.htm');
    CheckEquals(LPage, LResult);

    LResult:= TestHTTPRequest('testpage');
    CheckEquals(LPage, LResult);

    DeleteFile(LFileName);
    LFileName:= tiSwapExt(LFileName, 'html');
    tiStringToFile(LPage, LFileName);

    LResult:= TestHTTPRequest('testpage');
    CheckEquals(LPage, LResult);

  finally
    LO.Free;
  end;
end;

procedure TtestTIWebServer.tiWebServer_CanNotFindPage;
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

procedure TtestTIWebServer.tiWebServer_Default;
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

procedure TtestTIWebServer.tiWebServer_GetLogFile;
var
  LO: TtiWebServer;
  LResult: string;
  LPage: string;
  LFileName: string;
begin
  LFileName:= gLog.LogToFileName;
  LPage:= 'test log file';
  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
    DeleteFile(LFileName);
    tiStringToFile(LPage, LFileName);
    LResult:= TestHTTPRequest(cgTIDBProxyGetLog);
    CheckEquals('<HTML><PRE>'+LPage+'</PRE></HTML>', LResult);
  finally
    LO.Free;
  end;
end;

procedure TtestTIWebServer.tiWebServer_Ignore;
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

procedure TtestTIWebServer.tiWebServer_RunCGIExtension;
begin

end;

procedure TtestTIWebServer.tiDBProxyServer_ServerVersion;
begin

end;

function TtestTIWebServer.TestHTTPRequest(const ADocument: string; AFormatException: boolean = True): string;
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

procedure TtestTIWebServer.tiWebServer_PageInBlocks;
var
  LO: TtiWebServer;
  LResult: string;
  LFileName: string;
  LPage: string;
  LBlockCount, LBlockIndex, LBlockSize, LTransID: Longword;
begin
  LFileName:= tiGetEXEPath + PathDelim + cStaticPageDir + PathDelim + 'testpage.htm';
  LPage:= 'abcDEFghiJKLmn';
  tiStringToFile(LPage, LFileName);

  LO:= TtiWebServer.Create(cPort);
  try
    LO.Start;
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

function TtestTIWebServer.TestHTTPRequestInBlocks(const ADocument: string;
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
    CheckEquals( 3, LBlockCount);
    CheckEquals(1, LTransID);

    L.AddBlockStream('jklMNOpq', 3, LBlockText, LBlockCount, LTransID);
    CheckEquals(2, L.Count);
    CheckEquals('jkl', LBlockText);
    CheckEquals( 3, LBlockCount);
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

end.
