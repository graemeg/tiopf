unit tiHTTP_TST;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
  ,Classes
  ,idHTTPServer
  ,IdTCPServer
  ,IdCustomHTTPServer
  ,IdContext
  ,tiHTTP
;

type

  TTestTIHTTP = class(TtiTestCase)
  private
    FHTTPServer : TidHTTPServer;
    FRequest: TMemoryStream;
    FResponse: TMemoryStream;
    FDocName: string;
    FParams: string;
    FExpectedResult: string;
    function  MakeTestURL(const ADocPath: string): string;
    procedure HTTPGet_Event(AContext:TIdContext;
                           ARequestInfo: TIdHTTPRequestInfo;
                           AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPGet_ErrorEvent(AContext:TIdContext;
                        ARequestInfo: TIdHTTPRequestInfo;
                        AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPGet_CustomHeaderOutputEvent(AContext:TIdContext;
                        ARequestInfo: TIdHTTPRequestInfo;
                        AResponseInfo: TIdHTTPResponseInfo); // Test custom headers are being passed from server to client
    procedure HTTPGet_CustomHeaderInputEvent(AContext:TIdContext;
                        ARequestInfo: TIdHTTPRequestInfo;
                        AResponseInfo: TIdHTTPResponseInfo); // Test custom headers are being passed from client to server
    procedure HTTPGet_BlockResponseEvent(AContext:TIdContext;
                           ARequestInfo: TIdHTTPRequestInfo;
                           AResponseInfo: TIdHTTPResponseInfo);

    procedure tiHTTPGetTest(AClass: TtiHTTPClass);
    procedure tiHTTPPostTest(AClass: TtiHTTPClass);
    procedure tiHTTPGetErrorTest(AClass: TtiHTTPClass);
    procedure tiHTTPPostErrorTest(AClass: TtiHTTPClass);
    procedure tiHTTPGetPerformanceTest(AClass: TtiHTTPClass; ACount: Integer; ATimePerCall: integer);
    procedure tiHTTPPostPerformanceTest(AClass: TtiHTTPClass; ACount: Integer; ATimePerCall: integer);

    procedure tiHTTPGetCustomHeaderOutputTest(AClass: TtiHTTPClass);
    procedure tiHTTPPostCustomHeaderOutputTest(AClass: TtiHTTPClass);
    procedure tiHTTPGetCustomHeaderInputTest(AClass: TtiHTTPClass);
    procedure tiHTTPPostCustomHeaderInputTest(AClass: TtiHTTPClass);
    procedure tiHTTPGetBlockResponseTest(AClass: TtiHTTPClass);
    procedure tiHTTPPostBlockResponseInputTest(AClass: TtiHTTPClass);

    function  MakeXMLResponse(const pDocName, AParams: string): string;
    procedure CheckTIOPFBlockHeader(const ABlockHeader: string;
                                    const ABlockIndex, ABlockCount, ABlockSize, ATransID: Longword);
    function  GetRandom: string;


  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published

    procedure StringStreamCopyFrom;
    procedure CorrectURL;
    procedure tiPortFromURL;
    procedure tiMakeTIOPFHTTPBlockHeader;
    procedure tiParseTIOPFHTTPBlockHeader;

    procedure tiHTTPIndyGet;
    procedure tiHTTPIndyGetError;
    procedure tiHTTPIndyGetPerformance;
    procedure tiHTTPIndyGetCustomHeaderInput;
    procedure tiHTTPIndyGetCustomHeaderOutput;
    procedure tiHTTPIndyGetBlockResponse;

    procedure tiHTTPIndyPost;
    procedure tiHTTPIndyPostError;
    procedure tiHTTPIndyPostPerformance;
    procedure tiHTTPIndyPostCustomHeaderInput;
    procedure tiHTTPIndyPostCustomHeaderOutput;
    procedure tiHTTPIndyPostBlockResponse;

    procedure tiHTTPMSXMLHTTPGet;
    procedure tiHTTPMSXMLHTTPGetError;
    procedure tiHTTPMSXMLHTTPGetPerformance;
    procedure tiHTTPMSXMLHTTPGetCustomHeaderInput;
    procedure tiHTTPMSXMLHTTPGetCustomHeaderOutput;
    procedure tiHTTPMSXMLHTTPGetBlockResponse;

    procedure tiHTTPMSXMLHTTPPost;
    procedure tiHTTPMSXMLHTTPPostError;
    procedure tiHTTPMSXMLHTTPPostPerformance;
    procedure tiHTTPMSXMLHTTPPostCustomHeaderInput;
    procedure tiHTTPMSXMLHTTPPostCustomHeaderOutput;
    procedure tiHTTPMSXMLHTTPPostBlockResponse;

    procedure tiHTTPFactoryString;
    procedure tiHTTPFactoryConnectionDetails;
    procedure SetDefaultHTTPClass;
    procedure IsInstanceOfType;

  end;

procedure RegisterTests;

implementation
uses
   tiTestDependencies
  ,tiUtils
  ,tiWebServer
  ,TestFramework
  ,SysUtils
  ,Registry
  ,Windows
  ,tiHTTPIndy
  ,tiHTTPMSXML
  ,tiConstants
  ,tiWebServerClientConnectionDetails
  ;

const
  cTestDocName = 'testdoc';
  cTestParams  = 'prop1=value1,prop2=2,prop3=1.234';
  cExpectedResponseText = 'HTTP/1.1 200 OK';
  cExpectedResponseErrorText = 'HTTP/1.1 500 Internal Server Error';
  cExpectedResponseErrorTextCountAttempts = 'HTTP/1.1 500 Internal Server Error (After 1 attempts)';

  cIndyTimePerCall    = 50;
  cMSXMLTimePerCall   = 65;
  cHTTPPortToTestWith = 81;

  ctiOPFBlockIDValue = '2' + ctiOPFHTTPBlockDelim +
                       '3' + ctiOPFHTTPBlockDelim +
                       '4' + ctiOPFHTTPBlockDelim +
                       '5';
  cPerformanceIterationCountIndy = 100;
  cPerformanceIterationCountMSXML = 100;

  cBlockResponse0 = 'abc' + #13#10;
  cBlockResponse1 = 'def' + #13#10;
  cBlockResponse2 = 'ghi';


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIHTTP);
end;

{ TTestTIHTTP }

function TTestTIHTTP.GetRandom: string;
var
  i: integer;
begin
  result:= '';
  for i := 0 to 9 do
    result:= result + IntToStr(GetTickCount);
end;

procedure TTestTIHTTP.HTTPGet_Event(AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FDocName := Copy(ARequestInfo.Document, 2, Length(ARequestInfo.Document) - 1);
  FParams:= tiHTTPRequestInfoToParams(ARequestInfo);
  AResponseInfo.ContentText := MakeXMLResponse(FDocName, FParams);
end;

function TTestTIHTTP.MakeXMLResponse(const pDocName, AParams: string): string;
begin
  Result := '<xml> docname="' + pDocName + '" params="' + AParams + '"</xml>';
end;

procedure TTestTIHTTP.SetUpOnce;
begin
end;

procedure TTestTIHTTP.SetUp;
begin
  FHTTPServer := TidHTTPServer.Create(nil);
  FHTTPServer.OnCommandGet:= nil;
  FHTTPServer.DefaultPort:= cHTTPPortToTestWith;
  FRequest := TMemoryStream.Create;
  FResponse := TMemoryStream.Create;
  FDocName:= '';
  FParams := '';
  FExpectedResult:= '';
end;

procedure TTestTIHTTP.TearDown;
begin
  FHTTPServer.Free;
  FRequest.Free;
  FResponse.Free;
  FDocName:= '';
  FParams := '';
  FExpectedResult:= '';
end;

procedure TTestTIHTTP.TeardownOnce;
begin
end;

procedure TTestTIHTTP.TIHTTPIndyPostPerformance;
begin
  TIHTTPPostPerformanceTest(TtiHTTPIndy, cPerformanceIterationCountIndy, cIndyTimePerCall);
end;

procedure TTestTIHTTP.TIHTTPIndyGet;
begin
  tiHTTPGetTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPGetTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  lExpected : string;
  lActual  : string;
begin
  AllowedMemoryLeakSize := 24; // Allow fixed size read from file.
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet:= HTTPGet_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      // Nasty hack, sorry, but better than cut & paste all the code in this test
      if LHTTP is TtiHTTPMSXML then
        (LHTTP as TtiHTTPMSXML).AutoFlushCache:= false;

      //Get passes params in URL rather than body like post
      LHTTP.Get(MakeTestURL(cTestDocName + '?' + cTestParams));
      lActual := tiStreamToString(LHTTP.Output);
      lExpected := MakeXMLResponse(cTestDocName, cTestParams);
      CheckEquals(cTestDocName, FDocName, 'DocName');
      CheckEquals(cTestParams, FParams, 'Params');
      CheckEquals(lExpected, lActual, 'Response');
      CheckEquals(200, LHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseText, LHTTP.ResponseText, 'ResponseText');
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPPostTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  lExpected : string;
  lActual  : string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet:= HTTPGet_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      // Nasty hack, sorry, but better than cut & paste all the code in this test
      if LHTTP is TtiHTTPMSXML then
        (LHTTP as TtiHTTPMSXML).AutoFlushCache:= false;

      tiStringToStream(cTestParams, LHTTP.Input);
      LHTTP.Post(MakeTestURL(cTestDocName));
      lActual := tiStreamToString(LHTTP.Output);
      lExpected := MakeXMLResponse(cTestDocName, cTestParams);
      CheckEquals(cTestDocName, FDocName, 'DocName');
      CheckEquals(cTestParams, FParams, 'Params');
      CheckEquals(lExpected, lActual, 'Response');
      CheckEquals(200, LHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseText, LHTTP.ResponseText, 'ResponseText');
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.TIHTTPIndyPost;
begin
  tiHTTPPostTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.HTTPGet_ErrorEvent(AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AREsponseInfo.ResponseNo := 500;
end;

procedure TTestTIHTTP.TIHTTPIndyGetError;
begin
  tiHTTPGetErrorTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.TIHTTPIndyPostError;
begin
  tiHTTPPostErrorTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPGetErrorTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_ErrorEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.FormatExceptions := False;
      tiStringToStream(cTestParams, LHTTP.Input);
      try
        LHTTP.Get(MakeTestURL(cTestDocName));
        Fail('Exception not raised when it should have been');
      except
        on e:Exception do
          CheckEquals(cExpectedResponseErrorTextCountAttempts, e.message, 'E.Message');
      end;
      CheckEquals(500, LHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseErrorText, LHTTP.ResponseText, 'ResponseText');
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPPostErrorTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  lExpected : string;
  lActual  : string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_ErrorEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.FormatExceptions := False;
      tiStringToStream(cTestParams, LHTTP.Input);
      try
        LHTTP.Post(MakeTestURL(cTestDocName));
        Fail('Exception not raised when expected');
      except
        on e:Exception do
          CheckEquals(cExpectedResponseErrorTextCountAttempts, e.message, 'E.Message');
      end;
      lActual := tiStreamToString(LHTTP.Output);
      lExpected := MakeXMLResponse(cTestDocName, cTestParams);
      CheckEquals(500, LHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseErrorText, LHTTP.ResponseText, 'ResponseText');
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPFactoryConnectionDetails;
var
  LHTTP: TtiHTTPAbs;
  LParams: TtiWebServerClientConnectionDetails;
begin
  LParams:= TtiWebServerClientConnectionDetails.Create;
  try
    LParams.ConnectWith:= cHTTPMSXML;
    LHTTP := gTIHTTPFactory.CreateInstance(LParams);
    try
      CheckIs(LHTTP, TtiHTTPMSXML);
    finally
      LHTTP.Free;
    end;
    LParams.ConnectWith:= cHTTPIndy;
    LHTTP := gTIHTTPFactory.CreateInstance(LParams);
    try
      CheckIs(LHTTP, TtiHTTPIndy);
    finally
      LHTTP.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TTestTIHTTP.tiHTTPFactoryString;
var
  LHTTP: TtiHTTPAbs;
begin
  LHTTP := gTIHTTPFactory.CreateInstance(cHTTPMsXml);
  try
    CheckIs(LHTTP, TtiHTTPMSXML);
  finally
    LHTTP.Free;
  end;
  LHTTP := gTIHTTPFactory.CreateInstance(cHTTPIndy);
  try
    CheckIs(LHTTP, TtiHTTPIndy);
  finally
    LHTTP.Free;
  end;
end;

procedure TTestTIHTTP.SetDefaultHTTPClass;
begin
  tiHTTP.SetDefaultHTTPClass(cHTTPMsXml);
  Check(gTIHTTPClass = TtiHTTPMSXML, cHTTPMsXml);
  tiHTTP.SetDefaultHTTPClass(cHTTPIndy);
  Check(gTIHTTPClass = TtiHTTPIndy, cHTTPIndy);
end;

procedure TTestTIHTTP.IsInstanceOfType;
var
  LHTTP: TtiHTTPAbs;
begin
  LHTTP := gTIHTTPFactory.CreateInstance(cHTTPMsXml);
  try
    Check(gTIHTTPFactory.IsInstanceOfType(LHTTP, cHTTPMsXml), cHTTPMsXml);
  finally
    LHTTP.Free;
  end;
  LHTTP := gTIHTTPFactory.CreateInstance(cHTTPIndy);
  try
    Check(gTIHTTPFactory.IsInstanceOfType(LHTTP, cHTTPIndy), cHTTPIndy);
  finally
    LHTTP.Free;
  end;
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGet;
begin
  tiHTTPGetTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetError;
begin
  tiHTTPGetErrorTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPost;
begin
  tiHTTPPostTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPostError;
begin
  tiHTTPPostErrorTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPIndyGetPerformance;
begin
  tiHTTPGetPerformanceTest(TtiHTTPIndy, cPerformanceIterationCountIndy, cIndyTimePerCall);
end;

procedure TTestTIHTTP.tiHTTPGetPerformanceTest(AClass: TtiHTTPClass; ACount: Integer; ATimePerCall: integer);
var
  LHTTP : TtiHTTPAbs;
  i : Integer;
  lStart : DWord;
  lMSPer10Calls: Extended;
begin
  Check(True);
  FHTTPServer.OnCommandGet:= HTTPGet_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      tiStringToStream(cTestParams, LHTTP.Input);
      lStart := GetTickCount;
      for i := 1 to ACount do
        LHTTP.Post(MakeTestURL(cTestDocName));
      lMSPer10Calls := (GetTickCount - lStart) / ACount;
      Check(lMSPer10Calls < ATimePerCall, Format('Not fast enough %f ms per call. Should be %d ms per call.',
                                        [lMSPer10Calls, ATimePerCall]));
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPPostPerformanceTest(AClass: TtiHTTPClass; ACount: Integer; ATimePerCall: Integer);
var
  LHTTP : TtiHTTPAbs;
  i : Integer;
  lStart : DWord;
  lMSPer10Calls: Extended;
begin
  FHTTPServer.OnCommandGet:= HTTPGet_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      tiStringToStream(cTestParams, LHTTP.Input);
      lStart := GetTickCount;
      for i := 1 to ACount do
        LHTTP.Post(MakeTestURL(cTestDocName));
      lMSPer10Calls := (GetTickCount - lStart) / ACount;
      if ATimePerCall <> 0 then
        Check(lMSPer10Calls < ATimePerCall, Format('Not fast enough %f ms per call. Should be %d ms per call.', [lMSPer10Calls, ATimePerCall]));
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 0, 1, ctiOPDHTTPNullBlockSize, 0);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPostPerformance;
begin
  tiHTTPPostPerformanceTest(TtiHTTPMSXML, cPerformanceIterationCountMSXML, 0);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetPerformance;
begin
  tiHTTPGetPerformanceTest(TtiHTTPMSXML, cPerformanceIterationCountMSXML, cMSXMLTimePerCall);
end;

procedure TTestTIHTTP.CorrectURL;
var
  LHTTP: TtiHTTPAbs;
begin
  LHTTP:= TtiHTTPMSXML.Create;
  try
    CheckEquals('http://localhost', LHTTP.CorrectURL('http://localhost'), '#1');
    CheckEquals('http://localhost', LHTTP.CorrectURL('http:\\localhost'), '#2');
    CheckEquals('http://localhost/test', LHTTP.CorrectURL('http:\\localhost\test'), '#3');
    CheckEquals('http://localhost?param=value', LHTTP.CorrectURL('http:\\localhost?param=value'), '#4');
    CheckEquals('http://localhost?param="value"', LHTTP.CorrectURL('http:\\localhost?param="value"'), '#5');
    CheckEquals('http://localhost?param="value\value"', LHTTP.CorrectURL('http:\\localhost?param="value\value"'), '#6');
  finally
    LHTTP.Free;
  end;
end;

procedure TTestTIHTTP.tiPortFromURL;
var
  LDefPort : word;
begin
  LDefPort := StrToInt(tiConstants.CDefaultPort);

  CheckEquals(1234,tiHTTP.tiPortFromURL('http://localhost:1234',LDefPort));
  //Invalid should be default
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('http://localhost:/81',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('http://localhost/test:123',LDefPort));
  CheckEquals(123,tiHTTP.tiPortFromURL('http://localhost:123?param=value',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('http://localhost/kjf/ksd:123/aaa',LDefPort));
  CheckEquals(11,tiHTTP.tiPortFromURL('http://localhost:11?param="value\value"',LDefPort));
  CheckEquals(65535,tiHTTP.tiPortFromURL('http://localhost:65535?param="value\value"',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('http://localhost:65536?param="value\value"',LDefPort));
  CheckEquals(1,tiHTTP.tiPortFromURL('http://localhost:1/param="value\value"',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('://localhost:1234',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('localhost:5555',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL(':6543',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('dfasdfsdf',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('http://localhost:012345',LDefPort));
  CheckEquals(1234,tiHTTP.tiPortFromURL('http://localhost:01234',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('ftp://localhost:0',LDefPort));
  CheckEquals(LDefPort,tiHTTP.tiPortFromURL('',LDefPort));
end;

function TTestTIHTTP.MakeTestURL(const ADocPath: string): string;
begin
  Result:= 'http://localhost:' + IntToStr(cHTTPPortToTestWith) + '/' + ADocPath;
end;

procedure TTestTIHTTP.tiHTTPGetCustomHeaderOutputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LRandom: string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderOutputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(' ' + ctiOPFBlockIDValue, LHTTP.ResponseHeaders.Values[ctiOPFHTTPBlockHeader]);
      CheckEquals(ctiOPFBlockIDValue, LHTTP.ResponseHeader[ctiOPFHTTPBlockHeader]);
      CheckEquals(LHTTP.ResponseHeader[ctiOPFHTTPBlockHeader], LHTTP.ResponseTIOPFBlockHeader);
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 2, 3, 4, 5);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= false;
  end;
end;

procedure TTestTIHTTP.tiHTTPPostCustomHeaderOutputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderOutputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Post(MakeTestURL(cTestDocName));
      CheckEquals(' ' + ctiOPFBlockIDValue, LHTTP.ResponseHeaders.Values[ctiOPFHTTPBlockHeader]);
      CheckEquals(ctiOPFBlockIDValue, LHTTP.ResponseHeader[ctiOPFHTTPBlockHeader]);
      CheckEquals(LHTTP.ResponseHeader[ctiOPFHTTPBlockHeader], LHTTP.ResponseTIOPFBlockHeader);
      CheckTIOPFBlockHeader(LHTTP.ResponseTIOPFBlockHeader, 2, 3, 4, 5);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.HTTPGet_CustomHeaderOutputEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FDocName := Copy(ARequestInfo.Document, 2, Length(ARequestInfo.Document) - 1);
  FParams := ARequestInfo.UnparsedParams;
  AResponseInfo.ContentText := MakeXMLResponse(FDocName, FParams);
  AResponseInfo.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:= ctiOPFBlockIDValue;
end;

procedure TTestTIHTTP.TIHTTPIndyGetCustomHeaderOutput;
begin
  tiHTTPGetCustomHeaderOutputTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.TIHTTPIndyPostCustomHeaderOutput;
begin
  tiHTTPPostCustomHeaderOutputTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetCustomHeaderOutput;
begin
  tiHTTPGetCustomHeaderOutputTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPostCustomHeaderOutput;
begin
  tiHTTPPostCustomHeaderOutputTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.tiMakeTIOPFHTTPBlockHeader;
begin
  CheckEquals('1/2/3/4/5', tiHTTP.tiMakeTIOPFHTTPBlockHeader(1, 2, 3, 4, 5));
end;

procedure TTestTIHTTP.tiParseTIOPFHTTPBlockHeader;
var
  LBlockIndex: LongWord;
  LBlockCount:  LongWord;
  LBlockSize:   LongWord;
  LTransID:     LongWord;
  LBlockCRC:    LongWord;
begin
  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2/3/4/5', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(3, LBlockSize);
  CheckEquals(4, LTransID);
  CheckEquals(5, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2/3', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(3, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(1, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(1, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(0, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('a/b/c/d', LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(0, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);
  CheckEquals(0, LBlockCRC);

end;

procedure TTestTIHTTP.StringStreamCopyFrom;
var
  LStream1: TStringStream;
  LStream2: TStringStream;
begin
  LStream1:= TStringStream.Create('');
  try
    LStream2:= TStringStream.Create('');
    try
      LStream1.WriteString('abc');
      LStream2.WriteString('def');
      LStream1.CopyFrom(LStream2, 0);
      CheckEquals('abcdef', LStream1.DataString);
    finally
      LStream2.Free;
    end;
  finally
    LStream1.Free;
  end;
end;

procedure TTestTIHTTP.HTTPGet_CustomHeaderInputEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LText: string;
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  LongWord;
  LTransID:    Longword;
  LBlockCRC:   Longword;
begin
  LText:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LText, LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  LText:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  AResponseInfo.ContentText:= LText;
end;

procedure TTestTIHTTP.tiHTTPIndyGetCustomHeaderInput;
begin
  tiHTTPGetCustomHeaderInputTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPGetCustomHeaderInputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LHeader: string;
  LRandom: string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderInputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.DeriveRequestTIOPFBlockHeader:= False;
      LHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(2, 3, 4, 5, 0);
      LHTTP.RequestTIOPFBlockHeader:= LHeader;
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(LHeader, LHTTP.Output.DataString);
      LHTTP.RequestTIOPFBlockHeader:= '';
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= false;
  end;
end;

procedure TTestTIHTTP.CheckTIOPFBlockHeader(const ABlockHeader: string;
  const ABlockIndex, ABlockCount, ABlockSize, ATransID: Longword);
var
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  LongWord;
  LTransID:    Longword;
  LBlockCRC:   Longword;
begin
  tiHTTP.tiParseTIOPFHTTPBlockHeader(ABlockHeader, LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  CheckEquals(ABlockIndex, LBlockIndex);
  CheckEquals(ABlockCount, LBlockCount);
  CheckEquals(ABlockSize, LBlockSize);
  CheckEquals(ATransID, LTransID);
end;

procedure TTestTIHTTP.tiHTTPPostCustomHeaderInputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LHeader: string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderInputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.DeriveRequestTIOPFBlockHeader:= False;
      LHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(2, 3, 4, 5, 0);
      LHTTP.RequestTIOPFBlockHeader:= LHeader;
      LHTTP.Post(MakeTestURL(cTestDocName));
      CheckEquals(LHeader, LHTTP.Output.DataString);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPIndyPostCustomHeaderInput;
begin
  tiHTTPPostCustomHeaderInputTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPMSXMLHTTPGetCustomHeaderInput;
begin
  tiHTTPGetCustomHeaderInputTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.tiHTTPMSXMLHTTPPostCustomHeaderInput;
begin
  tiHTTPPostCustomHeaderInputTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.HTTPGet_BlockResponseEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LText: string;
  LBlockIndex: Longword;
  LBlockCount: Longword;
  LBlockSize:  Longword;
  LTransID:    Longword;
  LBlockCRC:   Longword;
begin
  LText:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LText, LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
  case LBlockIndex of
  0: begin
       AResponseInfo.ContentText:= cBlockResponse0;
       LBlockCount:= 3;
     end;
  1: AResponseInfo.ContentText:= cBlockResponse1;
  2: AResponseInfo.ContentText:= cBlockResponse2;
  else
    Assert(False, 'Invalid BlockIndex');
  end;
  AResponseInfo.CustomHeaders.Values[ctiOPFHTTPBlockHeader]:=
    tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID, LBlockCRC);
end;

procedure TTestTIHTTP.tiHTTPIndyGetBlockResponse;
begin
  tiHTTPGetBlockResponseTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPIndyPostBlockResponse;
begin
  tiHTTPPostBlockResponseInputTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPMSXMLHTTPGetBlockResponse;
begin
  tiHTTPGetBlockResponseTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.tiHTTPMSXMLHTTPPostBlockResponse;
begin
  tiHTTPPostBlockResponseInputTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.tiHTTPGetBlockResponseTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LRandom: string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_BlockResponseEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Post(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(cBlockResponse0 + cBlockResponse1 + cBlockResponse2, LHTTP.Output.DataString);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.tiHTTPPostBlockResponseInputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LRandom: string;
begin
  Assert(AClass<>nil, 'AClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_BlockResponseEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Post(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(cBlockResponse0 + cBlockResponse1 + cBlockResponse2, LHTTP.Output.DataString);
    finally
      LHTTP.Free;
    end;
  finally
    FHTTPServer.Active:= False;
  end;
end;



end.
