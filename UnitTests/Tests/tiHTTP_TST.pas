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
    procedure HTTPGetCache_Event(AContext:TIdContext;
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

    function  MakeXMLResponse(const pDocName, pParams: string): string;
    procedure CheckTIOPFBlockHeader(const ABlockHeader: string;
                                    const ABlockIndex, ABlockCount, ABlockSize, ATransID: Longword);
    function  GetRandom: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create(MethodName: string); override;
    destructor  Destroy; override;
  published

    procedure StringStreamCopyFrom;
    procedure CorrectURL;
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

    procedure tiHTTPMSXMLHTTPGetCacheFeature;

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

    procedure tiHTTPFactory;
    procedure SetDefaultHTTPClass;
    procedure IsInstanceOfType;

  end;

procedure RegisterTests;

implementation
uses
   tiDUnitDependencies
  ,SysUtils
  ,tiUtils
  ,Windows
  ,tiHTTPIndy
  ,tiHTTPMSXML
  ,tiConstants

  ,tiLog
  ,tiCom

 ;

const
  cTestDocName = 'testdoc';
  cTestParams  = 'prop1=value1,prop2=2,prop3=1.234';
  cExpectedResponseText = 'HTTP/1.1 200 OK';
  cExpectedResponseErrorText = 'HTTP/1.1 500 Internal Server Error';

  cIndyTimePerCall    = 35;
  cMSXMLTimePerCall   = 45;
  cHTTPPortToTestWith = 81;

  ctiOPFBlockIDValue = '2' + ctiOPFHTTPBlockDelim +
                       '3' + ctiOPFHTTPBlockDelim +
                       '4' + ctiOPFHTTPBlockDelim +
                       '5';
  cPerformanceIterationCount = 1;

  cBlockResponse0 = 'abc' + #13#10;
  cBlockResponse1 = 'def' + #13#10;
  cBlockResponse2 = 'ghi';


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIHTTP);
end;

{ TTestTIHTTP }

constructor TTestTIHTTP.Create(MethodName: string);
begin
  inherited Create(MethodName);
  FHTTPServer := TidHTTPServer.Create(nil);
  FHTTPServer.OnCommandGet := HTTPGet_Event;
  FHTTPServer.DefaultPort:= cHTTPPortToTestWith;
  FRequest  := TMemoryStream.Create;
  FResponse := TMemoryStream.Create;
  FDocName  := cTestDocName;
end;

destructor TTestTIHTTP.Destroy;
begin
  FHTTPServer.Free;
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

function TTestTIHTTP.GetRandom: string;
begin
  result:= tiCreateGUIDString;
end;

procedure TTestTIHTTP.HTTPGet_Event(AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FDocName := Copy(ARequestInfo.Document, 2, Length(ARequestInfo.Document) - 1);
  FParams  := ARequestInfo.UnparsedParams;
  AResponseInfo.ContentText := MakeXMLResponse(FDocName, FParams);
end;

function TTestTIHTTP.MakeXMLResponse(const pDocName, pParams: string) : string;
begin
  Result := '<xml> docname="' + pDocName + '" params="' + pParams + '"</xml>';
end;

procedure TTestTIHTTP.SetUp;
begin
  FHTTPServer.OnCommandGet:= nil;
  FHTTPServer.Active:= false;
  FRequest.Size:= 0;
  FResponse.Size:= 0;
  FDocName:= '';
  FParams := '';
end;

procedure TTestTIHTTP.TearDown;
begin
  FHTTPServer.Active := False;
  FHTTPServer.OnCommandGet:= nil;
end;

procedure TTestTIHTTP.TIHTTPIndyPostPerformance;
begin
  TIHTTPPostPerformanceTest(TtiHTTPIndy, cPerformanceIterationCount, cIndyTimePerCall);
end;

procedure TTestTIHTTP.TIHTTPIndyGet;
begin
  tiHTTPGetTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.tiHTTPGetTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  lExpected : string;
  lActual   : string;
begin
  Assert(AClass<>nil, 'pClass not assigned');
  FHTTPServer.OnCommandGet:= HTTPGet_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      // Nasty hack, sorry, but better than cut & paste all the code in this test
      if LHTTP is TtiHTTPMSXML then
        (LHTTP as TtiHTTPMSXML).AutoFlushCache:= false;

      tiStringToStream(cTestParams, LHTTP.Input);
      LHTTP.Get(MakeTestURL(cTestDocName));
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
  lActual   : string;
begin
  Assert(AClass<>nil, 'pClass not assigned');
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
  Assert(AClass<>nil, 'pClass not assigned');
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
          CheckEquals(cExpectedResponseErrorText, e.message, 'E.Message');
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
  lActual   : string;
begin
  Assert(AClass<>nil, 'pClass not assigned');
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
          CheckEquals(cExpectedResponseErrorText, e.message, 'E.Message');
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

procedure TTestTIHTTP.TIHTTPFactory;
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
  tiHTTPGetPerformanceTest(TtiHTTPIndy, cPerformanceIterationCount, cIndyTimePerCall);
end;

procedure TTestTIHTTP.tiHTTPGetPerformanceTest(AClass: TtiHTTPClass; ACount: Integer; ATimePerCall: integer);
var
  LHTTP : TtiHTTPAbs;
  i : Integer;
  lStart : DWord;
  lMSPer10Calls: real;
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
      lMSPer10Calls := (GetTickCount - lStart) / ACount ;
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
  lMSPer10Calls: real;
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
      lMSPer10Calls := (GetTickCount - lStart) / ACount ;
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
  tiHTTPPostPerformanceTest(TtiHTTPMSXML, 500, 0);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetPerformance;
begin
  tiHTTPGetPerformanceTest(TtiHTTPMSXML, 500, cMSXMLTimePerCall);
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

function TTestTIHTTP.MakeTestURL(const ADocPath: string): string;
begin
  Result:= 'http://localhost:' + IntToStr(cHTTPPortToTestWith) + '/' + ADocPath;
end;

procedure TTestTIHTTP.tiHTTPGetCustomHeaderOutputTest(AClass: TtiHTTPClass);
var
  LHTTP : TtiHTTPAbs;
  LRandom: string;
begin
  Assert(AClass<>nil, 'pClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderOutputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(ctiOPFBlockIDValue, LHTTP.ResponseHeaders.Values[ctiOPFHTTPBlockHeader]);
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
  Assert(AClass<>nil, 'pClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderOutputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.Post(MakeTestURL(cTestDocName));
      CheckEquals(ctiOPFBlockIDValue, LHTTP.ResponseHeaders.Values[ctiOPFHTTPBlockHeader]);
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
  FParams  := ARequestInfo.UnparsedParams;
  AResponseInfo.ContentText := MakeXMLResponse(FDocName, FParams);
  AResponseInfo.CustomHeaders.Values[ctiOPFHTTPBlockHeader] := ctiOPFBlockIDValue;
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
  CheckEquals('1/2/3/4', tiHTTP.tiMakeTIOPFHTTPBlockHeader(1, 2, 3, 4));
end;

procedure TTestTIHTTP.tiParseTIOPFHTTPBlockHeader;
var
  LBlockIndex: LongWord;
  LBlockCount:  LongWord;
  LBlockSize:   LongWord;
  LTransID:     LongWord;
begin
  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2/3/4', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(3, LBlockSize);
  CheckEquals(4, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2/3', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(3, LBlockSize);
  CheckEquals(0, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/2', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(1, LBlockIndex);
  CheckEquals(2, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1/', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(1, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('1', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(1, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(0, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);

  tiHTTP.tiParseTIOPFHTTPBlockHeader('a/b/c/d', LBlockIndex, LBlockCount, LBlockSize, LTransID);
  CheckEquals(0, LBlockIndex);
  CheckEquals(1, LBlockCount);
  CheckEquals(ctiOPDHTTPNullBlockSize, LBlockSize);
  CheckEquals(0, LTransID);

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
begin
  LText:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LText, LBlockIndex, LBlockCount, LBlockSize, LTransID);
  LText:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID);
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
  Assert(AClass<>nil, 'pClass not assigned');
  LRandom:= GetRandom;
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderInputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.DeriveRequestTIOPFBlockHeader:= False;
      LHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(2, 3, 4, 5);
      LHTTP.RequestTIOPFBlockHeader:= LHeader;
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      CheckEquals(LHeader, LHTTP.Output.DataString);
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
begin
  tiHTTP.tiParseTIOPFHTTPBlockHeader(ABlockHeader, LBlockIndex, LBlockCount, LBlockSize, LTransID);
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
  Assert(AClass<>nil, 'pClass not assigned');
  FHTTPServer.OnCommandGet := HTTPGet_CustomHeaderInputEvent;
  FHTTPServer.Active:= True;
  try
    LHTTP := AClass.Create;
    try
      LHTTP.DeriveRequestTIOPFBlockHeader:= False;
      LHeader:= tiHTTP.tiMakeTIOPFHTTPBlockHeader(2, 3, 4, 5);
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
begin
  LText:= ARequestInfo.RawHeaders.Values[ctiOPFHTTPBlockHeader];
  tiHTTP.tiParseTIOPFHTTPBlockHeader(LText, LBlockIndex, LBlockCount, LBlockSize, LTransID);
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
    tiHTTP.tiMakeTIOPFHTTPBlockHeader(LBlockIndex, LBlockCount, LBlockSize, LTransID);
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
  Assert(AClass<>nil, 'pClass not assigned');
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
  Assert(AClass<>nil, 'pClass not assigned');
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

procedure TTestTIHTTP.tiHTTPMSXMLHTTPGetCacheFeature;
var
  LHTTP : TtiHTTPMSXML;
  LActual   : string;
  LRandom: string;
begin

  // Read about it here: http://support.microsoft.com/kb/q201535/
  // and here: http://en.wikipedia.org/wiki/XMLHTTP#Microsoft_Internet_Explorer_Cache_issues
  // If this test starts failing, check the value of IE's "Check for newer versions of sotred pages"
  // under Tools | Internet Options | General | Temporary internet files | Settings...
  // Or, set the registry setting:
  //  HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\InternetSettings
  //    SyncMode5 = 0

  LRandom:= GetRandom; // To stop caching from previous tests
  FHTTPServer.OnCommandGet:= HTTPGetCache_Event;
  FHTTPServer.Active:= True;
  try
    LHTTP := TtiHTTPMSXML.Create;
    try
      LHTTP.AutoFlushCache:= false;

      FExpectedResult:= 'test';
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      LActual := tiStreamToString(LHTTP.Output);
      CheckEquals('test', LActual, '#1');

      FExpectedResult:= 'test1';
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom));
      LActual := tiStreamToString(LHTTP.Output);

      // Should return the old page.
      // If the test is failing here, change IE's cache setting as described
      // above.
      CheckEquals('test', LActual, '#2');

      FExpectedResult:= 'test3';
      LHTTP.Get(MakeTestURL(cTestDocName+LRandom) + '?param=1');
      LActual := tiStreamToString(LHTTP.Output);
      // Should return the new page
      CheckEquals('test3', LActual, '#3');

    finally
      LHTTP.Free;
    end;

    LHTTP := TtiHTTPMSXML.Create;
    try
      LHTTP.AutoFlushCache:= true;
      FExpectedResult:= 'test';
      LHTTP.Get(MakeTestURL(cTestDocName));
      LActual := tiStreamToString(LHTTP.Output);
      CheckEquals('test', LActual, '#1');

      FExpectedResult:= 'test1';
      LHTTP.Get(MakeTestURL(cTestDocName));
      LActual := tiStreamToString(LHTTP.Output);
      CheckEquals('test1', LActual, '#2');

      FExpectedResult:= 'test3';
      LHTTP.Get(MakeTestURL(cTestDocName));
      LActual := tiStreamToString(LHTTP.Output);
      CheckEquals('test3', LActual, '#3');
    finally
      LHTTP.Free;
    end;

  finally
    FHTTPServer.Active:= False;
  end;
end;

procedure TTestTIHTTP.HTTPGetCache_Event(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ContentText:= FExpectedResult;
end;

end.
