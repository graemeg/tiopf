{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiHTTP_TST;

interface
uses
  TestFrameWork
  ,Classes
  ,idHTTPServer
  ,IdTCPServer
  ,IdCustomHTTPServer
  ,IdContext
  ,tiHTTP
  ;

type

  TTestTIHTTP = class( TTestCase )
  private
    FHTTPServer : TidHTTPServer;
    FRequest: TMemoryStream;
    FResponse: TMemoryStream;
    FDocName: string ;
    FParams: string;
    procedure DoHTTPGet(AContext:TIdContext;
                        ARequestInfo: TIdHTTPRequestInfo;
                        AResponseInfo: TIdHTTPResponseInfo);
    procedure DoHTTPGetError(AContext:TIdContext;
                        ARequestInfo: TIdHTTPRequestInfo;
                        AResponseInfo: TIdHTTPResponseInfo);
    function  MakeXMLResponse(const pDocName, pParams: string): string;
    procedure DoTIHTTPGetTest(const pClass: TtiHTTPClass);
    procedure DoTIHTTPPostTest(const pClass: TtiHTTPClass);
    procedure DoTIHTTPGetErrorTest(const pClass: TtiHTTPClass);
    procedure DoTIHTTPPostErrorTest(const pClass: TtiHTTPClass);
    procedure DoTIHTTPGetPerformanceTest(const pClass: TtiHTTPClass; pCount: integer);
    procedure DoTIHTTPPostPerformanceTest(const pClass: TtiHTTPClass; pCount: Integer);
  protected
    procedure Setup; override;
    procedure TearDown; override;
  public
    constructor Create(MethodName: string); override;
    destructor  Destroy; override;
  published

    procedure CorrectURL;

    procedure TIHTTPIndyGet;
    procedure TIHTTPIndyGetError;
    procedure TIHTTPIndyGetPerformance;

    procedure TIHTTPIndyPost;
    procedure TIHTTPIndyPostError;
    procedure TIHTTPIndyPostPerformance;

    procedure TIHTTPMSXMLHTTPGet;
    procedure TIHTTPMSXMLHTTPGetError;
    procedure TIHTTPMSXMLHTTPGetPerformance;

    procedure TIHTTPMSXMLHTTPPost;
    procedure TIHTTPMSXMLHTTPPostError;
    procedure TIHTTPMSXMLHTTPPostPerformance;

    procedure TIHTTPFactory;
    procedure SetDefaultHTTPClass;
    procedure IsInstanceOfType;

  end ;

procedure RegisterTests ;

implementation
uses
   tiDUnitDependencies
  ,SysUtils
  ,tiUtils
  ,Windows
  ,tiHTTPIndy
  ,tiHTTPMSXML
  ,tiDialogs
  ,cTIPersist
  ;

const
  cTestDocName = 'testdoc';
  cTestParams  = 'prop1=value1,prop2=2,prop3=1.234';
  cExpectedResponseText = 'HTTP/1.1 200 OK';
  cExpectedResponseErrorText = 'HTTP/1.1 500 Internal Server Error';


procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIHTTP.Suite );
end;


{ TTestTIHTTP }

constructor TTestTIHTTP.Create(MethodName: string);
begin
  inherited Create(MethodName);
  FHTTPServer := TidHTTPServer.Create(nil);
  FHTTPServer.OnCommandGet := DoHTTPGet ;
  FRequest  := TMemoryStream.Create;
  FResponse := TMemoryStream.Create;
  FDocName  := cTestDocName ;
end;

destructor TTestTIHTTP.Destroy;
begin
  FHTTPServer.Free;
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure TTestTIHTTP.DoHTTPGet(AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FDocName := Copy( ARequestInfo.Document, 2, Length(ARequestInfo.Document) - 1 );
  FParams  := ARequestInfo.UnparsedParams;
  AResponseInfo.ContentText := MakeXMLResponse( FDocName, FParams ) ;
end;

function TTestTIHTTP.MakeXMLResponse(const pDocName, pParams: string ) : string ;
begin
  Result := '<xml> docname="' + pDocName + '" params="' + pParams + '"</xml>' ;
end;

procedure TTestTIHTTP.Setup;
begin
  FHTTPServer.Active := True ;
  FRequest.Size:= 0 ;
  FResponse.Size:= 0 ;
  FDocName:= '' ;
  FParams := '' ;
end;

procedure TTestTIHTTP.TearDown;
begin
  FHTTPServer.Active := False ;
end;

procedure TTestTIHTTP.TIHTTPIndyPostPerformance;
begin
  DoTIHTTPPostPerformanceTest(TtiHTTPIndy, 500);
end;

procedure TTestTIHTTP.TIHTTPIndyGet;
begin
  DoTIHTTPGetTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.DoTIHTTPGetTest(const pClass: TtiHTTPClass);
var
  lHTTP : TtiHTTPAbs ;
  lExpected : string ;
  lActual   : string ;
begin
  Assert(pClass<>nil, 'pClass not assigned');
  lHTTP := pClass.Create ;
  try
    tiStringToStream(cTestParams, lHTTP.Input );
    lHTTP.Get(cLocalHost + '/' + cTestDocName );
    lActual := tiStreamToString(lHTTP.Output);
    lExpected := MakeXMLResponse( cTestDocName, cTestParams ) ;
    CheckEquals(cTestDocName, FDocName, 'DocName');
    CheckEquals(cTestParams, FParams, 'Params');
    CheckEquals(lExpected, lActual, 'Response');
    CheckEquals(200, lHTTP.ResponseCode, 'ResponseCode');
    CheckEquals(cExpectedResponseText, lHTTP.ResponseText, 'ResponseText');
  finally
    lHTTP.Free;
  end;
end;

procedure TTestTIHTTP.DoTIHTTPPostTest(const pClass: TtiHTTPClass);
var
  lHTTP : TtiHTTPAbs ;
  lExpected : string ;
  lActual   : string ;
begin
  Assert(pClass<>nil, 'pClass not assigned');
  lHTTP := pClass.Create ;
  try
    tiStringToStream(cTestParams, lHTTP.Input );
    lHTTP.Post(cLocalHost + '/' + cTestDocName );
    lActual := tiStreamToString(lHTTP.Output);
    lExpected := MakeXMLResponse( cTestDocName, cTestParams ) ;
    CheckEquals(cTestDocName, FDocName, 'DocName');
    CheckEquals(cTestParams, FParams, 'Params');
    CheckEquals(lExpected, lActual, 'Response');
    CheckEquals(200, lHTTP.ResponseCode, 'ResponseCode');
    CheckEquals(cExpectedResponseText, lHTTP.ResponseText, 'ResponseText');
  finally
    lHTTP.Free;
  end;
end;

procedure TTestTIHTTP.TIHTTPIndyPost;
begin
  DoTIHTTPPostTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.DoHTTPGetError(AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AREsponseInfo.ResponseNo := 500 ;
end;

procedure TTestTIHTTP.TIHTTPIndyGetError;
begin
  DoTIHTTPGetErrorTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.TIHTTPIndyPostError;
begin
  DoTIHTTPPostErrorTest(TtiHTTPIndy);
end;

procedure TTestTIHTTP.DoTIHTTPGetErrorTest(const pClass: TtiHTTPClass);
var
  lHTTP : TtiHTTPAbs ;
begin
  Assert(pClass<>nil, 'pClass not assigned');
  FHTTPServer.OnCommandGet := DoHTTPGetError ;
  try
    lHTTP := pClass.Create ;
    try
      lHTTP.FormatExceptions := False ;
      tiStringToStream(cTestParams, lHTTP.Input );
      try
        lHTTP.Get(cLocalHost + '/' + cTestDocName );
        Fail('Exception not raised when it should have been');
      except
        on e:Exception do
          CheckEquals(cExpectedResponseErrorText, e.message, 'E.Message' ) ;
      end ;
      CheckEquals(500, lHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseErrorText, lHTTP.ResponseText, 'ResponseText');
    finally
      lHTTP.Free;
    end;
  finally
    FHTTPServer.OnCommandGet := DoHTTPGet ;
  end ;
end;

procedure TTestTIHTTP.DoTIHTTPPostErrorTest(const pClass: TtiHTTPClass);
var
  lHTTP : TtiHTTPAbs ;
  lExpected : string ;
  lActual   : string ;
begin
  Assert(pClass<>nil, 'pClass not assigned');
  FHTTPServer.OnCommandGet := DoHTTPGetError ;
  try
    lHTTP := pClass.Create ;
    try
      lHTTP.FormatExceptions := False ;
      tiStringToStream(cTestParams, lHTTP.Input );
      try
        lHTTP.Post(cLocalHost + '/' + cTestDocName );
        Fail('Exception not raised when expected');
      except
        on e:Exception do
          CheckEquals(cExpectedResponseErrorText, e.message, 'E.Message' ) ;
      end ;
      lActual := tiStreamToString(lHTTP.Output);
      lExpected := MakeXMLResponse( cTestDocName, cTestParams ) ;
      CheckEquals(500, lHTTP.ResponseCode, 'ResponseCode');
      CheckEquals(cExpectedResponseErrorText, lHTTP.ResponseText, 'ResponseText');
    finally
      lHTTP.Free;
    end;
  finally
    FHTTPServer.OnCommandGet := DoHTTPGet ;
  end ;
end;

procedure TTestTIHTTP.TIHTTPFactory;
var
  lHTTP: TtiHTTPAbs;
begin
  lHTTP := gTIHTTPFactory.CreateInstance(cHTTPMsXml);
  try
    CheckIs(lHTTP, TtiHTTPMSXML);
  finally
    lHTTP.Free;
  end;
  lHTTP := gTIHTTPFactory.CreateInstance(cHTTPIndy);
  try
    CheckIs(lHTTP, TtiHTTPIndy);
  finally
    lHTTP.Free;
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
  lHTTP: TtiHTTPAbs;
begin
  lHTTP := gTIHTTPFactory.CreateInstance(cHTTPMsXml);
  try
    Check(gTIHTTPFactory.IsInstanceOfType(lHTTP, cHTTPMsXml), cHTTPMsXml);
  finally
    lHTTP.Free;
  end;
  lHTTP := gTIHTTPFactory.CreateInstance(cHTTPIndy);
  try
    Check(gTIHTTPFactory.IsInstanceOfType(lHTTP, cHTTPIndy), cHTTPIndy);
  finally
    lHTTP.Free;
  end;
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGet;
begin
  DoTIHTTPGetTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetError;
begin
  DoTIHTTPGetErrorTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPost;
begin
  DoTIHTTPPostTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPostError;
begin
  DoTIHTTPPostErrorTest(TtiHTTPMSXML);
end;

procedure TTestTIHTTP.TIHTTPIndyGetPerformance;
begin
  DoTIHTTPGetPerformanceTest(TtiHTTPIndy, 500);
end;

procedure TTestTIHTTP.DoTIHTTPGetPerformanceTest(
  const pClass: TtiHTTPClass; pCount: integer);
var
  lHTTP : TtiHTTPAbs ;
  i : Integer ;
  lStart : DWord ;
  lMSPer10Calls: real ;
begin
  lHTTP := pClass.Create ;
  try
    tiStringToStream(cTestParams, lHTTP.Input);
    lStart := GetTickCount;
    for i := 1 to pCount do
      lHTTP.Post(cLocalHost + '/' + cTestDocName);
    lMSPer10Calls := ( GetTickCount - lStart ) / pCount  ;
    Check( lMSPer10Calls < 10, Format('Not fast enough %f ms per call. Should be %d ms per call.',
                                      [lMSPer10Calls, 10]));
  finally
    lHTTP.Free;
  end;
end;

procedure TTestTIHTTP.DoTIHTTPPostPerformanceTest(
  const pClass: TtiHTTPClass; pCount: Integer);
var
  lHTTP : TtiHTTPAbs ;
  i : Integer ;
  lStart : DWord ;
  lMSPer10Calls: real ;
begin
  lHTTP := pClass.Create ;
  try
    tiStringToStream(cTestParams, lHTTP.Input);
    lStart := GetTickCount;
    for i := 1 to pCount do
      lHTTP.Get(cLocalHost + '/' + cTestDocName);
    lMSPer10Calls := ( GetTickCount - lStart ) / pCount  ;
    Check( lMSPer10Calls < 10, Format('Not fast enough %f ms per call. Should be %d ms per call.', [lMSPer10Calls, 10]));
  finally
    lHTTP.Free;
  end;
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPPostPerformance;
begin
  DoTIHTTPPostPerformanceTest(TtiHTTPMSXML, 500);
end;

procedure TTestTIHTTP.TIHTTPMSXMLHTTPGetPerformance;
begin
  DoTIHTTPGetPerformanceTest(TtiHTTPMSXML, 500);
end;

procedure TTestTIHTTP.CorrectURL;
var
  lHTTP: TtiHTTPAbs;
begin
  lHTTP:= TtiHTTPMSXML.Create;
  try
    CheckEquals('http://localhost', lHTTP.CorrectURL('http://localhost'), '#1');
    CheckEquals('http://localhost', lHTTP.CorrectURL('http:\\localhost'), '#2');
    CheckEquals('http://localhost/test', lHTTP.CorrectURL('http:\\localhost\test'), '#3');
    CheckEquals('http://localhost?param=value', lHTTP.CorrectURL('http:\\localhost?param=value'), '#4');
    CheckEquals('http://localhost?param="value"', lHTTP.CorrectURL('http:\\localhost?param="value"'), '#5');
    CheckEquals('http://localhost?param="value\value"', lHTTP.CorrectURL('http:\\localhost?param="value\value"'), '#6');
  finally
    lHTTP.Free;
  end;
end;

end.
