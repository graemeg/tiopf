unit tiXML_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFrameWork
  {$ENDIF}
  ,Classes
  ,tiXML
  ,tiTestFramework
  ;

type

  TTestTIXML = class( TtiTestCase )
  private
    FXMLRCTrans: IXMLReservedCharsTranslator;
    function    tiReplicate1(const pStrValue: string;  pRepCount: Word): string;
  public
    constructor Create{$IFNDEF FPC}(AMethodName: string){$ENDIF}; override ;
  published
    procedure TestTIReplicate1;
    procedure XMLTag;
    procedure XMLTagEnd;
    procedure XMLDataTag;
    procedure DateTimeAsXMLString;
    procedure XMLStringToDateTime;

    // These test Peter's version, using lots of calls to Copy()
{
    procedure XMLRemoveReservedCharsXMLOld;
    procedure XMLInsertReservedCharsXMLOld;
    procedure XMLRemoveReservedCharsCSVOld;
    procedure XMLInsertReservedCharsCSVOld;
    procedure XMLRemoveReservedCharsTABOld;
    procedure XMLInsertReservedCharsTABOld;
}

    // This will test the performance improved version
    procedure XMLRemoveReservedCharsXML;
    procedure XMLInsertReservedCharsXML;
    procedure XMLRemoveReservedCharsCSV;
    procedure XMLInsertReservedCharsCSV;
    procedure XMLRemoveReservedCharsTAB;
    procedure XMLInsertReservedCharsTAB;

    // A few extra tests for things I did not think of with the first cut
    procedure XMLRemoveReservedCharsXML2;
    procedure XMLInsertReservedCharsXML2;

    // These will give an indication of how much faster you are
{
    procedure XMLRemoveReservedCharsXMLPerformance;
    procedure XMLInsertReservedCharsXMLPerformance;
    procedure XMLRemoveReservedCharsCSVPerformance;
    procedure XMLInsertReservedCharsCSVPerformance;
    procedure XMLRemoveReservedCharsTABPerformance;
    procedure XMLInsertReservedCharsTABPerformance;
}

    procedure tiParseForSingleNode;
    procedure tiParseForSingleNode1;
    procedure tiParseForSingleAttribute;
    procedure tiParseForSingleNodeAttribute;
    procedure tiParseForSingleNodeAttribute1;

  end ;

  TTestTIXMLParser = class(TtiTestCase)
  private
    FNode : string ;
    FAttributes : TStringList ;
    procedure   OnNode(const pStr : string );
    procedure   OnAttribute(const pName, pValue : string);
  protected
    procedure   SetUp; override ;
  public
    constructor Create{$IFNDEF FPC}(AMethodName: string){$ENDIF}; override ;
    destructor  Destroy; override;
  published
    procedure   ParseForNode1;
    procedure   ParseForNode2;
    procedure   ParseForAttribute1;
    procedure   ParseForAttribute2;
    procedure   ParseForAttribute3;
  end ;

type
  TClockTick = int64;

procedure RegisterTests ;
function GetTickCount1: int64;

implementation
uses
   tiDUnitDependencies
  ,SysUtils
  ,tiUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

const

  cWithResCharsXML    = 'yet &another <test> "string''ie"' ;
  cWithoutResCharsXML = 'yet &amp;another &lt;test&gt; &quot;string&apos;ie&quot;' ;

  cWithResCharsCSV    = 'yet, another'#13#10'test,'#10'string'#13 ;
  cWithoutResCharsCSV = 'yet&com; another&cr;&lf;test&com;&lf;string&cr;' ;

  cWithResCharsTAB    = 'yet'#9' another'#13#10'test'#9''#10'string'#13 ;
  cWithoutResCharsTAB = 'yet&tab; another&cr;&lf;test&tab;&lf;string&cr;' ;

  // Set your performance testing values here
  cStrReplicateLongString =  10; // What size string to work with
  cRepeatCount            = 100; // How many times to repeat the tests
  cImprovement            =   2; // How much faster than PH do you want to go


procedure RegisterTests ;
begin
  RegisterNonPersistentTest(TTestTIXML);
  RegisterNonPersistentTest(TTestTIXMLParser);
end;


function GetTickCount1: int64;
begin
  {$IFDEF MSWINDOWS}
  Windows.QueryPerformanceCounter(Result);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := tiUtils.tiGetTickCount; 
  {$ENDIF}
end;


{ TTestTIXML }

procedure TTestTIXML.DateTimeAsXMLString;
var
  lDate : TDateTime ;
  ls : string ;
begin
  lDate := EncodeDate( 2004, 01, 01 ) + EncodeTime( 13, 32, 45, 01 ) ;
  ls := tiDateTimeAsXMLString(lDate);
  CheckEquals( '01/01/2004 13:32:45:001', ls );
end;

procedure TTestTIXML.XMLDataTag;
begin
  CheckEquals( '<mytag somedata/>', tiXMLDataTag('mytag', 'somedata')) ;
end;

procedure TTestTIXML.XMLInsertReservedCharsCSV;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithoutResCharsCSV ;
  lAfter    := FXMLRCTrans.InsertReserved( rcCSV, lBefore ) ;
  lExpected := cWithResCharsCSV ;
  CheckEquals( lExpected, lAfter ) ;
end;


procedure TTestTIXML.XMLInsertReservedCharsTAB;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithoutResCharsTAB ;
  lAfter    := FXMLRCTrans.InsertReserved( rcTab, lBefore) ;
  lExpected := cWithResCharsTAB ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLInsertReservedCharsXML;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithoutResCharsXML ;
  lAfter    := FXMLRCTrans.InsertReserved( rcXML, lBefore ) ;
  lExpected   := cWithResCharsXML ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLRemoveReservedCharsCSV;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithResCharsCSV ;
  lAfter    := FXMLRCTrans.RemoveReserved( rcCSV, lBefore ) ;
  lExpected := cWithoutResCharsCSV ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLRemoveReservedCharsTAB;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithResCharsTAB ;
  lAfter    := FXMLRCTrans.RemoveReserved( rcTab, lBefore ) ;
  lExpected := cWithoutResCharsTAB ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLRemoveReservedCharsXML;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := cWithResCharsXML ;
  lAfter    := FXMLRCTrans.RemoveReserved( rcXML, lBefore ) ;
  lExpected := cWithoutResCharsXML ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLStringToDateTime;
var
  lDate : TDateTime ;
begin
  lDate := tiXMLStringToDateTime( '01/01/2004 13:32:45:001' ) ;
  CheckEquals( EncodeDate( 2004, 01, 01 ) + EncodeTime( 13, 32, 45, 01 ), lDate, 0.00001 ) ;
end;

procedure TTestTIXML.XMLTag;
begin
  CheckEquals( '<mytag>', tiXMLTag('mytag')) ;
end;

procedure TTestTIXML.XMLTagEnd;
begin
  CheckEquals( '</mytag>', tiXMLTagEnd('mytag')) ;
end;

procedure TTestTIXML.XMLInsertReservedCharsXML2;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := 'X&apos;d' ;
  lAfter    := FXMLRCTrans.InsertReserved( rcXML, lBefore ) ;
  lExpected   := 'X''d' ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.XMLRemoveReservedCharsXML2;
var
  lBefore   : string ;
  lAfter    : string ;
  lExpected : string ;
begin
  lBefore   := 'X''d' ;
  lAfter    := FXMLRCTrans.RemoveReserved( rcXML, lBefore ) ;
  lExpected := 'X&apos;d' ;
  CheckEquals( lExpected, lAfter ) ;
end;

procedure TTestTIXML.TestTIReplicate1;
var
  ls, ls1 : string ;
begin
  CheckEquals( 'x',          tiReplicate1( 'x', 1 ), 'Failed on 1' ) ;
  CheckEquals( 'xx',         tiReplicate1( 'x', 2 ), 'Failed on 2' ) ;
  CheckEquals( 'xxxxxxxxxx', tiReplicate1( 'x', 10 ), 'Failed on 3' ) ;
  CheckEquals( '12',         tiReplicate1( '12', 1 ), 'Failed on 4' ) ;
  CheckEquals( '1212',       tiReplicate1( '12', 2 ), 'Failed on 5' ) ;
  CheckEquals( '121212',     tiReplicate1( '12', 3 ), 'Failed on 6' ) ;
  CheckEquals( '12121212',   tiReplicate1( '12', 4 ), 'Failed on 7' ) ;

  ls := tiReplicate(cWithResCharsXML, cStrReplicateLongString);
  ls1 := tiReplicate1(cWithResCharsXML, cStrReplicateLongString);
  CheckEquals(ls, ls1, cWithResCharsXML);

  ls := tiReplicate(cWithResCharsCSV, cStrReplicateLongString);
  ls1 := tiReplicate1(cWithResCharsCSV, cStrReplicateLongString);
  CheckEquals(ls, ls1, cWithResCharsCSV );

  ls := tiReplicate(cWithResCharsTAB, cStrReplicateLongString);
  ls1 := tiReplicate1(cWithResCharsTAB, cStrReplicateLongString);
  CheckEquals(ls, ls1, cWithResCharsTAB);

end;

constructor TTestTIXML.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  FXMLRCTrans := CreateXMLReservedCharsTranslator;
end;

procedure TTestTIXML.tiParseForSingleAttribute;
const
  cXML = '<mynode attr1="result1" attr2="result2" />';
var
  lResult : string ;
begin
  lResult := tiXML.tiParseForSingleAttribute(cXML, 'attr1');
  CheckEquals('result1', lResult, '#1');
  lResult := tiXML.tiParseForSingleAttribute(cXML, 'attr2');
  CheckEquals('result2', lResult, '#2');
end;

procedure TTestTIXML.tiParseForSingleNode;
const
  cXML  = '<mydata attr1="test1"/>';
var
  lResult : string;
begin
  lResult := tiXML.tiParseForSingleNode(cXML, 'mydata');
  CheckEquals( 'attr1="test1"', lResult ) ;
end;

procedure TTestTIXML.tiParseForSingleNodeAttribute;
const
  cXML = '<mynode1 attr1="result11" attr2="result12" />';
var
  lResult : string;
begin
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode1', 'attr1');
  CheckEquals('result11', lResult, '#1');
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode1', 'attr2');
  CheckEquals('result12', lResult, '#2');
end;

procedure TTestTIXML.tiParseForSingleNode1;
const
  cXML  = '<mydata attr1="test1" />';
var
  lResult : string;
begin
  lResult := tiXML.tiParseForSingleNode(cXML, 'mydata');
  CheckEquals( 'attr1="test1"', lResult ) ;
end;

procedure TTestTIXML.tiParseForSingleNodeAttribute1;
const
  cXML = '<mynode1 attr1="result11" attr2="result12" />' +
         '<mynode2 attr1="result21" attr2="result22" />';
var
  lResult : string;
begin
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode1', 'attr1');
  CheckEquals('result11', lResult, '#1');
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode1', 'attr2');
  CheckEquals('result12', lResult, '#2');
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode2', 'attr1');
  CheckEquals('result21', lResult, '#3');
  lResult := tiXML.tiParseForSingleNodeAttribute(cXML, 'mynode2', 'attr2');
  CheckEquals('result22', lResult, '#4');
end;

{ TTestTIXMLParser }

constructor TTestTIXMLParser.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  FAttributes := TStringList.Create ;
end;

destructor TTestTIXMLParser.Destroy;
begin
  FAttributes.Free ;
  inherited;
end;

procedure TTestTIXMLParser.OnAttribute(const pName, pValue: string);
begin
  FAttributes.Values[pName] := pValue ;
end;

procedure TTestTIXMLParser.OnNode(const pStr: string);
begin
  FNode := pStr ;
end;

procedure TTestTIXMLParser.ParseForAttribute1;
var
  lXML : string ;
  lXMLParser : TtiXMLParser;
begin
  lXML := '<mynode attr1="result1" attr2="result2" />';
  lXMLParser := TtiXMLParser.Create;
  try
    lXMLParser.ParseForAttributes(lXML, {$IFDEF FPC}@{$ENDIF}OnAttribute);
    CheckEquals(2, FAttributes.Count, 'FAttributes.Count');
    CheckEquals('result1', FAttributes.Values['attr1'], 'attr1');
    CheckEquals('result2', FAttributes.Values['attr2'], 'attr1');
  finally
    lXMLParser.Free;
  end;
end;

procedure TTestTIXMLParser.ParseForAttribute2;
var
  lXML : string ;
  lXMLParser : TtiXMLParser;
begin
  lXML := '<mynode> attr1="result1" attr2="result2" </mynode>';
  lXMLParser := TtiXMLParser.Create;
  try
    lXMLParser.ParseForAttributes(lXML, {$IFDEF FPC}@{$ENDIF}OnAttribute);
    CheckEquals(2, FAttributes.Count, 'FAttributes.Count');
    CheckEquals('result1', FAttributes.Values['attr1'], 'attr1');
    CheckEquals('result2', FAttributes.Values['attr2'], 'attr1');
  finally
    lXMLParser.Free;
  end;
end;

procedure TTestTIXMLParser.ParseForAttribute3;
var
  lXML : string ;
  lXMLParser : TtiXMLParser;
begin
  lXML := 'attr1="result1" attr2="result2"';
  lXMLParser := TtiXMLParser.Create;
  try
    lXMLParser.ParseForAttributes(lXML, {$IFDEF FPC}@{$ENDIF}OnAttribute);
    CheckEquals(2, FAttributes.Count, 'FAttributes.Count');
    CheckEquals('result1', FAttributes.Values['attr1'], 'attr1');
    CheckEquals('result2', FAttributes.Values['attr2'], 'attr1');
  finally
    lXMLParser.Free;
  end;
end;

procedure TTestTIXMLParser.ParseForNode1;
var
  lXML : string ;
  lNode : string ;
  lXMLParser : TtiXMLParser;
begin
  lNode := 'attr1="test"';
  lXML  := '<mynode ' + lNode + ' />';
  lXMLParser := TtiXMLParser.Create;
  try
    lXMLParser.ParseForNode('<myxml>' + lXML + '</myxml>', '<mynode', '/>', {$IFDEF FPC}@{$ENDIF}OnNode);
    CheckEquals(lNode, FNode);
  finally
    lXMLParser.Free;
  end;
end;

procedure TTestTIXMLParser.ParseForNode2;
var
  lXML : string ;
  lNode : string ;
  lXMLParser : TtiXMLParser;
begin
  lNode := 'attr1="test"';
  lXML  := '<mynode>' + lNode + '</mynode>';
  lXMLParser := TtiXMLParser.Create;
  try
    lXMLParser.ParseForNode('<myxml> ' + lXML + ' </myxml>', '<mynode>', '</mynode>', {$IFDEF FPC}@{$ENDIF}OnNode);
    CheckEquals(lNode, FNode);
  finally
    lXMLParser.Free;
  end;
end;

procedure TTestTIXMLParser.SetUp;
begin
  inherited;
  FNode := '';
  FAttributes.Clear;
end;

function TTestTIXML.tiReplicate1( const pStrValue : string ; pRepCount : Word ) : string ;
var
  pResult, pValue: PChar;
  lenValue: cardinal;

begin
  if (pRepCount = 0) or (Pointer(pStrValue) = nil) then
    exit;
  lenValue := Length(pStrValue);
  SetLength(Result, lenValue * pRepCount);
  pResult := Pointer(Result);
  pValue := Pointer(pStrValue);

  while pRepCount <> 0 do
  begin
    Move(pValue^, pResult^, lenValue);
    Inc(pResult, lenValue);
    Dec(pRepCount);
  end;
end;


end.
