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

// Uncomment to run performance tests on XML parsre
//{$DEFINE PERFORMANCE_TESTS}

unit tiXMLToTIDataSet_TST;

interface
uses
  TestFrameWork
  ,tiQuery
  ,Classes
  ,tiPersistAbs_TST
  ,tiXMLToTIDataSet
//  ,tiXMLToTIDataSet1
  ,tiDataSet_BOM
  ,tiXML
  ;

type

  // This makes public some protected methods in TXMLToDataSetReader so
  // they can be tested
  TXMLToDataSetReaderTest = class( TXMLToDataSetReader )
  private
    FTableName: string;
    FFieldName: string;
    FFieldKind: string;
    FFieldSize: string;
    FAttributes: TStringList;
  public
    constructor Create ;
    destructor  Destroy; override ;
    property    XML;
    property    State;
    property    DataSets;
    property    Pos;
    property    Len;
    procedure   DoAddTable(const pTableName : string ); override ;
    procedure   DoAddField(const pFieldName, pFieldKind, pFieldSize : string ); override ;
    procedure   DoAddRow; override ;
    procedure   DoAddCell(const pFieldName: string; pFieldValue: string); override;
    procedure   Next; override ;
    function    RestOfString: string ;
    function    NextChars(pNoOfChars: Cardinal): string;
    property    Attributes : TStringList read FAttributes write FAttributes;

  end ;

  TTestTIXMLtoTIDataSet = class( TtiTestCase )
  private
    FXMLTags : TtiXMLTags ;

    function  MakeXMLDatabase(const pTableString : string): string;
    function  MakeXMLDatabaseWithMetaData(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: Integer): string;
    function  MakeXMLDatabaseWithData(
                const pFieldName: string;pFieldKind: TtiQueryFieldKind;
                pFieldSize: Integer; const pValue : string;
                pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;

    procedure CompoundDataToDataSets(const pDataSets: TtiDataSets;
                                     pDataSetCount: Integer; pRowCount: Integer;
                                     pIncludeBinField : Boolean);

    function  MakeXMLDatabaseCompoundWithReadWrite(
                pDataSetCount: Integer; pRowCount: Integer;
                pIncludeBinField : Boolean ;
                pXMLFieldNameStyle: TtiXMLFieldNameStyle): string ;
//    function  MakeXMLDatabaseCompoundWithReadWriteOld(pDataSetCount: Integer; pRowCount: integer; pIncludeBinField : Boolean ): string ;

//    procedure CheckXMLDatabaseWithCompoundDataOld(const pValue: string; pDataSetCount: Integer; pRowCount: integer; pIncludeBinField : Boolean );
    procedure CheckXMLDatabaseWithCompoundData(const pValue: string; pDataSetCount, pRowCount: Integer; pIncludeBinField: Boolean );

    procedure DoXMLToDataSetMetaDataBuffered(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: Integer);
    procedure DoXMLToDataSetDataBuffered(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: integer; const pValue : string);

    procedure DoXMLDBParserMetaData(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: integer);
    procedure DoXMLDBParser(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: integer; const pValue : string);

    procedure DoXMLToDataSetMetaDataWriteOnly(
      const pFieldName: string;pFieldKind: TtiQueryFieldKind;
      pFieldSize: Integer; pXMLFieldNameStyle : TtiXMLFieldNameStyle);

    procedure DoXMLToDataSetDataWriteOnly(
      const pFieldName: string;pFieldKind: TtiQueryFieldKind;
      pFieldSize: integer; const pValue : string;
      pXMLFieldNameStyle : TtiXMLFieldNameStyle);

    {$IFDEF PERFORMANCE_TESTS}
    procedure DoXMLToDataSetWriterPerformanceAbsolute(pXMLFieldNameStyle : TtiXMLFieldNameStyle);
    procedure DoXMLToDataSetReaderPerformanceAbsolute(pXMLFieldNameStyle : TtiXMLFieldNameStyle);
    {$ENDIF}

  public
    constructor Create(MethodName : string); override ;
    destructor  Destroy ; override ;
  published

    procedure XMLDBParser_StateChangeTableOptXMLSizeOff;
    procedure XMLDBParser_StateChangeTableOptXMLSizeOn;

    procedure XMLToDataSetEmptyFile_Read;
    procedure XMLToDataSetEmptyFile_Write;

    procedure XMLToDataSetMetaDataBuffered_String;
    procedure XMLToDataSetMetaDataBuffered_Integer;
    procedure XMLToDataSetMetaDataBuffered_Float;
    procedure XMLToDataSetMetaDataBuffered_DateTime;
    procedure XMLToDataSetMetaDataBuffered_Boolean;
    procedure XMLToDataSetMetaDataBuffered_Binary;

    procedure XMLToDataSetDataBuffered_String;
    procedure XMLToDataSetDataBuffered_Integer;
    procedure XMLToDataSetDataBuffered_Float;
    procedure XMLToDataSetDataBuffered_DateTime;
    procedure XMLToDataSetDataBuffered_Boolean;
    procedure XMLToDataSetDataBuffered_Binary;

    procedure XMLToDataSetCompound;
    procedure XMLToDataSetCompoundInvalidXML;

    procedure XMLDBParserMetaData_String;
    procedure XMLDBParserMetaData_Integer;
    procedure XMLDBParserMetaData_Float;
    procedure XMLDBParserMetaData_DateTime;
    procedure XMLDBParserMetaData_Boolean;
    procedure XMLDBParserMetaData_Binary;

    procedure XMLDBParser_String;
    procedure XMLDBParser_Integer;
    procedure XMLDBParser_Float;
    procedure XMLDBParser_DateTime;
    procedure XMLDBParser_Boolean;
    procedure XMLDBParser_Binary;

//    procedure XMLToDataSetCompoundOld;

    procedure XMLToDataSetMetaDataWriteOnly_Empty;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_String;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_Integer;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_Float;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_DateTime;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_Boolean;
    procedure XMLToDataSetMetaDataWriteOnlyFSString_Binary;

    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_Binary;
    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_Boolean;
    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_DateTime;
    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_Float;
    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_Integer;
    procedure XMLToDataSetMetaDataWriteOnlyFSInteger_String;

    procedure XMLToDataSetDataWriteOnlyFSString_String;
    procedure XMLToDataSetDataWriteOnlyFSString_Integer;
    procedure XMLToDataSetDataWriteOnlyFSString_Float;
    procedure XMLToDataSetDataWriteOnlyFSString_DateTime;
    procedure XMLToDataSetDataWriteOnlyFSString_Boolean;
    procedure XMLToDataSetDataWriteOnlyFSString_Binary;

    procedure XMLToDataSetDataWriteOnlyFSInteger_Binary;
    procedure XMLToDataSetDataWriteOnlyFSInteger_Boolean;
    procedure XMLToDataSetDataWriteOnlyFSInteger_DateTime;
    procedure XMLToDataSetDataWriteOnlyFSInteger_Float;
    procedure XMLToDataSetDataWriteOnlyFSInteger_Integer;
    procedure XMLToDataSetDataWriteOnlyFSInteger_String;

//    procedure XMLToDataSetWriterCompound;

    {$IFDEF PERFORMANCE_TESTS}
    procedure XMLToDataSetWriterPerformanceFSStringAbsolute;
    procedure XMLToDataSetReaderPerformanceFSStringAbsolute;
    procedure XMLToDataSetWriterPerformanceFSIntegerAbsolute;
    procedure XMLToDataSetReaderPerformanceFSIntegerAbsolute;

    procedure XMLToDataSetWriterPerformanceImprovement;
    procedure XMLToDataSetReaderPerformanceImprovement;
    {$ENDIF}

  end ;

procedure RegisterTests ;

implementation
uses
   tiDUnitDependencies
  ,SysUtils
  ,tiUtils
  ,Windows
  ,tiDialogs // Debugging
  ,tiDUnitUtils
  ,Dialogs
  ;

const

  cWithResCharsXML    = 'yet &another <test> "string''ie"' ;
  cWithoutResCharsXML = 'yet &amp;another &lt;test&gt; &quot;string&apos;ie&quot;' ;

  cWithResCharsCSV    = 'yet, another'#13#10'test,'#10'string'#13 ;
  cWithoutResCharsCSV = 'yet&com; another&cr;&lf;test&com;&lf;string&cr;' ;

  cWithResCharsTAB    = 'yet'#9' another'#13#10'test'#9''#10'string'#13 ;
  cWithoutResCharsTAB = 'yet&tab; another&cr;&lf;test&tab;&lf;string&cr;' ;

  cRowCount               = 20000;
  cImprovement            = 10;
  cMSPer1000Rows          = 20;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIXMLtoTIDataSet.Suite );
end;

{
function TestXMLRemoveReservedCharsBenchMark(const ps: string ; const aEscChars : array of TtiXMLReservedChar): string;
var
  i : integer ;
begin
  result := ps ;
  for i := Low(aEscChars) to High(aEscChars) do
    result := tiStrTran( result, aEscChars[i].ResChar, aEscChars[i].EscWith ) ;
end;
}

{
function TestXMLInsertReservedCharsBenchMark(const ps: string ; const aEscChars : array of TtiXMLReservedChar): string;
var
  i : integer ;
begin
  result := ps ;
  for i := High(aEscChars) downto Low(aEscChars) do
    result := tiStrTran( result, aEscChars[i].EscWith, aEscChars[i].ResChar ) ;
end ;
}


procedure TTestTIXMLtoTIDataSet.XMLToDataSetEmptyFile_Read;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lResult          : string ;
  lTarget          : string ;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lResult := lXMLToTIDataSets.AsString ;
    lTarget := MakeXMLDatabase('');
    CheckEquals(lTarget, lResult);
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetEmptyFile_Write;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  ls               : string ;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    ls := MakeXMLDatabase('');
    lDataSets := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := ls;
      CheckEquals(0, lDataSets.Count);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Integer;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkInteger, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_String;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkString, 10 );
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetMetaDataBuffered(
  const pFieldName : string ; pFieldKind : TtiQueryFieldKind ;
  pFieldSize : Integer) ;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  lDataSet         : TtiDataSet ;
  lResult          : string ;
  lAsString        : string ;
begin
  lAsString := MakeXMLDatabaseWithMetaData(pFieldName, pFieldKind, pFieldSize );

  // Test Read
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lDataSet := lDataSets.AddInstance('test');
      lDataSet.Fields.AddInstance(pFieldName, pFieldKind, pFieldSize );
      lResult := lXMLToTIDataSets.AsString ;
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := lAsString ;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count' ) ;
      CheckEquals(0, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[pFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Binary;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkBinary, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Boolean;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkLogical, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_DateTime;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkDateTime, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Float;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkFloat, 0 );
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetDataBuffered(const pFieldName: string;pFieldKind: TtiQueryFieldKind; pFieldSize: integer;const pValue : string);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  lDataSet         : TtiDataSet ;
  lDataSetRow      : TtiDataSetRow ;
  lResult          : string ;
  lAsString        : string ;
begin
  lAsString := MakeXMLDatabaseWithData(pFieldName, pFieldKind, pFieldSize, pValue, xfnsString );

  // Test Read
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lDataSet := lDataSets.AddInstance('test');
      lDataSet.Fields.AddInstance(pFieldName, pFieldKind, pFieldSize );
      lDataSetRow := lDataSet.AddInstance;
      lDataSetRow.FindByFieldName(pFieldName).ValueAsString := pValue ;
      lResult := lXMLToTIDataSets.AsString ;
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := lAsString ;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[pFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pValue, lDataSets.Items[0].Items[0].FindByFieldName(pFieldName).ValueAsString, 'lDataSets.Items[0].Items[0].FindByFieldName(pFieldName).ValueAsString');
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Binary;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, 'DATA' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Boolean;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, 'TRUE' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_DateTime;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '01/01/2004 10:11:12:13' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Float;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '123.456' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Integer;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '123' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_String;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 10, 'a test string' );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetCompound;
var
  ls       : string ;
begin
  ls := MakeXMLDatabaseCompoundWithReadWrite( 10, 20, True, xfnsString ) ;
  CheckXMLDatabaseWithCompoundData(ls, 10, 20, true);
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseWithMetaData(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: Integer): string;
var
  ls : string ;
begin
  ls :=
    '<'  + FXMLTags.Table + ' ' + FXMLTags.TableName + '="test"' + '>' +
    '<'  + FXMLTags.Fields + '>' +
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + pFieldName + '" '
                              + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[pFieldKind]) + '" '
                              + FXMLTags.FieldSize + '="' + IntToStr( pFieldSize ) + '"/>' +
    '</' + FXMLTags.Fields + '>' +
    '<'  + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Table + '>';
  Result := MakeXMLDatabase(ls);
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseWithData(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: Integer; const pValue : string;
  pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;
var
  ls : string ;
  lFieldName: string;
begin

  case pXMLFieldNameStyle of
    xfnsString : lFieldName := pFieldName ;
    xfnsInteger: lFieldName := tiEncodeWordBase26(0);
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end ;

  ls :=
    '<'  + FXMLTags.Table + ' ' + FXMLTags.TableName + '="test"' + '>' +
    '<'  + FXMLTags.Fields + '>' +
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + pFieldName + '" '
                              + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[pFieldKind]) + '" '
                              + FXMLTags.FieldSize + '="' + IntToStr( pFieldSize ) + '"/>' +
    '</' + FXMLTags.Fields + '>' +
    '<'  + FXMLTags.Rows + '>' +
    '<' + FXMLTags.Row   + ' ' + lFieldName + '="' + pValue + '"/>' +
    '</' + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Table + '>' ;

  Result := MakeXMLDatabase(ls);
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetDataWriteOnly(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; const pValue: string;
  pXMLFieldNameStyle : TtiXMLFieldNameStyle);
var
  lXMLWriter : TtiDataSetToXMLWriter ;
  lResult          : string ;
  lAsString        : string ;
begin
  lAsString :=
    MakeXMLDatabaseWithData(pFieldName, pFieldKind, pFieldSize, pValue, pXMLFieldNameStyle );

  // Test Read
  lXMLWriter := TtiDataSetToXMLWriter.Create ;
  try
    lXMLWriter.XMLFieldNameStyle := pXMLFieldNameStyle ;
    lXMLWriter.AddTable('test');
    lXMLWriter.AddField(pFieldName, pFieldKind, pFieldSize );
    lXMLWriter.AddRow;
    lXMLWriter.AddCellAsString(pFieldName, pValue);
    lResult := lXMLWriter.AsString ;
    CheckEquals(lAsString, lResult);
  finally
    lXMLWriter.Free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetMetaDataWriteOnly(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; pXMLFieldNameStyle : TtiXMLFieldNameStyle);
var
  lXMLWriter : TtiDataSetToXMLWriter ;
  lResult    : string ;
  lAsString  : string ;
begin
  lAsString := MakeXMLDatabaseWithMetaData(pFieldName, pFieldKind, pFieldSize);

  lXMLWriter := TtiDataSetToXMLWriter.Create ;
  try
    lXMLWriter.XMLFieldNameStyle := pXMLFieldNameStyle;
    lXMLWriter.AddTable('test');
    lXMLWriter.AddField(pFieldName, pFieldKind, pFieldSize );
    lResult := lXMLWriter.AsString ;
    CheckEquals(lAsString, lResult);
  finally
    lXMLWriter.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Binary;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkBinary, 0, 'DATA', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Boolean;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkLogical, 0, 'TRUE', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_DateTime;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkDateTime, 0, '01/01/2004 10:11:12:13', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Float;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkFloat, 0, '123.456', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Integer;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkInteger, 0, '123', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_String;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkString, 10, 'a test string', xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Binary;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkBinary, 0, 'DATA', xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Boolean;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkLogical, 0, 'TRUE', xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_DateTime;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkDateTime, 0, '01/01/2004 10:11:12:13', xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Float;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkFloat, 0, '123.456', xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Integer;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkInteger, 0, '123', xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_String;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkString, 10, 'a test string', xfnsInteger );
end;


procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Binary;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkBinary, 0, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Boolean;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkLogical, 0, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_DateTime;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkDateTime, 0, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Float;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkFloat, 0, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Integer;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkInteger, 0, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_String;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkString, 10, xfnsString );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Binary;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkBinary, 0, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Boolean;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkLogical, 0, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_DateTime;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkDateTime, 0, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Float;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkFloat, 0, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Integer;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkInteger, 0, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_String;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkString, 10, xfnsInteger );
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnly_Empty;
var
  lXMLWriter: TtiDataSetToXMLWriter;
  lTarget: string ;
  lResult: string ;
begin
  lXMLWriter := TtiDataSetToXMLWriter.Create;
  try
    lResult := lXMLWriter.AsString;
    lTarget := MakeXMLDatabase('');
    CheckEquals(lTarget, lResult);
  finally
    lXMLWriter.Free;
  end;
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabase(
  const pTableString: string): string;
begin
  Result :=
    FXMLTags.DocHeader +
    '<' + FXMLTags.DocData + '>' +
    FXMLTags.DatabaseDetails +
    '<' + FXMLTags.Tables +'>' +
    pTableString +
    '</' + FXMLTags.Tables + '>' +
    '</' + FXMLTags.DocData + '>' ;
end;

{
procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterCompound;
var
  ls : string ;
begin
  ls := MakeXMLDatabaseCompoundWithReadWriteOld(10, 20, true);
  CheckXMLDatabaseWithCompoundData(ls, 10, 20, true);
end;
}

procedure TTestTIXMLtoTIDataSet.CheckXMLDatabaseWithCompoundData(
  const pValue: string; pDataSetCount : Integer ;
  pRowCount : Integer;
  pIncludeBinField : Boolean );
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  lDataSet         : TtiDataSet ;
  lDataSetRow      : TtiDataSetRow ;
  lAsString2       : string ;
  lDataSetCount    : integer ;
  lRowCount        : integer ;
  lVal             : integer ;
  lStream1         : TStringStream;
  lStream2         : TMemoryStream;
begin

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := pValue ;
      CheckEquals(pDataSetCount, lDataSets.Count, 'lDataSets.Count' );
      for lDataSetCount := 0 to pDataSetCount - 1 do
      begin
        lDataSet := lDataSets.Items[lDataSetCount] ;
        CheckEquals('dataset' + IntToStr(lDataSetCount), lDataSet.Name, 'lDataSet.Name' ) ;
        if pIncludeBinField then
          CheckEquals(6, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count' )
        else
          CheckEquals(5, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count' );
        CheckEquals('string_field',  lDataSet.Fields.Items[0].Name);
        CheckEquals('integer_field', lDataSet.Fields.Items[1].Name);
        CheckEquals('float_field',   lDataSet.Fields.Items[2].Name);
        CheckEquals('boolean_field', lDataSet.Fields.Items[3].Name);
        CheckEquals('date_field',    lDataSet.Fields.Items[4].Name);
        if pIncludeBinField then
          CheckEquals('bin_field',     lDataSet.Fields.Items[5].Name);

        CheckEquals(10, lDataSet.Fields.Items[0].Width);
        CheckEquals( 0, lDataSet.Fields.Items[1].Width);
        CheckEquals( 0, lDataSet.Fields.Items[2].Width);
        CheckEquals( 0, lDataSet.Fields.Items[3].Width);
        CheckEquals( 0, lDataSet.Fields.Items[4].Width);
        if pIncludeBinField then
          CheckEquals( 0, lDataSet.Fields.Items[5].Width);

        Check(qfkString   = lDataSet.Fields.Items[0].Kind, 'Kind <> qfkString');
        Check(qfkInteger  = lDataSet.Fields.Items[1].Kind, 'Kind <> qfkInteger');
        Check(qfkFloat    = lDataSet.Fields.Items[2].Kind, 'Kind <> qfkFloat');
        Check(qfkLogical  = lDataSet.Fields.Items[3].Kind, 'Kind <> qfkLogical');
        Check(qfkDateTime = lDataSet.Fields.Items[4].Kind, 'Kind <> qfkDateTime');
        if pIncludeBinField then
          Check(qfkBinary   = lDataSet.Fields.Items[5].Kind, 'Kind <> qfkBinary');

        CheckEquals( pRowCount, lDataSet.Count, 'lDataSet.Count' ) ;
        for lRowCount := 0 to pRowCount - 1 do
        begin
          lVal := (lDataSetCount+1)*1000+lRowCount ;
          lDataSetRow := lDataSet.Items[lRowCount];
          CheckEquals( tstIntToStr(lVal), lDataSetRow.FindByFieldName('string_field').ValueAsString, 'string_field' );
          CheckEquals( lVal, lDataSetRow.FindByFieldName('integer_field').ValueAsInteger, 'integer_field' );
          CheckEquals( tstIntToFloat(lVal), lDataSetRow.FindByFieldName('float_field').ValueAsFloat, 'float_field' );
          CheckEquals( tstIntToBool(lVal), lDataSetRow.FindByFieldName('boolean_field').ValueAsBool, 'boolean_field' );
          CheckEquals( tstIntToDateTime(lVal), lDataSetRow.FindByFieldName('date_field').ValueAsDateTime, 5, 'date_field' );
          if pIncludeBinField then
          begin
            lStream1 := TStringStream.Create(LongString);
            try
              lStream2 := TMemoryStream.Create;
              try
                lDataSetRow.FindByFieldName('bin_field').AssignToStream(lStream2);
                CheckStreamContentsSame( lStream2, lStream1 ) ;
              finally
                lStream2.Free;
              end;
            finally
              lStream1.Free;
            end;
          end;
        end;
      end ;
      lAsString2 := lXMLToTIDataSEts.AsString;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
  CheckEquals(pValue,lAsString2);
end;

{
procedure TTestTIXMLtoTIDataSet.CheckXMLDatabaseWithCompoundDataOld(const pValue: string; pDataSetCount : Integer ; pRowCount : Integer; pIncludeBinField : Boolean  );
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriterOld ;
  lDataSets        : TtiDataSets ;
  lDataSet         : TtiDataSet ;
  lDataSetRow      : TtiDataSetRow ;
  lAsString2       : string ;
  lDataSetCount    : integer ;
  lRowCount        : integer ;
  lVal             : integer ;
  lStream1         : TStringStream;
  lStream2         : TMemoryStream;
begin

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriterOld.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := pValue ;
      CheckEquals(pDataSetCount, lDataSets.Count, 'lDataSets.Count' );
      for lDataSetCount := 0 to pDataSetCount - 1 do
      begin
        lDataSet := lDataSets.Items[lDataSetCount] ;
        CheckEquals('dataset' + IntToStr(lDataSetCount), lDataSet.Name, 'lDataSet.Name' ) ;
        if pIncludeBinField then
          CheckEquals(6, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count' )
        else
          CheckEquals(5, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count' );
        CheckEquals('string_field',  lDataSet.Fields.Items[0].Name);
        CheckEquals('integer_field', lDataSet.Fields.Items[1].Name);
        CheckEquals('float_field',   lDataSet.Fields.Items[2].Name);
        CheckEquals('boolean_field', lDataSet.Fields.Items[3].Name);
        CheckEquals('date_field',    lDataSet.Fields.Items[4].Name);
        if pIncludeBinField then
          CheckEquals('bin_field',     lDataSet.Fields.Items[5].Name);

        CheckEquals(10, lDataSet.Fields.Items[0].Width);
        CheckEquals( 0, lDataSet.Fields.Items[1].Width);
        CheckEquals( 0, lDataSet.Fields.Items[2].Width);
        CheckEquals( 0, lDataSet.Fields.Items[3].Width);
        CheckEquals( 0, lDataSet.Fields.Items[4].Width);
        if pIncludeBinField then
          CheckEquals( 0, lDataSet.Fields.Items[5].Width);

        Check(qfkString   = lDataSet.Fields.Items[0].Kind, 'Kind <> qfkString');
        Check(qfkInteger  = lDataSet.Fields.Items[1].Kind, 'Kind <> qfkInteger');
        Check(qfkFloat    = lDataSet.Fields.Items[2].Kind, 'Kind <> qfkFloat');
        Check(qfkLogical  = lDataSet.Fields.Items[3].Kind, 'Kind <> qfkLogical');
        Check(qfkDateTime = lDataSet.Fields.Items[4].Kind, 'Kind <> qfkDateTime');
        if pIncludeBinField then
          Check(qfkBinary   = lDataSet.Fields.Items[5].Kind, 'Kind <> qfkBinary');

        CheckEquals( pRowCount, lDataSet.Count, 'lDataSet.Count' ) ;
        for lRowCount := 0 to pRowCount - 1 do
        begin
          lVal := (lDataSetCount+1)*1000+lRowCount ;
          lDataSetRow := lDataSet.Items[lRowCount];
          CheckEquals( tstIntToStr(lVal), lDataSetRow.FindByFieldName('string_field').ValueAsString, 'string_field' );
          CheckEquals( lVal, lDataSetRow.FindByFieldName('integer_field').ValueAsInteger, 'integer_field' );
          CheckEquals( tstIntToFloat(lVal), lDataSetRow.FindByFieldName('float_field').ValueAsFloat, 'float_field' );
          CheckEquals( tstIntToBool(lVal), lDataSetRow.FindByFieldName('boolean_field').ValueAsBool, 'boolean_field' );
          CheckEquals( tstIntToDateTime(lVal), lDataSetRow.FindByFieldName('date_field').ValueAsDateTime, 5, 'date_field' );
          if pIncludeBinField then
          begin
            lStream1 := TStringStream.Create(LongString);
            try
              lStream2 := TMemoryStream.Create;
              try
                lDataSetRow.FindByFieldName('bin_field').AssignToStream(lStream2);
                CheckStreamContentsSame( lStream2, lStream1 ) ;
              finally
                lStream2.Free;
              end;
            finally
              lStream1.Free;
            end;
          end;
        end;
      end ;
      lAsString2 := lXMLToTIDataSets.AsString ;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;

  CheckEquals( lAsString2, pValue ) ;

end;
}

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseCompoundWithReadWrite(
           pDataSetCount: Integer; pRowCount: Integer;
           pIncludeBinField : Boolean ;
           pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lXMLToTIDataSets.XMLFieldNameStyle := pXMLFieldNameStyle;
    lDataSets        := TtiDataSets.Create ;
    try
      CompoundDataToDataSets(lDataSets, pDataSetCount, pRowCount, pIncludeBinField);
      lXMLToTIDataSets.DataSets := lDataSets ;
      result := lXMLToTIDataSets.AsString ;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

{
function TTestTIXMLtoTIDataSet.MakeXMLDatabaseCompoundWithReadWriteOld(
     pDataSetCount: Integer; pRowCount: Integer;
     pIncludeBinField : Boolean ): string;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriterOld ;
  lDataSets        : TtiDataSets ;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriterOld.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      CompoundDataToDataSets(lDataSets,pDataSetCount, pRowCount, pIncludeBinField);
      lXMLToTIDataSets.DataSets := lDataSets ;
      result := lXMLToTIDataSets.AsString ;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;
}

{$IFDEF PERFORMANCE_TESTS}
procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterPerformanceImprovement;
var
  lStart : DWord ;
  lOld : DWord ;
  lNew : DWord ;
  lRatio : real ;
  lsOld: string;
  lsNew: string;
  lXMLToTIDataSetsOld  : TtiXMLToDataSetReadWriterOld ;
  lXMLToTIDataSets     : TtiXMLToDataSetReadWriter ;
  lDataSets : TtiDataSets;
begin

  lDataSets := TtiDataSets.Create;
  try
    CompoundDataToDataSets(lDataSets,1, cRowCount, False);
    lXMLToTIDataSetsOld := TtiXMLToDataSetReadWriterOld.Create ;
    try
      lXMLToTIDataSetsOld.DataSets := lDataSets ;
      lStart := GetTickCount;
      lsOld := lXMLToTIDataSetsOld.AsString ;
      lOld := GetTickCount - lStart ;
    finally
      lXMLToTIDataSetsOld.Free;
    end;

    lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
    try
      lXMLToTIDataSets.XMLFieldNameStyle := xfnsInteger;
      lXMLToTIDataSets.XMLFieldNameStyle := xfnsString;
      lXMLToTIDataSets.DataSets := lDataSets ;
      lStart := GetTickCount;
      lsNew := lXMLToTIDataSets.AsString ;
      lNew := GetTickCount - lStart ;
    finally
      lXMLToTIDataSets.Free;
    end ;

  finally
    lDataSets.Free;
  end;

  CheckEquals( lsNew, lsOld, 'XML strings not the same');

  Check( lNew < lOld, 'It got slower! Old time: ' + IntToStr( lOld ) +
         ' New time: ' + IntToStr(lNew));
  lRatio := 10000 / (lNew * 10000 div lOld);
  Check( lRatio > cImprovement, 'Not fast enough: ' + tiFloatToStr( lRatio, 2 ) + ' x faster (should be ' + IntToStr(cImprovement) + ')' ) ;

end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetReaderPerformanceImprovement;
var
  lStart : DWord ;
  lOld : DWord ;
  lNew : DWord ;
  lRatio : real ;
  ls: string;
  lXMLToTIDataSetsOld  : TtiXMLToDataSetReadWriterOld ;
  lXMLToTIDataSets     : TtiXMLToDataSetReadWriter ;
  lDataSets : TtiDataSets;
begin

  ls := MakeXMLDatabaseCompoundWithReadWrite(1, cRowCount, False, xfnsString);

  lDataSets := TtiDataSets.Create;
  try
    lXMLToTIDataSetsOld := TtiXMLToDataSetReadWriterOld.Create ;
    try
      lXMLToTIDataSetsOld.DataSets := lDataSets ;
      lStart := GetTickCount;
      lXMLToTIDataSetsOld.AsString := ls ;
      lOld := GetTickCount - lStart ;
    finally
      lXMLToTIDataSetsOld.Free;
    end;
  finally
    lDataSets.Free;
  end;


  lDataSets := TtiDataSets.Create;
  try
    lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lStart := GetTickCount;
      lXMLToTIDataSets.AsString := ls ;
      lNew := GetTickCount - lStart ;
    finally
      lXMLToTIDataSets.Free;
    end ;
  finally
    lDataSets.Free;
  end;

  Check( lNew < lOld, 'It got slower! ReadWrite time: ' + IntToStr( lOld ) +
         ' Write only time: ' + IntToStr(lNew));
  lRatio := 10000 / (lNew * 10000 div lOld);
  Check( lRatio > cImprovement, 'Not fast enough: ' + tiFloatToStr( lRatio, 2 ) + ' x faster (should be ' + IntToStr(cImprovement) + ')' ) ;

end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetReaderPerformanceFSStringAbsolute;
begin
  DoXMLToDataSetReaderPerformanceAbsolute(xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterPerformanceFSStringAbsolute;
begin
  DoXMLToDataSetWriterPerformanceAbsolute(xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetReaderPerformanceFSIntegerAbsolute;
begin
  DoXMLToDataSetReaderPerformanceAbsolute(xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterPerformanceFSIntegerAbsolute;
begin
  DoXMLToDataSetWriterPerformanceAbsolute(xfnsInteger);
end;

{$ENDIF}

procedure TTestTIXMLtoTIDataSet.DoXMLDBParserMetaData(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: integer);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  lAsString        : string ;
begin
  lAsString := MakeXMLDatabaseWithMetaData(pFieldName, pFieldKind, pFieldSize );
  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := lAsString ;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count' ) ;
      CheckEquals(0, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[pFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLDBParser(
  const pFieldName: string; pFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; const pValue: string);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
  lDataSets        : TtiDataSets ;
  lAsString        : string ;
begin
  lAsString := MakeXMLDatabaseWithData(pFieldName, pFieldKind, pFieldSize, pValue, xfnsString );
  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lDataSets        := TtiDataSets.Create ;
    try
      lXMLToTIDataSets.DataSets := lDataSets ;
      lXMLToTIDataSets.AsString := lAsString ;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count' ) ;
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[pFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count' ) ;
      CheckEquals(pValue, lDataSets.Items[0].Items[0].FindByFieldName(pFieldName).ValueAsString, 'lDataSets.Items[0].Items[0].FindByFieldName(pFieldName).ValueAsString');
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Binary;
begin
  DoXMLDBParser('test_field', qfkString, 0, 'DATA' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Boolean;
begin
  DoXMLDBParser('test_field', qfkString, 0, 'TRUE' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_DateTime;
begin
  DoXMLDBParser('test_field', qfkString, 0, '01/01/2004 10:11:12:13' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Float;
begin
  DoXMLDBParser('test_field', qfkString, 0, '123.456' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Integer;
begin
  DoXMLDBParser('test_field', qfkString, 0, '123' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_String;
begin
  DoXMLDBParser('test_field', qfkString, 10, 'a test string' );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Binary;
begin
  DoXMLDBParserMetaData('test_field', qfkBinary, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Boolean;
begin
  DoXMLDBParserMetaData('test_field', qfkLogical, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_DateTime;
begin
  DoXMLDBParserMetaData('test_field', qfkDateTime, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Float;
begin
  DoXMLDBParserMetaData('test_field', qfkFloat, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Integer;
begin
  DoXMLDBParserMetaData('test_field', qfkInteger, 0 );
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_String;
begin
  DoXMLDBParserMetaData('test_field', qfkString, 10 );
end;

{
procedure TTestTIXMLtoTIDataSet.XMLToDataSetCompoundOld;
var
  ls       : string ;
begin
  ls := MakeXMLDatabaseCompoundWithReadWrite( 1, 1, True, xfnsString ) ;
  CheckXMLDatabaseWithCompoundDataOld(ls, 1, 1, true);
end;
}

procedure TTestTIXMLtoTIDataSet.XMLDBParser_StateChangeTableOptXMLSizeOn;
const
  cTableXMLDBSizeOptOn =
  '<?xml version="1.0"?>' +
  '<a>' +
  '  <tiopf version="1.300"/>' +
  '  <b>' +
  '    <c d="table_1">' +
  '      <e>' +
  '        <f g="oid" h="string" i="36"/>' +
  '        <f g="field_1" h="string" i="50"/>' +
  '      </e>' +
  '      <j>' +
  '        <k oid="1000" field_1="value 1"/>' +
  '        <k oid="2000" field_1="value 2"/>' +
  '      </j>' +
  '    </c>' +
  '    <c d="table_2">' +
  '      <e>' +
  '        <f g="oid" h="string" i="36"/>' +
  '        <f g="field_2" h="string" i="50"/>' +
  '      </e>' +
  '      <j>' +
  '        <k oid="3000" field_2="value 3"/>' +
  '        <k oid="4000" field_2="value 4"/>' +
  '      </j>' +
  '    </c>' +
  '  </b>' +
  '</a>';
{

                   FDocData         := 'a';
                   FTables          := 'b';
                   FTable           := 'c';
                   FTableName       := 'd';
                   FFields          := 'e';
                   FField           := 'f';
                   FFieldName       := 'g';
                   FFieldKind       := 'h';
                   FFieldSize       := 'i';
                   FRows            := 'j';
                   FRow             := 'k';
                   FValue           := 'l';
}
var
  lXMLDBParser : TXMLToDataSetReaderTest;
begin
  // Nasty, fragile test. Sorry.
  lXMLDBParser := TXMLToDataSetReaderTest.Create;
  try
    lXMLDBParser.OptXMLDBSize := optDBSizeOn ;
    lXMLDBParser.XML := cTableXMLDBSizeOptOn ;

    // Scan for <tables>
    lXMLDBParser.Next ;
    CheckEquals(56, lXMLDBParser.Pos, 'Pos');
    CheckEquals('    <c d="', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    // Scan for <table>
    lXMLDBParser.Next ;
    CheckEquals(63, lXMLDBParser.Pos, 'Pos');
    CheckEquals('d="table_1', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    // Parse for table attribute (table_name=table1)
    lXMLDBParser.FTableName:= '' ;
    lXMLDBParser.Next;
    CheckEquals(74, lXMLDBParser.Pos, 'Pos');
    CheckEquals('>      <e>     ', lXMLDBParser.NextChars(15), 'NextChars');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdbsFields');
    CheckEquals('table_1',lXMLDBParser.FTableName,'FTableName <> table_1');

    lXMLDBParser.Next;
    CheckEquals(95, lXMLDBParser.Pos, 'Pos');
    CheckEquals('g="oid" h="strin', lXMLDBParser.NextChars(16), 'NextChars');
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.FFieldName := '';
    lXMLDBParser.FFieldKind := '';
    lXMLDBParser.FFieldSize := '';
    lXMLDBParser.Next;
    CheckEquals(120, lXMLDBParser.Pos, 'Pos');
    CheckEquals('/>        <f g="', lXMLDBParser.NextChars(16), 'NextChars');
    CheckEquals('oid', lXMLDBParser.FFieldName, 'FFieldName <> ''oid''');
    CheckEquals('string', lXMLDBParser.FFieldKind, 'FFieldKind <> ''string''');
    CheckEquals('36', lXMLDBParser.FFieldSize, 'FFieldSize <> ''36''');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    CheckEquals(133, lXMLDBParser.Pos, 'Pos');
    CheckEquals('g="field_1" h="strin', lXMLDBParser.NextChars(20), 'NextChars');
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.FFieldName := '';
    lXMLDBParser.FFieldKind := '';
    lXMLDBParser.FFieldSize := '';
    lXMLDBParser.Next;
    CheckEquals(162, lXMLDBParser.Pos, 'Pos');
    CheckEquals('/>      </e>     ', lXMLDBParser.NextChars(17), 'NextChars');
    CheckEquals('field_1', lXMLDBParser.FFieldName, 'FFieldName <> ''field_1''');
    CheckEquals('string',  lXMLDBParser.FFieldKind, 'FFieldKind <> ''string''');
    CheckEquals('50',      lXMLDBParser.FFieldSize, 'FFieldSize <> ''50''');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    CheckEquals(174, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals('      <j>   ', lXMLDBParser.NextChars(12), 'NextChars');

    lXMLDBParser.Next;
    CheckEquals(194, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals('oid="1000"', lXMLDBParser.NextChars(10), 'NextChars');

    lXMLDBParser.Attributes.Clear;
    lXMLDBParser.Next;
    CheckEquals(224, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals(2, lXMLDBParser.Attributes.Count, 'lXMLDBParser.Attributes.Count');
    CheckEquals('1000', lXMLDBParser.Attributes.Values['oid'], 'oid <> "1000"');
    CheckEquals('value 1', lXMLDBParser.Attributes.Values['field_1'], 'field_1 <> "value 1"');

    lXMLDBParser.Next;
    CheckEquals(235, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Attributes.Clear;
    lXMLDBParser.Next;
    CheckEquals(265, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals(2, lXMLDBParser.Attributes.Count, 'lXMLDBParser.Attributes.Count');
    CheckEquals('2000', lXMLDBParser.Attributes.Values['oid'], 'oid <> "2000"');
    CheckEquals('value 2', lXMLDBParser.Attributes.Values['field_1'], 'field_1 <> "value 2"');

    lXMLDBParser.Next;
    CheckEquals(275, lXMLDBParser.Pos, 'Pos');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdbsFields');

    lXMLDBParser.Next;
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsEnd = lXMLDBParser.State, 'State <> xdbsEnd');

  finally
    lXMLDBParser.Free;
  end;

end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_StateChangeTableOptXMLSizeOff;
const
  cTableXMLDBSizeOptOff =
  '<?xml version="1.0"?>' +
  '<xmldocdata>' +
  '  <tiopf version="1.300"/>' +
  '  <tables>' +
  '    <table table_name="table_1">' +
  '      <fields>' +
  '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
  '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
  '      </fields>' +
  '      <rows>' +
  '        <row oid="1000" field_1="value 1"/>' +
  '        <row oid="2000" field_1="value 2"/>' +
  '      </rows>' +
  '    </table>' +
  '    <table table_name="table_2">' +
  '      <fields>' +
  '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
  '        <field field_name="field_2" field_kind="string" field_Size="50"/>' +
  '      </fields>' +
  '      <rows>' +
  '        <row oid="3000" field_2="value 3"/>' +
  '        <row oid="4000" field_2="value 4"/>' +
  '      </rows>' +
  '    </table>' +
  '  </tables>' +
  '</xmldocdata>';

var
  lXMLDBParser : TXMLToDataSetReaderTest;
begin
  // Nasty, fragile test. Sorry.
  lXMLDBParser := TXMLToDataSetReaderTest.Create;
  try
    lXMLDBParser.OptXMLDBSize := optDBSizeOff ;
    lXMLDBParser.XML := cTableXMLDBSizeOptOff ;

    // Scan for <tables>
    lXMLDBParser.Next ;
    CheckEquals(70, lXMLDBParser.Pos, 'Pos');
    CheckEquals('    <table', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    // Scan for <table>
    lXMLDBParser.Next ;
    CheckEquals(81, lXMLDBParser.Pos, 'Pos');
    CheckEquals('table_name', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    // Parse for table attribute (table_name=table1)
    lXMLDBParser.FTableName:= '' ;
    lXMLDBParser.Next;
    CheckEquals(101, lXMLDBParser.Pos, 'Pos');
    CheckEquals('>      <fields>', lXMLDBParser.NextChars(15), 'NextChars');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdbsFields');
    CheckEquals('table_1',lXMLDBParser.FTableName,'FTableName <> table_1');

    lXMLDBParser.Next;
    CheckEquals(131, lXMLDBParser.Pos, 'Pos');
    CheckEquals('field_name="oid"', lXMLDBParser.NextChars(16), 'NextChars');
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.FFieldName := '';
    lXMLDBParser.FFieldKind := '';
    lXMLDBParser.FFieldSize := '';
    lXMLDBParser.Next;
    CheckEquals(183, lXMLDBParser.Pos, 'Pos');
    CheckEquals('/>        <field', lXMLDBParser.NextChars(16), 'NextChars');
    CheckEquals('oid', lXMLDBParser.FFieldName, 'FFieldName <> ''oid''');
    CheckEquals('string', lXMLDBParser.FFieldKind, 'FFieldKind <> ''string''');
    CheckEquals('36', lXMLDBParser.FFieldSize, 'FFieldSize <> ''36''');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    CheckEquals(200, lXMLDBParser.Pos, 'Pos');
    CheckEquals('field_name="field_1"', lXMLDBParser.NextChars(20), 'NextChars');
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.FFieldName := '';
    lXMLDBParser.FFieldKind := '';
    lXMLDBParser.FFieldSize := '';
    lXMLDBParser.Next;
    CheckEquals(256, lXMLDBParser.Pos, 'Pos');
    CheckEquals('/>      </fields>', lXMLDBParser.NextChars(17), 'NextChars');
    CheckEquals('field_1', lXMLDBParser.FFieldName, 'FFieldName <> ''field_1''');
    CheckEquals('string',  lXMLDBParser.FFieldKind, 'FFieldKind <> ''string''');
    CheckEquals('50',      lXMLDBParser.FFieldSize, 'FFieldSize <> ''50''');
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    CheckEquals(273, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals('      <rows>', lXMLDBParser.NextChars(12), 'NextChars');

    lXMLDBParser.Next;
    CheckEquals(298, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals('oid="1000"', lXMLDBParser.NextChars(10), 'NextChars');

    lXMLDBParser.Attributes.Clear;
    lXMLDBParser.Next;
    CheckEquals(328, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals(2, lXMLDBParser.Attributes.Count, 'lXMLDBParser.Attributes.Count');
    CheckEquals('1000', lXMLDBParser.Attributes.Values['oid'], 'oid <> "1000"');
    CheckEquals('value 1', lXMLDBParser.Attributes.Values['field_1'], 'field_1 <> "value 1"');

    lXMLDBParser.Next;
    CheckEquals(341, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Attributes.Clear;
    lXMLDBParser.Next;
    CheckEquals(371, lXMLDBParser.Pos, 'Pos');
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');
    CheckEquals(2, lXMLDBParser.Attributes.Count, 'lXMLDBParser.Attributes.Count');
    CheckEquals('2000', lXMLDBParser.Attributes.Values['oid'], 'oid <> "2000"');
    CheckEquals('value 2', lXMLDBParser.Attributes.Values['field_1'], 'field_1 <> "value 2"');

    lXMLDBParser.Next;
    CheckEquals(384, lXMLDBParser.Pos, 'Pos');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdbsFields');

    lXMLDBParser.Next;
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsField = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsFields = lXMLDBParser.State, 'State <> xdsField');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRow = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsRows = lXMLDBParser.State, 'State <> xdbsRows');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    lXMLDBParser.Next;
    Check(xdbsEnd = lXMLDBParser.State, 'State <> xdbsEnd');

  finally
    lXMLDBParser.Free;
  end;

end;

{ TXMLToDataSetReaderTest }

constructor TXMLToDataSetReaderTest.Create;
begin
  inherited;
  FAttributes := TStringList.Create;
end;

destructor TXMLToDataSetReaderTest.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TXMLToDataSetReaderTest.DoAddCell(const pFieldName: string;pFieldValue: string);
begin
  Attributes.Values[pFieldName] := pFieldValue;
end;

procedure TXMLToDataSetReaderTest.DoAddField(const pFieldName, pFieldKind,pFieldSize: string);
begin
  FFieldName:= pFieldName;
  FFieldKind:= pFieldKind;
  FFieldSize:= pFieldSize;
end;

procedure TXMLToDataSetReaderTest.DoAddRow;
begin
  //
end;

procedure TXMLToDataSetReaderTest.DoAddTable(const pTableName: string);
begin
  FTableName:= pTableName;
end;

procedure TXMLToDataSetReaderTest.Next;
begin
  inherited;
end;

function TXMLToDataSetReaderTest.RestOfString: string;
begin
  result := Copy(XML,Pos,Len-Pos+1);
end;

function TXMLToDataSetReaderTest.NextChars(pNoOfChars: Cardinal): string;
begin
  result := Copy(XML, Pos, pNoOfChars);
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetReaderPerformanceAbsolute(
  pXMLFieldNameStyle: TtiXMLFieldNameStyle);
var
  lStart : DWord ;
  lWrite : DWord ;
  lMSPer100Rows : real ;
  ls: string;
  lXMLToTIDataSets     : TtiXMLToDataSetReadWriter ;
  lDataSets : TtiDataSets;
begin

  ls := MakeXMLDatabaseCompoundWithReadWrite(1, cRowCount, False, pXMLFieldNameStyle);

  lDataSets := TtiDataSets.Create;
  try
    lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
    try
      lXMLToTIDataSets.XMLFieldNameStyle := pXMLFieldNameStyle ;
      lXMLToTIDataSets.DataSets := lDataSets ;
      lStart := GetTickCount;
      lXMLToTIDataSets.AsString := ls ;
      lWrite := GetTickCount - lStart ;
    finally
      lXMLToTIDataSets.Free;
    end ;
  finally
    lDataSets.Free;
  end;

  lMSPer100Rows := lWrite * 1000 / cRowCount ;
  Check( lMSPer100Rows < cMSPer1000Rows, 'Not fast enough: ' + tiFloatToStr( lMSPer100Rows, 2 ) + 'ms / 1000 rows (should be ' + IntToStr(cMSPer1000Rows) + ')' ) ;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetWriterPerformanceAbsolute(pXMLFieldNameStyle: TtiXMLFieldNameStyle);
var
  lStart : DWord ;
  lWrite : DWord ;
  lMSPer100Rows:  real ;
  lsWrite: string ;
begin

  lStart := GetTickCount;
  lsWrite := MakeXMLDatabaseCompoundWithReadWrite(1, cRowCount, False, pXMLFieldNameStyle);
  lWrite := GetTickCount - lStart ;

  lMSPer100Rows := lWrite * 1000 / cRowCount ;
  Check( lMSPer100Rows < cMSPer1000Rows, 'Not fast enough: ' + tiFloatToStr( lMSPer100Rows, 2 ) + 'ms / 1000 rows (should be ' + IntToStr(cMSPer1000Rows) + ')' ) ;
end;
{$ENDIF}

procedure TTestTIXMLtoTIDataSet.CompoundDataToDataSets(
  const pDataSets: TtiDataSets; pDataSetCount, pRowCount: Integer;
  pIncludeBinField: Boolean);
var
  lDataSet         : TtiDataSet ;
  lDataSetRow      : TtiDataSetRow ;
  lDataSetCount    : integer ;
  lRowCount        : integer ;
  lVal             : integer ;
  lStream          : TStringStream ;
begin
  Assert(pDataSets<>nil, 'pDataSets not assigned');
  for lDataSetCount := 0 to pDataSetCount - 1 do
  begin
    lDataSet := pDataSets.AddInstance('dataset' + IntToStr(lDataSetCount));
    lDataSet.Fields.AddInstance('string_field',   qfkString,  10 ) ;
    lDataSet.Fields.AddInstance('integer_field',  qfkInteger,  0 ) ;
    lDataSet.Fields.AddInstance('float_field',    qfkFloat,    0 ) ;
    lDataSet.Fields.AddInstance('boolean_field',  qfkLogical,  0 ) ;
    lDataSet.Fields.AddInstance('date_field',     qfkDateTime, 0 ) ;
    if pIncludeBinField then
      lDataSet.Fields.AddInstance('bin_field',      qfkBinary,   0 ) ;
    for lRowCount := 0 to pRowCount - 1 do
    begin
      lVal := (lDataSetCount+1)*1000+lRowCount ;
      lDataSetRow := lDataSet.AddInstance;
      lDataSetRow.FindByFieldName('string_field').ValueAsString   := tstIntToStr(lVal) ;
      lDataSetRow.FindByFieldName('integer_field').ValueAsInteger := lVal ;
      lDataSetRow.FindByFieldName('float_field').ValueAsFloat     := tstIntToFloat(lVal);
      lDataSetRow.FindByFieldName('boolean_field').ValueAsBool    := tstIntToBool(lVal);
      lDataSetRow.FindByFieldName('date_field').ValueAsDateTime   := tstIntToDateTime(lVal);
      if pIncludeBinField then
      begin
        lStream := TStringStream.Create(LongString);
        try
          lDataSetRow.FindByFieldName('bin_field').AssignFromStream(lStream);
        finally
          lStream.Free;
        end;
      end;
    end;
  end;
end;

constructor TTestTIXMLtoTIDataSet.Create(MethodName: string);
begin
  inherited;
  FXMLTags := TtiXMLTags.Create ;
end;

destructor TTestTIXMLtoTIDataSet.Destroy;
begin
  FXMLTags.Free;
  inherited;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetCompoundInvalidXML;
  procedure _DoTest(const pXML: string; pPass: Boolean; const pTestID: string);
  var
    lXMLDBParser : TXMLToDataSetReaderTest;
    lPos: Cardinal;
  const
    cExceptionNotRaised = 'Exception not raised when it should have been' ;
  begin
    lXMLDBParser := TXMLToDataSetReaderTest.Create;
    try
      lXMLDBParser.OptXMLDBSize := optDBSizeOff ;
      lXMLDBParser.XML := pXML ;
      lPos := High(Cardinal);
      try
        while lXMLDBParser.State <> xdbsEnd do
        begin
          Check( lXMLDBParser.Pos <> lPos, 'Pos not incrumented. Test #' + pTestID);
          lPos := lXMLDBParser.Pos;
          lXMLDBParser.Next ;
        end ;
        if pPass then
          Check(lXMLDBParser.State = xdbsEnd, 'XMLDBParser.State <> xdbsEnd. Test #' + pTestID)
        else
          Fail(cExceptionNotRaised);
      except
        on e:Exception do
        begin
          if pPass then
            raise
          else begin
            if e.message <> cExceptionNotRaised then
            begin
              CheckIs(E, Exception);
              Check(Pos('Error parsing XML at position', e.Message) <> 0,
                    'Exception message does not contain the text <' +
                    cErrorParsingXMLAtPos + '> Test #' + pTestID);
            end;
          end;
        end ;
      end ;
    finally
      lXMLDBParser.Free;
    end;
  end;
begin
  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>'
    ,True, '1' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdataX>'
    ,False, '2' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', True, '3' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </tableX>' +
    '  </tables>' +
    '</xmldocdata>', False, '4' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rowsX>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '5' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <rowX oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '6' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fieldsX>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '7');

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <fieldX field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '8' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_nameX="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '9' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kindX="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '10' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_SizeX="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '11' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1>' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', False, '12' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '    <table table_name="table_2">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_2" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="3000" field_2="value 3"/>' +
    '        <row oid="4000" field_2="value 4"/>' +
    '      </rows>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdata>', True, '13' );

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    <table table_name="table_1">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_1" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="1000" field_1="value 1"/>' +
    '        <row oid="2000" field_1="value 2"/>' +
    '      </rows>' +
    '    </table>' +
    '    <table table_name="table_2">' +
    '      <fields>' +
    '        <field field_name="oid" field_kind="string" field_Size="36"/>' +
    '        <field field_name="field_2" field_kind="string" field_Size="50"/>' +
    '      </fields>' +
    '      <rows>' +
    '        <row oid="3000" field_2="value 3"/>' +
    '        <row oid="4000" field_2="value 4"/>' +
    '      </rows>' +
    '    </table>'{ +
    '  </tables>' +
    '</xmldocdata>'}, False, '14' );

end;

end.
