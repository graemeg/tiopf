unit tiXMLToTIDataSet_TST;

{$I tiDefines.inc}

interface
uses
  TestFrameWork
  ,tiTestFramework
  ,tiQuery
  ,Classes
  ,tiXMLToTIDataSet
  ,tiDataBuffer_BOM
  ,tiXML
;

type

  // This makes public some protected methods in TXMLToDataSetReader so
  // they can be tested
  TXMLToDataSetReaderTest = class(TXMLToDataSetReader)
  private
    FTableName: string;
    FFieldName: string;
    FFieldKind: string;
    FFieldSize: string;
    FAttributes: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    property    XML;
    property    State;
    property    DataSets;
    property    Pos;
    property    Len;
    procedure   DoAddTable(const ATableName : string); override;
    procedure   DoAddField(const AFieldName, AFieldKind, pFieldSize : string); override;
    procedure   DoAddRow; override;
    procedure   DoAddCell(const AFieldName: string; pFieldValue: string); override;
    procedure   Next; override;
    function    RestOfString: string;
    function    NextChars(pNoOfChars: Cardinal): string;
    property    Attributes : TStringList read FAttributes write FAttributes;

  end;

  TTestTIXMLtoTIDataSet = class(TtiTestCase)
  private
    FXMLTags : TtiXMLTags;

    function  MakeXMLDatabase(const pTableString : string): string;
    function  MakeXMLDatabaseWithMetaData(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: Integer): string;
    function  MakeXMLDatabaseWithData(
                const AFieldName: string;AFieldKind: TtiQueryFieldKind;
                pFieldSize: Integer; const AValue : string;
                pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;

    procedure CompoundDataToDataSets(const pDataSets: TtiDataBuffers;
                                     pDataSetCount: Integer; pRowCount: Integer;
                                     pIncludeBinField : Boolean);

    function  MakeXMLDatabaseCompoundWithReadWrite(
                pDataSetCount: Integer; pRowCount: Integer;
                pIncludeBinField : Boolean;
                pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;
    procedure CheckXMLDatabaseWithCompoundData(const AValue: string; pDataSetCount, pRowCount: Integer; pIncludeBinField: Boolean);

    procedure DoXMLToDataSetMetaDataBuffered(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: Integer);
    procedure DoXMLToDataSetDataBuffered(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: integer; const AValue : string);

    procedure DoXMLDBParserMetaData(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: integer);
    procedure DoXMLDBParser(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: integer; const AValue : string);

    procedure DoXMLToDataSetMetaDataWriteOnly(
      const AFieldName: string;AFieldKind: TtiQueryFieldKind;
      pFieldSize: Integer; pXMLFieldNameStyle : TtiXMLFieldNameStyle);

    procedure DoXMLToDataSetDataWriteOnly(
      const AFieldName: string;AFieldKind: TtiQueryFieldKind;
      pFieldSize: integer; const AValue : string;
      pXMLFieldNameStyle : TtiXMLFieldNameStyle);

    procedure DoXMLToDataSetWriterPerformanceAbsolute(pXMLFieldNameStyle : TtiXMLFieldNameStyle);
    procedure DoXMLToDataSetReaderPerformanceAbsolute(pXMLFieldNameStyle : TtiXMLFieldNameStyle);

  public
    constructor Create(MethodName : string); override;
    destructor  Destroy; override;
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

    procedure XMLToDataSetWriterCompound;

    procedure XMLToDataSetWriterPerformanceFSStringAbsolute;
    procedure XMLToDataSetReaderPerformanceFSStringAbsolute;
    procedure XMLToDataSetWriterPerformanceFSIntegerAbsolute;
    procedure XMLToDataSetReaderPerformanceFSIntegerAbsolute;

  end;

procedure RegisterTests;

implementation
uses
   tiDUnitDependencies
  ,SysUtils
  ,tiUtils
  ,Windows
  ,Dialogs
;

const

  cWithResCharsXML    = 'yet &another <test> "string''ie"';
  cWithoutResCharsXML = 'yet &amp;another &lt;test&gt; &quot;string&apos;ie&quot;';

  cWithResCharsCSV    = 'yet, another'#13#10'test,'#10'string'#13;
  cWithoutResCharsCSV = 'yet&com; another&cr;&lf;test&com;&lf;string&cr;';

  cWithResCharsTAB    = 'yet'#9' another'#13#10'test'#9''#10'string'#13;
  cWithoutResCharsTAB = 'yet&tab; another&cr;&lf;test&tab;&lf;string&cr;';

  cRowCount               = 20000;
  cImprovement            = 10;
  cMSPer1000Rows          = 130;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIXMLtoTIDataSet);
end;


procedure TTestTIXMLtoTIDataSet.XMLToDataSetEmptyFile_Read;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lResult         : string;
  lTarget         : string;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lResult := lXMLToTIDataSets.AsString;
    lTarget := MakeXMLDatabase('');
    CheckEquals(lTarget, lResult);
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetEmptyFile_Write;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  ls              : string;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    ls := MakeXMLDatabase('');
    lDataSets := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
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
  DoXMLToDataSetMetaDataBuffered('test_field', qfkInteger, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_String;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkString, 10);
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetMetaDataBuffered(
  const AFieldName : string; AFieldKind : TtiQueryFieldKind;
  pFieldSize : Integer);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  lDataSet        : TtiDataBuffer;
  lResult         : string;
  lAsString       : string;
begin
  lAsString := MakeXMLDatabaseWithMetaData(AFieldName, AFieldKind, pFieldSize);

  // Test Read
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lDataSet := lDataSets.AddInstance('test');
      lDataSet.Fields.AddInstance(AFieldName, AFieldKind, pFieldSize);
      lResult := lXMLToTIDataSets.AsString;
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lXMLToTIDataSets.AsString := lAsString;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count');
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count');
      CheckEquals(0, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[AFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
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
  DoXMLToDataSetMetaDataBuffered('test_field', qfkBinary, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Boolean;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkLogical, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_DateTime;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkDateTime, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataBuffered_Float;
begin
  DoXMLToDataSetMetaDataBuffered('test_field', qfkFloat, 0);
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetDataBuffered(const AFieldName: string;AFieldKind: TtiQueryFieldKind; pFieldSize: integer;const AValue : string);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  lDataSet        : TtiDataBuffer;
  lDataSetRow     : TtiDataBufferRow;
  lResult         : string;
  lAsString       : string;
begin
  lAsString := MakeXMLDatabaseWithData(AFieldName, AFieldKind, pFieldSize, AValue, xfnsString);

  // Test Read
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lDataSet := lDataSets.AddInstance('test');
      lDataSet.Fields.AddInstance(AFieldName, AFieldKind, pFieldSize);
      lDataSetRow := lDataSet.AddInstance;
      lDataSetRow.FindByFieldName(AFieldName).ValueAsString := AValue;
      lResult := lXMLToTIDataSets.AsString;
      CheckEquals(lAsString, lResult);
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lXMLToTIDataSets.AsString := lAsString;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count');
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[AFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AValue, lDataSets.Items[0].Items[0].FindByFieldName(AFieldName).ValueAsString, 'lDataSets.Items[0].Items[0].FindByFieldName(AFieldName).ValueAsString');
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
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, 'DATA');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Boolean;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, 'TRUE');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_DateTime;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '01/01/2004 10:11:12:13');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Float;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '123.456');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_Integer;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 0, '123');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataBuffered_String;
begin
  DoXMLToDataSetDataBuffered('test_field', qfkString, 10, 'a test string');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetCompound;
var
  ls      : string;
begin
  ls := MakeXMLDatabaseCompoundWithReadWrite(10, 20, True, xfnsString);
  CheckXMLDatabaseWithCompoundData(ls, 10, 20, true);
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseWithMetaData(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: Integer): string;
var
  ls : string;
begin
  ls :=
    '<'  + FXMLTags.Table + ' ' + FXMLTags.TableName + '="test"' + '>' +
    '<'  + FXMLTags.Fields + '>' +
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + AFieldName + '" '
                              + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[AFieldKind]) + '" '
                              + FXMLTags.FieldSize + '="' + IntToStr(pFieldSize) + '"/>' +
    '</' + FXMLTags.Fields + '>' +
    '<'  + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Table + '>';
  Result := MakeXMLDatabase(ls);
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseWithData(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: Integer; const AValue : string;
  pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;
var
  ls : string;
  lFieldName: string;
begin

  case pXMLFieldNameStyle of
    xfnsString : lFieldName := AFieldName;
    xfnsInteger: lFieldName := tiEncodeWordBase26(0);
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end;

  ls :=
    '<'  + FXMLTags.Table + ' ' + FXMLTags.TableName + '="test"' + '>' +
    '<'  + FXMLTags.Fields + '>' +
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + AFieldName + '" '
                              + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[AFieldKind]) + '" '
                              + FXMLTags.FieldSize + '="' + IntToStr(pFieldSize) + '"/>' +
    '</' + FXMLTags.Fields + '>' +
    '<'  + FXMLTags.Rows + '>' +
    '<' + FXMLTags.Row   + ' ' + lFieldName + '="' + AValue + '"/>' +
    '</' + FXMLTags.Rows + '>' +
    '</' + FXMLTags.Table + '>';

  Result := MakeXMLDatabase(ls);
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetDataWriteOnly(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; const AValue: string;
  pXMLFieldNameStyle : TtiXMLFieldNameStyle);
var
  lXMLWriter : TtiDataBufferToXMLWriter;
  lResult         : string;
  lAsString       : string;
begin
  lAsString :=
    MakeXMLDatabaseWithData(AFieldName, AFieldKind, pFieldSize, AValue, pXMLFieldNameStyle);

  // Test Read
  lXMLWriter := TtiDataBufferToXMLWriter.Create;
  try
    lXMLWriter.XMLFieldNameStyle := pXMLFieldNameStyle;
    lXMLWriter.AddTable('test');
    lXMLWriter.AddField(AFieldName, AFieldKind, pFieldSize);
    lXMLWriter.AddRow;
    lXMLWriter.AddCellAsString(AFieldName, AValue);
    lResult := lXMLWriter.AsString;
    CheckEquals(lAsString, lResult);
  finally
    lXMLWriter.Free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetMetaDataWriteOnly(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; pXMLFieldNameStyle : TtiXMLFieldNameStyle);
var
  lXMLWriter : TtiDataBufferToXMLWriter;
  lResult   : string;
  lAsString : string;
begin
  lAsString := MakeXMLDatabaseWithMetaData(AFieldName, AFieldKind, pFieldSize);

  lXMLWriter := TtiDataBufferToXMLWriter.Create;
  try
    lXMLWriter.XMLFieldNameStyle := pXMLFieldNameStyle;
    lXMLWriter.AddTable('test');
    lXMLWriter.AddField(AFieldName, AFieldKind, pFieldSize);
    lResult := lXMLWriter.AsString;
    CheckEquals(lAsString, lResult);
  finally
    lXMLWriter.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Binary;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkBinary, 0, 'DATA', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Boolean;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkLogical, 0, 'TRUE', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_DateTime;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkDateTime, 0, '01/01/2004 10:11:12:13', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Float;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkFloat, 0, '123.456', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_Integer;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkInteger, 0, '123', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSString_String;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkString, 10, 'a test string', xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Binary;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkBinary, 0, 'DATA', xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Boolean;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkLogical, 0, 'TRUE', xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_DateTime;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkDateTime, 0, '01/01/2004 10:11:12:13', xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Float;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkFloat, 0, '123.456', xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_Integer;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkInteger, 0, '123', xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetDataWriteOnlyFSInteger_String;
begin
  DoXMLToDataSetDataWriteOnly('test_field', qfkString, 10, 'a test string', xfnsInteger);
end;


procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Binary;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkBinary, 0, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Boolean;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkLogical, 0, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_DateTime;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkDateTime, 0, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Float;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkFloat, 0, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_Integer;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkInteger, 0, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSString_String;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkString, 10, xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Binary;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkBinary, 0, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Boolean;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkLogical, 0, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_DateTime;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkDateTime, 0, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Float;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkFloat, 0, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_Integer;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkInteger, 0, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnlyFSInteger_String;
begin
  DoXMLToDataSetMetaDataWriteOnly('test_field', qfkString, 10, xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetMetaDataWriteOnly_Empty;
var
  lXMLWriter: TtiDataBufferToXMLWriter;
  lTarget: string;
  lResult: string;
begin
  lXMLWriter := TtiDataBufferToXMLWriter.Create;
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
    '</' + FXMLTags.DocData + '>';
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterCompound;
var
  ls : string;
begin
  ls := MakeXMLDatabaseCompoundWithReadWrite(10, 20, True, xfnsString);
  CheckXMLDatabaseWithCompoundData(ls, 10, 20, true);
end;

procedure TTestTIXMLtoTIDataSet.CheckXMLDatabaseWithCompoundData(
  const AValue: string; pDataSetCount : Integer;
  pRowCount : Integer;
  pIncludeBinField : Boolean);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  lDataSet        : TtiDataBuffer;
  lDataSetRow     : TtiDataBufferRow;
  lAsString2      : string;
  lDataSetCount   : integer;
  lRowCount       : integer;
  lVal            : integer;
  lStream1        : TStringStream;
  lStream2        : TMemoryStream;
begin

  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lXMLToTIDataSets.AsString := AValue;
      CheckEquals(pDataSetCount, lDataSets.Count, 'lDataSets.Count');
      for lDataSetCount := 0 to pDataSetCount - 1 do
      begin
        lDataSet := lDataSets.Items[lDataSetCount];
        CheckEquals('dataset' + IntToStr(lDataSetCount), lDataSet.Name, 'lDataSet.Name');
        if pIncludeBinField then
          CheckEquals(6, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count')
        else
          CheckEquals(5, lDataSet.Fields.Count, 'lDataSets.Items[0].Fields.Count');
        CheckEquals('string_field',  lDataSet.Fields.Items[0].Name);
        CheckEquals('integer_field', lDataSet.Fields.Items[1].Name);
        CheckEquals('float_field',   lDataSet.Fields.Items[2].Name);
        CheckEquals('boolean_field', lDataSet.Fields.Items[3].Name);
        CheckEquals('date_field',    lDataSet.Fields.Items[4].Name);
        if pIncludeBinField then
          CheckEquals('bin_field',     lDataSet.Fields.Items[5].Name);

        CheckEquals(10, lDataSet.Fields.Items[0].Width);
        CheckEquals(0, lDataSet.Fields.Items[1].Width);
        CheckEquals(0, lDataSet.Fields.Items[2].Width);
        CheckEquals(0, lDataSet.Fields.Items[3].Width);
        CheckEquals(0, lDataSet.Fields.Items[4].Width);
        if pIncludeBinField then
          CheckEquals(0, lDataSet.Fields.Items[5].Width);

        Check(qfkString   = lDataSet.Fields.Items[0].Kind, 'Kind <> qfkString');
        Check(qfkInteger  = lDataSet.Fields.Items[1].Kind, 'Kind <> qfkInteger');
        Check(qfkFloat    = lDataSet.Fields.Items[2].Kind, 'Kind <> qfkFloat');
        Check(qfkLogical  = lDataSet.Fields.Items[3].Kind, 'Kind <> qfkLogical');
        Check(qfkDateTime = lDataSet.Fields.Items[4].Kind, 'Kind <> qfkDateTime');
        if pIncludeBinField then
          Check(qfkBinary   = lDataSet.Fields.Items[5].Kind, 'Kind <> qfkBinary');

        CheckEquals(pRowCount, lDataSet.Count, 'lDataSet.Count');
        for lRowCount := 0 to pRowCount - 1 do
        begin
          lVal := (lDataSetCount+1)*1000+lRowCount;
          lDataSetRow := lDataSet.Items[lRowCount];
          CheckEquals(tstIntToStr(lVal), lDataSetRow.FindByFieldName('string_field').ValueAsString, 'string_field');
          CheckEquals(lVal, lDataSetRow.FindByFieldName('integer_field').ValueAsInteger, 'integer_field');
          CheckNearEnough(tstIntToFloat(lVal), lDataSetRow.FindByFieldName('float_field').ValueAsFloat, 'float_field');
          CheckEquals(tstIntToBool(lVal), lDataSetRow.FindByFieldName('boolean_field').ValueAsBool, 'boolean_field');
          CheckEquals(tstIntToDateTime(lVal), lDataSetRow.FindByFieldName('date_field').ValueAsDateTime, 5, 'date_field');
          if pIncludeBinField then
          begin
            lStream1 := TStringStream.Create(LongString);
            try
              lStream2 := TMemoryStream.Create;
              try
                lDataSetRow.FindByFieldName('bin_field').AssignToStream(lStream2);
                CheckStreamContentsSame(lStream2, lStream1);
              finally
                lStream2.Free;
              end;
            finally
              lStream1.Free;
            end;
          end;
        end;
      end;
      lAsString2 := lXMLToTIDataSEts.AsString;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
  CheckEquals(AValue,lAsString2);
end;

function TTestTIXMLtoTIDataSet.MakeXMLDatabaseCompoundWithReadWrite(
           pDataSetCount: Integer; pRowCount: Integer;
           pIncludeBinField : Boolean;
           pXMLFieldNameStyle: TtiXMLFieldNameStyle): string;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lXMLToTIDataSets.XMLFieldNameStyle := pXMLFieldNameStyle;
    lDataSets       := TtiDataBuffers.Create;
    try
      CompoundDataToDataSets(lDataSets, pDataSetCount, pRowCount, pIncludeBinField);
      lXMLToTIDataSets.DataSets := lDataSets;
      result := lXMLToTIDataSets.AsString;
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLDBParser(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: integer; const AValue: string);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  lAsString       : string;
begin
  lAsString := MakeXMLDatabaseWithData(AFieldName, AFieldKind, pFieldSize, AValue, xfnsString);
  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lXMLToTIDataSets.AsString := lAsString;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count');
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[AFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
      CheckEquals(1, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AValue, lDataSets.Items[0].Items[0].FindByFieldName(AFieldName).ValueAsString, 'lDataSets.Items[0].Items[0].FindByFieldName(AFieldName).ValueAsString');
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.DoXMLDBParserMetaData(
  const AFieldName: string; AFieldKind: TtiQueryFieldKind;
  pFieldSize: integer);
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
  lDataSets       : TtiDataBuffers;
  lAsString       : string;
begin
  lAsString := MakeXMLDatabaseWithMetaData(AFieldName, AFieldKind, pFieldSize);
  // Test Write
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lDataSets       := TtiDataBuffers.Create;
    try
      lXMLToTIDataSets.DataSets := lDataSets;
      lXMLToTIDataSets.AsString := lAsString;
      CheckEquals(1, lDataSets.Count, 'lDataSets.Count');
      CheckEquals(1, lDataSets.Items[0].Fields.Count, 'lDataSets.Items[0].Fields.Count');
      CheckEquals(0, lDataSets.Items[0].Count, 'lDataSets.Items[0].Count');
      CheckEquals(AFieldName, lDataSets.Items[0].Fields.Items[0].Name, 'Name');
      CheckEquals(cgaQueryFieldKind[AFieldKind], lDataSets.Items[0].Fields.Items[0].KindAsStr, 'KindAsStr');
      CheckEquals(pFieldSize, lDataSets.Items[0].Fields.Items[0].Width, 'Width');
    finally
      lDataSets.Free;
    end;
  finally
    lXMLToTIDataSets.free;
  end;
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Binary;
begin
  DoXMLDBParser('test_field', qfkString, 0, 'DATA');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Boolean;
begin
  DoXMLDBParser('test_field', qfkString, 0, 'TRUE');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_DateTime;
begin
  DoXMLDBParser('test_field', qfkString, 0, '01/01/2004 10:11:12:13');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Float;
begin
  DoXMLDBParser('test_field', qfkString, 0, '123.456');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_Integer;
begin
  DoXMLDBParser('test_field', qfkString, 0, '123');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParser_String;
begin
  DoXMLDBParser('test_field', qfkString, 10, 'a test string');
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Binary;
begin
  DoXMLDBParserMetaData('test_field', qfkBinary, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Boolean;
begin
  DoXMLDBParserMetaData('test_field', qfkLogical, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_DateTime;
begin
  DoXMLDBParserMetaData('test_field', qfkDateTime, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Float;
begin
  DoXMLDBParserMetaData('test_field', qfkFloat, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_Integer;
begin
  DoXMLDBParserMetaData('test_field', qfkInteger, 0);
end;

procedure TTestTIXMLtoTIDataSet.XMLDBParserMetaData_String;
begin
  DoXMLDBParserMetaData('test_field', qfkString, 10);
end;

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

                   FDocData        := 'a';
                   FTables         := 'b';
                   FTable          := 'c';
                   FTableName      := 'd';
                   FFields         := 'e';
                   FField          := 'f';
                   FFieldName      := 'g';
                   FFieldKind      := 'h';
                   FFieldSize      := 'i';
                   FRows           := 'j';
                   FRow            := 'k';
                   FValue          := 'l';
}
var
  lXMLDBParser : TXMLToDataSetReaderTest;
begin
  // Nasty, fragile test. Sorry.
  lXMLDBParser := TXMLToDataSetReaderTest.Create;
  try
    lXMLDBParser.OptXMLDBSize := optDBSizeOn;
    lXMLDBParser.XML := cTableXMLDBSizeOptOn;

    // Scan for <tables>
    lXMLDBParser.Next;
    CheckEquals(56, lXMLDBParser.Pos, 'Pos');
    CheckEquals('    <c d="', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    // Scan for <table>
    lXMLDBParser.Next;
    CheckEquals(63, lXMLDBParser.Pos, 'Pos');
    CheckEquals('d="table_1', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    // Parse for table attribute (table_name=table1)
    lXMLDBParser.FTableName:= '';
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
    lXMLDBParser.OptXMLDBSize := optDBSizeOff;
    lXMLDBParser.XML := cTableXMLDBSizeOptOff;

    // Scan for <tables>
    lXMLDBParser.Next;
    CheckEquals(70, lXMLDBParser.Pos, 'Pos');
    CheckEquals('    <table', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTables = lXMLDBParser.State, 'State <> xdbsTables');

    // Scan for <table>
    lXMLDBParser.Next;
    CheckEquals(81, lXMLDBParser.Pos, 'Pos');
    CheckEquals('table_name', lXMLDBParser.NextChars(10), 'NextChars');
    Check(xdbsTable = lXMLDBParser.State, 'State <> xdbsTable');

    // Parse for table attribute (table_name=table1)
    lXMLDBParser.FTableName:= '';
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

procedure TXMLToDataSetReaderTest.DoAddCell(const AFieldName: string;pFieldValue: string);
begin
  Attributes.Values[AFieldName]:= pFieldValue;
end;

procedure TXMLToDataSetReaderTest.DoAddField(const AFieldName, AFieldKind,pFieldSize: string);
begin
  FFieldName:= AFieldName;
  FFieldKind:= AFieldKind;
  FFieldSize:= pFieldSize;
end;

procedure TXMLToDataSetReaderTest.DoAddRow;
begin
  //
end;

procedure TXMLToDataSetReaderTest.DoAddTable(const ATableName: string);
begin
  FTableName:= ATableName;
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
procedure TTestTIXMLtoTIDataSet.XMLToDataSetReaderPerformanceFSStringAbsolute;
begin
  DoXMLToDataSetReaderPerformanceAbsolute(xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterPerformanceFSStringAbsolute;
begin
  DoXMLToDataSetWriterPerformanceAbsolute(xfnsString);
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetReaderPerformanceAbsolute(
  pXMLFieldNameStyle: TtiXMLFieldNameStyle);
var
  lStart : DWord;
  lWrite : DWord;
  lMSPer100Rows : Extended;
  ls: string;
  lXMLToTIDataSets    : TtiXMLToDataSetReadWriter;
  lDataSets : TtiDataBuffers;
begin

  ls := MakeXMLDatabaseCompoundWithReadWrite(1, cRowCount, False, pXMLFieldNameStyle);

  lDataSets := TtiDataBuffers.Create;
  try
    lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
    try
      lXMLToTIDataSets.XMLFieldNameStyle := pXMLFieldNameStyle;
      lXMLToTIDataSets.DataSets := lDataSets;
      lStart := GetTickCount;
      lXMLToTIDataSets.AsString := ls;
      lWrite := GetTickCount - lStart;
    finally
      lXMLToTIDataSets.Free;
    end;
  finally
    lDataSets.Free;
  end;

  lMSPer100Rows := lWrite * 1000 / cRowCount;
  Check(lMSPer100Rows < cMSPer1000Rows, 'Not fast enough: ' + tiFloatToStr(lMSPer100Rows, 2) + 'ms / 1000 rows (should be ' + IntToStr(cMSPer1000Rows) + ')');
end;

procedure TTestTIXMLtoTIDataSet.DoXMLToDataSetWriterPerformanceAbsolute(pXMLFieldNameStyle: TtiXMLFieldNameStyle);
var
  lStart : DWord;
  lWrite : DWord;
  lMSPer100Rows:  Extended;
  lsWrite: string;
begin

  lStart := GetTickCount;
  lsWrite := MakeXMLDatabaseCompoundWithReadWrite(1, cRowCount, False, pXMLFieldNameStyle);
  lWrite := GetTickCount - lStart;

  lMSPer100Rows := lWrite * 1000 / cRowCount;
  Check(lMSPer100Rows < cMSPer1000Rows, 'Not fast enough: ' + tiFloatToStr(lMSPer100Rows, 2) + 'ms / 1000 rows (should be ' + IntToStr(cMSPer1000Rows) + ')');
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetReaderPerformanceFSIntegerAbsolute;
begin
  DoXMLToDataSetReaderPerformanceAbsolute(xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.XMLToDataSetWriterPerformanceFSIntegerAbsolute;
begin
  DoXMLToDataSetWriterPerformanceAbsolute(xfnsInteger);
end;

procedure TTestTIXMLtoTIDataSet.CompoundDataToDataSets(
  const pDataSets: TtiDataBuffers; pDataSetCount, pRowCount: Integer;
  pIncludeBinField: Boolean);
var
  lDataSet        : TtiDataBuffer;
  lDataSetRow     : TtiDataBufferRow;
  lDataSetCount   : integer;
  lRowCount       : integer;
  lVal            : integer;
  lStream         : TStringStream;
begin
  Assert(pDataSets<>nil, 'pDataSets not assigned');
  for lDataSetCount := 0 to pDataSetCount - 1 do
  begin
    lDataSet := pDataSets.AddInstance('dataset' + IntToStr(lDataSetCount));
    lDataSet.Fields.AddInstance('string_field',   qfkString,  10);
    lDataSet.Fields.AddInstance('integer_field',  qfkInteger,  0);
    lDataSet.Fields.AddInstance('float_field',    qfkFloat,    0);
    lDataSet.Fields.AddInstance('boolean_field',  qfkLogical,  0);
    lDataSet.Fields.AddInstance('date_field',     qfkDateTime, 0);
    if pIncludeBinField then
      lDataSet.Fields.AddInstance('bin_field',      qfkBinary,   0);
    for lRowCount := 0 to pRowCount - 1 do
    begin
      lVal := (lDataSetCount+1)*1000+lRowCount;
      lDataSetRow := lDataSet.AddInstance;
      lDataSetRow.FindByFieldName('string_field').ValueAsString  := tstIntToStr(lVal);
      lDataSetRow.FindByFieldName('integer_field').ValueAsInteger := lVal;
      lDataSetRow.FindByFieldName('float_field').ValueAsFloat    := tstIntToFloat(lVal);
      lDataSetRow.FindByFieldName('boolean_field').ValueAsBool   := tstIntToBool(lVal);
      lDataSetRow.FindByFieldName('date_field').ValueAsDateTime  := tstIntToDateTime(lVal);
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
  FXMLTags := TtiXMLTags.Create;
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
    cExceptionNotRaised = 'Exception not raised when it should have been';
  begin
    lXMLDBParser := TXMLToDataSetReaderTest.Create;
    try
      lXMLDBParser.OptXMLDBSize := optDBSizeOff;
      lXMLDBParser.XML := pXML;
      lPos := High(Cardinal);
      try
        while lXMLDBParser.State <> xdbsEnd do
        begin
          Check(lXMLDBParser.Pos <> lPos, 'Pos not incrumented. Test #' + pTestID);
          lPos := lXMLDBParser.Pos;
          lXMLDBParser.Next;
        end;
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
        end;
      end;
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
    ,True, '1');

  _DoTest(
    '<?xml version="1.0"?>' +
    '<xmldocdata>' +
    '  <tiopf version="1.300"/>' +
    '  <tables>' +
    '    </table>' +
    '  </tables>' +
    '</xmldocdataX>'
    ,False, '2');

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
    '</xmldocdata>', True, '3');

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
    '</xmldocdata>', False, '4');

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
    '</xmldocdata>', False, '5');

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
    '</xmldocdata>', False, '6');

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
    '</xmldocdata>', False, '8');

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
    '</xmldocdata>', False, '9');

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
    '</xmldocdata>', False, '10');

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
    '</xmldocdata>', False, '11');

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
    '</xmldocdata>', False, '12');

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
    '</xmldocdata>', True, '13');

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
    '</xmldocdata>'}, False, '14');

end;

end.
