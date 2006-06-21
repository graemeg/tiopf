unit tiDataSet_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTestFramework
  ,tiDataBuffer_Cli
  ,Classes
  ;


type
  TTesTtiDataBuffer = class(TtiTestCase)
  private
    FStringList: TStringList;
    procedure   DoExtractToken(pIndex: integer; const pValue: string);
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  public
    constructor Create{$IFNDEF FPC}(AMethodName: string){$ENDIF}; override;
    destructor  Destroy; override;
  published
    procedure   TIDataSetItems;
    procedure   TIDataSetAddInstance;
    procedure   TIDataSetRowItems;
    procedure   TIDataSetRowAddInstance;
    procedure   TIDataSetCellDataSetField;
    procedure   TIDataSetCellValueAsString;
    procedure   TIDataSetCellValueAsInteger;
    procedure   TIDataSetCellValueAsBool;
    procedure   TIDataSetCellValueAsReal;
    procedure   TIDataSetCellValueAsDateTime;
    procedure   TIDataSetCellValueAsStream;

    procedure   ExtractToken;
    procedure   CSVToTIDataSet_Props;
    procedure   CSVToTIDataSet_SaveHeaderOnly;
    procedure   CSVToTIDataSet_SaveNoHeader;
    procedure   CSVToTIDataSet_SaveFieldNames;
    procedure   CSVToTIDataSet_ReadHeaderOnly;
    procedure   CSVToTIDataSet_ReadDataOnly;
    procedure   CSVToTIDataSet_ReadHeaderAndData;
    procedure   CSVToTIDataSet_Read1000Rows;
  end;


procedure RegisterTests;


implementation
uses
   tiDataBuffer_BOM
  ,tiQuery
  ,tiQueryTXTAbs
  ,SysUtils
  ,tiUtils
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
  ,tiDUnitDependencies
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiDUnitUtils
  ;


var
  uTempFileName : string;


function TempFileName: string ;
begin
  if Length(uTempFileName) = 0 then
  begin
    uTempFileName := tiAddTrailingSlash(tiGetTempDir)  + 'temp.txt';
    end;
  result := uTempFileName;
end;


procedure RegisterTests ;
begin
  RegisterNonPersistentTest(TTesTtiDataBuffer);
end ;


{ TTesTtiDataBuffer }

constructor TTesTtiDataBuffer.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  FStringList := TStringList.Create ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_Props;
var
  lData : TCSVToTIDataSet ;
begin
  lData := TCSVToTIDataSet.Create ;
  try
    Check( [tfmdFieldName] = lData.TextFileMetaData, 'TextFileMetaData' ) ;
    CheckEquals( ',', lData.FieldDelim, 'FieldDelim' ) ;
    CheckEquals( '"', lData.StringDelim, 'StringDelim' ) ;
    CheckEquals( CrLf, lData.RowDelim, 'RowDelim' ) ;

    lData.TextFileMetaData := [] ;
    lData.FieldDelim := '|' ;
    lData.StringDelim := '' ;
    lData.RowDelim := Cr ;

    Check( [] = lData.TextFileMetaData, 'TextFileMetaData' ) ;
    CheckEquals( '|', lData.FieldDelim, 'FieldDelim' ) ;
    CheckEquals( '', lData.StringDelim, 'StringDelim' ) ;
    CheckEquals( Cr, lData.RowDelim, 'RowDelim' ) ;
  finally
    lData.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_ReadHeaderAndData;
var
  lDataSet : TtiDataBuffer ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('fieldA,fieldB'+CrLf+'a1,b1'+CrLf+'a2,b2'+CrLf, TempFileName);
  lDataSet := TtiDataBuffer.Create ;
  try
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [tfmdFieldName];
      lWriter.Read(lDataSet, TempFileName ) ;
      CheckEquals(lDataSet.Fields.Count, 2, 'lDataSet.Fields.Count' ) ;
      CheckEquals(lDataSet.Fields.Items[0].Name, 'fieldA', 'fieldA' ) ;
      CheckEquals(lDataSet.Fields.Items[1].Name, 'fieldB', 'fieldB' ) ;
      Check(lDataSet.Fields.Items[0].Kind = qfkString, 'fieldA.Kind' ) ;
      Check(lDataSet.Fields.Items[1].Kind = qfkString, 'fieldB.Kind' ) ;
      CheckEquals( 2, lDataSet.Count, 'DataSet.Count' ) ;
      CheckEquals( lDataSet.Items[0].Count, 2, 'DataSet.Items[0].Count' ) ;
      CheckEquals( lDataSet.Items[0].Items[0].ValueAsString, 'a1' ) ;
      CheckEquals( lDataSet.Items[0].Items[1].ValueAsString, 'b1' ) ;
      CheckEquals( lDataSet.Items[1].Count, 2, 'DataSet.Items[0].Count' ) ;
      CheckEquals( lDataSet.Items[1].Items[0].ValueAsString, 'a2' ) ;
      CheckEquals( lDataSet.Items[1].Items[1].ValueAsString, 'b2' ) ;
    finally
      lWriter.Free ;
    end ;
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_ReadDataOnly;
var
  lDataSet : TtiDataBuffer ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('a1,b1'+CrLf+'a2,b2'+CrLf, TempFileName);
  lDataSet := TtiDataBuffer.Create ;
  try
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [];
      lWriter.Read(lDataSet, TempFileName ) ;
      CheckEquals(lDataSet.Fields.Count, 2, 'lDataSet.Fields.Count' ) ;
      CheckEquals(lDataSet.Fields.Items[0].Name, 'Field1', 'Field1' ) ;
      CheckEquals(lDataSet.Fields.Items[1].Name, 'Field2', 'Field2' ) ;
      Check(lDataSet.Fields.Items[0].Kind = qfkString, 'Field1.Kind' ) ;
      Check(lDataSet.Fields.Items[1].Kind = qfkString, 'Field2.Kind' ) ;
      CheckEquals( 2, lDataSet.Count, 'DataSet.Count' ) ;
      CheckEquals( lDataSet.Items[0].Count, 2, 'DataSet.Items[0].Count' ) ;
      CheckEquals( lDataSet.Items[0].Items[0].ValueAsString, 'a1' ) ;
      CheckEquals( lDataSet.Items[0].Items[1].ValueAsString, 'b1' ) ;
      CheckEquals( lDataSet.Items[1].Count, 2, 'DataSet.Items[0].Count' ) ;
      CheckEquals( lDataSet.Items[1].Items[0].ValueAsString, 'a2' ) ;
      CheckEquals( lDataSet.Items[1].Items[1].ValueAsString, 'b2' ) ;
    finally
      lWriter.Free ;
    end ;
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_ReadHeaderOnly;
var
  lDataSet : TtiDataBuffer ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('fieldA,fieldB', TempFileName);
  lDataSet := TtiDataBuffer.Create ;
  try
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [tfmdFieldName];
      lWriter.Read(lDataSet, TempFileName ) ;
      CheckEquals(lDataSet.Fields.Count, 2, 'lDataSet.Fields.Count' ) ;
      CheckEquals(lDataSet.Fields.Items[0].Name, 'fieldA', 'fieldA' ) ;
      CheckEquals(lDataSet.Fields.Items[1].Name, 'fieldB', 'fieldB' ) ;
      Check(lDataSet.Fields.Items[0].Kind = qfkString, 'fieldA.Kind' ) ;
      Check(lDataSet.Fields.Items[1].Kind = qfkString, 'fieldB.Kind' ) ;
      CheckEquals( 0, lDataSet.Count, 'DataSet.Count' ) ;
    finally
      lWriter.Free ;
    end ;
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_SaveFieldNames;
var
  lDataSet : TtiDataBuffer ;
  lRow     : TtiDataBufferRow ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lDataSet.Fields.AddInstance( 'field1', qfkString, 10 ) ;
    lDataSet.Fields.AddInstance( 'field2', qfkString, 10 ) ;
    lRow := lDataSet.AddInstance ;
    lRow.Items[0].ValueAsString := 'a1' ;
    lRow.Items[1].ValueAsString := 'b1' ;
    lRow := lDataSet.AddInstance ;
    lRow.Items[0].ValueAsString := 'a2' ;
    lRow.Items[1].ValueAsString := 'b2' ;
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [tfmdFieldName];
      lWriter.Save(lDataSet, TempFileName ) ;
    finally
      lWriter.Free ;
    end ;
    ls := tiFileToString(TempFileName);
    CheckEquals('"field1","field2"'+CrLf+'"a1","b1"'+CrLf+'"a2","b2"'+CrLf, ls);
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_SaveHeaderOnly;
var
  lDataSet : TtiDataBuffer ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lDataSet.Fields.AddInstance( 'field1', qfkString, 10 ) ;
    lDataSet.Fields.AddInstance( 'field2', qfkString, 10 ) ;
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [tfmdFieldName];
      lWriter.Save(lDataSet, TempFileName ) ;
    finally
      lWriter.Free ;
    end ;
    ls := tiFileToString(TempFileName);
    CheckEquals('"field1","field2"'+CrLf, ls);
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_SaveNoHeader;
var
  lDataSet : TtiDataBuffer ;
  lRow     : TtiDataBufferRow ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lDataSet.Fields.AddInstance( 'field1', qfkString, 10 ) ;
    lDataSet.Fields.AddInstance( 'field2', qfkString, 10 ) ;
    lRow := lDataSet.AddInstance ;
    lRow.Items[0].ValueAsString := 'a1' ;
    lRow.Items[1].ValueAsString := 'b1' ;
    lRow := lDataSet.AddInstance ;
    lRow.Items[0].ValueAsString := 'a2' ;
    lRow.Items[1].ValueAsString := 'b2' ;
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [];
      lWriter.Save(lDataSet, TempFileName ) ;
    finally
      lWriter.Free ;
    end ;
    ls := tiFileToString(TempFileName);
    CheckEquals('"a1","b1"'+CrLf+'"a2","b2"'+CrLf, ls);
  finally
    lDataSet.Free;
  end ;
end;


destructor TTesTtiDataBuffer.Destroy;
begin
  FStringList.Free ;
  inherited;
end;


procedure TTesTtiDataBuffer.DoExtractToken(pIndex: integer; const pValue: string);
begin
  if FStringList.Count < pIndex then
    FStringList.Add(pValue)
  else
    FStringList.Strings[pIndex] := pValue ;
end;


procedure TTesTtiDataBuffer.ExtractToken;
begin
  stExtractTokensL( 'test',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#1' ) ;
  CheckEquals( 'test', FStringList.Strings[0], '#2' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1,test2',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#3' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#4' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#5' ) ;
  FStringList.Clear ;

  stExtractTokensL( ',test2',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#6' ) ;
  CheckEquals( '', FStringList.Strings[0], '#7' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#8' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1,',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#9' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#10' ) ;
  CheckEquals( '', FStringList.Strings[1], '#11' ) ;
  FStringList.Clear ;

  stExtractTokensL( ',,test1,,',
                    ',', '"', false, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#12' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#13' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"test1",test2',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#14' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#15' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#16' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"test1,test2',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#17' ) ;
  CheckEquals( 'test1,test2', FStringList.Strings[0], '#18' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1",test2',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#19' ) ;
  CheckEquals( 'test1",test2', FStringList.Strings[0], '#20' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"tes,t1","te,st2"',
                    ',', '"', true, {$IFDEF FPC}@{$ENDIF}DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#21' ) ;
  CheckEquals( 'tes,t1', FStringList.Strings[0], '#22' ) ;
  CheckEquals( 'te,st2', FStringList.Strings[1], '#23' ) ;
  FStringList.Clear ;
end;


procedure TTesTtiDataBuffer.SetUp;
begin
  inherited;
  FStringList.Clear ;
end;


procedure TTesTtiDataBuffer.TIDataSetAddInstance;
var
  lDataSet : TtiDataBuffer ;
  lRow : TtiDataBufferRow ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lDataSet.Fields.AddInstance('field1', qfkString, 10);
    lDataSet.Fields.AddInstance('field2', qfkString, 10);
    lRow := lDataSet.AddInstance ;
    CheckEquals(2,lRow.Count, 'Row.Count');
    CheckSame(lDataSet.Fields.Items[0], lRow.Items[0].DataSetField, '#1');
    CheckSame(lDataSet.Fields.Items[1], lRow.Items[1].DataSetField, '#2');
  finally
    lDataSet.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.TIDataSetCellDataSetField;
var
  lDataSet : TtiDataBuffer ;
  lRow : TtiDataBufferRow ;
  lCell : TtiDataBufferCell ;
  lField : TtiDBMetaDataField ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lField := TtiDBMetaDataField.Create ;
    lDataSet.Fields.Add(lField);
    lRow := TtiDataBufferRow.Create ;
    lDataSet.Add(lRow);
    lCell := TtiDataBufferCell.Create ;
    lRow.Add(lCell);
    CheckSame(lField, lCell.DataSetField, 'DataSetField');

    lField := TtiDBMetaDataField.Create ;
    lDataSet.Fields.Add(lField);
    lCell := TtiDataBufferCell.Create ;
    lRow.Add(lCell);
    CheckSame(lField, lCell.DataSetField, 'DataSetField');
    lDataSet.Fields.Remove(lField);
    try
      lCell.DataSetField;
      Fail('Exception should have been raised');
    except
      on e:exception do
      begin
        CheckEquals( cErrorTIDataSetCellMetaData, e.message ) ;
      end ;
    end;
  finally
    lDataSet.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.TIDataSetItems;
var
  lDataSet : TtiDataBuffer ;
  lRow : TtiDataBufferRow ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lRow := TtiDataBufferRow.Create ;
    lDataSet.Add( lRow ) ;
    CheckEquals( 1, lDataSet.Count, 'Count' ) ;
    CheckSame( lRow, lDataSet.Items[0], 'Items[0]' );
    CheckSame( lRow, TObject(lDataSet.List.Items[0]), 'List.Items[0]' ) ;
    CheckSame( lDataSet, lRow.Owner, 'lRow.Owner' ) ;
    CheckEquals( 0, lRow.Index, 'lRow.Index' ) ;
    lRow := TtiDataBufferRow.Create ;
    lDataSet.Add( lRow ) ;
    CheckEquals( 2, lDataSet.Count, 'Count' ) ;
    CheckSame( lRow, lDataSet.Items[1], 'Items[1]' );
    CheckSame( lRow, TObject(lDataSet.List.Items[1]), 'List.Items[1]' ) ;
    CheckSame( lDataSet, lRow.Owner, 'lRow.Owner' ) ;
    CheckEquals( 1, lRow.Index, 'lRow.Index' ) ;
    lDataSet.Clear ;
    CheckEquals( 0, lDataSet.Count, 'Count' ) ;
  finally
    lDataSet.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.TIDataSetRowAddInstance;
var
  lDataSet : TtiDataBuffer ;
  lRow     : TtiDataBufferRow ;
  lCell1   : TtiDataBufferCell ;
  lCell2   : TtiDataBufferCell ;
begin
  lDataSet := TtiDataBuffer.Create ;
  try
    lDataSet.Fields.AddInstance('field1', qfkString, 10);
    lDataSet.Fields.AddInstance('field2', qfkString, 10);
    lRow := TtiDataBufferRow.Create ;
    lDataSet.Add(lRow);
    lCell1 := lRow.AddInstance ;
    lCell2 := lRow.AddInstance ;
    CheckEquals(2,lRow.Count, 'Row.Count');
    CheckSame(lCell1, lRow.Items[0], '#1');
    CheckSame(lDataSet.Fields.Items[0], lCell1.DataSetField, '#2');
    CheckSame(lCell2, lRow.Items[1], '#3');
    CheckSame(lDataSet.Fields.Items[1], lCell2.DataSetField, '#4');
  finally
    lDataSet.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.TIDataSetRowItems;
var
  lDataSet     : TtiDataBuffer ;
  lDataSetRow  : TtiDataBufferRow ;
  lDataSetCell : TtiDataBufferCell ;
begin
  lDataSet     := TtiDataBuffer.Create ;
  try
    lDataSetRow  := TtiDataBufferRow.Create;
    lDataSet.Add(lDataSetRow) ;
    lDataSetCell := TtiDataBufferCell.Create ;
    lDataSetRow.Add( lDataSetCell ) ;
    CheckEquals( 1, lDataSetRow.Count, 'Count' ) ;
    CheckSame( lDataSetCell, lDataSetRow.Items[0], 'Items[0]' );
    CheckSame( lDataSetRow, lDataSetCell.Owner, 'lRow.Owner' ) ;
    CheckEquals( 0, lDataSetCell.Index, 'lRow.Index' ) ;
    lDataSetCell := TtiDataBufferCell.Create ;
    lDataSetRow.Add( lDataSetCell ) ;
    CheckEquals( 2, lDataSetRow.Count, 'Count' ) ;
    CheckSame( lDataSetCell, lDataSetRow.Items[1], 'Items[1]' );
    CheckSame( lDataSetRow, lDataSetCell.Owner, 'lRow.Owner' ) ;
    CheckEquals( 1, lDataSetCell.Index, 'lRow.Index' ) ;
  finally
    lDataSet.Free ;
  end ;
end;


procedure TTesTtiDataBuffer.CSVToTIDataSet_Read1000Rows;
var
  lDataSet : TtiDataBuffer ;
  lWriter  : TCSVToTIDataSet ;
  lLine, ls : string ;
  i, j : integer ;
const
  cCols = 99 ;
  cRows = 100;
begin
  lLine := '' ;
  for j := 1 to cCols do
  begin
    if lLine <> '' then lLine := lLine + ',' ;
    lLine := lLine + 'field' + IntToStr(j);
  end;
  ls := lLine + CrLf ;

  for i := 0 to cRows do
  begin
    lLine := '';
    for j := 1 to cCols do
    begin
      if lLine <> '' then lLine := lLine + ',' ;
      lLine := lLine + 'value' + IntToStr(((i+1)*100)+(j));
    end;
    ls := ls + lLine + CrLf ;
  end ;

  tiStringToFile(ls, TempFileName);
  lDataSet := TtiDataBuffer.Create ;
  try
    lWriter := TCSVToTIDataSet.Create ;
    try
      lWriter.TextFileMetaData := [tfmdFieldName];
      lWriter.Read(lDataSet, TempFileName ) ;
      CheckEquals(cCols, lDataSet.Fields.Count, 'lDataSet.Fields.Count' ) ;
      for j := 1 to cCols do
        CheckEquals(lDataSet.Fields.Items[j-1].Name, 'field' + IntToStr(j), 'field'+IntToStr(j)) ;
      for i := 0 to cRows do
        for j := 1 to cCols do
          CheckEquals( lDataSet.Items[i].Items[j-1].ValueAsString, 'value' + IntToStr(((i+1)*100)+(j))) ;
    finally
      lWriter.Free ;
    end ;
  finally
    lDataSet.Free;
  end ;
end;


procedure TTesTtiDataBuffer.TearDown;
begin
  inherited;
end;


{
procedure TTesTtiDataBuffer.CommaInField;
begin

end;

procedure TTesTtiDataBuffer.CrInField;
begin

end;

procedure TTesTtiDataBuffer.LFInField;
begin

end;
}


procedure TTesTtiDataBuffer.TIDataSetCellValueAsString;
var
  lCell : TtiDataBufferCell ;
begin
  lCell := TtiDataBufferCell.Create ;
  try
    lCell.ValueAsString:= 'test';
    CheckEquals('test', lCell.ValueAsString);
  finally
    lCell.Free;
  end;
end;


procedure TTesTtiDataBuffer.TIDataSetCellValueAsBool;
var
  lCell : TtiDataBufferCell ;
begin
  lCell := TtiDataBufferCell.Create ;
  try
    lCell.ValueAsBool := True ;
    CheckEquals(true, lCell.ValueAsBool);
  finally
    lCell.Free;
  end;
end;


procedure TTesTtiDataBuffer.TIDataSetCellValueAsDateTime;
var
  lCell : TtiDataBufferCell ;
  lDate : TDateTime;
begin
  lDate := EncodeDate(2004, 01, 01) + EncodeTime(10, 20, 30, 00);
  lCell := TtiDataBufferCell.Create ;
  try
    lCell.ValueAsDateTime := lDate;
    CheckEquals(lDate, lCell.ValueAsDateTime);
  finally
    lCell.Free;
  end;
end;


procedure TTesTtiDataBuffer.TIDataSetCellValueAsInteger;
var
  lCell : TtiDataBufferCell ;
begin
  lCell := TtiDataBufferCell.Create ;
  try
    lCell.ValueAsInteger:= 1234567890;
    CheckEquals(1234567890, lCell.ValueAsInteger);
  finally
    lCell.Free;
  end;
end;


procedure TTesTtiDataBuffer.TIDataSetCellValueAsReal;
var
  lCell : TtiDataBufferCell ;
begin
  lCell := TtiDataBufferCell.Create ;
  try
    lCell.ValueAsFloat := 1234.56789;
    CheckEquals(1234.56789, lCell.ValueAsFloat, 5);
  finally
    lCell.Free;
  end;
end;


procedure TTesTtiDataBuffer.TIDataSetCellValueAsStream;
var
  lCell : TtiDataBufferCell ;
  lStrFrom  : string ;
  lStrTo    : string ;
  lStream : TStream;
begin
  lStrFrom := tiCreateStringOfSize( 1000 ) ;
  lCell := TtiDataBufferCell.Create ;
  try
    lStream := TMemoryStream.Create;
    try
      tiStringToStream(lStrFrom, lStream);
      lCell.AssignFromStream(lStream);
    finally
      lStream.Free;
    end;

    lStream := TMemoryStream.Create;
    try
      lCell.AssignToStream(lStream);
      lStrTo := tiStreamToString(lStream);
    finally
      lStream.Free;
    end;

    CheckEquals(lStrFrom, lStrTo);
  finally
    lCell.Free;
  end;
end;


end.

