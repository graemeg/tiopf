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

unit tiDataSet_TST;

interface
uses
  TestFramework
  ,tiDataSet_Cli
  ,Classes
  ;

type

  TTestTIDataSet = class( TTestCase )
  private
    FStringList : TStringList ;
    procedure DoExtractToken(pIndex: integer; const pValue: string);
  protected
    procedure Setup ; override ;
    procedure TearDown ; override ;
  public
    constructor Create(MethodName: string); override ;
    destructor  Destroy ; override ;

  published
    procedure TIDataSetItems ;
    procedure TIDataSetAddInstance ;
    procedure TIDataSetRowItems ;
    procedure TIDataSetRowAddInstance ;
    procedure TIDataSetCellDataSetField ;
    procedure TIDataSetCellValueAsString;
    procedure TIDataSetCellValueAsInteger;
    procedure TIDataSetCellValueAsBool;
    procedure TIDataSetCellValueAsReal;
    procedure TIDataSetCellValueAsDateTime;
    procedure TIDataSetCellValueAsStream;

    procedure ExtractToken ;
    procedure CSVToTIDataSet_Props ;
    procedure CSVToTIDataSet_SaveHeaderOnly ;
    procedure CSVToTIDataSet_SaveNoHeader ;
    procedure CSVToTIDataSet_SaveFieldNames ;
    procedure CSVToTIDataSet_ReadHeaderOnly;
    procedure CSVToTIDataSet_ReadDataOnly ;
    procedure CSVToTIDataSet_ReadHeaderAndData ;
    procedure CSVToTIDataSet_Read1000Rows ;

  end;

procedure RegisterTests ;

implementation
uses
   tiPersistAbs_TST
  ,tiDataSet_BOM
  ,tiQuery
  ,tiQueryTXTAbs
  ,SysUtils
  ,tiUtils
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
  ,tiDUnitDependencies
  ,Windows
  ,tiDUnitUtils
  ;

var
  uTempFileName : string;

function TempFileName: string ;
var
  pcTemp : array[0..MAX_PATH] of char ;
begin
  if Length(uTempFileName) = 0 then begin
    GetTempPath( MAX_PATH, pcTemp );
    uTempFileName := tiAddTrailingSlash( string(pcTemp) )  + 'temp.txt';
    end ;
  result := uTempFileName;
end;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIDataSet.Suite ) ;
end ;


{ TTestTIDataSet }

constructor TTestTIDataSet.Create(MethodName: string);
begin
  inherited;
  FStringList := TStringList.Create ;
end;

procedure TTestTIDataSet.CSVToTIDataSet_Props;
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

procedure TTestTIDataSet.CSVToTIDataSet_ReadHeaderAndData;
var
  lDataSet : TtiDataSet ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('fieldA,fieldB'+CrLf+'a1,b1'+CrLf+'a2,b2'+CrLf, TempFileName);
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.CSVToTIDataSet_ReadDataOnly;
var
  lDataSet : TtiDataSet ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('a1,b1'+CrLf+'a2,b2'+CrLf, TempFileName);
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.CSVToTIDataSet_ReadHeaderOnly;
var
  lDataSet : TtiDataSet ;
  lWriter  : TCSVToTIDataSet ;
begin
  tiStringToFile('fieldA,fieldB', TempFileName);
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.CSVToTIDataSet_SaveFieldNames;
var
  lDataSet : TtiDataSet ;
  lRow     : TtiDataSetRow ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.CSVToTIDataSet_SaveHeaderOnly;
var
  lDataSet : TtiDataSet ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.CSVToTIDataSet_SaveNoHeader;
var
  lDataSet : TtiDataSet ;
  lRow     : TtiDataSetRow ;
  lWriter  : TCSVToTIDataSet ;
  ls       : string ;
begin
  lDataSet := TtiDataSet.Create ;
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

destructor TTestTIDataSet.Destroy;
begin
  FStringList.Free ;
  inherited;
end;

procedure TTestTIDataSet.DoExtractToken(pIndex: integer; const pValue: string);
begin
  if FStringList.Count < pIndex then
    FStringList.Add(pValue)
  else
    FStringList.Strings[pIndex] := pValue ;
end;

procedure TTestTIDataSet.ExtractToken;
begin
  stExtractTokensL( 'test',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#1' ) ;
  CheckEquals( 'test', FStringList.Strings[0], '#2' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1,test2',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#3' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#4' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#5' ) ;
  FStringList.Clear ;

  stExtractTokensL( ',test2',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#6' ) ;
  CheckEquals( '', FStringList.Strings[0], '#7' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#8' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1,',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#9' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#10' ) ;
  CheckEquals( '', FStringList.Strings[1], '#11' ) ;
  FStringList.Clear ;

  stExtractTokensL( ',,test1,,',
                    ',', '"', false, DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#12' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#13' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"test1",test2',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#14' ) ;
  CheckEquals( 'test1', FStringList.Strings[0], '#15' ) ;
  CheckEquals( 'test2', FStringList.Strings[1], '#16' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"test1,test2',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#17' ) ;
  CheckEquals( 'test1,test2', FStringList.Strings[0], '#18' ) ;
  FStringList.Clear ;

  stExtractTokensL( 'test1",test2',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 1, FStringList.Count, '#19' ) ;
  CheckEquals( 'test1",test2', FStringList.Strings[0], '#20' ) ;
  FStringList.Clear ;

  stExtractTokensL( '"tes,t1","te,st2"',
                    ',', '"', true, DoExtractToken ) ;
  CheckEquals( 2, FStringList.Count, '#21' ) ;
  CheckEquals( 'tes,t1', FStringList.Strings[0], '#22' ) ;
  CheckEquals( 'te,st2', FStringList.Strings[1], '#23' ) ;
  FStringList.Clear ;

end;

procedure TTestTIDataSet.Setup;
begin
  inherited;
  FStringList.Clear ;
  SysUtils.DeleteFile(TempFileName);
end;

procedure TTestTIDataSet.TIDataSetAddInstance;
var
  lDataSet : TtiDataSet ;
  lRow : TtiDataSetRow ;
begin
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.TIDataSetCellDataSetField;
var
  lDataSet : TtiDataSet ;
  lRow : TtiDataSetRow ;
  lCell : TtiDataSetCell ;
  lField : TtiDBMetaDataField ;
begin
  lDataSet := TtiDataSet.Create ;
  try
    lField := TtiDBMetaDataField.Create ;
    lDataSet.Fields.Add(lField);
    lRow := TtiDataSetRow.Create ;
    lDataSet.Add(lRow);
    lCell := TtiDataSetCell.Create ;
    lRow.Add(lCell);
    CheckSame(lField, lCell.DataSetField, 'DataSetField');

    lField := TtiDBMetaDataField.Create ;
    lDataSet.Fields.Add(lField);
    lCell := TtiDataSetCell.Create ;
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

procedure TTestTIDataSet.TIDataSetItems;
var
  lDataSet : TtiDataSet ;
  lRow : TtiDataSetRow ;
begin
  lDataSet := TtiDataSet.Create ;
  try
    lRow := TtiDataSetRow.Create ;
    lDataSet.Add( lRow ) ;
    CheckEquals( 1, lDataSet.Count, 'Count' ) ;
    CheckSame( lRow, lDataSet.Items[0], 'Items[0]' );
    CheckSame( lRow, TObject(lDataSet.List.Items[0]), 'List.Items[0]' ) ;
    CheckSame( lDataSet, lRow.Owner, 'lRow.Owner' ) ;
    CheckEquals( 0, lRow.Index, 'lRow.Index' ) ;
    lRow := TtiDataSetRow.Create ;
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

procedure TTestTIDataSet.TIDataSetRowAddInstance;
var
  lDataSet : TtiDataSet ;
  lRow     : TtiDataSetRow ;
  lCell1   : TtiDataSetCell ;
  lCell2   : TtiDataSetCell ;
begin
  lDataSet := TtiDataSet.Create ;
  try
    lDataSet.Fields.AddInstance('field1', qfkString, 10);
    lDataSet.Fields.AddInstance('field2', qfkString, 10);
    lRow := TtiDataSetRow.Create ;
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

procedure TTestTIDataSet.TIDataSetRowItems;
var
  lDataSet     : TtiDataSet ;
  lDataSetRow  : TtiDataSetRow ;
  lDataSetCell : TtiDataSetCell ;
begin
  lDataSet     := TtiDataSet.Create ;
  try
    lDataSetRow  := TtiDataSetRow.Create;
    lDataSet.Add(lDataSetRow) ;
    lDataSetCell := TtiDataSetCell.Create ;
    lDataSetRow.Add( lDataSetCell ) ;
    CheckEquals( 1, lDataSetRow.Count, 'Count' ) ;
    CheckSame( lDataSetCell, lDataSetRow.Items[0], 'Items[0]' );
    CheckSame( lDataSetRow, lDataSetCell.Owner, 'lRow.Owner' ) ;
    CheckEquals( 0, lDataSetCell.Index, 'lRow.Index' ) ;
    lDataSetCell := TtiDataSetCell.Create ;
    lDataSetRow.Add( lDataSetCell ) ;
    CheckEquals( 2, lDataSetRow.Count, 'Count' ) ;
    CheckSame( lDataSetCell, lDataSetRow.Items[1], 'Items[1]' );
    CheckSame( lDataSetRow, lDataSetCell.Owner, 'lRow.Owner' ) ;
    CheckEquals( 1, lDataSetCell.Index, 'lRow.Index' ) ;
  finally
    lDataSet.Free ;
  end ;
end;

procedure TTestTIDataSet.CSVToTIDataSet_Read1000Rows;
var
  lDataSet : TtiDataSet ;
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
  lDataSet := TtiDataSet.Create ;
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

procedure TTestTIDataSet.TearDown;
begin
  SysUtils.DeleteFile(TempFileName);
end;

{
procedure TTestTIDataSet.CommaInField;
begin

end;

procedure TTestTIDataSet.CrInField;
begin

end;

procedure TTestTIDataSet.LFInField;
begin

end;
}

procedure TTestTIDataSet.TIDataSetCellValueAsString;
var
  lCell : TtiDataSetCell ;
begin
  lCell := TtiDataSetCell.Create ;
  try
    lCell.ValueAsString:= 'test';
    CheckEquals('test', lCell.ValueAsString);
  finally
    lCell.Free;
  end;
end;

procedure TTestTIDataSet.TIDataSetCellValueAsBool;
var
  lCell : TtiDataSetCell ;
begin
  lCell := TtiDataSetCell.Create ;
  try
    lCell.ValueAsBool := True ;
    CheckEquals(true, lCell.ValueAsBool);
  finally
    lCell.Free;
  end;
end;

procedure TTestTIDataSet.TIDataSetCellValueAsDateTime;
var
  lCell : TtiDataSetCell ;
  lDate : TDateTime;
begin
  lDate := EncodeDate(2004, 01, 01) + EncodeTime(10, 20, 30, 00);
  lCell := TtiDataSetCell.Create ;
  try
    lCell.ValueAsDateTime := lDate;
    CheckEquals(lDate, lCell.ValueAsDateTime);
  finally
    lCell.Free;
  end;
end;

procedure TTestTIDataSet.TIDataSetCellValueAsInteger;
var
  lCell : TtiDataSetCell ;
begin
  lCell := TtiDataSetCell.Create ;
  try
    lCell.ValueAsInteger:= 1234567890;
    CheckEquals(1234567890, lCell.ValueAsInteger);
  finally
    lCell.Free;
  end;
end;

procedure TTestTIDataSet.TIDataSetCellValueAsReal;
var
  lCell : TtiDataSetCell ;
begin
  lCell := TtiDataSetCell.Create ;
  try
    lCell.ValueAsFloat := 1234.56789;
    CheckEquals(1234.56789, lCell.ValueAsFloat, 5);
  finally
    lCell.Free;
  end;
end;

procedure TTestTIDataSet.TIDataSetCellValueAsStream;
var
  lCell : TtiDataSetCell ;
  lStrFrom  : string ;
  lStrTo    : string ;
  lStream : TStream;
begin
  lStrFrom := tiCreateStringOfSize( 1000 ) ;
  lCell := TtiDataSetCell.Create ;
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
