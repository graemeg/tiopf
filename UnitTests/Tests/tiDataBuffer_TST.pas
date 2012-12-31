unit tiDataBuffer_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework,
  Classes;

type

  TtiDataBufferTestCase = class(TtiTestCase)
  private
    FStringList: TStringList;
    procedure   DoExtractToken(AIndex: integer; const AValue: string);
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  public
    constructor Create{$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF}; override;
    destructor  Destroy; override;
  published
    procedure   DataBufferList_Add;
    procedure   DataBufferList_AddInstance;
    procedure   DataBufferList_Items;
    procedure   DataBufferList_Clear;
    procedure   DataBufferList_Count;
    procedure   DataBufferList_FindByName;
    procedure   DataBufferList_FindCell;
    procedure   DataBufferList_Remove;
    procedure   DataBufferList_Extract;

    procedure   DataBuffer_Add;
    procedure   DataBuffer_AddInstance;
    procedure   DataBuffer_Items;
    procedure   DataBuffer_Clear;
    procedure   DataBuffer_ClearRows;
    procedure   DataBuffer_Count;
    procedure   DataBuffer_List;
    procedure   DataBuffer_IndexOf;
    procedure   DataBuffer_FindByFieldValue;
    procedure   DataBuffer_FindCell;
    procedure   DataBuffer_Remove;
    procedure   DataBuffer_Delete;

    procedure   DataBufferRow_Items;
    procedure   DataBufferRow_AddInstance;
    procedure   DataBufferRow_Owner;
    procedure   DataBufferRow_Add;
    procedure   DataBufferRow_IndexOf;
    procedure   DataBufferRow_Count;
    procedure   DataBufferRow_Index;
    procedure   DataBufferRow_FindByFieldName;
    procedure   DataBufferRow_FieldAsString;
    procedure   DataBufferRow_FieldAsFloat;
    procedure   DataBufferRow_FieldAsBoolean;
    procedure   DataBufferRow_FieldAsInteger;
    procedure   DataBufferRow_FieldAsDateTime;

    procedure   DataBufferCell_DataSetField;
    procedure   DataBufferCell_ValueAsString;
    procedure   DataBufferCell_ValueAsInteger;
    procedure   DataBufferCell_ValueAsBool;
    procedure   DataBufferCell_ValueAsReal;
    procedure   DataBufferCell_ValueAsDateTime;
    procedure   DataBufferCell_AssignToAndFromStream;
    procedure   DataBufferCell_Owner;
    procedure   DataBufferCell_Index;
    procedure   DataBufferCell_Name;

    procedure   ExtractToken;
    procedure   CSVToDataBuffer_Props;
    procedure   CSVToDataBuffer_SaveHeaderOnly;
    procedure   CSVToDataBuffer_SaveNoHeader;
    procedure   CSVToDataBuffer_SaveFieldNames;
    procedure   CSVToDataBuffer_ReadHeaderOnly;
    procedure   CSVToDataBuffer_ReadDataOnly;
    procedure   CSVToDataBuffer_ReadHeaderAndData;
    procedure   CSVToDataBuffer_Read1000Rows;

    procedure  Insert10000RowsOf10FieldsAndFindItems;
  end;


procedure RegisterTests;


implementation
uses
  tiTestDependencies,
  tiXMLToTIDataSet,
  tiDataBuffer_BOM,
  tiDataBuffer_Cli,
  tiQueryTXTAbs,
  tiQuery,
  tiUtils,
  {$IFNDEF VER130}
  Variants,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils;

var
  UTempFileName : string;

function TempFileName: string;
begin
  if Length(UTempFileName) = 0 then
  begin
    UTempFileName := tiAddTrailingSlash(tiGetTempDir)  + 'temp.txt';
    end;
  result := UTempFileName;
end;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiDataBufferTestCase);
end;


{ TTesTtiDataBuffer }

constructor TtiDataBufferTestCase.Create{$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  FStringList := TStringList.Create;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_Props;
var
  LData : TCSVToTIDataSet;
begin
  LData := TCSVToTIDataSet.Create;
  try
    Check([tfmdFieldName] = LData.TextFileMetaData, 'TextFileMetaData');
    CheckEquals(',', LData.FieldDelim, 'FieldDelim');
    CheckEquals('"', LData.StringDelim, 'StringDelim');
    CheckEquals(tiLineEnd, LData.RowDelim, 'RowDelim');

    LData.TextFileMetaData := [];
    LData.FieldDelim := '|';
    LData.StringDelim := '';
    LData.RowDelim := Cr;

    Check([] = LData.TextFileMetaData, 'TextFileMetaData');
    CheckEquals('|', LData.FieldDelim, 'FieldDelim');
    CheckEquals('', LData.StringDelim, 'StringDelim');
    CheckEquals(Cr, LData.RowDelim, 'RowDelim');
  finally
    LData.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_ReadHeaderAndData;
var
  LDataBuffer : TtiDataBuffer;
  LWriter : TCSVToTIDataSet;
begin
  tiStringToFile('fieldA,fieldB'+tiLineEnd+'a1,b1'+tiLineEnd+'a2,b2'+tiLineEnd, TempFileName);
  LDataBuffer := TtiDataBuffer.Create;
  try
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [tfmdFieldName];
      LWriter.Read(LDataBuffer, TempFileName);
      CheckEquals(2, LDataBuffer.Fields.Count, 'LDataBuffer.Fields.Count');
      CheckEquals('fieldA', LDataBuffer.Fields.Items[0].Name, 'fieldA');
      CheckEquals('fieldB', LDataBuffer.Fields.Items[1].Name, 'fieldB');
      Check(LDataBuffer.Fields.Items[0].Kind = qfkString, 'fieldA.Kind');
      Check(LDataBuffer.Fields.Items[1].Kind = qfkString, 'fieldB.Kind');
      CheckEquals(2, LDataBuffer.Count, 'DataSet.Count');
      CheckEquals(2, LDataBuffer.Items[0].Count, 'DataSet.Items[0].Count');
      CheckEquals('a1', LDataBuffer.Items[0].Items[0].ValueAsString);
      CheckEquals('b1', LDataBuffer.Items[0].Items[1].ValueAsString);
      CheckEquals(2, LDataBuffer.Items[1].Count, 'DataSet.Items[0].Count');
      CheckEquals('a2', LDataBuffer.Items[1].Items[0].ValueAsString);
      CheckEquals('b2', LDataBuffer.Items[1].Items[1].ValueAsString);
    finally
      LWriter.Free;
    end;
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_ReadDataOnly;
var
  LDataBuffer : TtiDataBuffer;
  LWriter : TCSVToTIDataSet;
begin
  tiStringToFile('a1,b1'+tiLineEnd+'a2,b2'+tiLineEnd, TempFileName);
  LDataBuffer := TtiDataBuffer.Create;
  try
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [];
      LWriter.Read(LDataBuffer, TempFileName);
      CheckEquals(2, LDataBuffer.Fields.Count, 'LDataBuffer.Fields.Count');
      CheckEquals('Field1', LDataBuffer.Fields.Items[0].Name, 'Field1');
      CheckEquals('Field2', LDataBuffer.Fields.Items[1].Name, 'Field2');
      Check(LDataBuffer.Fields.Items[0].Kind = qfkString, 'Field1.Kind');
      Check(LDataBuffer.Fields.Items[1].Kind = qfkString, 'Field2.Kind');
      CheckEquals(2, LDataBuffer.Count, 'DataSet.Count');
      CheckEquals(2, LDataBuffer.Items[0].Count, 'DataSet.Items[0].Count');
      CheckEquals('a1', LDataBuffer.Items[0].Items[0].ValueAsString);
      CheckEquals('b1', LDataBuffer.Items[0].Items[1].ValueAsString);
      CheckEquals(2, LDataBuffer.Items[1].Count, 'DataSet.Items[0].Count');
      CheckEquals('a2', LDataBuffer.Items[1].Items[0].ValueAsString);
      CheckEquals('b2', LDataBuffer.Items[1].Items[1].ValueAsString);
    finally
      LWriter.Free;
    end;
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_ReadHeaderOnly;
var
  LDataBuffer : TtiDataBuffer;
  LWriter : TCSVToTIDataSet;
begin
  tiStringToFile('fieldA,fieldB', TempFileName);
  LDataBuffer := TtiDataBuffer.Create;
  try
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [tfmdFieldName];
      LWriter.Read(LDataBuffer, TempFileName);
      CheckEquals(2, LDataBuffer.Fields.Count, 'LDataBuffer.Fields.Count');
      CheckEquals('fieldA', LDataBuffer.Fields.Items[0].Name, 'fieldA');
      CheckEquals('fieldB', LDataBuffer.Fields.Items[1].Name, 'fieldB');
      Check(LDataBuffer.Fields.Items[0].Kind = qfkString, 'fieldA.Kind');
      Check(LDataBuffer.Fields.Items[1].Kind = qfkString, 'fieldB.Kind');
      CheckEquals(0, LDataBuffer.Count, 'DataSet.Count');
    finally
      LWriter.Free;
    end;
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_SaveFieldNames;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
  LWriter : TCSVToTIDataSet;
  LS      : string;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LRow := LDataBuffer.AddInstance;
    LRow.Items[0].ValueAsString := 'a1';
    LRow.Items[1].ValueAsString := 'b1';
    LRow := LDataBuffer.AddInstance;
    LRow.Items[0].ValueAsString := 'a2';
    LRow.Items[1].ValueAsString := 'b2';
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [tfmdFieldName];
      LWriter.Save(LDataBuffer, TempFileName);
    finally
      LWriter.Free;
    end;
    LS := tiFileToString(TempFileName);
    CheckEquals('"field1","field2"'+tiLineEnd+'"a1","b1"'+tiLineEnd+'"a2","b2"'+tiLineEnd, LS);
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_SaveHeaderOnly;
var
  LDataBuffer : TtiDataBuffer;
  LWriter : TCSVToTIDataSet;
  LS      : string;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [tfmdFieldName];
      LWriter.Save(LDataBuffer, TempFileName);
    finally
      LWriter.Free;
    end;
    LS := tiFileToString(TempFileName);
    CheckEquals('"field1","field2"'+tiLineEnd, LS);
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.CSVToDataBuffer_SaveNoHeader;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
  LWriter : TCSVToTIDataSet;
  LS      : string;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LRow := LDataBuffer.AddInstance;
    LRow.Items[0].ValueAsString := 'a1';
    LRow.Items[1].ValueAsString := 'b1';
    LRow := LDataBuffer.AddInstance;
    LRow.Items[0].ValueAsString := 'a2';
    LRow.Items[1].ValueAsString := 'b2';
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [];
      LWriter.Save(LDataBuffer, TempFileName);
    finally
      LWriter.Free;
    end;
    LS := tiFileToString(TempFileName);
    CheckEquals('"a1","b1"'+tiLineEnd+'"a2","b2"'+tiLineEnd, LS);
  finally
    LDataBuffer.Free;
  end;
end;


destructor TtiDataBufferTestCase.Destroy;
begin
  FStringList.Free;
  inherited;
end;


procedure TtiDataBufferTestCase.DoExtractToken(AIndex: integer; const AValue: string);
begin
  if FStringList.Count < AIndex then
    FStringList.Add(AValue)
  else
    FStringList.Strings[AIndex]:= AValue;
end;


procedure TtiDataBufferTestCase.ExtractToken;
begin
  stExtractTokensL('test',
                    ',', '"', true, DoExtractToken);
  CheckEquals(1, FStringList.Count, '#1');
  CheckEquals('test', FStringList.Strings[0], '#2');
  FStringList.Clear;

  stExtractTokensL('test1,test2',
                    ',', '"', true, DoExtractToken);
  CheckEquals(2, FStringList.Count, '#3');
  CheckEquals('test1', FStringList.Strings[0], '#4');
  CheckEquals('test2', FStringList.Strings[1], '#5');
  FStringList.Clear;

  stExtractTokensL(',test2',
                    ',', '"', true, DoExtractToken);
  CheckEquals(2, FStringList.Count, '#6');
  CheckEquals('', FStringList.Strings[0], '#7');
  CheckEquals('test2', FStringList.Strings[1], '#8');
  FStringList.Clear;

  stExtractTokensL('test1,',
                    ',', '"', true, DoExtractToken);
  CheckEquals(2, FStringList.Count, '#9');
  CheckEquals('test1', FStringList.Strings[0], '#10');
  CheckEquals('', FStringList.Strings[1], '#11');
  FStringList.Clear;

  stExtractTokensL(',,test1,,',
                    ',', '"', false, DoExtractToken);
  CheckEquals(1, FStringList.Count, '#12');
  CheckEquals('test1', FStringList.Strings[0], '#13');
  FStringList.Clear;

  stExtractTokensL('"test1",test2',
                    ',', '"', true, DoExtractToken);
  CheckEquals(2, FStringList.Count, '#14');
  CheckEquals('test1', FStringList.Strings[0], '#15');
  CheckEquals('test2', FStringList.Strings[1], '#16');
  FStringList.Clear;

  stExtractTokensL('"test1,test2',
                    ',', '"', true, DoExtractToken);
  CheckEquals(1, FStringList.Count, '#17');
  CheckEquals('test1,test2', FStringList.Strings[0], '#18');
  FStringList.Clear;

  stExtractTokensL('test1",test2',
                    ',', '"', true, DoExtractToken);
  CheckEquals(1, FStringList.Count, '#19');
  CheckEquals('test1",test2', FStringList.Strings[0], '#20');
  FStringList.Clear;

  stExtractTokensL('"tes,t1","te,st2"',
                    ',', '"', true, DoExtractToken);
  CheckEquals(2, FStringList.Count, '#21');
  CheckEquals('tes,t1', FStringList.Strings[0], '#22');
  CheckEquals('te,st2', FStringList.Strings[1], '#23');
  FStringList.Clear;
end;


procedure TtiDataBufferTestCase.Insert10000RowsOf10FieldsAndFindItems;
var
  LFieldIndex, LRowIndex: integer;
  LDataBuffer: TtiDataBuffer;
  LField: TtiDBMetaDataField;
  LDataBufferRow: TtiDataBufferRow;
  LDataBufferCell: TtiDataBufferCell;
  LXMLWriter: TtiDataBufferToXMLWriter;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    for LFieldIndex := 1 to 10 do
    begin
      LField := TtiDBMetaDataField.Create;
      LField.Name := IntToStr(LFieldIndex);
      LField.Kind := qfkInteger;
      LField.Width := 10;
      LDataBuffer.Fields.Add(LField);
    end;

    for LRowIndex := 0 to 100000-1 do
    begin
      LDataBufferRow := TtiDataBufferRow.Create(LDataBuffer.Fields.Count);
      LDataBuffer.Add(LDataBufferRow);

      for LFieldIndex := 1 to LDataBuffer.Fields.Count do
      begin
        LDataBufferCell := TtiDataBufferCell.Create;
        LDataBufferCell.ValueAsInteger := LFieldIndex * LRowIndex;
        LDataBufferRow.Add(LDataBufferCell);
      end;
    end;
    CheckEquals(10, LDataBuffer.Fields.Count);
    CheckEquals(100000, LDataBuffer.Count);

    for LRowIndex := 0 to LDataBuffer.Count-1 do
    begin
      for LFieldIndex := 1 to LDataBuffer.Fields.Count do
      begin
        try
          LDataBufferRow := LDataBuffer.FindByFieldValue(IntToStr(LFieldIndex), IntToStr(LFieldIndex*LRowIndex));
        except
          raise Exception.Create('Row index: ' + IntToStr(LRowIndex) + ' Field index: ' + IntToStr(LFieldIndex));
        end;
        CheckEquals(10, LDataBufferRow.Count);
      end;
    end;

    for LRowIndex := 0 to LDataBuffer.Count-1 do
    begin
      LDataBufferRow := LDataBuffer.Items[LRowIndex];
      for LFieldIndex := 0 to LDataBuffer.Fields.Count-1 do
      begin
        LField := LDataBuffer.Fields.Items[LFieldIndex];
        LDataBufferCell := LDataBufferRow.FindByFieldName(LField.Name);
        CheckNotNull(LDataBufferCell);
      end;
    end;

    LXMLWriter := TtiDataBufferToXMLWriter.Create;
    try
      LXMLWriter.AssignFromTIDataSet(LDataBuffer);
      LXMLWriter.AsString;
    finally
      LXMLWriter.Free;
    end;
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.SetUp;
begin
  inherited;
  FStringList.Clear;
end;


procedure TtiDataBufferTestCase.DataBuffer_Add;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LRow:= TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LRow);
    CheckEquals(1, LDataBuffer.Count);
    CheckEquals(0, LRow.Count, 'Row.Count');
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_AddInstance;
var
  LDataBuffer : TtiDataBuffer;
  LRow : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LRow := LDataBuffer.AddInstance;
    CheckEquals(1, LDataBuffer.Count);
    CheckEquals(2,LRow.Count, 'Row.Count');
    CheckSame(LDataBuffer.Fields.Items[0], LRow.Items[0].DataSetField, '#1');
    CheckSame(LDataBuffer.Fields.Items[1], LRow.Items[1].DataSetField, '#2');
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_Clear;
var
  LDataBuffer : TtiDataBuffer;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LDataBuffer.AddInstance;
    CheckEquals(1, LDataBuffer.Count);
    CheckEquals(2, LDataBuffer.Fields.Count);
    LDataBuffer.Clear;
    CheckEquals(0, LDataBuffer.Count);
    CheckEquals(0, LDataBuffer.Fields.Count);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_ClearRows;
var
  LDataBuffer : TtiDataBuffer;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LDataBuffer.AddInstance;
    CheckEquals(1, LDataBuffer.Count);
    CheckEquals(2, LDataBuffer.Fields.Count);
    LDataBuffer.ClearRows;
    CheckEquals(0, LDataBuffer.Count);
    CheckEquals(2, LDataBuffer.Fields.Count);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_Count;
var
  LDataBuffer : TtiDataBuffer;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    CheckEquals(0, LDataBuffer.Count);
    LDataBuffer.AddInstance;
    CheckEquals(1, LDataBuffer.Count);
    LDataBuffer.AddInstance;
    CheckEquals(2, LDataBuffer.Count);
    LDataBuffer.AddInstance;
    CheckEquals(3, LDataBuffer.Count);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_Delete;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LRow1:= LDataBuffer.AddInstance;
    LRow2:= LDataBuffer.AddInstance;
    LRow3:= LDataBuffer.AddInstance;
    CheckEquals(3, LDataBuffer.Count);
    CheckSame(LRow1, LDataBuffer.Items[0]);
    CheckSame(LRow2, LDataBuffer.Items[1]);
    CheckSame(LRow3, LDataBuffer.Items[2]);

    LDataBuffer.Delete(0);
    CheckEquals(2, LDataBuffer.Count);
    CheckSame(LRow2, LDataBuffer.Items[0]);
    CheckSame(LRow3, LDataBuffer.Items[1]);

    LDataBuffer.Delete(0);
    CheckEquals(1, LDataBuffer.Count);
    CheckSame(LRow3, LDataBuffer.Items[0]);

    LDataBuffer.Delete(0);
    CheckEquals(0, LDataBuffer.Count);

  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_FindByFieldValue;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LRow1:= LDataBuffer.AddInstance;
    LRow1.FindByFieldName('field1').ValueAsString:= '11';
    LRow1.FindByFieldName('field2').ValueAsString:= '12';
    LRow2:= LDataBuffer.AddInstance;
    LRow2.FindByFieldName('field1').ValueAsString:= '21';
    LRow2.FindByFieldName('field2').ValueAsString:= '22';
    LRow3:= LDataBuffer.AddInstance;
    LRow3.FindByFieldName('field1').ValueAsString:= '31';
    LRow3.FindByFieldName('field2').ValueAsString:= '32';
    CheckSame(LRow1, LDataBuffer.FindByFieldValue('field1', '11'));
    CheckSame(LRow1, LDataBuffer.FindByFieldValue('field2', '12'));
    CheckSame(LRow2, LDataBuffer.FindByFieldValue('field1', '21'));
    CheckSame(LRow2, LDataBuffer.FindByFieldValue('field2', '22'));
    CheckSame(LRow3, LDataBuffer.FindByFieldValue('field1', '31'));
    CheckSame(LRow3, LDataBuffer.FindByFieldValue('field2', '32'));
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_FindCell;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LRow1:= LDataBuffer.AddInstance;
    LRow1.FindByFieldName('field1').ValueAsString:= '11';
    LRow1.FindByFieldName('field2').ValueAsString:= '12';
    LRow2:= LDataBuffer.AddInstance;
    LRow2.FindByFieldName('field1').ValueAsString:= '21';
    LRow2.FindByFieldName('field2').ValueAsString:= '22';
    LRow3:= LDataBuffer.AddInstance;
    LRow3.FindByFieldName('field1').ValueAsString:= '31';
    LRow3.FindByFieldName('field2').ValueAsString:= '32';
    CheckEquals('11', LDataBuffer.FindCell('field1', 0).ValueAsString);
    CheckEquals('12', LDataBuffer.FindCell('field2', 0).ValueAsString);
    CheckEquals('21', LDataBuffer.FindCell('field1', 1).ValueAsString);
    CheckEquals('22', LDataBuffer.FindCell('field2', 1).ValueAsString);
    CheckEquals('31', LDataBuffer.FindCell('field1', 2).ValueAsString);
    CheckEquals('32', LDataBuffer.FindCell('field2', 2).ValueAsString);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferCell_DataSetField;
var
  LDataBuffer : TtiDataBuffer;
  LRow : TtiDataBufferRow;
  LCell : TtiDataBufferCell;
  LField : TtiDBMetaDataField;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LField := TtiDBMetaDataField.Create;
    LDataBuffer.Fields.Add(LField);
    LRow := TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LRow);
    LCell := TtiDataBufferCell.Create;
    LRow.Add(LCell);
    CheckSame(LField, LCell.DataSetField, 'DataSetField');

    LField := TtiDBMetaDataField.Create;
    LDataBuffer.Fields.Add(LField);
    LCell := TtiDataBufferCell.Create;
    LRow.Add(LCell);
    CheckSame(LField, LCell.DataSetField, 'DataSetField');
    LDataBuffer.Fields.Remove(LField);
    try
      LCell.DataSetField;
      Fail('Exception should have been raised');
    except
      on e:exception do
      begin
        CheckEquals(cErrorTIDataSetCellMetaData, e.message);
      end;
    end;
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferCell_Index;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LDataBuffer.Fields.AddInstance('field3', qfkString);
    LRow := LDataBuffer.AddInstance;
    CheckEquals(0, LRow.FindByFieldName('field1').Index);
    CheckEquals(1, LRow.FindByFieldName('field2').Index);
    CheckEquals(2, LRow.FindByFieldName('field3').Index);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferCell_Name;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LDataBuffer.Fields.AddInstance('field3', qfkString);
    LRow := LDataBuffer.AddInstance;
    CheckEquals('field1', LRow.FindByFieldName('field1').Name);
    CheckEquals('field2', LRow.FindByFieldName('field2').Name);
    CheckEquals('field3', LRow.FindByFieldName('field3').Name);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferCell_Owner;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LRow := LDataBuffer.AddInstance;
    CheckSame(LRow, LRow.Items[0].Owner);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_IndexOf;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LRow1:= LDataBuffer.AddInstance;
    LRow2:= LDataBuffer.AddInstance;
    LRow3:= LDataBuffer.AddInstance;
    CheckEquals(0, LDataBuffer.IndexOf(LRow1));
    CheckEquals(1, LDataBuffer.IndexOf(LRow2));
    CheckEquals(2, LDataBuffer.IndexOf(LRow3));
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_Items;
var
  LDataBuffer : TtiDataBuffer;
  LRow : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LRow := TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LRow);
    CheckEquals(1, LDataBuffer.Count, 'Count');
    CheckSame(LRow, LDataBuffer.Items[0], 'Items[0]');
    CheckSame(LRow, TObject(LDataBuffer.List.Items[0]), 'List.Items[0]');
    CheckSame(LDataBuffer, LRow.Owner, 'lRow.Owner');
    CheckEquals(0, LRow.Index, 'lRow.Index');
    LRow := TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LRow);
    CheckEquals(2, LDataBuffer.Count, 'Count');
    CheckSame(LRow, LDataBuffer.Items[1], 'Items[1]');
    CheckSame(LRow, TObject(LDataBuffer.List.Items[1]), 'List.Items[1]');
    CheckSame(LDataBuffer, LRow.Owner, 'lRow.Owner');
    CheckEquals(1, LRow.Index, 'lRow.Index');
    LDataBuffer.Clear;
    CheckEquals(0, LDataBuffer.Count, 'Count');
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBuffer_List;
var
  LDataBuffer : TtiDataBuffer;
  LRow : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LRow:= LDataBuffer.AddInstance;
    CheckSame(LRow, LDataBuffer.Items[0]);
    CheckSame(LRow, LDataBuffer.List.Items[0]);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBuffer_Remove;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LRow1:= LDataBuffer.AddInstance;
    LRow2:= LDataBuffer.AddInstance;
    LRow3:= LDataBuffer.AddInstance;
    CheckEquals(3, LDataBuffer.Count);
    CheckSame(LRow1, LDataBuffer.Items[0]);
    CheckSame(LRow2, LDataBuffer.Items[1]);
    CheckSame(LRow3, LDataBuffer.Items[2]);

    LDataBuffer.Remove(LRow1);
    CheckEquals(2, LDataBuffer.Count);
    CheckSame(LRow2, LDataBuffer.Items[0]);
    CheckSame(LRow3, LDataBuffer.Items[1]);

    LDataBuffer.Remove(LRow2);
    CheckEquals(1, LDataBuffer.Count);
    CheckSame(LRow3, LDataBuffer.Items[0]);

    LDataBuffer.Remove(LRow3);
    CheckEquals(0, LDataBuffer.Count);

  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_Add;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
  LCell1: TtiDataBufferCell;
  LCell2: TtiDataBufferCell;
  LCell3: TtiDataBufferCell;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LDataBuffer.Fields.AddInstance('field3', qfkString);
    LRow:= TtiDataBufferRow.Create(0);
    LDataBuffer.Add(LRow);
    LCell1:= LRow.AddInstance;
    LCell2:= LRow.AddInstance;
    LCell3:= LRow.AddInstance;
    CheckSame(LCell1, LRow.Items[0]);
    CheckSame(LCell2, LRow.Items[1]);
    CheckSame(LCell3, LRow.Items[2]);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_AddInstance;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
  LCell1  : TtiDataBufferCell;
  LCell2  : TtiDataBufferCell;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString, 10);
    LDataBuffer.Fields.AddInstance('field2', qfkString, 10);
    LRow := TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LRow);
    LCell1 := LRow.AddInstance;
    LCell2 := LRow.AddInstance;
    CheckEquals(2,LRow.Count, 'Row.Count');
    CheckSame(LCell1, LRow.Items[0], '#1');
    CheckSame(LDataBuffer.Fields.Items[0], LCell1.DataSetField, '#2');
    CheckSame(LCell2, LRow.Items[1], '#3');
    CheckSame(LDataBuffer.Fields.Items[1], LCell2.DataSetField, '#4');
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferRow_Count;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LRow:= TtiDataBufferRow.Create(0);
    LDataBuffer.Add(LRow);
    CheckEquals(0, LRow.Count);
    LRow.AddInstance;
    CheckEquals(1, LRow.Count);
    LRow.AddInstance;
    CheckEquals(2, LRow.Count);
    LRow.AddInstance;
    CheckEquals(3, LRow.Count);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FieldAsBoolean;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkLogical);
    LRow:= LDataBuffer.AddInstance;
    LRow.FieldAsBoolean['field1']:= True;
    CheckEquals(True, LRow.FieldAsBoolean['field1']);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FieldAsDateTime;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
  LNow: TDateTime;
begin
  LNow:= Now;
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkDateTime);
    LRow:= LDataBuffer.AddInstance;
    LRow.FieldAsDateTime['field1']:= LNow;
    CheckNearEnough(LNow, LRow.FieldAsDateTime['field1']);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FieldAsFloat;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkFloat);
    LRow:= LDataBuffer.AddInstance;
    LRow.FieldAsFloat['field1']:= 123.456;
    CheckNearEnough(123.456, LRow.FieldAsFloat['field1']);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FieldAsInteger;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkInteger);
    LRow:= LDataBuffer.AddInstance;
    LRow.FieldAsInteger['field1']:= 123;
    CheckEquals(123, LRow.FieldAsInteger['field1']);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FieldAsString;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LRow:= LDataBuffer.AddInstance;
    LRow.FieldAsString['field1']:= 'test';
    CheckEquals('test', LRow.FieldAsString['field1']);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_FindByFieldName;
var
  LDataBuffer : TtiDataBuffer;
  LRow    : TtiDataBufferRow;
begin
  LDataBuffer := TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LDataBuffer.Fields.AddInstance('field3', qfkString);
    LRow:= LDataBuffer.AddInstance;
    CheckSame(LRow.Items[0], LRow.FindByFieldName('field1'));
    CheckSame(LRow.Items[1], LRow.FindByFieldName('field2'));
    CheckSame(LRow.Items[2], LRow.FindByFieldName('field3'));
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_Index;
var
  LDataBuffer: TtiDataBuffer;
  LRow1: TtiDataBufferRow;
  LRow2: TtiDataBufferRow;
  LRow3: TtiDataBufferRow;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LRow1:= LDataBuffer.AddInstance;
    LRow2:= LDataBuffer.AddInstance;
    LRow3:= LDataBuffer.AddInstance;
    CheckEquals(0, LRow1.Index);
    CheckEquals(1, LRow2.Index);
    CheckEquals(2, LRow3.Index);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_IndexOf;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
  LCell1: TtiDataBufferCell;
  LCell2: TtiDataBufferCell;
  LCell3: TtiDataBufferCell;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LDataBuffer.Fields.AddInstance('field1', qfkString);
    LDataBuffer.Fields.AddInstance('field2', qfkString);
    LDataBuffer.Fields.AddInstance('field3', qfkString);
    LRow:= TtiDataBufferRow.Create(0);
    LDataBuffer.Add(LRow);
    LCell1:= LRow.AddInstance;
    LCell2:= LRow.AddInstance;
    LCell3:= LRow.AddInstance;
    CheckEquals(0, LRow.IndexOf(LCell1));
    CheckEquals(1, LRow.IndexOf(LCell2));
    CheckEquals(2, LRow.IndexOf(LCell3));
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferRow_Items;
var
  LDataBuffer: TtiDataBuffer;
  LDataBufferRow: TtiDataBufferRow;
  LDataBufferCell: TtiDataBufferCell;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LDataBufferRow := TtiDataBufferRow.Create(2);
    LDataBuffer.Add(LDataBufferRow);
    LDataBufferCell := TtiDataBufferCell.Create;
    LDataBufferRow.Add(LDataBufferCell);
    CheckEquals(1, LDataBufferRow.Count, 'Count');
    CheckSame(LDataBufferCell, LDataBufferRow.Items[0], 'Items[0]');
    CheckSame(LDataBufferRow, LDataBufferCell.Owner, 'lRow.Owner');
    CheckEquals(0, LDataBufferCell.Index, 'lRow.Index');
    LDataBufferCell := TtiDataBufferCell.Create;
    LDataBufferRow.Add(LDataBufferCell);
    CheckEquals(2, LDataBufferRow.Count, 'Count');
    CheckSame(LDataBufferCell, LDataBufferRow.Items[1], 'Items[1]');
    CheckSame(LDataBufferRow, LDataBufferCell.Owner, 'lRow.Owner');
    CheckEquals(1, LDataBufferCell.Index, 'lRow.Index');
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferRow_Owner;
var
  LDataBuffer: TtiDataBuffer;
  LRow: TtiDataBufferRow;
begin
  LDataBuffer:= TtiDataBuffer.Create;
  try
    LRow := LDataBuffer.AddInstance;
    CheckSame(LDataBuffer, LRow.Owner);
  finally
    LDataBuffer.Free;
  end;
end;

procedure TtiDataBufferTestCase.CSVToDataBuffer_Read1000Rows;
var
  LDataBuffer : TtiDataBuffer;
  LWriter : TCSVToTIDataSet;
  LLine, LS : string;
  i, j : integer;
const
  cCols = 99;
  cRows = 100;
begin
  LLine := '';
  for j := 1 to cCols do
  begin
    if LLine <> '' then LLine := LLine + ',';
    LLine := LLine + 'field' + IntToStr(j);
  end;
  LS := LLine + tiLineEnd;

  for i := 0 to cRows do
  begin
    LLine := '';
    for j := 1 to cCols do
    begin
      if LLine <> '' then LLine := LLine + ',';
      LLine := LLine + 'value' + IntToStr(((i+1)*100)+(j));
    end;
    LS := LS + LLine + tiLineEnd;
  end;

  tiStringToFile(LS, TempFileName);
  LDataBuffer := TtiDataBuffer.Create;
  try
    LWriter := TCSVToTIDataSet.Create;
    try
      LWriter.TextFileMetaData := [tfmdFieldName];
      LWriter.Read(LDataBuffer, TempFileName);
      CheckEquals(cCols, LDataBuffer.Fields.Count, 'LDataBuffer.Fields.Count');
      for j := 1 to cCols do
        CheckEquals('field' + IntToStr(j), LDataBuffer.Fields.Items[j-1].Name, 'field'+IntToStr(j));
      for i := 0 to cRows do
        for j := 1 to cCols do
          CheckEquals('value' + IntToStr(((i+1)*100)+(j)), LDataBuffer.Items[i].Items[j-1].ValueAsString);
    finally
      LWriter.Free;
    end;
  finally
    LDataBuffer.Free;
  end;
end;


procedure TtiDataBufferTestCase.TearDown;
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


procedure TtiDataBufferTestCase.DataBufferCell_ValueAsString;
var
  LCell : TtiDataBufferCell;
begin
  LCell := TtiDataBufferCell.Create;
  try
    LCell.ValueAsString:= 'test';
    CheckEquals('test', LCell.ValueAsString);
  finally
    LCell.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferList_Add;
var
  LList: TtiDataBufferList;
  LItem: TtiDataBuffer;
begin
  LList:= TtiDataBufferList.Create;
  try
    LItem:= TtiDataBuffer.Create;
    LList.Add(LItem);
    CheckSame(LItem, LList.Items[0]);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_AddInstance;
var
  LList: TtiDataBufferList;
  LItem: TtiDataBuffer;
begin
  LList:= TtiDataBufferList.Create;
  try
    LItem:= LList.AddInstance;
    CheckSame(LItem, LList.Items[0]);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_Clear;
var
  LList: TtiDataBufferList;
begin
  LList:= TtiDataBufferList.Create;
  try
    LList.AddInstance;
    LList.AddInstance;
    LList.AddInstance;
    CheckEquals(3, LList.Count);
    LList.Clear;
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_Count;
var
  LList: TtiDataBufferList;
begin
  LList:= TtiDataBufferList.Create;
  try
    CheckEquals(0, LList.Count);
    LList.AddInstance;
    CheckEquals(1, LList.Count);
    LList.AddInstance;
    CheckEquals(2, LList.Count);
    LList.AddInstance;
    CheckEquals(3, LList.Count);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_Extract;
var
  LList: TtiDataBufferList;
  LItem1: TtiDataBuffer;
  LItem2: TtiDataBuffer;
begin
  LList:= nil;
  LItem1:= nil;
  LItem2:= nil;
  try
    LList:= TtiDataBufferList.Create;
    LItem1:= LList.AddInstance;
    LItem2:= LList.AddInstance;
    CheckEquals(2, LList.Count);
    LList.Extract(LItem1);
    CheckEquals(1, LList.Count);
    LList.Extract(LItem2);
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_FindByName;
var
  LList: TtiDataBufferList;
  LItem1: TtiDataBuffer;
  LItem2: TtiDataBuffer;
  LItem3: TtiDataBuffer;
begin
  LList:= TtiDataBufferList.Create;
  try
    LItem1:= LList.AddInstance('item1');
    LItem2:= LList.AddInstance('item2');
    LItem3:= LList.AddInstance('item3');
    CheckSame(LItem1, LList.FindByName('item1'));
    CheckSame(LItem2, LList.FindByName('item2'));
    CheckSame(LItem3, LList.FindByName('item3'));
    CheckSame(LItem3, LList.FindByName('Item3')); // Not case sensitive
    CheckNull(LList.FindByName('wont be found'));
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_FindCell;
var
  LList: TtiDataBufferList;
  LBuffer1: TtiDataBuffer;
  LRow11: TtiDataBufferRow;
  LRow12: TtiDataBufferRow;
  LBuffer2: TtiDataBuffer;
  LRow21: TtiDataBufferRow;
  LRow22: TtiDataBufferRow;
begin
  LList:= TtiDataBufferList.Create;
  try
    LBuffer1:= LList.AddInstance('Buffer1');
    LBuffer1.Fields.AddInstance('Field11', qfkString);
    LBuffer1.Fields.AddInstance('Field12', qfkString);
    LRow11:= LBuffer1.AddInstance;
    LRow11.FieldAsString['Field11']:= 'Value111';
    LRow11.FieldAsString['Field12']:= 'Value112';
    LRow12:= LBuffer1.AddInstance;
    LRow12.FieldAsString['Field11']:= 'Value121';
    LRow12.FieldAsString['Field12']:= 'Value122';

    LBuffer2:= LList.AddInstance('Buffer2');
    LBuffer2.Fields.AddInstance('Field21', qfkString);
    LBuffer2.Fields.AddInstance('Field22', qfkString);
    LRow21:= LBuffer2.AddInstance;
    LRow21.FieldAsString['Field21']:= 'Value211';
    LRow21.FieldAsString['Field22']:= 'Value212';
    LRow22:= LBuffer2.AddInstance;
    LRow22.FieldAsString['Field21']:= 'Value221';
    LRow22.FieldAsString['Field22']:= 'Value222';

    CheckEquals('Value111', LList.FindCell('Buffer1', 'Field11', 0).ValueAsString);
    CheckEquals('Value112', LList.FindCell('Buffer1', 'Field12', 0).ValueAsString);
    CheckEquals('Value121', LList.FindCell('Buffer1', 'Field11', 1).ValueAsString);
    CheckEquals('Value122', LList.FindCell('Buffer1', 'Field12', 1).ValueAsString);

    CheckEquals('Value211', LList.FindCell('Buffer2', 'Field21', 0).ValueAsString);
    CheckEquals('Value212', LList.FindCell('Buffer2', 'Field22', 0).ValueAsString);
    CheckEquals('Value221', LList.FindCell('Buffer2', 'Field21', 1).ValueAsString);
    CheckEquals('Value222', LList.FindCell('Buffer2', 'Field22', 1).ValueAsString);

  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_Items;
var
  LList: TtiDataBufferList;
  LItem0: TtiDataBuffer;
  LItem1: TtiDataBuffer;
  LItem2: TtiDataBuffer;
begin
  LList:= TtiDataBufferList.Create;
  try
    LItem0:= LList.AddInstance;
    LItem1:= LList.AddInstance;
    LItem2:= LList.AddInstance;
    CheckSame(LItem0, LList.Items[0]);
    CheckSame(LItem1, LList.Items[1]);
    CheckSame(LItem2, LList.Items[2]);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferList_Remove;
var
  LList: TtiDataBufferList;
  LItem1: TtiDataBuffer;
  LItem2: TtiDataBuffer;
begin
  LList:= TtiDataBufferList.Create;
  try
    LItem1:= LList.AddInstance;
    LItem2:= LList.AddInstance;
    CheckEquals(2, LList.Count);
    LList.Remove(LItem1);
    CheckEquals(1, LList.Count);
    LList.Remove(LItem2);
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;
end;

procedure TtiDataBufferTestCase.DataBufferCell_ValueAsBool;
var
  LCell : TtiDataBufferCell;
begin
  LCell := TtiDataBufferCell.Create;
  try
    LCell.ValueAsBool := True;
    CheckEquals(true, LCell.ValueAsBool);
  finally
    LCell.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferCell_ValueAsDateTime;
var
  LCell : TtiDataBufferCell;
  LDate : TDateTime;
begin
  LDate := EncodeDate(2004, 01, 01) + EncodeTime(10, 20, 30, 00);
  LCell := TtiDataBufferCell.Create;
  try
    LCell.ValueAsDateTime := LDate;
    CheckEquals(LDate, LCell.ValueAsDateTime);
  finally
    LCell.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferCell_ValueAsInteger;
var
  LCell : TtiDataBufferCell;
begin
  LCell := TtiDataBufferCell.Create;
  try
    LCell.ValueAsInteger:= 1234567890;
    CheckEquals(1234567890, LCell.ValueAsInteger);
  finally
    LCell.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferCell_ValueAsReal;
var
  LCell : TtiDataBufferCell;
begin
  LCell := TtiDataBufferCell.Create;
  try
    LCell.ValueAsFloat := 1234.56789;
    CheckEquals(1234.56789, LCell.ValueAsFloat, 5);
  finally
    LCell.Free;
  end;
end;


procedure TtiDataBufferTestCase.DataBufferCell_AssignToAndFromStream;
var
  LCell : TtiDataBufferCell;
  LFrom : string;
  LTo   : string;
  LStream : TStream;
begin
  LFrom := tiCreateStringOfSize(1000);
  LCell := TtiDataBufferCell.Create;
  try
    LStream := TMemoryStream.Create;
    try
      tiStringToStream(LFrom, LStream);
      LCell.AssignFromStream(LStream);
    finally
      LStream.Free;
    end;

    LStream := TMemoryStream.Create;
    try
      LCell.AssignToStream(LStream);
      LTo := tiStreamToString(LStream);
    finally
      LStream.Free;
    end;

    CheckEquals(LFrom, LTo);
  finally
    LCell.Free;
  end;
end;

end.
