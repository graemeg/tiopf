unit tiTextParserStructCSV_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework
  ,tiStructuredCSVReader
  ,Classes
 ;

const
  cExceptionNotRaisedWhenExpected = 'Exception not raised when expected';
  cTestExceptionText = 'A test exception';

type

  TTestTextParserStructCSV = class(TtiTestCase)
  private
    FParser:TTextParserStructCSV;
    FResults: TStringList;

    procedure OnGroup1Start(const pDataGroup: string);
    procedure OnGroup1Field1(const pDataGroup: string; const AFieldName: string; const AValue: string);
    procedure OnGroup1Field2(const pDataGroup: string; const AFieldName: string; const AValue: string);
    procedure OnGroup1End(const pDataGroup: string);

    procedure OnGroup2Start(const pDataGroup: string);
    procedure OnGroup2Field1(const pDataGroup: string; const AFieldName: string; const AValue: string);
    procedure OnGroup2Field2(const pDataGroup: string; const AFieldName: string; const AValue: string);
    procedure OnGroup2End(const pDataGroup: string);

    procedure OnGroup3Start(const pDataGroup: string);
    procedure OnGroup3Field1(const pDataGroup: string; const AFieldName: string; const AValue: string);
    procedure OnGroup3End(const pDataGroup: string);

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MetaDatasFindCreateByGroupName;
    procedure MetaDatasFindByGroupName;
    procedure MetaDataAddInstance;
    procedure ParseString_MetaDataOnly;
    procedure ParseString_SingleGroupCrLf;
    procedure ParseString_SingleGroupCr;
    procedure ParseString_SingleGroupOrderReversed;
    procedure ParseString_2GroupsMetaDataFirst;
    procedure ParseString_2GroupsMetaDataWithData;
    procedure ParseString_2GroupsMetaDataWithDataOrderReversed;

    procedure ParseError_IGroupNotRegistered;
    procedure ParseError_DGroupNotRegistered;
    procedure ParseError_IFieldNotRegistered;
    procedure ParseError_DuplicateGroup;
    procedure ParseError_DuplicateField;
    procedure ParseError_FirstColumnNotIOrD;
    procedure ParseError_MoreDataThanMetaData;
    procedure ParseError_MoreMetaDataThanData;
    procedure ParseError_ExceptionInPropSetter;
    procedure ParseError_MultipleErrors;
  end;


procedure RegisterTests;


implementation
uses
  SysUtils
  ,tiTestDependencies
 ;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTextParserStructCSV);
end;


procedure TTestTextParserStructCSV.MetaDataAddInstance;
var
  lMD : TtiStructCSVMetaData;
  lI1 : TtiStructCSVMetaDataItem;
  lI2 : TtiStructCSVMetaDataItem;
begin
  lMD := TtiStructCSVMetaData.Create;
  try
    lI1:= lMD.AddInstance('test1', OnGroup1Field1);
    CheckNotNull(lI1, 'lI1');
    CheckEquals('test1', lI1.FieldName, 'lI2.FieldName');
    lI2:= lMD.AddInstance('test2', OnGroup1Field1);
    CheckNotNull(lI1, 'lI2');
    CheckEquals('test2', lI2.FieldName, 'lI.2FieldName');
    Check(lI1 <> lI2, 'lI1 <> lI2');

    try
      lMD.AddInstance('test1', OnGroup1Field1);
      Fail('Exception should have been raised');
    except
      on e:Exception do
        CheckIs(e, Exception);
    end;
  finally
    lMD.Free;
  end;
end;


procedure TTestTextParserStructCSV.MetaDatasFindByGroupName;
var
  lMDs : TtiStructCSVMetaDatas;
  lMD1 : TtiStructCSVMetaData;
  lMD2 : TtiStructCSVMetaData;
  lMD3 : TtiStructCSVMetaData;
begin
  lMDs := TtiStructCSVMetaDatas.Create;
  try
    lMD1:= lMDs.FindByDataGroupName('group1');
    CheckNull(lMD1, 'lMD1');
    lMDs.AddInstance('group1', 'field1',
        OnGroup1Start,
        OnGroup1Field1,
        OnGroup1End);
    lMD1:= lMDs.FindByDataGroupName('group1');
    CheckNotNull(lMD1, 'lMD1');
    CheckEquals('group1', lMD1.DataGroupName);

    lMDs.AddInstance('group2', 'field1',
        OnGroup1Start,
        OnGroup1Field1,
        OnGroup1End);
    lMD2:= lMDs.FindByDataGroupName('group2');
    CheckNotNull(lMD1, 'lMD2');
    CheckEquals('group2', lMD2.DataGroupName);

    lMD3:= lMDs.FindByDataGroupName('group1');
    CheckNotNull(lMD3, 'lMD1');
    CheckEquals('group1', lMD3.DataGroupName);
    CheckSame(lMD1, lMD3, 'lMD1 <> lMD3');
  finally
    lMDs.Free;
  end;
end;


procedure TTestTextParserStructCSV.OnGroup1Field1(const pDataGroup, AFieldName: string; const AValue: string);
begin
  FResults.Add('EVENT_11,' +
               pDataGroup + ',' +
               AFieldName + ',' +
               AValue);
end;

procedure TTestTextParserStructCSV.OnGroup1Field2(const pDataGroup, AFieldName: string; const AValue: string);
begin
  FResults.Add('EVENT_12,' +
               pDataGroup + ',' +
               AFieldName + ',' +
               AValue);
end;

procedure TTestTextParserStructCSV.OnGroup2Field1(const pDataGroup, AFieldName: string; const AValue: string);
begin
  FResults.Add('EVENT_21,' +
               pDataGroup + ',' +
               AFieldName + ',' +
               AValue);
end;

procedure TTestTextParserStructCSV.OnGroup2Field2(const pDataGroup, AFieldName: string; const AValue: string);
begin
  FResults.Add('EVENT_22,' +
               pDataGroup + ',' +
               AFieldName + ',' +
               AValue);
end;

procedure TTestTextParserStructCSV.ParseString_MetaDataOnly;
var
  lMD : TtiStructCSVMetaData;
const
  cString = 'I,GROUP_1,FIELD_12,FIELD_11';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');
  CheckEquals(0, FResults.Count, 'FResults.Count');
end;

procedure TTestTextParserStructCSV.ParseString_SingleGroupCrLf;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_12,FIELD_11' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_11';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');
  CheckEquals(3, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[1], 'FResults.Strings[1]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[2], 'FResults.Strings[1]');
end;

procedure TTestTextParserStructCSV.ParseString_SingleGroupCr;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_12,FIELD_11' + #13 +
    'D,GROUP_1,DATA_12,DATA_11';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');
  CheckEquals(3, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[1], 'FResults.Strings[0]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[2], 'FResults.Strings[1]');
end;

procedure TTestTextParserStructCSV.ParseString_SingleGroupOrderReversed;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_12,FIELD_11' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_11';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');
  CheckEquals(3, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[1], 'FResults.Strings[1]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[2], 'FResults.Strings[2]');
end;

procedure TTestTextParserStructCSV.ParseString_2GroupsMetaDataFirst;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'I,GROUP_2,FIELD_21,FIELD_22' + #13 + #10 +
    'D,GROUP_1,DATA_11,DATA_12'   + #13 + #10 +
    'D,GROUP_2,DATA_21,DATA_22';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_2');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_21', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_22', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  CheckEquals(6, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[1], 'FResults.Strings[1]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[2], 'FResults.Strings[2]');
  CheckEquals('ONGROUP_2,GROUP_2',                 FResults.Strings[3], 'FResults.Strings[3]');
  CheckEquals('EVENT_21,GROUP_2,FIELD_21,DATA_21', FResults.Strings[4], 'FResults.Strings[4]');
  CheckEquals('EVENT_22,GROUP_2,FIELD_22,DATA_22', FResults.Strings[5], 'FResults.Strings[5]');

end;

procedure TTestTextParserStructCSV.ParseString_2GroupsMetaDataWithData;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_1,DATA_11,DATA_12'   + #13 + #10 +
    'I,GROUP_2,FIELD_21,FIELD_22' + #13 + #10 +
    'D,GROUP_2,DATA_21,DATA_22';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_2');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_21', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_22', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  CheckEquals(6, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[1], 'FResults.Strings[1]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[2], 'FResults.Strings[2]');
  CheckEquals('ONGROUP_2,GROUP_2',                 FResults.Strings[3], 'FResults.Strings[3]');
  CheckEquals('EVENT_21,GROUP_2,FIELD_21,DATA_21', FResults.Strings[4], 'FResults.Strings[4]');
  CheckEquals('EVENT_22,GROUP_2,FIELD_22,DATA_22', FResults.Strings[5], 'FResults.Strings[5]');
end;

procedure TTestTextParserStructCSV.ParseString_2GroupsMetaDataWithDataOrderReversed;
var
  lMD : TtiStructCSVMetaData;
const
  cString =
    'I,GROUP_1,FIELD_12,FIELD_11' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_11'   + #13 + #10 +
    'I,GROUP_2,FIELD_22,FIELD_21' + #13 + #10 +
    'D,GROUP_2,DATA_22,DATA_21';
begin
  FParser.ParseString(cString);
  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_1');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_12', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_11', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  lMD := FParser.MetaDatas.FindByDataGroupName('GROUP_2');
  CheckNotNull(lMD, 'lMD');
  CheckEquals( 2, lMD.Count, 'lMD.Count');
  CheckEquals( 2, lMD.IndexedCount, 'lMD.IndexedCount');
  CheckNotNull(lMD.IndexedItems[0], 'lMD.IndexedItems[0]');
  CheckNotNull(lMD.IndexedItems[1], 'lMD.IndexedItems[1]');
  CheckEquals( 'FIELD_22', lMD.IndexedItems[0].FieldName, 'lMD.IndexedItems[0].FieldName');
  CheckEquals( 'FIELD_21', lMD.IndexedItems[1].FieldName, 'lMD.IndexedItems[1].FieldName');

  CheckEquals(6, FResults.Count, 'FResults.Count');
  CheckEquals('ONGROUP_1,GROUP_1',                 FResults.Strings[0], 'FResults.Strings[0]');
  CheckEquals('EVENT_12,GROUP_1,FIELD_12,DATA_12', FResults.Strings[1], 'FResults.Strings[1]');
  CheckEquals('EVENT_11,GROUP_1,FIELD_11,DATA_11', FResults.Strings[2], 'FResults.Strings[2]');
  CheckEquals('ONGROUP_2,GROUP_2',                 FResults.Strings[3], 'FResults.Strings[3]');
  CheckEquals('EVENT_22,GROUP_2,FIELD_22,DATA_22', FResults.Strings[4], 'FResults.Strings[4]');
  CheckEquals('EVENT_21,GROUP_2,FIELD_21,DATA_21', FResults.Strings[5], 'FResults.Strings[5]');
end;


procedure TTestTextParserStructCSV.SetUp;
begin
  inherited;
  FParser := TTextParserStructCSV.Create;
  FParser.MetaDatas.AddInstance('GROUP_1', 'FIELD_11',
        OnGroup1Start,
        OnGroup1Field1,
        OnGroup1End);
  FParser.MetaDatas.AddInstance('GROUP_1', 'FIELD_12',
        OnGroup1Start,
        OnGroup1Field2,
        OnGroup1End);
  FParser.MetaDatas.AddInstance('GROUP_2', 'FIELD_21',
        OnGroup2Start,
        OnGroup2Field1,
        OnGroup2End);
  FParser.MetaDatas.AddInstance('GROUP_2', 'FIELD_22',
        OnGroup2Start,
        OnGroup2Field2,
        OnGroup2End);
  FParser.MetaDatas.AddInstance('GROUP_3', 'FIELD_31',
        OnGroup3Start,
        OnGroup3Field1,
        OnGroup3End);
  FResults:= TStringList.Create;
end;


procedure TTestTextParserStructCSV.TearDown;
begin
  FParser.Free;
  FResults.Free;
  inherited;
end;


procedure TTestTextParserStructCSV.ParseError_ExceptionInPropSetter;
var
  le : ETextParserStructCSVSetterException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_3,FIELD_31' + #13 + #10 +
    'D,GROUP_3,DATA_31';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVSetterException);
      le := ETextParserStructCSVSetterException(e);
      CheckEquals(cErrorInFieldSetter, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals(cTestExceptionText,  le.SetterErrorMessage, 'le.SetterErrorMessage');
      CheckEquals('DATA_31', le.Token, 'E.Token');
      CheckEquals(2, le.Row, 'E.Row');
      // Slight error here, because a trailing "," is
      // expected in determining the col pos
      CheckEquals(10, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_IFieldNotRegistered;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_13,FIELD_12' + #13 + #10 +
    'D,GROUP_2,DATA_12,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorIColumnNotFound, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('FIELD_13', le.Token, 'E.Token');
      CheckEquals(1, le.Row, 'E.Row');
      CheckEquals(11, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_FirstColumnNotIOrD;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'GROUP_2,DATA_12,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorFirstCharNotIorD, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('GROUP_2', le.Token, 'E.Token');
      CheckEquals(2, le.Row, 'E.Row');
      CheckEquals(1, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_IGroupNotRegistered;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_4,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_2,DATA_11,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorIGroupNotFound, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('GROUP_4', le.Token, 'E.Token');
      CheckEquals(1, le.Row, 'E.Row');
      CheckEquals(3, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_DGroupNotRegistered;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_4,DATA_11,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorDGroupNotFound, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('GROUP_4', le.Token, 'E.Token');
      CheckEquals(2, le.Row, 'E.Row');
      CheckEquals(3, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_MoreDataThanMetaData;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_12,DATA_13';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorMoreDataThanMetaData, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('DATA_13', le.Token, 'E.Token');
      CheckEquals(2,  le.Row, 'E.Row');
      // Slight error here, because a trailing "," is
      // expected in determining the col pos
      CheckEquals(26, le.Col, 'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.ParseError_MoreMetaDataThanData;
var
  le : ETextParserStructCSVException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_1,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVException);
      le := ETextParserStructCSVException(e);
      CheckEquals(cErrorMoreMetaDataThanData, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('DATA_12', le.Token, 'E.Token');
      CheckEquals(2,  le.Row,   'E.Row');
      // Slight error here, because a trailing "," is
      // expected in determining the col pos
      CheckEquals(10, le.Col,   'E.Col');
    end;
  end;
end;


procedure TTestTextParserStructCSV.MetaDatasFindCreateByGroupName;
var
  lMDs : TtiStructCSVMetaDatas;
  lMD1 : TtiStructCSVMetaData;
  lMD2 : TtiStructCSVMetaData;
  lMD3 : TtiStructCSVMetaData;
begin
  lMDs := TtiStructCSVMetaDatas.Create;
  try
    lMD1:= lMDs.FindCreateByDataGroupName('test1',
          OnGroup1Start,
          OnGroup1End);
    CheckEquals(1, lMDs.Count, 'lMDs.Count');
    CheckNotNull(lMD1, 'lMD1');
    CheckEquals('test1', lMD1.DataGroupName, 'lMD1.DataGroupName');
    CheckSame(lMD1, lMDs.LatestUsed, 'lMD1 <> lMDs.LatestUsed');

    lMD2 := lMDs.FindCreateByDataGroupName('test2',
          OnGroup2Start,
          OnGroup2End);
    CheckEquals(2, lMDs.Count, 'lMDs.Count');
    CheckNotNull(lMD2, 'lMD21');
    CheckEquals('test2', lMD2.DataGroupName, 'lMD2.DataGroupName');
    CheckSame(lMD2, lMDs.LatestUsed, 'lMD2 <> lMDs.LatestUsed');

    lMD3:= lMDs.FindCreateByDataGroupName('test1',
          OnGroup1Start,
          OnGroup2End);
    CheckEquals(2, lMDs.Count, 'lMDs.Count');
    CheckNotNull(lMD3, 'lMD3');
    CheckEquals('test1', lMD3.DataGroupName, 'lMD3.DataGroupName');
    CheckSame(lMD1, lMD3);
    CheckSame(lMD3, lMDs.LatestUsed, 'lMD3 <> lMDs.LatestUsed');
  finally
    lMDs.Free;
  end;
end;


procedure TTestTextParserStructCSV.OnGroup3Field1(const pDataGroup, AFieldName, AValue: string);
begin
  raise Exception.Create(cTestExceptionText);
end;


procedure TTestTextParserStructCSV.ParseError_MultipleErrors;
var
  ls : string;
begin
  FParser.LogExceptions := True;

  ls := 'I,GROUP_1,FIELD_11,FIELD_13' + #13 + #10;
  FParser.ParseString(ls);
  CheckEquals(1, FParser.ExceptionMessages.Count, '#1');

  ls := ls + 'D,GROUP_1,DATA_11,DATA_12'   + #13 + #10;
  FParser.ParseString(ls);
  CheckEquals(2, FParser.ExceptionMessages.Count, '#2');

  ls := ls + 'I,GROUP_2,FIELD_21,FIELD_23' + #13 + #10;
  FParser.ParseString(ls);
  CheckEquals(3, FParser.ExceptionMessages.Count, '#3');

  ls := ls + 'D,GROUP_2,DATA_11,DATA_12'   + #13 + #10;
  FParser.ParseString(ls);
  CheckEquals(4, FParser.ExceptionMessages.Count, '#4');

  ls := ls + 'I,GROUP_3,FIELD_31,FIELD_32' + #13 + #10;
  FParser.ParseString(ls);
  CheckEquals(5, FParser.ExceptionMessages.Count, '#5');

  ls := ls + 'D,GROUP_3,DATA_99';
  FParser.ParseString(ls);
  CheckEquals(6, FParser.ExceptionMessages.Count, '#6');

end;

procedure TTestTextParserStructCSV.ParseError_DuplicateField;
var
  le : ETextParserStructCSVDataGroupException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12,FIELD_11' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_12';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVDataGroupException);
      le := ETextParserStructCSVDataGroupException(e);
      CheckEquals(cErrorDuplicateColumnName, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('GROUP_1', le.DataGroupName, 'E.DataGroupName');
      CheckEquals('FIELD_11', le.Token, 'E.Token');
      CheckEquals(1, le.Row, 'E.Row');
      CheckEquals(29, le.Col, 'E.Col');
    end;
  end;
end;

procedure TTestTextParserStructCSV.ParseError_DuplicateGroup;
var
  le : ETextParserStructCSVDataGroupException;
const
  cString =
  //          10        20
  // 12345678901234567890123456789
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10 +
    'D,GROUP_1,DATA_12,DATA_12' + #13 + #10 +
    'I,GROUP_1,FIELD_11,FIELD_12' + #13 + #10;
//    'D,GROUP_1,DATA_22,DATA_22';
begin
  try
    FParser.ParseString(cString);
    Fail(cExceptionNotRaisedWhenExpected);
  except
    on e:Exception do
    begin
      CheckIs(e, ETextParserStructCSVDataGroupException);
      le := ETextParserStructCSVDataGroupException(e);
      CheckEquals(cErrorDuplicateGroupName, le.UnFormattedMessage, 'le.UnFormattedMessage');
      CheckEquals('GROUP_1', le.DataGroupName, 'E.DataGroupName');
      CheckEquals('', le.Token, 'E.Token');
      CheckEquals(3, le.Row, 'E.Row');
      CheckEquals(3, le.Col, 'E.Col');
    end;
  end;
end;

procedure TTestTextParserStructCSV.OnGroup1Start(const pDataGroup: string);
begin
  FResults.Add('ONGROUP_1,' +
               pDataGroup);
end;

procedure TTestTextParserStructCSV.OnGroup2Start(const pDataGroup: string);
begin
  FResults.Add('ONGROUP_2,' +
               pDataGroup);
end;

procedure TTestTextParserStructCSV.OnGroup3Start(const pDataGroup: string);
begin
  FResults.Add('ONGROUP_3,' +
               pDataGroup);
end;

procedure TTestTextParserStructCSV.OnGroup1End(const pDataGroup: string);
begin

end;

procedure TTestTextParserStructCSV.OnGroup2End(const pDataGroup: string);
begin

end;

procedure TTestTextParserStructCSV.OnGroup3End(const pDataGroup: string);
begin

end;

end.
