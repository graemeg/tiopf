unit tiAutomapCriteria_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  {$IFDEF FPC}
  ,testregistry
  {$ENDIF}
  ,tiTestFramework
  ,tiObject
  ;

type

  TCTDBMTestObj1  = class(TtiObject);
  TCTDBMTestList1 = class(TtiObjectList);
  TCTDBMTestObj2  = class(TtiObject);
  TCTDBMTestList2 = class(TtiObjectList);
  TCTDBMTestObj3  = class(TtiObject);
  TCTDBMTestList3 = class(TtiObjectList);


  TTestAutomappingCriteria = class(TtiTestCaseWithDatabaseConnection)
  private
    procedure InserTtiObjectListForTesting;
    function  TestIntToFloat(pInt : Integer): extended;
    function  TestIntToDate(pInt : Integer): TDateTime;
    function  TestIntToBool(pInt : Integer): Boolean;
  protected
    procedure SetUp; override;

  published
    procedure TestSetupAndTearDown; virtual;
    procedure CollectionReadAllNoCriteria;
    procedure CollectionReadStringCriteria;
    procedure ItemsNoCriteria;
    procedure ItemsStringLike;
    procedure ItemsIntegerInArray;
    procedure ItemsIntegerNotInArray;
    procedure ItemsOID;
    procedure ItemsOwnerOID;

    procedure CollectionOrderedNoCriteria;
    procedure CollectionOrderedWithCriteria;

  end;

implementation
uses
  SysUtils
  ,tiOPFManager
  ,tiBOMsForTesting
  ,tiQuery
  ,tiOIDGUID
  ,Contnrs
  ,tiCriteria
 ;

const
  cGroupCount = 5;
  cItemCount  = 5;
  cItemTotal  = cGroupCount * cItemCount;


{ TTestAutomappingCriteria }

procedure TTestAutomappingCriteria.CollectionOrderedNoCriteria;
var
  lData : TtiObjectListForTesting;
  i : integer;
  lGroupVal : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListForTesting.Create;

  TtiCriteria(lData.Criteria).ClearAll;
  TtiCriteria(lData.Criteria).AddOrderBy('StrField', false);

  try
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cGroupCount, lData.Count, 'Failed on 1');
    for i := 0 to cGroupCount - 1 do
    begin
      lGroupVal := cGroupCount - i;  // reversed order
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].OID.AsString, 'Failed on Group.OID');
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].StrField, 'Failed on Group.StrField');
      CheckEquals(lGroupVal, lData.Items[i].IntField, 'Failed on Group.IntField');
      CheckNearEnough(TestIntToFloat(lGroupVal), lData.Items[i].FloatField, 'Failed on Group.FloatField');
      CheckEquals(cItemCount, lData.Items[i].Count, 'Failed on Group.Count');

    end;
  finally
    lData.Free;
  end;
end;


procedure TTestAutomappingCriteria.CollectionOrderedWithCriteria;
var
  lData : TtiObjectListForTesting;
  i : integer;
  lGroupVal : integer;
const cExpectedRecords = 3;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListForTesting.Create;

  TtiCriteria(lData.Criteria).ClearAll;
  TtiCriteria(lData.Criteria).AddOrderBy('StrField', false);
  TtiCriteria(lData.Criteria).AddIn('IntField', [3,4,5]);

  try
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cExpectedRecords, lData.Count, 'Failed on 1 ');


    for i := 0 to cExpectedRecords - 1 do
    begin
      lGroupVal := cGroupCount - i;  // reversed order
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].OID.AsString, 'Failed on Group.OID');
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].StrField, 'Failed on Group.StrField');
      CheckEquals(lGroupVal, lData.Items[i].IntField, 'Failed on Group.IntField');
      CheckNearEnough(TestIntToFloat(lGroupVal), lData.Items[i].FloatField, 'Failed on Group.FloatField');
      CheckEquals(cItemCount, lData.Items[i].Count, 'Failed on Group.Count');

    end;
  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.CollectionReadAllNoCriteria;
var
  lData : TtiObjectListForTesting;
  i : integer;
  j : integer;
  lGroupVal : integer;
  lItemVal, lOID : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListForTesting.Create;

  TtiCriteria(lData.Criteria).ClearAll;

  try
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cGroupCount, lData.Count, 'Failed on 1');
    for i := 0 to cGroupCount - 1 do
    begin
      lGroupVal := i + 1;
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].OID.AsString, 'Failed on Group.OID');
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].StrField, 'Failed on Group.StrField');
      CheckEquals(lGroupVal, lData.Items[i].IntField, 'Failed on Group.IntField');
      CheckNearEnough(TestIntToFloat(lGroupVal), lData.Items[i].FloatField, 'Failed on Group.FloatField');
      CheckEquals(cItemCount, lData.Items[i].Count, 'Failed on Group.Count');
      for j := 0 to cItemCount - 1 do
      begin
        lItemVal :=  j + 1;
        lOID:= i * cItemCount + lItemVal;
        CheckEquals(IntToStr(lOID), lData.Items[i].Items[j].OID.AsString, 'Failed on Item.OID');
        CheckEquals(IntToStr(lItemVal), lData.Items[i].Items[j].StrField, 'Failed on Item.StrField');
        CheckEquals(lItemVal, lData.Items[i].Items[j].IntField, 'Failed on Item.IntField');
        CheckNearEnough(TestIntToFloat(lItemVal), lData.Items[j].FloatField, 'Failed on Item.FloatField');
      end;
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.CollectionReadStringCriteria;
var
  lData : TtiObjectListForTesting;
  j : integer;
  lGroupVal : integer;
  lItemVal, lItemOid : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListForTesting.Create;

  try
    TtiCriteria(lData.Criteria).ClearAll;
    TtiCriteria(lData.Criteria).AddEqualTo('StrField', '2');

    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(1, lData.Count, 'Failed on 1');

    lGroupVal :=2;
    CheckEquals(IntToStr(lGroupVal), lData.Items[0].OID.AsString, 'Failed on Group.OID');
    CheckEquals(IntToStr(lGroupVal), lData.Items[0].StrField, 'Failed on Group.StrField');
    CheckEquals(lGroupVal, lData.Items[0].IntField, 'Failed on Group.IntField');
    CheckNearEnough(TestIntToFloat(lGroupVal), lData.Items[0].FloatField, 'Failed on Group.FloatField');
    CheckEquals(cItemCount, lData.Items[0].Count, 'Failed on Group.Count');
    for j := 0 to cItemCount - 1 do
    begin
      lItemVal := j + 1;
      lItemOid:= cItemCount + lItemVal;
      CheckEquals(IntToStr(lItemOid), lData.Items[0].Items[j].OID.AsString, 'Failed on Item.OID');
      CheckEquals(IntToStr(lItemVal), lData.Items[0].Items[j].StrField, 'Failed on Item.StrField');
      CheckEquals(lItemVal, lData.Items[0].Items[j].IntField, 'Failed on Item.IntField');
      CheckNearEnough(TestIntToFloat(lItemVal), lData.Items[0].Items[j].FloatField, 'Failed on Item.FloatField');
    end;

  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.ItemsNoCriteria;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;

  try
    TtiCriteria(lData.Criteria).ClearAll;
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cItemTotal, lData.Count, 'Failed on 1');
    for i := 0 to cItemTotal - 1 do
    begin
      CheckEquals(IntToStr(i+1), lData.Items[i].OID.AsString, 'Failed on Item.OID');
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.ItemsOID;
var
  lData : TtiObjectListNestedForTesting;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;
  ;

  try
    TtiCriteria(lData.Criteria).AddEqualTo('OID', '23');
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(1, lData.Count, 'Failed on 1');

    CheckEquals('23', lData.Items[0].OID.AsString, 'Failed on Item.OID');
  finally
    lData.Free;
  end;

end;

procedure TTestAutomappingCriteria.ItemsOwnerOID;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;
  ;

  try
    TtiCriteria(lData.Criteria).AddEqualTo('Owner.OID', '4');
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cItemCount, lData.Count, 'Failed on 1');

    for i := 0 to cItemCount - 1 do
    begin
      // can't directly check the owner as it doesn't exist
      // as we are loading items directly
      // but notes fields starts woth the group no...
      CheckEquals('4', lData.Items[0].NotesField[1], 'Failed on Item.OID');
    end;
  finally
    lData.Free;
  end;

end;

procedure TTestAutomappingCriteria.InserTtiObjectListForTesting;
  procedure _InsertGroup(pI : integer);
  var
    lQueryParams : TtiQueryParams;
  begin
    lQueryParams := TtiQueryParams.Create;
    try
      lQueryParams.SetValueAsString('OID',                IntToStr(pI));
      lQueryParams.SetValueAsString('Group_Str_Field',    IntToStr(pI));
      lQueryParams.SetValueAsInteger('Group_Int_Field',    pI);
      lQueryParams.SetValueAsFloat('Group_Float_Field',  TestIntToFloat(pI));
      lQueryParams.SetValueAsDateTime('Group_Date_Field',   TestIntToDate(pI));
      lQueryParams.SetValueAsBoolean('Group_Bool_Field',   TestIntToBool(pI));
      lQueryParams.SetValueAsString('Group_Notes_Field',   IntToStr(pI) + '<<<' + IntToStr(pI * 2));
      GTIOPFManager.InsertRow('Test_Group', lQueryParams, DatabaseName, PersistenceLayerName );
    finally
      lQueryParams.Free;
    end;
  end;

  procedure _InsertItem(AOID, pI, pJ : integer);
  var
    lQueryParams : TtiQueryParams;
  begin
    lQueryParams := TtiQueryParams.Create;
    try
      lQueryParams.SetValueAsString('OID',              IntToStr(AOID));
      lQueryParams.SetValueAsString('OID_Group',        IntToStr(pI));
      lQueryParams.SetValueAsString('Item_Str_Field',   IntToStr(pJ));
      lQueryParams.SetValueAsInteger('Item_Int_Field',   pJ);
      lQueryParams.SetValueAsFloat('Item_Float_Field', TestIntToFloat(pJ));
      lQueryParams.SetValueAsDateTime('Item_Date_Field', TestIntToDate(pJ));
      lQueryParams.SetValueAsBoolean('Item_Bool_Field',  TestIntToBool(pJ));
      lQueryParams.SetValueAsString('Item_Notes_Field', IntToStr(pI) + '<<<' + IntToStr(pJ) + '>>>>' + IntToStr(pI * 2));
      GTIOPFManager.InsertRow('Test_Item', lQueryParams, DatabaseName, PersistenceLayerName );
    finally
      lQueryParams.Free;
    end;
  end;

var
  i, j  : integer;
  lItemOID : integer;
begin
  lItemOID := 1;
  for i := 1 to cGroupCount do
  begin
    _InsertGroup(i);
    for j := 1 to cItemCount do
    begin
      _InsertItem(lItemOID, i, j);
      inc(lItemOID);
    end;
  end;
end;

procedure TTestAutomappingCriteria.ItemsIntegerInArray;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
  myIntArray: array[0..1] of variant;
const
  cResultCount = cGroupCount * 2;
begin
  myIntArray[0]:= 2;
  myIntArray[1]:= 4;

  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;

  try
    TtiCriteria(lData.Criteria).AddIn('IntField', myIntArray);
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cResultCount, lData.Count, 'Failed on 1');
    for i := 0 to cResultCount - 1 do
    begin
      if i mod 2 = 0 then
        CheckEquals(2, lData.Items[i].IntField, 'Failed on Item ' + IntToStr(i))
      else
      CheckEquals(4, lData.Items[i].IntField, 'Failed on Item ' + IntToStr(i));
    end;
  finally
    lData.Free;
  end;
end;


procedure TTestAutomappingCriteria.ItemsIntegerNotInArray;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
  myIntArray: array[0..1] of variant;
const
  cResultCount = cGroupCount * (cItemCount - 2);
begin
  myIntArray[0]:= 2;
  myIntArray[1]:= 4;

  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;

  try
    TtiCriteria(lData.Criteria).AddNotIn('IntField', myIntArray);
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cResultCount, lData.Count, 'Failed on 1');
    for i := 0 to cResultCount - 1 do
    begin
      CheckNotEquals(2, lData.Items[i].IntField, 'Failed on Item ' + IntToStr(i));
      CheckNotEquals(4, lData.Items[i].IntField, 'Failed on Item ' + IntToStr(i));
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.ItemsStringLike;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
begin
  InserTtiObjectListForTesting;
  lData := TtiObjectListNestedForTesting.Create;

  try
    TtiCriteria(lData.Criteria).AddLike('NotesField', '%<3>%');
    lData.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(cGroupCount, lData.Count, 'Failed on 1');
    for i := 0 to cGroupCount - 1 do
    begin
      CheckEquals(IntToStr(3), lData.Items[i].StrField, 'Failed on Item ' + IntToStr(i));
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestAutomappingCriteria.SetUp;
begin
  inherited;
  CreateTestTables;
end;

function TTestAutomappingCriteria.TestIntToBool(pInt: Integer): Boolean;
begin
  result := (pInt mod 2 = 0);
end;

function TTestAutomappingCriteria.TestIntToDate(pInt: Integer): TDateTime;
begin
  result := EncodeDate(2002, 1, 1) + pInt;
end;

function TTestAutomappingCriteria.TestIntToFloat(pInt: Integer): extended;
begin
  result := pInt*2 / 10;
end;

procedure TTestAutomappingCriteria.TestSetupAndTearDown;
begin
  Check(true);
end;

end.






