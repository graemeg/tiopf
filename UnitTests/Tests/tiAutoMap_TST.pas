unit tiAutoMap_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  ,tiBOMsForTesting
  {$IFDEF FPC}
  ,testregistry
  {$ENDIF}
  ,tiTestFramework
  ,tiOPFTestCase
  ,tiObject
  ;

type

  TCTDBMTestObj1  = class(TtiObject);
  TCTDBMTestList1 = class(TtiObjectList);
  TCTDBMTestObj2  = class(TtiObject);
  TCTDBMTestList2 = class(TtiObjectList);
  TCTDBMTestObj3  = class(TtiObject);
  TCTDBMTestList3 = class(TtiObjectList);


  TTestTIAutoMapFramework = class(TtiTestCase)
  published
    procedure TestClassMaps_AddClassMap;
    procedure TestClassMaps_FindCreate;
    procedure TestClassMaps_IsClassReg;
    procedure TestClassMaps_RegisterInheritance;
    procedure TestClassMaps_FindParent;
    procedure TestClassMaps_HasParent;
    procedure TestClassMaps_FindAllParents;

    procedure TestClassMap_AddAttrMap;

    procedure TestDBMaps_AddDBMap;
    procedure TtiDBMaps_FindCreate;

    procedure TestDBMap_AddTableMap;
    procedure TestDBMap_FindCreate;

    procedure TestDBTableMap_AddColMap;

    procedure TestAttrColMaps_AddMapping;
    procedure TestAttrColMaps_FindAllMappingsByMapToClass;
    procedure TestAttrColMaps_FindAllPKMappingsByMapToClass;
    procedure TestAttrColMaps_FindByClassAttrMap;

    procedure TestClassDBCollections_AddClassCollectionMappingSimple;
    procedure TestClassDBCollections_AddClassCollectionMappingFK;
    procedure TestClassDBCollections_FindByCollectionOf;
    procedure TestClassDBCollections_FindByCollection;
    procedure TestClassDBCollections_IsCollection;
    procedure TestClassDBCollections_IsInCollection;

    procedure TestClassDBMappingMgr_RegisterMapping;
    procedure TestClassDBMappingMgr_RegisterCollection;
    procedure TestClassDBMappingMgr_RegisterInheritance;

  end;

  TTestTIAutoMapOperation = class(TtiTestCaseWithDatabaseConnection)
  private
    procedure InserTtiObjectListForTesting(
      const AGroupCount, AItemCount: Integer);
    function  CreateTIOPFTestData : TtiObjectListForTesting;
    procedure UpdateTIOPFTestData(AData : TtiObjectListForTesting);
    function  TestIntToFloat(pInt : Integer): extended;
    function  TestIntToDate(pInt : Integer): TDateTime;
    function  TestIntToBool(pInt : Integer): Boolean;
    procedure InserTtiObjectListForTestingInherited(const pParentTableName, ATableName : string; pI: Integer; pOwnerOID : integer);
    procedure InserTtiObjectListForTestingInheritedGroup(AOID: integer);
  protected
    procedure SetUp; override;
    procedure DoReadWriteString(const pLen : integer);
    procedure DoReadWriteInteger(AValue: integer);
{$IFDEF TESTINT64}
    procedure DoReadWriteInt64(AValue: Int64);
{$ENDIF}
    procedure DoReadWriteFloat(AValue: extended);
    procedure DoReadWriteBoolean(AValue: boolean);
    procedure DoReadWriteDateTime(AValue: TDateTime);
    procedure DoReadWriteOID(AValue: string);

  published
    procedure TestSetupAndTearDown; virtual;

    procedure ReadWriteString1;
    procedure ReadWriteString10;
    procedure ReadWriteString100;
    procedure ReadWriteString255; virtual;
    procedure ReadWriteString256; virtual;
    procedure ReadWriteString257;
    procedure ReadWriteString511;
    procedure ReadWriteString512;
    procedure ReadWriteString513;
    procedure ReadWriteString1023;
    procedure ReadWriteString1024;
    procedure ReadWriteString1025;
    procedure ReadWriteString5000;
    procedure ReadWriteString10000;
    procedure ReadWriteIntegerLow;
    procedure ReadWriteInteger10;
    procedure ReadWriteInteger0;
    procedure ReadWriteInteger_10;
    procedure ReadWriteIntegerHigh;
{$IFDEF TESTINT64}
    procedure ReadWriteInt64Low;
    procedure ReadWriteInt6410;
    procedure ReadWriteInt640;
    procedure ReadWriteInt64_10;
    procedure ReadWriteInt64High;
    procedure ReadWriteInt64High1;
    procedure ReadWriteInt64High2;
    procedure ReadWriteInt64High3;
    procedure ReadWriteInt64Low1;
    procedure ReadWriteInt64Low2;
    procedure ReadWriteInt64Low3;
{$ENDIF}
    procedure ReadWriteFloat;
    procedure ReadWriteDateDate;
    procedure ReadWriteDateNow;
    procedure ReadWriteDateMin; virtual;
    procedure ReadWriteDate0;
    procedure ReadWriteDateMax;
    procedure ReadWriteBooleanTrue;
    procedure ReadWriteBooleanFalse;
    procedure ReadWriteStream;
    procedure ReadWriteOID1;
    procedure ReadWriteOID2;

    procedure SingleFlatObjReadThis;
    procedure SingleFlatObjCreate  ;
    procedure SingleFlatObjUpdate  ;
    procedure SingleFlatObjDelete  ;

    procedure SingleInheritedObjRead;
    procedure SingleInheritedObjCreateEachLevel;
    procedure SingleInheritedObjCreateAll;
    procedure SingleInheritedObjUpdateEachLevel;
    procedure SingleInheritedObjUpdateAll;
    procedure SingleInheritedObjDeleteEachLevel;
    procedure SingleInheritedObjDeleteAll;
    procedure CollectionOfInheritedObjRead;
    procedure CollectionOfInheritedObjWithFKRead;
    procedure CollectionOfInheritedObjWithOutFKRead;
    procedure CollectionReadPK;
    procedure CollectionReadPKThreaded; virtual;
    procedure CollectionReadAll;
    procedure CollectionReadAllItems;
    procedure CollectionCreate;
    procedure CollectionUpdate;
    procedure CollectionDelete;

  //    procedure CollectionReadPKByCondition;

  end;

procedure RegisterTests;

implementation
uses
  SysUtils
  ,tiOPFManager
  ,tiConstants
  ,tiAutoMap
  ,tiQuery
  ,tiUtils
  ,Contnrs
  ,tiTestDependencies
  ,tiThread
 ;

const
  cGroupCount = 5;
  cItemCount  = 5;
  cItemTotal  = cGroupCount * cItemCount;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIAutoMapFramework);
end;

procedure TTestTIAutoMapOperation.CollectionCreate;
var
  lData1   : TtiObjectListForTesting;
  lData2   : TtiObjectListForTesting;
  i, j : integer;
  lGroupVal : integer;
  lItemVal : integer;
begin

  lData1 := CreateTIOPFTestData;
  try
    lData1.Save(DatabaseName, PersistenceLayerName);

    lData2 := TtiObjectListForTesting.Create;
    try
      lData2.Read(DatabaseName, PersistenceLayerName);
      lData2.SortByOID;
      CheckEquals(cGroupCount, lData2.Count, 'Failed on 1');
      for i := 0 to cGroupCount - 1 do
      begin
        lGroupVal := i + 1;
        CheckEquals(IntToStr(lGroupVal), lData2.Items[i].OID.AsString, 'Failed on Group.OID');
        CheckEquals(IntToStr(lGroupVal), lData2.Items[i].StrField, 'Failed on Group.StrField');
        CheckEquals(lGroupVal, lData2.Items[i].IntField, 'Failed on Group.IntField');
        CheckNearEnough(TestIntToFloat(lGroupVal), lData2.Items[i].FloatField, 'Failed on Group.FloatField');
        CheckEquals(cItemCount, lData2.Items[i].Count, 'Failed on Group.Count');
        for j := 0 to cItemCount - 1 do
        begin
          lItemVal := j + 1;
          CheckEquals(IntToStr(lItemVal), lData2.Items[j].OID.AsString, 'Failed on Item.OID');
          CheckEquals(IntToStr(lItemVal), lData2.Items[j].StrField, 'Failed on Item.StrField');
          CheckEquals(lItemVal, lData2.Items[j].IntField, 'Failed on Item.IntField');
          CheckNearEnough(TestIntToFloat(lItemVal), lData2.Items[j].FloatField, 'Failed on Item.FloatField');
        end;
      end;
    finally
      lData2.Free;
    end;
  finally
    lData1.Free;
  end;

end;

procedure TTestTIAutoMapOperation.CollectionDelete;
var
  lData1 : TtiObjectListForTesting;
  lData2 : TtiObjectListForTesting;
begin
  lData1 := CreateTIOPFTestData;
  try
    GTIOPFManager.Save(lData1, DatabaseName, PersistenceLayerName);
    lData1.Deleted := true;
    GTIOPFManager.Save(lData1, DatabaseName, PersistenceLayerName );
    lData2 := TtiObjectListForTesting.Create;
    try
      GTIOPFManager.Read(lData2, DatabaseName, PersistenceLayerName );
      CheckObjectState(posClean, lData2);
      CheckEquals(0, lData2.Count, 'Failed on lData2.Count = 0');
    finally
      lData2.Free;
    end;
  finally
    lData1.Free;
  end;
end;

procedure TTestTIAutoMapOperation.CollectionReadAll;
var
  lData : TtiObjectListForTesting;
  i : integer;
  j : integer;
  lGroupVal : integer;
  lItemVal, lOID : integer;
begin
  InserTtiObjectListForTesting(CGroupCount, CItemCount);
  lData := TtiObjectListForTesting.Create;
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
        lItemVal := j + 1;
        lOID:= i * cItemCount + lItemVal;
        CheckEquals(IntToStr(lOID), lData.Items[i].Items[j].OID.AsString, 'Failed on Item.OID');
        CheckEquals(IntToStr(lItemVal), lData.Items[i].Items[j].StrField, 'Failed on Item.StrField');
        CheckEquals(lItemVal, lData.Items[i].Items[j].IntField, 'Failed on Item.IntField');
        CheckNearEnough(TestIntToFloat(lItemVal), lData.Items[i].Items[j].FloatField, 'Failed on Item.FloatField');
      end;
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestTIAutoMapOperation.CollectionReadAllItems;
var
  lData : TtiObjectListNestedForTesting;
  i : integer;
begin
  InserTtiObjectListForTesting(CGroupCount, CItemCount);
  lData := TtiObjectListNestedForTesting.Create;
  try
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

procedure TTestTIAutoMapOperation.CollectionUpdate;
var
  lData1   : TtiObjectListForTesting;
  lData2   : TtiObjectListForTesting;
  lDataStr1 : string;
  lDataStr2 : string;      
begin

  lData1 := CreateTIOPFTestData;
  try
    lData1.Save(DatabaseName, PersistenceLayerName);

    // ToDo: Should not have to do this.
    lData1.ObjectState := posClean;

    UpdateTIOPFTestData(lData1);

    lData1.Save(DatabaseName, PersistenceLayerName);

    lData2 := TtiObjectListForTesting.Create;
    try
      lData2.Read(DatabaseName, PersistenceLayerName);

      lDataStr1 := lData1.AsDebugString;
      lDataStr2 := lData2.AsDebugString;
      if lDataStr1 <> lDataStr2 then
      begin
        lDataStr1 := 'Strings are not the same' + Cr(2) +
                     'First string' + Cr(2) + lDataStr1;
        lDataStr2 := 'Strings are not the same' + Cr(2) +
                     'First string' + Cr(2) + lDataStr2;
//        tiShowString(lDataStr1);
//        tiShowString(lDataStr2);
      end;
      Check(lDataStr1 = lDataStr2, 'Read failed');
    finally
      lData2.Free;
    end;
  finally
    lData1.Free;
  end;

end;

function TTestTIAutoMapOperation.CreateTIOPFTestData: TtiObjectListForTesting;
var
  lGroup : TtiObjectListNestedForTesting;
  lItem : TtiOPFTestItem ;
  i, j  : integer;
  lItemOID : integer;
begin
  result := TtiObjectListForTesting.Create;
  lItemOID := 1;
  for i := 1 to cGroupCount do
  begin
    lGroup := TtiObjectListNestedForTesting.Create;
    lGroup.ObjectState := posCreate;
    lGroup.OID.AsString := IntToStr(i);
    lGroup.StrField := IntToStr(i);
    lGroup.IntField := i;
    lGroup.FloatField := TestIntToFloat(i);
    lGroup.DateField := TestIntToDate(i);
    result.Add(lGroup);
    for j := 1 to cItemCount do
    begin
      lItem := TtiOPFTestItem.Create;
      lItem.ObjectState := posCreate;
      lItem.OID.AsString := IntToStr(lItemOID);
      lItem.StrField := IntToStr(lItemOID);
      lItem.IntField := lItemOID;
      lItem.FloatField := TestIntToFloat(lItemOID) / 10;
      lItem.DateField := TestIntToDate(lItemOID);
      lGroup.Add(lItem);
      Inc(lItemOID);
    end;
  end;
end;

procedure TTestTIAutoMapOperation.UpdateTIOPFTestData(AData: TtiObjectListForTesting);
var
  lGroup : TtiObjectListNestedForTesting;
  lItem : TtiOPFTestItem ;
  i, j  : integer;
  lItemOID : integer;
begin
  for i := 0 to AData.Count - 1 do
  begin
    lGroup := AData.Items[i];
    lGroup.ObjectState := posUpdate;
    lGroup.StrField := IntToStr(i*10);
    lGroup.IntField := i*10;
    lGroup.FloatField := TestIntToFloat(i) / 100;
    for j := 0 to lGroup.Count - 1 do
    begin
      lItem := lGroup.Items[i];
      lItem.ObjectState := posUpdate;
      lItemOID := StrToInt(lItem.OID.AsString)*100;
      lItem.StrField := IntToStr(lItemOID);
      lItem.IntField := lItemOID;
      lItem.FloatField := TestIntToFloat(lItemOID);
    end;
  end;
end;

procedure TTestTIAutoMapOperation.SetUp;
begin
  inherited;
  CreateTestTables;
end;

procedure TTestTIAutoMapOperation.SingleFlatObjCreate;
var
  lData1 : TtiObjectListNestedForTesting;
  lData2 : TtiObjectListNestedForTesting;
begin

  lData1 := TtiObjectListNestedForTesting.Create;
  try
    lData1.ObjectState := posCreate;
    lData1.OID.AsString := '1';
    lData1.IntField := 1;
    lData1.FloatField := 11.111;
    lData1.StrField  := '1';
    lData1.DateField  := TestIntToDate(1111);
    lData1.BoolField := TestIntToBool(1);
    lData1.NotesField := LongString;
    lData1.Save(DatabaseName, PersistenceLayerName);

    CheckObjectState(posClean, lData1);

    lData2 := TtiObjectListNestedForTesting.Create;
    try
      lData2.OID.AsString := '1';
      lData2.ReadThis(DatabaseName, PersistenceLayerName);
      CheckObjectState(posPK, lData2);
      CheckEquals(1, lData2.IntField);
      CheckEquals('1', lData2.StrField);
      CheckNearEnough(11.111, lData2.FloatField ,'Failed on 11.111');
      Check(TestIntToDate(1111) = lData2.DateField, 'Failed on DateField');
      CheckEquals(TestIntToBool(1), lData2.BoolField, 'BoolField');
      CheckEquals(LongString, lData2.NotesField, 'NotesField');
    finally
      lData2.Free;
    end;

  finally
    lData1.Free;
  end;
end;

procedure TTestTIAutoMapOperation.SingleFlatObjDelete;
var
  lData1 : TtiObjectListNestedForTesting;
  lData2 : TtiObjectListForTesting;
begin

  lData1 := TtiObjectListNestedForTesting.Create;
  try
    lData1.ObjectState := posCreate;
    lData1.OID.AsString := '1';
    lData1.IntField := 1;
    lData1.FloatField := 11.111;
    lData1.StrField  := '1';
    lData1.DateField := TestIntToDate(1111);
    lData1.Save(DatabaseName, PersistenceLayerName);
    CheckObjectState(posClean, lData1);
  finally
    lData1.Free;
  end;

  lData2 := TtiObjectListForTesting.Create;
  try
    lData2.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(1, lData2.Count, 'Failed on TtiObjectListForTesting.Count');
  finally
    lData2.Free;
  end;

  lData1 := TtiObjectListNestedForTesting.Create;
  try
    lData1.ObjectState := posDelete;
    lData1.OID.AsString := '1';
    lData1.Save(DatabaseName, PersistenceLayerName);
    Check(lData1.ObjectState = posDeleted, 'Failed on ObjectState = posDeleted');
  finally
    lData1.Free;
  end;

  lData2 := TtiObjectListForTesting.Create;
  try
    lData2.Read(DatabaseName, PersistenceLayerName);
    CheckEquals(0, lData2.Count, 'Failed on TtiObjectListForTesting.Count');
  finally
    lData2.Free;
  end;

end;

procedure TTestTIAutoMapOperation.SingleFlatObjReadThis;
var
  lQP : TtiQueryParams;
  lData : TtiObjectListNestedForTesting;
begin
  lQP := TtiQueryParams.Create;
  try
    lQP.SetValueAsString('OID',                '1');
    lQP.SetValueAsString('Group_Str_Field',    '1');
    lQP.SetValueAsInteger('Group_Int_Field',     1);
    lQP.SetValueAsFloat('Group_Float_Field',  11.111);
    lQP.SetValueAsDateTime('Group_Date_Field',   TestIntToDate(1111));
    lQP.SetValueAsBoolean('Group_Bool_Field',   TestIntToBool(1));
    lQP.SetValueAsString('Group_Notes_Field',  LongString);
    GTIOPFManager.InsertRow('Test_Group', lQP, DatabaseName, PersistenceLayerName);
  finally
    lQP.Free;
  end;

  lData := TtiObjectListNestedForTesting.Create;
  try
    lData.OID.AsString := '1';
    lData.ReadThis(DatabaseName, PersistenceLayerName);
    Check(lData.ObjectState = posPK, 'Failed on ObjectState = posPK');
    CheckEquals('1', lData.OID.AsString);
    CheckEquals(1, lData.IntField);
    CheckEquals('1', lData.StrField);
    CheckNearEnough(11.111, lData.FloatField, 'FloatField');
    CheckEquals(TestIntToDate(1111), lData.DateField, 'DateField');
    CheckEquals(TestIntToBool(1), lData.BoolField, 'BoolField');
    CheckEquals(LongString, lData.NotesField, 'NotesField');
  finally
    lData.Free;
  end;
end;

procedure TTestTIAutoMapOperation.SingleFlatObjUpdate;
var
  lData1 : TtiObjectListNestedForTesting;
  lData2 : TtiObjectListNestedForTesting;
  lString : string;
begin

  lData1 := TtiObjectListNestedForTesting.Create;
  try
    lData1.ObjectState := posCreate;
    lData1.OID.AsString := '1';
    lData1.IntField := 1;
    lData1.FloatField := 11.111;
    lData1.StrField  := '1';
    lData1.DateField := TestIntToDate(1111);
    lData1.BoolField := TestIntToBool(1);
    lData1.NotesField := LongString;
    lData1.Save(DatabaseName, PersistenceLayerName);

    lString := tiCreateStringOfSize(4000);
    Check(lData1.ObjectState = posClean, 'Failed on ObjectState = posClean');
    lData1.IntField := 2;
    lData1.FloatField := 22.222;
    lData1.StrField  := '2';
    lData1.DateField := TestIntToDate(1112);
    lData1.BoolField := TestIntToBool(2);
    lData1.NotesField := lString;
    lData1.ObjectState := posUpdate;
    lData1.Save(DatabaseName, PersistenceLayerName);

    lData2 := TtiObjectListNestedForTesting.Create;
    try                        
      lData2.OID.AsString := '1';
      lData2.ReadThis(DatabaseName, PersistenceLayerName);
      Check(lData2.ObjectState = posPK, 'Failed on ObjectState = posPK');
      CheckEquals(2, lData2.IntField);
      CheckEquals('2', lData2.StrField);
      CheckNearEnough(22.222, lData2.FloatField, 'Failed on 22.222');
      Check(TestIntToDate(1112) = lData2.DateField, 'Failed on DateField');
      CheckEquals(TestIntToBool(2), lData2.BoolField, 'BoolField');
      CheckEquals(lString, lData2.NotesField, 'NotesField');
    finally
      lData2.Free;
    end;
  finally
    lData1.Free;
  end;
end;

procedure TTestTIAutoMapOperation.SingleInheritedObjCreateEachLevel;
var
  lParent : TtiObjectParentForTesting;
  lChild : TtiObjectChildForTestingA;
begin

  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.StrField := '1';
    lParent.ObjectState := posCreate;
    lParent.Save(DatabaseName, PersistenceLayerName);
    Check(lParent.ObjectState = posClean, 'Failed on 1');
  finally
    lParent.Free;
  end;

  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lParent.ObjectState, 'Failed on 2');
    CheckEquals('1', lParent.StrField, 'Failed on 3');
  finally
    lParent.Free;
  end;

  EmptyTestTables;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.StrField := '1';
    lChild.IntField := 1;
    lChild.FloatField := TestIntToFloat(1) ;
    lChild.ObjectState := posCreate;
    lChild.Save(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on object state');
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on object state');
    CheckEquals(1, lChild.IntField, 'Failed on 4');
    CheckNearEnough(TestIntToFloat(1), lChild.FloatField, 'FloatField');
    CheckEquals('1', lChild.StrField, 'Failed on 6');
  finally
    lChild.Free;
  end;

end;

procedure TTestTIAutoMapOperation.SingleInheritedObjDeleteEachLevel;
var
  lParent : TtiObjectParentForTesting;
  lChild : TtiObjectChildForTestingA;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_B, 2, -1);
  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lParent.ObjectState, 'Failed on 1');
    lParent.ObjectState := posDelete;
    lParent.Save(DatabaseName, PersistenceLayerName);
  finally
    lParent.Free;
  end;

  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posEmpty = lParent.ObjectState, 'Failed on 2');
  finally
    lParent.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 3');
    lChild.ObjectState := posDelete;
    lChild.Save(DatabaseName, PersistenceLayerName);
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posEmpty = lChild.ObjectState, 'Failed on 4');
  finally
    lChild.Free;
  end;

end;

procedure TTestTIAutoMapOperation.SingleInheritedObjRead;
var
  lParent : TtiObjectParentForTesting;
  lChild : TtiObjectChildForTestingA;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_B, 2, -1);
  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    CheckObjectState(posClean, lParent);
    CheckEquals('1', lParent.StrField, 'Str');
  finally
    lParent.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    CheckObjectState(posClean, lChild);
    CheckEquals(1, lChild.IntField, 'Int');
    CheckNearEnough(TestIntToFloat(1), lChild.FloatField, 'FloatField');
    // This should be read as part of the parent class
    CheckEquals('1', lChild.StrField, 'Str');
  finally
    lChild.Free;
  end;
end;

procedure TTestTIAutoMapOperation.SingleInheritedObjUpdateEachLevel;
var
  lParent : TtiObjectParentForTesting;
  lChild : TtiObjectChildForTestingA;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_B, 2, -1);

  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lParent.ObjectState, 'Failed on 0');
    CheckEquals('1', lParent.StrField, 'Failed on 2');
    lParent.StrField := '2';
    lParent.ObjectState := posUpdate;
    lParent.Save(DatabaseName, PersistenceLayerName);
  finally
    lParent.Free;
  end;

  lParent := TtiObjectParentForTesting.Create;
  try
    lParent.OID.AsString := '1';
    lParent.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lParent.ObjectState, 'Failed on 3');
    CheckEquals('2', lParent.StrField, 'Failed on 4');
  finally
    lParent.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 5');
    CheckEquals(1, lChild.IntField, 'Failed on 6');
    CheckNearEnough(TestIntToFloat(1), lChild.FloatField, 'FloatField');
    CheckEquals('2', lChild.StrField, 'Failed on 8');
    lChild.IntField := 3;
    lChild.FloatField := TestIntToFloat(3);
    lChild.StrField := '3';
    lChild.ObjectState := posUpdate;
    lChild.Save(DatabaseName, PersistenceLayerName);
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 9');
    CheckEquals(3, lChild.IntField, 'Failed on 10');
    CheckNearEnough(TestIntToFloat(3), lChild.FloatField, 'FloatField');
    CheckEquals('3', lChild.StrField, 'Failed on 12');
  finally
    lChild.Free;
  end;

end;

{ TTestTIClassToDBMapFramework }

procedure TTestTIAutoMapFramework.TestClassMap_AddAttrMap;
var
  lClassMap : TtiClassMap;
  lAttr : TtiAttrMap;
begin
  lClassMap := TtiClassMap.Create;
  try
    lClassMap.PerObjAbsClass := TtiObject;
    // This should work...
    lAttr := lClassMap.AddAttrMap('caption');
    CheckNotNull(lAttr);
    CheckEquals('caption', lAttr.AttrName, 'Failed on lAttr.AttrName');
    CheckEquals(1, lClassMap.Count, 'Failed on lClassMap.Count');
    CheckEquals('caption', lClassMap.Items[0].AttrName, 'Failed on lClassMap.Items[0].AttrName');

    // This should raise an exception...
    try
      lClassMap.AddAttrMap('test');
      Fail('An exceptio was not raised when a property that does not exist was registered for a class.');
    except
      on e:exception do
        // nothing
    end;


  finally
    lClassMap.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassMaps_AddClassMap;
var
  lClassMaps : TtiClassMaps;
  lClassMap : TtiClassMap;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMap := lClassMaps.AddClassMap(TtiObject);
    CheckNotNull(lClassMap);
    CheckEquals(TtiObject, lClassMap.PerObjAbsClass, 'Failed on lClassMap.PerObjAbsClass');
    CheckEquals(1, lClassMaps.Count, 'Failed on lClassMaps.Count');
    CheckEquals(TtiObject, lClassMaps.Items[0].PerObjAbsClass, 'Failed on lClassMaps.Items[i].PerObjAbsClass');
  finally
    lClassMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassMaps_FindCreate;
var
  lClassMaps : TtiClassMaps;
  lClassMap : TtiClassMap;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMap := lClassMaps.FindCreate(TtiObject);
    CheckNotNull(lClassMap);
    CheckEquals(TtiObject, lClassMap.PerObjAbsClass, 'Failed on lClassMap.PerObjAbsClass');
    CheckEquals(1, lClassMaps.Count, 'Failed on lClassMaps.Count');
    CheckEquals(TtiObject, lClassMaps.Items[0].PerObjAbsClass, 'Failed on lClassMaps.Items[i].PerObjAbsClass');

    lClassMap := lClassMaps.FindCreate(TtiObject);
    CheckNotNull(lClassMap);
    CheckEquals(TtiObject, lClassMap.PerObjAbsClass, 'Failed on lClassMap.PerObjAbsClass');
    CheckEquals(1, lClassMaps.Count, 'Failed on lClassMaps.Count');
    CheckEquals(TtiObject, lClassMaps.Items[0].PerObjAbsClass, 'Failed on lClassMaps.Items[i].PerObjAbsClass');

  finally
    lClassMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassMaps_IsClassReg;
var
  lClassMaps : TtiClassMaps;
  lClassMap : TtiClassMap;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMap := lClassMaps.AddClassMap(TtiObject);
    CheckNotNull(lClassMap);
    Check(lClassMaps.IsClassReg(TtiObject), 'Failed on lClassMaps.IsClassReg(TtiObject)');
    Check(not lClassMaps.IsClassReg(TtiObjectList), 'Failed on not lClassMaps.IsClassReg(TtiObjectList)');
  finally
    lClassMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestDBMap_AddTableMap;
var
  lDBMap     : TtiDBMap ;
  lDBTableMap : TtiDBTableMap;
begin
  lDBMap := TtiDBMap.Create;
  try
    lDBTableMap := lDBMap.AddTableMap('test');
    CheckNotNull(lDBTableMap);
    CheckEquals('test', lDBTableMap.TableName, 'Failed on lDBTableMap.TableName');
    CheckEquals(1, lDBMap.Count, 'Failed on lDBMap.Count');
    CheckEquals('test', lDBMap.Items[0].TableName, 'Failed on lDBMaps.Items[0].TableName');
  finally
    lDBMap.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestDBMap_FindCreate;
var
  lDBMap     : TtiDBMap ;
  lDBTableMap : TtiDBTableMap;
begin
  lDBMap := TtiDBMap.Create;
  try
    lDBTableMap := lDBMap.FindCreate('test');
    CheckNotNull(lDBTableMap);
    CheckEquals('test', lDBTableMap.TableName, 'Failed on lDBTableMap.TableName');
    CheckEquals(1, lDBMap.Count, 'Failed on lDBMap.Count');
    CheckEquals('test', lDBMap.Items[0].TableName, 'Failed on lDBMaps.Items[0].TableName');
  finally
    lDBMap.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestDBMaps_AddDBMap;
var
  lDBMaps : TtiDBMaps;
  lDBMap : TtiDBMap ;
begin
  lDBMaps := TtiDBMaps.Create;
  try
    lDBMap := lDBMaps.AddDBMap('test');
    CheckNotNull(lDBMap);
    CheckEquals('test', lDBMap.DatabaseName, 'Failed on lDBMap.DatabaseName');
    CheckEquals(1, lDBMaps.Count, 'Failed on lDBMaps.Count');
    CheckEquals('test', lDBMaps.Items[0].DatabaseName, 'Failed on lDBMaps.Items[0].DatabaseName');
  finally
    lDBMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TtiDBMaps_FindCreate;
var
  lDBMaps : TtiDBMaps;
  lDBMap : TtiDBMap ;
begin
  lDBMaps := TtiDBMaps.Create;
  try
    lDBMap := lDBMaps.FindCreate('test');
    CheckNotNull(lDBMap);
    CheckEquals('test', lDBMap.DatabaseName, 'Failed on lDBMap.DatabaseName');
    CheckEquals(1, lDBMaps.Count, 'Failed on lDBMaps.Count');
    CheckEquals('test', lDBMaps.Items[0].DatabaseName, 'Failed on lDBMaps.Items[0].DatabaseName');
  finally
    lDBMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestDBTableMap_AddColMap;
var
  lDBTableMap : TtiDBTableMap;
  lDBColMap  : TtiDBColMap;
begin
  lDBTableMap := TtiDBTableMap.Create;
  try
    lDBColMap := lDBTableMap.AddColMap('test', []);
    CheckNotNull(lDBColMap);
    CheckEquals('test', lDBColMap.ColName, 'Failed on lDBColMap.ColName');
    Check(lDBColMap.PKInfo = [], 'Failed on lDBColMap.PKInfo');
    CheckEquals(1, lDBTableMap.Count, 'Failed on lDBTableMap.Count');
    CheckEquals('test', lDBTableMap.Items[0].ColName, 'Failed on lDBTableMap.Items[0].ColName');

    lDBColMap := lDBTableMap.AddColMap('test1', [pktDB]);
    CheckNotNull(lDBColMap);
    CheckEquals('test1', lDBColMap.ColName, 'Failed on lDBColMap.ColName');
    Check(lDBColMap.PKInfo = [pktDB], 'Failed on lDBColMap.PKInfo');
    CheckEquals(2, lDBTableMap.Count, 'Failed on lDBTableMap.Count');
    CheckEquals('test1', lDBTableMap.Items[1].ColName, 'Failed on lDBTableMap.Items[0].ColName');

  finally
    lDBTableMap.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestAttrColMaps_AddMapping;
var
  lAttrColMaps : TtiAttrColMaps;
  lAttrMap : TtiAttrMap;
  lDBColMap : TtiDBColMap;
begin
  lAttrColMaps := TtiAttrColMaps.Create;
  try
    lAttrMap := TtiAttrMap.Create;
    try
      lAttrMap.AttrName := 'attr1';
      lDBColMap := TtiDBColMap.Create;
      try
        lDBColMap.ColName := 'col1';
        lAttrColMaps.AddMapping(lAttrMap, lDBColMap);
        CheckEquals(1, lAttrColMaps.Count, 'Failed on lAttrColMaps.Count');
        Check('attr1' = lAttrColMaps.Items[0].AttrMap.AttrName, 'Failed on lAttrMap = lAttrColMaps.Items[0].AttrMap');
        Check('col1'  = lAttrColMaps.Items[0].DBColMap.ColName, 'Failed on lAttrMap = lAttrColMaps.Items[0].ColMap');
        finally
          lDBColMap.Free;
        end;
    finally
      lAttrMap.Free;
    end;
  finally
    lAttrColMaps.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestAttrColMaps_FindAllMappingsByMapToClass;
var
  lAttrColMaps : TtiAttrColMaps;

  lClassMap1  : TtiClassMap;
  lAttrMap11  : TtiAttrMap;
  lAttrMap12  : TtiAttrMap;
  lAttrMap13  : TtiAttrMap;

  lClassMap2  : TtiClassMap;
  lAttrMap21  : TtiAttrMap;
  lAttrMap22  : TtiAttrMap;
  lAttrMap23  : TtiAttrMap;

  lFoundList  : TtiAttrColMaps;

begin

  lAttrColMaps := TtiAttrColMaps.Create;
  try
    lClassMap1   := TtiClassMap.Create;
    lClassMap1.PerObjAbsClass := TtiObject;
    lAttrMap11    := TtiAttrMap.Create;
    lAttrMap11.AttrName := 'attr1';
    lClassMap1.Add(lAttrMap11);
    lAttrColMaps.AddMapping(lAttrMap11, nil);

    lAttrMap12    := TtiAttrMap.Create;
    lAttrMap12.AttrName := 'attr2';
    lClassMap1.Add(lAttrMap12);
    lAttrColMaps.AddMapping(lAttrMap12, nil);

    lAttrMap13    := TtiAttrMap.Create;
    lAttrMap13.AttrName := 'attr3';
    lClassMap1.Add(lAttrMap13);
    lAttrColMaps.AddMapping(lAttrMap13, nil);

    lClassMap2   := TtiClassMap.Create;
    lClassMap2.PerObjAbsClass := TtiObjectList;
    lAttrMap21    := TtiAttrMap.Create;
    lAttrMap21.AttrName := 'attr1';
    lClassMap2.Add(lAttrMap21);
    lAttrColMaps.AddMapping(lAttrMap21, nil);

    lAttrMap22    := TtiAttrMap.Create;
    lAttrMap22.AttrName := 'attr2';
    lClassMap2.Add(lAttrMap22);
    lAttrColMaps.AddMapping(lAttrMap22, nil);

    lAttrMap23    := TtiAttrMap.Create;
    lAttrMap23.AttrName := 'attr3';
    lClassMap2.Add(lAttrMap23);
    lAttrColMaps.AddMapping(lAttrMap23, nil);

    lFoundList := TtiAttrColMaps.Create;
    try
      lFoundList.OwnsObjects := false;
      lAttrColMaps.FindAllMappingsByMapToClass(TtiObject, lFoundList);
      CheckEquals(3, lFoundList.Count, 'Failed on lFondList.Count');
      Check(lFoundList.Items[0].AttrMap = lAttrMap11, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap11');
      Check(lFoundList.Items[1].AttrMap = lAttrMap12, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap12');
      Check(lFoundList.Items[2].AttrMap = lAttrMap13, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap13');

      lAttrColMaps.FindAllMappingsByMapToClass(TtiObjectList, lFoundList);
      CheckEquals(3, lFoundList.Count, 'Failed on lFondList.Count');
      Check(lFoundList.Items[0].AttrMap = lAttrMap21, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap21');
      Check(lFoundList.Items[1].AttrMap = lAttrMap22, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap22');
      Check(lFoundList.Items[2].AttrMap = lAttrMap23, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap23');

      lAttrColMaps.FindAllMappingsByMapToClass(TPerStream, lFoundList);
      CheckEquals(0, lFoundList.Count, 'Failed on lFondList.Count');

    finally
      lFoundList.Free;
    end;

    lClassMap1.Free;
    lClassMap2.Free;

  finally
    lAttrColMaps.Free;
  end;

end;

procedure TTestTIAutoMapFramework.TestAttrColMaps_FindByClassAttrMap;
var
  lAttrColMaps : TtiAttrColMaps;

  lClassMap1  : TtiClassMap;
  lAttrMap11  : TtiAttrMap;
  lAttrMap12  : TtiAttrMap;

  lClassMap2  : TtiClassMap;
  lAttrMap21  : TtiAttrMap;

  lFound : TtiAttrColMap;

begin

  lAttrColMaps := TtiAttrColMaps.Create;
  try
    lClassMap1   := TtiClassMap.Create;
    lClassMap1.PerObjAbsClass := TtiObject;
    lAttrMap11    := TtiAttrMap.Create;
    lAttrMap11.AttrName := 'attr1';
    lClassMap1.Add(lAttrMap11);
    lAttrColMaps.AddMapping(lAttrMap11, nil);

    lAttrMap12    := TtiAttrMap.Create;
    lAttrMap12.AttrName := 'attr2';
    lClassMap1.Add(lAttrMap12);
    lAttrColMaps.AddMapping(lAttrMap12, nil);

    lClassMap2   := TtiClassMap.Create;
    lClassMap2.PerObjAbsClass := TtiObjectList;
    lAttrMap21    := TtiAttrMap.Create;
    lAttrMap21.AttrName := 'attr1';
    lClassMap2.Add(lAttrMap21);
    lAttrColMaps.AddMapping(lAttrMap21, nil);

    lFound := lAttrColMaps.FindByClassAttrMap(TtiObject, 'attr1');
    CheckNotNull(lFound);
    Check(lFound.AttrMap = lAttrMap11, 'Failed on TtiObject, "attr1"');
    lFound := lAttrColMaps.FindByClassAttrMap(TtiObject, 'attr2');
    CheckNotNull(lFound);
    Check(lFound.AttrMap = lAttrMap12, 'Failed on TtiObject, "attr2"');

    lFound := lAttrColMaps.FindByClassAttrMap(TtiObjectList, 'attr1');
    CheckNotNull(lFound);
    Check(lFound.AttrMap = lAttrMap21, 'Failed on TtiObjectList, "attr1"');

    lFound := lAttrColMaps.FindByClassAttrMap(TtiObject, 'attr4');
    CheckNull(lFound, 'Failed on TtiObject, "attr4"');

    lFound := lAttrColMaps.FindByClassAttrMap(TPerStream, 'attr1');
    CheckNull(lFound, 'Failed on TPerObjStream, "attr1"');

    lClassMap1.Free;
    lClassMap2.Free;

  finally
    lAttrColMaps.Free;
  end;

end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_AddClassCollectionMappingSimple;
var
  lClassDBCollections : TtiClassDBCollections;
  lClassDBCollection : TtiClassDBCollection;
begin
  lClassDBCollections := TtiClassDBCollections.Create;
  try
    lClassDBCollection := lClassDBCollections.AddClassCollectionMapping(TtiObjectList, TtiObject);
    CheckEquals(1, lClassDBCollections.Count, 'Failed on count = 1');
    CheckNotNull(lClassDBCollection, 'Failed on NotNull');
    Check(lClassDBCollection.CollectionClass = TtiObjectList, 'Failed on lClassDBCollection.CollectionClass = TtiObjectList');
    Check(lClassDBCollection.PerObjAbsClass  = TtiObject, 'Failed on lClassDBCollection.PerObjAbsClass  = TtiObject');
//    CheckEquals(0, lClassDBCollection.ForeignKeyCols.Count, 'Failed on lClassDBCollection.ForeignKeyCols.Count = 0');
  finally
    lClassDBCollections.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_FindByCollection;
var
  lCDBCs : TtiClassDBCollections;
  lList : TObjectList;
begin
  lCDBCs := TtiClassDBCollections.Create;
  try
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList1, TCTDBMTestObj1);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList2, TCTDBMTestObj2);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList3, TCTDBMTestObj3);

    lList := TObjectList.Create(false);
    try

    lCDBCs.FindByCollection(TCTDBMTestList1, lList);
    CheckEquals(1, lList.Count, 'Failed to find collection for TCTDBMTestObj1');
    Check((lList.Items[0] as TtiClassDBCollection).PerObjAbsClass = TCTDBMTestObj1, 'Failed on lCDBC.PerObjAbsClass = TCTDBMTestObj1');

    lCDBCs.FindByCollection(TCTDBMTestList2, lList);
    CheckEquals(1, lList.Count, 'Failed to find collection for TCTDBMTestObj2');
    Check((lList.Items[0] as TtiClassDBCollection).PerObjAbsClass = TCTDBMTestObj2, 'Failed on lCDBC.PerObjAbsClass = TCTDBMTestObj2');

    lCDBCs.FindByCollection(TCTDBMTestList3, lList);
    CheckEquals(1, lList.Count, 'Failed to find collection for TCTDBMTestObj3');
    Check((lList.Items[0] as TtiClassDBCollection).PerObjAbsClass = TCTDBMTestObj3, 'Failed on lCDBC.PerObjAbsClass = TCTDBMTestObj3');

    lCDBCs.FindByCollection(TtiObjectList, lList);
    CheckEquals(0, lList.Count, 'Failed not to find collection for TtiObjectList');

    finally
      lList.Free;
    end;
  finally
    lCDBCs.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_FindByCollectionOf;
var
  lCDBCs : TtiClassDBCollections;
  lCDBC : TtiClassDBCollection;
begin
  lCDBCs := TtiClassDBCollections.Create;
  try
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList1, TCTDBMTestObj1);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList2, TCTDBMTestObj2);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList3, TCTDBMTestObj3);

    lCDBC := lCDBCs.FindByCollectionOf(TCTDBMTestObj1);
    Check(lCDBC.CollectionClass = TCTDBMTestList1, 'Failed on lCDBC.CollectionClass = TCTDBMTestList1');
    lCDBC := lCDBCs.FindByCollectionOf(TCTDBMTestObj2);
    Check(lCDBC.CollectionClass = TCTDBMTestList2, 'Failed on lCDBC.CollectionClass = TCTDBMTestList2');
    lCDBC := lCDBCs.FindByCollectionOf(TCTDBMTestObj3);
    Check(lCDBC.CollectionClass = TCTDBMTestList3, 'Failed on lCDBC.CollectionClass = TCTDBMTestList3');

    lCDBC := lCDBCs.FindByCollectionOf(TtiObject);
    CheckNull(lCDBC, 'Failed on lCDBCs.FindByCollectionOf(TtiObject)');

  finally
    lCDBCs.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_IsInCollection;
var
  lCDBCs : TtiClassDBCollections;
begin
  lCDBCs := TtiClassDBCollections.Create;
  try
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList1, TCTDBMTestObj1);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList2, TCTDBMTestObj2);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList3, TCTDBMTestObj3);

    Check(lCDBCs.IsInCollection(TCTDBMTestObj1), 'Failed on TCTDBMTestObj1');
    Check(lCDBCs.IsInCollection(TCTDBMTestObj2), 'Failed on TCTDBMTestObj2');
    Check(lCDBCs.IsInCollection(TCTDBMTestObj3), 'Failed on TCTDBMTestObj3');
    Check(not lCDBCs.IsInCollection(TtiObject), 'Failed on not TtiObject');

  finally
    lCDBCs.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_AddClassCollectionMappingFK;
var
  lCDBMM : TtiClassDBMappingMgr;
  lClassDBCollection : TtiClassDBCollection;
begin

  lCDBMM := TtiClassDBMappingMgr.Create;
  try
    lCDBMM.RegisterMapping('test', TtiObjectList, 'test', 'OID', 'OID', [pktDB]);
    lClassDBCollection := lCDBMM.Collections.AddClassCollectionMapping(TtiObjectList, TtiObject{, ['Owner_OID']});

    CheckEquals(1, lCDBMM.Collections.Count, 'Failed on count = 1');
    CheckNotNull(lClassDBCollection, 'Failed on NotNull');
    Check(lClassDBCollection.CollectionClass = TtiObjectList, 'Failed on lClassDBCollection.CollectionClass = TtiObjectList');
    Check(lClassDBCollection.PerObjAbsClass  = TtiObject, 'Failed on lClassDBCollection.PerObjAbsClass  = TtiObject');

  finally
    lCDBMM.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestClassDBMappingMgr_RegisterCollection;
begin
  TestClassDBCollections_AddClassCollectionMappingSimple;
  TestClassDBCollections_AddClassCollectionMappingFK;
end;

procedure TTestTIAutoMapFramework.TestClassDBMappingMgr_RegisterMapping;
var
  lCDBMM : TtiClassDBMappingMgr;
begin

  lCDBMM := TtiClassDBMappingMgr.Create;
  try
    lCDBMM.RegisterMapping('db_1', TtiObjectListNestedForTesting, 'table_1', 'StrField', 'col_1', [pktDB]);
    CheckEquals(1, lCDBMM.DBMaps.Count, 'Failed on DBMaps.Count');
    CheckEquals('db_1', lCDBMM.DBMaps.Items[0].DatabaseName, 'Failed on DatabaseName');
    CheckEquals(1, lCDBMM.DBMaps.Items[0].Count, 'Failed on lCDBMM.DBMaps.Items[0].Count = 1');
    CheckEquals('table_1', lCDBMM.DBMaps.Items[0].Items[0].TableName, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].TableName');
    CheckEquals(1, lCDBMM.DBMaps.Items[0].Items[0].Count, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Count = 1');
    CheckEquals('col_1', lCDBMM.DBMaps.Items[0].Items[0].Items[0].ColName, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Items[0].ColName');
    Check(lCDBMM.DBMaps.Items[0].Items[0].Items[0].PKInfo = [pktDB], 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Items[0].PKInfo = PKINfo');

    CheckEquals(1, lCDBMM.ClassMaps.Count, 'Failed on DBMaps.ClassMaps');
    Check(lCDBMM.ClassMaps.Items[0].PerObjAbsClass = TtiObjectListNestedForTesting, 'Failed on TtiObjectListNestedForTesting');
    CheckEquals(1, lCDBMM.ClassMaps.Items[0].Count, 'Failed on lCDBMM.ClassMaps.Items[0].Count = 1');
    CheckEquals('StrField', lCDBMM.ClassMaps.Items[0].Items[0].AttrName, 'Failed on lCDBMM.ClassMaps.Items[0].Items[0] = prop_1');

    CheckEquals(1, lCDBMM.AttrColMaps.Count, 'Failed on DBMaps.AttrColMaps');
    CheckEquals('StrField', lCDBMM.AttrColMaps.Items[0].AttrMap.AttrName, 'Failed on lCDBMM.AttrColMaps.Items[0].AttrMap.AttrName = StrField');
    CheckEquals('col_1', lCDBMM.AttrColMaps.Items[0].DBColMap.ColName, 'Failed on lCDBMM.AttrColMaps.Items[0].DBColMap.ColName = col_1');

    lCDBMM.RegisterMapping('db_2', TCTDBMTestObj1, 'table_2', 'Caption', 'col_2', []);
    CheckEquals(2, lCDBMM.DBMaps.Count, 'Failed on DBMaps.Count');
    CheckEquals('db_2', lCDBMM.DBMaps.Items[1].DatabaseName, 'Failed on DatabaseName');
    CheckEquals(1, lCDBMM.DBMaps.Items[0].Count, 'Failed on lCDBMM.DBMaps.Items[0].Count = 1');
    CheckEquals('table_2', lCDBMM.DBMaps.Items[1].Items[0].TableName, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].TableName');
    CheckEquals(1, lCDBMM.DBMaps.Items[1].Items[0].Count, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Count = 1');
    CheckEquals('col_2', lCDBMM.DBMaps.Items[1].Items[0].Items[0].ColName, 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Items[0].ColName');
    Check(lCDBMM.DBMaps.Items[1].Items[0].Items[0].PKInfo = [], 'Failed on lCDBMM.DBMaps.Items[0].Items[0].Items[0].PKInfo = PKINfo');

    CheckEquals(2, lCDBMM.ClassMaps.Count, 'Failed on DBMaps.ClassMaps');
    Check(lCDBMM.ClassMaps.Items[1].PerObjAbsClass = TCTDBMTestObj1, 'Failed on TtiObjectList');
    CheckEquals(1, lCDBMM.ClassMaps.Items[1].Count, 'Failed on lCDBMM.ClassMaps.Items[0].Count = 1');
    CheckEquals('Caption', lCDBMM.ClassMaps.Items[1].Items[0].AttrName, 'Failed on lCDBMM.ClassMaps.Items[0].Items[0] = Caption');

    CheckEquals(2, lCDBMM.AttrColMaps.Count, 'Failed on DBMaps.AttrColMaps');
    CheckEquals('Caption', lCDBMM.AttrColMaps.Items[1].AttrMap.AttrName, 'Failed on lCDBMM.AttrColMaps.Items[0].AttrMap.AttrName = prop_1');
    CheckEquals('col_2', lCDBMM.AttrColMaps.Items[1].DBColMap.ColName, 'Failed on lCDBMM.AttrColMaps.Items[0].DBColMap.ColName = col_1');

    try
      lCDBMM.RegisterMapping('db_1', TCTDBMTestObj2, 'table_1', 'prop_1', 'col_1', [pktDB]);
      Check(false, 'Failed to raise exception when registering duplicate DB-Table-Col mapping');
    except
      on e:exception do
        Check(e is Exception, 'Failed to raise correct class of exception');
    end;

    try
      lCDBMM.RegisterMapping('db_3', TtiObjectList, 'table_1', 'prop_1', 'col_1', [pktDB]);
      Check(false, 'Failed to raise exception when registering duplicate class-property mapping');
    except
      on e:exception do
        Check(e is Exception, 'Failed to raise correct class of exception');
    end;

  finally
    lCDBMM.Free;
  end;
end;

procedure TTestTIAutoMapFramework.TestAttrColMaps_FindAllPKMappingsByMapToClass;
var
  lAttrColMaps : TtiAttrColMaps;

  lClassMap1  : TtiClassMap;
  lAttrMap11  : TtiAttrMap;
  lAttrMap12  : TtiAttrMap;
  lAttrMap13  : TtiAttrMap;

  lDBTableMap1 : TtiDBTableMap;
  lDBColMap11 : TtiDBColMap;
  lDBColMap12 : TtiDBColMap;
  lDBColMap13 : TtiDBColMap;

  lClassMap2  : TtiClassMap;
  lAttrMap21  : TtiAttrMap;
  lAttrMap22  : TtiAttrMap;
  lAttrMap23  : TtiAttrMap;

  lDBTableMap2 : TtiDBTableMap;
  lDBColMap21 : TtiDBColMap;
  lDBColMap22 : TtiDBColMap;
  lDBColMap23 : TtiDBColMap;
  lFoundList  : TtiAttrColMaps;

begin

  lAttrColMaps := TtiAttrColMaps.Create;
  try
    lClassMap1   := TtiClassMap.Create;
    lClassMap1.PerObjAbsClass := TtiObject;
    lDBTableMap1 := TtiDBTableMap.Create;
    lDBTableMap1.TableName := 'table1';

    lAttrMap11    := TtiAttrMap.Create;
    lAttrMap11.AttrName := 'attr1';
    lClassMap1.Add(lAttrMap11);
    lDBColMap11 := TtiDBColMap.Create;
    lDBColMap11.ColName := 'col1';
    lDBColMap11.PKINfo := [pktDB];
    lDBTableMap1.Add(lDBColMap11);
    lAttrColMaps.AddMapping(lAttrMap11, lDBColMap11);

    lAttrMap12    := TtiAttrMap.Create;
    lAttrMap12.AttrName := 'attr2';
    lClassMap1.Add(lAttrMap12);
    lDBColMap12 := TtiDBColMap.Create;
    lDBColMap12.ColName := 'col2';
    lDBTableMap1.Add(lDBColMap12);
    lAttrColMaps.AddMapping(lAttrMap12, lDBColMap12);

    lAttrMap13    := TtiAttrMap.Create;
    lAttrMap13.AttrName := 'attr3';
    lClassMap1.Add(lAttrMap13);
    lDBColMap13 := TtiDBColMap.Create;
    lDBColMap13.ColName := 'col3';
    lDBTableMap1.Add(lDBColMap13);
    lAttrColMaps.AddMapping(lAttrMap13, lDBColMap13);

    lClassMap2   := TtiClassMap.Create;
    lClassMap2.PerObjAbsClass := TtiObjectList;
    lDBTableMap2 := TtiDBTableMap.Create;
    lDBTableMap2.TableName := 'table2';

    lAttrMap21    := TtiAttrMap.Create;
    lAttrMap21.AttrName := 'attr1';
    lClassMap2.Add(lAttrMap21);
    lDBColMap21 := TtiDBColMap.Create;
    lDBColMap21.ColName := 'col1';
    lDBColMap21.PKINfo := [pktDB];
    lDBTableMap2.Add(lDBColMap21);
    lAttrColMaps.AddMapping(lAttrMap21, lDBColMap21);

    lAttrMap22    := TtiAttrMap.Create;
    lAttrMap22.AttrName := 'attr2';
    lClassMap2.Add(lAttrMap22);
    lDBColMap22 := TtiDBColMap.Create;
    lDBColMap22.ColName := 'col2';
    lDBColMap22.PKINfo := [pktDB];
    lDBTableMap2.Add(lDBColMap22);
    lAttrColMaps.AddMapping(lAttrMap22, lDBColMap22);

    lAttrMap23    := TtiAttrMap.Create;
    lAttrMap23.AttrName := 'attr3';
    lClassMap2.Add(lAttrMap23);
    lDBColMap23 := TtiDBColMap.Create;
    lDBColMap23.ColName := 'col3';
    lDBTableMap2.Add(lDBColMap23);
    lAttrColMaps.AddMapping(lAttrMap23, lDBColMap23);

    lFoundList := TtiAttrColMaps.Create;
    try
      lFoundList.OwnsObjects := false;
      lAttrColMaps.FindAllPKMappingsByMapToClass(TtiObject, lFoundList);
      CheckEquals(1, lFoundList.Count, 'Failed on lFondList.Count');
      Check(lFoundList.Items[0].AttrMap = lAttrMap11, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap11');

      lAttrColMaps.FindAllPKMappingsByMapToClass(TtiObjectList, lFoundList);
      CheckEquals(2, lFoundList.Count, 'Failed on lFondList.Count');
      Check(lFoundList.Items[0].AttrMap = lAttrMap21, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap21');
      Check(lFoundList.Items[1].AttrMap = lAttrMap22, 'Failed on lFoundList.Items[0].AttrMap = lAttrMap22');

      lAttrColMaps.FindAllPKMappingsByMapToClass(TPerStream, lFoundList);
      CheckEquals(0, lFoundList.Count, 'Failed on lFondList.Count');

    finally
      lFoundList.Free;
    end;

    lClassMap1.Free;
    lClassMap2.Free;
    lDBTableMap1.Free;
    lDBTableMap2.Free;

  finally
    lAttrColMaps.Free;
  end;

end;

procedure TTestTIAutoMapOperation.CollectionReadPK;
var
  lData : TtiObjectListForTesting;
  i : integer;
  lGroupVal : integer;
begin
  InserTtiObjectListForTesting(CGroupCount, CItemCount);
  lData := TtiObjectListForTesting.Create;
  try
    lData.ReadPK(DatabaseName, PersistenceLayerName);
    Check(posClean = lData.ObjectState, 'ObjectState');
    CheckEquals(cGroupCount, lData.Count, 'Count');
    for i := 0 to cGroupCount - 1 do
    begin
      lGroupVal := i + 1;
      Check(posPK = lData.Items[i].ObjectState, 'ObjectState');
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].OID.AsString, 'Failed on Group.OID');
      CheckEquals(IntToStr(lGroupVal), lData.Items[i].StrField, 'Failed on Group.StrField');
      CheckEquals(0, lData.Items[i].IntField, 'Failed on Group.IntField');
      Check(      0 = lData.Items[i].FloatField, 'FloatField');
      CheckEquals(0, lData.Items[i].Count, 'Failed on Group.Count');
    end;
  finally
    lData.Free;
  end;
end;

type

  TtiObjectyListForTestingThread = class(TtiThread)
  private
    FTestCase: TtiTestCase;
    FDatabaseName: String;
    FPersistenceLayerName: String;
    FGroupCount: integer;
  public
    constructor Create(const ATestCase: TtiTestCase;
      const ADatabaseName: string; const APersistenceLayerName: string;
      const AGroupCount: integer);
    procedure Execute; override;
  end;

  constructor TtiObjectyListForTestingThread.Create(
    const ATestCase: TtiTestCase;
    const ADatabaseName, APersistenceLayerName: string;
    const AGroupCount: integer);
  begin
    inherited Create(True);
    FreeOnTerminate:= False;
    FTestCase:= ATestCase;
    FDatabaseName:= ADatabaseName;
    FPersistenceLayerName:= APersistenceLayerName;
    FGroupCount:= AGroupCount;
  end;

  procedure TtiObjectyListForTestingThread.Execute;
  var
    LData : TtiObjectListForTesting;
  begin
    LData := TtiObjectListForTesting.Create;
    try
      LData.ReadPK(FDatabaseName, FPersistenceLayerName);
      FTestCase.Check(posClean = LData.ObjectState, 'ObjectState');
      FTestCase.CheckEquals(FGroupCount, LData.Count);
    finally
      LData.Free;
    end;
  end;

procedure TTestTIAutoMapOperation.CollectionReadPKThreaded;
var
  LThread: TtiObjectyListForTestingThread;
begin
  if PersistenceLayerSupportsMultiUser then
  begin
    InserTtiObjectListForTesting(1, 1);
    LThread:= TtiObjectyListForTestingThread.Create(Self, DatabaseName, PersistenceLayerName, 1);
    LThread.Resume;
    LThread.WaitFor;
    LThread.Free;
  end else
    Check(True);
end;

procedure TTestTIAutoMapFramework.TestClassDBCollections_IsCollection;
var
  lCDBCs : TtiClassDBCollections;
begin
  lCDBCs := TtiClassDBCollections.Create;
  try
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList1, TCTDBMTestObj1);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList2, TCTDBMTestObj2);
    lCDBCs.AddClassCollectionMapping(TCTDBMTestList3, TCTDBMTestObj3);

    Check(lCDBCs.IsCollection(TCTDBMTestList1), 'Failed on 1');
    Check(lCDBCs.IsCollection(TCTDBMTestList2), 'Failed on 2');
    Check(lCDBCs.IsCollection(TCTDBMTestList3), 'Failed on 3');
    Check(not lCDBCs.IsCollection(TtiObjectList), 'Failed on 4');

  finally
    lCDBCs.Free;
  end;
end;

procedure TTestTIAutoMapOperation.InserTtiObjectListForTesting(
  const AGroupCount, AItemCount: Integer);
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
      lQueryParams.SetValueAsString('Group_Notes_Field',  LongString);
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
      lQueryParams.SetValueAsString('Item_Notes_Field', LongString);
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
  for i := 1 to AGroupCount do
  begin
    _InsertGroup(i);
    for j := 1 to AItemCount do
    begin
      _InsertItem(lItemOID, i, j);
      inc(lItemOID);
    end;
  end;
end;

function TTestTIAutoMapOperation.TestIntToFloat(pInt: Integer): extended;
begin
  result := pInt*2 / 10;
end;

procedure TTestTIAutoMapOperation.InserTtiObjectListForTestingInheritedGroup(AOID : integer);
var
  lQueryParams : TtiQueryParams;
begin
  lQueryParams := TtiQueryParams.Create;
  try
    lQueryParams.SetValueAsString('OID', IntToStr(AOID));
    GTIOPFManager.InsertRow(cTableNameTIOPFTestParentGroup, lQueryParams, DatabaseName, PersistenceLayerName );
  finally
    lQueryParams.Free;
  end;
end;

procedure TTestTIAutoMapOperation.InserTtiObjectListForTestingInherited(const pParentTableName, ATableName : string; pI: Integer; pOwnerOID : integer);
  procedure _InsertTestParent(const ATableName : string; pI : integer; pOwnerOID : integer);
  var
    lQueryParams : TtiQueryParams;
  begin
    lQueryParams := TtiQueryParams.Create;
    try
      lQueryParams.SetValueAsString('OID',              IntToStr(pI));
      lQueryParams.SetValueAsString('Owner_OID',        IntToStr(pOwnerOID));
      lQueryParams.SetValueAsString('Parent_Str_Field', IntToStr(pI));
      GTIOPFManager.InsertRow(ATableName, lQueryParams, DatabaseName, PersistenceLayerName );
    finally
      lQueryParams.Free;
    end;
  end;

  procedure _InsertTestChild(const ATableName : TTableName; pI : integer);
  var
    lQueryParams : TtiQueryParams;
  begin
    lQueryParams := TtiQueryParams.Create;
    try
      lQueryParams.SetValueAsString('OID', IntToStr(pI));
      lQueryParams.SetValueAsInteger('Child_Int_Field', pI);
      lQueryParams.SetValueAsFloat('Child_Float_Field', TestIntToFloat(pI));
      GTIOPFManager.InsertRow(ATableName, lQueryParams, DatabaseName, PersistenceLayerName );
    finally
      lQueryParams.Free;
    end;
  end;

begin
  _InsertTestParent(pParentTableName, pI, pOwnerOID);
  _InsertTestChild(ATableName, pI);
end;

procedure TTestTIAutoMapOperation.SingleInheritedObjCreateAll;
var
  lChild : TtiObjectChildForTestingA;
begin

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.StrField := '1';
    lChild.IntField := 1;
    lChild.FloatField := TestIntToFloat(1) ;
    lChild.ObjectState := posCreate;
    lChild.Save(DatabaseName, PersistenceLayerName);
    Check(lChild.ObjectState = posClean, 'Failed on 4');
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 3');
    CheckEquals(1, lChild.IntField, 'Failed on 4');
    CheckNearEnough(TestIntToFloat(1), lChild.FloatField, 'FloatField');
    CheckEquals('1', lChild.StrField, 'Failed on 6');
  finally
    lChild.Free;
  end;

end;

procedure TTestTIAutoMapOperation.SingleInheritedObjUpdateAll;
var
  lChild : TtiObjectChildForTestingA;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_B, 2, -1);
  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 1');
    lChild.IntField := 2;
    lChild.FloatField := TestIntToFloat(2);
    lChild.StrField := '2';
    lChild.ObjectState := posUpdate;
    lChild.Save(DatabaseName, PersistenceLayerName);
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 2');
    CheckEquals(2, lChild.IntField, 'Failed on 3');
    CheckNearEnough(TestIntToFloat(2), lChild.FloatField, 'FloatField');
    CheckEquals('2', lChild.StrField, 'Failed on 5');
  finally
    lChild.Free;
  end;

end;

procedure TTestTIAutoMapOperation.SingleInheritedObjDeleteAll;
var
  lChild : TtiObjectChildForTestingA;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParent, cTableNameTIOPFTestChild_B, 2, -1);

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posClean = lChild.ObjectState, 'Failed on 1');
    lChild.ObjectState := posDelete;
    lChild.Save(DatabaseName, PersistenceLayerName);
  finally
    lChild.Free;
  end;

  lChild := TtiObjectChildForTestingA.Create;
  try
    lChild.OID.AsString := '1';
    lChild.ReadThis(DatabaseName, PersistenceLayerName);
    Check(posEmpty = lChild.ObjectState, 'Failed on 2');
  finally
    lChild.Free;
  end;

end;

procedure TTestTIAutoMapFramework.TestClassMaps_RegisterInheritance;
var
  lClassMaps     : TtiClassMaps;
  lClassMapChild : TtiClassMap;
  lClassMapParent : TtiClassMap;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    try
      lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);
      Check(false, 'Exception not raised with un-registered parent');
    except
      on e:exception do
      begin
      end;
    end;

    lClassMapParent := lClassMaps.AddClassMap(TtiObject);
    CheckNotNull(lClassMapParent, 'Failed creating classmap for TtiObject');
    try
      lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);
      Check(false, 'Exception not raised with un-registered child');
    except
      on e:exception do
      begin
      end;
    end;

    lClassMapChild := lClassMaps.AddClassMap(TtiObjectList);
    CheckNotNull(lClassMapChild, 'Failed creating classmap for TtiObjectList');

    try
      lClassMaps.RegisterInheritance(TtiObjectList, TtiObject);
      Fail('Exception not raised with <child> not a descendant of <parent>');
    except
      on e:exception do
      begin
      end;
    end;

    lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);

    CheckNull(lClassMapParent.ParentClassMap, 'Parent''s parent classmap should be nil');
    CheckNotNull(lClassMapChild.ParentClassMap, 'Child''s parent classmap should be assigned');
    Check(lClassMapParent = lClassMapChild.ParentClassMap, 'Child''s parent classmap not correctly assigned');
  finally
    lClassMaps.Free;
  end;
end;


procedure TTestTIAutoMapFramework.TestClassDBMappingMgr_RegisterInheritance;
begin
  TestClassMaps_RegisterInheritance;
end;


procedure TTestTIAutoMapFramework.TestClassMaps_FindParent;
var
  lClassMaps     : TtiClassMaps;
  lClassMap      : TtiClassMap;
  lClassMapChild : TtiClassMap;
  lClassMapParent : TtiClassMap;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMapParent := lClassMaps.AddClassMap(TtiObject);
    lClassMapChild := lClassMaps.AddClassMap(TtiObjectList);
    lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);
    lClassMap := lClassMaps.FindParent(TtiObjectList);
    Check(lClassMap = lClassMapParent, 'Parent classmap not correctly found');
    Check(lClassMap.PerObjAbsClass = TtiObject, 'Parent classmap.PerObjClass not of correct type');
    Check(lClassMapChild.ParentClassMap = lClassMapParent, 'Parent <> lClassMapChild.ParentClass');
  finally
    lClassMaps.Free;
  end;
end;


procedure TTestTIAutoMapFramework.TestClassMaps_HasParent;
var
  lClassMaps     : TtiClassMaps;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMaps.AddClassMap(TtiObjectList);
    lClassMaps.AddClassMap(TtiObject);
    Check((Not lClassMaps.HasParent(TtiObjectList)), 'Failed on 1');
    Check((Not lClassMaps.HasParent(TtiObject)), 'Failed on 2');
    lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);
    Check((Not lClassMaps.HasParent(TtiObject)), 'Failed on 3');
    Check(lClassMaps.HasParent(TtiObjectList), 'Failed on 4');
  finally
    lClassMaps.Free;
  end;
end;


procedure TTestTIAutoMapFramework.TestClassMaps_FindAllParents;
var
  lClassMaps : TtiClassMaps;
  lParents  : TtiClassMaps;
  lChild    : TtiClassMap ;
  lParent   : TtiClassMap ;
begin
  lClassMaps := TtiClassMaps.Create;
  try
    lParents := TtiClassMaps.Create;
    try
      lParents.OwnsObjects := false;
      lChild := lClassMaps.AddClassMap(TtiObjectList);
      lClassMaps.FindAllParents(TtiObjectList, lParents);
      CheckEquals(1, lParents.Count, 'Failed on 1');

      lParent := lClassMaps.AddClassMap(TtiObject);
      lClassMaps.RegisterInheritance(TtiObject, TtiObjectList);
      lClassMaps.FindAllParents(TtiObjectList, lParents);
      CheckEquals(2, lParents.Count, 'Failed on 2');
      Check(lParent = lParents.Items[0], 'Failed on 3');
      Check(lChild = lParents.Items[1], 'Failed on 4');

    finally
      lParents.Free;
    end;
  finally
    lClassMaps.Free;
  end;
end;


function TTestTIAutoMapOperation.TestIntToDate(pInt: Integer): TDateTime;
begin
  result := EncodeDate(2002, 1, 1) + pInt;
end;


procedure TTestTIAutoMapOperation.CollectionOfInheritedObjRead;
var
  lParentGroup : TtiObjectParentForTestingGroup;
begin
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_A, 1, -1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_B, 2, -1);
  lParentGroup := TtiObjectParentForTestingGroup.Create;
  try
    lParentGroup.OID.AsString := '-1';
    lParentGroup.Read(DatabaseName, PersistenceLayerName);

    Check(posClean = lParentGroup.ObjectState, 'lParents.ObjectState <> posClean');
    CheckEquals(2, lParentGroup.Count, 'lParents.Count');

    Check(posClean = lParentGroup.Items[0].ObjectState, 'lParents.Items[0].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[0], TtiOPFTestChildGrouped_A);
    CheckEquals('1', lParentGroup.Items[0].StrField, 'StrField');
    CheckEquals(1, lParentGroup.Items[0].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(1), lParentGroup.Items[0].FloatField, 'FloatField');

    Check(posClean = lParentGroup.Items[1].ObjectState, 'lParents.Items[1].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[1], TtiOPFTestChildGrouped_B);
    CheckEquals('2', lParentGroup.Items[1].StrField, 'StrField');
    CheckEquals(2, lParentGroup.Items[1].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(2), lParentGroup.Items[1].FloatField, 'FloatField');

  finally
    lParentGroup.Free;
  end;
end;


procedure TTestTIAutoMapOperation.CollectionOfInheritedObjWithFKRead;
var
  lParentGroup : TtiObjectParentForTestingGroup;
begin

  InserTtiObjectListForTestingInheritedGroup(1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_A, 2, 1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_B, 3, 1);

  InserTtiObjectListForTestingInheritedGroup(4);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_A, 5, 4);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_B, 6, 4);


  lParentGroup := TtiObjectParentForTestingGroup.Create;
  try
    lParentGroup.OID.AsString := '4';
    lParentGroup.Read(DatabaseName, PersistenceLayerName);

    Check(posClean = lParentGroup.ObjectState, 'lParents.ObjectState <> posClean');
    CheckEquals(2, lParentGroup.Count, 'lParents.Count');

    Check(posClean = lParentGroup.Items[0].ObjectState, 'lParents.Items[1].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[0], TtiOPFTestChildGrouped_A);
    CheckEquals('5', lParentGroup.Items[0].StrField, 'StrField');
    CheckEquals(5, lParentGroup.Items[0].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(5), lParentGroup.Items[0].FloatField, 'FloatField');

    Check(posClean = lParentGroup.Items[1].ObjectState, 'lParents.Items[0].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[1], TtiOPFTestChildGrouped_B);
    CheckEquals('6', lParentGroup.Items[1].StrField, 'StrField');
    CheckEquals(6, lParentGroup.Items[1].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(6), lParentGroup.Items[1].FloatField, 'FloatField');

  finally
    lParentGroup.Free;
  end;
end;

procedure TTestTIAutoMapOperation.CollectionOfInheritedObjWithoutFKRead;
var
  lParentGroup : TtiObjectParentForTestingGroup;
begin

  InserTtiObjectListForTestingInheritedGroup(1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_A, 2, 1);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_B, 3, 1);

  InserTtiObjectListForTestingInheritedGroup(4);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_A, 5, 4);
  InserTtiObjectListForTestingInherited(cTableNameTIOPFTestParentGrouped, cTableNameTIOPFTestChildGrouped_B, 6, 4);


  lParentGroup := TtiObjectParentForTestingGroup.Create;
  try
//    lParentGroup.OID.AsString := '4';
    CheckTrue(lParentGroup.OID.IsNull, 'lParentGroup.OID is not null');
    lParentGroup.Read(DatabaseName, PersistenceLayerName);

    Check(posClean = lParentGroup.ObjectState, 'lParents.ObjectState <> posClean');
    CheckEquals(4, lParentGroup.Count, 'lParents.Count');

    Check(posClean = lParentGroup.Items[0].ObjectState, 'lParents.Items[1].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[0], TtiOPFTestChildGrouped_A);
    CheckEquals('2', lParentGroup.Items[0].StrField, 'StrField');
    CheckEquals(2, lParentGroup.Items[0].IntField, 'IntField');
    CheckNearEnough(0.4, lParentGroup.Items[0].FloatField, 'FloatField');

    Check(posClean = lParentGroup.Items[1].ObjectState, 'lParents.Items[0].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[1], TtiOPFTestChildGrouped_B);
    CheckEquals('3', lParentGroup.Items[1].StrField, 'StrField');
    CheckEquals(3, lParentGroup.Items[1].IntField, 'IntField');
    CheckNearEnough(0.6, lParentGroup.Items[1].FloatField, 'FloatField');
        
    Check(posClean = lParentGroup.Items[2].ObjectState, 'lParents.Items[1].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[2], TtiOPFTestChildGrouped_A);
    CheckEquals('5', lParentGroup.Items[2].StrField, 'StrField');
    CheckEquals(5, lParentGroup.Items[2].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(5), lParentGroup.Items[2].FloatField, 'FloatField');

    Check(posClean = lParentGroup.Items[3].ObjectState, 'lParents.Items[0].ObjectState <> posClean');
    CheckIs(lParentGroup.Items[3], TtiOPFTestChildGrouped_B);
    CheckEquals('6', lParentGroup.Items[3].StrField, 'StrField');
    CheckEquals(6, lParentGroup.Items[3].IntField, 'IntField');
    CheckNearEnough(TestIntToFloat(6), lParentGroup.Items[3].FloatField, 'FloatField');

  finally
    lParentGroup.Free;
  end;
end;

function TTestTIAutoMapOperation.TestIntToBool(pInt: Integer): Boolean;
begin
  result := (pInt mod 2 = 0);
end;


procedure TTestTIAutoMapOperation.TestSetupAndTearDown;
begin
  Check(True);
end;

procedure TTestTIAutoMapOperation.ReadWriteBooleanFalse;
begin
  DoReadWriteBoolean(false);
end;


procedure TTestTIAutoMapOperation.ReadWriteBooleanTrue;
begin
  DoReadWriteBoolean(true);
end;


procedure TTestTIAutoMapOperation.DoReadWriteBoolean(AValue : boolean);
var
  lData : TtiOPFTestBooleanProp;
begin
  CreateTableBoolean;
  try
    lData := TtiOPFTestBooleanProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.BoolField   := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestBooleanProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(AValue, lData.BoolField);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;
end;

procedure TTestTIAutoMapOperation.ReadWriteDateDate;
begin
  DoReadWriteDateTime(Date);
end;

procedure TTestTIAutoMapOperation.DoReadWriteDateTime(AValue : TDateTime);
var
  lData : TtiOPFTestDateTimeProp;
begin
  CreateTableDateTime;
  try
    lData := TtiOPFTestDateTimeProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.DateField  := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestDateTimeProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(AValue, lData.DateField, 0.0001);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;
end;

procedure TTestTIAutoMapOperation.ReadWriteFloat;
begin
  DoReadWriteFloat(123.456);
end;

procedure TTestTIAutoMapOperation.DoReadWriteFloat(AValue : extended);
var
  lData : TtiOPFTestFloatProp;
begin
  CreateTableFloat;
  try
    lData := TtiOPFTestFloatProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.FloatField  := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestFloatProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckNearEnough(lData.FloatField, AValue);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;
end;

procedure TTestTIAutoMapOperation.ReadWriteIntegerLow;
begin
  // ToDo: Create an abstract class to handle this difference between per-layers
  //       BDEParadox layer can't handle Low(Integer), but can handle Low(Integer)+1.
  if SameText(cTIPersistBDEParadox, PersistenceLayerName) then
    DoReadWriteInteger(Low(Integer)+1)
  else if SameText(cTIPersistFBL, PersistenceLayerName) then
    DoReadWriteInteger(Low(Integer)+1)
  else
    DoReadWriteInteger(Low(Integer));
end;

procedure TTestTIAutoMapOperation.DoReadWriteInteger(AValue : integer);
var
  lData : TtiOPFTestIntegerProp;
begin
  CreateTableInteger;
  try
    lData := TtiOPFTestIntegerProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.IntField  := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestIntegerProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(AValue, lData.IntField);
    finally
      Assert(lData.TestValid(TtiOPFTestIntegerProp), CTIErrorInvalidObject);
      lData.Free;
    end;

  finally
    DropCreatedTables;
  end;
end;

procedure TTestTIAutoMapOperation.DoReadWriteOID(AValue: string);
var
  lData : TtiOPFTestOIdProp;
begin
  CreateTableString;

  try
    lData := TtiOPFTestOIdProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.OIDField.AsString  := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestOIdProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(AValue, lData.OIDField.AsString);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;

end;

procedure TTestTIAutoMapOperation.ReadWriteStream;
var
  lData : TtiOPFTestStreamProp;
  ls : string;
begin
  CreateTableStream;
  try
    lData := TtiOPFTestStreamProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      tiStringToStream(LongString, lData.StreamField);
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestStreamProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      ls := tiStreamToString(lData.StreamField);
      CheckEquals(LongString, ls);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;
end;

procedure TTestTIAutoMapOperation.ReadWriteString1;
begin
  DoReadWriteString(1);
end;

procedure TTestTIAutoMapOperation.DoReadWriteString(const pLen : integer);
var
  lData : TtiOPFTestStringProp;
  ls : string;
begin
  if pLen <= 255 then
    CreateTableString
  else
    CreateTableLongString;

  try
    ls := tiCreateStringOfSize(pLen);
    lData := TtiOPFTestStringProp.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.StrField  := ls;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestStringProp.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(Length(ls), Length(lData.StrField));
      CheckEquals(ls, lData.StrField);
    finally
      lData.Free;
    end;
  finally
    DropCreatedTables;
  end;

end;

procedure TTestTIAutoMapOperation.ReadWriteString10;
begin
  DoReadWriteString(10);
end;

procedure TTestTIAutoMapOperation.ReadWriteString100;
begin
  DoReadWriteString(100);
end;

procedure TTestTIAutoMapOperation.ReadWriteString10000;
begin
  DoReadWriteString(10000);
end;

procedure TTestTIAutoMapOperation.ReadWriteString1023;
begin
  DoReadWriteString(1023);
end;

procedure TTestTIAutoMapOperation.ReadWriteString1024;
begin
  DoReadWriteString(1024);
end;

procedure TTestTIAutoMapOperation.ReadWriteString1025;
begin
  DoReadWriteString(1025);
end;

procedure TTestTIAutoMapOperation.ReadWriteString255;
begin
  DoReadWriteString(255);
end;

procedure TTestTIAutoMapOperation.ReadWriteString256;
begin
  DoReadWriteString(256);
end;

procedure TTestTIAutoMapOperation.ReadWriteString257;
begin
  DoReadWriteString(257);
end;

procedure TTestTIAutoMapOperation.ReadWriteString5000;
begin
  DoReadWriteString(5000);
end;

procedure TTestTIAutoMapOperation.ReadWriteString511;
begin
  DoReadWriteString(511);
end;

procedure TTestTIAutoMapOperation.ReadWriteString512;
begin
  DoReadWriteString(512);
end;

procedure TTestTIAutoMapOperation.ReadWriteString513;
begin
  DoReadWriteString(513);
end;

procedure TTestTIAutoMapOperation.ReadWriteDateNow;
begin
  DoReadWriteDateTime(Now);
end;

procedure TTestTIAutoMapOperation.ReadWriteDateMax;
begin
  DoReadWriteDateTime(MaxDateTime);
end;

procedure TTestTIAutoMapOperation.ReadWriteDateMin;
begin
  DoReadWriteDateTime(MinDateTime);
end;

procedure TTestTIAutoMapOperation.ReadWriteDate0;
begin
  DoReadWriteDateTime(0);
end;

procedure TTestTIAutoMapOperation.ReadWriteInteger_10;
begin
  DoReadWriteInteger(-10);
end;

procedure TTestTIAutoMapOperation.ReadWriteOID1;
begin
  DoReadWriteOID('1');
end;

procedure TTestTIAutoMapOperation.ReadWriteOID2;
begin
  DoReadWriteOID('753951');
end;

procedure TTestTIAutoMapOperation.ReadWriteInteger0;
begin
  DoReadWriteInteger(0);
end;

procedure TTestTIAutoMapOperation.ReadWriteInteger10;
begin
  DoReadWriteInteger(10);
end;

procedure TTestTIAutoMapOperation.ReadWriteIntegerHigh;
begin
  // ToDo: Create an abstract class to handle this difference between per-layers
  //       FBLib layer can't handle High(Integer), but can handle High(Integer)-1.
  if SameText(cTIPersistFBL, PersistenceLayerName) then
    DoReadWriteInteger(High(Integer)-1)
  else
    DoReadWriteInteger(High(Integer));
end;

{$IFDEF TESTINT64}
procedure TTestTIAutoMapOperation.DoReadWriteInt64(AValue: Int64);
var
  lData : TtiOPFTestInt64Prop;
begin
// Assume table exists
//  CreateTableInt64(DatabaseName, PersistenceLayerName);

  try
    // clear table (seeing that we are not creating it above)
    GTIOPFManager.DeleteRow(cTIQueryTableNameInt64, nil);
    lData := TtiOPFTestInt64Prop.Create;
    try
      lData.ObjectState := posCreate;
      lData.OID.AsString := '1';
      lData.Int64Field  := AValue;
      lData.Save(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
    finally
      lData.Free;
    end;

    lData := TtiOPFTestInt64Prop.Create;
    try
      lData.OID.AsString := '1';
      lData.Read(DatabaseName, PersistenceLayerName);
      Check(lData.ObjectState = posClean, 'Failed on ObjectState = posClean');
      CheckEquals(AValue, lData.Int64Field);
    finally
      Assert(lData.TestValid(TtiOPFTestInt64Prop), CTIErrorInvalidObject);
      lData.Free;
    end;

  finally
//    DropTestTable;
  end;
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64_10;
begin
  DoReadWriteInt64(-10);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt640;
begin
  DoReadWriteInt64(0);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt6410;
begin
  DoReadWriteInt64(10);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64High;
begin
  DoReadWriteInt64(High(Int64));
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64Low;
begin
  DoReadWriteInt64(Low(Int64));
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64High1;
begin
  DoReadWriteInt64(High(Integer) - 1);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64Low1;
var
  val : Int64;
begin
  val := Low(Integer);
  val := val - 1;
  DoReadWriteInt64(val);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64High2;
begin
  DoReadWriteInt64(High(Integer));
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64Low2;
begin
  DoReadWriteInt64(Low(Integer));
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64High3;
var
  val : Int64;
begin
  val := High(Integer);
  val := val + 1;
  DoReadWriteInt64(val);
end;

procedure TTestTIAutoMapOperation.ReadWriteInt64Low3;
begin
  DoReadWriteInt64(Low(Integer) + 1);
end;

{$ENDIF}

end.











