unit AdrsType_TST;

interface
uses
  tiTestFramework,
  AdrsType_BOM,
  Adrs_TST;

type

  TTestAdrsType = class(TAdrsTestCase)
  published
    procedure EAdrsTypeList_Read;
    procedure EAdrsType_Save;
    procedure EAdrsType_Update;
    procedure EAdrsType_Delete;
  end;

procedure RegisterTests;

implementation
uses
  TestFramework,
  SysUtils,
  tiObject,
  tiOPFManager,
  tiQuery,
  AdrsUnitTestConstants;

{ TTestAdrs }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestAdrsType.Suite);
end;

{ TTestAdrs }

procedure TTestAdrsType.EAdrsType_Delete;
var
  LList: TEAdrsTypeList;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);

  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LList.Items[0].Deleted:= True;
    LList.Save;
  finally
    LList.Free;
  end;

  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;

end;

procedure TTestAdrsType.EAdrsTypeList_Read;
var
  LList: TEAdrsTypeList;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(2, LList.Count);
    CheckObjectState(posClean, LList);
    CheckObjectState(posClean, LList.Items[0]);
    CheckObjectState(posClean, LList.Items[1]);

    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[0], cOIDEAdrsType1);
    CheckEquals(cOIDEAdrsType1, LList.Items[0].OID.AsString);

    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[1], cOIDEAdrsType2);
    CheckEquals(cOIDEAdrsType2, LList.Items[1].OID.AsString);

  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.EAdrsType_Save;
var
  LList: TEAdrsTypeList;
  LItem: TEAdrsType;
begin
  LList:= TEAdrsTypeList.Create;
  try
    LItem:= AdrsTypeSetup.EAdrsTypeCreate(cOIDEAdrsType1);
    LItem.Dirty:= True;
    LList.Add(LItem);
    LList.Save;
    CheckObjectState(posClean, LItem);
  finally
    LList.Free;
  end;

  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[0], cOIDEAdrsType1);
  finally
    LList.Free;
  end;

end;

procedure TTestAdrsType.EAdrsType_Update;
var
  LList: TEAdrsTypeList;
  LItem: TEAdrsType;
begin

  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LItem:= LList.Items[0];
    AdrsTypeSetup.EAdrsTypeCheck(LItem, cOIDEAdrsType1);
    AdrsTypeSetup.EAdrsTypeAssign(LItem, cOIDEAdrsType2);
    LItem.Dirty:= True;
    LList.Save;
  finally
    LList.Free;
  end;

  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[0], cOIDEAdrsType2);
  finally
    LList.Free;
  end;

end;



end.
