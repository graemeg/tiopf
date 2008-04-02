unit AdrsType_TST;

interface
uses
  tiTestFramework,
  AdrsType_BOM,
  AdrsBase_TST;

type

  TTestAdrsType = class(TAdrsBaseTestCase)
  published

    procedure AdrsTypeAbs_Find;
    procedure AdrsTypeAbs_AdrsTypeAsStringByOID;

    procedure AdrsTypeList_Read;
    procedure AdrsType_Save;
    procedure AdrsType_Update;
    procedure AdrsType_Delete;
    procedure AdrsType_Equals;
    procedure AdrsType_Assign;
    procedure AdrsType_IsValid;

    procedure EAdrsTypeList_Read;
    procedure EAdrsType_Save;
    procedure EAdrsType_Update;
    procedure EAdrsType_Delete;
    procedure EAdrsType_Equals;
    procedure EAdrsType_Assign;
    procedure EAdrsType_IsValid;

  end;

  TAdrsTypeXMLLightTestCase = class(TTestAdrsType)
  protected
    procedure SetUpOnce; override;
  end;

  TAdrsTypeIBXTestCase = class(TTestAdrsType)
  protected
    procedure SetUpOnce; override;
  end;

procedure RegisterTests;

implementation
uses
  TestFramework,
  SysUtils,
  tiObject,
  tiOPFManager,
  tiQuery,
  tiConstants,
  AdrsUnitTestConstants;

{ TTestAdrs }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(cTIPersistIBX, TAdrsTypeIBXTestCase.Suite);
  TestFramework.RegisterTest(cTIPersistXMLLight, TAdrsTypeXMLLightTestCase.Suite);
end;

{ TTestAdrs }

procedure TTestAdrsType.EAdrsType_Assign;
var
  LItem1: TEAdrsType;
  LItem2: TEAdrsType;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1);
    LItem2:= AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType2);
    LItem2.Assign(LItem1);
    AdrsTypeSetup.EAdrsTypeCheck(LItem2, COIDEAdrsType1);
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

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

procedure TTestAdrsType.EAdrsType_Equals;
var
  LItem1: TEAdrsType;
  LItem2: TEAdrsType;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1);
    LItem2:= AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1);
    TestTIObjectEquals(LItem1, LItem2, 'Text');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestAdrsType.EAdrsType_IsValid;
var
  LItem: TEAdrsType;
  LErrors: TtiObjectErrors;
begin
  LErrors:= nil;
  LItem:= nil;
  try
    LErrors:= TtiObjectErrors.Create;
    LItem:= AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1);

    Check(LItem.IsValid(LErrors));

    LItem.Text:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals('Text', LErrors.Items[0].ErrorProperty);
    CheckEquals(CErrorAdrsTypeAbsTextNotAssigned, LErrors.Items[0].ErrorMessage);

  finally
    LErrors.Free;
    LItem.Free;
  end;
end;

type

  TAdrsTypeListForTesting = class(TAdrsTypeListAbs)
  end;

procedure TTestAdrsType.AdrsTypeAbs_AdrsTypeAsStringByOID;
var
  LList: TAdrsTypeListForTesting;
begin
  LList:= TAdrsTypeListForTesting.Create;
  try
    LList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1));
    LList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType2));
    CheckEquals(TAdrsType(LList.Items[0]).Text, LList.AdrsTypeAsStringByOID(COIDAdrsType1));
    CheckEquals(TAdrsType(LList.Items[1]).Text, LList.AdrsTypeAsStringByOID(COIDAdrsType2));
    CheckEquals('', LList.AdrsTypeAsStringByOID('YouWontFindMe'));
  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.AdrsTypeAbs_Find;
var
  LList: TAdrsTypeListForTesting;
begin
  LList:= TAdrsTypeListForTesting.Create;
  try
    LList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1));
    LList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType2));
    CheckNotNull(LList.Find(COIDAdrsType1));
    CheckEquals(COIDAdrsType1, LList.Find(COIDAdrsType1).OID.AsString);
    CheckNotNull(LList.Find(COIDAdrsType2));
    CheckEquals(COIDAdrsType2, LList.Find(COIDAdrsType2).OID.AsString);
    CheckNull(LList.Find('YouWontFindMe'));
  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.AdrsTypeList_Read;
var
  LList: TAdrsTypeList;
begin
  AdrsTypeSetup.AdrsTypeInsert(cOIDEAdrsType2);
  AdrsTypeSetup.AdrsTypeInsert(cOIDEAdrsType1);
  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(2, LList.Count);
    CheckObjectState(posClean, LList);
    CheckObjectState(posClean, LList.Items[0]);
    CheckObjectState(posClean, LList.Items[1]);

    AdrsTypeSetup.AdrsTypeCheck(LList.Items[0], COIDEAdrsType1);
    CheckEquals(cOIDEAdrsType1, LList.Items[0].OID.AsString);

    AdrsTypeSetup.AdrsTypeCheck(LList.Items[1], cOIDEAdrsType2);
    CheckEquals(cOIDEAdrsType2, LList.Items[1].OID.AsString);

  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.AdrsType_Assign;
var
  LItem1: TAdrsType;
  LItem2: TAdrsType;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1);
    LItem2:= AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType2);
    LItem2.Assign(LItem1);
    AdrsTypeSetup.AdrsTypeCheck(LItem2, COIDAdrsType1);
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestAdrsType.AdrsType_Delete;
var
  LList: TAdrsTypeList;
begin
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);

  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LList.Items[0].Deleted:= True;
    LList.Save;
  finally
    LList.Free;
  end;

  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.AdrsType_Equals;
var
  LItem1: TAdrsType;
  LItem2: TAdrsType;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1);
    LItem2:= AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1);
    TestTIObjectEquals(LItem1, LItem2, 'Text');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestAdrsType.AdrsType_IsValid;
var
  LItem: TAdrsType;
  LErrors: TtiObjectErrors;
begin
  LErrors:= nil;
  LItem:= nil;
  try
    LErrors:= TtiObjectErrors.Create;
    LItem:= AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1);

    Check(LItem.IsValid(LErrors));

    LItem.Text:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals('Text', LErrors.Items[0].ErrorProperty);
    CheckEquals(CErrorAdrsTypeAbsTextNotAssigned, LErrors.Items[0].ErrorMessage);

  finally
    LErrors.Free;
    LItem.Free;
  end;
end;

procedure TTestAdrsType.AdrsType_Save;
var
  LList: TAdrsTypeList;
  LItem: TAdrsType;
begin
  LList:= TAdrsTypeList.Create;
  try
    LItem:= AdrsTypeSetup.AdrsTypeCreate(cOIDAdrsType1);
    LItem.Dirty:= True;
    LList.Add(LItem);
    LList.Save;
    CheckObjectState(posClean, LItem);
  finally
    LList.Free;
  end;

  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    AdrsTypeSetup.AdrsTypeCheck(LList.Items[0], cOIDAdrsType1);
  finally
    LList.Free;
  end;

end;

procedure TTestAdrsType.AdrsType_Update;
var
  LList: TAdrsTypeList;
  LItem: TAdrsType;
begin

  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LItem:= LList.Items[0];
    AdrsTypeSetup.AdrsTypeCheck(LItem, cOIDAdrsType1);
    AdrsTypeSetup.AdrsTypeAssign(LItem, cOIDAdrsType2);
    LItem.Dirty:= True;
    LList.Save;
  finally
    LList.Free;
  end;

  LList:= TAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    AdrsTypeSetup.AdrsTypeCheck(LList.Items[0], cOIDAdrsType2);
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



{ TAdrsTypeXMLLightTestCase }

procedure TAdrsTypeXMLLightTestCase.SetUpOnce;
begin
  PersistenceLayerName:= CTIPersistXMLLight;
  inherited;
end;

{ TAdrsTypeIBXTestCase }

procedure TAdrsTypeIBXTestCase.SetUpOnce;
begin
  PersistenceLayerName:= CTIPersistIBX;
  inherited;
end;

end.
