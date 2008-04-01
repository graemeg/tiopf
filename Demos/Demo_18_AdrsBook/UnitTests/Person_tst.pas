unit Person_tst;

interface
uses
  tiTestFramework,
  tiVisitorDB,
  Adrs_BOM,
  Adrs_tst,
  AdrsType_BOM;

type

  TTestPerson = class(TAdrsTestCase)
  published
    procedure TestPersonFlat_Read;
    procedure TestPersonFlat_Save;
    procedure TestPersonFlat_Update;
    procedure TestPersonFlat_Delete;
    procedure TestPersonFlat_Equals;
    procedure TestPersonFlat_Assign;
    procedure TestPersonFlat_Clone;

    procedure TestEAdrs_Read            ;
    procedure TestEAdrs_Save            ;
    procedure TestEAdrs_Update          ;
    procedure TestEAdrs_Delete          ;
    procedure TestEAdrs_Equals          ;
    procedure TestEAdrs_Assign          ;
    procedure TestEAdrs_Clone           ;
    procedure TestEAdrs_AdrsType;

    procedure TestAdrs_Read             ;
    procedure TestAdrs_Save             ;
    procedure TestAdrs_Update           ;
    procedure TestAdrs_Delete           ;
    procedure TestAdrs_Equals           ;
    procedure TestAdrs_Assign           ;
    procedure TestAdrs_Clone            ;
    procedure TestAdrs_EAdrsType        ;

  public

    procedure TestPersonCompound_Read   ;
    procedure TestPersonCompound_Save   ;
    procedure TestPersonCompound_Update ;
    procedure TestPersonCompound_Delete ;

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

{ TTestPerson }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestPerson.Suite);
end;

{ TTestPerson }

procedure TTestPerson.TestAdrs_Delete;
begin
end;

procedure TTestPerson.TestAdrs_EAdrsType;
begin

end;

procedure TTestPerson.TestAdrs_Equals;
begin

end;

procedure TTestPerson.TestAdrs_Read;
begin
end;

procedure TTestPerson.TestAdrs_Save;
begin
end;

procedure TTestPerson.TestAdrs_Update;
begin
end;

procedure TTestPerson.TestEAdrs_Delete;
var
  LList: TPersonList;
  LPerson: TPerson;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  PersonTestSetup.PersonInsert(cOIDPerson1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs1, COIDEAdrsType1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    EAdrsTestSetup.EAdrsAssign(LPerson.EAddressList.Items[0], COIDEAdrs2, COIDEAdrsType2);
    LPerson.EAddressList.Items[0].Deleted:= True;
    LPerson.Save;
    CheckObjectState(posDeleted, LPerson.EAddressList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    CheckEquals(0, LPerson.EAddressList.Count);
  finally
    LList.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Equals;
var
  LItem1: TEAdrs;
  LItem2: TEAdrs;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, cOIDEAdrsType1);
    LItem2:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, cOIDEAdrsType1);
    Check(LItem1.Equals(LItem2));

  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Read;
var
  LList: TPersonList;
  LPerson: TPerson;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);
  PersonTestSetup.PersonInsert(cOIDPerson1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs1, COIDEAdrsType1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs2, COIDEAdrsType2);
  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LPerson:= LList.Items[0];
    CheckEquals(2, LPerson.EAddressList.Count);
    EAdrsTestSetup.EAdrsCheck(LPerson.EAddressList.Items[0], COIDEAdrs1, COIDEAdrsType1);
    CheckEquals(COIDEAdrs1, LPerson.EAddressList.Items[0].OID);
    EAdrsTestSetup.EAdrsCheck(LPerson.EAddressList.Items[1], COIDEAdrs2, COIDEAdrsType2);
    CheckEquals(COIDEAdrs2, LPerson.EAddressList.Items[1].OID);
  finally
    LList.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Save;
var
  LList: TPersonList;
  LPerson: TPerson;
  LItem: TEAdrs;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);
  PersonTestSetup.PersonInsert(cOIDPerson1);

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    LItem:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, cOIDEAdrsType1);
    LItem.Dirty:= True;
    LPerson.EAddressList.Add(LItem);
    LPerson.Save;
    CheckObjectState(posClean, LItem);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LPerson:= LList.Items[0];
    EAdrsTestSetup.EAdrsCheck(LPerson.EAddressList.Items[0], COIDEAdrs1, COIDEAdrsType1);
    CheckEquals(COIDEAdrs1, LPerson.EAddressList.Items[0].OID);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.TestEAdrs_Update;
var
  LList: TPersonList;
  LPerson: TPerson;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);
  PersonTestSetup.PersonInsert(cOIDPerson1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs1, COIDEAdrsType1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    EAdrsTestSetup.EAdrsAssign(LPerson.EAddressList.Items[0], COIDEAdrs2, COIDEAdrsType2);
    LPerson.EAddressList.Items[0].Dirty:= True;
    LPerson.Save;
    CheckObjectState(posClean, LPerson.EAddressList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    EAdrsTestSetup.EAdrsCheck(LPerson.EAddressList.Items[0], COIDEAdrs2, COIDEAdrsType2);
    CheckEquals(COIDEAdrs1, LPerson.EAddressList.Items[0].OID);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.TestPersonFlat_Delete;
var
  LList: TPersonList;
begin
  PersonTestSetup.PersonInsert(COIDPerson1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LList.Items[0].Deleted:= True;
    LList.Items[0].Save;
    CheckObjectState(posDeleted, LList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.TestPersonFlat_Equals;
var
  LItem1: TPerson;
  LItem2: TPerson;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= PersonTestSetup.PersonCreate(COIDPerson1);
    LItem2:= PersonTestSetup.PersonCreate(COIDPerson1);
    Check(LItem1.Equals(LItem2));
    
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Read;
var
  LList: TPersonList;
begin
  PersonTestSetup.PersonInsert(COIDPerson2);
  PersonTestSetup.PersonInsert(COIDPerson1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(2, LList.Count, 'Failed on lPeople.Count');
    CheckObjectState(posClean, LList);
    CheckObjectState(posPK, LList.Items[0]);
    CheckObjectState(posPK, LList.Items[1]);
    PersonTestSetup.PersonCheck(LList.Items[0], COIDPerson1);
    CheckEquals(COIDPerson1, LList.Items[0].OID.AsString);
    PersonTestSetup.PersonCheck(LList.Items[1], COIDPerson2);
    CheckEquals(COIDPerson2, LList.Items[1].OID.AsString);
  finally
    LList.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Save;
var
  LList: TPersonList;
  LItem: TPerson;
begin

  LItem:= PersonTestSetup.PersonCreate(COIDPerson1);
  try
    LItem.Dirty:= True;
    LItem.Save;
    CheckObjectState(posClean, LItem);
  finally
    LItem.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count, 'Failed on lPeople.Count');
    PersonTestSetup.PersonCheck(LList.Items[0], COIDPerson1);
    CheckEquals(COIDPerson1, LList.Items[0].OID.AsString);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.TestPersonFlat_Update;
var
  LList: TPersonList;
begin

  PersonTestSetup.PersonInsert(COIDPerson1);

  LList:= TPersonList.Create;
  try
    LList.Read;
    CheckEquals(1, LList.Count, 'Failed on lPeople.Count');
    PersonTestSetup.PersonAssign(LList.Items[0], COIDPerson2);
    LList.Items[0].Dirty:= True;
    LList.Items[0].Save;
    CheckObjectState(posClean, LList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    PersonTestSetup.PersonCheck(LList.Items[0], COIDPerson2);
    CheckEquals(COIDPerson1, LList.Items[0].OID.AsString);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.TestPersonCompound_Delete;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestPerson(-1, cOIDPerson, IntToStr(cOIDPerson));
//  InsertTestAdrs(  cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.People.Count, 'gAdrsBook.People.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count');
  gAdrsBook.People.Items[0].ObjectState:= posDelete;
Assert(False, 'Under construction');
//  gAdrsBook.Save;
  Check(posDeleted = gAdrsBook.People.Items[0].ObjectState, 'Failed on posDeleted = gAdrsBook.People.Items[0]');

  FreeAndNilAdrsBook;
  gAdrsBook.Read;
  CheckEquals(0, gAdrsBook.People.Count, 'gAdrsBook.People.Count');

end;

procedure TTestPerson.TestPersonCompound_Read;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestPerson(-1, cOIDPerson, IntToStr(cOIDPerson));
//  InsertTestAdrs(  cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.People.Count, 'gAdrsBook.People.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count');
//  CheckPerson(gAdrsBook.People.Items[0], cOIDPerson, IntToStr(cOIDPerson));
//  CheckAdrs(gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
//  CheckEAdrs(gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr(cOIDEAdrs));
end;

procedure TTestPerson.TestPersonCompound_Save;
var
  lPerson: TPerson;
  lAdrs  : TAdrs;
  lEAdrs : TEAdrs;
begin

//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  gAdrsBook.Read;

  lPerson:= TPerson.Create;
//  lPerson.OID.AsString:= IntToStr(cOIDPerson);
//  lPerson.ObjectState:= posCreate;
//  lPerson.Title:= IntToStr(cOIDPerson);
//  lPerson.Initials := IntToStr(cOIDPerson);
//  lPerson.FirstName:= IntToStr(cOIDPerson);
//  lPerson.LastName := IntToStr(cOIDPerson);
//  lPerson.Notes    := IntToStr(cOIDPerson);
//  gAdrsBook.People.Add(lPerson);

  lAdrs  := TAdrs.Create;
  lAdrs.OID.AsString:= IntToStr(cOIDAdrs);
  lAdrs.ObjectState:= posCreate;
//  lAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
  lAdrs.Lines:= IntToStr(cOIDAdrs);
  lAdrs.Suburb:= IntToStr(cOIDAdrs);
  lAdrs.State:= IntToStr(cOIDAdrs);
  lAdrs.PCode:= IntToStr(cOIDAdrs);
  lAdrs.Country:= IntToStr(cOIDAdrs);
  lPerson.AddressList.Add(lAdrs);

  lEAdrs := TEAdrs.Create;;
//  lEADrs.OID.AsString:= IntToStr(cOIDEAdrs);
  lEAdrs.ObjectState:= posCreate;
//  lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
//  lEAdrs.Text:= IntToStr(cOIDEAdrs);
  lPerson.EAddressList.Add(lEAdrs);
Assert(False, 'Under construction');
//  gAdrsBook.Save;

  FreeAndNilAdrsBook;
  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.People.Count, 'gAdrsBook.People.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count');
  //CheckPerson(gAdrsBook.People.Items[0], cOIDPerson, IntToStr(cOIDPerson));
//  CheckAdrs(gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
//  CheckEAdrs(gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr(cOIDEAdrs));

end;

procedure TTestPerson.TestPersonCompound_Update;
var
  lPerson: TPerson;
  lAdrs  : TAdrs;
  lEAdrs : TEAdrs;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  //InsertTestPerson(-1, cOIDPerson, IntToStr(cOIDPerson));
//  InsertTestAdrs(  cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));
  gAdrsBook.Read;

  lPerson:= gAdrsBook.People.Items[0];
  lPerson.ObjectState:= posUpdate;
  lPerson.Title      := cUpdateValue;
  lPerson.Initials   := cUpdateValue;
  lPerson.FirstName  := cUpdateValue;
  lPerson.LastName   := cUpdateValue;
  lPerson.Notes      := cUpdateValue;

  lAdrs            := lPerson.AddressList.Items[0];
  lAdrs.ObjectState:= posUpdate;
//  lAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
  lAdrs.Lines      := cUpdateValue;
  lAdrs.Suburb     := cUpdateValue;
  lAdrs.State      := cUpdateValue;
  lAdrs.PCode      := cUpdateValue;
  lAdrs.Country    := cUpdateValue;

  lEAdrs           := lPerson.EAddressList.Items[0];
  lEAdrs.ObjectState:= posUpdate;
//  lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
  lEAdrs.Text:= cUpdateValue;
Assert(False, 'Under construction');
//  gAdrsBook.Save;

  FreeAndNilAdrsBook;
  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.People.Count, 'gAdrsBook.People.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count');
  //CheckPerson(gAdrsBook.People.Items[0], cOIDPerson, cUpdateValue);
//  CheckAdrs(gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem1, cUpdateValue);
//  CheckEAdrs(gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem, cUpdateValue);

end;

procedure TTestPerson.TestAdrs_Assign;
begin
end;

procedure TTestPerson.TestAdrs_Clone;
begin
end;

procedure TTestPerson.TestEAdrs_AdrsType;
begin

end;

procedure TTestPerson.TestEAdrs_Assign;
var
  LFrom: TEAdrs;
  LTo  : TEAdrs;
begin
  LFrom:= EAdrsTestSetup.EAdrsCreate(COIDEAdrs1, cOIDEAdrsType1);
  try
    LTo:= TEAdrs.Create;
    try
      LTo.Assign(LFrom);
      Check(LFrom.Equals(LTo));
    finally
      LTo.Free;
    end;
  finally
    LFrom.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Clone;
var
  LFrom: TEAdrs;
  LTo  : TEAdrs;
begin
  LFrom:= EAdrsTestSetup.EAdrsCreate(COIDEAdrs1, cOIDEAdrsType1);
  try
    LTo:= LFrom.Clone as TEAdrs;
    try
      Check(LFrom.Equals(LTo));
    finally
      LTo.Free;
    end;
  finally
    LFrom.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Assign;
var
  LFrom: TPerson;
  LTo  : TPerson;
begin
  LFrom:= PersonTestSetup.PersonCreate(COIDPerson1);
  try
    LTo:= TPerson.Create;
    try
      LTo.Assign(LFrom);
      PersonTestSetup.PersonCheck(LTo, COIDPerson1);
    finally
      LTo.Free;
    end;
  finally
    LFrom.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Clone;
var
  LFrom: TPerson;
  LTo  : TPerson;
begin
  LFrom:= PersonTestSetup.PersonCreate(COIDPerson1);
  try
    LTo:= LFrom.Clone;
    try
      PersonTestSetup.PersonCheck(LTo, COIDPerson1);
    finally
      LTo.Free;
    end;
  finally
    LFrom.Free;
  end;
end;

end.
