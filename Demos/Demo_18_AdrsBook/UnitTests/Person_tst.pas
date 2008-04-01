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
  private
    procedure InsertTestAdrs(pOwnerOID: Integer; pOID: Integer; pAdrsTypeOID: integer; const pValue: string);
    procedure InsertTestEAdrs(pOwnerOID: Integer; pOID: Integer; pAdrsTypeOID: integer; const pValue: string);
    procedure InsertTestCompany(pOID: Integer; const pValue: string);

    procedure CheckAdrs(pData: TAdrs; pOID: Integer; pAdrsTypeOID: integer; const pValue: string);
    procedure CheckEAdrs(pData: TEAdrs; pOID: Integer; pAdrsTypeOID: integer; const pValue: string);
    procedure CheckCompany(pData: TCompany; pOID: Integer; const pValue: string);
    function  CreateCompoundCompany: TCompany;
    procedure CheckCompoundCompany(pCompany: TCompany);

  public

    procedure TestAdrs_Read             ;
    procedure TestAdrs_Save             ;
    procedure TestAdrs_Update           ;
    procedure TestAdrs_Delete           ;
    procedure TestAdrs_Assign           ;
    procedure TestAdrs_Clone            ;

    procedure TestEAdrs_Read            ;
    procedure TestEAdrs_Save            ;
    procedure TestEAdrs_Update          ;
    procedure TestEAdrs_Delete          ;
    procedure TestEAdrs_Assign          ;
    procedure TestEAdrs_Clone           ;

  published
    procedure TestPersonFlat_Read       ;
    procedure TestPersonFlat_Save       ;
    procedure TestPersonFlat_Update     ;
    procedure TestPersonFlat_Delete     ;
    procedure TestPersonFlat_Assign     ;
    procedure TestPersonFlat_Clone      ;

public

    procedure TestPersonCompound_Read   ;
    procedure TestPersonCompound_Save   ;
    procedure TestPersonCompound_Update ;
    procedure TestPersonCompound_Delete ;

    procedure TestCompanyFlat_Read      ;
    procedure TestCompanyFlat_Save      ;
    procedure TestCompanyFlat_Update    ;
    procedure TestCompanyFlat_Delete    ;
    procedure TestCompanyFlat_Assign    ;
    procedure TestCompanyFlat_Clone     ;

    procedure TestCompanyCompound_Read  ;
    procedure TestCompanyCompound_Save  ;
    procedure TestCompanyCompound_Update;
    procedure TestCompanyCompound_Delete;
    procedure TestCompanyCompound_Assign;
    procedure TestCompanyCompound_Clone ;

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
var
  lAdrses: TAddressList;
  lAdrs: TAdrs;
begin
//  InsertTestLookupListName(cOIDEAdrsType1);
  InsertTestAdrs(-1, cOIDAdrs, StrToInt(cOIDEAdrsType1), IntToStr(cOIDAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
    lAdrs:= lAdrses.Items[0];
    lAdrs.ObjectState:= posDelete;
//    lAdrses.Save;
Assert(False, 'Under construction');
    Check(posDeleted = lAdrs.ObjectState, 'Failed on lAdr.ObjectState = posDelete');
  finally
    lAdrses.Free;
  end;

  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(0, lAdrses.Count, 'Failed on lAdrses.Count');
  finally
    lAdrses.Free;
  end;

end;

procedure TTestPerson.TestAdrs_Read;
var
  lAdrses: TAddressList;
begin
//  InsertTestLookupListName(cOIDEAdrsType1);
  InsertTestAdrs(-1, cOIDAdrs, StrToInt(cOIDEAdrsType1), IntToStr(cOIDAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
    CheckAdrs(lAdrses.Items[0], cOIDAdrs, StrToInt(cOIDEAdrsType1), IntToStr(cOIDAdrs));
  finally
    lAdrses.Free;
  end;
end;

procedure TTestPerson.TestAdrs_Save;
var
  lAdrses: TAddressList;
  lAdrs: TAdrs;
begin
//  InsertTestLookupListName(cOIDEAdrsType1);
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
    lAdrses.ObjectState:= posClean;
    lAdrs:= TAdrs.Create;
    lAdrs.OID.AsString:= IntToStr(cOIDAdrs);
    lAdrs.ObjectState:= posCreate;
    lAdrs.Lines  := IntToStr(cOIDAdrs);
    lAdrs.Suburb := IntToStr(cOIDAdrs);
    lAdrs.State  := IntToStr(cOIDAdrs);
    lAdrs.PCode  := IntToStr(cOIDAdrs);
    lAdrs.Country:= IntToStr(cOIDAdrs);
//    lAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
    lAdrses.Add(lAdrs);
//    lAdrses.Save;
Assert(False, 'Under construction');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
  finally
    lAdrses.Free;
  end;

  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
//    CheckAdrs(lAdrses.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
  finally
    lAdrses.Free;
  end;

end;

procedure TTestPerson.TestAdrs_Update;
var
  lAdrses: TAddressList;
  lAdrs  : TAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestAdrs(-1, cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
    lAdrs:= lAdrses.Items[0];
    lAdrs.Lines  := cUpdateValue;
    lAdrs.Suburb := cUpdateValue;
    lAdrs.State  := cUpdateValue;
    lAdrs.PCode  := cUpdateValue;
    lAdrs.Country:= cUpdateValue;
//    lAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
    lAdrs.ObjectState:= posUpdate;
//    lAdrses.Save;
Assert(False, 'Under construction');
  finally
    lAdrses.Free;
  end;

  lAdrses:= TAddressList.Create;
  try
    lAdrses.OID.AsString:= '-1';
//    lAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
//    CheckAdrs(lAdrses.Items[0], cOIDAdrs, cOIDLookupListItem1, cUpdateValue);
  finally
    lAdrses.Free;
  end;

end;

procedure TTestPerson.TestCompanyFlat_Delete;
var
  lCompanies: TCompanyList;
begin
  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lCompanies.Count, 'Failed on lCompanies.Count');
    lCompanies.Items[0].ObjectState:= posDelete;
//    lCompanies.Save;
    Check(posDeleted = lCompanies.Items[0].ObjectState, 'Failed on lCompanies.Items[0].ObjectState = posDeleted');
  finally
    lCompanies.Free;
  end;
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
    CheckEquals(0, lCompanies.Count, 'Failed on lCompanies.Count');
  finally
    lCompanies.Free;
  end;
end;

procedure TTestPerson.TestCompanyFlat_Read;
var
  lCompanies: TCompanyList;
begin
  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lCompanies.Count, 'Failed on lCompanies.Count');
    Check(posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean');
    Check(posPK = lCompanies.Items[0].ObjectState, 'Failed on lCompanies.Items[0].ObjectState = posClean');
    CheckCompany(lCompanies.Items[0], cOIDCompany, IntToStr(cOIDCompany));
  finally
    lCompanies.Free;
  end;
end;

procedure TTestPerson.TestCompanyFlat_Save;
var
  lCompanies: TCompanyList;
  lCompany: TCompany;
begin
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
    lCompany:= TCompany.Create;
    lCompany.OID.AsString:= IntToStr(cOIDCompany);
    lCompany.Notes:= IntToStr(cOIDCompany);
    lCompany.CompanyName:= IntToStr(cOIDCompany);
    lCompany.ObjectState:= posCreate;
    lCompanies.Add(lCompany);
//    lCompanies.Save;
Assert(False, 'Under construction');
    Check(posClean = lCompany.ObjectState, 'Failed on lCompany.ObjectState = posClean');
  finally
    lCompanies.Free;
  end;
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lCompanies.Count, 'Failed on lCompanies.Count');
    Check(posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean');
    Check(posPK = lCompanies.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
    CheckCompany(lCompanies.Items[0], cOIDCompany, IntToStr(cOIDCompany));
  finally
    lCompanies.Free;
  end;
end;

procedure TTestPerson.TestCompanyFlat_Update;
var
  lCompanies: TCompanyList;
  lCompany: TCompany;
begin
  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lCompanies.Count, 'Failed on lCompanies.Count');
    lCompany:= lCompanies.Items[0];
    lCompany.Notes:= cUpdateValue;
    lCompany.CompanyName:= cUpdateValue;
    lCompany.ObjectState:= posUpdate;
//    lCompanies.Save;
    Check(posClean = lCompany.ObjectState, 'Failed on lCompany.ObjectState = posClean');
  finally
    lCompanies.Free;
  end;
  lCompanies:= TCompanyList.Create;
  try
    lCompanies.OID.AsString:= '-1';
//    lCompanies.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lCompanies.Count, 'Failed on lCompanies.Count');
    Check(posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean');
    Check(posPK = lCompanies.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
    CheckCompany(lCompanies.Items[0], cOIDCompany, cUPdateValue);
  finally
    lCompanies.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Delete;
var
  lEAdrses: TEAddressList;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestEAdrs(-1, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
//    lEAdrses.Read;
Assert(False, 'Under construction');
    CheckEquals(1, lEAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
    lEAdrses.Items[0].ObjectState:= posDelete;
Assert(False, 'Under construction');
//    lEAdrses.Save;
    Check(posDeleted = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posDeleted');
  finally
    lEAdrses.Free;
  end;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lEAdrses.Read;
    CheckEquals(0, lEAdrses.Count, 'Failed on lEAdrses.Count');
  finally
    lEAdrses.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Read;
var
  lEAdrses: TEAddressList;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestEAdrs(-1, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lEAdrses.Read;
    CheckEquals(1, lEAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
//    CheckEAdrs(lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
  finally
    lEAdrses.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Save;
var
  lEAdrses: TEAddressList;
  lEAdrs  : TEAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
    lEAdrs  := TEAdrs.Create;
    lEAdrs.ObjectState:= posCreate;
    lEAdrses.Add(lEAdrs);
    lEAdrs.OID.AsString:= IntToStr(cOIDEAdrs);
    lEAdrs.Text:= IntToStr(cOIDEAdrs);
//    lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
Assert(False, 'Under construction');
//    lEAdrses.Save;
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
  finally
    lEAdrses.Free;
  end;

  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lEAdrses.Read;
    CheckEquals(1, lEAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
//    CheckEAdrs(lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
  finally
    lEAdrses.Free;
  end;

end;

procedure TTestPerson.TestEAdrs_Update;
var
  lEAdrses: TEAddressList;
  lEAdrs: TEAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestEAdrs(-1, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lEAdrses.Read;
    CheckEquals(1, lEAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
    lEAdrs  := lEAdrses.Items[0];
//    CheckEAdrs(lEAdrs, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
    lEAdrs.Text:= cUpdateValue;
//    lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
    lEAdrs.ObjectState:= posUpdate;
Assert(False, 'Under construction');
//    lEAdrses.Save;
  finally
    lEAdrses.Free;
  end;
  lEAdrses:= TEAddressList.Create;
  try
    lEAdrses.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lEAdrses.Read;
    CheckEquals(1, lEAdrses.Count, 'Failed on lAdrses.Count');
    Check(posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean');
    Check(posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean');
//    CheckEAdrs(lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem1, cUpdateValue);
  finally
    lEAdrses.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Delete;
var
  lPeople: TPersonList;
begin
//  InsertTestPerson(-1, cOIDPerson, IntToStr(cOIDPerson));
  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lPeople.Read;
    CheckEquals(1, lPeople.Count, 'Failed on lPeople.Count');
    lPeople.Items[0].ObjectState:= posDelete;
Assert(False, 'Under construction');
//    lPeople.Save;
    Check(posDeleted = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posDeleted');
  finally
    lPeople.Free;
  end;

  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lPeople.Read;
    CheckEquals(0, lPeople.Count, 'Failed on lPeople.Count');
  finally
    lPeople.Free;
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
  Check(True);
end;

procedure TTestPerson.TestPersonFlat_Save;
var
  lPeople: TPersonList;
  lPerson: TPerson;
begin
  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
    lPerson:= TPerson.Create;
    lPerson.ObjectState:= posCreate;
//    lPerson.OID.AsString:= IntToStr(cOIDPerson);
//    lPerson.Notes:= IntToStr(cOIDPerson);
//    lPerson.Title:= IntToStr(cOIDPerson);
//    lPerson.Initials:= IntToStr(cOIDPerson);
//    lPerson.FirstName:= IntToStr(cOIDPerson);
//    lPerson.LastName := IntToStr(cOIDPerson);
//    lPeople.Add(lPerson);
Assert(False, 'Under construction');
//    lPeople.Save;
    Check(posClean = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
  finally
    lPeople.Free;
  end;
  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lPeople.Read;
    CheckEquals(1, lPeople.Count, 'Failed on lPeople.Count');
    Check(posClean = lPeople.ObjectState, 'Failed on lPeople.ObjectState = posClean');
    Check(posPK = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
//    CheckPerson(lPeople.Items[0], cOIDPerson, IntToStr(cOIDPerson));
  finally
    lPeople.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Update;
var
  lPeople: TPersonList;
  lPerson: TPerson;
begin
//  InsertTestPerson(-1, cOIDPerson, IntToStr(cOIDPerson));
  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lPeople.Read;
    CheckEquals(1, lPeople.Count, 'Failed on lPeople.Count');
    lPerson:= lPeople.Items[0];
    lPerson.Notes:= cUpdateValue;
    lPerson.Title:= cUpdateValue;
    lPerson.Initials:= cUpdateValue;
    lPerson.FirstName:= cUpdateValue;
    lPerson.LastName := cUpdateValue;
    lPerson.ObjectState:= posUpdate;
Assert(False, 'Under construction');
//    lPeople.Save;
    Check(posClean = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
  finally
    lPeople.Free;
  end;

  lPeople:= TPersonList.Create;
  try
    lPeople.OID.AsString:= '-1';
Assert(False, 'Under construction');
//    lPeople.Read;
    CheckEquals(1, lPeople.Count, 'Failed on lPeople.Count');
    Check(posClean = lPeople.ObjectState, 'Failed on lPeople.ObjectState = posClean');
    Check(posPK = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean');
//    CheckPerson(lPeople.Items[0], cOIDPerson, cUpdateValue);
  finally
    lPeople.Free;
  end;
end;

procedure TTestPerson.InsertTestAdrs(
  pOwnerOID, pOID, pAdrsTypeOID: Integer;
  const pValue: string);
var
  lParams: TtiQueryParams;
begin
  lParams:= TtiQueryParams.Create;
  try
Assert(False, 'Under construction');
//    lParams.ParamAsVariant['OID']        := pOID;
//    lParams.ParamAsVariant['Owner_OID']  := pOwnerOID;
//    lParams.ParamAsVariant['Adrs_Type']  := pAdrsTypeOID;
//    lParams.ParamAsVariant['Country']    := pValue;
//    lParams.ParamAsVariant['Lines']      := pValue;
//    lParams.ParamAsVariant['Suburb']     := pValue;
//    lParams.ParamAsVariant['State']      := pValue;
//    lParams.ParamAsVariant['PCode']      := pValue;
    GTIOPFManager.InsertRow('Adrs', lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestPerson.CheckAdrs(pData: TAdrs; pOID, pAdrsTypeOID: integer; const pValue: string);
begin
  CheckEquals(IntToStr(pOID), pData.OID.AsString, 'Failed on OID');
  CheckEquals(IntToStr(pAdrsTypeOID), pData.AdrsTypeOID, 'Failed on AdrsTypeOID');
  CheckEquals(pValue, pData.Lines, 'Failed on Lines');
  CheckEquals(pValue, pData.Suburb, 'Failed on Suburb');
  CheckEquals(pValue, pData.State, 'Failed on State');
  CheckEquals(pValue, pData.PCode, 'Failed on PostCode');
  CheckEquals(pValue, pData.Country, 'Failed on Country');
end;

procedure TTestPerson.InsertTestEAdrs(
  pOwnerOID, pOID, pAdrsTypeOID: integer;
  const pValue: string);
var
  lParams: TtiQueryParams;
begin
  lParams:= TtiQueryParams.Create;
  try
Assert(False, 'Under construction');
//    lParams.ParamAsVariant['OID']        := pOID;
//    lParams.ParamAsVariant['Owner_OID']  := pOwnerOID;
//    lParams.ParamAsVariant['EAdrs_Type'] := pAdrsTypeOID;
//    lParams.ParamAsVariant['EAdrs_Text'] := pValue;
    GTIOPFManager.InsertRow('EAdrs', lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestPerson.CheckEAdrs(
  pData: TEAdrs; pOID, pAdrsTypeOID: integer;
  const pValue: string);
begin
  CheckEquals(IntToStr(pOID), pData.OID.AsString, 'Failed on OID');
  CheckEquals(IntToStr(pAdrsTypeOID), pData.AdrsTypeOID, 'Failed on AdrsTypeOID');
  CheckEquals(pValue, pData.Text, 'Failed on Text');
end;

procedure TTestPerson.TestCompanyCompound_Delete;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
//  InsertTestAdrs(  cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));
//  InsertTestPerson(cOIDCompany, cOIDPerson, IntToStr(cOIDPerson));

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  gAdrsBook.Companies.Items[0].ObjectState:= posDelete;
Assert(False, 'Under construction');
//  gAdrsBook.Save;
  Check(posDeleted = gAdrsBook.Companies.Items[0].ObjectState, 'gAdrsBook.Companies.Items[0].ObjectState');

  FreeAndNilAdrsBook;
  gAdrsBook.Read;
  CheckEquals(0, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');

end;

procedure TTestPerson.TestCompanyCompound_Read;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
//  InsertTestAdrs(  cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));
//  InsertTestPerson(cOIDCompany, cOIDPerson, IntToStr(cOIDPerson));

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckCompany(gAdrsBook.Companies.Items[0], cOIDCompany, IntToStr(cOIDCompany));
//  CheckAdrs(gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
//  CheckEAdrs(gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr(cOIDEAdrs));
//  CheckPerson(gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, IntToStr(cOIDPerson));
end;

procedure TTestPerson.TestCompanyCompound_Save;
var
  lCompany: TCompany;
  lAdrs: TAdrs;
  lEAdrs: TEAdrs;
  lPerson: TPerson;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));

  gAdrsBook.Read;

  lCompany:= TCompany.Create;
  lCompany.OID.AsString:= IntToStr(cOIDCompany);
  lCompany.ObjectState:= posCreate;
  lCompany.CompanyName:= IntToStr(cOIDCompany);
  lCompany.Notes      := IntToStr(cOIDCompany);
  gAdrsBook.Companies.Add(lCompany);

  lAdrs   := TAdrs.Create;
  lAdrs.OID.AsString:= IntToStr(cOIDAdrs);
  lAdrs.ObjectState:= posCreate;
  lAdrs.Lines  := IntToStr(cOIDAdrs);
  lAdrs.Suburb := IntToStr(cOIDAdrs);
  lAdrs.State  := IntToStr(cOIDAdrs);
  lAdrs.PCode  := IntToStr(cOIDAdrs);
  lAdrs.Country:= IntToStr(cOIDAdrs);
//  lAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
  lCompany.AddressList.Add(lAdrs);

  lEAdrs  := TEAdrs.Create;
  lEAdrs.OID.AsString:= IntToStr(cOIDEAdrs);
  lEAdrs.ObjectState:= posCreate;
  lEAdrs.Text:= IntToStr(cOIDEAdrs);
//  lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
  lCompany.EAddressList.Add(lEAdrs);

  lPerson := TPerson.Create;
//  lPerson.OID.AsString:= IntToStr(cOIDPerson);
//  lPerson.ObjectState:= posCreate;
//  lPerson.Title      := IntToStr(cOIDPerson);
//  lPerson.Initials   := IntToStr(cOIDPerson);
//  lPerson.LastName   := IntToStr(cOIDPerson);
//  lPerson.FirstName  := IntToStr(cOIDPerson);
//  lPerson.Notes      := IntToStr(cOIDPerson);
  lCompany.People.Add(lPerson);
Assert(False, 'Under construction');
//  gAdrsBook.Save;
  FreeAndNilAdrsBook;

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckCompany(gAdrsBook.Companies.Items[0], cOIDCompany, IntToStr(cOIDCompany));
//  CheckAdrs(gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
//  CheckEAdrs(gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr(cOIDEAdrs));
//  CheckPerson(gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, IntToStr(cOIDPerson));

end;

procedure TTestPerson.TestCompanyCompound_Update;
var
  lCompany: TCompany;
  lAdrs: TAdrs;
  lEAdrs: TEAdrs;
  lPerson: TPerson;
begin
  gAdrsBook;
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
//  InsertTestCompany(cOIDCompany, IntToStr(cOIDCompany));
//  InsertTestAdrs(  cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr(cOIDAdrs));
//  InsertTestEAdrs( cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr(cOIDEAdrs));
//  InsertTestPerson(cOIDCompany, cOIDPerson, IntToStr(cOIDPerson));

  gAdrsBook.Read;
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count');
  CheckEquals(1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count');

  lCompany:= gAdrsBook.Companies.Items[0];
  lCompany.ObjectState:= posUpdate;
  lCompany.CompanyName:= cUpdateValue;
  lCompany.Notes      := cUpdateValue;

  lAdrs   := lCompany.AddressList.Items[0];
  lAdrs.ObjectState:= posUpdate;
  lAdrs.Lines  := cUpdateValue;
  lAdrs.Suburb := cUpdateValue;
  lAdrs.State  := cUpdateValue;
  lAdrs.PCode  := cUpdateValue;
  lAdrs.Country:= cUpdateValue;

  lEAdrs  := lCompany.EAddressList.Items[0];
  lEAdrs.ObjectState:= posUpdate;
  lEAdrs.Text:= cUpdateValue;

  lPerson := lCompany.People.Items[0];
  lPerson.ObjectState:= posUpdate;
  lPerson.Title:= cUpdateValue;
  lPerson.Initials:= cUpdateValue;
  lPerson.LastName:= cUpdateValue;
  lPerson.FirstName:= cUpdateValue;
  lPerson.Notes:= cUpdateValue;
Assert(False, 'Under construction');
//  gAdrsBook.Save;

  CheckCompany(gAdrsBook.Companies.Items[0], cOIDCompany, cUpdateValue);
//  CheckAdrs(gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, cUpdateValue);
//  CheckEAdrs(gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, cUpdateValue);
//  CheckPerson(gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, cUpdateValue);

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
  lEADrs.OID.AsString:= IntToStr(cOIDEAdrs);
  lEAdrs.ObjectState:= posCreate;
//  lEAdrs.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
  lEAdrs.Text:= IntToStr(cOIDEAdrs);
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

procedure TTestPerson.InsertTestCompany(pOID: Integer;
  const pValue: string);
var
  lParams: TtiQueryParams;
begin
  lParams:= TtiQueryParams.Create;
  try
Assert(False, 'Under construction');
//    lParams.ParamAsVariant['OID']:= pOID;
//    lParams.ParamAsVariant['Notes']:= pValue;
//    lParams.ParamAsVariant['Company_Name']:= pValue;
    GTIOPFManager.InsertRow('Company', lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestPerson.CheckCompany(pData: TCompany; pOID: Integer;
  const pValue: string);
begin
  CheckEquals(IntToStr(pOID), pData.OID.AsString, 'Failed on OID');
  CheckEquals(pValue, pData.Notes, 'Failed on Notes');
  CheckEquals(pValue, pData.CompanyName, 'Failed on CompanyName');
end;

procedure TTestPerson.TestAdrs_Assign;
var
  lAdrsFrom: TAdrs;
  lAdrsTo  : TAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrsFrom:= TAdrs.Create;
  try
    lAdrsFrom.OID.AsString:= IntToStr(cOIDAdrs);
    lAdrsFrom.ObjectState:= posClean;
    lAdrsFrom.Lines  := IntToStr(cOIDAdrs);
    lAdrsFrom.Suburb := IntToStr(cOIDAdrs);
    lAdrsFrom.State  := IntToStr(cOIDAdrs);
    lAdrsFrom.PCode  := IntToStr(cOIDAdrs);
    lAdrsFrom.Country:= IntToStr(cOIDAdrs);
//    lAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
    lAdrsTo:= TAdrs.Create;
    try
      lAdrsTo.Assign(lAdrsFrom);
//      CheckAdrs(lAdrsTo, cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
    finally
      lAdrsTo.Free;
    end;
  finally
    lAdrsFrom.Free;
  end;

end;

procedure TTestPerson.TestAdrs_Clone;
var
  lAdrsFrom: TAdrs;
  lAdrsTo  : TAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lAdrsFrom:= TAdrs.Create;
  try
    lAdrsFrom.OID.AsString:= IntToStr(cOIDAdrs);
    lAdrsFrom.ObjectState:= posClean;
    lAdrsFrom.Lines  := IntToStr(cOIDAdrs);
    lAdrsFrom.Suburb := IntToStr(cOIDAdrs);
    lAdrsFrom.State  := IntToStr(cOIDAdrs);
    lAdrsFrom.PCode  := IntToStr(cOIDAdrs);
    lAdrsFrom.Country:= IntToStr(cOIDAdrs);
//    lAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
    lAdrsTo:= lAdrsFrom.Clone;
    try
//      CheckAdrs(lAdrsTo, cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
    finally
      lAdrsTo.Free;
    end;
  finally
    lAdrsFrom.Free;
  end;
end;

procedure TTestPerson.TestCompanyCompound_Assign;
var
  lCompanyFrom: TCompany;
  lCompanyTo  : TCompany;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  gAdrsBook.Read;

  lCompanyFrom:= CreateCompoundCompany;
  try
    CheckCompoundCompany(lCompanyFrom);
    lCompanyTo:= TCompany.Create;
    try
      lCompanyTo.Assign(lCompanyFrom);
      CheckCompoundCompany(lCompanyTo);
    finally
      lCompanyTo.Free;
    end;
  finally
    lCompanyFrom.Free;
  end;
end;

procedure TTestPerson.TestCompanyCompound_Clone;
var
  lCompanyFrom: TCompany;
  lCompanyTo  : TCompany;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem1, IntToStr(cOIDLookupListItem));
  gAdrsBook.Read;

  lCompanyFrom:= CreateCompoundCompany;
  try
    CheckCompoundCompany(lCompanyFrom);
    lCompanyTo:= lCompanyFrom.Clone;
    try
      CheckCompoundCompany(lCompanyTo);
    finally
      lCompanyTo.Free;
    end;
  finally
    lCompanyFrom.Free;
  end;
end;

procedure TTestPerson.TestCompanyFlat_Assign;
var
  lCompanyFrom: TCompany;
  lCompanyTo  : TCompany;
begin
  lCompanyFrom:= TCompany.Create;
  try
    lCompanyFrom.ObjectState:= posCreate;
    lCompanyFrom.OID.AsString:= IntToStr(cOIDCompany);
    lCompanyFrom.Notes:= IntToStr(cOIDCompany);
    lCompanyFrom.CompanyName:= IntToStr(cOIDCompany);
    CheckCompany(lCompanyFrom, cOIDCompany, IntToStr(cOIDCompany));
    lCompanyTo:= TCompany.Create;
    try
      lCompanyTo.Assign(lCompanyFrom);
      CheckCompany(lCompanyTo, cOIDCompany, IntToStr(cOIDCompany));
    finally
      lCompanyTo.Free;
    end;
  finally
    lCompanyFrom.Free;
  end;
end;

procedure TTestPerson.TestCompanyFlat_Clone;
var
  lCompanyFrom: TCompany;
  lCompanyTo  : TCompany;
begin
  lCompanyFrom:= TCompany.Create;
  try
    lCompanyFrom.ObjectState:= posCreate;
    lCompanyFrom.OID.AsString:= IntToStr(cOIDCompany);
    lCompanyFrom.Notes:= IntToStr(cOIDCompany);
    lCompanyFrom.CompanyName:= IntToStr(cOIDCompany);
    CheckCompany(lCompanyFrom, cOIDCompany, IntToStr(cOIDCompany));
    lCompanyTo:= lCompanyFrom.Clone;
    try
      CheckCompany(lCompanyTo, cOIDCompany, IntToStr(cOIDCompany));
    finally
      lCompanyTo.Free;
    end;
  finally
    lCompanyFrom.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Assign;
var
  lEAdrsFrom: TEAdrs;
  lEAdrsTo  : TEAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrsFrom:= TEAdrs.Create;
  try
    lEAdrsFrom.OID.AsString:= IntToStr(cOIDEAdrs);
    lEAdrsFrom.ObjectState:= posClean;
    lEAdrsFrom.Text  := IntToStr(cOIDEAdrs);
//    lEAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
    lEAdrsTo:= TEAdrs.Create;
    try
      lEAdrsTo.Assign(lEAdrsFrom);
//      CheckEAdrs(lEAdrsTo, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
    finally
      lEAdrsTo.Free;
    end;
  finally
    lEAdrsFrom.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_Clone;
var
  lEAdrsFrom: TEAdrs;
  lEAdrsTo  : TEAdrs;
begin
//  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
  gAdrsBook.EAdrsTypeList.Read;
  lEAdrsFrom:= TEAdrs.Create;
  try
    lEAdrsFrom.OID.AsString:= IntToStr(cOIDEAdrs);
    lEAdrsFrom.ObjectState:= posClean;
    lEAdrsFrom.Text  := IntToStr(cOIDEAdrs);
//    lEAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
    lEAdrsTo:= lEAdrsFrom.Clone;
    try
//      CheckEAdrs(lEAdrsTo, cOIDEAdrs, cOIDLookupListItem, IntToStr(cOIDEAdrs));
    finally
      lEAdrsTo.Free;
    end;
  finally
    lEAdrsFrom.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Assign;
var
  lPersonFrom: TPerson;
  lPersonTo  : TPerson;
begin
  lPersonFrom:= TPerson.Create;
  try
    lPersonFrom.ObjectState:= posCreate;
//    lPersonFrom.OID.AsString:= IntToStr(cOIDPerson);
//    lPersonFrom.Notes:= IntToStr(cOIDPerson);
//    lPersonFrom.Title:= IntToStr(cOIDPerson);
//    lPersonFrom.Initials:= IntToStr(cOIDPerson);
//    lPersonFrom.FirstName:= IntToStr(cOIDPerson);
//    lPersonFrom.LastName := IntToStr(cOIDPerson);
    //CheckPerson(lPersonFrom, cOIDPerson, IntToStr(cOIDPerson));
    lPersonTo:= TPerson.Create;
    try
      lPersonTo.Assign(lPersonFrom);
      //CheckPerson(lPersonTo, cOIDPerson, IntToStr(cOIDPerson));
    finally
      lPersonTo.Free;
    end;
  finally
    lPersonFrom.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_Clone;
var
  lPersonFrom: TPerson;
  lPersonTo  : TPerson;
begin
  lPersonFrom:= TPerson.Create;
  try
    lPersonFrom.ObjectState:= posCreate;
//    lPersonFrom.OID.AsString:= IntToStr(cOIDPerson);
//    lPersonFrom.Notes:= IntToStr(cOIDPerson);
//    lPersonFrom.Title:= IntToStr(cOIDPerson);
//    lPersonFrom.Initials:= IntToStr(cOIDPerson);
//    lPersonFrom.FirstName:= IntToStr(cOIDPerson);
//    lPersonFrom.LastName := IntToStr(cOIDPerson);
    //CheckPerson(lPersonFrom, cOIDPerson, IntToStr(cOIDPerson));
    lPersonTo:= lPersonFrom.Clone;
    try
      //CheckPerson(lPersonTo, cOIDPerson, IntToStr(cOIDPerson));
    finally
      lPersonTo.Free;
    end;
  finally
    lPersonFrom.Free;
  end;
end;

procedure TTestPerson.CheckCompoundCompany(pCompany: TCompany);
begin
  CheckEquals(1, pCompany.AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count');
  CheckEquals(1, pCompany.EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count');
  CheckCompany(pCompany, cOIDCompany, IntToStr(cOIDCompany));
//  CheckAdrs(pCompany.AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr(cOIDAdrs));
//  CheckEAdrs(pCompany.EAddressList.Items[0], cOIDEAdr0s, cOIDLookupListItem1, IntToStr(cOIDEAdrs));
  //CheckPerson(pCompany.People.Items[0], cOIDPerson, IntToStr(cOIDPerson));
end;

function TTestPerson.CreateCompoundCompany: TCompany;
var
  lAdrsFrom: TAdrs;
  lEAdrsFrom: TEAdrs;
  lPersonFrom: TPerson;
begin
  result:= TCompany.Create;
  result.OID.AsString:= IntToStr(cOIDCompany);
  result.ObjectState:= posCreate;
  result.CompanyName:= IntToStr(cOIDCompany);
  result.Notes      := IntToStr(cOIDCompany);

  lAdrsFrom   := TAdrs.Create;
  lAdrsFrom.OID.AsString:= IntToStr(cOIDAdrs);
  lAdrsFrom.ObjectState:= posCreate;
  lAdrsFrom.Lines  := IntToStr(cOIDAdrs);
  lAdrsFrom.Suburb := IntToStr(cOIDAdrs);
  lAdrsFrom.State  := IntToStr(cOIDAdrs);
  lAdrsFrom.PCode  := IntToStr(cOIDAdrs);
  lAdrsFrom.Country:= IntToStr(cOIDAdrs);
//  lAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem);
  result.AddressList.Add(lAdrsFrom);

  lEAdrsFrom  := TEAdrs.Create;
  lEAdrsFrom.OID.AsString:= IntToStr(cOIDEAdrs);
  lEAdrsFrom.ObjectState:= posCreate;
  lEAdrsFrom.Text:= IntToStr(cOIDEAdrs);
//  lEAdrsFrom.AdrsTypeOID:= IntToStr(cOIDLookupListItem1);
  result.EAddressList.Add(lEAdrsFrom);

  lPersonFrom := TPerson.Create;
//  lPersonFrom.OID.AsString:= IntToStr(cOIDPerson);
//  lPersonFrom.ObjectState:= posCreate;
//  lPersonFrom.Title      := IntToStr(cOIDPerson);
//  lPersonFrom.Initials   := IntToStr(cOIDPerson);
//  lPersonFrom.LastName   := IntToStr(cOIDPerson);
//  lPersonFrom.FirstName  := IntToStr(cOIDPerson);
//  lPersonFrom.Notes      := IntToStr(cOIDPerson);
  result.People.Add(lPersonFrom);
end;

end.
