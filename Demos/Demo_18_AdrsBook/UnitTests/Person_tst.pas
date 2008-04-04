unit Person_tst;

interface
uses
  tiTestFramework,
  tiVisitorDB,
  Adrs_BOM,
  AdrsBase_TST,
  AdrsType_BOM;

type

  TTestPerson = class(TAdrsBaseTestCase)
  private
    function CreateCompoundPerson(const AOIDPerson, AOIDAdrs, AOIDAdrsType,
                                   AOIDEAdrs, AOIDEAdrsType: string): TPerson;

  published
    procedure PersonFlat_Read;
    procedure PersonFlat_Save;
    procedure PersonFlat_Update;
    procedure PersonFlat_Delete;
    procedure PersonFlat_Equals;
    procedure PersonFlat_Assign;
    procedure PersonFlat_IsValid;

    procedure EAdrs_Read            ;
    procedure EAdrs_Save            ;
    procedure EAdrs_Update          ;
    procedure EAdrs_Delete          ;
    procedure EAdrs_Equals          ;
    procedure EAdrs_Assign          ;
    procedure EAdrs_IsValid         ;
    procedure EAdrs_AdrsType;
    procedure EAdrs_Caption;

    procedure Adrs_Read             ;
    procedure Adrs_Save             ;
    procedure Adrs_Update           ;
    procedure Adrs_Delete           ;
    procedure Adrs_Equals           ;
    procedure Adrs_Assign           ;
    procedure Adrs_IsValid          ;
    procedure Adrs_EAdrsType        ;
    procedure Adrs_AsSingleLine;
    procedure Adrs_Caption;

    procedure PersonCompound_Read;
    procedure PersonCompound_Save;
    procedure PersonCompound_Delete;
    // procedure PersonCompound_Equals; // Design decision
    procedure PersonCompound_Assign;

    procedure AdrsBook_Parent;

  end;

  TPersonXMLLightTestCase = class(TTestPerson)
  protected
    procedure SetUpOnce; override;
  end;

  TPersonIBXTestCase = class(TTestPerson)
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

{ TTestPerson }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(cTIPersistIBX, TPersonIBXTestCase.Suite);
  TestFramework.RegisterTest(cTIPersistXMLLight, TPersonXMLLightTestCase.Suite);
end;

{ TTestPerson }

procedure TTestPerson.Adrs_Delete;
var
  LList: TPersonList;
  LPerson: TPerson;
begin
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  PersonTestSetup.PersonInsert(cOIDPerson1);
  AdrsTestSetup.AdrsInsert(COIDPerson1, COIDAdrs1, cOIDAdrsType1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    LPerson.AddressList.Items[0].Deleted:= True;
    LPerson.Save;
    CheckObjectState(posDeleted, LPerson.AddressList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    CheckEquals(0, LPerson.AddressList.Count);
  finally
    LList.Free;
  end;
end;

type
  TAdrsForTesting = class(TAdrs)
  private
    FAdrsTypeList: TAdrsTypeListAbs;
  protected
    function AdrsTypeList: TAdrsTypeListAbs; override;
    procedure SetAdrsTypeList(const AAdrsTypeList: TAdrsTypeListAbs);
  end;

  function TAdrsForTesting.AdrsTypeList: TAdrsTypeListAbs;
  begin
    result:= FAdrsTypeList;
  end;

  procedure TAdrsForTesting.SetAdrsTypeList(const AAdrsTypeList: TAdrsTypeListAbs);
  begin
    FAdrsTypeList:= AAdrsTypeList;
  end;

procedure TTestPerson.Adrs_EAdrsType;
var
  LItem: TAdrsForTesting;
  LAdrsTypeList: TAdrsTypeList;
begin
  LAdrsTypeList:= TAdrsTypeList.Create;
  try
    LAdrsTypeList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1));
    LAdrsTypeList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType2));
    LItem:= TAdrsForTesting.Create;
    try
      LItem.SetAdrsTypeList(LAdrsTypeList);

      LItem.OIDAdrsType:= COIDAdrsType1;
      CheckNotNull(LItem.AdrsType);
      CheckEquals(COIDAdrsType1, LItem.AdrsType.OID.AsString);

      LItem.OIDAdrsType:= COIDAdrsType2;
      CheckNotNull(LItem.AdrsType);
      CheckEquals(COIDAdrsType2, LItem.AdrsType.OID.AsString);

    finally
      LItem.Free;
    end;
  finally
    LAdrsTypeList.Free;
  end;
end;

procedure TTestPerson.Adrs_Equals;
var
  LItem1: TAdrs;
  LItem2: TAdrs;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= AdrsTestSetup.AdrsCreate(cOIDAdrs1, cOIDAdrsType1);
    LItem2:= AdrsTestSetup.AdrsCreate(cOIDAdrs1, cOIDAdrsType1);
    TestTIObjectEquals(LItem1, LItem2, 'Lines');
    TestTIObjectEquals(LItem1, LItem2, 'Suburb');
    TestTIObjectEquals(LItem1, LItem2, 'State');
    TestTIObjectEquals(LItem1, LItem2, 'PCode');
    TestTIObjectEquals(LItem1, LItem2, 'Country');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.Adrs_IsValid;
var
  LItem: TAdrs;
  LErrors: TtiObjectErrors;
begin
  LErrors:= nil;
  LItem:= nil;
  try
    LErrors:= TtiObjectErrors.Create;
    LItem:= AdrsTestSetup.AdrsCreate(COIDAdrs1, cOIDAdrsType1);

    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.OIDAdrsType:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(CErrorAdrsAbsAdrsTypeNotAssigned, LErrors.Items[0].ErrorMessage);
    LItem.OIDAdrsType:= COIDAdrsType1;

    LItem.Lines:= '';
    LItem.Suburb:= '';
    LItem.State:= '';
    LItem.PCode:= '';
    LItem.Country:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(CErrorAdrsAddressNotAssigned, LErrors.Items[0].ErrorMessage);

  finally
    LErrors.Free;
    LItem.Free;
  end;

end;

procedure TTestPerson.Adrs_Read;
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

procedure TTestPerson.Adrs_Save;
var
  LList: TPersonList;
  LPerson: TPerson;
  LItem: TAdrs;
begin
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType2);
  PersonTestSetup.PersonInsert(cOIDPerson1);

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    LItem:= AdrsTestSetup.AdrsCreate(cOIDAdrs1, cOIDAdrsType1);
    LItem.Dirty:= True;
    LPerson.AddressList.Add(LItem);
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
    AdrsTestSetup.AdrsCheck(LPerson.AddressList.Items[0], cOIDAdrs1, cOIDAdrsType1);
    CheckEquals(cOIDAdrs1, LPerson.AddressList.Items[0].OID);
  finally
    LList.Free;
  end;

end;

procedure TTestPerson.Adrs_Update;
var
  LList: TPersonList;
  LPerson: TPerson;
begin
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType2);
  PersonTestSetup.PersonInsert(cOIDPerson1);
  AdrsTestSetup.AdrsInsert(COIDPerson1, COIDAdrs1, cOIDAdrsType1);
  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    AdrsTestSetup.AdrsAssign(LPerson.AddressList.Items[0], COIDAdrs2, cOIDAdrsType2);
    LPerson.AddressList.Items[0].Dirty:= True;
    LPerson.Save;
    CheckObjectState(posClean, LPerson.AddressList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= TPersonList.Create;
  try
    LList.Read;
    LPerson:= LList.Items[0];
    AdrsTestSetup.AdrsCheck(LPerson.AddressList.Items[0], COIDAdrs2, cOIDAdrsType2);
    CheckEquals(COIDAdrs1, LPerson.AddressList.Items[0].OID);
  finally
    LList.Free;
  end;

end;

function TTestPerson.CreateCompoundPerson(const AOIDPerson, AOIDAdrs,
  AOIDAdrsType, AOIDEAdrs, AOIDEAdrsType: string): TPerson;
var
  LAdrs: TAdrs;
  LEAdrs: TEAdrs;
begin
  Result:= PersonTestSetup.PersonCreate(AOIDPerson);
  LAdrs:= AdrsTestSetup.AdrsCreate(AOIDAdrs, AOIDAdrsType);
  Result.AddressList.Add(LAdrs);
  LEAdrs:= EAdrsTestSetup.EAdrsCreate(AOIDEAdrs, AOIDEAdrsType);
  Result.EAddressList.Add(LEAdrs);
end;

procedure TTestPerson.EAdrs_Delete;
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

procedure TTestPerson.EAdrs_Equals;
var
  LItem1: TEAdrs;
  LItem2: TEAdrs;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, cOIDEAdrsType1);
    LItem2:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, cOIDEAdrsType1);
    TestTIObjectEquals(LItem1, LItem2, 'Text');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.EAdrs_IsValid;
var
  LItem: TEAdrs;
  LErrors: TtiObjectErrors;
begin
  LErrors:= nil;
  LItem:= nil;
  try
    LErrors:= TtiObjectErrors.Create;
    LItem:= EAdrsTestSetup.EAdrsCreate(COIDEAdrs1, COIDEAdrsType1);
    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.OIDAdrsType:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(CErrorAdrsAbsAdrsTypeNotAssigned, LErrors.Items[0].ErrorMessage);
    LItem.OIDAdrsType:= COIDEAdrsType1;

    LItem.Text:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(CErrorEAdrsTextNotAssigned, LErrors.Items[0].ErrorMessage);

  finally
    LErrors.Free;
    LItem.Free;
  end;
end;

procedure TTestPerson.EAdrs_Read;
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

procedure TTestPerson.EAdrs_Save;
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

procedure TTestPerson.EAdrs_Update;
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

procedure TTestPerson.PersonFlat_Delete;
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

procedure TTestPerson.PersonFlat_Equals;
var
  LItem1: TPerson;
  LItem2: TPerson;
begin
  LItem1:= nil;
  LItem2:= nil;
  try
    LItem1:= PersonTestSetup.PersonCreate(COIDPerson1);
    LItem2:= PersonTestSetup.PersonCreate(COIDPerson1);
    TestTIObjectEquals(LItem1, LItem2, 'LastName');
    TestTIObjectEquals(LItem1, LItem2, 'FirstName');
    TestTIObjectEquals(LItem1, LItem2, 'Title');
    TestTIObjectEquals(LItem1, LItem2, 'Initials');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.PersonFlat_IsValid;
var
  LItem: TPerson;
  LErrors: TtiObjectErrors;
begin
  LErrors:= nil;
  LItem:= nil;
  try
    LErrors:= TtiObjectErrors.Create;
    LItem:= PersonTestSetup.PersonCreate(cOIDPerson1);
    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.Title:= '';
    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.Initials:= '';
    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.FirstName:= '';
    Check(LItem.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);

    LItem.LastName:= '';
    Check(not LItem.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(CErrorPersonNameNotAssigned, LErrors.Items[0].ErrorMessage);

  finally
    LErrors.Free;
    LItem.Free;
  end;
end;

procedure TTestPerson.PersonFlat_Read;
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

procedure TTestPerson.PersonFlat_Save;
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

procedure TTestPerson.PersonFlat_Update;
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

procedure TTestPerson.PersonCompound_Assign;
var
  LAdrsBook: TAdrsBook;
  LFrom: TPerson;
  LTo: TPerson;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);

  LAdrsBook:= nil;
  try
    LAdrsBook:= TAdrsBook.Create;
    LAdrsBook.Read;
    LFrom:= CreateCompoundPerson(
      COIDPerson1, COIDAdrs1, COIDAdrsType1, COIDEAdrs1, COIDEAdrsType1);
    LAdrsBook.PersonList.Add(LFrom);
    LTo:= TPerson.Create;
    LAdrsBook.PersonList.Add(LTo);
    LTo.Assign(LFrom);
    Check(LFrom.Equals(LTo));
  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestPerson.PersonCompound_Delete;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  PersonTestSetup.PersonInsert(COIDPerson1);
  AdrsTestSetup.AdrsInsert(COIDPerson1, COIDAdrs1, COIDAdrsType1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs1, COIDEAdrsType1);

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.PersonList.Count, 'Failed on lPeople.Count');
    LPerson:= LAdrsBook.PersonList.Items[0];
    LPerson.Deleted:= True;
    CheckObjectState(posDelete, LPerson);
    CheckObjectState(posDelete, LPerson.AddressList.Items[0]);
    CheckObjectState(posDelete, LPerson.EAddressList.Items[0]);
    LPerson.Save;
    CheckObjectState(posDeleted, LPerson);
    CheckObjectState(posDeleted, LPerson.AddressList.Items[0]);
    CheckObjectState(posDeleted, LPerson.EAddressList.Items[0]);

  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestPerson.PersonCompound_Read;
var
  LAdrsBook: TAdrsBook;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);

  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType2);

  PersonTestSetup.PersonInsert(COIDPerson2);
  PersonTestSetup.PersonInsert(COIDPerson1);

  AdrsTestSetup.AdrsInsert(COIDPerson1, COIDAdrs1, COIDAdrsType1);
  AdrsTestSetup.AdrsInsert(COIDPerson2, COIDAdrs2, COIDAdrsType2);

  EAdrsTestSetup.EAdrsInsert(COIDPerson1, COIDEAdrs1, COIDEAdrsType1);
  EAdrsTestSetup.EAdrsInsert(COIDPerson2, COIDEAdrs2, COIDEAdrsType2);

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    CheckEquals(2, LAdrsBook.PersonList.Count, 'Failed on lPeople.Count');
    PersonTestSetup.PersonCheck(LAdrsBook.PersonList.Items[0], COIDPerson1);
    PersonTestSetup.PersonCheck(LAdrsBook.PersonList.Items[1], COIDPerson2);

    AdrsTestSetup.AdrsCheck(LAdrsBook.PersonList.Items[0].AddressList.Items[0], COIDAdrs1, COIDAdrsType1);
    EAdrsTestSetup.EAdrsCheck(LAdrsBook.PersonList.Items[0].EAddressList.Items[0], COIDEAdrs1, COIDEAdrsType1);

    AdrsTestSetup.AdrsCheck(LAdrsBook.PersonList.Items[1].AddressList.Items[0], COIDAdrs2, COIDAdrsType2);
    EAdrsTestSetup.EAdrsCheck(LAdrsBook.PersonList.Items[1].EAddressList.Items[0], COIDEAdrs2, COIDEAdrsType2);

  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestPerson.PersonCompound_Save;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LAdrs: TAdrs;
  LEAdrs: TEAdrs;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);

  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType1);
  AdrsTypeSetup.AdrsTypeInsert(cOIDAdrsType2);

  LAdrsBook:= TAdrsBook.Create;
  try
    LPerson:= CreateCompoundPerson(
      COIDPerson1, COIDAdrs1, COIDAdrsType1, COIDEAdrs1, COIDEAdrsType1);

    LPerson.Dirty:= True;
    LAdrsBook.PersonList.Add(LPerson);

    LAdrs:= LPerson.AddressList.Items[0];
    LAdrs.Dirty:= True;

    LEAdrs:= LPerson.EAddressList.Items[0];
    LEAdrs.Dirty:= True;
    LPerson.Save;

    CheckObjectState(posClean, LPerson);
    CheckObjectState(posClean, LAdrs);
    CheckObjectState(posClean, LEAdrs);

  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.PersonList.Count, 'Failed on lPeople.Count');
    PersonTestSetup.PersonCheck(LAdrsBook.PersonList.Items[0], COIDPerson1);

    AdrsTestSetup.AdrsCheck(LAdrsBook.PersonList.Items[0].AddressList.Items[0], COIDAdrs1, COIDAdrsType1);
    EAdrsTestSetup.EAdrsCheck(LAdrsBook.PersonList.Items[0].EAddressList.Items[0], COIDEAdrs1, COIDEAdrsType1);

  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestPerson.Adrs_AsSingleLine;
var
  LItem: TAdrs;
  LExpected: string;
begin
  LExpected:= '5011 5012 5013 5014 5015';
  LItem:= AdrsTestSetup.AdrsCreate(cOIDAdrs1, COIDAdrsType1);
  try
    CheckEquals(LItem.AsSingleLine, LExpected);
  finally
    LItem.Free;
  end;
end;

procedure TTestPerson.Adrs_Caption;
var
  LItem: TAdrs;
begin
  LItem:= AdrsTestSetup.AdrsCreate(cOIDAdrs1, COIDAdrsType1);
  try
    CheckEquals(LItem.Caption, LItem.AsSingleLine);
  finally
    LItem.Free;
  end;
end;

procedure TTestPerson.AdrsBook_Parent;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LAdrs: TAdrs;
  LEAdrs: TEAdrs;
begin
  LAdrsBook:= TAdrsBook.Create;
  try
    LPerson:= PersonTestSetup.PersonCreate(COIDPerson1);
    LAdrsBook.PersonList.Add(LPerson);

    LAdrs:= AdrsTestSetup.AdrsCreate(COIDAdrs1, COIDAdrsType1);
    LPerson.AddressList.Add(LAdrs);

    LEAdrs:= EAdrsTestSetup.EAdrsCreate(COIDEAdrs1, COIDEAdrsType1);
    LPerson.EAddressList.Add(LEAdrs);

    CheckIs(LPerson.Parent, TAdrsBook, 'Person');
    CheckIs(LAdrs.Parent, TPerson, 'Adrs');
    CheckIs(LEAdrs.Parent, TPerson, 'EAdrs');

  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestPerson.Adrs_Assign;
var
  LFrom: TAdrsForTesting;
  LTo  : TAdrsForTesting;
  LAdrsTypeList: TAdrsTypeList;
begin
  LAdrsTypeList:= TAdrsTypeList.Create;
  try
    LAdrsTypeList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType1));
    LAdrsTypeList.Add(AdrsTypeSetup.AdrsTypeCreate(COIDAdrsType2));
    LFrom:= TAdrsForTesting.Create;
    try
      LFrom.SetAdrsTypeList(LAdrsTypeList);
      LTo:= TAdrsForTesting.Create;
      try
        LTo.SetAdrsTypeList(LAdrsTypeList);
        LTo.Assign(LFrom);
        Check(LFrom.Equals(LTo));
      finally
        LTo.Free;
      end;
    finally
      LFrom.Free;
    end;
  finally
    LAdrsTypeList.Free;
  end;
end;

type
  TEAdrsForTesting = class(TEAdrs)
  private
    FAdrsTypeList: TAdrsTypeListAbs;
  protected
    function AdrsTypeList: TAdrsTypeListAbs; override;
    procedure SetAdrsTypeList(const AAdrsTypeList: TAdrsTypeListAbs);
  end;

  function TEAdrsForTesting.AdrsTypeList: TAdrsTypeListAbs;
  begin
    result:= FAdrsTypeList;
  end;

  procedure TEAdrsForTesting.SetAdrsTypeList(const AAdrsTypeList: TAdrsTypeListAbs);
  begin
    FAdrsTypeList:= AAdrsTypeList;
  end;

procedure TTestPerson.EAdrs_AdrsType;
var
  LItem: TEAdrsForTesting;
  LEAdrsTypeList: TEAdrsTypeList;
begin
  LEAdrsTypeList:= TEAdrsTypeList.Create;
  try
    LEAdrsTypeList.Add(AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1));
    LEAdrsTypeList.Add(AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType2));
    LItem:= TEAdrsForTesting.Create;
    try
      LItem.SetAdrsTypeList(LEAdrsTypeList);

      LItem.OIDAdrsType:= COIDEAdrsType1;
      CheckNotNull(LItem.AdrsType);
      CheckEquals(COIDEAdrsType1, LItem.AdrsType.OID.AsString);

      LItem.OIDAdrsType:= COIDEAdrsType2;
      CheckNotNull(LItem.AdrsType);
      CheckEquals(COIDEAdrsType2, LItem.AdrsType.OID.AsString);

    finally
      LItem.Free;
    end;
  finally
    LEAdrsTypeList.Free;
  end;
end;

procedure TTestPerson.EAdrs_Assign;
var
  LFrom: TEAdrsForTesting;
  LTo  : TEAdrsForTesting;
  LEAdrsTypeList: TEAdrsTypeList;
begin
  LEAdrsTypeList:= TEAdrsTypeList.Create;
  try
    LEAdrsTypeList.Add(AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType1));
    LEAdrsTypeList.Add(AdrsTypeSetup.EAdrsTypeCreate(COIDEAdrsType2));
    LFrom:= TEAdrsForTesting.Create;
    try
      LFrom.SetAdrsTypeList(LEAdrsTypeList);
      EAdrsTestSetup.EAdrsAssign(LFrom, COIDEAdrs1, cOIDEAdrsType1);
      LTo:= TEAdrsForTesting.Create;
      try
        LTo.SetAdrsTypeList(LEAdrsTypeList);
        LTo.Assign(LFrom);
        Check(LFrom.Equals(LTo));
      finally
        LTo.Free;
      end;
    finally
      LFrom.Free;
    end;
  finally
    LEAdrsTypeList.Free;
  end;
end;

procedure TTestPerson.EAdrs_Caption;
var
  LItem: TEAdrs;
begin
  LItem:= EAdrsTestSetup.EAdrsCreate(cOIDEAdrs1, COIDEAdrsType1);
  try
    CheckEquals(LItem.Caption, LItem.Text);
  finally
    LItem.Free;
  end;
end;

procedure TTestPerson.PersonFlat_Assign;
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

{ TEAdrsForTesting }

{ TEAdrsForTesting }

{ TPersonXMLLightTestCase }

procedure TPersonXMLLightTestCase.SetUpOnce;
begin
  PersistenceLayerName:= CTIPersistXMLLight;
  inherited;
end;

{ TPersonIBXTestCase }

procedure TPersonIBXTestCase.SetUpOnce;
begin
  PersistenceLayerName:= CTIPersistIBX;
  inherited;
end;

end.
