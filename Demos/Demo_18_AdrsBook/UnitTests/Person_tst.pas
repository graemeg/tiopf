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
    procedure TestPersonFlat_IsValid;

    procedure TestEAdrs_Read            ;
    procedure TestEAdrs_Save            ;
    procedure TestEAdrs_Update          ;
    procedure TestEAdrs_Delete          ;
    procedure TestEAdrs_Equals          ;
    procedure TestEAdrs_Assign          ;
    procedure TestEAdrs_IsValid         ;
    procedure TestEAdrs_AdrsType;

    procedure TestAdrs_Read             ;
    procedure TestAdrs_Save             ;
    procedure TestAdrs_Update           ;
    procedure TestAdrs_Delete           ;
    procedure TestAdrs_Equals           ;
    procedure TestAdrs_Assign           ;
    procedure TestAdrs_IsValid          ;
    procedure TestAdrs_EAdrsType        ;

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

procedure TTestPerson.TestAdrs_EAdrsType;
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

procedure TTestPerson.TestAdrs_Equals;
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

procedure TTestPerson.TestAdrs_IsValid;
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

procedure TTestPerson.TestAdrs_Read;
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

procedure TTestPerson.TestAdrs_Save;
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

procedure TTestPerson.TestAdrs_Update;
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
    TestTIObjectEquals(LItem1, LItem2, 'Text');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.TestEAdrs_IsValid;
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
    TestTIObjectEquals(LItem1, LItem2, 'LastName');
    TestTIObjectEquals(LItem1, LItem2, 'FirstName');
    TestTIObjectEquals(LItem1, LItem2, 'Title');
    TestTIObjectEquals(LItem1, LItem2, 'Initials');
  finally
    LItem1.Free;
    LItem2.Free;
  end;
end;

procedure TTestPerson.TestPersonFlat_IsValid;
begin

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
end;

procedure TTestPerson.TestPersonCompound_Read;
begin
end;

procedure TTestPerson.TestPersonCompound_Save;
begin
end;

procedure TTestPerson.TestPersonCompound_Update;
begin
end;

procedure TTestPerson.TestAdrs_Assign;
var
  LFrom: TAdrs;
  LTo  : TAdrs;
begin
  LFrom:= AdrsTestSetup.AdrsCreate(COIDAdrs1, cOIDAdrsType1);
  try
    LTo:= TAdrs.Create;
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

procedure TTestPerson.TestEAdrs_AdrsType;
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

{ TEAdrsForTesting }

end.
