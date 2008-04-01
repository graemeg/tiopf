unit EAdrs_TSTSetup;

interface
uses
  tiTestSetup,
  Adrs_BOM;

type

  TEAdrsTestSetup = class(TtiTestSetup)
  public
    function  EAdrsCreate(const AOID: string): TEAdrs;
    procedure EAdrsAssign(const AData: TEAdrs; const AOID: string);
    procedure EAdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
    procedure EAdrsCheck(const AData: TEAdrs; const AOID, AOIDEAdrsType: string);
  end;


implementation
uses
  TestFramework,
  SysUtils,
  tiObject,
  tiOPFManager,
  tiQuery,
  AdrsUnitTestConstants;

{ TTestAdrs }

function TEAdrsTestSetup.EAdrsCreate(const AOID: string): TEAdrs;
begin
  result:= TEAdrs.Create;
  result.OID.AsString:= AOID;
  EAdrsAssign(result, AOID);
end;

procedure TEAdrsTestSetup.EAdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
var
  LParams: TtiQueryParams;
  LData: TEAdrs;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= EAdrsCreate(AOID);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid',            LData.OID.AsString);
    LParams.SetValueAsString('oid_person',     AOIDPerson);
    LParams.SetValueAsString('eadrs_text',     LData.Text);
    LParams.SetValueAsString('oid_adrs_type',  AOIDAdrsType);
    GTIOPFManager.InsertRow('eadrs',           LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TEAdrsTestSetup.EAdrsAssign(const AData: TEAdrs;
  const AOID: string);
begin
//  AData.LastName:= tvToStr(AOID, 1);
//  AData.FirstName:= tvToStr(AOID, 2);
//  AData.Title:= tvToStr(AOID, 3);
//  AData.Initials:= tvToStr(AOID, 4);
//  AData.Notes:= tvToStr(AOID, 5);
end;

procedure TEAdrsTestSetup.EAdrsCheck(const AData: TEAdrs;
  const AOID, AOIDEAdrsType: string);
var
  LExpected: TEAdrs;
begin
  LExpected:= EAdrsCreate(AOID);
  try
    TC.CheckEquals(LExpected.Text, AData.Text, 'Text');
    TC.CheckEquals(AOIDEAdrsType, AData.OIDAdrsType, 'OIDAdrsType');
  finally
    LExpected.Free;
  end;
end;

end.
