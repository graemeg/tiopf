unit EAdrs_TSTSetup;

interface
uses
  tiTestSetup,
  Adrs_BOM;

type

  TEAdrsTestSetup = class(TtiTestSetup)
  public
    function  EAdrsCreate(const AOID, AOIDAdrsType: string): TEAdrs;
    procedure EAdrsAssign(const AData: TEAdrs; const AOID, AOIDAdrsType: string);
    procedure EAdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
    procedure EAdrsCheck(const AData: TEAdrs; const AOID, AOIDAdrsType: string);
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

function TEAdrsTestSetup.EAdrsCreate(const AOID, AOIDAdrsType: string): TEAdrs;
begin
  result:= TEAdrs.Create;
  result.OID.AsString:= AOID;
  EAdrsAssign(result, AOID, AOIDAdrsType);
end;

procedure TEAdrsTestSetup.EAdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
var
  LParams: TtiQueryParams;
  LData: TEAdrs;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= EAdrsCreate(AOID, AOIDAdrsType);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid',            LData.OID.AsString);
    LParams.SetValueAsString('oid_person',     AOIDPerson);
    LParams.SetValueAsString('eadrs_text',     LData.Text);
    LParams.SetValueAsString('oid_eadrs_type',  AOIDAdrsType);
    GTIOPFManager.InsertRow('eadrs',           LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TEAdrsTestSetup.EAdrsAssign(const AData: TEAdrs;
  const AOID, AOIDAdrsType: string);
begin
  AData.Text:= tvToStr(AOID, 1);
  AData.OIDAdrsType:= AOIDAdrsType;
end;

procedure TEAdrsTestSetup.EAdrsCheck(const AData: TEAdrs;
  const AOID, AOIDAdrsType: string);
var
  LExpected: TEAdrs;
begin
  LExpected:= EAdrsCreate(AOID, AOIDAdrsType);
  try
    TC.CheckEquals(LExpected.Text, AData.Text, 'Text');
    TC.CheckEquals(AOIDAdrsType, AData.OIDAdrsType, 'OIDAdrsType');
  finally
    LExpected.Free;
  end;
end;

end.
