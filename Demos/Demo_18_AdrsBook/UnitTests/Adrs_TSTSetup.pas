unit Adrs_TSTSetup;

interface
uses
  tiTestSetup,
  Adrs_BOM;

type

  TAdrsTestSetup = class(TtiTestSetup)
  public
    function  AdrsCreate(const AOID, AOIDAdrsType: string): TAdrs;
    procedure AdrsAssign(const AData: TAdrs; const AOID, AOIDAdrsType: string);
    procedure AdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
    procedure AdrsCheck(const AData: TAdrs; const AOID, AOIDAdrsType: string);
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

function TAdrsTestSetup.AdrsCreate(const AOID, AOIDAdrsType: string): TAdrs;
begin
  result:= TAdrs.Create;
  result.OID.AsString:= AOID;
  AdrsAssign(result, AOID, AOIDAdrsType);
end;

procedure TAdrsTestSetup.AdrsInsert(const AOIDPerson, AOID, AOIDAdrsType: string);
var
  LParams: TtiQueryParams;
  LData: TAdrs;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= AdrsCreate(AOID, AOIDAdrsType);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid',            LData.OID.AsString);
    LParams.SetValueAsString('oid_person',     AOIDPerson);
    LParams.SetValueAsString('lines',          LData.Lines);
    LParams.SetValueAsString('suburb',         LData.Suburb);
    LParams.SetValueAsString('state',          LData.State);
    LParams.SetValueAsString('pcode',          LData.PCode);
    LParams.SetValueAsString('country',        LData.Country);
    LParams.SetValueAsString('oid_adrs_type',  AOIDAdrsType);
    GTIOPFManager.InsertRow('adrs',           LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TAdrsTestSetup.AdrsAssign(const AData: TAdrs;
  const AOID, AOIDAdrsType: string);
begin
  AData.Lines:= tvToStr(AOID,  1);
  AData.Suburb:= tvToStr(AOID, 2);
  AData.State:= tvToStr(AOID,  3);
  AData.PCode:= tvToStr(AOID,  4);
  AData.Country:= tvToStr(AOID, 5);
  AData.OIDAdrsType:= AOIDAdrsType;
end;

procedure TAdrsTestSetup.AdrsCheck(const AData: TAdrs;
  const AOID, AOIDAdrsType: string);
var
  LExpected: TAdrs;
begin
  LExpected:= AdrsCreate(AOID, AOIDAdrsType);
  try
    TC.CheckEquals(LExpected.Lines,   AData.Lines,   'Lines');
    TC.CheckEquals(LExpected.Suburb,  AData.Suburb,  'Suburb');
    TC.CheckEquals(LExpected.State,   AData.State,   'State');
    TC.CheckEquals(LExpected.PCode,   AData.PCode,   'PCode');
    TC.CheckEquals(LExpected.Country, AData.Country, 'Country');
    TC.CheckEquals(AOIDAdrsType, AData.OIDAdrsType,  'OIDAdrsType');
  finally
    LExpected.Free;
  end;
end;

end.
