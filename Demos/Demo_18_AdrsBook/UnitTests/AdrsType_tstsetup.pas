unit AdrsType_TSTSetup;

interface
uses
  tiTestSetup,
  AdrsType_BOM;

type

  TAdrsTypeTestSetup = class(TtiTestSetup)
  public
    function  AdrsTypeCreate(const AOID: string): TAdrsType;
    procedure AdrsTypeAssign(const AData: TAdrsType; const AOID: string);
    procedure AdrsTypeInsert(const AOID: string);
    procedure AdrsTypeCheck(const AData: TAdrsType; const AOID: string);

    function  EAdrsTypeCreate(const AOID: string): TEAdrsType;
    procedure EAdrsTypeAssign(const AData: TEAdrsType; const AOID: string);
    procedure EAdrsTypeInsert(const AOID: string);
    procedure EAdrsTypeCheck(const AData: TEAdrsType; const AOID: string);
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

function TAdrsTypeTestSetup.EAdrsTypeCreate(const AOID: string): TEAdrsType;
begin
  result:= TEAdrsType.Create;
  result.OID.AsString:= AOID;
  EAdrsTypeAssign(result, AOID);
end;

procedure TAdrsTypeTestSetup.EAdrsTypeInsert(const AOID: String);
var
  LParams: TtiQueryParams;
  LData: TEAdrsType;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= EAdrsTypeCreate(AOID);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid', LData.OID.AsString);
    LParams.SetValueAsString('text', LData.Text);
    GTIOPFManager.InsertRow('eadrs_type', LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TAdrsTypeTestSetup.AdrsTypeAssign(const AData: TAdrsType;
  const AOID: string);
begin
  AData.Text:= tvToStr(AOID, 1);
end;

procedure TAdrsTypeTestSetup.AdrsTypeCheck(const AData: TAdrsType;
  const AOID: string);
var
  LExpected: TAdrsType;
begin
  LExpected:= AdrsTypeCreate(AOID);
  try
    TC.CheckEquals(LExpected.Text, AData.Text, 'Text');
  finally
    LExpected.Free;
  end;
end;

function TAdrsTypeTestSetup.AdrsTypeCreate(const AOID: string): TAdrsType;
begin
  result:= TAdrsType.Create;
  result.OID.AsString:= AOID;
  AdrsTypeAssign(result, AOID);
end;

procedure TAdrsTypeTestSetup.AdrsTypeInsert(const AOID: string);
var
  LParams: TtiQueryParams;
  LData: TAdrsType;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= AdrsTypeCreate(AOID);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid', LData.OID.AsString);
    LParams.SetValueAsString('text', LData.Text);
    GTIOPFManager.InsertRow('adrs_type', LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TAdrsTypeTestSetup.EAdrsTypeAssign(const AData: TEAdrsType;
  const AOID: string);
begin
  AData.Text:= tvToStr(AOID, 1);
end;

procedure TAdrsTypeTestSetup.EAdrsTypeCheck(const AData: TEAdrsType;
  const AOID: string);
var
  LExpected: TEAdrsType;
begin
  LExpected:= EAdrsTypeCreate(AOID);
  try
    TC.CheckEquals(LExpected.Text, AData.Text, 'Text');
  finally
    LExpected.Free;
  end;
end;

end.
