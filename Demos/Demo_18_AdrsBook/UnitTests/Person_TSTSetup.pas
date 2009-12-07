unit Person_TSTSetup;

interface
uses
  tiTestSetup,
  Adrs_BOM;

type

  TPersonTestSetup = class(TtiTestSetup)
  public
    function  PersonCreate(const AOID: string): TPerson;
    procedure PersonAssign(const AData: TPerson; const AOID: string);
    procedure PersonInsert(const AOID: string);
    procedure PersonCheck(const AData: TPerson; const AOID: string);
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

function TPersonTestSetup.PersonCreate(const AOID: string): TPerson;
begin
  result:= TPerson.Create;
  result.OID.AsString:= AOID;
  PersonAssign(result, AOID);
end;

procedure TPersonTestSetup.PersonInsert(const AOID: String);
var
  LParams: TtiQueryParams;
  LData: TPerson;
begin
  LData:= nil;
  LParams:= nil;
  try
    LData:= PersonCreate(AOID);
    LParams:= TtiQueryParams.Create;
    LParams.SetValueAsString('oid',        LData.OID.AsString);
    LParams.SetValueAsString('last_name',  LData.LastName);
    LParams.SetValueAsString('first_name', LData.FirstName);
    LParams.SetValueAsString('title',      LData.Title);
    LParams.SetValueAsString('initials',   LData.Initials);
    LParams.SetValueAsString('notes',      LData.Notes);
    GTIOPFManager.InsertRow('person',      LParams);
  finally
    LData.Free;
    LParams.Free;
  end;
end;

procedure TPersonTestSetup.PersonAssign(const AData: TPerson;
  const AOID: string);
begin
  AData.LastName:= tvToStr(AOID, 1);
  AData.FirstName:= tvToStr(AOID, 2);
  AData.Title:= tvToStr(AOID, 3);
  AData.Initials:= tvToStr(AOID, 4);
  AData.Notes:= tvToStr(AOID, 5);
end;

procedure TPersonTestSetup.PersonCheck(const AData: TPerson;
  const AOID: string);
var
  LExpected: TPerson;
begin
  LExpected:= PersonCreate(AOID);
  try
    TC.CheckEquals(LExpected.LastName, AData.LastName, 'LastName');
    TC.CheckEquals(LExpected.FirstName, AData.FirstName, 'FirstName');
    TC.CheckEquals(LExpected.Title, AData.Title, 'Title');
    TC.CheckEquals(LExpected.Initials, AData.Initials, 'Initials');
    TC.CheckEquals(LExpected.Notes, AData.Notes, 'Notes');
  finally
    LExpected.Free;
  end;
end;

end.
