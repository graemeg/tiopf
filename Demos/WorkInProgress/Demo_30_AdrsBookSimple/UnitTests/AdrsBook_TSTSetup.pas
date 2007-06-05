unit AdrsBook_TSTSetup;

interface
uses
  tiDUnitTestSetup
  ,Adrs_BOM
 ;

type

  TAdrsBookTestSetup = class(TtiTestSetup)
  public
    function    Person_Create(const AOID: String): TPerson;
    procedure   Person_Set(const AOID: String; AData: TPerson);
    procedure   Person_Check(const AOID: String; AData: TPerson);
    procedure   Person_Insert(const AOID: String);

    function    EAdrs_Create(const AOID: String): TEAdrs;
    procedure   EAdrs_Set(const AOID: String; AData: TEAdrs);
    procedure   EAdrs_Check(const AOID: String; AData: TEAdrs);
    procedure   EAdrs_Insert(const AOIDPerson, AOID: String);

  end;

implementation
uses
   tiQuery
  ,tiOPFManager
  ,tiConstants
 ;

{ TAdrsBookTestSetup }

procedure TAdrsBookTestSetup.EAdrs_Check(const AOID: String; AData: TEAdrs);
var
  LData: TEAdrs;
begin
  LData:= EAdrs_Create(AOID);
  try
    TC.CheckEquals(LData.AdrsType, AData.AdrsType);
    TC.CheckEquals(LData.AdrsText, AData.AdrsText);
  finally
    LData.Free;
  end;
end;

function TAdrsBookTestSetup.EAdrs_Create(const AOID: String): TEAdrs;
begin
  result:= TEAdrs.Create;
  result.OID.AsString:= AOID;
  EAdrs_Set(AOID, result);
end;

procedure TAdrsBookTestSetup.EAdrs_Insert(const AOIDPerson, AOID: String);
var
  LData: TEAdrs;
  LParams: TtiQueryParams;
begin
  LData:= EAdrs_Create(AOID);
  try
    LParams:= TtiQueryParams.Create;
    try
      LParams.SetValueAsString('oid', LData.OID.AsString);
      LParams.SetValueAsString('oid_person', AOIDPerson);
      LParams.SetValueAsString('eadrs_type', LData.AdrsType);
      LParams.SetValueAsString('eadrs_text', LData.AdrsText);
      gTIOPFManager.InsertRow('eadrs', LParams);
    finally
      LParams.Free;
    end;
  finally
    LData.Free;
  end;
end;

procedure TAdrsBookTestSetup.EAdrs_Set(const AOID: String; AData: TEAdrs);
begin
  AData.AdrsType:= AOID + '1';
  AData.AdrsText:= AOID + '2';
end;

procedure TAdrsBookTestSetup.Person_Check(const AOID: String; AData: TPerson);
var
  LData: TPerson;
begin
  LData:= Person_Create(AOID);
  try
    TC.CheckEquals(LData.FirstName, AData.FirstName);
    TC.CheckEquals(LData.LastName,  AData.LastName);
    TC.CheckEquals(LData.Title,     AData.Title);
    TC.CheckEquals(LData.Initials,  AData.Initials);
    TC.CheckEquals(LData.Notes,     AData.Notes);
  finally
    LData.Free;
  end;
end;

function TAdrsBookTestSetup.Person_Create(const AOID: String): TPerson;
begin
  result:= TPerson.Create;
  result.OID.AsString:= AOID;
  Person_Set(AOID, result);
end;

procedure TAdrsBookTestSetup.Person_Insert(const AOID: String);
var
  LData: TPerson;
  LParams: TtiQueryParams;
begin
  LData:= Person_Create(AOID);
  try
    LParams:= TtiQueryParams.Create;
    try
      LParams.SetValueAsString('oid', LData.OID.AsString);
      LParams.SetValueAsString('first_name', LData.FirstName);
      LParams.SetValueAsString('last_name', LData.LastName);
      LParams.SetValueAsString('title', LData.Title);
      LParams.SetValueAsString('initials', LData.Initials);
      LParams.SetValueAsString('notes', LData.Notes);
      gTIOPFManager.InsertRow('person', LParams);
    finally
      LParams.Free;
    end;
  finally
    LData.Free;
  end;
end;

procedure TAdrsBookTestSetup.Person_Set(const AOID: String; AData: TPerson);
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  AData.FirstName:= AOID + '1';
  AData.LastName := AOID + '2';
  AData.Title    := AOID + '3';
  AData.Initials := AOID + '4';
  AData.Notes    := AOID + '5';
end;

end.
