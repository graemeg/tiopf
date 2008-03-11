unit tiOIDGUID;

{$I tiDefines.inc}

{$IFDEF OID_AS_INT64}
  tiOIDGUID.pas should not be linked when OID_AS_INT64 is used
{$ENDIF}

interface
uses
  tiOID,
  tiBaseObject;

type

  TOIDGUID = class(TtiOID)
  private
    FAsString : string;
  protected
    function  GetAsString: ShortString; override;
    procedure SetAsString(const AValue: ShortString); override;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const AValue: Variant);override;
  public
    function  IsNull : boolean; override;
    procedure AssignToTIQueryParam(const AFieldName : string; const AParams : TtiBaseObject); override;
    procedure AssignToTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    procedure AssignFromTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    function  EqualsQueryField(const AFieldName : string; const AQuery : TtiBaseObject): boolean; override;
    procedure Assign(const ASource : TtiOID); override;
    function  Compare(const ACompareWith : TtiOID): integer; override;
    procedure SetToNull; override;
    function  NullOIDAsString : string; override;
  end;


  TtiOIDGeneratorGUID = class(TtiOIDGenerator)
  public
    class function OIDClass: TtiOIDClass; override;
    procedure   AssignNextOID(
      const AAssignTo : TtiOID;
      const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

implementation
uses
  tiQuery,
  tiOPFManager,
  tiConstants,
  tiUtils;

{ TOIDGUID }

function TOIDGUID.getAsString: ShortString;
begin
  result := FAsString;
end;


procedure TOIDGUID.SetAsString(const AValue: ShortString);
begin
  FAsString := AValue;
end;


function TOIDGUID.IsNull: boolean;
begin
  result := (FAsString = NullOIDAsString);
end;


procedure TOIDGUID.AssignFromTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  FAsString := lQuery.FieldAsString[ AFieldName ];
end;


procedure TOIDGUID.AssignToTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  lQuery.ParamAsString[ AFieldName ]:= FAsString;
end;


function TOIDGUID.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  result := (FAsString = lQuery.FieldAsString[ AFieldName ]);
end;


procedure TOIDGUID.Assign(const ASource: TtiOID);
begin
  FAsString := ASource.AsString;
end;


function TOIDGUID.Compare(const ACompareWith: TtiOID): integer;
begin
  if AsString < ACompareWith.AsString then
    result := -1
  else if AsString > ACompareWith.AsString then
    result := 1
  else
    result := 0;
end;


procedure TOIDGUID.SetToNull;
begin
  FAsString := NullOIDAsString;
end;


{ TtiOIDGeneratorGUID }

procedure TtiOIDGeneratorGUID.AssignNextOID(
      const AAssignTo : TtiOID;
      const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = '');
var
  lValue : string;
const
  cGUIDLength = 38;
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  lValue := tiCreateGUIDString;

//           10        20        30
// 01234567890123456789012345678901234567
// {81A9C48C-DEF3-11D6-81C4-0002E31296EB}
// A GUID will be 38 chars long when created,
// or 36 chars long when {} are removed.

  if (lValue[1] = '{') and
     (lValue[cGUIDLength] = '}') then
    lValue := Copy(lValue, 2, cGUIDLength - 2);
  AAssignTo.AsString := lValue;
end;

function TOIDGUID.GetAsVariant: Variant;
begin
  result := FAsString;
end;


procedure TOIDGUID.SetAsVariant(const AValue: Variant);
begin
  FAsString := AValue;
end;



function TOIDGUID.NullOIDAsString: string;
begin
  result := '';
end;



procedure TOIDGUID.AssignToTIQueryParam(const AFieldName: string;const AParams: TtiBaseObject);
var
  lParams : TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsString(AFieldName, FAsString);
end;


class function TtiOIDGeneratorGUID.OIDClass: TtiOIDClass;
begin
  result:= TOIDGUID;
end;

initialization
  GTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorGUID.Create;

end.

