unit tiOIDGUID;
                 
{$I tiDefines.inc}

{$IFNDEF OID_AS_INT64}
interface
uses
  tiOID
  ,tiBaseObject
 ;


type

  TOIDGUID = class(TOID)
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
    procedure Assign(const ASource : TOID); override;
    function  Compare(const ACompareWith : TOID): integer; override;
    procedure SetToNull; override;
    function  NullOIDAsString : string; override;
    procedure GetNextValue(const ADatabaseName : string; const APersistenceLayerName : string); override;
  end;


  TNextOIDGeneratorGUID = class(TNextOIDGenerator)
  private
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   AssignNextOID(const AAssignTo : TOID; const ADatabaseName : string; APersistenceLayerName : string); override;
  end;


const
  cOIDClassNameGUID = 'OIDClassNameGUID';


implementation
uses
  tiQuery
  ,tiOPFManager
  ,tiConstants
  ,tiUtils
 ;


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


procedure TOIDGUID.Assign(const ASource: TOID);
begin
  FAsString := ASource.AsString;
end;


function TOIDGUID.Compare(const ACompareWith: TOID): integer;
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


{ TNextOIDGeneratorGUID }

procedure TNextOIDGeneratorGUID.AssignNextOID(const AAssignTo: TOID; const ADatabaseName: string; APersistenceLayerName: string);
var
  lValue : string;
const
  cGUIDLength = 38;
begin
  Assert(ADatabaseName = ADatabaseName);  // Getting rid of compiler hints, param not used.
  Assert(APersistenceLayerName = APersistenceLayerName);  // Getting rid of compiler hints, param not used.

  Assert(AAssignTo.TestValid(TOID), CTIErrorInvalidObject);
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


constructor TNextOIDGeneratorGUID.Create;
begin
  inherited;
end;


destructor TNextOIDGeneratorGUID.destroy;
begin
  inherited;
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


procedure TOIDGUID.GetNextValue(const ADatabaseName, APersistenceLayerName: string);
var
  lNextOIDGenerator : TNextOIDGeneratorGUID;
begin
  lNextOIDGenerator := TNextOIDGeneratorGUID.Create;
  try
    lNextOIDGenerator.AssignNextOID(Self, ADatabaseName, APersistenceLayerName);
  finally
    lNextOIDGenerator.Free;
  end;
end;


procedure TOIDGUID.AssignToTIQueryParam(const AFieldName: string;const AParams: TtiBaseObject);
var
  lParams : TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsString(AFieldName, FAsString);
end;


initialization

  gTIOPFManager.OIDFactory.RegisterMapping(cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID) ;
  if gTIOPFManager.DefaultOIDClassName = '' then
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameGUID;

{$ELSE}
interface
implementation
{$ENDIF}

end.
