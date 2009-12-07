unit tiOID;

{$I tiDefines.inc}

{$IFDEF OID_AS_INT64}
  {$I tiOIDAsInt64.pas}
{$ELSE}

interface

uses
  Classes,
  tiBaseObject;

const
  cDefaultOIDFieldName = 'OID';

type

  TtiOID               = class;
  TtiOIDGenerator      = class;
  TtiOIDGeneratorClass = class of TtiOIDGenerator;

  // The abstract OID class
  TtiOID = class(TtiBaseObject)
  protected
    function GetAsString: String; virtual; abstract;
    procedure SetAsString(const AValue: String); virtual; abstract;
    function GetAsVariant: variant; virtual; abstract;
    procedure SetAsVariant(const AValue: variant); virtual; abstract;
  public
    constructor Create; virtual;

    // Test these ones under NonPersistent group
    property AsString: String read GetAsString write SetAsString;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    function IsNull: boolean; virtual; abstract;
    procedure AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject); virtual; abstract;
    procedure AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject); overload; virtual; abstract;
    procedure AssignToTIQuery(const AQuery: TtiBaseObject); overload;
    procedure AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
      overload; virtual; abstract;
    procedure AssignFromTIQuery(const AQuery: TtiBaseObject); overload;

    function EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean; virtual; abstract;
    procedure Assign(const ASource: TtiOID); reintroduce; virtual;
    function Compare(const ACompareWith: TtiOID): integer; virtual; abstract;
    function Equals(const ACompareWith: TtiOID): boolean;
    procedure SetToNull; virtual; abstract;
    function Clone: TtiOID;
    function NullOIDAsString: string; virtual; abstract;

  end;

  TtiOIDClass = class of TtiOID;

  TOIDStringAbs = class(TtiOID)
  private
    FAsString: string;
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
    function GetAsVariant: variant; override;
    procedure SetAsVariant(const AValue: variant); override;
  public
    function IsNull: boolean; override;
    procedure AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject); override;
    procedure AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject); override;
    procedure AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject); override;
    function EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean; override;
    procedure Assign(const ASource: TtiOID); override;
    function Compare(const ACompareWith: TtiOID): integer; override;
    procedure SetToNull; override;
    function NullOIDAsString: string; override;
  end;

  // Each TtiOID must have an associated TtiOIDGenerator that is responsible
  // for returning the next OID for that generation stratergy
  TtiOIDGenerator = class(TtiBaseObject)
  public
    constructor Create; virtual;
    class function OIDClass: TtiOIDClass; virtual; abstract;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); virtual; abstract;
  end;

 // These are necessary to support OID_AS_INT64 and will be moved to TtiOID
 // when OID_AS_INT64 has been removed
function OIDToString(const AOID: TtiOID): string;
function OIDEquals(const AOID1, AOID2: TtiOID): boolean;
function OIDCompare(const AOID1, AOID2: TtiOID): integer;

implementation

uses
  SysUtils,
  tiQuery;

function OIDToString(const AOID: TtiOID): string;
begin
  Result := AOID.AsString;
end;

function OIDEquals(const AOID1, AOID2: TtiOID): boolean;
begin
  Result := AOID1.Equals(AOID2);
end;

function OIDCompare(const AOID1, AOID2: TtiOID): integer;
begin
  Result := AOID1.Compare(AOID2);
end;

procedure TtiOID.AssignFromTIQuery(const AQuery: TtiBaseObject);
begin
  AssignFromTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TtiOID.AssignToTIQuery(const AQuery: TtiBaseObject);
begin
  AssignToTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TtiOID.Assign(const ASource: TtiOID);
begin
  Assert(False, ClassName + '.Assign not implemented');
end;

function TtiOID.Clone: TtiOID;
begin
  Result := TtiOID(TtiOIDClass(ClassType).Create);
  Result.Assign(self);
end;

constructor TtiOID.Create;
begin
  inherited;
  SetToNull; // Just to be sure that ALWAYS it will be setted to NULL value...
end;

function TtiOID.Equals(const ACompareWith: TtiOID): boolean;
begin
  Result := Compare(ACompareWith) = 0;
end;

{ TtiOIDGenerator }

constructor TtiOIDGenerator.Create;
begin
  inherited;
end;

function TOIDStringAbs.getAsString: String;
begin
  Result := FAsString;
end;


procedure TOIDStringAbs.SetAsString(const AValue: String);
begin
  FAsString := AValue;
end;


function TOIDStringAbs.IsNull: boolean;
begin
  Result := (FAsString = NullOIDAsString);
end;


procedure TOIDStringAbs.AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery    := TtiQuery(AQuery);
  FAsString := lQuery.FieldAsString[AFieldName];
end;


procedure TOIDStringAbs.AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  if IsNull then
    lQuery.ParamIsNull[AFieldName] := True
  else
    lQuery.ParamAsString[AFieldName] := FAsString;
end;


function TOIDStringAbs.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  Result := (FAsString = lQuery.FieldAsString[AFieldName]);
end;


procedure TOIDStringAbs.Assign(const ASource: TtiOID);
begin
  FAsString := ASource.AsString;
end;


function TOIDStringAbs.Compare(const ACompareWith: TtiOID): integer;
begin
  if AsString < ACompareWith.AsString then
    Result := -1
  else if AsString > ACompareWith.AsString then
    Result := 1
  else
    Result := 0;
end;


procedure TOIDStringAbs.SetToNull;
begin
  FAsString := NullOIDAsString;
end;


function TOIDStringAbs.GetAsVariant: variant;
begin
  Result := FAsString;
end;


procedure TOIDStringAbs.SetAsVariant(const AValue: variant);
begin
  FAsString := AValue;
end;

function TOIDStringAbs.NullOIDAsString: string;
begin
  Result := '';
end;


procedure TOIDStringAbs.AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject);
var
  lParams: TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQueryParams  ');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsString(AFieldName, FAsString);
end;

{$ENDIF}

end.

