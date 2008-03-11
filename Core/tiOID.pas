unit tiOID;

{$I tiDefines.inc}

{$IFDEF OID_AS_INT64}
  {$I tiOIDAsInt64.pas}
{$ELSE}

interface

uses
  Classes,
  tiBaseObject,
  Contnrs;

const
  cDefaultOIDFieldName       = 'OID';

type

  TtiOID = class;
  TtiOIDGenerator = class;
  TtiOIDGeneratorClass = class of TtiOIDGenerator;

  // The abstract OID class
  TtiOID = class(TtiBaseObject)
  protected
    function  GetAsString: ShortString; virtual; abstract;
    procedure SetAsString(const AValue: ShortString); virtual; abstract;
    function  GetAsVariant: Variant;virtual; abstract;
    procedure SetAsVariant(const AValue: Variant);virtual; abstract;
  public
    constructor Create; virtual;

    // Test these ones under NonPersistent group
    property    AsString : ShortString read GetAsString write SetAsString;
    property    AsVariant : Variant read GetAsVariant write SetAsVariant;
    function    IsNull : boolean; virtual; abstract;
    procedure   AssignToTIQueryParam(const AFieldName : string; const AParams : TtiBaseObject); virtual; abstract;
    procedure   AssignToTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); overload; virtual; abstract;
    procedure   AssignToTIQuery(const AQuery : TtiBaseObject); overload;
    procedure   AssignFromTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); overload; virtual; abstract;
    procedure   AssignFromTIQuery(const AQuery : TtiBaseObject); overload;

    function    EqualsQueryField(const AFieldName : string; const AQuery : TtiBaseObject): boolean; virtual; abstract;
    procedure   Assign(const ASource : TtiOID); reintroduce; virtual;
    function    Compare(const ACompareWith : TtiOID): integer; virtual; abstract;
    function    Equals(const ACompareWith : TtiOID): boolean;
    procedure   SetToNull; virtual; abstract;
    function    Clone : TtiOID;
    function    NullOIDAsString : string; virtual; abstract;

    // This one gets tested for each DB, and each OID type
    procedure   GetNextValue; virtual;
  end;

  TtiOIDClass = class of TtiOID;

  // Each TtiOID must have an associated TtiOIDGenerator that is responsible
  // for returning the next OID for that generation stratergy
  TtiOIDGenerator = class(TtiBaseObject)
  public
    constructor Create; virtual;
    class function OIDClass: TtiOIDClass; virtual; abstract;
    procedure   AssignNextOID(
      const AAssignTo : TtiOID;
      const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); virtual; abstract;
  end;

  // These are necessary to support OID_AS_INT64 and will be moved to TtiOID
  // when OID_AS_INT64 has been removed
  function OIDToString(const AOID : TtiOID): string;
  function OIDEquals(const AOID1, AOID2 : TtiOID): boolean;
  function OIDCompare(const AOID1, AOID2 : TtiOID): Integer;

implementation
uses
  SysUtils,
  tiUtils,
  tiOPFManager,
  tiConstants,
  tiPersistenceLayers;

function OIDToString(const AOID : TtiOID): string;
begin
  result := AOID.AsString;
end;

function OIDEquals(const AOID1, AOID2 : TtiOID): boolean;
begin
  result := AOID1.Equals(AOID2);
end;

function OIDCompare(const AOID1, AOID2 : TtiOID): Integer;
begin
  result := AOID1.Compare(AOID2);
end;

procedure TtiOID.AssignFromTIQuery(const AQuery: TtiBaseObject);
begin
  AssignFromTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TtiOID.AssignToTIQuery(const AQuery: TtiBaseObject);
begin
  AssignToTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TtiOID.Assign(const ASource : TtiOID);
begin
  Assert(false, ClassName + '.Assign not implemented');
end;

function TtiOID.Clone: TtiOID;
begin
  result:=TtiOID(TtiOIDClass(ClassType).Create);
  result.Assign(self);
end;

constructor TtiOID.Create;
begin
  inherited;
  SetToNull; // Just to be sure that ALWAYS it will be setted to NULL value...
end;

function TtiOID.Equals(const ACompareWith: TtiOID): boolean;
begin
  result := Compare(ACompareWith) = 0;
end;

procedure TtiOID.GetNextValue;
begin
  GTIOPFManager.DefaultOIDGenerator.AssignNextOID(Self);
end;

{ TtiOIDGenerator }

constructor TtiOIDGenerator.Create;
begin
  inherited;
end;

{$ENDIF}

end.




