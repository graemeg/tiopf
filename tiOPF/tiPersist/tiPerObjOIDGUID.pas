unit tiPerObjOIDGUID;
                 
{$I tiDefines.inc}

{$IFNDEF OID_AS_INT64}
interface
uses
  tiPerObjOIDAbs
  ,tiObjAbs
  ;

type

  TOIDGUID = class( TOID )
  private
    FAsString : string ;
  protected
    function  GetAsString: ShortString; override ;
    procedure SetAsString(const Value: ShortString); override ;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const Value: Variant);override;
  public
    function  IsNull : boolean ; override ;
    procedure AssignToTIQueryParam( const pFieldName : string ; const pParams : TtiObjAbs ) ; override ;
    procedure AssignToTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    procedure AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    function  EqualsQueryField( const pFieldName : string ; const pQuery : TtiObjAbs ) : boolean ; override;
    procedure Assign( const pSource : TOID ) ; override ;
    function  Compare( const pCompareWith : TOID ) : integer ; override ;
    procedure SetToNull; override;
    function  NullOIDAsString : string ; override ;
    procedure GetNextValue( const pDatabaseName : string ; const pPerLayerName : string ) ; override ;
  end ;

  TNextOIDGeneratorGUID = class( TNextOIDGenerator )
  private
  public
    constructor Create ; override ;
    destructor  destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ; pPerLayerName : string ) ; override ;
  end ;

const
  cOIDClassNameGUID = 'OIDClassNameGUID' ;

implementation
uses
  tiQuery
  ,tiPersist
  {$IFDEF MSWINDOWS}
  ,tiWin32
  {$ENDIF MSWINDOWS}
  ,SysUtils     // linux CreateGUID()
  ,tiLog
  ,ctiPersist
  ;

{ TOIDGUID }

function TOIDGUID.getAsString: ShortString;
begin
  result := FAsString ;
end;

procedure TOIDGUID.SetAsString(const Value: ShortString);
begin
  FAsString := Value ;
end;

function TOIDGUID.IsNull: boolean;
begin
  result := (FAsString = NullOIDAsString);
end;

procedure TOIDGUID.AssignFromTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsString := lQuery.FieldAsString[ pFieldName ] ;
end;

procedure TOIDGUID.AssignToTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsString[ pFieldName ] := FAsString ;
end;

function TOIDGUID.EqualsQueryField(const pFieldName: string; const pQuery: TtiObjAbs): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsString = lQuery.FieldAsString[ pFieldName ] ) ;
end;

procedure TOIDGUID.Assign(const pSource: TOID);
begin
  FAsString := pSource.AsString ;
end;

function TOIDGUID.Compare(const pCompareWith: TOID): integer;
begin
  if AsString < pCompareWith.AsString then
    result := -1
  else if AsString > pCompareWith.AsString then
    result := 1
  else
    result := 0 ;
end;

procedure TOIDGUID.SetToNull;
begin
  FAsString := NullOIDAsString ;
end;



const
  cuLowRange = 100 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorGUID
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorGUID.AssignNextOID(const pAssignTo: TOID; const pDatabaseName : string ; pPerLayerName : string );
var
  {$IFDEF LINUX}
  lGuid : TGUID;
  {$ENDIF}
  lValue : string ;
const
  cGUIDLength = 38 ;
begin
  Assert( pAssignTo.TestValid(TOID), cTIInvalidObjectError ) ;
  {$IFDEF MSWINDOWS}
  lValue := tiWin32CoCreateGUID;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  CreateGUID(lGUID);
  lValue := GuidToString(lGUID);
  {$ENDIF LINUX}

//           10        20        30
// 01234567890123456789012345678901234567
// {81A9C48C-DEF3-11D6-81C4-0002E31296EB}
// A GUID will be 38 chars long when created,
// or 36 chars long when {} are removed.

  if ( lValue[1] = '{' ) and
     ( lValue[cGUIDLength] = '}' ) then
    lValue := Copy( lValue, 2, cGUIDLength - 2 ) ;
  pAssignTo.AsString := lValue ;

end;

constructor TNextOIDGeneratorGUID.Create;
begin
  inherited ;
end;

//------------------------------------------------------------------------------
destructor TNextOIDGeneratorGUID.destroy;
begin
  inherited;
end;

function TOIDGUID.GetAsVariant: Variant;
begin
  result := FAsString ;
end;

procedure TOIDGUID.SetAsVariant(const Value: Variant);
begin
  FAsString := Value ;
end;

function TOIDGUID.NullOIDAsString: string;
begin
  result := '' ;
end;

procedure TOIDGUID.GetNextValue(const pDatabaseName, pPerLayerName: string);
var
  lNextOIDGenerator : TNextOIDGeneratorGUID ;
begin
  lNextOIDGenerator := TNextOIDGeneratorGUID.Create ;
  try
    lNextOIDGenerator.AssignNextOID(Self, pDatabaseName, pPerLayerName );
  finally
    lNextOIDGenerator.Free;
  end;
end;

procedure TOIDGUID.AssignToTIQueryParam(const pFieldName: string;const pParams: TtiObjAbs);
var
  lParams : TtiQueryParams ;
begin
  Assert( pParams is TtiQueryParams, 'pQuery not a TtiQuery' ) ;
  lParams := TtiQueryParams( pParams ) ;
  lParams.SetValueAsString(pFieldName, FAsString);
end;

initialization

  gTIPerMgr.OIDFactory.RegisterMapping( cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID )  ;
  if gTIPerMgr.DefaultOIDClassName = '' then
    gTIPerMgr.DefaultOIDClassName := cOIDClassNameGUID ;

{$ELSE}
interface
implementation
{$ENDIF}

end.
