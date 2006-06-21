unit tiOIDInteger;

{$I tiDefines.inc}  

{$IFNDEF OID_AS_INT64}

interface
uses
  tiOID
  ,tiBaseObject
  ,tiObject
  ,tiVisitorDB
  ,tiVisitor
  ;

type

  TOIDInteger = class( TOID )
  private
    FAsInteger : integer ;
  protected
    function  GetAsString: ShortString; override ;
    procedure SetAsString(const Value: ShortString); override ;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const Value: Variant);override;
  public
    function  IsNull : boolean ; override ;
    procedure AssignToTIQueryParam( const pFieldName : string ; const pParams : TtiBaseObject ) ; override ;
    procedure AssignToTIQuery( const pFieldName : string ; const pQuery : TtiBaseObject ) ; override ;
    procedure AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiBaseObject ) ; override ;
    function  EqualsQueryField( const pFieldName : string ; const pQuery : TtiBaseObject ) : boolean ; override;
    procedure Assign( const pSource : TOID ) ; override ;
    function  Compare( const pCompareWith : TOID ) : integer ; override ;
    procedure SetToNull; override;
    function  NullOIDAsString: String; override ;
    property  AsInteger : Integer read FAsInteger write FAsInteger ;
  end ;

  TNextOIDData = class( TtiObject )
  private
    FNextOID: integer;
  public
    property NextOID : integer read FNextOID write FNextOID ;
  end ;

  TNextOIDGeneratorInteger = class( TNextOIDGenerator )
  private
    FLow  : Integer ;
    FLowRange: Integer;
    FDirty: boolean;
    FNextOIDData : TNextOIDData ;
    function NextOID( const pDatabaseName : string ; pPerLayerName : string ) : Integer ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ; pPerLayerName : string ) ; override ;
  end ;

  TVisDBNextOIDAmblerRead = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;

  TVisDBNextOIDAmblerUpdate = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;


const
  cOIDClassNameInteger = 'OIDClassNameInteger' ;

implementation
uses
  tiQuery
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ;

{ TOIDInteger }

function TOIDInteger.getAsString: ShortString;
begin
  result := IntToStr( FAsInteger ) ;
end;

procedure TOIDInteger.SetAsString(const Value: ShortString);
begin
  if Value <> '' then
    FAsInteger := StrToInt( Value )
  else
    FAsInteger := 0 ;
end;

function TOIDInteger.IsNull: boolean;
begin
  result := FAsInteger = cNullOIDInteger ;
end;

procedure TOIDInteger.AssignFromTIQuery(const pFieldName : string ; const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsInteger := lQuery.FieldAsInteger[ pFieldName ] ;
end;

procedure TOIDInteger.AssignToTIQuery(const pFieldName : string ; const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsInteger[ pFieldName ] := FAsInteger ;
end;

function TOIDInteger.EqualsQueryField(const pFieldName: string; const pQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsInteger = lQuery.FieldAsInteger[ pFieldName ] ) ;
end;

procedure TOIDInteger.Assign(const pSource: TOID);
begin
  AsString := pSource.AsString ;
end;

function TOIDInteger.Compare(const pCompareWith: TOID): integer;
begin
  Assert( pCompareWith is TOIDInteger, 'pCompareWith not a pCompareWith' ) ;
  if AsInteger < TOIDInteger(pCompareWith).AsInteger then
    result := -1
  else if AsInteger > TOIDInteger(pCompareWith).AsInteger then
    result := 1
  else
    result := 0 ;
end;

const
  cuLowRange = 100 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorInteger
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorInteger.AssignNextOID(const pAssignTo: TOID ; const pDatabaseName : string ; pPerLayerName : string );
begin
  Assert( pAssignTo.TestValid(TOID), cTIInvalidObjectError ) ;
  pAssignTo.AsString := IntToStr( NextOID( pDatabaseName, pPerLayerName )) ;
end;

constructor TNextOIDGeneratorInteger.Create;
begin
  inherited ;
  FLow  := 0 ;
  FLowRange := cuLowRange ;
  FDirty := true ;
  FNextOIDData := TNextOIDData.Create ;
end;

destructor TNextOIDGeneratorInteger.Destroy;
begin
  FNextOIDData.Free ;
  inherited;
end;

function TNextOIDGeneratorInteger.NextOID( const pDatabaseName : string ; pPerLayerName : string ) : Integer;
begin
  if FDirty then
  begin
    gTIOPFManager.VisitorManager.Execute( cNextOIDReadHigh, FNextOIDData, pDatabaseName, pPerLayerName ) ;
    FDirty := false ;
  end ;

  result := ( FNextOIDData.NextOID * FLowRange ) + FLow ;

  Inc( FLow ) ;
  if FLow = FLowRange then
  begin
    FDirty := true ;
    FLow  := 0 ;
  end ;

end;

procedure TOIDInteger.SetToNull;
begin
  FAsInteger := cNullOIDInteger ;
end;

function TOIDInteger.GetAsVariant: Variant;
begin
  result := FAsInteger ;
end;

procedure TOIDInteger.SetAsVariant(const Value: Variant);
begin
  FAsInteger := Value ;
end;

function TOIDInteger.NullOIDAsString: String;
begin
  result := IntToStr( cNullOIDInteger ) ;
end;

procedure TOIDInteger.AssignToTIQueryParam(const pFieldName: string; const pParams : TtiBaseObject );
var
  lParams : TtiQueryParams ;
begin
  Assert( pParams is TtiQueryParams, 'pQuery not a TtiQuery' ) ;
  lParams := TtiQueryParams( pParams ) ;
  lParams.SetValueAsInteger(pFieldName, FAsInteger);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDData ) ;
end;

procedure TVisDBNextOIDAmblerRead.Execute(const pData: TtiVisited);
begin

  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  Query.SelectRow( 'Next_OID', nil ) ;
  try
    TNextOIDData( Visited ).NextOID := Query.FieldAsInteger[ 'OID' ] ;
  finally
    Query.Close ;
  end ;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDData ) ;
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const pData: TtiVisited);
var
  lParams : TtiQueryParams ;
begin
  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsInteger('OID', Integer( TNextOIDData( Visited ).NextOID + 1 )) ;
    Query.UpdateRow( 'Next_OID', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;

initialization

  gTIOPFManager.OIDFactory.RegisterMapping( cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger )  ;
  if gTIOPFManager.DefaultOIDClassName = '' then
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameInteger ;

  gTIOPFManager.VisitorManager.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerRead ) ;
  gTIOPFManager.VisitorManager.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate ) ;
{$ELSE}
interface
implementation
{$ENDIF}
end.
