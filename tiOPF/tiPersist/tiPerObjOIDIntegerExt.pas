{-----------------------------------------------------------------------------
 Unit Name: tiPerObjOIDIntegerExt
 Author:    Lukasz Zeligowski
 Purpose:   OID generator. This is just a COPY-PASTE version of TOIDInteger.
            There is change: null value. Main Null value in this OID is 0,
            but even when value is lower it is still 'null'.
            This also works proper with real null DB value (when field Integer
            is null then AsInteger gives 0).

 History:   0.9Beta first version
-----------------------------------------------------------------------------}

unit tiPerObjOIDIntegerExt;

{$I tiDefines.inc}

interface
uses
  tiPerObjOIDAbs
  ,tiObjAbs
  ,tiPtnVisPerObj
  ,tiPtnVisSQL
  ,tiPtnVis
  ;

type

  TOIDIntegerExt = class( TOID )
  private
    FAsInteger : integer ;
  protected
    function  GetAsString: ShortString; override ;
    procedure SetAsString(const Value: ShortString); override ;
  public
    function  IsNull : boolean ; override ;
    procedure AssignToTIQueryParam( const pFieldName : string ; const pParams : TtiObjAbs ) ; override ;
    procedure AssignToTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    procedure AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    function  EqualsQueryField( const pFieldName : string ; const pQuery : TtiObjAbs ) : boolean ; override;
    procedure Assign( const pSource : TOID ) ; override ;
    function  Compare( const pCompareWith : TOID ) : integer ; override ;
    procedure SetToNull; override;
    function  NullOIDAsString: String; override ;
  end ;

  TNextOIDDataExt = class( TPerObjAbs )
  private
    FNextOID: integer;
  public
    property NextOID : integer read FNextOID write FNextOID ;
  end ;

  TNextOIDGeneratorIntegerExt = class( TNextOIDGenerator )
  private
//    FHigh : Integer ;
    FLow  : Integer ;
    FLowRange: Integer;
    FDirty: boolean;
    FNextOIDDataExt : TNextOIDDataExt ;
    function NextOID : Integer ;
  public
    constructor Create ; override ;
    destructor  destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ) ; override ;
  end ;

  TVisDBNextOIDExtAmblerRead = class( TVisQryAbs )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisDBNextOIDExtAmblerUpdate = class( TVisQryAbs )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;


const
  cgsNextOIDExtReadHigh = 'NextOIDExtReadHigh' ;

implementation
uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiPersist
  ;

{ TOIDIntegerExt }

function TOIDIntegerExt.getAsString: ShortString;
begin
  result := IntToStr( FAsInteger ) ;
end;

procedure TOIDIntegerExt.SetAsString(const Value: ShortString);
begin
  try
    FAsInteger := StrToInt( Value ) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetAsString' ) ;
  end ;
end;

function TOIDIntegerExt.IsNull: boolean;
begin
  result := FAsInteger <= 0;
end;

procedure TOIDIntegerExt.AssignFromTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsInteger := lQuery.FieldAsInteger[ pFieldName ] ;
end;

procedure TOIDIntegerExt.AssignToTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsInteger[ pFieldName ] := FAsInteger ;
end;

function TOIDIntegerExt.EqualsQueryField(const pFieldName: string; const pQuery: TtiObjAbs): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsInteger = lQuery.FieldAsInteger[ pFieldName ] ) ;
end;

procedure TOIDIntegerExt.Assign(const pSource: TOID);
begin
  AsString := pSource.AsString ;
end;

function TOIDIntegerExt.Compare(const pCompareWith: TOID): integer;
begin
  if AsString < pCompareWith.AsString then
    result := -1
  else if AsString > pCompareWith.AsString then
    result := 1
  else
    result := 0 ;
end;

const
  cuLowRange = 100 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorIntegerExt
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorIntegerExt.AssignNextOID(const pAssignTo: TOID);
begin
  pAssignTo.AsString := IntToStr( NextOID ) ;
end;

constructor TNextOIDGeneratorIntegerExt.Create;
begin
  inherited ;
  FLow  := 0 ;
  FLowRange := cuLowRange ;
  FDirty := true ;
  FNextOIDDataExt := TNextOIDDataExt.Create ;
end;

//------------------------------------------------------------------------------
destructor TNextOIDGeneratorIntegerExt.destroy;
begin
  FNextOIDDataExt.Free ;
  inherited;
end;

function TNextOIDGeneratorIntegerExt.NextOID: Integer;
begin
  if FDirty then
  begin
    gTIPerMgr.VisMgr.Execute( cgsNextOIDExtReadHigh, FNextOIDDataExt ) ;
    FDirty := false ;
  end ;

  result := ( FNextOIDDataExt.NextOID * FLowRange ) + FLow ;

  Inc( FLow ) ;
  if FLow = FLowRange then
  begin
    FDirty := true ;
    FLow  := 0 ;
  end ;

end;

procedure TOIDIntegerExt.SetToNull;
begin
  FAsInteger := 0;
end;

function TOIDIntegerExt.NullOIDAsString: String;
begin
  result := '0' ;
end;

procedure TOIDIntegerExt.AssignToTIQueryParam(const pFieldName: string;const pParams: TtiObjAbs);
var
  lParams : TtiQueryParams ;
begin
  Assert( pParams is TtiQueryParams, 'pQuery not a TtiQuery' ) ;
  lParams := TtiQueryParams( pParams ) ;
  lParams.SetValue(pFieldName, FAsInteger);
end;

{ TVisDBNextOIDExtAmblerRead }

function TVisDBNextOIDExtAmblerRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDDataExt ) ;
end;

procedure TVisDBNextOIDExtAmblerRead.Execute(const pData: TVisitedAbs);
begin

  if gTIPerMgr.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  Query.SelectRow( 'Next_OID', nil ) ;
  try
    TNextOIDDataExt( Visited ).NextOID := Query.FieldAsInteger[ 'OID' ] ;
  finally
    Query.Close ;
  end ;
end;

{ TVisDBNextOIDExtAmblerUpdate }

function TVisDBNextOIDExtAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDDataExt ) ;
end;

procedure TVisDBNextOIDExtAmblerUpdate.Execute(const pData: TVisitedAbs);
var
  lParams : TtiQueryParams ;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant[ 'OID' ] := Integer( TNextOIDDataExt( Visited ).NextOID + 1 ) ;
    Query.UpdateRow( 'Next_OID', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;

initialization

  gTIPerMgr.OIDFactory.RegisterMapping( TOIDIntegerExt.ClassName, TOIDIntegerExt, TNextOIDGeneratorIntegerExt )  ;
  if gTIPerMgr.DefaultOIDClassName = '' then
    gTIPerMgr.DefaultOIDClassName := TOIDIntegerExt.ClassName ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDExtReadHigh, TVisDBNextOIDExtAmblerRead ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDExtReadHigh, TVisDBNextOIDExtAmblerUpdate ) ;

end.
