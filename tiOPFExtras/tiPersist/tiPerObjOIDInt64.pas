unit tiPerObjOIDInt64;

{ This is a clone of tiPerObjOIDInteger, but with Int64 as our OID container.
  Created by ipk on 2003-01-09
  }

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

  TOIDInt64 = class( TOID )
  private
    FAsInt64 : Int64 ;
  protected
    function  GetAsString: ShortString; override ;
    procedure SetAsString(const Value: ShortString); override ;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const Value: Variant);override;
  public
    function  IsNull : boolean ; override ;
    procedure AssignToTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    procedure AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; override ;
    function  EqualsQueryField( const pFieldName : string ; const pQuery : TtiObjAbs ) : boolean ; override;
    procedure Assign( const pSource : TOID ) ; override ;
    function  Compare( const pCompareWith : TOID ) : Integer ; override ;
    procedure  SetToNull; override;
    property   AsInt64 : Int64 read FAsInt64 write FAsInt64 ;
  end ;

  TNextOIDData = class( TPerObjAbs )
  private
    FNextOID: Int64;
  public
    property NextOID : Int64 read FNextOID write FNextOID ;
  end ;

  TNextOIDGeneratorInt64 = class( TNextOIDGenerator )
  private
//    FHigh : Integer ;
    FLow  : Int64 ;
    FLowRange: Int64;
    FDirty: boolean;
    FNextOIDData : TNextOIDData ;
    function NextOID : Int64 ;
  public
    constructor Create ; override ;
    destructor  destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ) ; override ;
  end ;

  TVisDBNextOIDAmblerRead = class( TVisQryAbs )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisDBNextOIDAmblerUpdate = class( TVisQryAbs )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;


const
  cOIDClassNameInt64 = 'OIDClassNameInt64' ;
  cgsNextOIDReadHigh = 'NextOIDReadHigh' ;
  cUnAssignedOID = -1 ;

implementation
uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiPersist
  ;

{ TOIDInt64 }

function TOIDInt64.getAsString: ShortString;
begin
  result := IntToStr( FAsInt64 ) ;
end;

procedure TOIDInt64.SetAsString(const Value: ShortString);
begin
  try
    FAsInt64 := StrToInt( Value ) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetAsString' ) ;
  end ;
end;

function TOIDInt64.IsNull: boolean;
begin
  result := FAsInt64 = cUnAssignedOID ;
end;

procedure TOIDInt64.AssignFromTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsInt64 := lQuery.FieldAsInteger[ pFieldName ] ;
end;

procedure TOIDInt64.AssignToTIQuery(const pFieldName : string ; const pQuery: TtiObjAbs);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsInteger[ pFieldName ] := FAsInt64 ;
end;

function TOIDInt64.EqualsQueryField(const pFieldName: string; const pQuery: TtiObjAbs): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsInt64 = lQuery.FieldAsInteger[ pFieldName ] ) ;
end;

procedure TOIDInt64.Assign(const pSource: TOID);
begin
  AsString := pSource.AsString ;
end;

function TOIDInt64.Compare(const pCompareWith: TOID): Integer;
begin
  Assert( pCompareWith is TOIDInt64, 'pCompareWith not a pCompareWith' ) ;
  if AsInt64 < TOIDInt64(pCompareWith).AsInt64 then
    result := -1
  else if AsInt64 > TOIDInt64(pCompareWith).AsInt64 then
    result := 1
  else
    result := 0 ;
end;

const
  cuLowRange = 100 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorInt64
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorInt64.AssignNextOID(const pAssignTo: TOID);
begin
  pAssignTo.AsString := IntToStr( NextOID ) ;
end;

constructor TNextOIDGeneratorInt64.Create;
begin
  inherited ;
  FLow  := 0 ;
  FLowRange := cuLowRange ;
  FDirty := true ;
  FNextOIDData := TNextOIDData.Create ;
end;

//------------------------------------------------------------------------------
destructor TNextOIDGeneratorInt64.destroy;
begin
  FNextOIDData.Free ;
  inherited;
end;

function TNextOIDGeneratorInt64.NextOID: Int64;
begin
  if FDirty then
  begin
    gTIPerMgr.VisMgr.Execute( cgsNextOIDReadHigh, FNextOIDData ) ;
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

procedure TOIDInt64.SetToNull;
begin
  FAsInt64 := cUnAssignedOID ;
end;

function TOIDInt64.GetAsVariant: Variant;
begin
  result := FAsInt64 ;
end;

procedure TOIDInt64.SetAsVariant(const Value: Variant);
begin
  FAsInt64 := Value ;
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDData ) ;
end;

procedure TVisDBNextOIDAmblerRead.Execute(const pData: TVisitedAbs);
begin

  if gTIPerMgr.Terminated then
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

procedure TVisDBNextOIDAmblerUpdate.Execute(const pData: TVisitedAbs);
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
    lParams.ParamAsVariant[ 'OID' ] := Int64( TNextOIDData( Visited ).NextOID + 1 ) ;
    Query.UpdateRow( 'Next_OID', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;

initialization

  gTIPerMgr.OIDFactory.RegisterMapping( cOIDClassNameInt64, TOIDInt64, TNextOIDGeneratorInt64 )  ;
  if gTIPerMgr.DefaultOIDClassName = '' then
    gTIPerMgr.DefaultOIDClassName := cOIDClassNameInt64 ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDReadHigh, TVisDBNextOIDAmblerRead ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDReadHigh, TVisDBNextOIDAmblerUpdate ) ;

end.
