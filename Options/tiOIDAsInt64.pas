// Note, this pas file will not compile on it's own. It is designed to be
// included into tiOIDAbs when the conditional define OID_AS_INT64
// is used. See tiOIDAbs for details.

interface

uses
  Classes
  ,tiBaseObject
  ,Contnrs
  ,tiVisitor
  ;

const
  cDefaultOIDFieldName       = 'OID' ;

type

  TOID = Int64 ;
  TNextOIDMgr = class( TtiBaseObject )
  private
    FList : TObjectList ;
  protected
  public
    constructor Create ; virtual ;
    destructor  destroy ; override ;
    function    NextOID( const pDatabaseName : string = '' ) : TOID ; virtual ;
    procedure   UnloadNextOIDGenerator( const pDatabaseName : string ) ;
    function    FindByDatabaseName( const pDatabaseName : string ) : TtiBaseObject ;
  end ;

  function OIDToString( pOID : TOID ) : string ;
  function OIDEquals( pOID1, pOID2 : TOID ) : boolean ;
  function OIDCompare( pOID1, pOID2 : TOID ) : Integer ;

implementation

uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiOPFManager
  ,tiDialogs
  ,tiObject
  ,tiVisitorDB
  ,tiConstants
  ,tiPersistenceLayers
  ;

type

  TNextOIDGenerator = class( TtiObject )
  private
    FHigh : TOID ;
    FLow  : TOID ;
    FLowRange: TOID;
    FDirty: boolean;
    FDatabaseName: string;
    procedure SetHigh(const Value: TOID);
  public
    constructor Create ; override ;
    function NextOID : TOID ;
  published
    property High     : TOID read FHigh  write SetHigh ;
    property Low      : TOID read FLow ;
    property LowRange : TOID read FLowRange write FLowRange ;
    property DatabaseName : string read FDatabaseName write FDatabaseName ;
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
  cuLowRange = 100 ;

function OIDToString( pOID : TOID ) : string ;
begin
  result := IntToStr( pOID ) ;
end;

function OIDEquals( pOID1, pOID2 : TOID ) : boolean ;
begin
  result := pOID1 = pOID2 ;
end;

function OIDCompare( pOID1, pOID2 : TOID ) : Integer;
begin
  if pOID1 < pOID2 then
    result := -1
  else if pOID1 > pOID2 then
    result := 1
  else
    result := 0 ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGenerator
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNextOIDGenerator.Create;
begin
  inherited ;
  FLow  := 0 ;
  FHigh := 0 ;
  FLowRange := cuLowRange ;
  FDirty := true ;
end;

function TNextOIDGenerator.NextOID: TOID;
begin
  if FDirty then
    gTIOPFManager.VisMgr.Execute( cNextOIDReadHigh, Self ) ;

  Inc( FLow ) ;
  if FLow >= FLowRange then
    FDirty := true ;

  result := ( FHigh * FLowRange ) + FLow ;

end;

procedure TNextOIDGenerator.SetHigh( const Value : TOID );
begin
  FHigh := Value ;
  FLow  := 0 ;
  FDirty := false ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDMgr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNextOIDMgr.Create;
begin
  inherited create ;
  FList := TObjectList.Create ;
end;

destructor TNextOIDMgr.destroy;
begin
  FList.Free ;
  inherited ;
end;

function TNextOIDMgr.FindByDatabaseName( const pDatabaseName: string): TtiBaseObject;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FList.Count - 1 do
    if SameText( pDatabaseName,
                 TNextOIDGenerator( FList.Items[i] ).DatabaseName ) then
    begin
      result := TNextOIDGenerator( FList.Items[i] );
      Exit ; //==>
    end ;

  if result = nil then
  begin
    result := TNextOIDGenerator.Create ;
    TNextOIDGenerator(result).DatabaseName := pDatabaseName ;
    FList.Add( result ) ;
  end ;

end;

{
function TNextOIDMgr.NewPerObjAbs( pClass : TtiClass ) : TtiObject;
begin
  result     := pClass.Create ;
  result.OID := NextOID ;
  result.ObjectState := posCreate ;
end;
}
function TNextOIDMgr.NextOID( const pDatabaseName : string = '' ) : TOID;
var
  lNextOIDGenerator : TNextOIDGenerator ;
  lDatabaseName : string ;
begin
  if pDatabaseName = '' then
    lDatabaseName := gTIOPFManager.DefaultDBConnectionName
  else
    lDatabaseName := pDatabaseName ;
  lNextOIDGenerator := ( FindByDatabaseName( lDatabaseName ) as TNextOIDGenerator ) ;
  Assert( lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + lDatabaseName ) ;
  result := lNextOIDGenerator.NextOID ;
end;

procedure TNextOIDMgr.UnloadNextOIDGenerator(const pDatabaseName: string);
var
  lNextOIDGenerator : TNextOIDGenerator ;
begin
  lNextOIDGenerator := ( FindByDatabaseName( pDatabaseName ) as TNextOIDGenerator );
  Assert( lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + pDatabaseName ) ;
  FList.Remove( lNextOIDGenerator ) ;
end;

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := ( TtiVisited(Visited)is TNextOIDGenerator ) ;
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
    TNextOIDGenerator( Visited ).High := Query.FieldAsInteger[ 'OID' ] ;
  finally
    Query.Close ;
  end ;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( TtiVisited(Visited) is TNextOIDGenerator ) ;
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
    lParams.SetValueAsInteger( 'OID', TNextOIDGenerator( Visited ).High + 1 ) ;
    Query.UpdateRow( 'Next_OID', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;


initialization
  gTIOPFManager.VisMgr.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerRead ) ;
  gTIOPFManager.VisMgr.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate ) ;


