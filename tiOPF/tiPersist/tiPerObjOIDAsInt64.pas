{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)
                                        
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

// Note, this pas file will not compile on it's own. It is designed to be
// included into tiPerObjOIDAbs when the conditional define OID_AS_INT64
// is used. See tiPerObjOIDAbs for details.

interface

uses
  Classes
  ,tiObjAbs
  ,Contnrs
  ,tiPtnVis
  ;

const
  cDefaultOIDFieldName       = 'OID' ;

type

  TOID = Int64 ;
  // ---------------------------------------------------------------------------
  TNextOIDMgr = class( TtiObjAbs )
  private
    FList : TObjectList ;
  protected
  public
    constructor create ; virtual ;
    destructor  destroy ; override ;
    function    NextOID( const pDatabaseName : string = '' ) : TOID ; virtual ;
    procedure   UnloadNextOIDGenerator( const pDatabaseName : string ) ;
    function    FindByDatabaseName( const pDatabaseName : string ) : TtiObjAbs ;
  end ;

  function OIDToString( pOID : TOID ) : string ;
  function OIDEquals( pOID1, pOID2 : TOID ) : boolean ;

  const
    cNullOID = -1 ;
    cgsNextOIDReadHigh = 'NextOIDReadHigh' ; // Visitor constant

implementation

uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiPersist
  ,tiDialogs
  ,tiPtnVisPerObj
  ,tiPtnVisSQL
  ,cTIPersist
  ,tiRegPerLayer
  ;

type

  TNextOIDGenerator = class( TPerObjAbs )
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
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisDBNextOIDAmblerUpdate = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
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

//------------------------------------------------------------------------------
function TNextOIDGenerator.NextOID: TOID;
begin
  if FDirty then
    gTIPerMgr.VisMgr.Execute( cgsNextOIDReadHigh, Self ) ;

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
constructor TNextOIDMgr.create;
begin
  inherited create ;
  FList := TObjectList.Create ;
end;

//------------------------------------------------------------------------------
destructor TNextOIDMgr.destroy;
begin
  FList.Free ;
  inherited ;
end;

//------------------------------------------------------------------------------
function TNextOIDMgr.FindByDatabaseName( const pDatabaseName: string): TtiObjAbs;
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
function TNextOIDMgr.NewPerObjAbs( pClass : TPerObjAbsClass ) : TPerObjAbs;
begin
  result     := pClass.Create ;
  result.OID := NextOID ;
  result.ObjectState := posCreate ;
end;
}
//------------------------------------------------------------------------------
function TNextOIDMgr.NextOID( const pDatabaseName : string = '' ) : TOID;
var
  lNextOIDGenerator : TNextOIDGenerator ;
  lDatabaseName : string ;
begin
  if pDatabaseName = '' then
    lDatabaseName := gTIPerMgr.DefaultDBConnectionName
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
  result := ( TVisitedAbs(Visited)is TNextOIDGenerator ) ;
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
    TNextOIDGenerator( Visited ).High := Query.FieldAsInteger[ 'OID' ] ;
  finally
    Query.Close ;
  end ;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( TVisitedAbs(Visited) is TNextOIDGenerator ) ;
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
    lParams.SetValueAsInteger( 'OID', TNextOIDGenerator( Visited ).High + 1 ) ;
    Query.UpdateRow( 'Next_OID', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;


initialization
  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDReadHigh, TVisDBNextOIDAmblerRead ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsNextOIDReadHigh, TVisDBNextOIDAmblerUpdate ) ;


