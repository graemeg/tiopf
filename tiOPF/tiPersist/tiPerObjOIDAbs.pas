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
                                
unit tiPerObjOIDAbs;

{$I tiDefines.inc}

{$IFDEF OID_AS_INT64}
  {$I tiPerObjOIDAsInt64.pas}
{$ELSE}

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

  // The abstract OID class
  TOID = class( TtiObjAbs )
  private
  protected
    function  GetAsString: ShortString; virtual ; abstract ;
    procedure SetAsString(const Value: ShortString); virtual ; abstract ;
    function  GetAsVariant: Variant;virtual ; abstract ;
    procedure SetAsVariant(const Value: Variant);virtual ; abstract ;
  public
    constructor Create ; virtual ;
    property    AsString : ShortString read GetAsString write SetAsString ;
    property    AsVariant : Variant read GetAsVariant write SetAsVariant ;
    function    IsNull : boolean ; virtual ; abstract ;
    procedure   AssignToTIQueryParam( const pFieldName : string ; const pParams : TtiObjAbs ) ; virtual ; abstract ;
    procedure   AssignToTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; overload ; virtual ; abstract ;
    procedure   AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiObjAbs ) ; overload ; virtual ; abstract ;
    procedure   AssignToTIQuery( const pQuery : TtiObjAbs ) ; overload ;
    procedure   AssignFromTIQuery( const pQuery : TtiObjAbs ) ; overload ;
    function    EqualsQueryField( const pFieldName : string ; const pQuery : TtiObjAbs ) : boolean ; virtual ; abstract ;
    procedure   Assign( const pSource : TOID ) ; reintroduce ; virtual ;
    function    Compare( const pCompareWith : TOID ) : integer ; virtual ; abstract ;
    function    Equals( const pCompareWith : TOID ) : boolean ;
    procedure   SetToNull; virtual; abstract;
    function    Clone : TOID;
    function    NullOIDAsString : string ; virtual ; abstract ;

    procedure   GetNextValue( const pDatabaseName : string ; const pPerLayerName : string ) ; virtual ;
  end ;

  TOIDClass = class of TOID ;

  // Each TOID must have an associated TNextOIDGenerator that is responsible
  // for returning the next OID for that generation stratergy
  TNextOIDGenerator = class( TtiObjAbs )
  private
    FDatabaseName: string;
  public
    constructor Create ; virtual ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ; pPerLayerName : string ) ; virtual ; abstract ;
    property    DatabaseName : string read FDatabaseName write FDatabaseName ;
  end ;

  TNextOIDGeneratorClass = class of TNextOIDGenerator ;

  // Keeps a list of databases and their associated OIDGenerators
  // For example, you might be working with two databases, using the TOIDInteger
  // class of TOID. You might have a different Next_OID table in each database.
  // The TNextOIDMgr keeps track of this.
  // ToDo: Must be able to override this so all OIDs come from one source
  //       This should be given some more thought as this Mgr was introduced
  //       when we had only one class of OID. The relationship between
  //       Persistence Layer->OIDClass->Database->Instance of NextOIDGenerator
  //       must be tidied up.
  // ---------------------------------------------------------------------------
  TNextOIDMgr = class( TtiObjAbs )
  private
    FList : TObjectList ;
    FOwner: TtiObjAbs;
  protected
  public
    constructor create ; virtual ;
    destructor  destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ) ; virtual ;
    procedure   UnloadNextOIDGenerator( const pDatabaseName : string ) ;
    function    FindCreateByDatabaseName( const pDatabaseName : string ) : TNextOIDGenerator ;
    function    FindByDatabaseName( const pDatabaseName : string ) : TNextOIDGenerator ;
    procedure   Clear ;
    property    Owner : TtiObjAbs read FOwner write FOwner ; // A TtiRegPerLayer
  end ;

  // A mapping betweeen a TOID and TNextOIDGenerator. Used under the hood
  // to populate TOID.Value with the correct value
  // ---------------------------------------------------------------------------
  TOIDClassMapping = class( TtiObjAbs )
  private
    FOIDClassName: string;
    FOIDClass: TOIDClass;
    FNextOIDGeneratorClass: TNextOIDGeneratorClass;
  public
    property OIDClass : TOIDClass read FOIDClass write FOIDClass ;
    property OIDClassName : string read FOIDClassName write FOIDClassName ;
    property NextOIDGeneratorClass : TNextOIDGeneratorClass read FNextOIDGeneratorClass write FNextOIDGeneratorClass ;
  end ;

  // The factory produces the correct class of TOID and it's associated
  // TNextOIDGenerator
  // ---------------------------------------------------------------------------
  TOIDFactory = class( TtiObjAbs )
  protected
    // These are protected so they can be accessed in a descendant class
    // for unit testing.
    FList : TObjectList ;
    function FindByOIDClassName( const pClassName : string )  : TOIDClassMapping ;
  public
    Constructor Create ;
    Destructor  Destroy ; override ;
    procedure   RegisterMapping( const pOIDClassName : string ; const pOIDClass : TOIDClass ; const pNextOIDGeneratorClass : TNextOIDGeneratorClass ) ;
    function    CreateOID( const pOIDClassName : string = '' ) : TOID ;
    function    CreateNextOIDGenerator( const pOIDClassName : string ) : TNextOIDGenerator ;
  end ;

  function OIDToString( const pOID : TOID ) : string ;
  function OIDEquals( const pOID1, pOID2 : TOID ) : boolean ;

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
  ,tiLog
  ;

function OIDToString( const pOID : TOID ) : string ;
begin
  result := pOID.AsString ;
end;

function OIDEquals( const pOID1, pOID2 : TOID ) : boolean ;
begin
  result := pOID1.Equals( pOID2 ) ;
end;

constructor TOIDFactory.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

function TOIDFactory.CreateNextOIDGenerator( const pOIDClassName: string): TNextOIDGenerator;
var
  lOIDClassMapping : TOIDClassMapping ;
begin
  lOIDClassMapping := FindByOIDClassName( pOIDClassName ) ;
  Assert( lOIDClassMapping <> nil, 'Attempt to create unregistered OID class <' + pOIDClassName + '>' ) ;
  result := lOIDClassMapping.NextOIDGeneratorClass.Create ;
end;

function TOIDFactory.CreateOID(const pOIDClassName: string = '' ): TOID;
var
  lOIDClassMapping : TOIDClassMapping ;
  lOIDClassName : string ;
begin
  if pOIDClassName = '' then
    lOIDClassName := gTIPerMgr.DefaultOIDClassName
  else
    lOIDClassName := pOIDClassName ;

  lOIDClassMapping := FindByOIDClassName( lOIDClassName ) ;
  Assert( lOIDClassMapping <> nil,
          'Attempt to create unregistered OID class <' + lOIDClassName + '>' + Cr(2) +
          'You must include one of the delphi pas files tiPerObjOIDXXX.pas in your application.') ;
  result := lOIDClassMapping.OIDClass.Create ;

end;

destructor TOIDFactory.Destroy;
begin
  FList.Free ;
  inherited;
end;

function TOIDFactory.FindByOIDClassName( const pClassName : string ) : TOIDClassMapping;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FList.Count - 1 do
    if SameText(TOIDClassMapping(FList.Items[i]).OIDClassName, pClassName ) then
    begin
      result := TOIDClassMapping( FList.Items[i] );
      Exit ; //==>
    end ;
end;

procedure TOIDFactory.RegisterMapping(
  const pOIDClassName: string;
  const pOIDClass: TOIDClass ;
  const pNextOIDGeneratorClass : TNextOIDGeneratorClass );
var
  lOIDClassMapping : TOIDClassMapping ;
begin
  if FindByOIDClassName( pOIDClassName ) <> nil then
  begin
    tiAppError( 'Attempt to register duplicated OID type: ' + pOIDClassName + Cr(2) +
                'You can only include one of the delphi PAS files tiPerObjOIDXXX.pas in your application.') ;
    LogError( 'About to call HALT from ' + ClassName + '.RegisterMapping' ) ;
    Halt ;
  end ;
  lOIDClassMapping := TOIDClassMapping.Create ;
  lOIDClassMapping.OIDClassName := pOIDClassName ;
  lOIDClassMapping.OIDClass := pOIDClass ;
  lOIDClassMapping.NextOIDGeneratorClass := pNextOIDGeneratorClass ;
  FList.Add( lOIDClassMapping ) ;

end;

procedure TOID.AssignFromTIQuery(const pQuery: TtiObjAbs);
begin
  AssignFromTIQuery( cDefaultOIDFieldName, pQuery ) ;
end;

procedure TOID.AssignToTIQuery(const pQuery: TtiObjAbs);
begin
  AssignToTIQuery( cDefaultOIDFieldName, pQuery ) ;
end;

procedure TOID.Assign( const pSource : TOID ) ;
begin
  Assert(false, ClassName + '.Assign not implemented' ) ;
end ;

function TOID.Clone: TOID;
begin
  result:=TOID(ClassType.Create);
  result.Assign(self);
end;

constructor TOID.Create;
begin
  inherited ;
  SetToNull; // Just to be sure that ALWAYS it will be setted to NULL value...
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
function TNextOIDMgr.FindCreateByDatabaseName( const pDatabaseName: string): TNextOIDGenerator;
begin
  result := FindByDatabaseName(pDatabaseName);
  if result = nil then
  begin
    result :=
      gTIPerMgr.OIDFactory.CreateNextOIDGenerator(
        gTIPerMgr.DefaultOIDClassName ) ;
    result.DatabaseName := pDatabaseName ;
    FList.Add( result ) ;
  end ;
end;

//------------------------------------------------------------------------------
function TNextOIDMgr.FindByDatabaseName( const pDatabaseName: string): TNextOIDGenerator;
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
end;

//------------------------------------------------------------------------------
procedure TNextOIDMgr.AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string );
var
  lNextOIDGenerator : TNextOIDGenerator ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  Assert( pAssignTo.TestValid(TOID), cTIInvalidObjectError ) ;
  Assert( pDatabaseName <> '', 'Database name not assigned' ) ;
  lNextOIDGenerator := FindCreateByDatabaseName( pDatabaseName ) ;
  Assert( lNextOIDGenerator.TestValid(TNextOIDGenerator), cTIInvalidObjectError + ' No NextOIDGenerator found for ' + pDatabaseName ) ;
  lRegPerLayer := Owner as TtiRegPerLayer ;
  Assert( lRegPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );
  lNextOIDGenerator.AssignNextOID( pAssignTo, pDatabaseName, lRegPerLayer.PerLayerName ) ;
end;

procedure TNextOIDMgr.UnloadNextOIDGenerator(const pDatabaseName: string);
var
  lNextOIDGenerator : TNextOIDGenerator ;
begin
  lNextOIDGenerator := FindByDatabaseName( pDatabaseName ) ;
  Assert( lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + pDatabaseName ) ;
  FList.Remove( lNextOIDGenerator ) ;
end;

function TOID.Equals(const pCompareWith: TOID): boolean;
begin
  result := Compare( pCompareWith ) = 0 ;
end;

procedure TOID.GetNextValue(const pDatabaseName: string ; const pPerLayerName : string );
var
  lRegPerLayer : TtiRegPerLayer ;
  lDatabaseName : string ;
begin
  if pPerLayerName = '' then
    lRegPerLayer := gTIPerMgr.DefaultPerLayer
  else
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(pPerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + pPerLayerName + '>' ) ;
  if pDatabaseName = '' then
    lDatabaseName := gTIPerMgr.DefaultDBConnectionName
  else
    lDatabaseName := pDatabaseName ;
  Assert(lDatabaseName <> '', 'Unable to determine DatabaseName' ) ;
  lRegPerLayer.NextOIDMgr.AssignNextOID( Self, lDatabaseName ) ;
end;

{ TNextOIDGenerator }

constructor TNextOIDGenerator.Create;
begin
  inherited ;
end;

procedure TNextOIDMgr.Clear;
begin
  FList.Clear ;
end;

{$ENDIF}

end.
