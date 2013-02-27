unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ,tiPerObjOIDAbs
  ;

const
  cErrorClientName   = 'Please enter a client name' ;
  cErrorAdrsText     = 'Please enter an address' ;
  cErrorAdrsLocality = 'Please enter an address locality' ;
  cErrorAdrsState    = 'Please enter an address state' ;
  cErrorAdrsPostCode = 'Please enter a post code' ;
type

  TClient  = class ;
  TClients = class ;
  TAdrs    = class ;

  TClientName   = String[200];
  TAdrsText     = String[240];
  TAdrsLocality = String[46];
  TAdrsState    = String[3];
  TAdrsPostCode = String[4];

  TClients = class( TPerObjList ) ;

  TClient = class( TPerObjAbs )
  private
    FClientName: TClientName;
    FAdrs: TAdrs;
  protected
    procedure   AssignClassProps(pSource: TPerObjAbs); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    constructor CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; override ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    Adrs       : TAdrs read FAdrs ;
  end ;

  TAdrs = class( TPerObjAbs )
  private
    FAdrsText: TAdrsText ;
    FLocality: TAdrsLocality;
    FState   : TAdrsState;
    FPostCode: TAdrsPostCode;
    function  GetAsOneLine: string;
  protected
    function GetOID : TOID ; override ;
  public
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    AdrsText         : TAdrsText         read FAdrsText write FAdrsText ;
    property    Locality         : TAdrsLocality     read FLocality write FLocality ;
    property    State            : TAdrsState        read FState    write FState ;
    property    PostCode         : TAdrsPostCode     read FPostCode write FPostCode ;
    property    AsOneLine        : string            read GetAsOneLine ;
  end ;


procedure RegisterMappings ;

implementation
uses
  tiPersist
  ,tiClassToDBMap_BOM
  ;

procedure RegisterMappings ;
begin
  //                                          Class,   Table,    Property,     Column,       Special Info
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'         );
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClient);

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TAdrs, 'Adrs', 'OID',       'OID',       [pktDB, pktFK] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TAdrs, 'Adrs', 'AdrsText',  'Adrs_Text');
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TAdrs, 'Adrs', 'Locality',  'Locality' );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TAdrs, 'Adrs', 'State',     'State' );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TAdrs, 'Adrs', 'PostCode',  'Post_Code' );

end ;

{ TClient }

procedure TClient.AssignClassProps(pSource: TPerObjAbs);
begin
  Adrs.Assign(( pSource as TClient ).Adrs ) ;
end;

constructor TClient.Create;
begin
  inherited;
  FAdrs := TAdrs.Create ;
  FAdrs.Owner := Self ;
end;

constructor TClient.CreateNew(const pDatabaseName : string = '' ; const pPerLayerName : string = '');
begin
  inherited;
  FAdrs.OID.GetNextValue(pDatabaseName, pPerLayerName) ;
  FAdrs.ObjectState := posCreate ;
end;

destructor TClient.Destroy;
begin
  FAdrs.Free ;
  inherited;
end;

function TClient.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid( pErrors ) ;
  if ClientName = '' then
    pErrors.AddError( 'ClientName', cErrorClientName ) ;

  Adrs.IsValid(pErrors);

  result := pErrors.Count = 0 ;
end;

{ TAdrs }

function TAdrs.GetAsOneLine: string;
begin
  result :=
    AdrsText + ', ' + Locality + ' ' +
    State + '  ' + PostCode;
end;

function TAdrs.GetOID: TOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := OID ;
end;

function TAdrs.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  // Do not call inherited, because we do not want to clear pErrors
  if AdrsText = '' then
    pErrors.AddError( 'AdrsText', cErrorAdrsText ) ;

  if Locality = '' then
    pErrors.AddError( 'Locality', cErrorAdrsLocality ) ;

  if State = '' then
    pErrors.AddError( 'State', cErrorAdrsState ) ;

  if PostCode = '' then
    pErrors.AddError( 'State', cErrorAdrsPostCode ) ;

  result := pErrors.Count = 0 ;
end;

{ TClients }

end.

