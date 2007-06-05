unit Client_BOM;

interface
uses
  tiObject
  ;

const
  cErrorClientNameMissing = 'Please enter a client name' ;
  cErrorClientIDMissing = 'Please enter a client ID' ;

type

  TClient = class ;
  TClients = class ;

  TClients = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TClient ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce ;
  public
    property    Items[i:integer] : TClient read GetItems write SetItems ;
    procedure   Add( pObject : TClient   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TClient = class( TtiObject )
  private
    FClientID: string;
    FClientName: string;
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
  public
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
  published
    property    ClientName : string read FClientName write FClientName ;
    property    ClientID   : string read FClientID write FClientID ;
  end ;

procedure RegisterMappings ;

implementation
uses
  tiOPFManager
  ,tiClassToDBMap_BOM
  ;

procedure RegisterMappings ;
begin
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID', 'OID', [pktDB] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name' );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID', 'Client_ID' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end ;

{ TClients }

procedure TClients.Add(pObject: TClient; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TClients.GetItems(i: integer): TClient;
begin
  result := TClient( inherited GetItems( i )) ;
end;

procedure TClients.SetItems(i: integer; const Value: TClient);
begin
  inherited SetItems( i, Value ) ;
end;

{ TClient }

function TClient.GetOwner: TClients;
begin
  result := TClients( inherited GetOwner );
end;

function TClient.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if ClientName = '' then
    pErrors.AddError( 'ClientName', cErrorClientNameMissing ) ;

  if ClientID = '' then
    pErrors.AddError( 'ClientID', cErrorClientIDMissing ) ;

  result := pErrors.Count = 0 ;

end;

procedure TClient.SetOwner(const Value: TClients);
begin
  inherited SetOwner( Value ) ;
end;

end.

