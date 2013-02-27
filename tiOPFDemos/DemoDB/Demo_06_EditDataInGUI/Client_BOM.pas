unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ;

const
  cErrorClientNameMissing = 'Please enter a client name' ;
  cErrorClientIDMissing = 'Please enter a client ID' ;

type

  TClient = class ;
  TClients = class ;

  TClientName = String[200];
  TClientID   = String[9];

  TClients = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TClient ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce ;
  public
    property    Items[i:integer] : TClient read GetItems write SetItems ;
    procedure   Add( pObject : TClient   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TClient = class( TPerObjAbs )
  private
    FClientID: TClientID;
    FClientName: TClientName;
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
  public
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    ClientID   : TClientID read FClientID write FClientID ;
  end ;

procedure RegisterMappings ;

implementation
uses
  tiPersist
  ,tiClassToDBMap_BOM
  ;

procedure RegisterMappings ;
begin
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID', 'OID', [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name' );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID', 'Client_ID' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
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

function TClient.IsValid(const pErrors: TPerObjErrors): boolean;
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

