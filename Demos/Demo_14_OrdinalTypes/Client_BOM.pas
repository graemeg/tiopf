unit Client_BOM;

interface
uses
  tiObject
  ,Classes
  ;

const
  cErrorClientName = 'Please enter a client name' ;
  cErrorClientSex  = 'Please select a sex' ;

type
  TSex = ( sexUnknown, sexFemail, sexMale ) ;

const
  cSexDB : array[TSex] of string =
    ( 'UNKNOWN', 'FEMALE', 'MALE' );

  cSexGUI : array[TSex] of string =
    ( 'Unknown', 'Female', 'Male' );

procedure AssignSexs( const pStrings : TStrings ) ;

type

  TClient = class ;
  TClients = class ;

  TClientName = String[200];

  TClients = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TClient ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce ;
  public
    property    Items[i:integer] : TClient read GetItems write SetItems ;
    procedure   Add( pObject : TClient ) ; reintroduce ;
  published
  end ;

  TClient = class( TtiObject )
  private
    FSex : TSex ;
    FClientName: TClientName;
    function    GetSexAsDBString: string;
    function    GetSexAsGUIString: string;
    procedure   SetSexAsDBString(const Value: string);
    procedure   SetSexAsGUIString(const Value: string);
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
  public
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
    property    Sex : TSex read FSex write FSex ;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    SexAsDBString : string read GetSexAsDBString write SetSexAsDBString ;
    property    SexAsGUIString : string read GetSexAsGUIString write SetSexAsGUIString ;
  end ;

procedure RegisterMappings ;

implementation
uses
  tiOPFManager
  ,tiClassToDBMap_BOM
  ,SysUtils
  ;

procedure RegisterMappings ;
begin
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID', 'OID', [pktDB] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name' );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'SexAsDBString', 'Sex' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end ;

procedure AssignSexs( const pStrings : TStrings ) ;
var
  i : TSex ;
begin
  pStrings.Clear ;
  for i := Low(TSex) to High(TSex) do
    pStrings.Add(cSexGUI[i]);
end ;

{ TClients }

procedure TClients.Add(pObject: TClient);
begin
  inherited Add( pObject ) ;
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

function TClient.GetSexAsDBString: string;
begin
  result := cSexDB[FSex];
end;

function TClient.GetSexAsGUIString: string;
begin
  result := cSexGUI[FSex];
end;

function TClient.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if ClientName = '' then
    pErrors.AddError( 'ClientName', cErrorClientName ) ;

  if Sex = sexUnknown then
    pErrors.AddError( 'Sex', cErrorClientSex ) ;

  result := pErrors.Count = 0 ;

end;

procedure TClient.SetOwner(const Value: TClients);
begin
  inherited SetOwner( Value ) ;
end;

procedure TClient.SetSexAsDBString(const Value: string);
var
  i : TSex ;
begin
  FSex := sexUnknown ;
  for i := Low(TSex) to High(TSex) do
    if SameText( cSexDB[i], Value ) then
    begin
      FSex := i ;
      Exit ; //==>
    end ;
end;

procedure TClient.SetSexAsGUIString(const Value: string);
var
  i : TSex ;
begin
  FSex := sexUnknown ;
  for i := Low(TSex) to High(TSex) do
    if SameText( cSexGUI[i], Value ) then
    begin
      FSex := i ;
      Exit ; //==>
    end ;
end;

end.

