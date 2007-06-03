unit Client_BOM;

interface
uses
   tiObject
  ,tiOIDGUID
  ,Classes
  ;

const
  cErrorClientNameMissing = 'Please enter a client name' ;
  cErrorClientIDMissing   = 'Please enter a client ID' ;
  cErrorClientCompanyName = 'Please enter a company name';
  cErrorClientFamilyName  = 'Please enter a family name';
  cErrorClientGivenName   = 'Please enter a given name';
type

  TClientAbs = class ;
  TClients = class ;

  TClients = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TClientAbs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TClientAbs); reintroduce ;
  public
    property    Items[i:integer] : TClientAbs read GetItems write SetItems ;
    procedure   Add( pObject : TClientAbs   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    procedure   Clear ; override ;
    procedure   Read ; override ;
  published
  end ;

  TClientAbs = class( TtiObject )
  private
    FClientID : string ;
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
    function    GetClientType: string; virtual ; abstract ;
  public
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
  published
    property    ClientID   : string   read FClientID   write FClientID ;
    property    ClientType : string read GetClientType ;
  end ;

  TClientCompany = class( TClientAbs )
  private
    FCompanyName: string;
  protected
    function    GetCaption : string ; override ;
    function    GetClientType: string; override ;
  public
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
  published
    property CompanyName : string read FCompanyName write FCompanyName;
  end ;

  TClientPerson = class( TClientAbs )
  private
    FFamilyName: string;
    FGivenName: string;
  protected
    function    GetCaption : string ; override ;
    function    GetClientType: string; override ;
  public
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
  published
    property GivenName     : string  read FGivenName write FGivenName ;
    property FamilyName    : string read FFamilyName write FFamilyName ;
  end ;

implementation
uses
  tiOPFManager
  ,tiClassToDBMap_BOM
  ,Windows
  ,tiLog
  ,SysUtils
  ;

{ TClients }

procedure TClients.Add(pObject: TClientAbs; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

procedure TClients.Clear;
begin
  inherited;
  ObjectState := posEmpty ;
end;

function TClients.GetItems(i: integer): TClientAbs;
begin
  result := TClientAbs( inherited GetItems( i )) ;
end;

procedure TClients.Read;
var
  lNow : DWord ;
begin
  lNow := GetTickCount ;
  inherited;
  Log('Time to load ' + IntToStr( Count ) + ' Clients: ' +
      IntToStr( GetTickCount - lNow )) ;
end;

procedure TClients.SetItems(i: integer; const Value: TClientAbs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TClient }


function TClientAbs.GetOwner: TClients;
begin
  result := TClients( inherited GetOwner );
end;

function TClientAbs.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if ClientID = '' then
    pErrors.AddError( 'ClientID', cErrorClientIDMissing ) ;

  result := pErrors.Count = 0 ;

end;

procedure TClientAbs.SetOwner(const Value: TClients);
begin
  inherited SetOwner( Value ) ;
end;

{ TClientCompany }

function TClientCompany.GetCaption: string;
begin
  result := 'Company name: "' + CompanyName + '"';
end;

function TClientCompany.GetClientType: string;
begin
  result := 'Company' ;
end;

function TClientCompany.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(pErrors);
  if CompanyName = '' then
    pErrors.AddError('CompanyName', cErrorClientCompanyName);
  result := pErrors.Count = 0;
end;

{ TClientPerson }

function TClientPerson.GetCaption: string;
begin
  result :=
    'Given name: "' + GivenName +
    '" Family name: "' + FamilyName + '"';
end;

function TClientPerson.GetClientType: string;
begin
  result := 'Person' ;
end;

function TClientPerson.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(pErrors);
  if GivenName = '' then
    pErrors.AddError('GivenName', cErrorClientGivenName);
  if FamilyName = '' then
    pErrors.AddError('FamilyName', cErrorClientFamilyName);
  result := pErrors.Count = 0;
end;

end.

