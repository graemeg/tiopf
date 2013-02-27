unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ,tiPerObjOIDAbs
  ,Classes
  ;

const
  cErrorClientNameMissing = 'Please enter a client name' ;
  cErrorClientIDMissing   = 'Please enter a client ID' ;
  cErrorClientCompanyName = 'Please enter a company name';
  cErrorClientFamilyName  = 'Please enter a family name';
  cErrorClientGivenName   = 'Please enter a given name';
  cErrorClientNameTitle   = 'Please enter a name title';
type

  TClientAbs = class ;
  TClients = class ;

  TCompanyName     = String[200];
  TClientID        = String[9];
  TGivenName       = String[40];
  TFamilyName      = String[40];
  TNameTitle       = String[12];

  TClients = class( TPerObjList )
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

  TClientAbs = class( TPerObjAbs )
  private
    FClientID : TClientID ;
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
    function    GetClientType: string; virtual ; abstract ;
  public
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    ClientID   : TClientID   read FClientID   write FClientID ;
    property    ClientType : string read GetClientType ;
  end ;

  TClientCompany = class( TClientAbs )
  private
    FCompanyName: TCompanyName;
  protected
    function    GetCaption : string ; override ;
    function    GetClientType: string; override ;
  public
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property CompanyName : TCompanyName read FCompanyName write FCompanyName;
  end ;

  TClientPerson = class( TClientAbs )
  private
    FFamilyName: TFamilyName;
    FGivenName: TGivenName;
    FNameTitle: TNameTitle;
  protected
    function    GetCaption : string ; override ;
    function    GetClientType: string; override ;
  public
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property GivenName     : TGivenName  read FGivenName write FGivenName ;
    property FamilyName    : TFamilyName read FFamilyName write FFamilyName ;
    property NameTitle     : TNameTitle  read FNameTitle write FNameTitle ;
  end ;

procedure RegisterMappings ;
procedure AssignNameTitles(const pStrings : TStrings ) ;

implementation
uses
  tiPersist
  ,tiClassToDBMap_BOM
  ,Windows
  ,tiLog
  ,SysUtils
  ;

// RegisterMappings only gets called in the One to Many - Auto Map demo
procedure RegisterMappings ;
begin
  //                                          Class,       Table,       Property,     Column
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'OID',        'OID', [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'ClientID',   'Client_ID'    ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClientAbs);

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'OID',         'OID', [pktDB, pktFK] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'CompanyName', 'Company_Name'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients,    TClientCompany);
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientCompany);

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'OID',        'OID', [pktDB, pktFK] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'GivenName',  'Given_Name'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'FamilyName', 'Family_Name'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'NameTitle',  'Name_Title'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients,    TClientPerson);
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientPerson);

end ;

procedure AssignNameTitles(const pStrings : TStrings ) ;
begin
  pStrings.Clear ;
  pStrings.Add('Mr');
  pStrings.Add('Mrs');
  pStrings.Add('Ms');
  pStrings.Add('Miss');
  pStrings.Add('Dr');
  pStrings.Add('Prof');
end;

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

function TClientAbs.IsValid(const pErrors: TPerObjErrors): boolean;
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
  result := CompanyName ;
end;

function TClientCompany.GetClientType: string;
begin
  result := 'Company' ;
end;

function TClientCompany.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid(pErrors);
  if CompanyName = '' then
    pErrors.AddError('CompanyName', cErrorClientCompanyName);
  result := pErrors.Count = 0;
end;

{ TClientPerson }

function TClientPerson.GetCaption: string;
begin
  result := NameTitle + ' ' + GivenName + ' ' + FamilyName ;
end;

function TClientPerson.GetClientType: string;
begin
  result := 'Person' ;
end;

function TClientPerson.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid(pErrors);
  if GivenName = '' then
    pErrors.AddError('GivenName', cErrorClientGivenName);
  if FamilyName = '' then
    pErrors.AddError('FamilyName', cErrorClientFamilyName);
  if NameTitle = '' then
    pErrors.AddError('NameTitle', cErrorClientNameTitle);
  result := pErrors.Count = 0;
end;

end.

