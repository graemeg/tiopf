unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ,tiPerObjOIDAbs
  ;

const
  cErrorClientNameMissing = 'Please enter a client name' ;
  cErrorClientIDMissing = 'Please enter a client ID' ;
  cErrorPhoneNumberTypeMissing = 'Please enter a phone number type' ;
  cErrorPhoneNumberTextMissing = 'Please enter the phone number text' ;
type

  TClient = class ;
  TClients = class ;
  TPhoneNumbers = class ;
  TPhoneNumber  = class ;

  TClientName      = String[200];
  TClientID        = String[9];
  TPhoneNumberType = String[20];
  TPhoneNumberText = string[19];

  TClients = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TClient ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce ;
  public
    property    Items[i:integer] : TClient read GetItems write SetItems ;
    procedure   Add( pObject : TClient   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    procedure   Clear ; override ;
    procedure   Read ; override ;
  published
  end ;

  TClient = class( TPerObjAbs )
  private
    FClientID : TClientID ;
    FClientName: TClientName;
    FPhoneNumbers: TPhoneNumbers;
  protected
    function    GetOwner: TClients; reintroduce ;
    procedure   SetOwner(const Value: TClients ); reintroduce ;
  public

    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TClients             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;

    // Explain this...
    procedure   AssignClassProps(pSource: TPerObjAbs); override ;

  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    ClientID   : TClientID   read FClientID   write FClientID ;
    property    PhoneNumbers : TPhoneNumbers read FPhoneNumbers ;
  end ;

  TPhoneNumbers = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TPhoneNumber ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPhoneNumber); reintroduce ;
    function    GetOwner: TClient; reintroduce ;
    procedure   SetOwner(const Value: TClient); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    property    Items[i:integer] : TPhoneNumber read GetItems write SetItems ;
    procedure   Add( pObject : TPhoneNumber   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TClient read GetOwner      write SetOwner ;
  published
  end ;

  TPhoneNumber = class( TPerObjAbs )
  private
    FNumberText: TPhoneNumberText;
    FNumberType: TPhoneNumberType;
  protected
    function    GetOwner: TPhoneNumbers; reintroduce ;
    procedure   SetOwner(const Value: TPhoneNumbers ); reintroduce ;
  public
    property    Owner       : TPhoneNumbers             read GetOwner      write SetOwner ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    NumberType : TPhoneNumberType read FNumberType write FNumberType ;
    property    NumberText : TPhoneNumberText read FNumberText write FNumberText ;
  end ;


procedure RegisterMappings ;

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
  //                                          Class,   Table,    Property,     Column
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID', [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'  );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'    ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClient);

  //                                          Class,        Table,          Property,     Column
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'OID',        'OID',        [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'Owner.OID',  'Client_OID', [pktFK] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberType', 'Number_Type'         );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberText', 'Number_Text'         ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TPhoneNumbers, TPhoneNumber);

end ;

{ TClients }

procedure TClients.Add(pObject: TClient; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

procedure TClients.Clear;
begin
  inherited;
  ObjectState := posEmpty ;
end;

function TClients.GetItems(i: integer): TClient;
begin
  result := TClient( inherited GetItems( i )) ;
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

procedure TClients.SetItems(i: integer; const Value: TClient);
begin
  inherited SetItems( i, Value ) ;
end;

{ TClient }

procedure TClient.AssignClassProps(pSource: TPerObjAbs);
begin
 PhoneNumbers.Assign(TClient(pSource).PhoneNumbers);
end;

constructor TClient.Create;
begin
  inherited;
  FPhoneNumbers := TPhoneNumbers.Create;
  FPhoneNumbers.Owner := Self ;
end;

destructor TClient.Destroy;
begin
  FPhoneNumbers.Free;
  inherited;
end;

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

{ TPhoneNumbers }

procedure TPhoneNumbers.Add(pObject: TPhoneNumber; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TPhoneNumbers.GetItems(i: integer): TPhoneNumber;
begin
  result := TPhoneNumber( inherited GetItems( i )) ;
end;

function TPhoneNumbers.GetOID: TOID;
begin
  result := Owner.OID ;
end;

function TPhoneNumbers.GetOwner: TClient;
begin
  result := TClient( inherited GetOwner );
end;

procedure TPhoneNumbers.SetItems(i: integer; const Value: TPhoneNumber);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TPhoneNumbers.SetOwner(const Value: TClient);
begin
  inherited SetOwner( Value ) ;
end;

{ TPhoneNumber }

function TPhoneNumber.GetOwner: TPhoneNumbers;
begin
  result := TPhoneNumbers( inherited GetOwner );
end;

function TPhoneNumber.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if NumberType = '' then
    pErrors.AddError( 'NumberType', cErrorPhoneNumberTypeMissing) ;

  if NumberText = '' then
    pErrors.AddError( 'NumberText', cErrorPhoneNumberTextMissing ) ;

  result := pErrors.Count = 0 ;
end;

procedure TPhoneNumber.SetOwner(const Value: TPhoneNumbers);
begin
  inherited SetOwner( Value ) ;
end;

end.

