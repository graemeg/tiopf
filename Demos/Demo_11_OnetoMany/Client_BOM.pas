unit Client_BOM;

interface
uses
   tiObject
  ,tiOID
  ,tiOIDGUID // To force linking GUID OIDs. Must be included in application at least once.
 ;

const
  cErrorClientNameMissing = 'Please enter a client name';
  cErrorClientIDMissing = 'Please enter a client ID';
  cErrorPhoneNumberTypeMissing = 'Please enter a phone number type';
  cErrorPhoneNumberTextMissing = 'Please enter the phone number text';
type

  TClient = class;
  TClients = class;
  TPhoneNumbers = class;
  TPhoneNumber  = class;

  TClients = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TClient; reintroduce;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce;
  public
    property    Items[i:integer]: TClient read GetItems write SetItems;
    procedure   Add(AObject: TClient); reintroduce;
    procedure   Clear; override;
    procedure   Read; override;
    procedure   Save; override;
  end;

  TClient = class(TtiObject)
  private
    FClientID: string;
    FClientName: string;
    FPhoneNumbers: TPhoneNumbers;
  protected
    function    GetOwner: TClients; reintroduce;
    procedure   SetOwner(const Value: TClients); reintroduce;
    procedure   SetObjectState(const AValue: TPerObjectState); override;
  public

    constructor Create; override;
    destructor  Destroy; override;
    property    Owner      : TClients             read GetOwner      write SetOwner;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;

    // Explain this...
    procedure   AssignClassProps(pSource: TtiObject); override;

  published
    property    ClientName: string read FClientName write FClientName;
    property    ClientID  : string   read FClientID   write FClientID;
    property    PhoneNumbers: TPhoneNumbers read FPhoneNumbers;
  end;

  TPhoneNumbers = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TPhoneNumber; reintroduce;
    procedure   SetItems(i: integer; const Value: TPhoneNumber); reintroduce;
    function    GetOwner: TClient; reintroduce;
    procedure   SetOwner(const Value: TClient); reintroduce;
    function    GetOID: TtiOID; override;
  public
    property    Items[i:integer]: TPhoneNumber read GetItems write SetItems;
    procedure   Add(AObject: TPhoneNumber); reintroduce;
    property    Owner: TClient read GetOwner      write SetOwner;
  published
  end;

  TPhoneNumber = class(TtiObject)
  private
    FNumberText: string;
    FNumberType: string;
  protected
    function    GetOwner: TPhoneNumbers; reintroduce;
    procedure   SetOwner(const Value: TPhoneNumbers); reintroduce;
  public
    property    Owner      : TPhoneNumbers             read GetOwner      write SetOwner;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    NumberType: string read FNumberType write FNumberType;
    property    NumberText: string read FNumberText write FNumberText;
  end;

implementation
uses
   tiOPFManager
  ,tiAutoMap
  ,tiConstants
  ,Windows
  ,tiLog
  ,SysUtils
 ;

{ TClients }

procedure TClients.Add(AObject: TClient);
begin
  inherited Add(AObject);
end;

procedure TClients.Clear;
begin
  inherited;
  ObjectState:= posEmpty;
end;

function TClients.GetItems(i: integer): TClient;
begin
  result:= TClient(inherited GetItems(i));
end;

procedure TClients.Read;
var
  lNow: DWord;
begin
  lNow:= GetTickCount;
  inherited;
  Log('Time to load ' + IntToStr(Count) + ' Clients: ' +
      IntToStr(GetTickCount - lNow));
end;

procedure TClients.Save;
begin
  inherited;
end;

procedure TClients.SetItems(i: integer; const Value: TClient);
begin
  inherited SetItems(i, Value);
end;

{ TClient }

procedure TClient.AssignClassProps(pSource: TtiObject);
begin
 PhoneNumbers.Assign(TClient(pSource).PhoneNumbers);
end;

constructor TClient.Create;
begin
  inherited;
  FPhoneNumbers:= TPhoneNumbers.Create;
  FPhoneNumbers.Owner:= Self;
end;

destructor TClient.Destroy;
begin
  FPhoneNumbers.Free;
  inherited;
end;

function TClient.GetOwner: TClients;
begin
  result:= TClients(inherited GetOwner);
end;

function TClient.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);

  if ClientName = '' then
    AErrors.AddError('ClientName', cErrorClientNameMissing);

  if ClientID = '' then
    AErrors.AddError('ClientID', cErrorClientIDMissing);

  result:= AErrors.Count = 0;

end;

procedure TClient.SetObjectState(const AValue: TPerObjectState);
begin
  inherited;
  Assert(PhoneNumbers.TestValid, CTIErrorInvalidObject);
  // ToDo. This call should not be requred. There should be a flat on TPhoneNumbers
  //       indicating it's not persisted.
  if (AValue = posDeleted) and (PhoneNumbers.ObjectState = posDelete) then
    PhoneNumbers.ObjectState:= posDeleted;
end;

procedure TClient.SetOwner(const Value: TClients);
begin
  inherited SetOwner(Value);
end;

{ TPhoneNumbers }

procedure TPhoneNumbers.Add(AObject: TPhoneNumber);
begin
  inherited Add(AObject);
end;

function TPhoneNumbers.GetItems(i: integer): TPhoneNumber;
begin
  result:= TPhoneNumber(inherited GetItems(i));
end;

function TPhoneNumbers.GetOID: TtiOID;
begin
  result:= Owner.OID;
end;

function TPhoneNumbers.GetOwner: TClient;
begin
  result:= TClient(inherited GetOwner);
end;

procedure TPhoneNumbers.SetItems(i: integer; const Value: TPhoneNumber);
begin
  inherited SetItems(i, Value);
end;

procedure TPhoneNumbers.SetOwner(const Value: TClient);
begin
  inherited SetOwner(Value);
end;

{ TPhoneNumber }

function TPhoneNumber.GetOwner: TPhoneNumbers;
begin
  result:= TPhoneNumbers(inherited GetOwner);
end;

function TPhoneNumber.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);

  if NumberType = '' then
    AErrors.AddError('NumberType', cErrorPhoneNumberTypeMissing);

  if NumberText = '' then
    AErrors.AddError('NumberText', cErrorPhoneNumberTextMissing);

  result:= AErrors.Count = 0;
end;

procedure TPhoneNumber.SetOwner(const Value: TPhoneNumbers);
begin
  inherited SetOwner(Value);
end;

end.




