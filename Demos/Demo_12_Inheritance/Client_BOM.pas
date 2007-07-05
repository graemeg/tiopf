unit Client_BOM;

interface
uses
   tiObject
  ,tiOIDGUID
  ,Classes
 ;

const
  cErrorClientNameMissing = 'Please enter a client name';
  cErrorClientIDMissing   = 'Please enter a client ID';
  cErrorClientCompanyName = 'Please enter a company name';
  cErrorClientFamilyName  = 'Please enter a family name';
  cErrorClientGivenName   = 'Please enter a given name';
type

  TClientAbs = class;
  TClients = class;

  TClients = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TClientAbs; reintroduce;
    procedure   SetItems(i: integer; const Value: TClientAbs); reintroduce;
  public
    property    Items[i:integer]: TClientAbs read GetItems write SetItems;
    procedure   Add(AObject: TClientAbs); reintroduce;
    procedure   Clear; override;
    procedure   Read; override;
  published
  end;

  TClientAbs = class(TtiObject)
  private
    FClientID: string;
  protected
    function    GetOwner: TClients; reintroduce;
    procedure   SetOwner(const Value: TClients); reintroduce;
    function    GetClientType: string; virtual; abstract;
  public
    property    Owner      : TClients             read GetOwner      write SetOwner;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    ClientID  : string   read FClientID   write FClientID;
    property    ClientType: string read GetClientType;
  end;

  TClientCompany = class(TClientAbs)
  private
    FCompanyName: string;
  protected
    function    GetCaption: string; override;
    function    GetClientType: string; override;
  public
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property CompanyName: string read FCompanyName write FCompanyName;
  end;

  TClientPerson = class(TClientAbs)
  private
    FFamilyName: string;
    FGivenName: string;
  protected
    function    GetCaption: string; override;
    function    GetClientType: string; override;
  public
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property GivenName    : string  read FGivenName write FGivenName;
    property FamilyName   : string read FFamilyName write FFamilyName;
  end;

implementation
uses
  tiOPFManager
  ,tiAutoMap
  ,Windows
  ,tiLog
  ,SysUtils
 ;

{ TClients }

procedure TClients.Add(AObject: TClientAbs);
begin
  inherited Add(AObject);
end;

procedure TClients.Clear;
begin
  inherited;
  ObjectState:= posEmpty;
end;

function TClients.GetItems(i: integer): TClientAbs;
begin
  result:= TClientAbs(inherited GetItems(i));
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

procedure TClients.SetItems(i: integer; const Value: TClientAbs);
begin
  inherited SetItems(i, Value);
end;

{ TClient }


function TClientAbs.GetOwner: TClients;
begin
  result:= TClients(inherited GetOwner);
end;

function TClientAbs.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);

  if ClientID = '' then
    AErrors.AddError('ClientID', cErrorClientIDMissing);

  result:= AErrors.Count = 0;

end;

procedure TClientAbs.SetOwner(const Value: TClients);
begin
  inherited SetOwner(Value);
end;

{ TClientCompany }

function TClientCompany.GetCaption: string;
begin
  result:= 'Company name: "' + CompanyName + '"';
end;

function TClientCompany.GetClientType: string;
begin
  result:= 'Company';
end;

function TClientCompany.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if CompanyName = '' then
    AErrors.AddError('CompanyName', cErrorClientCompanyName);
  result:= AErrors.Count = 0;
end;

{ TClientPerson }

function TClientPerson.GetCaption: string;
begin
  result:=
    'Given name: "' + GivenName +
    '" Family name: "' + FamilyName + '"';
end;

function TClientPerson.GetClientType: string;
begin
  result:= 'Person';
end;

function TClientPerson.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if GivenName = '' then
    AErrors.AddError('GivenName', cErrorClientGivenName);
  if FamilyName = '' then
    AErrors.AddError('FamilyName', cErrorClientFamilyName);
  result:= AErrors.Count = 0;
end;

end.






