unit Client_BOM;

interface
uses
  tiObject
 ;

const
  cErrorClientNameMissing = 'Please enter a client name';
  cErrorClientIDMissing = 'Please enter a client ID';

type

  TClient = class;
  TClients = class;

  TClients = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TClient; reintroduce;
    procedure   SetItems(i: integer; const Value: TClient); reintroduce;
  public
    procedure   Read; override;
    procedure   Save; override;
    property    Items[i:integer]: TClient read GetItems write SetItems;
    procedure   Add(AObject: TClient); reintroduce;
  published
  end;

  TClient = class(TtiObject)
  private
    FClientID: string;
    FClientName: string;
  protected
    function    GetOwner: TClients; reintroduce;
    procedure   SetOwner(const Value: TClients); reintroduce;
  public
    property    Owner      : TClients             read GetOwner      write SetOwner;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    ClientName: string read FClientName write FClientName;
    property    ClientID  : string read FClientID write FClientID;
  end;

procedure RegisterMappings;

implementation
uses
  tiOPFManager
  ,tiAutoMap
 ;

procedure RegisterMappings;
begin
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID', 'Client_ID');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end;

{ TClients }

procedure TClients.Add(AObject: TClient);
begin
  inherited Add(AObject);
end;

function TClients.GetItems(i: integer): TClient;
begin
  result:= TClient(inherited GetItems(i));
end;

procedure TClients.Read;
begin
  inherited;
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

procedure TClient.SetOwner(const Value: TClients);
begin
  inherited SetOwner(Value);
end;

end.




