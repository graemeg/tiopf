unit Client_BOM;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  tiObject
  ,tiOID
  ,tiOIDGUID
 ;

const
  cErrorClientName   = 'Please enter a client name';
  cErrorAdrsText     = 'Please enter an address';
  cErrorAdrsLocality = 'Please enter an address locality';
  cErrorAdrsState    = 'Please enter an address state';
  cErrorAdrsPostCode = 'Please enter a post code';

type
  { forward declarations }
  TClient  = class;
  TClients = class;
  TAdrs    = class;

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
    FClientName: string;
    FAdrs: TAdrs;
  protected
    procedure   AssignClassProps(pSource: TtiObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    constructor CreateNew(const pDatabaseName: string = ''; const pPerLayerName: string = ''); override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
    procedure   Read; override;
    procedure   Save; override;
  published
    property    ClientName: string read FClientName write FClientName;
    property    Adrs      : TAdrs read FAdrs;
  end;

  TAdrs = class(TtiObject)
  private
    FAdrsText: string;
    FLocality: string;
    FState  : string;
    FPostCode: string;
    function    GetAsOneLine: string;
  protected
    function    GetOID: TtiOID; override;
  public
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    AdrsText        : string     read FAdrsText write FAdrsText;
    property    Locality        : string     read FLocality write FLocality;
    property    State           : string     read FState    write FState;
    property    PostCode        : string     read FPostCode write FPostCode;
    property    AsOneLine       : string     read GetAsOneLine;
  end;


procedure RegisterMappings;


implementation
uses
  tiOPFManager
  ,tiAutoMap
  ,tiConstants
  ,Windows
  ,tiLog
  ,SysUtils
 ;


procedure RegisterMappings;
begin
  //                                              Class,    Table,    Property,     Column,       Special Info
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient,  'Client', 'OID',        'OID',        [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient,  'Client', 'ClientName', 'Client_Name'        );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs,    'Adrs',   'OID',        'OID',        [pktDB, pktFK]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs,    'Adrs',   'AdrsText',   'Adrs_Text');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs,    'Adrs',   'Locality',   'Locality');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs,    'Adrs',   'State',      'State');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs,    'Adrs',   'PostCode',   'Post_Code');
end;

{ TClient }

procedure TClient.AssignClassProps(pSource: TtiObject);
begin
  Adrs.Assign((pSource as TClient).Adrs);
end;


constructor TClient.Create;
begin
  inherited;
  FAdrs:= TAdrs.Create;
  FAdrs.Owner:= Self;
end;


constructor TClient.CreateNew(const pDatabaseName: string = ''; const pPerLayerName: string = '');
begin
  inherited;
  FAdrs.OIDGenerator.AssignNextOID(FAdrs.OID, pDatabaseName, pPerLayerName);
  FAdrs.ObjectState:= posCreate;
end;


destructor TClient.Destroy;
begin
  FAdrs.Free;
  inherited;
end;


function TClient.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if ClientName = '' then
    AErrors.AddError('ClientName', cErrorClientName);

  Adrs.IsValid(AErrors);

  result:= AErrors.Count = 0;
end;


procedure TClient.Read;
begin
  inherited;
end;

procedure TClient.Save;
begin
  inherited;
end;

{ TAdrs }

function TAdrs.GetAsOneLine: string;
begin
  result:=
    AdrsText + ', ' + Locality + ' ' +
    State + '  ' + PostCode;
end;


function TAdrs.GetOID: TtiOID;
begin
  Assert(Owner.TestValid, CTIErrorInvalidObject);
  result:= Owner.OID;
end;


function TAdrs.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  // Do not call inherited, because we do not want to clear AErrors
  if AdrsText = '' then
    AErrors.AddError('AdrsText', cErrorAdrsText);

  if Locality = '' then
    AErrors.AddError('Locality', cErrorAdrsLocality);

  if State = '' then
    AErrors.AddError('State', cErrorAdrsState);

  if PostCode = '' then
    AErrors.AddError('State', cErrorAdrsPostCode);

  result:= AErrors.Count = 0;
end;


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

end.




