unit Client_BOM;

interface
uses
  tiObject
 ;

type

  TClient = class;
  TClients = class;

  TClients = class(TtiObjectList)
  public
    procedure Read; override;
    procedure Save; override;
  end;

  TClient = class(TtiObject)
  private
    FClientID: string;
    FClientName: string;
  public
    constructor CreateNew(const pDatabaseName: string = ''; const pPersistenceLayerName: string = ''); override;
    procedure   Save; override;
  published
    property    ClientName: string read FClientName write FClientName;
    property    ClientID  : string read FClientID write FClientID;
  end;

implementation
uses
   tiOPFManager
  ,SysUtils
  ,Windows
 ;

{ TClient }

constructor TClient.CreateNew(const pDatabaseName: string = ''; const pPersistenceLayerName: string = '');
begin
  inherited;
  // Set some default values for the demo
  ClientName:= 'TEST ' + DateTimeToStr(Now);
  ClientID:= IntToStr(GetTickCount);
end;

procedure TClient.Save;
begin
  inherited;
end;

{ TClients }

procedure TClients.Read;
begin
  inherited;
end;

procedure TClients.Save;
begin
  inherited;
end;

end.

