unit tiQueryDatasnapSocket;

{$I tiDefines.inc}

interface

uses
  tiQuery,
  DBClient,
  SConnect,
  tiQueryDatasnap,
  tiPersistenceLayers;

type
  TtiPersistenceLayerDatasnapSocket = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  // Databasename is interpreted in the form ServerName[|PortNumber][:ProviderName][@machinename]
  // Providername is optional, defaults to DSPTiOPF
  // Machinename is optional, empty means current machine.

  TtiDatabaseDatasnapSocket = class(TtiDatabaseDatasnap)
  private
    FConnection: TSocketCOnnection;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
    function GetConnection: TSocketCOnnection;
  protected
    procedure SetConnected(AValue: Boolean); override;
    function GetConnected: Boolean; override;
    function GetRemoteServer: TCustomRemoteServer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Connection: TSocketCOnnection read FConnection;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean; override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string); override;
    procedure StartTransaction; override;
    function InTransaction: Boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    function Test: Boolean; override;
    function TIQueryClass: TtiQueryClass; override;
  end;


const
  CtiPersistDatasnapSocket = 'Datasnap_Socket';


implementation

uses
  SysUtils,
  tiOPFManager;

{ TtiPersistenceLayerDatasnapSocket }

procedure TtiPersistenceLayerDatasnapSocket.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  APersistenceLayerDefaults.PersistenceLayerName := CtiPersistDatasnapSocket;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
  APersistenceLayerDefaults.DatabaseName := 'TIOPFSERVER';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.UserName := '';
  APersistenceLayerDefaults.Password := '';
end;

function TtiPersistenceLayerDatasnapSocket.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseDatasnapSocket;
end;

function TtiPersistenceLayerDatasnapSocket.GetPersistenceLayerName: string;
begin
  Result := CtiPersistDatasnapSocket;
end;

function TtiPersistenceLayerDatasnapSocket.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDatasnap;
end;


{ TtiDatabaseDatasnapSocket }

procedure TtiDatabaseDatasnapSocket.Commit;
begin
  // None
end;

constructor TtiDatabaseDatasnapSocket.Create;
begin
  FConnection  := TSocketConnection.Create(nil);
  ProviderName := 'DSPTiOPF';
end;

class procedure TtiDatabaseDatasnapSocket.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDatasnapSocket.DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean;
begin
  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseDatasnapSocket.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class procedure TtiDatabaseDatasnapSocket.DropDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'Drop Database not implemented in ' + ClassName);
end;

function TtiDatabaseDatasnapSocket.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
  // nothing
end;

function TtiDatabaseDatasnapSocket.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TtiDatabaseDatasnapSocket.GetConnection: TSocketCOnnection;
begin
  Result := TSocketConnection(RemoteServer);
end;

function TtiDatabaseDatasnapSocket.GetRemoteServer: TCustomRemoteServer;
begin
  Result := FConnection;
end;

function TtiDatabaseDatasnapSocket.InTransaction: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseDatasnapSocket.RollBack;
begin
  // No transaction support
end;

procedure TtiDatabaseDatasnapSocket.SetConnected(AValue: Boolean);
var
  P: integer;
  S, M, PN, Po: string;
  NR: integer;
begin
  if AValue then
  begin
    // Hostname
    M := self.DatabaseName;
    P := Pos('@', M);
    if (P = 0) then
      P := Length(M) + 1;
    PN := Copy(M, 1, P - 1);
    Delete(M, 1, P);
    if (M = '') then
      M := 'LocalHost';
    // Providername
    P := Pos(':', PN);
    if (P = 0) then
      P := Length(PN) + 1;
    PO := Copy(PN, 1, P);
    Delete(PN, 1, P);
    // Portnumber/Servername
    P := Pos('|', PO);
    if (P = 0) then
      P := Length(PO) + 1;
    S := Copy(PO, 1, P);
    Delete(PO, 1, P);
    NR := StrToIntDef(Po, 211);
    FConnection.Host       := M;
    FConnection.Port       := NR;
    FConnection.ServerName := S;
    if (PN <> '') then
      ProviderName := PN;
  end;
  FConnection.Connected := AValue;
end;

procedure TtiDatabaseDatasnapSocket.StartTransaction;
begin
  // No transaction support
end;

function TtiDatabaseDatasnapSocket.Test: Boolean;
var
  S: string;
begin
  try
    FConnection.Connected := True;
    S      := FConnection.GetServer.AS_GetProviderNames;
    Result := True;
    FConnection.Connected := False;
  except
    Result := False;
  end;
end;

function TtiDatabaseDatasnapSocket.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDataSnap;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDatasnapSocket);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(CtiPersistDatasnapSocket);

end.

