unit tiQueryDatasnapDCOM;

{$I tiDefines.inc}

interface

uses
  tiQuery,
  DBClient,
  MConnect,
  tiQueryDatasnap,
  tiPersistenceLayers;

type

  TtiPersistenceLayerDatasnapDCOM = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  // Databasename is interpreted in the form ServerName[:ProviderName][@machinename]
  // Providername is optional, defaults to DSPTiOPF
  // Machinename is optional, empty means current machine.
  // Servername is the DCOMConnection servername property.

  TtiDatabaseDatasnapDCOM = class(TtiDatabaseDatasnap)
  private
    FConnection: TDCOMCOnnection;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
    function GetConnection: TDCOMCOnnection;
  protected
    procedure SetConnected(AValue: Boolean); override;
    function GetConnected: Boolean; override;
    function GetRemoteServer: TCustomRemoteServer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Connection: TDCOMCOnnection read FConnection;
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
  CtiPersistDatasnapDCOM = 'Datasnap_DCOM';

implementation

uses
  SysUtils,
  tiOPFManager;

{ TtiPersistenceLayerDatasnapDCOM }

procedure TtiPersistenceLayerDatasnapDCOM.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  APersistenceLayerDefaults.PersistenceLayerName := CtiPersistDatasnapDCOM;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
  APersistenceLayerDefaults.DatabaseName := 'TIOPFSERVER';
  APersistenceLayerDefaults.UserName := '';
  APersistenceLayerDefaults.Password := '';
end;

function TtiPersistenceLayerDatasnapDCOM.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseDatasnapDCOM;
end;

function TtiPersistenceLayerDatasnapDCOM.GetPersistenceLayerName: string;
begin
  Result := CtiPersistDatasnapDCOM;
end;

function TtiPersistenceLayerDatasnapDCOM.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDatasnap;
end;

{ TtiDatabaseDatasnapDCOM }

procedure TtiDatabaseDatasnapDCOM.Commit;
begin
  // None
end;

constructor TtiDatabaseDatasnapDCOM.Create;
begin
  FConnection  := TDCOMConnection.Create(nil);
  ProviderName := 'DSPTiOPF';
end;

class procedure TtiDatabaseDatasnapDCOM.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDatasnapDCOM.DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean;
begin
  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseDatasnapDCOM.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class procedure TtiDatabaseDatasnapDCOM.DropDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'Drop Database not implemented in ' + ClassName);
end;

function TtiDatabaseDatasnapDCOM.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
end;

function TtiDatabaseDatasnapDCOM.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TtiDatabaseDatasnapDCOM.GetConnection: TDCOMCOnnection;
begin
  Result := TDCOMConnection(RemoteServer);
end;

function TtiDatabaseDatasnapDCOM.GetRemoteServer: TCustomRemoteServer;
begin
  Result := FConnection;
end;

function TtiDatabaseDatasnapDCOM.InTransaction: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseDatasnapDCOM.RollBack;
begin
  // No transaction support
end;

procedure TtiDatabaseDatasnapDCOM.SetConnected(AValue: Boolean);
var
  P: integer;
  S, M, PN: string;
begin
  if AValue then
  begin
    M := self.DatabaseName;
    P := Pos('@', M);
    if (P = 0) then
      P := Length(M) + 1;
    PN := Copy(M, 1, P - 1);
    Delete(M, 1, P);
    P  := Pos(':', PN);
    if (P = 0) then
      P := Length(PN) + 1;
    S := Copy(PN, 1, P);
    Delete(PN, 1, P);
    FConnection.ComputerName := M;
    FConnection.ServerName := S;
    if (PN <> '') then
      ProviderName := PN;
  end;
  FConnection.Connected := AValue;
end;

procedure TtiDatabaseDatasnapDCOM.StartTransaction;
begin
  // No transaction support
end;

function TtiDatabaseDatasnapDCOM.Test: Boolean;
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

function TtiDatabaseDatasnapDCOM.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDataSnap;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDatasnapDCOM);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(CtiPersistDatasnapDCOM);

end.

