unit tiQueryDatasnapSoap;

{$I tiDefines.inc}

interface

uses
  tiQuery,
  DBClient,
  SoapConn,
  tiQueryDatasnap,
  tiPersistenceLayers;

type

  TtiPersistenceLayerDatasnapSoap = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  // Databasename is interpreted in the form [([ProviderName@]SoapServerIID)]URL
  // Providername is optional, defaults to DSPTiOPF
  // SoapServerIID is optional, defaults to 'IAppServerSOAP - {C99F4735-D6D2-495C-8CA2-E53E5A439E61}'
  // Machinename is required.

  TtiDatabaseDatasnapSoap = class(TtiDatabaseDatasnap)
  private
    FConnection: TSoapCOnnection;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
    function GetConnection: TSoapCOnnection;
  protected
    procedure SetConnected(AValue: Boolean); override;
    function GetConnected: Boolean; override;
    function GetRemoteServer: TCustomRemoteServer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Connection: TSoapCOnnection read FConnection;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string=''); override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string=''): Boolean; override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string=''); override;
    procedure StartTransaction; override;
    function InTransaction: Boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    function Test: Boolean; override;
    function TIQueryClass: TtiQueryClass; override;
  end;

const
  CtiPersistDatasnapSoap = 'Datasnap_Soap';

implementation

uses
  SysUtils,
  tiOPFManager;

{ TtiPersistenceLayerDatasnapSoap }

procedure TtiPersistenceLayerDatasnapSoap.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  APersistenceLayerDefaults.PersistenceLayerName := CtiPersistDatasnapSoap;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
  APersistenceLayerDefaults.DatabaseName := 'TIOPFSERVER';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.UserName := '';
  APersistenceLayerDefaults.Password := '';
end;

function TtiPersistenceLayerDatasnapSoap.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseDatasnapSoap;
end;

function TtiPersistenceLayerDatasnapSoap.GetPersistenceLayerName: string;
begin
  Result := CtiPersistDatasnapSoap;
end;

function TtiPersistenceLayerDatasnapSoap.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDatasnap;
end;

{ TtiDatabaseDatasnapSoap }

procedure TtiDatabaseDatasnapSoap.Commit;
begin
  // None
end;

constructor TtiDatabaseDatasnapSoap.Create;
begin
  FConnection  := TSoapConnection.Create(nil);
  ProviderName := 'DSPTiOPF';
end;

class procedure TtiDatabaseDatasnapSoap.CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string='');
begin
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDatasnapSoap.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string=''): Boolean;
begin
  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseDatasnapSoap.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class procedure TtiDatabaseDatasnapSoap.DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string='');
begin
  Assert(False, 'Drop Database not implemented in ' + ClassName);
end;

function TtiDatabaseDatasnapSoap.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
end;

function TtiDatabaseDatasnapSoap.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TtiDatabaseDatasnapSoap.GetConnection: TSoapCOnnection;
begin
  Result := TSoapConnection(RemoteServer);
end;

function TtiDatabaseDatasnapSoap.GetRemoteServer: TCustomRemoteServer;
begin
  Result := FConnection;
end;

function TtiDatabaseDatasnapSoap.InTransaction: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseDatasnapSoap.RollBack;
begin
  // No transaction support
end;

function GetNextWord(var S: string; Sep: char): string;
var
  P: integer;
begin
  P := Pos(Sep, S);
  if (P = 0) then
    P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
  Delete(S, 1, P);
end;


procedure TtiDatabaseDatasnapSoap.SetConnected(AValue: Boolean);
var
  S, SN, PN: string;
begin
  if AValue then
  begin
    S := DatabaseName;
    if Pos('(', S) <> 0 then
    begin
      Delete(S, 1, 1);
      SN := GetNextWord(S, ')');
      if Pos('@', SN) <> 0 then
        PN := GetNextWord(SN, '@');
    end
    else
    begin
      PN := '';
      SN := 'IAppServerSOAP - {C99F4735-D6D2-495C-8CA2-E53E5A439E61}';
    end;
    FConnection.URL := S;
    FConnection.SoapServerIID := SN;
    FConnection.UserName      := UserName;
    FConnection.Password      := Password;
    if (PN <> '') then
      ProviderName := PN;
  end;
  FConnection.Connected := AValue;
end;

procedure TtiDatabaseDatasnapSoap.StartTransaction;
begin
  // No transaction support
end;

function TtiDatabaseDatasnapSoap.Test: Boolean;
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

function TtiDatabaseDatasnapSoap.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDataSnap;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDatasnapSoap);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(CtiPersistDatasnapSoap);

end.

