unit tiQueryDatasnapWeb;

{$I tiDefines.inc}

interface

uses
  tiQuery,
  DBClient,
  SConnect,
  tiQueryDatasnap,
  tiPersistenceLayers;

type
  TtiPersistenceLayerDatasnapWeb = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  // Databasename is interpreted in the form ([ProviderName@]ServerName)URL
  // Providername is optional, defaults to DSPTiOPF
  // Machinename is optional, empty means current machine.

  TtiDatabaseDatasnapWeb = class(TtiDatabaseDatasnap)
  private
    FConnection: TWebCOnnection;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
    function GetConnection: TWebCOnnection;
  protected
    procedure SetConnected(AValue: Boolean); override;
    function GetConnected: Boolean; override;
    function GetRemoteServer: TCustomRemoteServer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Connection: TWebCOnnection read FConnection;
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
  CtiPersistDatasnapWeb = 'Datasnap_Web';

implementation

uses
  SysUtils,
  tiExcept,
  tiOPFManager;

resourcestring
  sErrNoServerName = 'No server name specified';

{ TtiPersistenceLayerDatasnapWeb }

procedure TtiPersistenceLayerDatasnapWeb.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  APersistenceLayerDefaults.PersistenceLayerName := CtiPersistDatasnapWeb;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
  APersistenceLayerDefaults.DatabaseName := 'TIOPFSERVER';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.UserName := '';
  APersistenceLayerDefaults.Password := '';
end;

function TtiPersistenceLayerDatasnapWeb.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseDatasnapWeb;
end;

function TtiPersistenceLayerDatasnapWeb.GetPersistenceLayerName: string;
begin
  Result := CtiPersistDatasnapWeb;
end;

function TtiPersistenceLayerDatasnapWeb.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDatasnap;
end;

{ TtiDatabaseDatasnapWeb }

procedure TtiDatabaseDatasnapWeb.Commit;
begin
  // None
end;

constructor TtiDatabaseDatasnapWeb.Create;
begin
  FConnection  := TWebConnection.Create(nil);
  ProviderName := 'DSPTiOPF';
end;

class procedure TtiDatabaseDatasnapWeb.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDatasnapWeb.DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean;
begin
  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseDatasnapWeb.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class procedure TtiDatabaseDatasnapWeb.DropDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(False, 'Drop Database not implemented in ' + ClassName);
end;

function TtiDatabaseDatasnapWeb.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
end;

function TtiDatabaseDatasnapWeb.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TtiDatabaseDatasnapWeb.GetConnection: TWebCOnnection;
begin
  Result := TWebConnection(RemoteServer);
end;

function TtiDatabaseDatasnapWeb.GetRemoteServer: TCustomRemoteServer;
begin
  Result := FConnection;
end;

function TtiDatabaseDatasnapWeb.InTransaction: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseDatasnapWeb.RollBack;
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

procedure TtiDatabaseDatasnapWeb.SetConnected(AValue: Boolean);
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
      raise EtiOPFProgrammerException.Create(sErrNoServerName);
    FConnection.URL        := S;
    FConnection.ServerName := SN;
    FConnection.UserName   := UserName;
    FConnection.Password   := Password;
    if (PN <> '') then
      ProviderName := PN;
  end;
  FConnection.Connected := AValue;
end;

procedure TtiDatabaseDatasnapWeb.StartTransaction;
begin
  // No transaction support
end;

function TtiDatabaseDatasnapWeb.Test: Boolean;
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

function TtiDatabaseDatasnapWeb.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQueryDataSnap;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDatasnapWeb);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(CtiPersistDatasnapWeb);

end.

