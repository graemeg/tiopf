unit tiOPFRemote_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST,
  tiDBProxyServer;

const
  cErrorCanNotLoadtiDBProxyServer = 'Can not load tiDBProxyServer'#13'' +
    'File location: %s'#13'' +
    'Error message: %s';

  // ToDo: cErrorFileNotFound should be a generic exception
  cErrorFileNotFound = 'File not found';

type

  TtiDBApplicationServerForTesting = class(TtiDBProxyServer)
  private
    FDatabaseName: string;
    FDefaultPersistenceLayer: string;
    FDefaultDatabase: string;
  public
    constructor Create(APort: integer); override;
    destructor  Destroy; override;
  end;

  TTestTIPersistenceLayersRemote = class(TTestTIPersistenceLayers)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseRemote = class(TTestTIDatabase)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  public
    class function PersistenceLayerName: string; override;
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  published
    procedure Transaction_TimeOut;
  end;

  TTestTIQueryRemote = class(TTestTIQuerySQL)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationRemote = class(TTestTIAutoMapOperation)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDRemote = class(TTestTIOIDPersistentGUID)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    procedure SetUp; override;
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerRemote = class(TTestTIOIDPersistentInteger)
  private
    FAppServer: TtiDBApplicationServerForTesting;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    procedure SetUp; override;
  public
    class function PersistenceLayerName: string; override;
  end;

procedure RegisterTests;

implementation

uses
  tiConstants,
  {$IFDEF FPC}
  tiFPCUnitUtils,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  SysUtils,
  tiTestDependencies,
  tiQuery,
  tiOPFTestCase,
  tiOPFTestManager,
  tiOPFManager;

const
  cRemoteServerMainFormName = 'TFormMainTIDBProxyServer';

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersRemote);
  tiRegisterPersistenceTest(TTestTIDatabaseRemote);
  tiRegisterPersistenceTest(TTestTIQueryRemote);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDRemote);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerRemote);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationRemote);
end;

constructor TtiDBApplicationServerForTesting.Create(APort: integer);
var
  LSetup: TtiOPFTestSetupData;
begin
  inherited Create(APort);
  LSetup:= GTIOPFTestManager.FindByPersistenceLayerName(cTIPersistIBX);
  Assert(LSetup <> nil, 'Remote persistence layer must be tested along with the IBX persistence layer');
  FDatabaseName:= LSetup.DBName;
  FDefaultPersistenceLayer:= GTIOPFManager.DefaultPersistenceLayerName;
  FDefaultDatabase:= GTIOPFManager.DefaultDBConnectionName;
  GTIOPFManager.DefaultPersistenceLayerName:= CTIPersistIBX;
  if not FileExists(LSetup.DBName) then
    GTIOPFManager.DefaultPerLayer.DatabaseClass.CreateDatabase(LSetup.DBName, LSetup.Username, LSetup.Password);
  GTIOPFManager.ConnectDatabase(LSetup.DBName, LSetup.Username, LSetup.Password, '', CTIPersistIBX);
  GTIOPFManager.DefaultDBConnectionName:= FDatabaseName;
  Start;
end;

destructor TtiDBApplicationServerForTesting.Destroy;
begin
  Stop;
  if GTIOPFManager.DefaultPerLayer.DBConnectionPools.IsConnected(FDatabaseName) then
    GTIOPFManager.DisconnectDatabase(FDatabaseName, CTIPersistIBX);
  GTIOPFManager.DefaultDBConnectionName:= FDefaultDatabase;
  GTIOPFManager.DefaultPersistenceLayerName:= FDefaultPersistenceLayer;
  inherited;
end;

{ TTestTIDatabaseRemote }

procedure TTestTIDatabaseRemote.CreateDatabase;
begin
  // Not supported in remote persistence layer
end;

procedure TTestTIDatabaseRemote.DatabaseExists;
begin
  // Not supported in remote persistence layer
end;

class function TTestTIDatabaseRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIDatabaseRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIDatabaseRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

procedure TTestTIDatabaseRemote.Transaction_TimeOut;
var
  LQuery: TtiQuery;
  LDatabase: TtiDatabase;
begin
  DropTable(cTableNameTestGroup);
  CreateTableTestGroup;
  InsertIntoTestGroup(1);

  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.StartTransaction;
    LQuery := LDatabase.CreateAndAttachTIQuery;
    try
      LDatabase.StartTransaction;
      LQuery.SelectRow(cTableNameTestGroup, nil);
      Check(not LQuery.EOF, 'Transaction not committed');
      LQuery.Next;
      Check(LQuery.EOF, 'Wrong number of records');
      Sleep(Trunc(cDBProxyServerTimeOut * 60000 * 1.5));
      try
        lDatabase.Commit;
        Fail('tiDBProxyServer did not time out as expected');
      except
        on e: Exception do
          Check(Pos('TIMED OUT', UpperCase(e.message)) <> 0,
            'tiDBProxyServer did not raise the right exception. Exception message: ' + e.message);
      end;
    finally
      LQuery.Free;
    end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
  DropTable(cTableNameTestGroup);
end;

{ TTestTIPersistenceLayersRemote }

class function TTestTIPersistenceLayersRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIPersistenceLayersRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIPersistenceLayersRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

{ TTestTIQueryRemote }

class function TTestTIQueryRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIQueryRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIQueryRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

{ TTestTIAutoMapOperationRemote }

class function TTestTIAutoMapOperationRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIAutoMapOperationRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIAutoMapOperationRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

{ TTestTIOIDPersistentGUIDRemote }

class function TTestTIOIDPersistentGUIDRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIOIDPersistentGUIDRemote.SetUp;
begin
  inherited;
  FRepeatCount:= 10;
  FNumThreads:= 3;
end;

procedure TTestTIOIDPersistentGUIDRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIOIDPersistentGUIDRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

{ TTestTIOIDPersistentIntegerRemote }

class function TTestTIOIDPersistentIntegerRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIOIDPersistentIntegerRemote.SetUp;
begin
  inherited;
  FRepeatCount:= 10;
  FNumThreads:= 3;
end;

procedure TTestTIOIDPersistentIntegerRemote.SetUpOnce;
begin
  FAppServer:= TtiDBApplicationServerForTesting.Create(80);
  inherited;
end;

procedure TTestTIOIDPersistentIntegerRemote.TearDownOnce;
begin
  inherited;
  FAppServer.Free;
end;

{ TtiDBApplicationServerForTesting }

end.
