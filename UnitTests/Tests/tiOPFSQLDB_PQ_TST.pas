unit tiOPFSQLDB_PQ_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiAutoMap_TST
  ,tiOID_TST
  ;


type

  TTestTIPersistenceLayersSQLDB_PQ = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure   ThreadedDBConnectionPool; override;
  end;
  

  TTestTIDatabaseSQLDB_PQ = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
    procedure DatabaseExists; override;
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIQuerySQLDB_PQ = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationSQLDB_PQ = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentGUIDSQLDB_PQ = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
  end;


  TTestTIOIDPersistentIntegerSQLDB_PQ = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;



procedure RegisterTests;


implementation
uses
  tiConstants
  {$IFDEF FPC}
  ,tiFPCUnitUtils
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ,tiOPFTestManager
  ,SysUtils
  ,tiUtils
  ,tiTestDependencies
  ;
  
procedure RegisterTests ;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersSQLDB_PQ);
  tiRegisterPersistenceTest(TTestTIDatabaseSQLDB_PQ);
  tiRegisterPersistenceTest(TTestTIQuerySQLDB_PQ);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDSQLDB_PQ);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerSQLDB_PQ);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationSQLDB_PQ);
end ;

{ TTestTIDatabaseSQLDB_PQ }

procedure TTestTIDatabaseSQLDB_PQ.CreateDatabase;
var
  LDB:       string;
  LDBExists: boolean;
begin
  LDB := TestSetupData.DBName + 'tmp';
  PersistenceLayer.DatabaseClass.CreateDatabase(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);

  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);

  CheckTrue(LDBExists, 'Failed on 1');
  PersistenceLayer.DatabaseClass.DropDatabase(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);
end;

class function TTestTIDatabaseSQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

procedure TTestTIDatabaseSQLDB_PQ.DatabaseExists;
var
  LDBExists: boolean;
begin
  // DB should exist
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password);
  CheckTrue(LDBExists, 'Failed on 1');
  // DB should not exist
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName + 'Tmp',
    TestSetupData.Username,
    TestSetupData.Password);
  CheckFalse(LDBExists, 'Failed on 2');
end;

{ TTestTIPersistenceLayersSQLDB_PQ }

class function TTestTIPersistenceLayersSQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

procedure TTestTIPersistenceLayersSQLDB_PQ.ThreadedDBConnectionPool;
begin
  {$IFDEF FPC}
  Fail('This freezes up under FPC compiler.');
  {$ELSE}
  inherited ThreadedDBConnectionPool;
  {$ENDIF}
end;

{ TTestTIQuerySQLDB_PQ }

class function TTestTIQuerySQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

{ TTestTIAutoMapOperationSQLDB_PQ }

class function TTestTIAutoMapOperationSQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

{ TTestTIOIDPersistentGUIDSQLDB_PQ }

class function TTestTIOIDPersistentGUIDSQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

procedure TTestTIOIDPersistentGUIDSQLDB_PQ.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
  {$IFDEF FPC}
  Fail('This freezes up under FPC compiler.');
  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

{ TTestTIOIDPersistentIntegerSQLDB_PQ }

class function TTestTIOIDPersistentIntegerSQLDB_PQ.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbPQ;
end;

end.
