unit tiOPFSQLDB_IB_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiAutoMap_TST
  ,tiOID_TST
  ;


type

  TTestTIPersistenceLayersSQLDB_IB = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure   ThreadedDBConnectionPool; override;
  end;
  

  TTestTIDatabaseSQLDB_IB = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
    procedure DatabaseExists; override;
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIQuerySQLDB_IB = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationSQLDB_IB = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure CollectionReadPKThreaded; override;
  end;


  TTestTIOIDPersistentGUIDSQLDB_IB = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;


  TTestTIOIDPersistentIntegerSQLDB_IB = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;



procedure RegisterTests;


implementation
uses
  tiConstants
  ,TestFramework
  ,tiOPFTestManager
  ,SysUtils
  ,tiUtils
  ,tiTestDependencies
  ;
  
procedure RegisterTests ;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersSQLDB_IB);
  tiRegisterPersistenceTest(TTestTIDatabaseSQLDB_IB);
  tiRegisterPersistenceTest(TTestTIQuerySQLDB_IB);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDSQLDB_IB);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerSQLDB_IB);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationSQLDB_IB);
end ;

{ TTestTIDatabaseSQLDB_IB }

procedure TTestTIDatabaseSQLDB_IB.CreateDatabase;
var
  LDB:       string;
  LDBExists: boolean;
begin
  LDB := ExpandFileName(TestSetupData.DBName);
  LDB := tiSwapExt(LDB, 'tmp');
  if FileExists(LDB) then
  begin
    tiDeleteFile(LDB);
    if FileExists(LDB) then
      Fail('Can not remove old database file');
  end;

  Check(not FileExists(LDB), 'Database exists when it should not');
  PersistenceLayer.DatabaseClass.CreateDatabase(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(FileExists(LDB), 'Database not created');

  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);

  Check(LDBExists, 'Database does not exist when it should do');
  tiDeleteFile(LDB);
end;

class function TTestTIDatabaseSQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

procedure TTestTIDatabaseSQLDB_IB.DatabaseExists;
var
  LDB:       string;
  LDBExists: boolean;
begin
  LDB       := TestSetupData.DBName;
  Check(FileExists(LDB), Format('Database file <%s> not found so test can not be performed', [LDB]));
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(LDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(LDB + 'Tmp'), 'Database file found so test can not be performed');
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName + 'Tmp',
    TestSetupData.Username,
    TestSetupData.Password);
  Check(not LDBExists, 'DBExists returned true when it should return false');
end;

{ TTestTIPersistenceLayersSQLDB_IB }

class function TTestTIPersistenceLayersSQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

procedure TTestTIPersistenceLayersSQLDB_IB.ThreadedDBConnectionPool;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ELSE}
  inherited ThreadedDBConnectionPool;
//  {$ENDIF}
end;

{ TTestTIQuerySQLDB_IB }

class function TTestTIQuerySQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

{ TTestTIAutoMapOperationSQLDB_IB }

class function TTestTIAutoMapOperationSQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

procedure TTestTIAutoMapOperationSQLDB_IB.CollectionReadPKThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited CollectionReadPKThreaded;
end;

{ TTestTIOIDPersistentGUIDSQLDB_IB }

class function TTestTIOIDPersistentGUIDSQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

procedure TTestTIOIDPersistentGUIDSQLDB_IB.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

procedure TTestTIOIDPersistentGUIDSQLDB_IB.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDMultiUser;
end;

{ TTestTIOIDPersistentIntegerSQLDB_IB }

class function TTestTIOIDPersistentIntegerSQLDB_IB.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;

procedure TTestTIOIDPersistentIntegerSQLDB_IB.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

procedure TTestTIOIDPersistentIntegerSQLDB_IB.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDMultiUser;
end;

end.
