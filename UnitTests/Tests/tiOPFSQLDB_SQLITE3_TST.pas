unit tiOPFSQLDB_SQLITE3_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiAutoMap_TST
  ,tiOID_TST
  ;


type

  TTestTIPersistenceLayersSQLDB_SQLITE3 = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure   ThreadedDBConnectionPool; override;
  end;
  

  TTestTIDatabaseSQLDB_SQLITE3 = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
    procedure DatabaseExists; override;
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIQuerySQLDB_SQLITE3 = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationSQLDB_SQLITE3 = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure CollectionReadPKThreaded; override;
  end;


  TTestTIOIDPersistentGUIDSQLDB_SQLITE3 = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;


  TTestTIOIDPersistentIntegerSQLDB_SQLITE3 = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersSQLDB_SQLITE3);
  tiRegisterPersistenceTest(TTestTIDatabaseSQLDB_SQLITE3);
  tiRegisterPersistenceTest(TTestTIQuerySQLDB_SQLITE3);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDSQLDB_SQLITE3);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerSQLDB_SQLITE3);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationSQLDB_SQLITE3);
end ;

{ TTestTIDatabaseSQLDB_SQLITE3 }

procedure TTestTIDatabaseSQLDB_SQLITE3.CreateDatabase;
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

class function TTestTIDatabaseSQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

procedure TTestTIDatabaseSQLDB_SQLITE3.DatabaseExists;
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

{ TTestTIPersistenceLayersSQLDB_SQLITE3 }

class function TTestTIPersistenceLayersSQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

procedure TTestTIPersistenceLayersSQLDB_SQLITE3.ThreadedDBConnectionPool;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ELSE}
  inherited ThreadedDBConnectionPool;
//  {$ENDIF}
end;

{ TTestTIQuerySQLDB_SQLITE3 }

class function TTestTIQuerySQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

{ TTestTIAutoMapOperationSQLDB_SQLITE3 }

class function TTestTIAutoMapOperationSQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

procedure TTestTIAutoMapOperationSQLDB_SQLITE3.CollectionReadPKThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited CollectionReadPKThreaded;
end;

{ TTestTIOIDPersistentGUIDSQLDB_SQLITE3 }

class function TTestTIOIDPersistentGUIDSQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

procedure TTestTIOIDPersistentGUIDSQLDB_SQLITE3.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

procedure TTestTIOIDPersistentGUIDSQLDB_SQLITE3.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDMultiUser;
end;

{ TTestTIOIDPersistentIntegerSQLDB_SQLITE3 }

class function TTestTIOIDPersistentIntegerSQLDB_SQLITE3.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbSQLite3;
end;

procedure TTestTIOIDPersistentIntegerSQLDB_SQLITE3.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

procedure TTestTIOIDPersistentIntegerSQLDB_SQLITE3.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
//  {$IFDEF FPC}
//  Fail('This freezes up under FPC compiler.');
//  {$ENDIF}
  inherited TtiNextOIDGeneratorAssignNextOIDMultiUser;
end;

end.
