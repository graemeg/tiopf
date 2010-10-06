unit tiOPFADOAccess_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersADOAccess = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure ConnectDatabase; override;
    procedure CreateTIQuery_LayerName; override;
    procedure CreateTIQuery_DatabaseClass; override;
  end;

  TTestTIDatabaseADOAccess = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure Transaction_Commit; override;
    procedure Transaction_RollBack; override;
    procedure CreateTableDropTable; override;
  end;

  TTestTIQueryADOAccess = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure GetSetSQL; override;
  end;

  TTestTIAutoMapOperationADOAccess = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TestSetupAndTearDown; override;
    procedure CollectionReadPKThreaded; override;
  end;

  TTestAutomappingCriteriaADOAccess = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TestSetupAndTearDown; override;
  end;

  TTestTIOIDPersistentGUIDADOAccess = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerADOAccess = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDSingleUser; override;
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;

procedure RegisterTests;

implementation

uses
  tiConstants,
  TestFramework,
  tiOPFTestManager,
  SysUtils,
  tiUtils,
  tiTestDependencies,
  tiQuery;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersADOAccess);
  tiRegisterPersistenceTest(TTestTIDatabaseADOAccess);
  tiRegisterPersistenceTest(TTestTIQueryADOAccess);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDADOAccess);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerADOAccess);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationADOAccess);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaADOAccess);
end;

{ TTestTIDatabaseADOAccess }

procedure TTestTIDatabaseADOAccess.CreateDatabase;
var
  LDatabaseName:       string;
  LDatabaseExists: boolean;
  LDatabaseClass: TtiDatabaseClass;
begin
  LDatabaseClass:= PersistenceLayer.DatabaseClass;
  LDatabaseName := ExpandFileName(TestSetupData.DBName);
  LDatabaseName := tiSwapExt(LDatabaseName, 'tmp');
  if FileExists(LDatabaseName) then
  begin
    tiDeleteFile(LDatabaseName);
    if FileExists(LDatabaseName) then
      Fail('Can not remove old database file');
  end;

  Check(not FileExists(LDatabaseName), 'Database exists when it should not');
  LDatabaseClass.CreateDatabase(
    LDatabaseName,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(FileExists(LDatabaseName), 'Database not created');

  LDatabaseExists :=
    LDatabaseClass.DatabaseExists(
    LDatabaseName,
    TestSetupData.Username,
    TestSetupData.Password);

  Check(LDatabaseExists, 'Database does not exist when it should do');
  tiDeleteFile(LDatabaseName);
end;

procedure TTestTIDatabaseADOAccess.CreateTableDropTable;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

procedure TTestTIDatabaseADOAccess.DatabaseExists;
var
  LDatabaseName:       string;
  LDatabaseExists: boolean;
  LDatabaseClass: TtiDatabaseClass;
begin
  LDatabaseName       := TestSetupData.DBName;
  LDatabaseClass:= PersistenceLayer.DatabaseClass;
  Check(FileExists(LDatabaseName), 'Database file not found so test can not be performed');
  LDatabaseExists :=
    LDatabaseClass.DatabaseExists(
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(LDatabaseExists, 'DBExists returned false when it should return true');
  Check(not FileExists(LDatabaseName + 'Tmp'), 'Database file found so test can not be performed');
  LDatabaseExists :=
    LDatabaseClass.DatabaseExists(
    TestSetupData.DBName + 'Tmp',
    TestSetupData.Username,
    TestSetupData.Password);
  Check(not LDatabaseExists, 'DBExists returned true when it should return false');
end;

class function TTestTIDatabaseADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

procedure TTestTIDatabaseADOAccess.Transaction_Commit;
begin
  AllowedMemoryLeakSize:= 104;
  inherited;
end;

procedure TTestTIDatabaseADOAccess.Transaction_RollBack;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

{ TTestTIPersistenceLayersADOAccess }

procedure TTestTIPersistenceLayersADOAccess.ConnectDatabase;
begin
  AllowedMemoryLeakSize:= 24;
  inherited;
end;

procedure TTestTIPersistenceLayersADOAccess.CreateTIQuery_DatabaseClass;
begin
  AllowedMemoryLeakSize:= 32; 
  inherited;
end;

procedure TTestTIPersistenceLayersADOAccess.CreateTIQuery_LayerName;
begin
  AllowedMemoryLeakSize:= 32;
  inherited;
end;

class function TTestTIPersistenceLayersADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIQueryADOAccess }

procedure TTestTIQueryADOAccess.GetSetSQL;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

class function TTestTIQueryADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIAutoMapOperationADOAccess }

procedure TTestTIAutoMapOperationADOAccess.CollectionReadPKThreaded;
begin
  AllowedMemoryLeakSize:= 32;
  inherited;
end;

class function TTestTIAutoMapOperationADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

procedure TTestTIAutoMapOperationADOAccess.TestSetupAndTearDown;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

{ TTestAutomappingCriteriaADOAccess }

class function TTestAutomappingCriteriaADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

procedure TTestAutomappingCriteriaADOAccess.TestSetupAndTearDown;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

{ TTestTIOIDPersistentIntegerADOAccess }

class function TTestTIOIDPersistentIntegerADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

procedure TTestTIOIDPersistentIntegerADOAccess.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
  SetAllowedLeakArray([32, 88]);
  inherited;
end;

procedure TTestTIOIDPersistentIntegerADOAccess.TtiNextOIDGeneratorAssignNextOIDSingleUser;
begin
  AllowedMemoryLeakSize:= 72;
  inherited;
end;

procedure TTestTIOIDPersistentIntegerADOAccess.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
  AllowedMemoryLeakSize:= 32;
  inherited;
end;

{ TTestTIOIDPersistentGUIDADOAccess }

class function TTestTIOIDPersistentGUIDADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

end.
