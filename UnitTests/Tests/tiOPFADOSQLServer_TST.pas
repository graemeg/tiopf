unit tiOPFADOSQLServer_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersADOSQLServer = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseADOSQLServer = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryADOSQLServer = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationADOSQLServer = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure ReadWriteDateMin; override;
  end;

  TTestAutomappingCriteriaADOSQLServer = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDADOSQLServer = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerADOSQLServer = class(TTestTIOIDPersistentInteger)
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
  tiOPFTestManager,
  SysUtils,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersADOSQLServer);
  tiRegisterPersistenceTest(TTestTIDatabaseADOSQLServer);
  tiRegisterPersistenceTest(TTestTIQueryADOSQLServer);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDADOSQLServer);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerADOSQLServer);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationADOSQLServer);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaADOSQLServer);
end;

procedure TTestTIDatabaseADOSQLServer.CreateDatabase;
begin
  try
    PersistenceLayer.DatabaseClass.CreateDatabase(TestSetupData.DBName, TestSetupData.Username, TestSetupData.Password);
    Fail('Exception not raised when it should have been');
  except
    on e: Exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + PersistenceLayer.DatabaseClass.ClassName, e.Message) <> 0);
    end;
  end;
end;

procedure TTestTIDatabaseADOSQLServer.DatabaseExists;
var
  lDB:       string;
  lDBExists: boolean;
begin
  lDB       := TestSetupData.DBName;
  Check(FileExists(LDB), 'Database file not found so test can not be performed');
  lDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(LDB + 'Tmp'), 'Database file found so test can not be performed');
  lDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName + 'Tmp',
    TestSetupData.Username,
    TestSetupData.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

class function TTestTIDatabaseADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

class function TTestTIAutoMapOperationADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

procedure TTestTIAutoMapOperationADOSQLServer.ReadWriteDateMin;
begin
  // SQL server has a non standard minimum date
  DoReadWriteDateTime(EncodeDate(1753, 1, 1));
end;

{ TTestTIPersistenceLayersADOSQLServer }

class function TTestTIPersistenceLayersADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

{ TTestTIQueryADOSQLServer }

class function TTestTIQueryADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

{ TTestAutomappingCriteriaADOSQLServer }

class function TTestAutomappingCriteriaADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

{ TTestTIOIDPersistentGUIDADOSQLServer }

class function TTestTIOIDPersistentGUIDADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

{ TTestTIOIDPersistentIntegerADOSQLServer }

class function TTestTIOIDPersistentIntegerADOSQLServer.PersistenceLayerName: string;
begin
  Result := cTIPersistADOSQLServer;
end;

end.
