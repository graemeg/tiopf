unit tiOPFADOSQLServer_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiAutoMap_TST
  ,tiAutomapCriteria_TST  
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersADOSQLServer = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseADOSQLServer = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryADOSQLServer = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;

  TTestTIAutoMapOperationADOSQLServer = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  published
    procedure ReadWriteDateMin; override;
  end;

  TTestAutomappingCriteriaADOSQLServer = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerADOSQLServer = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
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
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistADOSQLServer) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIPersistenceLayersADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIDatabaseADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIQueryADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIOIDManagerADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIAutoMapOperationADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestAutomappingCriteriaADOSQLServer.Suite);

  end;
end;

procedure TTestTIDatabaseADOSQLServer.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end;
  end;
end;

procedure TTestTIDatabaseADOSQLServer.DatabaseExists;
var
  lDB : string;
  lDBExists : boolean;
begin
  lDB := PerFrameworkSetup.DBName;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');

  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName + 'Tmp',
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

{ TTestTIPersistenceLayersADOSQLServer }

procedure TTestTIPersistenceLayersADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

procedure TTestTIDatabaseADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIQueryADOSQLServer }

procedure TTestTIQueryADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIAutoMapOperationADOSQLServer }

procedure TTestTIAutoMapOperationADOSQLServer.ReadWriteDateMin;
begin
  // SQL server has a non standard minimum date
  DoReadWriteDateTime(EncodeDate(1753, 1, 1));
end;

procedure TTestTIAutoMapOperationADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIOIDManagerADOSQLServer }

procedure TTestTIOIDManagerADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestAutomappingCriteriaADOSQLServer }

procedure TTestAutomappingCriteriaADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

end.
