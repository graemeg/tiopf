unit tiOPFADOAccess_TST;

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

  TTestTIPersistenceLayersADOAccess = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseADOAccess = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryADOAccess = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;

  TTestTIAutoMapOperationADOAccess = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestAutomappingCriteriaADOAccess = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerADOAccess = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistADOAccess) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIPersistenceLayersADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIDatabaseADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIQueryADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIOIDManagerADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIAutoMapOperationADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestAutomappingCriteriaADOAccess.Suite);
  end;
end;

{ TtiOPFTestSetupDataADOAccess }

{ TTestTIDatabaseADOAccess }

procedure TTestTIDatabaseADOAccess.CreateDatabase;
var
  lDB : string;
  lDBExists : boolean;
begin
  lDB := ExpandFileName(PerFrameworkSetup.DBName);
  lDB := tiSwapExt(lDB, 'tmp');
  if FileExists(lDB) then
  begin
    tiDeleteFile(lDB);
    if FileExists(lDB) then
      Fail('Can not remove old database file');
  end;

  Check(not FileExists(lDB), 'Database exists when it should not');
  FDatabaseClass.CreateDatabase(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(FileExists(lDB), 'Database not created');

  lDBExists :=
    FDatabaseClass.DatabaseExists(
      lDB,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);

  Check(lDBExists, 'Database does not exist when it should do');
  tiDeleteFile(lDB);
end;

procedure TTestTIDatabaseADOAccess.DatabaseExists;
var
  lDB : string;
  lDBExists : boolean;
begin
  lDB := PerFrameworkSetup.DBName;
  Check(FileExists(lDB), 'Database file not found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(lDB + 'Tmp'), 'Database file found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName + 'Tmp',
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

{ TtiOPFTestSetupDecoratorADOAccess }

procedure TTestTIDatabaseADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIQueryADOAccess }

procedure TTestTIQueryADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIPersistenceLayersADOAccess }

procedure TTestTIPersistenceLayersADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIAutoMapOperationADOAccess }

procedure TTestTIAutoMapOperationADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIAutoMapOperationADOAccess }

procedure TTestAutomappingCriteriaADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIOIDManagerADOAccess }

procedure TTestTIOIDManagerADOAccess.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

end.
