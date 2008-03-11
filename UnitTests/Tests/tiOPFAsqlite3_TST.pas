unit tiOPFAsqlite3_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiAutoMap_TST
  ,tiAutomapCriteria_TST
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersAsqlite3 = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseAsqlite3 = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryAsqlite3 = class(TTestTIQuerySQL)
  protected
    procedure   SetUp; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationAsqlite3 = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestAutomappingCriteriaAsqlite3 = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDPersistentGUIDAsqlite3 = class(TTestTIOIDPersistentGUID)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDPersistentIntegerAsqlite3 = class(TTestTIOIDPersistentInteger)
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
  ,tiOPFTestManager
  ,SysUtils
  ,tiUtils
  ,tiTestDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
//  ,tiLog
//  ,tiQuery
  , tiTestFramework;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistAsqlite3) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIPersistenceLayersAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIDatabaseAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIQueryAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIOIDPersistentGUIDAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIOIDPersistentIntegerAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestTIAutoMapOperationAsqlite3.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistAsqlite3), TTestAutomappingCriteriaAsqlite3.Suite);
  end;
end;

{ TtiOPFTestSetupDataAsqlite3 }

procedure TTestTIDatabaseAsqlite3.CreateDatabase;
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

procedure TTestTIDatabaseAsqlite3.DatabaseExists;
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

procedure TTestTIDatabaseAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TtiOPFTestSetupDecoratorAsqlite3 }

{ TTestTIPersistenceLayersAsqlite3 }

procedure TTestTIPersistenceLayersAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TTestTIAutoMapOperationAsqlite3 }

procedure TTestTIAutoMapOperationAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TTestTIOIDManagerAsqlite3 }

procedure TTestTIOIDPersistentIntegerAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TTestTIQueryAsqlite3 }

procedure TTestTIQueryAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TTestAutomappingCriteriaAsqlite3 }

procedure TTestAutomappingCriteriaAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

{ TTestTIOIDPersistentGUIDAsqlite3 }

procedure TTestTIOIDPersistentGUIDAsqlite3.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistAsqlite3);
  inherited;
end;

end.
