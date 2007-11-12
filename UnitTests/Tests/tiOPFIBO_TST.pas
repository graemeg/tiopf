unit tiOPFIBO_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiTestFramework
  ,tiAutoMap_TST
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersIBO = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseIBO = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryIBO = class(TTestTIQuerySQL)
  protected
    procedure   SetUp; override;
  end;

  TTestTIAutoMapOperationIBO = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerIBO = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
  end;

procedure RegisterTests;

implementation
uses
   tiConstants
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiDUnitDependencies
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIQueryTestName) then
  begin
    RegisterTest(cTIQueryTestName, TTestTIPersistenceLayersIBO.Suite);
    RegisterTest(cTIQueryTestName, TTestTIDatabaseIBO.Suite);
    RegisterTest(cTIQueryTestName, TTestTIQueryIBO.Suite);
    RegisterTest(cTIQueryTestName, TTestTIOIDManagerIBO.Suite);
    RegisterTest(cTIQueryTestName, TTestTIAutoMapOperationIBO.Suite);
  end;
end;

{ TTestTIDatabaseIBO }

procedure TTestTIDatabaseIBO.CreateDatabase;
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

procedure TTestTIDatabaseIBO.DatabaseExists;
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

{ TtiOPFTestSetupDecoratorIBO }

procedure TTestTIDatabaseIBO.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistIBO);
  inherited;
end;

{ TTestTIPersistenceLayersIBO }

procedure TTestTIPersistenceLayersIBO.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistIBO);
  inherited;
end;

{ TTestTIQueryIBO }

procedure TTestTIQueryIBO.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistIBO);
  inherited;
end;

procedure TTestTIAutoMapOperationIBO.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistIBO);
  inherited;
end;

{ TTestTIOIDManagerXMLLight }

procedure TTestTIOIDManagerIBO.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistIBO);
  inherited;
end;

end.
