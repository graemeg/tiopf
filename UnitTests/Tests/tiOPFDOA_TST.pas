unit tiOPFDOA_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiAutoMap_TST
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersDOA = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseDOA = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryDOA = class(TTestTIQuerySQL)
  protected
    procedure   SetUp; override;
  end;

  TTestTIAutoMapOperationDOA = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerDOA = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistDOA) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIPersistenceLayersDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIDatabaseDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIQueryDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIOIDManagerDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIAutoMapOperationDOA.Suite);
  end;
end;

{ TtiOPFTestSetupDataDOA }

procedure TTestTIDatabaseDOA.CreateDatabase;
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

procedure TTestTIDatabaseDOA.DatabaseExists;
begin
  try
    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end;
  end;
end;

{ TTestTIPersistenceLayersDOA }

procedure TTestTIPersistenceLayersDOA.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

procedure TTestTIDatabaseDOA.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIQueryDOA }

procedure TTestTIQueryDOA.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIAutoMapOperationDOA }

procedure TTestTIAutoMapOperationDOA.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIOIDManagerDOA }

procedure TTestTIOIDManagerDOA.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

end.
