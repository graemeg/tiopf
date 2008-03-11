unit tiOPFCrSdac_TST;

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

  TTestTIPersistenceLayersCrSdac = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseCrSdac = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryCrSdac = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;

  TTestTIAutoMapOperationCrSdac = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  published
    procedure ReadWriteDateMin; override;
  end;

  TTestAutomappingCriteriaCrSdac = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDPersistentGUIDCrSdac = class(TTestTIOIDPersistentGUID)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDPersistentIntegerCrSdac = class(TTestTIOIDPersistentInteger)
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
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistCrSdac) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIPersistenceLayersCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIDatabaseCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIQueryCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIOIDPersistentGUIDCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIOIDPersistentIntegerCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestTIAutoMapOperationCrSdac.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCrSdac), TTestAutomappingCriteriaCrSdac.Suite);

  end;
end;

procedure TTestTIDatabaseCrSdac.CreateDatabase;
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

procedure TTestTIDatabaseCrSdac.DatabaseExists;
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

{ TTestTIPersistenceLayersCrSdac }

procedure TTestTIPersistenceLayersCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

procedure TTestTIDatabaseCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

{ TTestTIQueryCrSdac }

procedure TTestTIQueryCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

{ TTestTIAutoMapOperationCrSdac }

procedure TTestTIAutoMapOperationCrSdac.ReadWriteDateMin;
begin
  // SQL server has a non standard minimum date
  DoReadWriteDateTime(EncodeDate(1753, 1, 1));
end;

procedure TTestTIAutoMapOperationCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

{ TTestAutomappingCriteriaCrSdac }

procedure TTestAutomappingCriteriaCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

{ TTestTIOIDPersistentIntegerCrSdac }

procedure TTestTIOIDPersistentIntegerCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

{ TTestTIOIDPersistentGUIDCrSdac }

procedure TTestTIOIDPersistentGUIDCrSdac.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCrSdac);
  inherited;
end;

end.
