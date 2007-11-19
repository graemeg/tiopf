unit tiOPFBDEParadox_TST;

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

  TTestTIPersistenceLayersBDEParadox = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseBDEParadox = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure Transaction_RollBack; override;
  end;

  TTestTIQueryBDEParadox = class(TTestTIQuerySQL)
  protected
    procedure   SetUp; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationBDEParadox = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestAutomappingCriteriaBDEParadox = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerBDEParadox = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
  published
    procedure   NextOIDInteger_MultiUserAccess;    override;
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
  ,tiTestDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiLog
  , tiTestFramework;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistBDEParadox) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIPersistenceLayersBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIDatabaseBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIQueryBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIOIDManagerBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIAutoMapOperationBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestAutomappingCriteriaBDEParadox.Suite);
  end;
end;

{ TtiOPFTestSetupDataBDEParadox }

procedure TTestTIDatabaseBDEParadox.CreateDatabase;
var
  lDir : string;
begin
  lDir := tiGetTempFile('');
  try
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(lDir, 'null', 'null');
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  finally
    tiForceRemoveDir(lDir);
  end;
end;

procedure TTestTIDatabaseBDEParadox.DatabaseExists;
var
  lDir : string;
begin
  lDir := tiSwapExt(TempFileName, '');
  try
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(lDir, 'null', 'null'),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(lDir, 'null', 'null'),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
  finally
    tiForceRemoveDir(lDir);
  end;
end;

procedure TTestTIDatabaseBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

procedure TTestTIDatabaseBDEParadox.Transaction_RollBack;
begin
  // inherited;
  // RollBack not supported in Paradox tables without indexes.
  // See note in TtiDatabaseBDEParadox.RollBack
  // Not may users of the BDEParadox layer these days so we won't spend the
  // time implementing a test.
  Check(True);
  LogWarning(ClassName + '.RollBack not tested');
end;

{ TtiOPFTestSetupDecoratorBDEParadox }

{ TTestTIPersistenceLayersBDEParadox }

procedure TTestTIPersistenceLayersBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIAutoMapOperationBDEParadox }

procedure TTestTIAutoMapOperationBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIOIDManagerBDEParadox }

procedure TTestTIOIDManagerBDEParadox.NextOIDInteger_MultiUserAccess;
begin
  Check(True);
  LogWarning(ClassName + '.NextOIDInteger_MultiUserAccess not tested');
end;

procedure TTestTIOIDManagerBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIQueryBDEParadox }

procedure TTestTIQueryBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestAutomappingCriteriaBDEParadox }

procedure TTestAutomappingCriteriaBDEParadox.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

end.
