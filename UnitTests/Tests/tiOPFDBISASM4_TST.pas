unit tiOPFDBISASM4_TST;

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

  TTestTIPersistenceLayersDBISAM4 = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseDBISAM4 = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryDBISAM4 = class(TTestTIQuerySQL)
  protected
    procedure   SetUp; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationDBISAM4 = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestAutomappingCriteriaDBISAM4 = class(TTestAutomappingCriteria)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerDBISAM4 = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
  published
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
  ,tiTestFramework
  ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistDBISAM4) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestTIPersistenceLayersDBISAM4.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestTIDatabaseDBISAM4.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestTIQueryDBISAM4.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestTIOIDManagerDBISAM4.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestTIAutoMapOperationDBISAM4.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDBISAM4), TTestAutomappingCriteriaDBISAM4.Suite);
  end;
end;

{ TtiOPFTestSetupDataDBISAM4 }

procedure TTestTIDatabaseDBISAM4.CreateDatabase;
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

procedure TTestTIDatabaseDBISAM4.DatabaseExists;
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

procedure TTestTIDatabaseDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

{ TtiOPFTestSetupDecoratorDBISAM4 }

{ TTestTIPersistenceLayersDBISAM4 }

procedure TTestTIPersistenceLayersDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

{ TTestTIAutoMapOperationDBISAM4 }

procedure TTestTIAutoMapOperationDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

{ TTestTIOIDManagerDBISAM4 }

procedure TTestTIOIDManagerDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

{ TTestTIQueryDBISAM4 }

procedure TTestTIQueryDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

{ TTestAutomappingCriteriaDBISAM4 }

procedure TTestAutomappingCriteriaDBISAM4.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDBISAM4);
  inherited;
end;

end.
