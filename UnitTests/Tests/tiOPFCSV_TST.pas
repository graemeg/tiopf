unit tiOPFCSV_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiAutoMap_TST
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersCSV = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseCSV = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryCSV = class(TTestTIQueryNonSQL)
  protected
    procedure   SetUp; override;
  end;
  

  TTestTIAutoMapOperationCSV = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;
  

  TTestTIOIDManagerCSV = class(TTestTIOIDManager)
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
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistCSV) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIPersistenceLayersCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIDatabaseCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIQueryCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIOIDManagerCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIAutoMapOperationCSV.Suite);
  end;
end;

{ TtiOPFTestSetupDataCSV }

procedure TTestTIDatabaseCSV.CreateDatabase;
var
  lDir : string;
begin
  lDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseCSV.DatabaseExists;
var
  lDir : string;
begin
  lDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseCSV.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIPersistenceLayersCSV }

procedure TTestTIPersistenceLayersCSV.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIQueryCSV }

procedure TTestTIQueryCSV.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIAutoMapOperationCSV }

procedure TTestTIAutoMapOperationCSV.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIOIDManagerCSV }

procedure TTestTIOIDManagerCSV.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

end.


