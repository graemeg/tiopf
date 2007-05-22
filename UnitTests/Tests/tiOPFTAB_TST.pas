unit tiOPFTAB_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_tst
 ;

type

  TtiOPFTestSetupDataTAB = class(TtiOPFTestSetupData)
  public
    constructor Create; override;
  end;

  TTestTIPersistenceLayersTab = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseTAB = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure ThreadedDBConnectionPool; override;
  end;

  TTestTIQueryTAB = class(TTestTIQueryNonSQL)
  protected
    procedure SetUp; override;
  end;

  TTestTIClassToDBMapOperationTab = class(TTestTIClassToDBMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerTab = class(TTestTIOIDManager)
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
  ,tiDUnitDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistTab) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistTab), TTestTIPersistenceLayersTab.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistTab), TTestTIDatabaseTAB.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistTab), TTestTIQueryTAB.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistTab), TTestTIOIDManagerTab.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistTab), TTestTIClassToDBMapOperationTab.Suite);
  end;
end;

{ TtiOPFTestSetupDataTAB }

constructor TtiOPFTestSetupDataTAB.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_TAB }
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistTAB;
  FDBName  := ExpandFileName(ReadFromReg(cTIPersistTAB, 'DBName', gTestDataRoot + 'TAB'));
  FUserName := ReadFromReg(cTIPersistTAB, 'UserName', 'null');
  FPassword := ReadFromReg(cTIPersistTAB, 'Password', 'null');
  FCanCreateDatabase := true;
  ForceTestDataDirectory;
end;

{ TTestTIDatabaseTAB }

procedure TTestTIDatabaseTAB.CreateDatabase;
var
  lDir : string;
begin
  lDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseTAB.DatabaseExists;
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

procedure TTestTIDatabaseTAB.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistTAB);
  inherited;
end;

procedure TTestTIDatabaseTAB.ThreadedDBConnectionPool;
begin
  LogWarning('The TAB persistence layer can only manage one thread.');
  DoThreadedDBConnectionPool(1);
end;

{ TtiOPFTestSetupDecoratorTAB }

{ TTestTIQueryTAB }

procedure TTestTIQueryTAB.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistTAB);
  inherited;
end;

{ TTestTIPersistenceLayersTab }

procedure TTestTIPersistenceLayersTab.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistTAB);
  inherited;
end;

{ TTestTIClassToDBMapOperationTab }

procedure TTestTIClassToDBMapOperationTab.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistTAB);
  inherited;
end;

{ TTestTIOIDManagerTab }

procedure TTestTIOIDManagerTab.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistTAB);
  inherited;
end;

end.
