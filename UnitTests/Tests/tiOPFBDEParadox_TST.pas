unit tiOPFBDEParadox_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_tst
  ;

type

  TtiOPFTestSetupDataBDEParadox = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;

  TTestTIPersistenceLayersBDEParadox = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;

  TTestTIDatabaseBDEParadox = class( TTestTIDatabase )
  protected
    procedure   Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure Transaction_RollBack ; override ;
  end ;

  TTestTIQueryBDEParadox = class( TTestTIQuerySQL )
  protected
    procedure   Setup; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream ; override ;
  end ;

  TTestTIClassToDBMapOperationBDEParadox = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;

  TTestTIOIDManagerBDEParadox = class(TTestTIOIDManager)
  protected
    procedure   Setup; override;
  end;

procedure RegisterTests;

implementation
uses
  tiConstants
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiDUnitDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiLog
  , tiTestFramework;

procedure RegisterTests ;
begin
  if gTIOPFTestManager.ToRun(cTIPersistBDEParadox) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIPersistenceLayersBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIDatabaseBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIQueryBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIOIDManagerBDEParadox.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistBDEParadox), TTestTIClassToDBMapOperationBDEParadox.Suite);
  end;
end ;

{ TtiOPFTestSetupDataBDEParadox }

constructor TtiOPFTestSetupDataBDEParadox.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_BDEPARADOX}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistBDEParadox ;
  FDBName   := ExpandFileName(ReadFromReg( cTIPersistBDEParadox, 'DBName', gTestDataRoot + 'Paradox' ));
  FUserName := ReadFromReg( cTIPersistBDEParadox, 'UserName', 'null' ) ;
  FPassword := ReadFromReg( cTIPersistBDEParadox, 'Password', 'null' ) ;
  FCanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

procedure TTestTIDatabaseBDEParadox.CreateDatabase;
var
  lDir : string ;
begin
  lDir := tiGetTempFile('');
  try
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(lDir, 'null', 'null' );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  finally
    tiForceRemoveDir(lDir);
  end;
end;

procedure TTestTIDatabaseBDEParadox.DatabaseExists;
var
  lDir : string ;
begin
  lDir := tiSwapExt(TempFileName, '');
  try
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(lDir, 'null', 'null' ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(lDir, 'null', 'null' ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
  finally
    tiForceRemoveDir(lDir);
  end;
end;

procedure TTestTIDatabaseBDEParadox.Setup;
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
  LogWarning( ClassName + '.RollBack not tested' ) ;
end;

{ TtiOPFTestSetupDecoratorBDEParadox }

{ TTestTIPersistenceLayersBDEParadox }

procedure TTestTIPersistenceLayersBDEParadox.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIClassToDBMapOperationBDEParadox }

procedure TTestTIClassToDBMapOperationBDEParadox.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIOIDManagerBDEParadox }

procedure TTestTIOIDManagerBDEParadox.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

{ TTestTIQueryBDEParadox }

procedure TTestTIQueryBDEParadox.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistBDEParadox);
  inherited;
end;

end.
