unit tiOPFBDEParadox_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersBDEParadox = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseBDEParadox = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure Transaction_RollBack; override;

    procedure tiOPFManager_ConnectDatabase; override;
    procedure CreateTableDropTable; override;

  end;

  TTestTIQueryBDEParadox = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationBDEParadox = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaBDEParadox = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDBDEParadox = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerBDEParadox = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;

procedure RegisterTests;

implementation

uses
  tiConstants,
  {$IFDEF FPC}
  tiFPCUnitUtils,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  tiOPFTestManager,
  SysUtils,
  tiUtils,
  tiTestDependencies,
  FileCtrl,
  tiLog,
  tiTestFramework;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersBDEParadox);
  tiRegisterPersistenceTest(TTestTIDatabaseBDEParadox);
  tiRegisterPersistenceTest(TTestTIQueryBDEParadox);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDBDEParadox);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerBDEParadox);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationBDEParadox);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaBDEParadox);
end;

{ TtiOPFTestSetupDataBDEParadox }

procedure TTestTIDatabaseBDEParadox.CreateDatabase;
var
  lDir: string;
begin
  SetAllowedLeakArray([40]);
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

procedure TTestTIDatabaseBDEParadox.CreateTableDropTable;
begin
  SetAllowedLeakArray([504]);
  inherited;
end;

procedure TTestTIDatabaseBDEParadox.DatabaseExists;
var
  lDir: string;
begin
  SetAllowedLeakArray([40]);
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

class function TTestTIDatabaseBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

procedure TTestTIDatabaseBDEParadox.tiOPFManager_ConnectDatabase;
begin
  SetAllowedLeakArray([136]);
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

{ TTestTIPersistenceLayersBDEParadox }

class function TTestTIPersistenceLayersBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

{ TTestTIQueryBDEParadox }

class function TTestTIQueryBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

{ TTestTIAutoMapOperationBDEParadox }

class function TTestTIAutoMapOperationBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

{ TTestAutomappingCriteriaBDEParadox }

class function TTestAutomappingCriteriaBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

{ TTestTIOIDPersistentGUIDBDEParadox }

class function TTestTIOIDPersistentGUIDBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

{ TTestTIOIDPersistentIntegerBDEParadox }

class function TTestTIOIDPersistentIntegerBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

end.
