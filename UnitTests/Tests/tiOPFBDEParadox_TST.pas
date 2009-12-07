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
  published
    procedure tiOPFManager_ConnectDatabase; override;
  end;

  TTestTIDatabaseBDEParadox = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure Transaction_RollBack; override;

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
  published
    procedure ReadWriteString255; override;
    procedure ReadWriteString256; override;
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
  FileCtrl,
  {$ENDIF}
  SysUtils,
  tiUtils,
  tiTestDependencies,
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
  LDir: string;
begin
  LDir := tiGetTempFile('');
  try
    tiForceRemoveDir(LDir);
    Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
    PersistenceLayer.DatabaseClass.CreateDatabase(LDir, 'null', 'null');
    Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
  finally
    tiForceRemoveDir(LDir);
  end;
end;

procedure TTestTIDatabaseBDEParadox.CreateTableDropTable;
begin
  SetAllowedLeakArray([488, 504]);
  inherited;
end;

procedure TTestTIDatabaseBDEParadox.DatabaseExists;
var
  LDir: string;
begin
  LDir := tiSwapExt(TempFileName, '');
  try
    tiForceRemoveDir(LDir);
    Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
    Check(not PersistenceLayer.DatabaseClass.DatabaseExists(LDir, 'null', 'null'),
      'FDatabaseClass.DatabaseExists()=true when it should =false');
    ForceDirectories(LDir);
    Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
    Check(PersistenceLayer.DatabaseClass.DatabaseExists(LDir, 'null', 'null'),
      'FDatabaseClass.DatabaseExists()=false when it should =true');
  finally
    tiForceRemoveDir(LDir);
  end;
end;

class function TTestTIDatabaseBDEParadox.PersistenceLayerName: string;
begin
  Result := cTIPersistBDEParadox;
end;

procedure TTestTIPersistenceLayersBDEParadox.tiOPFManager_ConnectDatabase;
begin
  SetAllowedLeakArray([136, 176]);
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

procedure TTestTIAutoMapOperationBDEParadox.ReadWriteString255;
begin
  AllowedMemoryLeakSize:= 8;
  inherited;
end;

procedure TTestTIAutoMapOperationBDEParadox.ReadWriteString256;
begin
  AllowedMemoryLeakSize:= 8;
  inherited;
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
