unit tiOPFTAB_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQueryNonSQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersTab = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseTAB = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryTAB = class(TTestTIQueryNonSQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationTab = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDTab = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerTab = class(TTestTIOIDPersistentInteger)
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
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersTab);
  tiRegisterPersistenceTest(TTestTIDatabaseTAB);
  tiRegisterPersistenceTest(TTestTIQueryTAB);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationTab);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDTab);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerTab);
end;

 { TtiOPFTestSetupDataTAB }


 { TTestTIDatabaseTAB }

procedure TTestTIDatabaseTAB.CreateDatabase;
var
  LDir: string;
begin
  LDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(LDir);
  Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
  PersistenceLayer.DatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseTAB.DatabaseExists;
var
  LDir: string;
begin
  LDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(LDir);
  Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
  Check(not PersistenceLayer.DatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username,
    PerFrameworkSetup.Password),
    'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(LDir);
  Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
  Check(PersistenceLayer.DatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username,
    PerFrameworkSetup.Password),
    'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

class function TTestTIDatabaseTAB.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

{ TTestTIPersistenceLayersTab }

class function TTestTIPersistenceLayersTab.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

{ TTestTIQueryTAB }

class function TTestTIQueryTAB.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

{ TTestTIAutoMapOperationTab }

class function TTestTIAutoMapOperationTab.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

{ TTestTIOIDPersistentGUIDTab }

class function TTestTIOIDPersistentGUIDTab.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

{ TTestTIOIDPersistentIntegerTab }

class function TTestTIOIDPersistentIntegerTab.PersistenceLayerName: string;
begin
  Result := cTIPersistTab;
end;

end.
