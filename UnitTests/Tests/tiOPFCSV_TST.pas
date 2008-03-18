unit tiOPFCSV_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQueryNonSQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersCSV = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseCSV = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryCSV = class(TTestTIQueryNonSQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationCSV = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDCSV = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerCSV = class(TTestTIOIDPersistentInteger)
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
  FileCtrl;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersCSV);
  tiRegisterPersistenceTest(TTestTIDatabaseCSV);
  tiRegisterPersistenceTest(TTestTIQueryCSV);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDCSV);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerCSV);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationCSV);
end;

{ TtiOPFTestSetupDataCSV }

procedure TTestTIDatabaseCSV.CreateDatabase;
var
  LDir: string;
begin
  LDir := PerFrameworkSetup.DBName;
  tiForceRemoveDir(LDir);
  Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
  PersistenceLayer.DatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseCSV.DatabaseExists;
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

class function TTestTIDatabaseCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

{ TTestTIPersistenceLayersCSV }

class function TTestTIPersistenceLayersCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

{ TTestTIQueryCSV }

class function TTestTIQueryCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

{ TTestTIAutoMapOperationCSV }

class function TTestTIAutoMapOperationCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

{ TTestTIOIDPersistentGUIDCSV }

class function TTestTIOIDPersistentGUIDCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

{ TTestTIOIDPersistentIntegerCSV }

class function TTestTIOIDPersistentIntegerCSV.PersistenceLayerName: string;
begin
  Result := cTIPersistCSV;
end;

end.
