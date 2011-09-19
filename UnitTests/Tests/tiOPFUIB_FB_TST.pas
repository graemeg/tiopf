unit tiOPFUIB_FB_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersUIB = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseUIB = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryUIB = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationUIB = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDUIB = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerUIB = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;

procedure RegisterTests;

implementation

uses
  TestFramework,
  tiOPFTestManager,
  SysUtils,
  tiUtils,
  tiTestDependencies,
  tiConstants;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersUIB);
  tiRegisterPersistenceTest(TTestTIDatabaseUIB);
  tiRegisterPersistenceTest(TTestTIQueryUIB);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDUIB);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerUIB);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationUIB);
end;

{ TTestTIDatabaseUIB }

procedure TTestTIDatabaseUIB.CreateDatabase;
var
  LDB:       string;
  LDBExists: boolean;
begin
  LDB := ExpandFileName(TestSetupData.DBName);
  LDB := tiSwapExt(LDB, 'tmp');
  if FileExists(LDB) then
  begin
    tiDeleteFile(LDB);
    if FileExists(LDB) then
      Fail('Can not remove old database file');
  end;

  Check(not FileExists(LDB), 'Database exists when it should not');
  PersistenceLayer.DatabaseClass.CreateDatabase(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(FileExists(LDB), 'Database not created');

  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    LDB,
    TestSetupData.Username,
    TestSetupData.Password);

  Check(LDBExists, 'Database does not exist when it should do');
  tiDeleteFile(LDB);
end;

procedure TTestTIDatabaseUIB.DatabaseExists;
var
  LDB:       string;
  LDBExists: boolean;
begin
  LDB       := TestSetupData.DBName;
  Check(FileExists(LDB), 'Database file not found so test can not be performed');
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password);
  Check(LDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(LDB + 'Tmp'), 'Database file found so test can not be performed');
  LDBExists :=
    PersistenceLayer.DatabaseClass.DatabaseExists(
    TestSetupData.DBName + 'Tmp',
    TestSetupData.Username,
    TestSetupData.Password);
  Check(not LDBExists, 'DBExists returned true when it should return false');
end;

class function TTestTIDatabaseUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

{ TTestTIPersistenceLayersUIB }

class function TTestTIPersistenceLayersUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

{ TTestTIQueryUIB }

class function TTestTIQueryUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

{ TTestTIAutoMapOperationUIB }

class function TTestTIAutoMapOperationUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

{ TTestTIOIDPersistentGUIDUIB }

class function TTestTIOIDPersistentGUIDUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

{ TTestTIOIDPersistentIntegerUIB }

class function TTestTIOIDPersistentIntegerUIB.PersistenceLayerName: string;
begin
  Result := cTIPersistUIB_FB;
end;

end.
