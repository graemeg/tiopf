unit tiOPFZeos_FB15_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersZeos = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIDatabaseZeos = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
  end;


  TTestTIQueryZeos = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationZeos = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentGUIDZeos = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerZeos = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;


procedure RegisterTests;


implementation

uses
  tiConstants,
  TestFramework,
  tiOPFTestManager,
  SysUtils,
  tiUtils,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersZeos);
  tiRegisterPersistenceTest(TTestTIDatabaseZeos);
  tiRegisterPersistenceTest(TTestTIQueryZeos);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDZeos);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerZeos);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationZeos);
end;

{ TTestTIDatabaseZeos }

procedure TTestTIDatabaseZeos.CreateDatabase;
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

procedure TTestTIDatabaseZeos.DatabaseExists;
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

class function TTestTIDatabaseZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIPersistenceLayersZeos }

class function TTestTIPersistenceLayersZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIQueryZeos }

class function TTestTIQueryZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIAutoMapOperationZeos }

class function TTestTIAutoMapOperationZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIOIDPersistentGUIDZeos }

class function TTestTIOIDPersistentGUIDZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIOIDPersistentIntegerZeos }

class function TTestTIOIDPersistentIntegerZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

end.
