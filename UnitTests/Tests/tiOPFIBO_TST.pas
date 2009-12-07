unit tiOPFIBO_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersIBO = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseIBO = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryIBO = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationIBO = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDIBO = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerIBO = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;

procedure RegisterTests;

implementation

uses
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersIBO);
  tiRegisterPersistenceTest(TTestTIDatabaseIBO);
  tiRegisterPersistenceTest(TTestTIQueryIBO);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDIBO);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerIBO);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationIBO);
end;

{ TTestTIDatabaseIBO }

procedure TTestTIDatabaseIBO.CreateDatabase;
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

procedure TTestTIDatabaseIBO.DatabaseExists;
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

class function TTestTIDatabaseIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

{ TTestTIPersistenceLayersIBO }

class function TTestTIPersistenceLayersIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

{ TTestTIQueryIBO }

class function TTestTIQueryIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

{ TTestTIAutoMapOperationIBO }

class function TTestTIAutoMapOperationIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

{ TTestTIOIDPersistentGUIDIBO }

class function TTestTIOIDPersistentGUIDIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

{ TTestTIOIDPersistentIntegerIBO }

class function TTestTIOIDPersistentIntegerIBO.PersistenceLayerName: string;
begin
  Result := cTIQueryTestName;
end;

end.
