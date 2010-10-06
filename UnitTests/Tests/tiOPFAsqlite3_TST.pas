unit tiOPFAsqlite3_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersAsqlite3 = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseAsqlite3 = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryAsqlite3 = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationAsqlite3 = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaAsqlite3 = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDAsqlite3 = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerAsqlite3 = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;

procedure RegisterTests;

implementation

uses
  tiConstants,
  TestFramework,
  {$IFNDEF FPC}
  FileCtrl,
  {$ENDIF}
  tiOPFTestManager,
  SysUtils,
  tiUtils,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersAsqlite3);
  tiRegisterPersistenceTest(TTestTIDatabaseAsqlite3);
  tiRegisterPersistenceTest(TTestTIQueryAsqlite3);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDAsqlite3);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerAsqlite3);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationAsqlite3);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaAsqlite3);
end;

procedure TTestTIDatabaseAsqlite3.CreateDatabase;
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

procedure TTestTIDatabaseAsqlite3.DatabaseExists;
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

class function TTestTIDatabaseAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestTIPersistenceLayersAsqlite3 }

class function TTestTIPersistenceLayersAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestTIQueryAsqlite3 }

class function TTestTIQueryAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestTIAutoMapOperationAsqlite3 }

class function TTestTIAutoMapOperationAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestAutomappingCriteriaAsqlite3 }

class function TTestAutomappingCriteriaAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestTIOIDPersistentGUIDAsqlite3 }

class function TTestTIOIDPersistentGUIDAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

{ TTestTIOIDPersistentIntegerAsqlite3 }

class function TTestTIOIDPersistentIntegerAsqlite3.PersistenceLayerName: string;
begin
  Result := cTIPersistAsqlite3;
end;

end.
