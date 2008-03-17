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
  tiConstants,
  {$IFDEF FPC}
  tiFPCUnitUtils,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  tiOPFTestManager,
  tiTestFramework,
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
  lDB:       string;
  lDBExists: boolean;
begin
  lDB := ExpandFileName(PerFrameworkSetup.DBName);
  lDB := tiSwapExt(lDB, 'tmp');
  if FileExists(lDB) then
  begin
    tiDeleteFile(lDB);
    if FileExists(lDB) then
      Fail('Can not remove old database file');
  end;

//  Check(not FileExists(lDB), 'Database exists when it should not');
//  FDatabaseClass.CreateDatabase(
//    lDB,
//    PerFrameworkSetup.Username,
//    PerFrameworkSetup.Password);
//  Check(FileExists(lDB), 'Database not created');
//
//  lDBExists :=
//    FDatabaseClass.DatabaseExists(
//    lDB,
//    PerFrameworkSetup.Username,
//    PerFrameworkSetup.Password);
//
//  Check(lDBExists, 'Database does not exist when it should do');
//  tiDeleteFile(lDB);
end;

procedure TTestTIDatabaseIBO.DatabaseExists;
var
  lDB:       string;
  lDBExists: boolean;
begin
  lDB       := PerFrameworkSetup.DBName;
  Check(FileExists(lDB), 'Database file not found so test can not be performed');
//  lDBExists :=
//    FDatabaseClass.DatabaseExists(
//    PerFrameworkSetup.DBName,
//    PerFrameworkSetup.Username,
//    PerFrameworkSetup.Password);
//  Check(lDBExists, 'DBExists returned false when it should return true');
//  Check(not FileExists(lDB + 'Tmp'), 'Database file found so test can not be performed');
//  lDBExists :=
//    FDatabaseClass.DatabaseExists(
//    PerFrameworkSetup.DBName + 'Tmp',
//    PerFrameworkSetup.Username,
//    PerFrameworkSetup.Password);
//  Check(not lDBExists, 'DBExists returned true when it should return false');
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
