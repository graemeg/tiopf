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
  tiTestFramework;

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

  Check(not FileExists(lDB), 'Database exists when it should not');
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

procedure TTestTIDatabaseAsqlite3.DatabaseExists;
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
