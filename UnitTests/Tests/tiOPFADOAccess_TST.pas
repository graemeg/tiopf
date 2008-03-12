unit tiOPFADOAccess_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersADOAccess = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseADOAccess = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryADOAccess = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationADOAccess = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaADOAccess = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDADOAccess = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerADOAccess = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersADOAccess);
  tiRegisterPersistenceTest(TTestTIDatabaseADOAccess);
  tiRegisterPersistenceTest(TTestTIQueryADOAccess);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDADOAccess);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerADOAccess);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationADOAccess);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaADOAccess);
end;

{ TTestTIDatabaseADOAccess }

procedure TTestTIDatabaseADOAccess.CreateDatabase;
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
  FDatabaseClass.CreateDatabase(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(FileExists(lDB), 'Database not created');

  lDBExists :=
    FDatabaseClass.DatabaseExists(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);

  Check(lDBExists, 'Database does not exist when it should do');
  tiDeleteFile(lDB);
end;

procedure TTestTIDatabaseADOAccess.DatabaseExists;
var
  lDB:       string;
  lDBExists: boolean;
begin
  SetAllowedLeakArray([24]);
  lDB       := PerFrameworkSetup.DBName;
  Check(FileExists(lDB), 'Database file not found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(lDB + 'Tmp'), 'Database file found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName + 'Tmp',
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

class function TTestTIDatabaseADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIPersistenceLayersADOAccess }

class function TTestTIPersistenceLayersADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIQueryADOAccess }

class function TTestTIQueryADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIAutoMapOperationADOAccess }

class function TTestTIAutoMapOperationADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestAutomappingCriteriaADOAccess }

class function TTestAutomappingCriteriaADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIOIDPersistentGUIDADOAccess }

class function TTestTIOIDPersistentGUIDADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

{ TTestTIOIDPersistentIntegerADOAccess }

class function TTestTIOIDPersistentIntegerADOAccess.PersistenceLayerName: string;
begin
  Result := cTIPersistADOAccess;
end;

end.
