unit tiOPFFIBP_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST,
  tiAutomapCriteria_TST;

type

  TTestTIPersistenceLayersFIBP = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseFIBP = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryFIBP = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationFIBP = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaFIBP = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDFIBP = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerFIBP = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersFIBP);
  tiRegisterPersistenceTest(TTestTIDatabaseFIBP);
  tiRegisterPersistenceTest(TTestTIQueryFIBP);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDFIBP);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerFIBP);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationFIBP);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaFIBP);
end;

{ TTestTIDatabaseFIBP }

procedure TTestTIDatabaseFIBP.CreateDatabase;
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

procedure TTestTIDatabaseFIBP.DatabaseExists;
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

class function TTestTIDatabaseFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestTIPersistenceLayersFIBP }

class function TTestTIPersistenceLayersFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestTIQueryFIBP }

class function TTestTIQueryFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestTIAutoMapOperationFIBP }

class function TTestTIAutoMapOperationFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestAutomappingCriteriaFIBP }

class function TTestAutomappingCriteriaFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestTIOIDPersistentGUIDFIBP }

class function TTestTIOIDPersistentGUIDFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

{ TTestTIOIDPersistentIntegerFIBP }

class function TTestTIOIDPersistentIntegerFIBP.PersistenceLayerName: string;
begin
  Result := cTIPersistFIBP;
end;

end.
