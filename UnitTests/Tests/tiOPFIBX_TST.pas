unit tiOPFIBX_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST,
  tiAutomapCriteria_TST;

type

  TTestTIPersistenceLayersIBX = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseIBX = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryIBX = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationIBX = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaIBX = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDIBX = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerIBX = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersIBX);
  tiRegisterPersistenceTest(TTestTIDatabaseIBX);
  tiRegisterPersistenceTest(TTestTIQueryIBX);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDIBX);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerIBX);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationIBX);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaIBX);
end;

{ TTestTIDatabaseIBX }

procedure TTestTIDatabaseIBX.CreateDatabase;
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

procedure TTestTIDatabaseIBX.DatabaseExists;
var
  LDB:       string;
  LDBExists: boolean;
begin
  SetAllowedLeakArray([504]);
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

class function TTestTIDatabaseIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestTIPersistenceLayersIBX }

class function TTestTIPersistenceLayersIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestTIQueryIBX }

class function TTestTIQueryIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestTIAutoMapOperationIBX }

class function TTestTIAutoMapOperationIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestAutomappingCriteriaIBX }

class function TTestAutomappingCriteriaIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestTIOIDPersistentGUIDIBX }

class function TTestTIOIDPersistentGUIDIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

{ TTestTIOIDPersistentIntegerIBX }

class function TTestTIOIDPersistentIntegerIBX.PersistenceLayerName: string;
begin
  Result := cTIPersistIBX;
end;

end.
