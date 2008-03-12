unit tiOPFCrSdac_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersCrSdac = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseCrSdac = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryCrSdac = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationCrSdac = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure ReadWriteDateMin; override;
  end;

  TTestAutomappingCriteriaCrSdac = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDCrSdac = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerCrSdac = class(TTestTIOIDPersistentInteger)
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
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersCrSdac);
  tiRegisterPersistenceTest(TTestTIDatabaseCrSdac);
  tiRegisterPersistenceTest(TTestTIQueryCrSdac);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDCrSdac);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerCrSdac);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationCrSdac);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaCrSdac);
end;

procedure TTestTIDatabaseCrSdac.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
    Fail('Exception not raised when it should have been');
  except
    on e: Exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message) <> 0);
    end;
  end;
end;

procedure TTestTIDatabaseCrSdac.DatabaseExists;
var
  lDB:       string;
  lDBExists: boolean;
begin
  lDB       := PerFrameworkSetup.DBName;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');

  lDBExists :=
    FDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName + 'Tmp',
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

class function TTestTIDatabaseCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

class function TTestTIAutoMapOperationCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

procedure TTestTIAutoMapOperationCrSdac.ReadWriteDateMin;
begin
  // SQL server has a non standard minimum date
  DoReadWriteDateTime(EncodeDate(1753, 1, 1));
end;

{ TTestTIPersistenceLayersCrSdac }

class function TTestTIPersistenceLayersCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

{ TTestTIQueryCrSdac }

class function TTestTIQueryCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

{ TTestAutomappingCriteriaCrSdac }

class function TTestAutomappingCriteriaCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

{ TTestTIOIDPersistentGUIDCrSdac }

class function TTestTIOIDPersistentGUIDCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

{ TTestTIOIDPersistentIntegerCrSdac }

class function TTestTIOIDPersistentIntegerCrSdac.PersistenceLayerName: string;
begin
  Result := cTIPersistCrSdac;
end;

end.
