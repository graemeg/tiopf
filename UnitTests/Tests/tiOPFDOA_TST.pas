unit tiOPFDOA_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersDOA = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseDOA = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryDOA = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationDOA = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDDOA = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerDOA = class(TTestTIOIDPersistentInteger)
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
  SysUtils,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersDOA);
  tiRegisterPersistenceTest(TTestTIDatabaseDOA);
  tiRegisterPersistenceTest(TTestTIQueryDOA);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDDOA);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerDOA);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationDOA);
end;

{ TtiOPFTestSetupDataDOA }

procedure TTestTIDatabaseDOA.CreateDatabase;
begin
//  try
//    FDatabaseClass.CreateDatabase(TestSetupData.DBName, TestSetupData.Username, TestSetupData.Password);
//    Fail('Exception not raised when it should have been');
//  except
//    on e: Exception do
//    begin
//      CheckIs(e, EAssertionFailed);
//      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message) <> 0);
//    end;
//  end;
end;

procedure TTestTIDatabaseDOA.DatabaseExists;
begin
//  try
//    FDatabaseClass.DatabaseExists(TestSetupData.DBName, TestSetupData.Username, TestSetupData.Password);
//    Fail('Exception not raised when it should have been');
//  except
//    on e: Exception do
//    begin
//      CheckIs(e, EAssertionFailed);
//      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message) <> 0);
//    end;
//  end;
end;

class function TTestTIDatabaseDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

{ TTestTIPersistenceLayersDOA }

class function TTestTIPersistenceLayersDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

{ TTestTIQueryDOA }

class function TTestTIQueryDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

{ TTestTIAutoMapOperationDOA }

class function TTestTIAutoMapOperationDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

{ TTestTIOIDPersistentGUIDDOA }

class function TTestTIOIDPersistentGUIDDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

{ TTestTIOIDPersistentIntegerDOA }

class function TTestTIOIDPersistentIntegerDOA.PersistenceLayerName: string;
begin
  Result := cTIPersistDOA;
end;

end.
