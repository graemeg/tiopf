unit tiOPFFBL_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersFBL = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseFBL = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
  end;


  TTestTIQueryFBL = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationFBL = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentGUIDFBL = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerFBL = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersFBL);
  tiRegisterPersistenceTest(TTestTIDatabaseFBL);
  tiRegisterPersistenceTest(TTestTIQueryFBL);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDFBL);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerFBL);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationFBL);
end;

{ TTestTIDatabaseFBL }

procedure TTestTIDatabaseFBL.CreateDatabase;
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

procedure TTestTIDatabaseFBL.DatabaseExists;
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

class function TTestTIDatabaseFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

{ TTestTIPersistenceLayersFBL }

class function TTestTIPersistenceLayersFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

{ TTestTIQueryFBL }

class function TTestTIQueryFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

{ TTestTIAutoMapOperationFBL }

class function TTestTIAutoMapOperationFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

{ TTestTIOIDPersistentGUIDFBL }

class function TTestTIOIDPersistentGUIDFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

{ TTestTIOIDPersistentIntegerFBL }

class function TTestTIOIDPersistentIntegerFBL.PersistenceLayerName: string;
begin
  Result := cTIPersistFBL;
end;

end.
