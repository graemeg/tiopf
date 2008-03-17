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
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerFBL);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationFBL);
end;

{ TTestTIDatabaseFBL }

procedure TTestTIDatabaseFBL.CreateDatabase;
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

procedure TTestTIDatabaseFBL.DatabaseExists;
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
