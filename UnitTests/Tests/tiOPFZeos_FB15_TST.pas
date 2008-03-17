unit tiOPFZeos_FB15_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersZeos = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIDatabaseZeos = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
  end;


  TTestTIQueryZeos = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationZeos = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentGUIDZeos = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerZeos = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersZeos);
  tiRegisterPersistenceTest(TTestTIDatabaseZeos);
  tiRegisterPersistenceTest(TTestTIQueryZeos);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDZeos);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerZeos);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationZeos);
end;

{ TTestTIDatabaseZeos }

procedure TTestTIDatabaseZeos.CreateDatabase;
var
  lDB:       string;
  lDBExists: boolean;
begin
//  lDB := ExpandFileName(PerFrameworkSetup.DBName);
//  lDB := tiSwapExt(lDB, 'tmp');
//  if FileExists(lDB) then
//  begin
//    tiDeleteFile(lDB);
//    if FileExists(lDB) then
//      Fail('Can not remove old database file');
//  end;
//
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

procedure TTestTIDatabaseZeos.DatabaseExists;
var
  lDB:       string;
  lDBExists: boolean;
begin
//  Exit;
//  lDB       := PerFrameworkSetup.DBName;
//  Check(FileExists(lDB), 'Database file not found so test can not be performed');
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

class function TTestTIDatabaseZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIPersistenceLayersZeos }

class function TTestTIPersistenceLayersZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIQueryZeos }

class function TTestTIQueryZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIAutoMapOperationZeos }

class function TTestTIAutoMapOperationZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIOIDPersistentGUIDZeos }

class function TTestTIOIDPersistentGUIDZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

{ TTestTIOIDPersistentIntegerZeos }

class function TTestTIOIDPersistentIntegerZeos.PersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB15;
end;

end.
