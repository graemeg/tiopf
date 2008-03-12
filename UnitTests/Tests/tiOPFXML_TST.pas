unit tiOPFXML_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQueryNonSQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersXML = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseXML = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryXML = class(TTestTIQueryNonSQL)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure FieldByNameVSFieldByIndex; override;
  end;

  TTestTIAutoMapOperationXM = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDXML = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerXML = class(TTestTIOIDPersistentInteger)
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
  tiRegisterPersistenceTest(TTestTIPersistenceLayersXML);
  tiRegisterPersistenceTest(TTestTIDatabaseXML);
  tiRegisterPersistenceTest(TTestTIQueryXML);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationXM);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDXML);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerXML);
end;

{ TTestTIDatabaseXML }

procedure TTestTIDatabaseXML.CreateDatabase;
var
  lFileName: string;
begin
  lFileName := PerFrameworkSetup.DBName;
  tiDeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXML.DatabaseExists;
var
  lFileName: string;
begin
  SetAllowedLeakArray([24]);
  lFileName := PerFrameworkSetup.DBName;
  tiDeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username,
    PerFrameworkSetup.Password),
    'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test', lFileName);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username,
    PerFrameworkSetup.Password),
    'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

class function TTestTIDatabaseXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

procedure TTestTIQueryXML.FieldByNameVSFieldByIndex;
begin
  Check(True); // Dont test because it will always fail.
end;

{ TTestTIPersistenceLayersXML }

class function TTestTIPersistenceLayersXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

class function TTestTIQueryXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIAutoMapOperationXM }

class function TTestTIAutoMapOperationXM.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIOIDPersistentGUIDXML }

class function TTestTIOIDPersistentGUIDXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIOIDPersistentIntegerXML }

class function TTestTIOIDPersistentIntegerXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

end.
