unit tiOPFXMLLight_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQueryNonSQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersXMLLight = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseXMLLight = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryXMLLight = class(TTestTIQueryNonSQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationXMLLight = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDXMLLight = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerXMLLight = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;


procedure RegisterTests;

implementation

uses
  tiConstants,
  tiOPFTestManager,
  tiUtils,
  tiTestDependencies,
  tiQuery,
  {$IFDEF FPC}
  tiFPCUnitUtils,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  SysUtils;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersXMLLight);
  tiRegisterPersistenceTest(TTestTIDatabaseXMLLight);
  tiRegisterPersistenceTest(TTestTIQueryXMLLight);
//  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDXMLLight);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerXMLLight);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationXMLLight);
end;

 { TtiOPFTestSetupDataXMLLight }

 { TTestTIDatabaseXMLLight }

procedure TTestTIDatabaseXMLLight.CreateDatabase;
var
  LFileName: string;
  LDatabaseClass: TtiDatabaseClass;
begin
  LDatabaseClass:= PersistenceLayer.DatabaseClass;
  LFileName := TestSetupData.DBName;
  tiDeleteFile(LFileName);
  Check(not FileExists(LFileName), '<' + LFileName + '> Exists when it should not');
  LDatabaseClass.CreateDatabase(TestSetupData.DBName, TestSetupData.Username, TestSetupData.Password);
  Check(FileExists(LFileName), '<' + LFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXMLLight.DatabaseExists;
var
  LFileName: string;
  LDatabaseClass: TtiDatabaseClass;
begin
  LDatabaseClass:= PersistenceLayer.DatabaseClass;
  LFileName := TestSetupData.DBName;
  tiDeleteFile(LFileName);
  Check(not FileExists(LFileName), '<' + LFileName + '> Exists when it should not');
  Check(not LDatabaseClass.DatabaseExists(TestSetupData.DBName, TestSetupData.Username,
    TestSetupData.Password),
    'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test', LFileName);
  Check(FileExists(LFileName), '<' + LFileName + '> Does not exists when it should');
  Check(LDatabaseClass.DatabaseExists(TestSetupData.DBName, TestSetupData.Username,
    TestSetupData.Password),
    'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

class function TTestTIDatabaseXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

{ TTestTIQueryXMLLight }

class function TTestTIQueryXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

{ TTestTIPersistenceLayersXMLLight }

class function TTestTIPersistenceLayersXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

{ TTestTIAutoMapOperationXMLLight }

class function TTestTIAutoMapOperationXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

{ TTestTIOIDManagerXMLLight }

class function TTestTIOIDPersistentIntegerXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

{ TTestTIOIDPersistentGUIDXMLLight }

class function TTestTIOIDPersistentGUIDXMLLight.PersistenceLayerName: string;
begin
  Result := cTIPersistXMLLight;
end;

end.
