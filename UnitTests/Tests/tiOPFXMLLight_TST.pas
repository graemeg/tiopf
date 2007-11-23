unit tiOPFXMLLight_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiAutoMap_TST
  ,tiOID_TST
 ;

type

  TTestTIPersistenceLayersXMLLight = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseXMLLight = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryXMLLight = class(TTestTIQueryNonSQL)
  protected
    procedure   SetUp; override;
  end;

  TTestTIAutoMapOperationXMLLight = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerXMLLight = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
  end;


procedure RegisterTests;

implementation
uses
  tiConstants
  {$IFDEF FPC}
  ,tiFPCUnitUtils
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ,tiOPFTestManager
  ,SysUtils
  ,tiUtils
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistXMLLight) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIPersistenceLayersXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIDatabaseXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIQueryXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIOIDManagerXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIAutoMapOperationXMLLight.Suite);
  end;
end;

{ TtiOPFTestSetupDataXMLLight }

{ TTestTIDatabaseXMLLight }

procedure TTestTIDatabaseXMLLight.CreateDatabase;
var
  lFileName : string;
begin
  lFileName := PerFrameworkSetup.DBName;
  tiDeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXMLLight.DatabaseExists;
var
  lFileName : string;
begin
  lFileName := PerFrameworkSetup.DBName;
  tiDeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test',lFileName);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseXMLLight.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIQueryXMLLight }

procedure TTestTIQueryXMLLight.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIPersistenceLayersXMLLight }

procedure TTestTIPersistenceLayersXMLLight.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIAutoMapOperationXMLLight }

procedure TTestTIAutoMapOperationXMLLight.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIOIDManagerXMLLight }

procedure TTestTIOIDManagerXMLLight.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

end.


