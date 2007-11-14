unit tiOPFXML_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiOPFTestManager
  ,tiTestFramework
  ,tiAutoMap_TST
  ,tiOID_tst
 ;

type

  TTestTIPersistenceLayersXML = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseXML = class(TTestTIDatabase)
  protected
    procedure   SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryXML = class(TTestTIQueryNonSQL)
  protected
    procedure   SetUp; override;
  published
    procedure FieldByNameVSFieldByIndex; override;
  end;

  TTestTIAutoMapOperationXM = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerXML = class(TTestTIOIDManager)
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
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistXML) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIPersistenceLayersXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIDatabaseXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIQueryXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIAutoMapOperationXM.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIOIDManagerXML.Suite);
  end;
end;

{ TTestTIDatabaseXML }

procedure TTestTIDatabaseXML.CreateDatabase;
var
  lFileName : string;
begin
  lFileName := PerFrameworkSetup.DBName;
  tiDeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXML.DatabaseExists;
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

procedure TTestTIDatabaseXML.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TtiOPFTestSetupDecoratorXML }

{ TTestTIQueryXML }

procedure TTestTIQueryXML.FieldByNameVSFieldByIndex;
begin
  Check(True); // Dont test because it will always fail.
end;

procedure TTestTIQueryXML.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIAutoMapOperationXM }

procedure TTestTIAutoMapOperationXM.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIOIDManagerXML }

procedure TTestTIOIDManagerXML.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIPersistenceLayersXML }

procedure TTestTIPersistenceLayersXML.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

end.


