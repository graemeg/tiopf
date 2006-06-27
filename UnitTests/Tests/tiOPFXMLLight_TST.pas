unit tiOPFXMLLight_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_tst
  ;

type

  TtiOPFTestSetupDataXMLLight = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;

  TTestTIPersistenceLayersXMLLight = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;

  TTestTIDatabaseXMLLight = class( TTestTIDatabase )
  protected
    procedure Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure ThreadedDBConnectionPool ; override ;
  end;

  TTestTIQueryXMLLight = class( TTestTIQueryNonSQL )
  protected
    procedure   Setup; override;
  end;

  TTestTIClassToDBMapOperationXMLLight = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;

  TTestTIOIDManagerXMLLight = class(TTestTIOIDManager)
  protected
    procedure   Setup; override;
  end;


procedure RegisterTests;

implementation
uses
  tiConstants
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiDUnitDependencies
  ;

procedure RegisterTests ;
begin
  if gTIOPFTestManager.ToRun(cTIPersistXMLLight) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIPersistenceLayersXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIDatabaseXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIQueryXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIOIDManagerXMLLight.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXMLLight), TTestTIClassToDBMapOperationXMLLight.Suite);
  end;
end ;

{ TtiOPFTestSetupDataXMLLight }

constructor TtiOPFTestSetupDataXMLLight.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_XMLLIGHT}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistXMLLight ;
  FDBName   := ExpandFileName(ReadFromReg( cTIPersistXMLLight, 'DBName', gTestDataRoot + '_XMLLight.XML' )) ;
  FUsername := ReadFromReg( cTIPersistXMLLight, 'Username', 'null') ;
  FPassword := ReadFromReg( cTIPersistXMLLight, 'Password', 'null') ;
  FCanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseXMLLight }

procedure TTestTIDatabaseXMLLight.CreateDatabase;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXMLLight.DatabaseExists;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test',lFileName);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseXMLLight.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

procedure TTestTIDatabaseXMLLight.ThreadedDBConnectionPool;
begin
  LogWarning( 'The XMLLight persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestTIQueryXMLLight }

procedure TTestTIQueryXMLLight.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIPersistenceLayersXMLLight }

procedure TTestTIPersistenceLayersXMLLight.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIClassToDBMapOperationXMLLight }

procedure TTestTIClassToDBMapOperationXMLLight.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

{ TTestTIOIDManagerXMLLight }

procedure TTestTIOIDManagerXMLLight.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
  inherited;
end;

end.
