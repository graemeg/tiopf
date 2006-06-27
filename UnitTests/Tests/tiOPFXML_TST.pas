unit tiOPFXML_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiOPFTestManager
  ,tiTestFramework
  ,tiClassToDBMap_TST
  ,tiOID_tst
  ;

type

  TtiOPFTestSetupDataXML = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;

  TTestTIPersistenceLayersXML = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;

  TTestTIDatabaseXML = class( TTestTIDatabase )
  protected
    procedure   Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure ThreadedDBConnectionPool ; override ;
  end;

  TTestTIQueryXML = class( TTestTIQueryNonSQL )
  protected
    procedure   Setup; override;
  published
    procedure FieldByNameVSFieldByIndex; override;
  end;

  TTestTIClassToDBMapOperationXM = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;

  TTestTIOIDManagerXML = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistXML) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIPersistenceLayersXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIDatabaseXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIQueryXML.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIClassToDBMapOperationXM.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistXML), TTestTIOIDManagerXML.Suite);
  end;
end ;

{ TtiOPFTestSetupDataXML }

constructor TtiOPFTestSetupDataXML.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_XML}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistXML ;
  FDBName   := ExpandFileName(ReadFromReg( cTIPersistXML, 'DBName', gTestDataRoot + '_MSXML.xml' )) ;
  FUsername := ReadFromReg( cTIPersistXML, 'Username', 'null') ;
  FPassword := ReadFromReg( cTIPersistXML, 'Password', 'null') ;
  FCanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseXML }

procedure TTestTIDatabaseXML.CreateDatabase;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXML.DatabaseExists;
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

procedure TTestTIDatabaseXML.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

procedure TTestTIDatabaseXML.ThreadedDBConnectionPool;
begin
  LogWarning( 'The XML persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TtiOPFTestSetupDecoratorXML }

{ TTestTIQueryXML }

procedure TTestTIQueryXML.FieldByNameVSFieldByIndex;
begin
  Check(True); // Dont test because it will always fail.
end;

procedure TTestTIQueryXML.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIClassToDBMapOperationXM }

procedure TTestTIClassToDBMapOperationXM.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIOIDManagerXML }

procedure TTestTIOIDManagerXML.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

{ TTestTIPersistenceLayersXML }

procedure TTestTIPersistenceLayersXML.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistXML);
  inherited;
end;

end.
