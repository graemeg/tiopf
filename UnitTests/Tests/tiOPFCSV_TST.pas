unit tiOPFCSV_TST;

{$I tiDefines.inc}
{$IFDEF FPC}
   {$Define STATIC_PERLAYER_LINKING}
{$ENDIF}

interface
uses
   tiQuery_TST
  ,tiQueryNonSQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_tst
  ;

type

  TtiOPFTestSetupDataCSV = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;
  

  TTestTIPersistenceLayersCSV = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;
  

  TTestTIDatabaseCSV = class( TTestTIDatabase )
  protected
    procedure   Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure ThreadedDBConnectionPool ; override ;
  end;
  

  TTestTIQueryCSV = class( TTestTIQueryNonSQL )
  protected
    procedure   Setup; override;
  end;
  

  TTestTIClassToDBMapOperationCSV = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;
  

  TTestTIOIDManagerCSV = class(TTestTIOIDManager)
  protected
    procedure   Setup; override;
  end;
  

procedure RegisterTests;


implementation
uses
  tiConstants
  {$IFDEF FPC}
  ,testregistry
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiDUnitDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ;

procedure RegisterTests ;
begin
  if gTIOPFTestManager.ToRun(cTIPersistCSV) then
  begin
    {$IFDEF FPC}
    RegisterTest(TTestTIPersistenceLayersCSV);
    RegisterTest(TTestTIDatabaseCSV);
    RegisterTest(TTestTIQueryCSV);
    RegisterTest(TTestTIOIDManagerCSV);
    RegisterTest(TTestTIClassToDBMapOperationCSV);
    {$ELSE}
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIPersistenceLayersCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIDatabaseCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIQueryCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIOIDManagerCSV.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistCSV), TTestTIClassToDBMapOperationCSV.Suite);
    {$ENDIF}
  end;
end ;

{ TtiOPFTestSetupDataCSV }

constructor TtiOPFTestSetupDataCSV.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_CSV}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistCSV ;
  FDBName   := ExpandFileName( ReadFromReg( cTIPersistCSV, 'DBName', gTestDataRoot + 'CSV' )) ;
  FUserName := ReadFromReg( cTIPersistCSV, 'UserName', 'null') ;
  FPassword := ReadFromReg( cTIPersistCSV, 'Password', 'null') ;
  FCanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

procedure TTestTIDatabaseCSV.CreateDatabase;
var
  lDir : string ;
begin
  lDir := PerFrameworkSetup.DBName ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseCSV.DatabaseExists;
var
  lDir : string ;
begin
  lDir := PerFrameworkSetup.DBName ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseCSV.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

procedure TTestTIDatabaseCSV.ThreadedDBConnectionPool;
begin
  LogWarning( 'The CSV persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TtiOPFTestSetupDecoratorCSV }

{ TTestTIPersistenceLayersCSV }

procedure TTestTIPersistenceLayersCSV.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIQueryCSV }

procedure TTestTIQueryCSV.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIClassToDBMapOperationCSV }

procedure TTestTIClassToDBMapOperationCSV.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

{ TTestTIOIDManagerCSV }

procedure TTestTIOIDManagerCSV.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistCSV);
  inherited;
end;

end.
