unit tiOPFADOAccess_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_tst
  ;

type

  TtiOPFTestSetupDataADOAccess = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;

  TTestTIPersistenceLayersADOAccess = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;

  // ThreadedDBConnectionPool will hang for the second call, but only after
  // a call to CreateDatabase. Have seen this on Win2K but not on XP
  // Investigate...
  TTestTIDatabaseADOAccess = class( TTestTIDatabase )
  protected
    procedure   Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure ThreadedDBConnectionPool ; override ;
  end;

  TTestTIQueryADOAccess = class(TTestTIQuerySQL)
  protected
    procedure Setup; override;
  end;

  TTestTIClassToDBMapOperationADOAccess = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;

  TTestTIOIDManagerADOAccess = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistADOAccess) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIPersistenceLayersADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIDatabaseADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIQueryADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIOIDManagerADOAccess.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOAccess), TTestTIClassToDBMapOperationADOAccess.Suite);
  end;
end ;

{ TtiOPFTestSetupDataADOAccess }

constructor TtiOPFTestSetupDataADOAccess.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_ADOACCESS}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName  := cTIPersistADOAccess ;
  FDBName   := ReadFromReg( cTIPersistADOAccess, 'DBUserName', gTestDataRoot + '.mdb' ) ;
  FUserName := ReadFromReg( cTIPersistADOAccess, 'UserName', 'null' ) ;
  FPassword := ReadFromReg( cTIPersistADOAccess, 'Password', 'null' ) ;
  FCanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseADOAccess }

procedure TTestTIDatabaseADOAccess.CreateDatabase;
var
  lDB : string ;
  lDBExists : boolean ;
begin
  lDB := ExpandFileName( PerFrameworkSetup.DBName ) ;
  lDB := tiSwapExt( lDB, 'tmp' ) ;
  if FileExists( lDB ) then
  begin
    SysUtils.DeleteFile( lDB ) ;
    if FileExists( lDB ) then
      Fail( 'Can not remove old database file' ) ;
  end ;

  Check( not FileExists( lDB ), 'Database exists when it should not' ) ;
  FDatabaseClass.CreateDatabase(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) ;
  Check( FileExists( lDB ), 'Database not created' ) ;

  lDBExists :=
    FDatabaseClass.DatabaseExists(
      lDB,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;

  Check( lDBExists, 'Database does not exist when it should do' ) ;
  SysUtils.DeleteFile( lDB ) ;
end;

procedure TTestTIDatabaseADOAccess.DatabaseExists;
var
  lDB : string ;
  lDBExists : boolean ;
begin
  lDB := PerFrameworkSetup.DBName ;
  Check( FileExists( lDB ), 'Database file not found so test can not be performed' ) ;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
  Check( lDBExists, 'DBExists returned false when it should return true' ) ;
  Check( not FileExists( lDB + 'Tmp' ), 'Database file found so test can not be performed' ) ;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName + 'Tmp',
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
  Check( not lDBExists, 'DBExists returned true when it should return false' ) ;
end;

{ TtiOPFTestSetupDecoratorADOAccess }

procedure TTestTIDatabaseADOAccess.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

procedure TTestTIDatabaseADOAccess.ThreadedDBConnectionPool;
begin
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestTIQueryADOAccess }

procedure TTestTIQueryADOAccess.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIPersistenceLayersADOAccess }

procedure TTestTIPersistenceLayersADOAccess.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIClassToDBMapOperationADOAccess }

procedure TTestTIClassToDBMapOperationADOAccess.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

{ TTestTIOIDManagerADOAccess }

procedure TTestTIOIDManagerADOAccess.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOAccess);
  inherited;
end;

end.
