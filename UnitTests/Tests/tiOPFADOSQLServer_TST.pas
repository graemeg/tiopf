unit tiOPFADOSQLServer_TST;

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

  TtiOPFTestSetupDataADOSQLServer = class(TtiOPFTestSetupData)
  public
    constructor Create; override;
  end;

  TTestTIPersistenceLayersADOSQLServer = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseADOSQLServer = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryADOSQLServer = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;

  TTestTIClassToDBMapOperationADOSQLServer = class(TTestTIClassToDBMapOperation)
  protected
    procedure   SetUp; override;
  end;

  TTestTIOIDManagerADOSQLServer = class(TTestTIOIDManager)
  protected
    procedure   SetUp; override;
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

procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistADOSQLServer) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIPersistenceLayersADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIDatabaseADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIQueryADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIOIDManagerADOSQLServer.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistADOSQLServer), TTestTIClassToDBMapOperationADOSQLServer.Suite);
  end;
end;

{ TtiOPFTestSetupDataADOSQLServer }

constructor TtiOPFTestSetupDataADOSQLServer.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_ADOSQLSERVER}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistADOSQLServer;
  FDBName  := ReadFromReg(cTIPersistADOSQLServer, 'DBName', 'EnterValueHere');
  FUserName := ReadFromReg(cTIPersistADOSQLServer, 'UserName', 'null');
  FPassword := ReadFromReg(cTIPersistADOSQLServer, 'Password', 'null');
  FCanCreateDatabase := false;
end;

procedure TTestTIDatabaseADOSQLServer.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end;
  end;
end;

procedure TTestTIDatabaseADOSQLServer.DatabaseExists;
begin
  try
    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end;
  end;
end;

{ TTestTIPersistenceLayersADOSQLServer }

procedure TTestTIPersistenceLayersADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

procedure TTestTIDatabaseADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIQueryADOSQLServer }

procedure TTestTIQueryADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIClassToDBMapOperationADOSQLServer }

procedure TTestTIClassToDBMapOperationADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

{ TTestTIOIDManagerADOSQLServer }

procedure TTestTIOIDManagerADOSQLServer.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistADOSQLServer);
  inherited;
end;

end.
