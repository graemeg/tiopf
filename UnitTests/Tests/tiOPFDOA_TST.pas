unit tiOPFDOA_TST;

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

  TtiOPFTestSetupDataDOA = class( TtiOPFTestSetupData )
  public
    constructor Create ; override ;
  end ;

  TTestTIPersistenceLayersDOA = class( TTestTIPersistenceLayers )
  protected
    procedure Setup; override;
  end;

  TTestTIDatabaseDOA = class( TTestTIDatabase )
  protected
    procedure   Setup; override;
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIQueryDOA = class( TTestTIQuerySQL )
  protected
    procedure   Setup; override;
  end;

  TTestTIClassToDBMapOperationDOA = class(TTestTIClassToDBMapOperation)
  protected
    procedure   Setup; override;
  end;

  TTestTIOIDManagerDOA = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistDOA) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIPersistenceLayersDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIDatabaseDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIQueryDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIOIDManagerDOA.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistDOA), TTestTIClassToDBMapOperationDOA.Suite);
  end;
end ;

{ TtiOPFTestSetupDataDOA }

constructor TtiOPFTestSetupDataDOA.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_DOA}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected:= FEnabled;
  FPerLayerName := cTIPersistDOA ;
  FDBName   := ReadFromReg( cTIPersistDOA, 'DBName',   'orcl' ) ;
  FUserName := ReadFromReg( cTIPersistDOA, 'UserName', 'scott') ;
  FPassword := ReadFromReg( cTIPersistDOA, 'Password', 'tiger') ;
  FCanCreateDatabase := false ;
end;

procedure TTestTIDatabaseDOA.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

procedure TTestTIDatabaseDOA.DatabaseExists;
begin
  try
    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

{ TTestTIPersistenceLayersDOA }

procedure TTestTIPersistenceLayersDOA.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

procedure TTestTIDatabaseDOA.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIQueryDOA }

procedure TTestTIQueryDOA.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIClassToDBMapOperationDOA }

procedure TTestTIClassToDBMapOperationDOA.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

{ TTestTIOIDManagerDOA }

procedure TTestTIOIDManagerDOA.Setup;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistDOA);
  inherited;
end;

end.
