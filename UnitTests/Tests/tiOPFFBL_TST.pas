unit tiOPFFBL_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiAutoMap_TST
  ,tiOID_TST
 ;


type

  TTestTIPersistenceLayersFBL = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;

  TTestTIDatabaseFBL = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
    procedure CreateDatabase; override;
  published
    procedure DatabaseExists; override;
  end;


  TTestTIQueryFBL = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;


  TTestTIAutoMapOperationFBL = class(TTestTIAutoMapOperation)
  protected
    procedure   SetUp; override;
  end;


  TTestTIOIDManagerFBL = class(TTestTIOIDManager)
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
  ,tiDUnitDependencies
 ;
  
procedure RegisterTests;
begin
  if gTIOPFTestManager.ToRun(cTIPersistFBL) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIPersistenceLayersFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIDatabaseFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIQueryFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIOIDManagerFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIAutoMapOperationFBL.Suite);
  end;
end;

{ TTestTIDatabaseFBL }

procedure TTestTIDatabaseFBL.CreateDatabase;
var
  lDB : string;
  lDBExists : boolean;
begin
  lDB := ExpandFileName(PerFrameworkSetup.DBName);
  lDB := tiSwapExt(lDB, 'tmp');
  if FileExists(lDB) then
  begin
    tiDeleteFile(lDB);
    if FileExists(lDB) then
      Fail('Can not remove old database file');
  end;

  Check(not FileExists(lDB), 'Database exists when it should not');
  FDatabaseClass.CreateDatabase(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password);
  Check(FileExists(lDB), 'Database not created');

  lDBExists :=
    FDatabaseClass.DatabaseExists(
      lDB,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);

  Check(lDBExists, 'Database does not exist when it should do');
  tiDeleteFile(lDB);
end;

procedure TTestTIDatabaseFBL.DatabaseExists;
var
  lDB : string;
  lDBExists : boolean;
begin
  lDB := PerFrameworkSetup.DBName;
  Check(FileExists(lDB), 'Database file not found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(lDBExists, 'DBExists returned false when it should return true');
  Check(not FileExists(lDB + 'Tmp'), 'Database file found so test can not be performed');
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName + 'Tmp',
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
  Check(not lDBExists, 'DBExists returned true when it should return false');
end;

procedure TTestTIDatabaseFBL.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistFBL);
  inherited;
end;

{ TTestTIPersistenceLayersFBL }

procedure TTestTIPersistenceLayersFBL.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistFBL);
  inherited;
end;

{ TTestTIQueryFBL }

procedure TTestTIQueryFBL.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistFBL);
  inherited;
end;


{ TTestTIAutoMapOperationFBL }

procedure TTestTIAutoMapOperationFBL.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistFBL);
  inherited;
end;

{ TTestTIOIDManagerFBL }

procedure TTestTIOIDManagerFBL.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistFBL);
  inherited;
end;

end.
