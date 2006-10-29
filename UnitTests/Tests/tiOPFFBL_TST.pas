unit tiOPFFBL_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiOPFTestManager
  ,tiClassToDBMap_TST
  ,tiOID_TST
 ;


type
  TtiOPFTestSetupDataFBL = class(TtiOPFTestSetupData)
  public
    constructor Create; override;
  end;


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


  TTestTIClassToDBMapOperationFBL = class(TTestTIClassToDBMapOperation)
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
  ,TestRegistry
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
    {$IFDEF FPC}
    RegisterTest(TTestTIPersistenceLayersFBL);
    RegisterTest(TTestTIDatabaseFBL);
    RegisterTest(TTestTIQueryFBL);
    RegisterTest(TTestTIOIDManagerFBL);
    RegisterTest(TTestTIClassToDBMapOperationFBL);
    {$ELSE}
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIPersistenceLayersFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIDatabaseFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIQueryFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIOIDManagerFBL.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistFBL), TTestTIClassToDBMapOperationFBL.Suite);
    {$ENDIF}
  end;
end;

{ TtiOPFTestSetupDataFBL }

constructor TtiOPFTestSetupDataFBL.Create;
begin
  inherited;
  {$IFNDEF STATIC_PERLAYER_LINKING}
    FEnabled := True;
  {$ELSE}
    {$IFDEF LINK_FBL}
      FEnabled := True;
    {$ELSE}
      FEnabled := False;
    {$ENDIF}
  {$ENDIF}
  FSelected    := FEnabled;
  FPerLayerName := cTIPersistFBL;
  // You can specify a hostname or a local database.
  FDBName      := 'localhost|' + ReadFromReg(cTIPersistFBL, 'DBName', gTestDataRoot + '.fbd');
//  FDBName      := ExpandFileName(ReadFromReg(cTIPersistFBL, 'DBName', gTestDataRoot + '.fbd'));
//  writeln(FDBName);
  FUsername    := ReadFromReg(cTIPersistFBL, 'Username', 'SYSDBA');
  FPassword    := ReadFromReg(cTIPersistFBL, 'Password', 'masterkey');
  FCanCreateDatabase := False;
  ForceTestDataDirectory;
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
  Exit;
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


{ TTestTIClassToDBMapOperationFBL }

procedure TTestTIClassToDBMapOperationFBL.SetUp;
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
