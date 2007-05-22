unit tiOPFZeos_FB15_TST;

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
  TtiOPFTestSetupDataZeos = class(TtiOPFTestSetupData)
  public
    constructor Create; override;
  end;


  TTestTIPersistenceLayersZeos = class(TTestTIPersistenceLayers)
  protected
    procedure SetUp; override;
  end;


  TTestTIDatabaseZeos = class(TTestTIDatabase)
  protected
    procedure SetUp; override;
    procedure CreateDatabase; override;
  published
    procedure DatabaseExists; override;
  end;


  TTestTIQueryZeos = class(TTestTIQuerySQL)
  protected
    procedure SetUp; override;
  end;


  TTestTIClassToDBMapOperationZeos = class(TTestTIClassToDBMapOperation)
  protected
    procedure   SetUp; override;
  end;


  TTestTIOIDManagerZeos = class(TTestTIOIDManager)
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
  if gTIOPFTestManager.ToRun(cTIPersistZeosFB15) then
  begin
    RegisterTest(PersistentSuiteName(cTIPersistZeosFB15), TTestTIPersistenceLayersZeos.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistZeosFB15), TTestTIDatabaseZeos.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistZeosFB15), TTestTIQueryZeos.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistZeosFB15), TTestTIOIDManagerZeos.Suite);
    RegisterTest(PersistentSuiteName(cTIPersistZeosFB15), TTestTIClassToDBMapOperationZeos.Suite);
  end;
end;

{ TtiOPFTestSetupDataZeos }

constructor TtiOPFTestSetupDataZeos.Create;
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
  FPerLayerName := cTIPersistZeosFB15;
  // You can specify a hostname or a local database.
//  FDBName      := 'localhost|' + ReadFromReg(cTIPersistZeosFB15, 'DBName', gTestDataRoot + '.fbd');
  FDBName      := ReadFromReg(cTIPersistZeosFB15, 'DBName', gTestDataRoot + '.fbd');
//  writeln(FDBName);
  FUsername    := ReadFromReg(cTIPersistZeosFB15, 'Username', 'SYSDBA');
  FPassword    := ReadFromReg(cTIPersistZeosFB15, 'Password', 'masterkey');
  FCanCreateDatabase := False;
  ForceTestDataDirectory;
end;

{ TTestTIDatabaseZeos }

procedure TTestTIDatabaseZeos.CreateDatabase;
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

procedure TTestTIDatabaseZeos.DatabaseExists;
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

procedure TTestTIDatabaseZeos.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistZeosFB15);
  inherited;
end;

{ TTestTIPersistenceLayersZeos }

procedure TTestTIPersistenceLayersZeos.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistZeosFB15);
  inherited;
end;

{ TTestTIQueryZeos }

procedure TTestTIQueryZeos.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistZeosFB15);
  inherited;
end;


{ TTestTIClassToDBMapOperationZeos }

procedure TTestTIClassToDBMapOperationZeos.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistZeosFB15);
  inherited;
end;

{ TTestTIOIDManagerZeos }

procedure TTestTIOIDManagerZeos.SetUp;
begin
  PerFrameworkSetup:= gTIOPFTestManager.FindByPerLayerName(cTIPersistZeosFB15);
  inherited;
end;

end.
