unit tiPersistenceLayers_TST;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiTestFramework              
  ,tiBOMsForTesting
  ,tiPersistenceLayers
  ,tiOPFTestManager
 ;


type

  TTestPersistenceLayers = class(TtiOPFTestCase)
  private
    procedure LoadAllPersistenceLayers;
    procedure CheckLoadedPerLayerCount;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  public
    constructor Create {$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF}; override;
  published
    procedure   ConfirmStaticLinking;
    procedure   DefaultPerLayerName;
    procedure   FindByLayerName;
    procedure   FindByTIDatabaseClass;
    procedure   IsLoaded;
    procedure   CreateTIQuery_LayerName;
    procedure   CreateTIQuery_DatabaseClass;
    procedure   CreateTIDatabase;
    procedure   CreateTIDBConnectionPoolData;

  end;

procedure RegisterTests;


implementation
uses
  {$IFDEF FPC}
  TestRegistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiOPFManager
  ,tiTestDependencies
  ,tiQuery
  ,tiDBConnectionPool
  ,tiConstants
 ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestPersistenceLayers);
end;


{ TTestPersistenceLayers }

constructor TTestPersistenceLayers.Create{$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  SetupTasks := [];
end;


procedure TTestPersistenceLayers.CreateTIDatabase;
var
  i            : integer;
  LPersistenceLayerName : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LDatabase    : TtiDatabase;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPersistenceLayerName);
    LDatabase := gTIOPFManager.PersistenceLayers.CreateTIDatabase(LPersistenceLayerName);
    try
      CheckNotNull(LDatabase, 'Failed creating TtiDatabase for <' + LPersistenceLayerName + '>');
      CheckIs(LDatabase, LPersistenceLayer.DatabaseClass, 'Database wrong class');
    finally
      LDatabase.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIDBConnectionPoolData;
var
  i            : integer;
  LPersistenceLayerName : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LDBConnectionPoolData : TtiDBConnectionPoolDataAbs;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPersistenceLayerName);
    LDBConnectionPoolData := gTIOPFManager.PersistenceLayers.CreateTIDBConnectionPoolData(LPersistenceLayerName);
    try
      CheckNotNull(LDBConnectionPoolData, 'Failed creating TtiDBConnectionPoolData for <' + LPersistenceLayerName + '>');
      CheckIs(LDBConnectionPoolData, LPersistenceLayer.DBConnectionPoolDataClass, 'DBConnectionPoolData wrong class');
    finally
      LDBConnectionPoolData.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIQuery_DatabaseClass;
var
  i : integer;
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LQuery : TtiQuery;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPersistenceLayerName);
    LQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(LPersistenceLayer.DatabaseClass);
    try
      CheckNotNull(LQuery, 'Failed creating TtiQuery for <' + LPersistenceLayerName + '>');
      CheckIs(LQuery, LPersistenceLayer.QueryClass, 'Query wrong class');
    finally
      LQuery.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIQuery_LayerName;
var
  i : integer;
  LPerLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  lQuery : TtiQuery;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPerLayerName);
    lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(LPerLayerName);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + LPerLayerName + '>');
      CheckIs(lQuery, LPersistenceLayer.QueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.FindByLayerName;
var
  i : integer;
  LPerLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPerLayerName);
    CheckNotNull(LPersistenceLayer, 'Can not find PerLayerName <' + LPerLayerName +'>');
    CheckEquals(LPersistenceLayer.PersistenceLayerName, LPerLayerName, 'LPersistenceLayer.LayerName <> ' + LPerLayerName);
  end;
end;


procedure TTestPersistenceLayers.FindByTIDatabaseClass;
var
  i : integer;
  LPerLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LPersistenceLayer1 : TtiPersistenceLayer ;
  LClass : TtiDatabaseClass;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPerLayerName);
    CheckNotNull(LPersistenceLayer, 'Can not find PerLayerName <' + LPerLayerName +'>');
    CheckEquals(LPersistenceLayer.PersistenceLayerName, LPerLayerName, 'LPersistenceLayer.LayerName <> ' + LPerLayerName);
    LClass := LPersistenceLayer.DatabaseClass;
    LPersistenceLayer1 := gTIOPFManager.PersistenceLayers.FindByTIDatabaseClass(LClass);
    CheckNotNull(LPersistenceLayer1, 'Can not find PerLayerName by class <' + LClass.ClassName + '>');
    CheckSame(LPersistenceLayer, LPersistenceLayer1);
  end;
end;


procedure TTestPersistenceLayers.IsLoaded;
var
  i : integer;
  LPerLayerName   : string;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    Check(gTIOPFManager.PersistenceLayers.IsLoaded(LPerLayerName), 'Failed for <' + LPerLayerName + '>');
  end;
end;

  procedure TTestPersistenceLayers.ConfirmStaticLinking;
  begin
    CheckLoadedPerLayerCount;
  end;

procedure TTestPersistenceLayers.CheckLoadedPerLayerCount;
var
  lSetupCount : integer;
  lLayerCount : integer;
begin
  lSetupCount := gTIOPFTestManager.Count;
  lLayerCount := gTIOPFManager.PersistenceLayers.Count;

  if gTIOPFTestManager.ToRun(cTIPersistRemote) and
     (not gTIOPFTestManager.ToRun(cTIPersistXML)) then
    Dec(lLayerCount);
  CheckEquals(lSetupCount, lLayerCount,
            'gPerFrameworkSetupFactory.Count <> gTIOPFManager.PersistenceLayers.Count');
end;


procedure TTestPersistenceLayers.SetUp;
begin
  LoadAllPersistenceLayers;
end;


procedure TTestPersistenceLayers.LoadAllPersistenceLayers;
begin
  ConfirmStaticLinking;
end;


procedure TTestPersistenceLayers.TearDown;
begin
  ConfirmStaticLinking;
  inherited;
end;

procedure TTestPersistenceLayers.DefaultPerLayerName;
var
  LDefaultPerLayerName: string;
  i: integer;
  LPerLayerName: string;
  LPersistenceLayer: TtiPersistenceLayer;
begin
  LDefaultPerLayerName:= gTIOPFManager.DefaultPerLayerName;
  try
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(LPerLayerName);
    gTIOPFManager.DefaultPerLayerName:= LPerLayerName;
    CheckNotNull(gTIOPFManager.DefaultPerLayer);
    CheckSame(LPersistenceLayer, gTIOPFManager.DefaultPerLayer);
    CheckEquals(LPerLayerName, gTIOPFManager.DefaultPerLayerName);
    CheckEquals(LPerLayerName, gTIOPFManager.DefaultPerLayer.PersistenceLayerName);
  end;
  finally
    gTIOPFManager.DefaultPerLayerName:= LDefaultPerLayerName;
  end;
  CheckEquals(LDefaultPerLayerName, gTIOPFManager.DefaultPerLayerName);
end;


end.


