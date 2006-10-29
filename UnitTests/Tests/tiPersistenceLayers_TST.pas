unit tiPersistenceLayers_TST;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiTestFramework              
  ,tstPerFramework_BOM
  ,tiPersistenceLayers
  ,tiOPFTestManager
 ;


type

  TTestPersistenceLayers = class(TtiOPFTestCase)
  private
    procedure LoadAllPersistenceLayers;
    {$IFNDEF STATIC_PERLAYER_LINKING}
    procedure UnloadAllPersistenceLayers;
    {$ENDIF}
    procedure CheckLoadedPerLayerCount;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: string); override;
    {$ENDIF}
  published
    {$IFDEF STATIC_PERLAYER_LINKING}
    procedure   ConfirmStaticLinking;
    {$ELSE}
    procedure   Load_Unload_PersistenceLayer;
    {$ENDIF}
//    procedure   DefaultPerLayer;
    procedure   DefaultPerLayerName;
    procedure   FindByLayerName;
    procedure   FindByTIDatabaseClass;
    procedure   IsLoaded;
    procedure   CreateTIQuery_LayerName;
    procedure   CreateTIQuery_DatabaseClass;
    procedure   CreateTIDatabase;
    procedure   CreateTIDBConnectionPoolData;

//    Will have to check each of these.
//    property  tiDBConnectionPoolDataClass
//    property  tiQueryClass
//    property  tiDatabaseClass
//    property  LayerName


{
    property  ModuleID : HModule read FModuleID write FModuleID;
    property  DefaultDBConnectionName : string read GetDefaultDBConnectionName write SetDefaultDBConnectionName;
    property  DefaultDBConnectionPool : TDBConnectionPool read GetDefaultDBConnectionPool;
    property  DBConnectionPools : TDBConnectionPools read FDBConnectionPools;
    function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean;
    procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string);
}

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
  ,tiDUnitDependencies
  ,tiQuery
  ,tiDBConnectionPool
  ,tiConstants
 ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestPersistenceLayers);
end;


{ TTestPersistenceLayers }

constructor TTestPersistenceLayers.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  SetupTasks := [];
end;


procedure TTestPersistenceLayers.CreateTIDatabase;
var
  i            : integer;
  lPerLayerName : string;
  lRegPerLayer : TtiPersistenceLayer ;
  lDatabase    : TtiDatabase;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    lDatabase := gTIOPFManager.PersistenceLayers.CreateTIDatabase(lPerLayerName);
    try
      CheckNotNull(lDatabase, 'Failed creating TtiDatabase for <' + lPerLayerName + '>');
      CheckIs(lDatabase, lRegPerLayer.tiDatabaseClass, 'Database wrong class');
    finally
      lDatabase.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIDBConnectionPoolData;
var
  i            : integer;
  lPerLayerName : string;
  lRegPerLayer : TtiPersistenceLayer ;
  lDBConnectionPoolData : TtiDBConnectionPoolDataAbs;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    lDBConnectionPoolData := gTIOPFManager.PersistenceLayers.CreateTIDBConnectionPoolData(lPerLayerName);
    try
      CheckNotNull(lDBConnectionPoolData, 'Failed creating TtiDBConnectionPoolData for <' + lPerLayerName + '>');
      CheckIs(lDBConnectionPoolData, lRegPerLayer.tiDBConnectionPoolDataClass, 'DBConnectionPoolData wrong class');
    finally
      lDBConnectionPoolData.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIQuery_DatabaseClass;
var
  i : integer;
  lPerLayerName   : string;
  lRegPerLayer : TtiPersistenceLayer ;
  lQuery : TtiQuery;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(lRegPerLayer.tiDatabaseClass);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + lPerLayerName + '>');
      CheckIs(lQuery, lRegPerLayer.tiQueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.CreateTIQuery_LayerName;
var
  i : integer;
  lPerLayerName   : string;
  lRegPerLayer : TtiPersistenceLayer ;
  lQuery : TtiQuery;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(lPerLayerName);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + lPerLayerName + '>');
      CheckIs(lQuery, lRegPerLayer.tiQueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.FindByLayerName;
var
  i : integer;
  lPerLayerName   : string;
  lRegPerLayer : TtiPersistenceLayer ;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    CheckNotNull(lRegPerLayer, 'Can not find PerLayerName <' + lPerLayerName +'>');
    CheckEquals(lRegPerLayer.PerLayerName, lPerLayerName, 'lRegPerLayer.LayerName <> ' + lPerLayerName);
  end;
end;


procedure TTestPersistenceLayers.FindByTIDatabaseClass;
var
  i : integer;
  lPerLayerName   : string;
  lRegPerLayer : TtiPersistenceLayer ;
  lRegPerLayer1 : TtiPersistenceLayer ;
  lClass : TtiDatabaseClass;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    CheckNotNull(lRegPerLayer, 'Can not find PerLayerName <' + lPerLayerName +'>');
    CheckEquals(lRegPerLayer.PerLayerName, lPerLayerName, 'lRegPerLayer.LayerName <> ' + lPerLayerName);
    lClass := lRegPerLayer.tiDatabaseClass;
    lRegPerLayer1 := gTIOPFManager.PersistenceLayers.FindByTIDatabaseClass(lClass);
    CheckNotNull(lRegPerLayer1, 'Can not find PerLayerName by class <' + lClass.ClassName + '>');
    CheckSame(lRegPerLayer, lRegPerLayer1);
  end;
end;


procedure TTestPersistenceLayers.IsLoaded;
var
  i : integer;
  lPerLayerName   : string;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    Check(gTIOPFManager.PersistenceLayers.IsLoaded(lPerLayerName), 'Failed for <' + lPerLayerName + '>');
  end;
end;


{$IFDEF STATIC_PERLAYER_LINKING}
  procedure TTestPersistenceLayers.ConfirmStaticLinking;
  begin
    CheckLoadedPerLayerCount;
  end;
{$ELSE}
  procedure TTestPersistenceLayers.Load_Unload_PersistenceLayer;
  var
    i : integer;
    lRegPerLayer : TtiPersistenceLayer ;
    lPerFrameworkSetup : TtiOPFTestSetupData;
  begin
    if gTIOPFManager.PersistenceLayers.Count <> 0 then
      UnloadAllPersistenceLayers;
    LoadAllPersistenceLayers;
    CheckLoadedPerLayerCount;

    for i := 0 to gTIOPFTestManager.Count - 1 do
    begin
      lPerFrameworkSetup := gTIOPFTestManager.Items[i];
      Check(gTIOPFManager.PersistenceLayers.IsLoaded(lPerFrameworkSetup.PerLayerName), 'Persistence layer <' +
             lPerFrameworkSetup.PerLayerName + '> not loaded');
      lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerFrameworkSetup.PerLayerName);
      CheckNotNull(lRegPerLayer, 'Can not find RegPerLayer <' + lPerFrameworkSetup.PerLayerName + '>');
      CheckEquals(lRegPerLayer.PerLayerName, lPerFrameworkSetup.PerLayerName, 'lRegPerLayer.LayerName <> lPerFrameworkSetup.PerLayer');
    end;
  end;
{$ENDIF}


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
{$IFDEF STATIC_PERLAYER_LINKING}
begin
  ConfirmStaticLinking;
{$ELSE}
var
  i: integer;
  lPerLayerName: string;
begin
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
      lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
      if not gTIOPFManager.PersistenceLayers.IsLoaded(lPerLayerName) then
        gTIOPFManager.PersistenceLayers.LoadPersistenceLayer(lPerLayerName);
  end;
{$ENDIF}
end;


procedure TTestPersistenceLayers.TearDown;
begin
{$IFDEF STATIC_PERLAYER_LINKING}
  ConfirmStaticLinking;
{$ELSE}
  UnLoadAllPersistenceLayers;
{$ENDIF}
  inherited;
end;


{$IFNDEF STATIC_PERLAYER_LINKING}
procedure TTestPersistenceLayers.UnloadAllPersistenceLayers;
var
  i : integer;
  lPerLayerName : string;
begin
  for i := gTIOPFManager.PersistenceLayers.Count - 1 downto 0 do
  begin
    lPerLayerName := gTIOPFManager.PersistenceLayers.Items[i].PerLayerName;
    gTIOPFManager.PersistenceLayers.UnLoadPersistenceLayer(lPerLayerName);
  end;
end;
{$ENDIF}


procedure TTestPersistenceLayers.DefaultPerLayerName;
var
  LDefaultPerLayerName: string;
  i: integer;
  LPerLayerName: string;
  LRegPerLayer: TtiPersistenceLayer;
begin
  LDefaultPerLayerName:= gTIOPFManager.DefaultPerLayerName;
  try
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    LRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(lPerLayerName);
    gTIOPFManager.DefaultPerLayerName:= LPerLayerName;
    CheckNotNull(gTIOPFManager.DefaultPerLayer);
    CheckSame(LRegPerLayer, gTIOPFManager.DefaultPerLayer);
    CheckEquals(LPerLayerName, gTIOPFManager.DefaultPerLayerName);
    CheckEquals(LPerLayerName, gTIOPFManager.DefaultPerLayer.PerLayerName);
  end;
  finally
    gTIOPFManager.DefaultPerLayerName:= LDefaultPerLayerName;
  end;
  CheckEquals(LDefaultPerLayerName, gTIOPFManager.DefaultPerLayerName);
end;


end.

