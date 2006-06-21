unit tiRegPerLayer_TST;

interface
uses
  Classes
  ,tstPerFramework_BOM
  ,tiPersistAbs_TST
  ,tiRegPerLayer
  ,tiDBConnectionSetupAbs_TST
  ;

type

  TTestRegPerLayer = class( TtiPerTestCase )
  private
    procedure LoadAllPersistenceLayers;
    procedure UnloadAllPersistenceLayers;
    procedure CheckLoadedPerLayerCount;
  protected
    procedure   Setup ; override ;
    procedure   TearDown ; override ;
  public
    constructor Create(MethodName: string); override ;
  published
    procedure   Load_Unload_PersistenceLayer;
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
    property  ModuleID : HModule read FModuleID write FModuleID ;
    property  DefaultDBConnectionName : string read GetDefaultDBConnectionName write SetDefaultDBConnectionName ;
    property  DefaultDBConnectionPool : TDBConnectionPool read GetDefaultDBConnectionPool ;
    property  DBConnectionPools : TDBConnectionPools read FDBConnectionPools ;
    function  DatabaseExists( const pDatabaseName, pUserName, pPassword : string ) : boolean ;
    procedure CreateDatabase( const pDatabaseName, pUserName, pPassword : string );
}

  end ;

procedure RegisterTests ;

implementation
uses
  TestFramework
  ,tiDUnitDependencies
  ,tiPersist
  ,tiQuery
  ,tiDBConnectionPool
  ,cTIPersist
  ;

procedure RegisterTests ;
begin
  RegisterTest(TTestRegPerLayer.Suite);
end ;

{ TTestRegPerLayer }

constructor TTestRegPerLayer.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [] ;
end;

procedure TTestRegPerLayer.CreateTIDatabase;
var
  i             : integer ;
  lPerLayerName : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lDatabase     : TtiDatabase ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    lDatabase := gTIPerMgr.RegPerLayers.CreateTIDatabase(lPerLayerName);
    try
      CheckNotNull(lDatabase, 'Failed creating TtiDatabase for <' + lPerLayerName + '>' );
      CheckIs(lDatabase, lRegPerLayer.tiDatabaseClass, 'Database wrong class');
    finally
      lDatabase.Free;
    end;
  end ;
end;

procedure TTestRegPerLayer.CreateTIDBConnectionPoolData;
var
  i             : integer ;
  lPerLayerName : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lDBConnectionPoolData : TtiDBConnectionPoolDataAbs ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    lDBConnectionPoolData := gTIPerMgr.RegPerLayers.CreateTIDBConnectionPoolData(lPerLayerName);
    try
      CheckNotNull(lDBConnectionPoolData, 'Failed creating TtiDBConnectionPoolData for <' + lPerLayerName + '>' );
      CheckIs(lDBConnectionPoolData, lRegPerLayer.tiDBConnectionPoolDataClass, 'DBConnectionPoolData wrong class');
    finally
      lDBConnectionPoolData.Free;
    end;
  end ;
end;

procedure TTestRegPerLayer.CreateTIQuery_DatabaseClass;
var
  i : integer ;
  lPerLayerName    : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lQuery : TtiQuery ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    lQuery := gTIPerMgr.RegPerLayers.CreateTIQuery(lRegPerLayer.tiDatabaseClass);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + lPerLayerName + '>' );
      CheckIs(lQuery, lRegPerLayer.tiQueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end ;
end;

procedure TTestRegPerLayer.CreateTIQuery_LayerName;
var
  i : integer ;
  lPerLayerName    : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lQuery : TtiQuery ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    lQuery := gTIPerMgr.RegPerLayers.CreateTIQuery(lPerLayerName);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + lPerLayerName + '>' );
      CheckIs(lQuery, lRegPerLayer.tiQueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end ;
end;

procedure TTestRegPerLayer.FindByLayerName;
var
  i : integer ;
  lPerLayerName    : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    CheckNotNull( lRegPerLayer, 'Can not find PerLayerName <' + lPerLayerName +'>');
    CheckEquals( lRegPerLayer.PerLayerName, lPerLayerName, 'lRegPerLayer.LayerName <> ' + lPerLayerName) ;
  end ;

end;

procedure TTestRegPerLayer.FindByTIDatabaseClass;
var
  i : integer ;
  lPerLayerName    : string ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lRegPerLayer1 : TtiRegPerLayer  ;
  lClass : TtiDatabaseClass ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerLayerName);
    CheckNotNull( lRegPerLayer, 'Can not find PerLayerName <' + lPerLayerName +'>');
    CheckEquals( lRegPerLayer.PerLayerName, lPerLayerName, 'lRegPerLayer.LayerName <> ' + lPerLayerName) ;
    lClass := lRegPerLayer.tiDatabaseClass ;
    lRegPerLayer1 := gTIPerMgr.RegPerLayers.FindByTIDatabaseClass(lClass);
    CheckNotNull( lRegPerLayer1, 'Can not find PerLayerName by class <' + lClass.ClassName + '>' );
    CheckSame( lRegPerLayer, lRegPerLayer1 ) ;
  end ;

end;

procedure TTestRegPerLayer.IsLoaded;
var
  i : integer ;
  lPerLayerName    : string ;
begin
  CheckLoadedPerLayerCount ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    Check( gTIPerMgr.RegPerLayers.IsLoaded(lPerLayerName), 'Failed for <' + lPerLayerName + '>') ;
  end ;
end;

procedure TTestRegPerLayer.Load_Unload_PersistenceLayer;
var
  i : integer ;
  lRegPerLayer  : TtiRegPerLayer  ;
  lPerFrameworkSetup : TPerFrameworkSetup ;
begin
  if gTIPerMgr.RegPerLayers.Count <> 0 then
    UnloadAllPersistenceLayers;
  LoadAllPersistenceLayers;
  CheckLoadedPerLayerCount ;

  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerFrameworkSetup := gPerFrameworkSetupFactory.Items[i] ;
    Check( gTIPerMgr.RegPerLayers.IsLoaded(lPerFrameworkSetup.PerLayerName), 'Persistence layer <' +
           lPerFrameworkSetup.PerLayerName + '> not loaded' );
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(lPerFrameworkSetup.PerLayerName) ;
    CheckNotNull( lRegPerLayer, 'Can not find RegPerLayer <' + lPerFrameworkSetup.PerLayerName + '>' ) ;
    CheckEquals( lRegPerLayer.PerLayerName, lPerFrameworkSetup.PerLayerName, 'lRegPerLayer.LayerName <> lPerFrameworkSetup.PerLayer') ;
  end ;

end;

procedure TTestRegPerLayer.CheckLoadedPerLayerCount ;
var
  lSetupCount : integer ;
  lLayerCount : integer ;
begin

  lSetupCount := gPerFrameworkSetupFactory.Count;
  lLayerCount := gTIPerMgr.RegPerLayers.Count;

  if gPerFrameworkSetupFactory.ToRun(cTIPersistRemote) and
     (not gPerFrameworkSetupFactory.ToRun(cTIPersistXML)) then
    Dec(lLayerCount);
  CheckEquals(lSetupCount, lLayerCount,
            'gPerFrameworkSetupFactory.Count <> gTIPerMgr.RegPerLayers.Count' ) ;
end ;

procedure TTestRegPerLayer.Setup;
begin
  LoadAllPersistenceLayers ;
end ;


procedure TTestRegPerLayer.LoadAllPersistenceLayers;
var
  i : integer ;
  lPerLayerName : string ;
begin
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
      lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
      if not gTIPerMgr.RegPerLayers.IsLoaded(lPerLayerName) then
        gTIPerMgr.RegPerLayers.LoadPersistenceLayer(lPerLayerName);
  end;
end;

procedure TTestRegPerLayer.TearDown;
begin
  UnLoadAllPersistenceLayers;
  inherited ;
end;

procedure TTestRegPerLayer.UnloadAllPersistenceLayers;
var
  i : integer ;
  lPerLayerName : string ;
begin
  for i := gPerFrameworkSetupFactory.Count - 1 downto 0 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    if gTIPerMgr.RegPerLayers.IsLoaded(lPerLayerName) then
      gTIPerMgr.RegPerLayers.UnLoadPersistenceLayer(gPerFrameworkSetupFactory.Items[i].PerLayerName);
  end ;
end;

end.
