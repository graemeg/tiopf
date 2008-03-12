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

  TTestPersistenceLayers = class(TtiTestCaseWithPersistenceLayer)
  private
    procedure LoadAllPersistenceLayers;
    procedure CheckLoadedPerLayerCount;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure   ConfirmStaticLinking;
    procedure   DefaultPersistenceLayerName;
    procedure   FindByLayerName;
    procedure   FindByTIDatabaseClass;
    procedure   IsLoaded;
    procedure   CreateTIQuery_LayerName;
    procedure   CreateTIQuery_DatabaseClass;
    procedure   CreateTIDatabase;

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
  tiRegisterNonPersistentTest(TTestPersistenceLayers);
end;


{ TTestPersistenceLayers }

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
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    LDatabase := gTIOPFManager.PersistenceLayers.CreateTIDatabase(LPersistenceLayerName);
    try
      CheckNotNull(LDatabase, 'Failed creating TtiDatabase for <' + LPersistenceLayerName + '>');
      CheckIs(LDatabase, LPersistenceLayer.DatabaseClass, 'Database wrong class');
    finally
      LDatabase.Free;
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
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
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
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  lQuery : TtiQuery;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(LPersistenceLayerName);
    try
      CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + LPersistenceLayerName + '>');
      CheckIs(lQuery, LPersistenceLayer.QueryClass, 'Query wrong class');
    finally
      lQuery.Free;
    end;
  end;
end;


procedure TTestPersistenceLayers.FindByLayerName;
var
  i : integer;
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    CheckNotNull(LPersistenceLayer, 'Can not find PersistenceLayerName <' + LPersistenceLayerName +'>');
    CheckEquals(LPersistenceLayer.PersistenceLayerName, LPersistenceLayerName, 'LPersistenceLayer.LayerName <> ' + LPersistenceLayerName);
  end;
end;


procedure TTestPersistenceLayers.FindByTIDatabaseClass;
var
  i : integer;
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LPersistenceLayer1 : TtiPersistenceLayer ;
  LClass : TtiDatabaseClass;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    CheckNotNull(LPersistenceLayer, 'Can not find PersistenceLayerName <' + LPersistenceLayerName +'>');
    CheckEquals(LPersistenceLayer.PersistenceLayerName, LPersistenceLayerName, 'LPersistenceLayer.LayerName <> ' + LPersistenceLayerName);
    LClass := LPersistenceLayer.DatabaseClass;
    LPersistenceLayer1 := gTIOPFManager.PersistenceLayers.FindByTIDatabaseClass(LClass);
    CheckNotNull(LPersistenceLayer1, 'Can not find PersistenceLayerName by class <' + LClass.ClassName + '>');
    CheckSame(LPersistenceLayer, LPersistenceLayer1);
  end;
end;


procedure TTestPersistenceLayers.IsLoaded;
var
  i : integer;
  LPersistenceLayerName   : string;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    Check(gTIOPFManager.PersistenceLayers.IsLoaded(LPersistenceLayerName), 'Failed for <' + LPersistenceLayerName + '>');
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

procedure TTestPersistenceLayers.DefaultPersistenceLayerName;
var
  LDefaultPersistenceLayerName: string;
  i: integer;
  LPersistenceLayerName: string;
  LPersistenceLayer: TtiPersistenceLayer;
begin
  LDefaultPersistenceLayerName:= gTIOPFManager.DefaultPersistenceLayerName;
  try
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := gTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    gTIOPFManager.DefaultPersistenceLayerName:= LPersistenceLayerName;
    CheckNotNull(gTIOPFManager.DefaultPerLayer);
    CheckSame(LPersistenceLayer, gTIOPFManager.DefaultPerLayer);
    CheckEquals(LPersistenceLayerName, gTIOPFManager.DefaultPersistenceLayerName);
    CheckEquals(LPersistenceLayerName, gTIOPFManager.DefaultPerLayer.PersistenceLayerName);
  end;
  finally
    gTIOPFManager.DefaultPersistenceLayerName:= LDefaultPersistenceLayerName;
  end;
  CheckEquals(LDefaultPersistenceLayerName, gTIOPFManager.DefaultPersistenceLayerName);
end;


end.



