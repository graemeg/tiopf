unit tiPersistenceLayers_TST;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiTestFramework
 ;


type

  TTestPersistenceLayers = class(TtiTestCase)
  private
    procedure CheckLoadedPerLayerCount;
  published
    procedure   ConfirmStaticLinking;
    procedure   DefaultPersistenceLayerName;
    procedure   FindByLayerName;
    procedure   FindByTIDatabaseClass;
    procedure   IsLoaded;

  end;

procedure RegisterTests;


implementation
uses
  {$IFDEF FPC}
  TestRegistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiPersistenceLayers
  ,tiOPFTestManager
  ,tiOPFManager
  ,tiTestDependencies
  ,tiQuery
  ,tiConstants
 ;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestPersistenceLayers);
end;


{ TTestPersistenceLayers }


procedure TTestPersistenceLayers.FindByLayerName;
var
  i : integer;
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
begin
  CheckLoadedPerLayerCount;
  for i := 0 to GTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := GTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
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
  for i := 0 to GTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := GTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    CheckNotNull(LPersistenceLayer, 'Can not find PersistenceLayerName <' + LPersistenceLayerName +'>');
    CheckEquals(LPersistenceLayer.PersistenceLayerName, LPersistenceLayerName, 'LPersistenceLayer.LayerName <> ' + LPersistenceLayerName);
    LClass := LPersistenceLayer.DatabaseClass;
    LPersistenceLayer1 := GTIOPFManager.PersistenceLayers.FindByTIDatabaseClass(LClass);
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
  for i := 0 to GTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := GTIOPFTestManager.Items[i].PersistenceLayerName;
    Check(GTIOPFManager.PersistenceLayers.IsLoaded(LPersistenceLayerName), 'Failed for <' + LPersistenceLayerName + '>');
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
  lSetupCount := GTIOPFTestManager.Count;
  lLayerCount := GTIOPFManager.PersistenceLayers.Count;

  if GTIOPFTestManager.ToRun(cTIPersistRemote) and
     (not GTIOPFTestManager.ToRun(cTIPersistXML)) then
    Dec(lLayerCount);
  CheckEquals(lSetupCount, lLayerCount,
            'gTestSetupDataFactory.Count <> GTIOPFManager.PersistenceLayers.Count');
end;

procedure TTestPersistenceLayers.DefaultPersistenceLayerName;
var
  LDefaultPersistenceLayerName: string;
  i: integer;
  LPersistenceLayerName: string;
  LPersistenceLayer: TtiPersistenceLayer;
begin
  LDefaultPersistenceLayerName:= GTIOPFManager.DefaultPersistenceLayerName;
  try
  for i := 0 to GTIOPFTestManager.Count - 1 do
  begin
    LPersistenceLayerName := GTIOPFTestManager.Items[i].PersistenceLayerName;
    LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
    GTIOPFManager.DefaultPersistenceLayerName:= LPersistenceLayerName;
    CheckNotNull(GTIOPFManager.DefaultPerLayer);
    CheckSame(LPersistenceLayer, GTIOPFManager.DefaultPerLayer);
    CheckEquals(LPersistenceLayerName, GTIOPFManager.DefaultPersistenceLayerName);
    CheckEquals(LPersistenceLayerName, GTIOPFManager.DefaultPerLayer.PersistenceLayerName);
  end;
  finally
    GTIOPFManager.DefaultPersistenceLayerName:= LDefaultPersistenceLayerName;
  end;
  CheckEquals(LDefaultPersistenceLayerName, GTIOPFManager.DefaultPersistenceLayerName);
end;


end.





