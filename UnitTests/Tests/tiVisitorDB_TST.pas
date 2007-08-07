unit tiVisitorDB_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework
  {$IFNDEF FPC}
  ,TestFrameWork
  {$ENDIF}
  ,tiObject
 ;

type

  TTestTIVisitorDB = class(TtiTestCase)
  private
  protected
  published
    procedure TIObjectVisitorControllerConfig_SetDatabaseAndPersistenceLayerNames;

    procedure TIObjectVisitorController_BeforeExecuteVisitorGroup;
    procedure TIObjectVisitorController_BeforeExecuteVisitor;
    procedure TIObjectVisitorController_AfterExecuteVisitor;
    procedure TIObjectVisitorController_AfterExecuteVisitorGroup;
    procedure TIObjectVisitorController_AfterExecuteVisitorGroupError;
    procedure TIVisitorManager_ExecutePassDatabaseName;
    procedure TIVisitorManager_Execute;
  end;

procedure RegisterTests;

implementation
uses
   tiDUnitDependencies
  ,tiVisitor
  ,tiVisitorDB
  ,tiOPFManager
  ,tiExcept
  ,tiQueryXMLLight
  ,tiConstants
  ,tiUtils
  ,tiBaseObject
  ,SysUtils
 ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIVisitorDB);
end;


{ TTestTIVisitorDB }

type

  TTestObjectVisitorControllerConfig = class(TtiObjectVisitorControllerConfig)
  private
    FTIOPFManager: TtiBaseObject;
  protected
    function TIOPFManager: TtiBaseObject; override;
    procedure SetTIOPFManager(const ATIOPFManager: TtiBaseObject);
  end;

  procedure TTestObjectVisitorControllerConfig.SetTIOPFManager(
    const ATIOPFManager: TtiBaseObject);
  begin
    FTIOPFManager:= ATIOPFManager;
  end;

  function TTestObjectVisitorControllerConfig.TIOPFManager: TtiBaseObject;
  begin
    result:= FTIOPFManager;
  end;


procedure TTestTIVisitorDB.TIObjectVisitorControllerConfig_SetDatabaseAndPersistenceLayerNames;
var
  LVCC: TTestObjectVisitorControllerConfig;
  LM: TtiOPFManager;
  LDatabaseName: string;
begin
  LDatabaseName:= TempFileName('TestTIVisitorDB.xml');
  LM:= nil;
  LVCC:= nil;
  try
    LM:= TtiOPFManager.Create;
    LVCC:= TTestObjectVisitorControllerConfig.Create;
    LVCC.SetTIOPFManager(LM);

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('', '');
      Fail('Exception not raised');
    except
      on e: exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultPersistenceLayerNotAssigned, E.Message);
      end;
    end;

    tiQueryXMLLight.RegisterPersistenceLayer(LM.PersistenceLayers);
    CheckEquals(cTIPersistXMLLight, LM.DefaultPerLayerName);

    try
      LVCC.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, '');
      Fail('Exception not raised');
    except
      on e: exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultDatabaseNotAssigned, E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('', '');
      Fail('Exception not raised');
    except
      on e: exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultDatabaseNotAssigned, E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, 'test_db');
      Fail('Exception not raised');
    except
      on e: exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckFormattedMessage(CErrorAttemptToUseUnConnectedDatabase, ['test_db'], E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('test_pl', LDatabaseName);
      Fail('Exception not raised');
    except
      on e: exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckFormattedMessage(CErrorAttemptToUseUnRegisteredPersistenceLayer, ['test_pl'], E.Message);
      end;
    end;

    if not LM.DefaultPerLayer.DatabaseExists(LDatabaseName, '', '') then
      LM.DefaultPerLayer.CreateDatabase(LDatabaseName, '', '');
    LM.DefaultPerLayer.DBConnectionPools.Connect(LDatabaseName, '', '', '');

    LVCC.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, LDatabaseName);
    CheckEquals(cTIPersistXMLLight, LVCC.PersistenceLayerName);
    CheckEquals(LDatabaseName, LVCC.DatabaseName);

    LVCC.SetDatabaseAndPersistenceLayerNames('', '');
    CheckEquals(cTIPersistXMLLight, LVCC.PersistenceLayerName);
    CheckEquals(LDatabaseName, LVCC.DatabaseName);


  finally
    LVCC.Free;
    LM.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

type

  TTestTIObjectVisitorController = class(TtiObjectVisitorController)
  private
    FTIOPFManager: TtiBaseObject;
  protected
    function TIOPFManager: TtiBaseObject; override;
  end;

  TTestTIObjectVisitorControllerConfig = class(TtiObjectVisitorControllerConfig)
  private
    FTIOPFManager: TtiBaseObject;
  protected
    function TIOPFManager: TtiBaseObject; override;
  end;

  TTestTIObjectVisited = class(TtiObject)
  private
    FTouched: Boolean;
  public
    property Touched: Boolean read FTouched write FTouched;
  end;

  TTestTIObjectVisitor = class(TtiObjectVisitor)
  public
    property    Database;
    property    Query;
    procedure   Final(const AVisited: TtiObject); override;
  end;

  function TTestTIObjectVisitorController.TIOPFManager: TtiBaseObject;
  begin
    result:= FTIOPFManager;
  end;

  function TTestTIObjectVisitorControllerConfig.TIOPFManager: TtiBaseObject;
  begin
    result:= FTIOPFManager;
  end;

  procedure TTestTIObjectVisitor.Final(const AVisited: TtiObject);
  begin
    (AVisited as TTestTIObjectVisited).Touched:= True;
  end;


procedure CreateTIObjectVisitorControllerTestInstance(
  var AOPDMSManager: TtiOPFManager;
  var AVisitorController: TTestTIObjectVisitorController;
  var AConfig: TTestTIObjectVisitorControllerConfig;
  const ADatabaseName: string);
begin
  AOPDMSManager:= TtiOPFManager.Create;
  tiQueryXMLLight.RegisterPersistenceLayer(AOPDMSManager.PersistenceLayers);
  AOPDMSManager.DefaultPerLayer.CreateDatabase(ADatabaseName, '', '');
  AOPDMSManager.DefaultPerLayer.DBConnectionPools.Connect(ADatabaseName, '', '', '');
  AConfig:= TTestTIObjectVisitorControllerConfig.Create;
  AConfig.FTIOPFManager:= AOPDMSManager;
  AConfig.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, ADatabaseName);
  AVisitorController:= TTestTIObjectVisitorController.Create(AConfig);
  AVisitorController.FTIOPFManager:= AOPDMSManager;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_BeforeExecuteVisitorGroup;
var
  LM: TtiOPFManager;
  LVC: TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
begin
  LDatabaseName:= TempFileName('TestTIVisitorDB.xml');
  LM:= nil;
  LConfig:= nil;
  LVC:= nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVC.BeforeExecuteVisitorGroup;
    CheckEquals(cTIPersistXMLLight, LVC.PersistenceLayerName);
    CheckEquals(LDatabaseName, LVC.DatabaseName);
    CheckNotNull(LVC.Database);
    Check(LVC.Database.InTransaction);
    CheckEquals(1, LM.DefaultDBConnectionPool.CountLocked);
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_BeforeExecuteVisitor;
var
  LM: TtiOPFManager;
  LVC: TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
begin
  LDatabaseName:= TempFileName('TestTIVisitorDB.xml');
  LM:= nil;
  LConfig:= nil;
  LVC:= nil;
  LVisitor:= nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor:= TTestTIObjectVisitor.Create;

    LVC.BeforeExecuteVisitorGroup;
    LVC.BeforeExecuteVisitor(LVisitor);

    CheckNotNull(LVisitor.Query);
    CheckNotNull(LVisitor.Database);
    CheckSame(LVC.Database, LVisitor.Database);

  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisitor.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitor;
var
  LM: TtiOPFManager;
  LVC: TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
begin
  LDatabaseName:= TempFileName('TestTIVisitorDB.xml');
  LM:= nil;
  LConfig:= nil;
  LVC:= nil;
  LVisitor:= nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor:= TTestTIObjectVisitor.Create;

    LVC.BeforeExecuteVisitorGroup;
    LVC.BeforeExecuteVisitor(LVisitor);
    CheckNotNull(LVisitor.Database);
    CheckSame(LVC.Database, LVisitor.Database);
    LVC.AfterExecuteVisitor(LVisitor);
    CheckNull(LVisitor.Database);

  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisitor.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitorGroup;
var
  LM: TtiOPFManager;
  LVC: TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
  LVisited: TTestTIObjectVisited;
  LTouchedByVisitorList: TtiTouchedByVisitorList;
  LTouchedByVisitor: TtiTouchedByVisitor;
begin
  LDatabaseName:= TempFileName('TestTIVisitorDB.xml');
  LM:= nil;
  LConfig:= nil;
  LVC:= nil;
  LVisitor:= nil;
  LVisited:= nil;
  LTouchedByVisitorList:= nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor:= TTestTIObjectVisitor.Create;
    LVisited:= TTestTIObjectVisited.Create;
    LTouchedByVisitorList:= TtiTouchedByVisitorList.Create(True);
    LTouchedByVisitor:= TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LTouchedByVisitorList.Add(LTouchedByVisitor);

    LVC.BeforeExecuteVisitorGroup;
    LVC.BeforeExecuteVisitor(LVisitor);
    LVC.AfterExecuteVisitor(LVisitor);
    CheckEquals(False, LVisited.Touched);
    CheckEquals(1, LM.DefaultDBConnectionPool.CountLocked);
    LVC.AfterExecuteVisitorGroup(LTouchedByVisitorList);
    CheckEquals(True, LVisited.Touched);
    CheckEquals(0, LM.DefaultDBConnectionPool.CountLocked);

  finally
    LTouchedByVisitorList.Free;
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisitor.Free;
    LVisited.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitorGroupError;
begin

end;

procedure TTestTIVisitorDB.TIVisitorManager_Execute;
begin

end;

procedure TTestTIVisitorDB.TIVisitorManager_ExecutePassDatabaseName;
begin

end;

{ TTestTIObjectVisitor }

end.

