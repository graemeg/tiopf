unit tiVisitorDB_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFramework,
  {$IFNDEF FPC}
  TestFrameWork,
  {$ENDIF}
  tiObject;

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
    procedure TIVisitorManager_ExecutePassDatabaseNameException;
    procedure TIVisitorManager_ExecuteUnregisteredVisitor;
    procedure TIVisitorManager_Execute;
// ToDo: Require a for handling of exception in TtiObjectVisitorController.AfterExecuteVisitorGroup    
  end;

procedure RegisterTests;

implementation

uses
  tiDUnitDependencies,
  tiVisitor,
  tiVisitorDB,
  tiOPFManager,
  tiExcept,
  tiQueryXMLLight,
  tiConstants,
  tiUtils,
  tiBaseObject,
  SysUtils,
  Classes;

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
  FTIOPFManager := ATIOPFManager;
end;

function TTestObjectVisitorControllerConfig.TIOPFManager: TtiBaseObject;
begin
  Result := FTIOPFManager;
end;


procedure TTestTIVisitorDB.TIObjectVisitorControllerConfig_SetDatabaseAndPersistenceLayerNames;
var
  LVCC: TTestObjectVisitorControllerConfig;
  LM:   TtiOPFManager;
  LDatabaseName: string;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM   := nil;
  LVCC := nil;
  try
    LM := TtiOPFManager.Create;
    LVCC := TTestObjectVisitorControllerConfig.Create(LM.VisitorManager);
    LVCC.SetTIOPFManager(LM);

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('', '');
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultPersistenceLayerNotAssigned, E.Message);
      end;
    end;

    tiQueryXMLLight.RegisterPersistenceLayer(LM.PersistenceLayers);
    CheckEquals(cTIPersistXMLLight, LM.DefaultPerLayerName);

    try
      LVCC.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, '');
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultDatabaseNotAssigned, E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('', '');
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckEquals(CErrorDefaultDatabaseNotAssigned, E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, 'test_db');
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckFormattedMessage(CErrorAttemptToUseUnConnectedDatabase, ['test_db'], E.Message);
      end;
    end;

    try
      LVCC.SetDatabaseAndPersistenceLayerNames('test_pl', LDatabaseName);
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFDataException);
        CheckFormattedMessage(CErrorAttemptToUseUnRegisteredPersistenceLayer,
          ['test_pl'], E.Message);
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
    FTouched: boolean;
  public
    property Touched: boolean read FTouched write FTouched;
  end;

  TTestTIObjectVisitor = class(TtiObjectVisitor)
  public
    property Database;
    property Query;
    procedure Final(const AVisited: TtiObject); override;
  end;

function TTestTIObjectVisitorController.TIOPFManager: TtiBaseObject;
begin
  Result := FTIOPFManager;
end;

function TTestTIObjectVisitorControllerConfig.TIOPFManager: TtiBaseObject;
begin
  Result := FTIOPFManager;
end;

procedure TTestTIObjectVisitor.Final(const AVisited: TtiObject);
begin
  (AVisited as TTestTIObjectVisited).Touched := True;
end;


procedure CreateTIObjectVisitorControllerTestInstance(var AOPDMSManager: TtiOPFManager;
  var AVisitorController: TTestTIObjectVisitorController;
  var AConfig: TTestTIObjectVisitorControllerConfig; const ADatabaseName: string);
begin
  AOPDMSManager := TtiOPFManager.Create;
  tiQueryXMLLight.RegisterPersistenceLayer(AOPDMSManager.PersistenceLayers);
  AOPDMSManager.DefaultPerLayer.CreateDatabase(ADatabaseName, '', '');
  AOPDMSManager.DefaultPerLayer.DBConnectionPools.Connect(ADatabaseName, '', '', '');
  AConfig := TTestTIObjectVisitorControllerConfig.Create(AOPDMSManager.VisitorManager);
  AConfig.FTIOPFManager := AOPDMSManager;
  AConfig.SetDatabaseAndPersistenceLayerNames(cTIPersistXMLLight, ADatabaseName);
  AVisitorController := TTestTIObjectVisitorController.Create(
    AOPDMSManager.VisitorManager, AConfig);
  AVisitorController.FTIOPFManager := AOPDMSManager;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_BeforeExecuteVisitorGroup;
var
  LM:      TtiOPFManager;
  LVC:     TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM      := nil;
  LConfig := nil;
  LVC     := nil;
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
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisitor := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor := TTestTIObjectVisitor.Create;

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
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisitor := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor := TTestTIObjectVisitor.Create;

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
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisitor: TTestTIObjectVisitor;
  LVisited: TTestTIObjectVisited;
  LTouchedByVisitorList: TtiTouchedByVisitorList;
  LTouchedByVisitor: TtiTouchedByVisitor;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisitor := nil;
  LVisited := nil;
  LTouchedByVisitorList := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisitor := TTestTIObjectVisitor.Create;
    LVisited          := TTestTIObjectVisited.Create;
    LTouchedByVisitorList := TtiTouchedByVisitorList.Create(True);
    LTouchedByVisitor := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
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
var
  LM:      TtiOPFManager;
  LVC:     TTestTIObjectVisitorController;
  LConfig: TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM      := nil;
  LConfig := nil;
  LVC     := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVC.BeforeExecuteVisitorGroup;
    CheckEquals(1, LM.DefaultDBConnectionPool.CountLocked);
    LVC.AfterExecuteVisitorGroupError;
    CheckNull(LVC.Database);
    CheckEquals(0, LM.DefaultDBConnectionPool.CountLocked);
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

const
  CErrorTestException = 'Test exception';

type
  TtiObjectSensingVisitor = class(TtiObjectVisitor)
  protected
    procedure Init; override;
    procedure SetupParams; override;
    procedure Final(const AVisited: TtiObject); override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TtiObjectSensingVisitorException = class(TtiObjectSensingVisitor)
  protected
    procedure SetupParams; override;
  end;

  TtiObjectSensingVisited = class(TtiObject)
  private
    FData: TStringList;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property Data: TStringList read FData;
  end;

constructor TtiObjectSensingVisited.Create;
begin
  inherited;
  FData := TStringList.Create;
end;

destructor TtiObjectSensingVisited.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TtiObjectSensingVisitorException.SetupParams;
begin
  inherited;
  raise Exception.Create(CErrorTestException);
end;

procedure TtiObjectSensingVisitor.Execute(const AVisited: TtiVisited);
var
  LVisited: TtiObjectSensingVisited;
begin
  inherited Execute(AVisited);
  LVisited := (AVisited as TtiObjectSensingVisited);
  LVisited.Data.Add(ClassName + '.Execute\' + AVisited.ClassName);
  Init;
  SetupParams;
end;

procedure TtiObjectSensingVisitor.Final(const AVisited: TtiObject);
begin
  (AVisited as TtiObjectSensingVisited).Data.Add(ClassName + '.Final\' + AVisited.ClassName);
end;

procedure TtiObjectSensingVisitor.Init;
begin
  (Visited as TtiObjectSensingVisited).Data.Add(ClassName + '.Init\' + Visited.ClassName);
end;

procedure TtiObjectSensingVisitor.SetupParams;
begin
  (Visited as TtiObjectSensingVisited).Data.Add(ClassName + '.SetupParams\' + Visited.ClassName);
end;

procedure TTestTIVisitorDB.TIVisitorManager_ExecutePassDatabaseName;
var
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisited: TtiObjectSensingVisited;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisited := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LM.VisitorManager.RegisterVisitor('test', TtiObjectSensingVisitor);
    LVisited := TtiObjectSensingVisited.Create;
    LM.VisitorManager.Execute('test', LVisited, LDatabaseName, cTIPersistXMLLight);
    CheckEquals(4, LVisited.Data.Count);
    CheckEquals('TtiObjectSensingVisitor.Execute\TtiObjectSensingVisited',
      LVisited.Data.Strings[0]);
    CheckEquals('TtiObjectSensingVisitor.Init\TtiObjectSensingVisited', LVisited.Data.Strings[1]);
    CheckEquals('TtiObjectSensingVisitor.SetupParams\TtiObjectSensingVisited',
      LVisited.Data.Strings[2]);
    CheckEquals('TtiObjectSensingVisitor.Final\TtiObjectSensingVisited', LVisited.Data.Strings[3]);
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisited.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIVisitorManager_ExecutePassDatabaseNameException;
var
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisited: TtiObjectSensingVisited;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisited := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LM.VisitorManager.RegisterVisitor('test', TtiObjectSensingVisitorException);
    LVisited := TtiObjectSensingVisited.Create;
    try
      LM.VisitorManager.Execute('test', LVisited, LDatabaseName, cTIPersistXMLLight);
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
        CheckEquals(CErrorTestException, e.message);
    end;
    CheckEquals(3, LVisited.Data.Count);
    CheckEquals('TtiObjectSensingVisitorException.Execute\TtiObjectSensingVisited',
      LVisited.Data.Strings[0]);
    CheckEquals('TtiObjectSensingVisitorException.Init\TtiObjectSensingVisited',
      LVisited.Data.Strings[1]);
    CheckEquals('TtiObjectSensingVisitorException.SetupParams\TtiObjectSensingVisited',
      LVisited.Data.Strings[2]);
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisited.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIVisitorManager_ExecuteUnregisteredVisitor;
var
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisited: TtiObject;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisited := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LVisited := TtiObject.Create;
    try
      LM.VisitorManager.Execute('test', LVisited, LDatabaseName, cTIPersistXMLLight);
      Fail(CErrorExceptionNotRaised);
    except
      on e: Exception do
      begin
        CheckIs(E, EtiOPFProgrammerException);
        CheckFormattedMessage(CErrorInvalidVisitorGroup, ['test'], E.Message);
      end;
    end;
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisited.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

procedure TTestTIVisitorDB.TIVisitorManager_Execute;
var
  LM:       TtiOPFManager;
  LVC:      TTestTIObjectVisitorController;
  LConfig:  TTestTIObjectVisitorControllerConfig;
  LDatabaseName: string;
  LVisited: TtiObjectSensingVisited;
begin
  LDatabaseName := TempFileName('TestTIVisitorDB.xml');
  LM       := nil;
  LConfig  := nil;
  LVC      := nil;
  LVisited := nil;
  try
    CreateTIObjectVisitorControllerTestInstance(LM, LVC, LConfig, LDatabaseName);
    LM.VisitorManager.RegisterVisitor('test', TtiObjectSensingVisitor);
    LVisited := TtiObjectSensingVisited.Create;
    LM.VisitorManager.Execute('test', LVisited);
    CheckEquals(4, LVisited.Data.Count);
    CheckEquals('TtiObjectSensingVisitor.Execute\TtiObjectSensingVisited',
      LVisited.Data.Strings[0]);
    CheckEquals('TtiObjectSensingVisitor.Init\TtiObjectSensingVisited', LVisited.Data.Strings[1]);
    CheckEquals('TtiObjectSensingVisitor.SetupParams\TtiObjectSensingVisited',
      LVisited.Data.Strings[2]);
    CheckEquals('TtiObjectSensingVisitor.Final\TtiObjectSensingVisited', LVisited.Data.Strings[3]);
  finally
    LConfig.Free;
    LVC.Free;
    LM.Free;
    LVisited.Free;
    tiDeleteFile(LDatabaseName);
  end;
end;

end.
