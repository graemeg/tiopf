unit tiVisitorDB_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework
  {$IFNDEF FPC}
  ,TestFrameWork
  {$ENDIF}
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
  ,tiVisitorDB
  ,tiOPFManager
  ,tiExcept
  ,tiQueryXMLLight
  ,tiConstants
  ,tiUtils
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
    FTIOPFManager: TObject;
  protected
    function TIOPFManager: TObject; override;
    procedure SetTIOPFManager(const ATIOPFManager: TObject);
  end;

  procedure TTestObjectVisitorControllerConfig.SetTIOPFManager(
    const ATIOPFManager: TObject);
  begin
    FTIOPFManager:= ATIOPFManager;
  end;

  function TTestObjectVisitorControllerConfig.TIOPFManager: TObject;
  begin
    result:= FTIOPFManager;
  end;


procedure TTestTIVisitorDB.TIObjectVisitorControllerConfig_SetDatabaseAndPersistenceLayerNames;
var
  LVCC: TTestObjectVisitorControllerConfig;
  LM: TtiOPFManager;
  LDatabaseName: string;
begin
  LDatabaseName:= TempFileName('VisitorControllerConfig.xml');
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

    if FileExists(LDatabaseName) then
      tiDeleteFile(LDatabaseName);

  finally
    LVCC.Free;
    LM.Free;
  end;
end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitor;
begin

end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitorGroup;
begin

end;

procedure TTestTIVisitorDB.TIObjectVisitorController_AfterExecuteVisitorGroupError;
begin

end;

procedure TTestTIVisitorDB.TIObjectVisitorController_BeforeExecuteVisitor;
begin

end;

procedure TTestTIVisitorDB.TIObjectVisitorController_BeforeExecuteVisitorGroup;
begin

end;

procedure TTestTIVisitorDB.TIVisitorManager_Execute;
begin

end;

procedure TTestTIVisitorDB.TIVisitorManager_ExecutePassDatabaseName;
begin

end;

end.

