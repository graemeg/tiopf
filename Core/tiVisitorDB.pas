{

  // Templates
  // Code template key combination: VR
  TVisXXX_Read = class(TVisOwnedQrySelect)
  protected
    function  AcceptVisitor : boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  // Code template key combination: VC
  TVisXXX_Create = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor : boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

  // Code template key combination: VU
  TVisXXX_Update = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor : boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

  // Code template key combination: VD
  TVisXXX_Delete = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor : boolean; override;
    procedure Init         ; override;
  end;

}


unit tiVisitorDB;

{$I tiDefines.inc}

interface
uses
   tiVisitor
  ,tiObject
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ,tiQuery
  ,SysUtils
  ,Classes
 ;

const
  CErrorDefaultPersistenceLayerNotAssigned = 'Attempt to connect to the default persistence layer, but the default persistence layer has not been assigned.';
  CErrorDefaultDatabaseNotAssigned = 'Attempt to connect to the default database but the default database has not been assigned.';
  CErrorAttemptToUseUnRegisteredPersistenceLayer = 'Attempt to use unregistered persistence layer "%s"';
  CErrorAttemptToUseUnConnectedDatabase = 'Attempt to use unconnected database "%s"';
type

  // A visitor manager for TVisDBAbs visitors
  TtiObjectVisitorController = class(TtiVisitorController)
  private
    FPersistenceLayer: TtiPersistenceLayer;
    FPooledDB: TPooledDB;
    FDatabase: TtiDatabase;
  protected
    function  PersistenceLayerName: string;
    function  DatabaseName: string;
  public
    procedure BeforeExecuteVisitorGroup(const AVisitors : TList); override;
    procedure BeforeExecuteVisitor(const AVisitor : TtiVisitor); override;
    procedure AfterExecuteVisitor(const AVisitor : TtiVisitor ); override;
    procedure AfterExecuteVisitorGroup(const AVisitors : TList); override;
    procedure AfterExecuteVisitorGroupError(const AVisitors : TList); override;
  end;

  TtiObjectVisitorControllerConfig = class(TtiVisitorControllerConfig)
  private
    FDatabaseName: string;
    FPersistenceLayerName: string;
  protected
    function TIOPFManager: TObject; virtual;
  public
    property PersistenceLayerName: string read FPersistenceLayerName;
    property DatabaseName: string read FDatabaseName;
    procedure SetDatabaseAndPersistenceLayerNames(
      const APersistenceLayerName, ADBConnectionName: string);
  end;

  TtiObjectVisitorManager = class(TtiVisitorManager)
  public
    function Execute(const AGroupName: string;
               const AVisited: TtiVisited;
               const ADBConnectionName: string;
               const APersistenceLayerName: string = ''): string; overload;
  end;


  // Adds an owned query object
  // Note: It is not necessary to manually lock and unlock DBConnections
  // from this level and below - TVisitorMgrDB does this for you.
  // Adds a pooled database connection
  TtiPerObjVisitor = class(TtiVisitor)
  private
    FVisitedList : TList;
    FDatabase    : TtiDatabase;
    FQuery       : TtiQuery;
    function    GetQuery: TtiQuery;
    procedure   SetQuery(const AValue: TtiQuery);
  protected
    function    GetVisited: TtiObject; reintroduce;
    procedure   SetVisited(const AValue: TtiObject); reintroduce; virtual;
    procedure   LogQueryTiming(const pQueryName : string;
                                pQueryTime : integer;
                                pScanTime : integer);
    procedure   SetupParams    ; virtual;
    procedure   Final; virtual;
    property    VisitedList : TList read FVisitedList;
    property    Database : TtiDatabase read FDatabase write FDatabase;
    property    Query : TtiQuery read GetQuery write SetQuery;
  public
    Constructor Create; override;
    destructor  Destroy; override;
    procedure   Execute(const AVisited: TtiVisited); override;
    class function    VisitorControllerClass: TtiVisitorControllerClass; override;
    property    Visited: TtiObject read GetVisited write SetVisited;
  end;
  

  // Don't use TVisOwnedQrySelectAbs as the parent for any of your visitors,
  // it's for internal tiOPF use only.
  TVisOwnedQrySelectAbs = class(TtiPerObjVisitor)
  protected
    procedure   Init; virtual;
    procedure   MapRowToObject ; virtual;
    procedure   OpenQuery; virtual; abstract;
  public
    procedure   Execute(const AData: TtiVisited); override;
  end;
  

  // ToDo: Rename to TtiVisitorSelect
  TVisOwnedQrySelect = class(TVisOwnedQrySelectAbs)
  protected
    procedure OpenQuery; override;
  end;

  TtiVisitorSelect = class(TVisOwnedQrySelect);

  // ToDo: Rename to TtiVisitorUpdate
  TVisOwnedQryUpdate = class(TtiPerObjVisitor)
  protected
    procedure   Init; virtual;
  public
    procedure   Execute(const AData: TtiVisited); override;
  end;

  TtiVisitorUpdate = class(TVisOwnedQryUpdate);
  

implementation
uses
   tiUtils
  ,tiLog
  ,tiOPFManager
  ,tiOID
  ,tiConstants
  ,tiExcept
  ,Dialogs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,Types
  {$ENDIF LINUX}
 ;


{ TtiPerObjVisitor }

constructor TtiPerObjVisitor.Create;
begin
  inherited;
  FVisitedList    := TList.Create;
  // Query is created and assigned by TtiPerObjVisitorCtrlr
end;

destructor TtiPerObjVisitor.destroy;
begin
  FVisitedList.Free;
  FQuery.Free;
  inherited;
end;

procedure TtiPerObjVisitor.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  if AcceptVisitor then
    VisitedList.Add(AVisited);
end;

procedure TtiPerObjVisitor.Final;
begin
  case TtiObject(Visited).ObjectState of
  posDeleted :; // Do nothing
  posDelete : TtiObject(Visited).ObjectState := posDeleted;
  else
    TtiObject(Visited).ObjectState := posClean;
  end;
end;

function TtiPerObjVisitor.GetQuery: TtiQuery;
begin
  Assert(FQuery <> nil, 'FQuery not assigned');
  result := FQuery;
end;

function TtiPerObjVisitor.GetVisited: TtiObject;
begin
  result := TtiObject(inherited GetVisited);
end;

procedure TtiPerObjVisitor.LogQueryTiming(const pQueryName: string;
                                    pQueryTime : integer;
                                    pScanTime: integer);
var
  lClassName : string;
begin

  lClassName := ClassName;

  // We don't want to log access to the SQLManager queries, and this
  // is one possible way of blocking this.
  if SameText(lClassName, 'TVisReadGroupPK') or
     SameText(lClassName, 'TVisReadQueryPK') or
     SameText(lClassName, 'TVisReadQueryDetail') or
     SameText(lClassName, 'TVisReadParams') or
     SameText(lClassName, 'TVisReadQueryByName') then
    Exit; //==>

  Log({tiPadR(lClassName, cuiQueryTimingSpacing) +}
       tiPadR(pQueryName, 20) + ' ' +
       tiPadR(IntToStr(pQueryTime), 7) +
       tiPadR(IntToStr(pScanTime), 7),
       lsQueryTiming)

end;

procedure TtiPerObjVisitor.SetQuery(const AValue: TtiQuery);
begin
  Assert(FQuery = nil, 'FQuery already assigned');
  FQuery := AValue;
end;

procedure TtiPerObjVisitor.SetupParams;
begin
// Do nothing
end;

procedure TtiPerObjVisitor.SetVisited(const AValue: TtiObject);
begin
  inherited SetVisited(AValue);
end;

class function TtiPerObjVisitor.VisitorControllerClass: TtiVisitorControllerClass;
begin
  result := TtiObjectVisitorController;
end;

procedure TtiObjectVisitorController.AfterExecuteVisitorGroup(const AVisitors : TList);
var
  i, j : integer;
  lVisitor : TtiPerObjVisitor;
  lVisited : TtiObject;
begin
  FDatabase.Commit;

  // ToDo: Refactor this so it touches the list of visited objects, which contains
  //       pairs of Visitor-Object relationships
  for i := 0 to AVisitors.Count - 1 do begin
    // This should not be necessary, however there are times when a visitor
    // group will contain several types of visitors. We should add some code so
    // we do not get passed a list of visitors which contains incompatable types
    if not (TObject(AVisitors.Items[i]) is TtiPerObjVisitor) then
      Continue; //==>
    lVisitor := (TObject(AVisitors.Items[i]) as TtiPerObjVisitor);
    for j := 0 to lVisitor.VisitedList.Count - 1 do
    begin
      lVisited := TtiObject(lVisitor.VisitedList.Items[j]);
      lVisitor.Visited := lVisited;
      lVisitor.Final;
      lVisitor.Visited := nil;
    end;
  end;

  inherited AfterExecuteVisitorGroup(AVisitors);

  FPersistenceLayer.DBConnectionPools.UnLock(DatabaseName, FPooledDB);

end;

procedure TtiObjectVisitorController.AfterExecuteVisitorGroupError(const AVisitors : TList);
begin
  FDatabase.RollBack;
  FPersistenceLayer.DBConnectionPools.UnLock(DatabaseName, FPooledDB);
end;

procedure TtiObjectVisitorController.AfterExecuteVisitor(const AVisitor : TtiVisitor);
begin
  TtiPerObjVisitor(AVisitor).Database := nil;
end;

procedure TtiObjectVisitorController.BeforeExecuteVisitorGroup(const AVisitors : TList);
begin
  FPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PersistenceLayerName);
  Assert(FPersistenceLayer <> nil, 'Unable to find RegPerLayer <' + PersistenceLayerName +'>');
  FPooledDB := FPersistenceLayer.DBConnectionPools.Lock(DatabaseName);
  FDatabase:= FPooledDB.Database;
  FDatabase.StartTransaction;
end;

function TtiObjectVisitorController.DatabaseName: string;
begin
  result:= (Config as TtiObjectVisitorControllerConfig).DatabaseName;
end;

function TtiObjectVisitorController.PersistenceLayerName: string;
begin
  result:= (Config as TtiObjectVisitorControllerConfig).PersistenceLayerName;
end;

procedure TtiObjectVisitorController.BeforeExecuteVisitor(const AVisitor : TtiVisitor);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(FDatabase.TestValid, cTIInvalidObjectError);
  TtiPerObjVisitor(AVisitor).Database := FDatabase;
  TtiPerObjVisitor(AVisitor).Query := FDatabase.CreateTIQuery;
  TtiPerObjVisitor(AVisitor).Query.AttachDatabase(FDatabase);
end;

procedure TVisOwnedQrySelectAbs.Execute(const AData: TtiVisited);
  procedure _ScanQuery;
  begin
    Query.ContinueScan := True;
    while (not Query.EOF) and
          (Query.ContinueScan) and
          (not gTIOPFManager.Terminated) do
    begin
      MapRowToObject;
      Query.Next;
    end;
    Query.Close;
  end;
var
  liStart : DWord;
  liQueryTime : DWord;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Assert(Database <> nil, 'DBConnection not set in ' + ClassName);

  if AData <> nil then begin
    Visited := TtiObject(AData);
  end else begin
    Visited := nil;
  end;

  Init;

  SetupParams;
  liStart := tiGetTickCount;
  OpenQuery;
  try
    liQueryTime := tiGetTickCount - liStart;
    liStart := tiGetTickCount;
    _ScanQuery;
    LogQueryTiming(ClassName, liQueryTime, tiGetTickCount - liStart);
  finally
    Query.Close;
  end;
end;

procedure TVisOwnedQrySelectAbs.Init;
begin
  // Do nothing
end;

procedure TVisOwnedQrySelectAbs.MapRowToObject;
begin
  raise exception.Create('MapRowToObject has not been ' +
                          'overridden in the concrete: ' + ClassName);
end;

procedure TVisOwnedQryUpdate.Execute(const AData: TtiVisited);
var
  lStart : DWord;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>
  Inherited Execute(AData);
  if not AcceptVisitor then
    exit; //==>
  Init;
  lStart := tiGetTickCount;
  SetupParams;
  Query.ExecSQL;
  LogQueryTiming(ClassName, tiGetTickCount - lStart, 0);
end;

procedure TVisOwnedQryUpdate.Init;
begin
  // Do nothing
end;

procedure TVisOwnedQrySelect.OpenQuery;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>
  Query.Open;
end;

{ TtiObjectVisitorManager }

function TtiObjectVisitorManager.Execute(const AGroupName: string;
  const AVisited: TtiVisited; const ADBConnectionName,
  APersistenceLayerName: string): string;
var
  FVisitorControllerConfig: TtiObjectVisitorControllerConfig;
begin
  FVisitorControllerConfig:= TtiObjectVisitorControllerConfig.Create;
  try
    FVisitorControllerConfig.SetDatabaseAndPersistenceLayerNames(APersistenceLayerName, ADBConnectionName);
    ProcessVisitors(AGroupName, AVisited, FVisitorControllerConfig);
  finally
    FVisitorControllerConfig.Free;
  end;
end;

{ TtiObjectVisitorControllerConfig }

procedure TtiObjectVisitorControllerConfig.SetDatabaseAndPersistenceLayerNames(
  const APersistenceLayerName, ADBConnectionName: string);
var
  LTIOPFManager: TtiOPFManager;
begin
  LTIOPFManager:= TtiOPFManager(TIOPFManager);

  if APersistenceLayerName = '' then
  begin
    if not Assigned(TtiOPFManager(TIOPFManager).DefaultPerLayer) then
      raise EtiOPFDataException.Create(CErrorDefaultPersistenceLayerNotAssigned);
    FPersistenceLayerName:= LTIOPFManager.DefaultPerLayer.PerLayerName;
  end else
    FPersistenceLayerName:= APersistenceLayerName;

  if ADBConnectionName = '' then
  begin
    if not Assigned(TtiOPFManager(TIOPFManager).DefaultDBConnectionPool) then
      raise EtiOPFDataException.Create(CErrorDefaultDatabaseNotAssigned);
    FDatabaseName := LTIOPFManager.DefaultDBConnectionName;
  end else
    FDatabaseName := ADBConnectionName;

  if not LTIOPFManager.PersistenceLayers.IsLoaded(FPersistenceLayerName) then
    raise EtiOPFDataException.CreateFmt(CErrorAttemptToUseUnRegisteredPersistenceLayer, [FPersistenceLayerName])
  else if not LTIOPFManager.PersistenceLayers.FindByPerLayerName(
    FPersistenceLayerName).DBConnectionPools.IsConnected(FDatabaseName) then
    raise EtiOPFDataException.CreateFmt(CErrorAttemptToUseUnConnectedDatabase, [FDatabaseName])

end;

function TtiObjectVisitorControllerConfig.TIOPFManager: TObject;
begin
  result:= GTIOPFManager;
end;

end.


