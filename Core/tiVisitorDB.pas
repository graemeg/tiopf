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
   tiDBConnectionPool
  ,tiVisitor
  ,SysUtils
  ,tiQuery
  ,Classes
  ,tiObject
 ;

type

  // A visitor manager for TVisDBAbs visitors
  TtiPerObjVisitorCtrlr = class(TtiVisitorCtrlr)
  private
    FPooledDB : TPooledDB;
    FtiQueryClass : TtiQueryClass;
  protected
    property  PooledDB : TPooledDB read FPooledDB write FPooledDB;
    procedure SetPerLayerName(const AValue: string); override;
  public
    procedure BeforeExecuteAll(AVisitors : TList)     ; override;
    procedure BeforeExecuteOne(AVisitor : TtiVisitor); override;
    procedure AfterExecuteOne(AVisitor : TtiVisitor ); override;
    procedure AfterExecuteAll(AVisitors : TList)      ; override;
    procedure AfterExecuteError(AVisitors : TList)    ; override;
  end;


  { TODO : Add a reference to the TUserName object so the _Svr units do not have to ref the Security_Cli unit. }

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
    function    VisitorControllerClass: TtiVisitorControllerClass; override;
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
  ,tiPersistenceLayers
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

function TtiPerObjVisitor.VisitorControllerClass: TtiVisitorControllerClass;
begin
  result := TtiPerObjVisitorCtrlr;
end;

procedure TtiPerObjVisitorCtrlr.AfterExecuteAll(AVisitors : TList);
var
  i, j : integer;
  lVisitor : TtiPerObjVisitor;
  lVisited : TtiObject;
  lRegPerLayer : TtiPersistenceLayer;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  FPooledDB.Database.Commit;

  for i := 0 to AVisitors.Count - 1 do begin
    // This should not be necessary, however there are times when a visitor
    // group will contain several types of visitors. We should add some code so
    // we do not get passed a list of visitors which contains incompatable types
    if not (TObject(AVisitors.Items[i]) is TtiPerObjVisitor) then
      Continue; //==>
    lVisitor := (TObject(AVisitors.Items[i]) as TtiPerObjVisitor);
    for j := 0 to lVisitor.VisitedList.Count - 1 do
    begin
      if gTIOPFManager.Terminated then
        Exit; //==>
      lVisited := TtiObject(lVisitor.VisitedList.Items[j]);
      lVisitor.Visited := lVisited;
      lVisitor.Final;
      lVisitor.Visited := nil;
    end;
  end;
  if gTIOPFManager.Terminated then
    Exit; //==>
  inherited AfterExecuteAll(AVisitors);

  if gTIOPFManager.Terminated then
    Exit; //==>

  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>');
  lRegPerLayer.DBConnectionPools.UnLock(DBConnectionName, FPooledDB);

end;

procedure TtiPerObjVisitorCtrlr.AfterExecuteError(AVisitors : TList);
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>');
  FPooledDB.Database.RollBack;
  lRegPerLayer.DBConnectionPools.UnLock(DBConnectionName, FPooledDB);
end;

procedure TtiPerObjVisitorCtrlr.AfterExecuteOne(AVisitor : TtiVisitor);
begin
  TtiPerObjVisitor(AVisitor).Database := nil;
end;

procedure TtiPerObjVisitorCtrlr.BeforeExecuteAll(AVisitors : TList);
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>');
  FPooledDB := lRegPerLayer.DBConnectionPools.Lock(DBConnectionName);
  FPooledDB.Database.StartTransaction;
end;

procedure TtiPerObjVisitorCtrlr.BeforeExecuteOne(AVisitor : TtiVisitor);
begin
  Assert(AVisitor is TtiPerObjVisitor, AVisitor.ClassName + ' not a TVisDBAbs');
  Assert(TtiPerObjVisitor(AVisitor).FQuery = nil, 'TtiPerObjVisitor(AVisitor).Query already assigned');
  Assert(FtiQueryClass <> nil, 'FtiQueryClass not assigned');
  TtiPerObjVisitor(AVisitor).FQuery := FtiQueryClass.Create;
  TtiPerObjVisitor(AVisitor).Database := FPooledDB.Database;
  TtiPerObjVisitor(AVisitor).FQuery.AttachDatabase(
    TtiPerObjVisitor(AVisitor).Database);
end;

procedure TtiPerObjVisitorCtrlr.SetPerLayerName(const AValue: string);
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  inherited;
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);
  FtiQueryClass := lRegPerLayer.tiQueryClass;
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

end.


