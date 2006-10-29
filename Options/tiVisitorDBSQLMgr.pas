{$I tiDefines.inc}

unit tiVisitorDBSQLMgr;

interface
uses
   tiDBConnectionPool
  ,tiVisitor
  ,SysUtils
  ,tiQuery
  ,Classes
  ,tiObject
  ,tiVisitorDB
 ;

type

  TSQLMgrDatabaseMappings = class;
  TSQLMgrDatabaseMapping  = class;

  TSQLMgrDatabaseMappings = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TSQLMgrDatabaseMapping; reintroduce;
    procedure   SetItems(i: integer; const AValue: TSQLMgrDatabaseMapping); reintroduce;
  public
    property    Items[i:integer]: TSQLMgrDatabaseMapping read GetItems write SetItems;
    procedure   Add(AObject : TSQLMgrDatabaseMapping  ; ADefaultDispOrder : boolean = true); reintroduce;
    procedure   RegisterMapping(const ADatabaseName : string; pSQLMgrDataSource : string);
    procedure   UnRegisterMapping(const ADatabaseName : string);
    function    FindSQLMgrDataSourceByDatabaseName(const ADatabaseName : string): string;
  published
  end;

  TSQLMgrDatabaseMapping  = class(TtiObject)
  private
    FDatabaseName: string;
    FSQLMgrDataSource: string;
  protected
    function    GetOwner: TSQLMgrDatabaseMappings; reintroduce;
    procedure   SetOwner(const AValue: TSQLMgrDatabaseMappings); reintroduce;
  public
    property    Owner      : TSQLMgrDatabaseMappings read GetOwner      write SetOwner;
  published
    property SQLMgrDataSource : string read FSQLMgrDataSource write FSQLMgrDataSource;
    property DatabaseName    : string read FDatabaseName     write FDatabaseName;
  end;

  // Uses the SQLManager to get the SQL
  TVisSQLMgrAbs = class(TtiPerObjVisitor)
  private
    FsQueryName     : string;
    FbInitCalled    : boolean;
    FbGetQueryCalled : boolean;

  protected
    procedure   Init           ; virtual;
    procedure   DoInit;

    procedure   DoSetupParams;
    procedure   DoGetQuery;
    procedure   AssignSQLFromSQLManager(AQuery : TtiQuery; pSQLMgrDataSource : string; pQueryName : string);

  public
    Constructor Create; override;
    procedure   Execute(const AData : TtiVisited); override;
    property    QueryName : string read FsQueryName write FsQueryName;

  end;

  // Perhaps this could be replaced with a property on the TtiObject which stops
  // the object state being set to diryt in the first place - or even better, just another
  // object state would do.
  TVisSQLMgrObjectStateDeleted = class(TVisSQLMgrAbs)
  protected
    function  AcceptVisitor : boolean; override;
    procedure Init             ; override;
    procedure SetupParams      ; override;
    procedure Final            ; override;
  end;

  TVisSQLMgrSelect = class(TVisSQLMgrAbs)
  protected
    procedure MapRowToObject; virtual;
  public
    constructor Create; override;
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisSQLMgrUpdate = class(TVisSQLMgrAbs)
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisSQLMgrDelete = class(TVisSQLMgrUpdate)
  protected
    // Override this and check the visitor's class type
    function  AcceptVisitor : boolean; override;
    // Override this to set the query name
    // procedure Init             ; override;
    // (We could set the table name using a macro param, but this will reduce
    //  the value of any compile time checking against the db structure, so it
    //  is not done.)
    procedure SetupParams      ; override;
  end;

function gSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings;

implementation
uses
   tiUtils
  ,tiLog
  ,tiOPFManager
  ,tiSQLMgr_BOM
  ,tiOID
  ,tiExcept
  ,Dialogs
  ,Windows
 ;

var
  uSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings;

function gSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings;
begin
  if uSQLMgrDatabaseMappings = nil then
    uSQLMgrDatabaseMappings := TSQLMgrDatabaseMappings.Create;
  result := uSQLMgrDatabaseMappings;
end;
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisSQLMgrAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisSQLMgrSelect
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisSQLMgrSelect.Create;
begin
  inherited;
end;

procedure TVisSQLMgrSelect.Execute(const AData : TtiVisited);
  procedure _OpenQuery;
  begin
    if gTIOPFManager.Terminated then
      Exit; //==>
    Query.Open;
  end;

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

  if gTIOPFManager.Terminated then Exit; //==>
  try
    Inherited Execute(AData);

    if not AcceptVisitor then
      Exit; //==>

    Assert(Database <> nil, 'Database not set in ' + ClassName);

    if AData <> nil then begin
      Visited := TtiObject(AData);
    end else begin
      Visited := nil;
    end;

    DoInit;
    DoGetQuery;
    DoSetupParams;
    liStart := GetTickCount;
    _OpenQuery;
    try
      liQueryTime := GetTickCount - liStart;
      liStart := GetTickCount;
      _ScanQuery;
      LogQueryTiming(QueryName, liQueryTime, GetTickCount-liStart);
    finally
      Query.Close;
    end;
  except
    on e:exception do
      raise EtiOPFInternalException.Create(
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString(TtiObject(Visited).OID) + Cr +
                      '  ObjectState:  ' + TtiObject(Visited).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName + Cr +
                      '  Message:      ' + e.Message);

  end;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisSQLMgrUpdate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TVisSQLMgrUpdate.Execute(const  AData: TtiVisited);
  procedure _ExecuteQuery;
  var
    liStart : DWord;
  begin
    liStart := GetTickCount;
    Query.ExecSQL;
    LogQueryTiming(QueryName, GetTickCount - liStart, 0);
  end;
begin
  try
    Inherited Execute(AData);
    if not AcceptVisitor then
      exit; //==>

    Init;
    DoGetQuery;
      DoSetupParams;
      _ExecuteQuery;
  except
    on e:exception do
      raise EtiOPFInternalException.Create(
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString(TtiObject(Visited).OID) + Cr +
                      '  ObjectState:  ' + TtiObject(Visited).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName + Cr +
                      '  Message:      ' + e.Message);

  end;

end;


procedure TVisSQLMgrAbs.Init;
begin
  raise exception.Create('Init has not been ' +
                          'overridden in the concrete: ' + ClassName);
end;

procedure TVisSQLMgrSelect.MapRowToObject;
begin
  raise exception.Create('MapRowToObject has not been ' +
                          'overridden in the concrete: ' + ClassName);
end;

constructor TVisSQLMgrAbs.Create;
begin
  inherited;
  FbInitCalled    := false;
  FbGetQueryCalled := false;
end;

procedure TVisSQLMgrAbs.Execute(const AData: TtiVisited);
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if gTIOPFManager.Terminated then
    Exit; //==>

  Assert(AData <> nil,
          'Visited is nil');
  Assert((AData is TtiObject),
          'Visited is not a TtiObject' + #13 +
          'Visited: ' + AData.ClassName + #13 +
         'Visitor: ' + ClassName + #13);
end;

procedure TVisSQLMgrAbs.DoInit;
begin
  if FbInitCalled then
    Exit; //==>

  if gTIOPFManager.Terminated then
    Exit; //==>
    
  Init;
  FbInitCalled := true;
  Assert(QueryName <> '', 'QueryName unassigned.');
end;

procedure TVisSQLMgrAbs.DoSetupParams;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>
  SetupParams;
end;

procedure TVisSQLMgrAbs.DoGetQuery;
var
  lSQLMgrDataSource : string;
begin
  if FbGetQueryCalled then
    Exit; //==>

  if gTIOPFManager.Terminated then
    Exit; //==>

  Assert(QueryName <> '', 'QueryName not assigned');
  Assert(VisitorController <> nil, 'VisitorMgr not assigned');
  Assert(VisitorController.SQLMgrDataSource <> '', 'SQLMgrDataSource not assigned');

  // This will fail in the tiAppLaunchWeb because it is linking in the
  // tiQueryHTTP directly, and there is no mapping between query factory and
  // db setup. Fix.

  lSQLMgrDataSource :=
    gSQLMgrDatabaseMappings.FindSQLMgrDataSourceByDatabaseName(
      VisitorController.DBConnectionName);

  Assert(lSQLMgrDataSource <> '', 'SQLMgrDataSource not assigned.');

  AssignSQLFromSQLManager(Query, lSQLMgrDataSource, QueryName);

  FbGetQueryCalled := true;
  Assert(Query <> nil, 'Query not assigned');

  Assert(Database <> nil, 'DBConnection not set in ' + ClassName);

end;

{ TVisSQLMgrObjectStateDeleted }

function TVisSQLMgrObjectStateDeleted.AcceptVisitor: boolean;
begin
  result := (Visited is TtiObject) and
            (TtiObject(Visited).ObjectState = posDelete);
end;

procedure TVisSQLMgrObjectStateDeleted.Final;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>
  TtiObject(Visited).ObjectState := posDeleted;
end;

procedure TVisSQLMgrObjectStateDeleted.Init;
begin
  // Do nothing
end;

procedure TVisSQLMgrObjectStateDeleted.SetupParams;
begin
  // Do nothing
end;

{ TVisSQLMgrDelete }

function TVisSQLMgrDelete.AcceptVisitor: boolean;
begin
  result := (Visited.ObjectState = posDelete);
end;

procedure TVisSQLMgrDelete.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID']:= Visited.OID;
  {$ELSE}
    Visited.OID.AssignToTIQuery('OID', Query);
  {$ENDIF}
end;

{ TSQLMgrDatabaseMappings }

procedure TSQLMgrDatabaseMappings.Add(AObject: TSQLMgrDatabaseMapping; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TSQLMgrDatabaseMappings.FindSQLMgrDataSourceByDatabaseName(
  const ADatabaseName: string): string;
var
  lData : TSQLMgrDatabaseMapping;
begin
  lData := TSQLMgrDatabaseMapping(FindByProps(['DatabaseName'], [LowerCase(ADatabaseName)]));
  Assert(lData <> nil,
          'Unable to find SQLMgrDatabaseMapping for <' +
          ADatabaseName + '>');
  result := lData.SQLMgrDataSource;
end;

function TSQLMgrDatabaseMappings.GetItems(i: integer): TSQLMgrDatabaseMapping;
begin
  result := TSQLMgrDatabaseMapping(inherited GetItems(i));
end;

procedure TSQLMgrDatabaseMappings.RegisterMapping(
  const ADatabaseName: string; pSQLMgrDataSource: string);
var
  lData : TSQLMgrDatabaseMapping;
begin
  lData := TSQLMgrDatabaseMapping.Create;
  lData.DatabaseName := LowerCase(ADatabaseName);
  lData.SQLMgrDataSource := LowerCase(pSQLMgrDataSource);
  Add(lData);
end;

procedure TSQLMgrDatabaseMappings.SetItems(i: integer; const AValue: TSQLMgrDatabaseMapping);
begin
  inherited SetItems(i, AValue);
end;

procedure TSQLMgrDatabaseMappings.UnRegisterMapping(const ADatabaseName: string);
var
  lData : TSQLMgrDatabaseMapping;
begin
  lData := TSQLMgrDatabaseMapping(FindByProps(['DatabaseName'], [LowerCase(ADatabaseName)]));
  if lData <> nil then
    Remove(lData);
end;

{ TSQLMgrDatabaseMapping }

function TSQLMgrDatabaseMapping.GetOwner: TSQLMgrDatabaseMappings;
begin
  result := TSQLMgrDatabaseMappings(inherited GetOwner);
end;

procedure TSQLMgrDatabaseMapping.SetOwner(const AValue: TSQLMgrDatabaseMappings);
begin
  inherited SetOwner(AValue);
end;

procedure TVisSQLMgrAbs.AssignSQLFromSQLManager(AQuery: TtiQuery; pSQLMgrDataSource, pQueryName: string);
var
  lSQLMgr : TSQLMgr;
  lSQLMgrQuery  : TSQLMgrQuery;
  lsl : TStringList;
begin

  if gTIOPFManager.Terminated then
    Exit; //==>

  lSQLMgr := gSQLMgrs.FindByDatabaseName(pSQLMgrDataSource);
  if lSQLMgr = nil then
    raise EtiOPFInternalException.Create('Unable to find SQLMgr for database <' +
                    pSQLMgrDataSource + '>');

  lSQLMgrQuery := lSQLMgr.FindCreateQueryByName(pQueryName);

  if lSQLMgrQuery = nil then
    raise EtiOPFInternalException.Create('Query <' +
                    pQueryName +
                   '> not found in query factory.');
  // This was added in an attempt to force SetSQL to be called.
  // (SetSQL might contain some custom code that will not execute
  //  when SQL.Text := bla is called.
  lsl := TStringList.Create;
  try
    lsl.Text :=
    '/*' + lSQLMgrQuery.QueryName + '*/' + CrLf +
    lSQLMgrQuery.SQL;
    AQuery.SQL := lsl;
  finally
    lsl.Free;
  end;

end;

initialization

finalization
  uSQLMgrDatabaseMappings.Free;

end.


