unit tiVisitorDBAutoGen;

interface
uses
  tiQuery
  ,tiVisitorDB
  ,tiVisitor
 ;

const
  cErrorInVisitorExecute = 'Error in Visitor.Execute. Visitor: %s Visited: %s Message %s';

type

  TVisDBAutoGenRead = class(TVisOwnedQrySelectAbs)
  private
    FQueryParams : TtiQueryParams;
    FTableName : string;
  protected
    procedure OpenQuery; override;
    property TableName : string read FTableName write FTableName;
    property QueryParams : TtiQueryParams read FQueryParams;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;


  TVisDBAutoGenUpdate = class(TtiObjectVisitor)
  private
    FQueryParams : TtiQueryParams;
    FQueryWhere : TtiQueryParams;
    FTableName : string;
    FQueryType: TtiQueryType;
  protected
    property    QueryType: TtiQueryType read FQueryType Write FQueryType;
    property    TableName : string read FTableName write FTableName;
    property    QueryParams : TtiQueryParams read FQueryParams;
    property    QueryWhere : TtiQueryParams read FQueryWhere;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Execute(const AData: TtiVisited); override;
  end;


  TVisDBAutoGenDelete = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: Boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;


implementation
uses
  // tiOPF
   tiOPFManager
  ,tiObject
  ,tiConstants
  ,tiExcept
  ,tiCriteria
  // Delphi
  ,SysUtils
 ;
  
{ TVisDBAutoGenRead }

constructor TVisDBAutoGenRead.Create;
begin
  inherited;
  FQueryParams := TtiQueryParams.Create;
end;

destructor TVisDBAutoGenRead.destroy;
begin
  FQueryParams.Free;
  inherited;
end;

procedure TVisDBAutoGenUpdate.Execute(const AData: TtiVisited);
begin

  if GTIOPFManager.Terminated then
    Exit; //==>

  try
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
    try
      SetupParams;
      Assert(FTableName <> '', 'TableName not assigned');
      case FQueryType of
        qtInsert : begin
                     Assert(FQueryWhere.Count = 0, 'FQueryWhere.Count <> 0');
                     Query.InsertRow(FTableName, FQueryParams);
                   end;
        qtUpdate : begin
                     Assert(FQueryParams.Count <> 0, 'FQueryParams.Count = 0');
                     Query.UpdateRow(FTableName, FQueryParams, FQueryWhere);
                   end;
        qtDelete : begin
                     Assert(FQueryParams.Count = 0, 'FQueryParams.Count <> 0');
                     Query.DeleteRow(FTableName, FQueryWhere);
                   end;
      else
        raise Exception.Create(cTIOPFExcMsgTIQueryType);
      end;
    finally
      UnInit;
    end;

  except
    on E: Exception do
      raise EtiOPFProgrammerException.CreateFmt(cErrorInVisitorExecute,
        [ClassName, Visited.ClassName, E.Message]);
  end;

end;

procedure TVisDBAutoGenRead.OpenQuery;
var
  lFilter: ItiFiltered;
  lCriteria: TtiCriteria;
begin
  lCriteria := nil;

  if Supports(Visited, ItiFiltered, lFilter) then
  begin
    if lFilter.HasCriteria then
      lCriteria := lFilter.GetCriteria;
  end;

  Assert(FTableName <> '', 'TableName not assigned');

  // use old SelectRow if we don't have a lCriteria just in case!
  if Assigned(lCriteria) then
    Query.SelectRow(FTableName, FQueryParams, lCriteria)
  else
    Query.SelectRow(FTableName, FQueryParams);
end;

{ TVisDBAutoGenUpdate }

constructor TVisDBAutoGenUpdate.Create;
begin
  inherited;
  FQueryParams := TtiQueryParams.Create;
  FQueryWhere := TtiQueryParams.Create;
  FQueryType  := qtSelect;
end;

destructor TVisDBAutoGenUpdate.destroy;
begin
  FQueryParams.Free;
  FQueryWhere.Free;
  inherited;
end;

{ TVisDBAutoGenDelete }

function TVisDBAutoGenDelete.AcceptVisitor: Boolean;
begin
  Result := (Visited.ObjectState = posDelete);
end;

procedure TVisDBAutoGenDelete.Init;
begin
  QueryType := qtDelete;
end;

procedure TVisDBAutoGenDelete.SetupParams;
begin
  Assert(Visited.TestValid(TtiObject), CTIErrorInvalidObject);
  {$IFDEF OID_AS_INT64}
    QueryWhere.SetValueAsInteger('OID', Visited.OID);
  {$ELSE}
    QueryWhere.SetValueAsString('OID', Visited.OID.AsString);
  {$ENDIF}
end;

end.
