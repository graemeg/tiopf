unit tiVisitorCriteria;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiVisitor
  ,tiObject
  ,tiCriteria
  ,tiUtils        { GetPropNames }
  ,TypInfo
  ;

type
  TVisPerObjToSQL = class(TVisStringStream)
  private
    FGroupByList: TPerColumns;
    FOrderByList: TPerColumns;
    FWithComments: Boolean;
  protected
    function    AcceptVisitor: Boolean; override;
  public
    constructor Create(pWithComments: boolean = False); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   Execute(const pVisited: TtiVisited); override;
    function    GroupByClausesAsText: string;
    function    OrderByClausesAsText: string;
  end;


// Helper functions
function tiCriteriaAsSQL(pVisited: TtiObject; pWithComments: boolean = false): string;

function ToSelectClause(ACriteria: TPerSelectionCriteriaAbs): string; overload;
function ToSelectClause(ACriteria: TPerSQLCriteria): string; overload;
function ToSelectClause(ACriteria: TPerNullCriteria): string; overload;
function ToSelectClause(ACriteria: TPerExistsCriteria): string; overload;
function ToSelectClause(ACriteria: TPerBetweenCriteria): string; overload;
function ToSelectClause(ACriteria: TPerInCriteria): string; overload;
function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string; overload;

function AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;


implementation


function tiCriteriaAsSQL(pVisited: TtiObject; pWithComments: boolean = false): string;
var
  lVisitor: TVisPerObjToSQL;
begin
  lVisitor := TVisPerObjToSQL.Create(pWithComments);
  try
    pVisited.Iterate(lVisitor);
    result := lVisitor.Text;

    if TPerCriteria(pVisited).isEmbraced then
      result := '(' + result + ')' + CrLf
      else
      result := result + CrLf;

    result := result + lVisitor.GroupByClausesAsText + CrLf;

    result := result + lVisitor.OrderByClausesAsText + CrLf;

  finally
    lVisitor.Free;
  end;
end;

function ToSelectClause(ACriteria: TPerSelectionCriteriaAbs): string;
begin
  Result := ACriteria.Attribute + ACriteria.GetClause + ACriteria.Value;
end;

function ToSelectClause(ACriteria: TPerSQLCriteria): string;
begin
  Result := ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TPerNullCriteria): string;
begin
  Result := ACriteria.Attribute + ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TPerExistsCriteria): string;
begin
  Result := ACriteria.GetClause + '(' + ACriteria.Value + ')';
end;

function ToSelectClause(ACriteria: TPerBetweenCriteria): string;
begin
  Result := ACriteria.Attribute + ACriteria.GetClause + ACriteria.Value +
    ' AND ' + ACriteria.Value_2;
end;

function ToSelectClause(ACriteria: TPerInCriteria): string;
var
  i: Integer;
begin
  if Length(ACriteria.ValueArray) > 0 then
  begin
    { value as array elements }
    Result := ACriteria.Attribute + ACriteria.GetClause + '(';
    for i := Low(ACriteria.ValueArray) to (High(ACriteria.ValueArray) - 1) do
    begin
      Result := Result + ACriteria.ValueArray[i] + ', ';
    end;
    Result := Result + ACriteria.ValueArray[High(ACriteria.ValueArray)];
    Result := Result + ')';
  end
  else
  begin;
    { value as SQL statement }
    Result := ACriteria.Attribute + ACriteria.GetClause + '(';
    Result := Result + ACriteria.Value;
    Result := Result + ')';
  end;
end;

function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string;
begin
  Result := ACriteria.Attribute + ACriteria.GetClause + ACriteria.Value;
end;

function AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;
var
  i: integer;
  FAppendBegin: string;
  FAppendEnd: string;

  //-----------------
  procedure Include(AClause: string);
  begin
    Result := Result + FAppendBegin + AClause + FAppendEnd;
  end;

  //-----------------
  procedure Apply(FSelCriteria: TPerSelectionCriteriaAbs);
  begin
    if FSelCriteria is TPerBetweenCriteria then
      begin
        Include(ToSelectClause(TPerBetweenCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerNullCriteria then
      begin
        Include(ToSelectClause(TPerNullCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerInCriteria then
      begin
        Include(ToSelectClause(TPerInCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerExistsCriteria then
      begin
        Include(ToSelectClause(TPerExistsCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerSQLCriteria then
      begin
        Include(ToSelectClause(TPerSQLCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerFieldCriteriaAbs then
      begin
        Include(ToSelectClause(TPerFieldCriteriaAbs(FSelCriteria)));
        Exit;
      end;

    Include(ToSelectClause(FSelCriteria));
  end;

begin
  FAppendBegin := '';
  FAppendEnd := '';
  if ACriterias.Count = 0 then
    Exit; //==>

  Apply(ACriterias.Items[0]);

  if ACriterias.Count = 1 then
    Exit; //==>

  Result := '(' + Result + ')';

  FAppendBegin := ' AND (';
  FAppendEnd := ')';

  for i := 1 to ACriterias.Count - 2 do
    begin
      Apply(ACriterias.Items[i]);
    end;

  Apply(ACriterias.Items[ACriterias.Count - 1]);
end;


{ TVisPerObjToSQL }

constructor TVisPerObjToSQL.Create(pWithComments: boolean = False);
begin
  inherited Create;
  FWithComments             := pWithComments;
  FGroupByList              := TPerColumns.Create;
  FGroupByList.OwnsObjects  := False;
  FOrderByList              := TPerColumns.Create;
  FOrderByList.OwnsObjects  := False;
end;

destructor TVisPerObjToSQL.Destroy;
begin
  FOrderByList.Free;
  FGroupByList.Free;
  inherited Destroy;
end;

function TVisPerObjToSQL.AcceptVisitor: Boolean;
begin
  result := (Visited is TPerCriteria) and
    (not TtiObject(Visited).Deleted);
end;

procedure TVisPerObjToSQL.Execute(const pVisited: TtiVisited);
begin
  inherited Execute(pVisited);

  if not AcceptVisitor then
    Exit; //==>

  case TPerCriteria(pVisited).CriteriaType of
    crAND: Write(' AND ' + CrLf + '(');
    crOR: Write(' OR ' + CrLf + '(');
    crNONE: Write('(');
  end;

  Write(AsSQLClause(TPerCriteria(pVisited).SelectionCriterias));

  Write(')');

  if FWithComments then
  Write(' // ' + TPerCriteria(pVisited).Name + '// ');
  FGroupByList.CopyReferences(TPerCriteria(pVisited).GetGroupByList);
  FOrderByList.CopyReferences(TPerCriteria(pVisited).GetOrderByList);
end;

function TVisPerObjToSQL.OrderByClausesAsText: string;
var
  i: integer;
begin
  result := '';

  if FOrderByList.Count = 0 then
    Exit; //==>

  result := ' ORDER BY ' + FOrderByList.Items[0].Name;

  for i := 1 to (FOrderByList.Count - 1) do
    result := result + ', ' + FOrderByList.Items[i].Name;

end;

function TVisPerObjToSQL.GroupByClausesAsText: string;
var
  i: integer;
begin
 result := '';

  if FGroupByList.Count = 0 then
    Exit; //==>

  result := ' GROUP BY ' + FGroupByList.Items[0].Name;

  for i := 1 to (FGroupByList.Count - 1) do
    result := result + ', ' + FGroupByList.Items[i].Name;
end;

end.

