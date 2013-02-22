unit tiVisitorCriteria;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiVisitor
  ,tiObject
  ,tiQuery
  ,tiCriteria
  ,TypInfo
  ,SysUtils
  ;

type
  TVisObjectToSQL = class(TVisStringStream)
  private
    FGroupByList: TtiColumns;
    FWithComments: Boolean;
    FParams: TtiQueryParams;
    FCurrentParamNo: integer;
  protected
    function    AcceptVisitor: Boolean; override;
  public
    constructor Create(pWithComments: boolean = False); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   Execute(const pVisited: TtiVisited); override;
    function    GroupByClausesAsText: string;
    function    AsSQLClause(ACriterias: TtiSelectionCriteriaList): string;
    property    Params: TtiQueryParams read FParams write FParams;
  end;

// Helper functions
function tiCriteriaAsSQL(pVisited: TtiObject; pWithComments: boolean = false): string; overload;
function tiCriteriaAsSQL(pVisited: TtiObject; AParams: TtiQueryParams; pWithComments: boolean = false): string; overload;

function tiCriteriaOrderByAsSQL(pVisited: TtiObject): string;

function ToSelectClause(ACriteria: TtiSelectionCriteriaAbs; AParams: TtiQueryParams; var AParamNo: integer ): string; overload;
function ToSelectClause(ACriteria: TtiSQLCriteria): string; overload;
function ToSelectClause(ACriteria: TtiNullCriteria): string; overload;
function ToSelectClause(ACriteria: TtiExistsCriteria): string; overload;
function ToSelectClause(ACriteria: TtiBetweenCriteria; AParams: TtiQueryParams; var AParamNo: integer): string; overload;
function ToSelectClause(ACriteria: TtiInCriteria; AParams: TtiQueryParams; var AParamNo: integer): string; overload;
function ToSelectClause(ACriteria: TtiFieldCriteriaAbs): string; overload;



implementation

uses
  variants
  ,tiUtils        { GetPropNames }
  ,tiRTTI
  ,tiLog
  ;

const
  PARAM_PREFIX = 'Criteria_';

function GetParamName(const AParamNo: integer; AddColon: boolean): string;
begin
  if AddColon then
    result:= ':'
  else
    result:= '';
  result:= result + PARAM_PREFIX + IntToStr(AParamNo);
end;

function GetSqlValue(AValue: variant): string;
begin
  if tiVarSimplePropType(AValue) = tiTKString then
     result:= QuotedStr(AValue)
  else
    result:= VarToStr(AValue);
end;

procedure AddParam(AParams: TtiQueryParams; AParamNo: integer; AValue: variant);
begin
  if assigned(AParams) then
    AParams.SetValueAsVariant(GetParamName(AParamNo, false), AValue);
end;

function tiCriteriaAsSQL(pVisited: TtiObject; AParams: TtiQueryParams; pWithComments: boolean = false): string;
var
  lVisitor: TVisObjectToSQL;
begin
  Assert(pVisited is TtiCriteria, 'TtiCriteria required');

  result:= '';
  if not TtiCriteria(pVisited).HasCriteria then
    exit;

  lVisitor := TVisObjectToSQL.Create(pWithComments);
  try
    lVisitor.Params:= AParams;

    pVisited.Iterate(lVisitor);
    result := lVisitor.Text;

    if TtiCriteria(pVisited).isEmbraced and (result <> '') then
      result := '(' + result + ')' + tiLineEnd
    else
      result := result + tiLineEnd;

// TODO: the following are temporarily removed until they can be properly tested
//    result := result + lVisitor.GroupByClausesAsText + CrLf;

//    result := result + lVisitor.OrderByClausesAsText + CrLf;

  finally
    lVisitor.Free;
  end;
end;

function tiCriteriaAsSQL(pVisited: TtiObject; pWithComments: boolean = false): string;
begin
  result:= tiCriteriaAsSQL(pVisited, nil, pWithComments);
end;

function tiCriteriaOrderByAsSQL(pVisited: TtiObject): string;
var
  lOrderByList: TtiColumns;
   i: integer;
  function OrderText(AColumnId: integer): string;
  begin
    if lOrderByList.Items[AColumnId].Ascending then
      result:= lOrderByList.Items[AColumnId].FieldName
    else
      result:= lOrderByList.Items[AColumnId].FieldName + ' DESC';
  end;
begin
  Assert(pVisited is TtiCriteria, 'TtiCriteria required');

  result:= '';
  if not TtiCriteria(pVisited).HasOrderBy then
    exit;

  lOrderByList:= TtiCriteria(pVisited).GetOrderByList;

  result := ' ORDER BY ' + OrderText(0);

  for i := 1 to (lOrderByList.Count - 1) do
    result := result + ', ' + OrderText(i);

  result:= result + CrLf;
end;


function ToSelectClause(ACriteria: TtiSelectionCriteriaAbs; AParams: TtiQueryParams; var AParamNo: integer): string;
begin
  if assigned(AParams) then
  begin
    Result := ACriteria.FieldName + ACriteria.GetClause + GetParamName(AParamNo, true);
    AddParam(AParams, AParamNo,ACriteria.Value);
    inc(AParamNo);
  end
  else
    Result := ACriteria.FieldName + ACriteria.GetClause + GetSqlValue(ACriteria.Value);
end;

function ToSelectClause(ACriteria: TtiSQLCriteria): string;
begin
  Result := ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TtiNullCriteria): string;
begin
  Result := ACriteria.FieldName + ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TtiExistsCriteria): string;
begin
  Result := ACriteria.GetClause + '(' + ACriteria.Value + ')';
end;

function ToSelectClause(ACriteria: TtiBetweenCriteria; AParams: TtiQueryParams; var AParamNo: integer): string;
begin
  if assigned(AParams) then
  begin
  Result := ACriteria.FieldName + ACriteria.GetClause + GetParamName(AParamNo, true) +
    ' AND ' + GetParamName(AParamNo + 1, true);
    AddParam(AParams, AParamNo, ACriteria.Value);
    AddParam(AParams, AParamNo + 1, ACriteria.Value_2);

    inc(AParamNo, 2);
  end
  else
    Result := ACriteria.FieldName + ACriteria.GetClause + GetSqlValue(ACriteria.Value) +
    ' AND ' + GetSqlValue(ACriteria.Value_2);
end;

function ToSelectClause(ACriteria: TtiInCriteria; AParams: TtiQueryParams; var AParamNo: integer): string;
var
  i: Integer;
  sep: string;
begin
  sep:= '';
  if Length(ACriteria.ValueArray) > 0 then
  begin
    { value as array elements }
    Result := ACriteria.FieldName + ACriteria.GetClause + '(';
    for i := Low(ACriteria.ValueArray) to (High(ACriteria.ValueArray)) do
    begin
      if assigned(AParams) then
      begin
        Result := Result + sep + GetParamName(AParamNo, true);
        AddParam(AParams, AParamNo, ACriteria.ValueArray[i]);
        inc(AParamNo);
      end
      else
      Result := Result + sep + GetSqlValue(ACriteria.ValueArray[i]);
      sep:= ', ';
    end;
    Result := Result + ')';
  end
  else
  begin;
    { value as SQL statement }
    Result := ACriteria.FieldName + ACriteria.GetClause + '(';
    Result := Result + ACriteria.Value;
    Result := Result + ')';
  end;
end;

function ToSelectClause(ACriteria: TtiFieldCriteriaAbs): string;
begin
  Result := ACriteria.FieldName + ACriteria.GetClause + GetSqlValue(ACriteria.Value);
end;


{ TVisObjectToSQL }

function TVisObjectToSQL.AsSQLClause(
  ACriterias: TtiSelectionCriteriaList): string;
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
  procedure Apply(FSelCriteria: TtiSelectionCriteriaAbs);
  begin
    if FSelCriteria is TtiBetweenCriteria then
      begin
        Include(ToSelectClause(TtiBetweenCriteria(FSelCriteria), FParams, FCurrentParamNo));
        Exit;
      end;

    if FSelCriteria is TtiNullCriteria then
      begin
        Include(ToSelectClause(TtiNullCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TtiInCriteria then
      begin
        Include(ToSelectClause(TtiInCriteria(FSelCriteria), FParams, FCurrentParamNo));
        Exit;
      end;

    if FSelCriteria is TtiExistsCriteria then
      begin
        Include(ToSelectClause(TtiExistsCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TtiSQLCriteria then
      begin
        Include(ToSelectClause(TtiSQLCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TtiFieldCriteriaAbs then
      begin
        Include(ToSelectClause(TtiFieldCriteriaAbs(FSelCriteria)));
        Exit;
      end;

    Include(ToSelectClause(FSelCriteria, FParams, FCurrentParamNo));
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

  for i := 1 to ACriterias.Count - 1 do
  begin
    Apply(ACriterias.Items[i]);
  end;

//  Apply(ACriterias.Items[ACriterias.Count - 1]);
end;

constructor TVisObjectToSQL.Create(pWithComments: boolean = False);
begin
  inherited Create;
  FWithComments             := pWithComments;
  FGroupByList              := TtiColumns.Create;
  FGroupByList.OwnsObjects  := False;
  FParams                   := nil;
  FCurrentParamNo           := 1;
end;

destructor TVisObjectToSQL.Destroy;
begin
  FGroupByList.Free;
  inherited Destroy;
end;

function TVisObjectToSQL.AcceptVisitor: Boolean;
begin
  result := (Visited is TtiCriteria) and
    (not TtiObject(Visited).Deleted);
  Log([ClassName, Visited.ClassName, Result], lsAcceptVisitor);
end;

procedure TVisObjectToSQL.Execute(const pVisited: TtiVisited);
begin
  inherited Execute(pVisited);

  if not AcceptVisitor then
    Exit; //==>

  if not Assigned(TtiCriteria(pVisited).SelectionCriterias) or (TtiCriteria(pVisited).SelectionCriterias.Count = 0) then
    exit;

  if Stream.Size = 0 then
  begin
    Write('(');
  end
  else
  begin
    case TtiCriteria(pVisited).CriteriaType of
      crAND: Write(' AND ' + tiLineEnd + '(');
      crOR: Write(' OR ' + tiLineEnd + '(');
      crNONE: Write('(');
    end;
  end;

  Write(AsSQLClause(TtiCriteria(pVisited).SelectionCriterias));

  Write(')');

  if FWithComments then
    Write(' /* ' + TtiCriteria(pVisited).Name + ' */ ');
  FGroupByList.CopyReferences(TtiCriteria(pVisited).GetGroupByList);
end;

function TVisObjectToSQL.GroupByClausesAsText: string;
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






