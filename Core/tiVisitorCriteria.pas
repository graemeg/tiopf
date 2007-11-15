unit tiVisitorCriteria;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiVisitor
  ,tiObject
  ,tiQuery
  ,tiCriteria
  ,tiUtils        { GetPropNames }
  ,TypInfo
  , SysUtils
  ;

type
  TVisPerObjToSQL = class(TVisStringStream)
  private
    FGroupByList: TPerColumns;
    FOrderByList: TPerColumns;
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
    function    OrderByClausesAsText: string;
    function    AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;
    property    Params: TtiQueryParams read FParams write FParams;
  end;

// Helper functions
function tiCriteriaAsSQL(pVisited: TtiObject; pWithComments: boolean = false): string; overload;
function tiCriteriaAsSQL(pVisited: TtiObject; AParams: TtiQueryParams; pWithComments: boolean = false): string; overload;

function ToSelectClause(ACriteria: TPerSelectionCriteriaAbs; AParams: TtiQueryParams; var AParamNo: integer ): string; overload;
function ToSelectClause(ACriteria: TPerSQLCriteria): string; overload;
function ToSelectClause(ACriteria: TPerNullCriteria): string; overload;
function ToSelectClause(ACriteria: TPerExistsCriteria): string; overload;
function ToSelectClause(ACriteria: TPerBetweenCriteria; AParams: TtiQueryParams; var AParamNo: integer): string; overload;
function ToSelectClause(ACriteria: TPerInCriteria; AParams: TtiQueryParams; var AParamNo: integer): string; overload;
function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string; overload;




implementation

uses
  variants
  , tiRTTI
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
  lVisitor: TVisPerObjToSQL;
begin
  Assert(pVisited is TPerCriteria, 'TPerCriteria required');

  result:= '';
  if not TPerCriteria(pVisited).HasCriteria then
    exit;

  lVisitor := TVisPerObjToSQL.Create(pWithComments);
  try
    lVisitor.Params:= AParams;

    pVisited.Iterate(lVisitor);
    result := lVisitor.Text;

    if TPerCriteria(pVisited).isEmbraced and (result <> '') then
      result := '(' + result + ')' + CrLf
    else
      result := result + CrLf;

// the following are temerarily removed until they can be properly tested
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

function ToSelectClause(ACriteria: TPerSelectionCriteriaAbs; AParams: TtiQueryParams; var AParamNo: integer): string;
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

function ToSelectClause(ACriteria: TPerSQLCriteria): string;
begin
  Result := ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TPerNullCriteria): string;
begin
  Result := ACriteria.FieldName + ACriteria.GetClause;
end;

function ToSelectClause(ACriteria: TPerExistsCriteria): string;
begin
  Result := ACriteria.GetClause + '(' + ACriteria.Value + ')';
end;

function ToSelectClause(ACriteria: TPerBetweenCriteria; AParams: TtiQueryParams; var AParamNo: integer): string;
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

function ToSelectClause(ACriteria: TPerInCriteria; AParams: TtiQueryParams; var AParamNo: integer): string;
var
  i: Integer;
  sep: string;
begin
//  if assigned(AParams) then
//  begin
//    Result := ACriteria.FieldName + ACriteria.GetClause + GetParamName(AParamNo, true);
//    AddParam(AParams, AParamNo,ACriteria.Value);
//    inc(AParamNo);
//  end
//  else
//    Result := ACriteria.FieldName + ACriteria.GetClause + GetSqlValue(ACriteria.Value);
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
//    Result := Result + GetSqlValue(ACriteria.ValueArray[High(ACriteria.ValueArray)]);
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

function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string;
begin
  Result := ACriteria.FieldName + ACriteria.GetClause + GetSqlValue(ACriteria.Value);
end;

//function AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;
//var
//  i: integer;
//  FAppendBegin: string;
//  FAppendEnd: string;
//
//  //-----------------
//  procedure Include(AClause: string);
//  begin
//    Result := Result + FAppendBegin + AClause + FAppendEnd;
//  end;
//
//  //-----------------
//  procedure Apply(FSelCriteria: TPerSelectionCriteriaAbs);
//  begin
//    if FSelCriteria is TPerBetweenCriteria then
//      begin
//        Include(ToSelectClause(TPerBetweenCriteria(FSelCriteria)));
//        Exit;
//      end;
//
//    if FSelCriteria is TPerNullCriteria then
//      begin
//        Include(ToSelectClause(TPerNullCriteria(FSelCriteria)));
//        Exit;
//      end;
//
//    if FSelCriteria is TPerInCriteria then
//      begin
//        Include(ToSelectClause(TPerInCriteria(FSelCriteria)));
//        Exit;
//      end;
//
//    if FSelCriteria is TPerExistsCriteria then
//      begin
//        Include(ToSelectClause(TPerExistsCriteria(FSelCriteria)));
//        Exit;
//      end;
//
//    if FSelCriteria is TPerSQLCriteria then
//      begin
//        Include(ToSelectClause(TPerSQLCriteria(FSelCriteria)));
//        Exit;
//      end;
//
//    if FSelCriteria is TPerFieldCriteriaAbs then
//      begin
//        Include(ToSelectClause(TPerFieldCriteriaAbs(FSelCriteria)));
//        Exit;
//      end;
//
//    Include(ToSelectClause(FSelCriteria, ));
//  end;
//
//begin
//  FAppendBegin := '';
//  FAppendEnd := '';
//  if ACriterias.Count = 0 then
//    Exit; //==>
//
//  Apply(ACriterias.Items[0]);
//
//  if ACriterias.Count = 1 then
//    Exit; //==>
//
//  Result := '(' + Result + ')';
//
//  FAppendBegin := ' AND (';
//  FAppendEnd := ')';
//
//  for i := 1 to ACriterias.Count - 1 do
//  begin
//    Apply(ACriterias.Items[i]);
//  end;
//
////  Apply(ACriterias.Items[ACriterias.Count - 1]);
//end;


{ TVisPerObjToSQL }

function TVisPerObjToSQL.AsSQLClause(
  ACriterias: TPerSelectionCriteriaList): string;
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
        Include(ToSelectClause(TPerBetweenCriteria(FSelCriteria), FParams, FCurrentParamNo));
        Exit;
      end;

    if FSelCriteria is TPerNullCriteria then
      begin
        Include(ToSelectClause(TPerNullCriteria(FSelCriteria)));
        Exit;
      end;

    if FSelCriteria is TPerInCriteria then
      begin
        Include(ToSelectClause(TPerInCriteria(FSelCriteria), FParams, FCurrentParamNo));
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

constructor TVisPerObjToSQL.Create(pWithComments: boolean = False);
begin
  inherited Create;
  FWithComments             := pWithComments;
  FGroupByList              := TPerColumns.Create;
  FGroupByList.OwnsObjects  := False;
  FOrderByList              := TPerColumns.Create;
  FOrderByList.OwnsObjects  := False;
  FParams                   := nil;
  FCurrentParamNo           := 1;
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

  if not assigned(TPerCriteria(pVisited).SelectionCriterias) or (TPerCriteria(pVisited).SelectionCriterias.Count = 0) then
    exit;

  if Stream.Size = 0 then
  begin
    Write('(');
  end
  else
  begin
    case TPerCriteria(pVisited).CriteriaType of
      crAND: Write(' AND ' + CrLf + '(');
      crOR: Write(' OR ' + CrLf + '(');
      crNONE: Write('(');
    end;
  end;

  Write(AsSQLClause(TPerCriteria(pVisited).SelectionCriterias));

  Write(')');

  if FWithComments then
  Write(' /* ' + TPerCriteria(pVisited).Name + '*/ ');
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



