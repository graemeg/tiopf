{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    11/11/2003 Created by Carlos Augusto - carlosa
  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}
unit tiPtnVisCriteria;

interface

uses
  Classes
  , tiObjAbs
  , tiPtnVis
  , tiPtnVisPerObj
  , tiCriteria
  , tiUtils // GetPropNames
  , TypInfo
  ;

type
  TVisPerObjToSQL = class(TVisStringStream)
  private
    FGroupByList: TPerColumns;
    FOrderByList: TPerColumns;
    FWithComments: Boolean;
  protected
    function AcceptVisitor: Boolean; override;
  public
    constructor Create(pWithComments: boolean = false); reintroduce; virtual;
    destructor Destroy; override;
    procedure Execute(const pVisited: TVisitedAbs); override;
    function GroupByClausesAsText: string;
    function OrderByClausesAsText: string;
  end;

function tiPerCriteriaAsSQL(pVisited: TPerObjAbs; pWithComments: boolean = 
    false): string;

function ToSelectClause(ACriteria: TPerSelectionCriteriaAbs): string; overload;

function ToSelectClause(ACriteria: TPerSQLCriteria): string; overload;

function ToSelectClause(ACriteria: TPerNullCriteria): string; overload;

function ToSelectClause(ACriteria: TPerExistsCriteria): string; overload;

function ToSelectClause(ACriteria: TPerBetweenCriteria): string; overload;

function ToSelectClause(ACriteria: TPerInCriteria): string; overload;

function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string; overload;

function AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;

implementation


function tiPerCriteriaAsSQL(pVisited: TPerObjAbs; pWithComments: boolean = 
    false): string;
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
  Result := ACriteria.Attribute + ACriteria.GetClause + '(';
  for i := Low(ACriteria.Value) to (High(ACriteria.Value) - 1) do
    begin
      Result := Result + ACriteria.Value[i] + ', ';
    end;
  Result := Result + ACriteria.Value[High(ACriteria.Value)];
  Result := Result + ')';
end;

function ToSelectClause(ACriteria: TPerFieldCriteriaAbs): string;
begin
  Result := ACriteria.Attribute + ACriteria.GetClause +
    ACriteria.Value;
end;

function AsSQLClause(ACriterias: TPerSelectionCriteriaList): string;
var
  i: integer;
  FAppendBegin: string;
  FAppendEnd: string;

  procedure Include(AClause: string);
  begin
    Result := Result + FAppendBegin + AClause + FAppendEnd;
  end;

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

{
******************************* TVisPerObjToSQL ********************************
}

constructor TVisPerObjToSQL.Create(pWithComments: boolean = false);
begin
  inherited Create;
  FWithComments := pWithComments;
  FGroupByList := TPerColumns.Create;
  FGroupByList.OwnsObjects := false;
  FOrderByList := TPerColumns.Create;
  FOrderByList.OwnsObjects := false;
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
    (not TPerObjAbs(Visited).Deleted);
end;

procedure TVisPerObjToSQL.Execute(const pVisited: TVisitedAbs);
begin

  inherited Execute(pVisited);

  if not AcceptVisitor then
    exit; //==>

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

