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
    11/05/2003 Created by Carlos Augusto - carlosa
  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}
unit tiCriteria;

interface

uses
  tiPtnVisPerObj;

type
  TCriteriaType = (crAND, crOR, crNONE);

  TPerSelectionCriteriaAbs = class;
  TPerCriteriaList = class;
  TPerSelectionCriteriaList = class;
  TPerColumns = class;
  TPerCriteria = class;

  TPerColumn = class (TPerObjAbs)
  private
    FAscending: Boolean;
    FName: string;
  protected
    function GetOwner: TPerColumns; reintroduce; virtual;
    procedure SetOwner(const Value: TPerColumns); reintroduce; virtual;
  public
    property Owner: TPerColumns read GetOwner write SetOwner;
  published
    property Ascending: Boolean read FAscending write FAscending;
    property Name: string read FName write FName;
  end;
  
  TPerColumns = class (TPerObjList)
  private
    function GetItems(Index: Integer): TPerColumn; reintroduce;
    procedure SetItems(Index: Integer; Value: TPerColumn); reintroduce;
  protected
    function GetOwner: TPerCriteria; reintroduce; virtual;
    procedure SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    procedure Add(pObject: TPerColumn; pDefDispOrdr: boolean = true); 
            reintroduce;
    procedure CopyReferences(pSource: TPerColumns);
    property Items[Index: Integer]: TPerColumn read GetItems write SetItems;
    property Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  
  TPerCriteria = class (TPerObjAbs)
  private
    FCriterias: TPerCriteriaList;
    FCriteriaType: TCriteriaType;
    FGroupByList: TPerColumns;
    FIsEmbraced: Boolean;
    FName: string;
    FOrderByList: TPerColumns;
    FSelectionCriterias: TPerSelectionCriteriaList;
    function GetCriterias: TPerCriteriaList;
    function GetSelectionCriterias: TPerSelectionCriteriaList;
  protected
    function GetOwner: TPerCriteria; reintroduce; virtual;
    procedure SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    constructor Create(pName: string); reintroduce; virtual;
    destructor Destroy; override;
    procedure AddAndCriteria(ACriteria: TPerCriteria);
    procedure AddBetween(AAtribute, AValue_1, AValue_2: string);
    procedure AddEqualTo(AAttribute, AValue: string);
    procedure AddGreaterOrEqualThan(AAtribute, AValue: string);
    procedure AddGreaterThan(AAtribute, AValue: string);
    procedure AddGroupBy(AField: string); overload;
    procedure AddGroupBy(AFields: array of string); overload;
    procedure AddIn(AAtribute, ASubQuery: string);
    procedure AddLessOrEqualThan(AAtribute, AValue: string);
    procedure AddLessThan(AAtribute, AValue: string);
    procedure AddNotEqualTo(AAtribute, AValue: string);
    procedure AddNotIn(AAtribute, ASubQuery: string);
    procedure AddNotNull(AAtribute: string);
    procedure AddOrCriteria(ACriteria: TPerCriteria);
    procedure AddOrderBy(AField: string; ASorterAscending: boolean = false); 
            overload;
    procedure AddOrderBy(AFields: array of string); overload;
    procedure AddOrderByAscending(AField: string); overload;
    procedure AddOrderByAscending(AFields: array of string); overload;
    procedure AddOrderByDescending(AField: string); overload;
    procedure AddOrderByDescending(AFields: array of string); overload;
    procedure ClearAll;
    function GetGroupByList: TPerColumns;
    function GetOrderByList: TPerColumns;
    function isEmbraced: Boolean;
    property CriteriaType: TCriteriaType read FCriteriaType write FCriteriaType;
    property Owner: TPerCriteria read GetOwner write SetOwner;
  published
    property Criterias: TPerCriteriaList read GetCriterias;
    property Name: string read FName;
    property SelectionCriterias: TPerSelectionCriteriaList read 
            GetSelectionCriterias;
  end;
  
  TPerCriteriaList = class (TPerObjList)
  private
    function GetItems(Index: Integer): TPerCriteria; reintroduce;
    procedure SetItems(Index: Integer; Value: TPerCriteria); reintroduce;
  protected
    function GetOwner: TPerCriteria; reintroduce; virtual;
    procedure SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    procedure Add(pObject: TPerCriteria; pDefDispOrdr: boolean = true); 
            reintroduce;
    property Items[Index: Integer]: TPerCriteria read GetItems write SetItems;
    property Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  
  TPerSelectionCriteriaAbs = class (TPerObjAbs)
  private
    FAlias: string;
    FAttribute: string;
    FisNegative: Boolean;
    FValue: string;
  protected
    function GetOwner: TPerCriteria; reintroduce; virtual;
    procedure SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    constructor Create(AAttribute, AValue: string; ANegative: boolean = false; 
            AAlias: string = ''); reintroduce; virtual;
    destructor Destroy; override;
    function GetClause: string; virtual; abstract;
    function isNegative: Boolean;
    property Owner: TPerCriteria read GetOwner write SetOwner;
  published
    property Attribute: string read FAttribute;
    property Value: string read FValue;
  end;
  
  TPerSelectionCriteriaList = class (TPerObjList)
  private
    function GetItems(Index: Integer): TPerSelectionCriteriaAbs; reintroduce;
    procedure SetItems(Index: Integer; Value: TPerSelectionCriteriaAbs); 
            reintroduce;
  protected
    function GetOwner: TPerCriteria; reintroduce; virtual;
    procedure SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    procedure Add(pObject: TPerSelectionCriteriaAbs; pDefDispOrdr: boolean = 
            true); reintroduce;
    function AsSQL: string;
    property Items[Index: Integer]: TPerSelectionCriteriaAbs read GetItems 
            write SetItems;
    property Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  
  TPerValueCriteriaAbs = class (TPerSelectionCriteriaAbs)
  end;
  
  TPerFieldCriteriaAbs = class (TPerSelectionCriteriaAbs)
  end;
  
  TPerEqualToCriteria = class (TPerValueCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerEqualToFieldCriteria = class (TPerFieldCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerExistsCriteria = class (TPerValueCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerGreatherThanCriteria = class (TPerValueCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerGreatherThanFieldCriteria = class (TPerFieldCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerInCriteria = class (TPerValueCriteriaAbs)
  public
    Value: array of string;
    function GetClause: string; override;
  end;
  
  TPerLessThanCriteria = class (TPerValueCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerLessThanFieldCriteria = class (TPerFieldCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerLikeCriteria = class (TPerValueCriteriaAbs)
  public
    function GetClause: string; override;
  end;
  
  TPerNullCriteria = class (TPerValueCriteriaAbs)
  public
    constructor Create(AAttribute: string; ANegative: boolean = false; AAlias: 
            string = ''); reintroduce; virtual;
    function GetClause: string; override;
  end;
  
  TPerBetweenCriteria = class (TPerValueCriteriaAbs)
  private
    FValue_2: string;
  public
    constructor Create(AAttribute, AArg_1, AArg_2: string; ANegative: boolean = 
            false; AAlias: string = ''); reintroduce; virtual;
    function GetClause: string; override;
  published
    property Value_2: string read FValue_2;
  end;
  
  TPerSQLCriteria = class (TPerSelectionCriteriaAbs)
  public
    constructor Create(ASQLStm: string); reintroduce; virtual;
    function GetClause: string; override;
  end;
  
implementation

uses tiPtnVis;


{
********************************** TPerColumn **********************************
}
function TPerColumn.GetOwner: TPerColumns;
begin
  Result := TPerColumns(inherited Owner);
end;

procedure TPerColumn.SetOwner(const Value: TPerColumns);
begin
  inherited Owner := Value;
end;


{
********************************* TPerColumns **********************************
}
procedure TPerColumns.Add(pObject: TPerColumn; pDefDispOrdr: boolean = true);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

procedure TPerColumns.CopyReferences(pSource: TPerColumns);
var
  i: Integer;
begin
  Assert(pSource <> nil, 'Source not assigned - CopyReferences');
  for i := 0 to pSource.Count - 1 do
    Add(pSource.Items[i]);
end;

function TPerColumns.GetItems(Index: Integer): TPerColumn;
begin
  Result := TPerColumn(inherited Items[Index]);
end;

function TPerColumns.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerColumns.SetItems(Index: Integer; Value: TPerColumn);
begin
  inherited Items[Index] := Value;
end;

procedure TPerColumns.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{
********************************* TPerCriteria *********************************
}
constructor TPerCriteria.Create(pName: string);
begin
  inherited Create;
  FName := pName;
  FCriteriaType := crNONE;
  FCriterias := TPerCriteriaList.Create;
  FCriterias.Owner := Self;
  FCriterias.OwnsObjects := true;
  FCriterias.AutoSetItemOwner := false;
  FSelectionCriterias := TPerSelectionCriteriaList.Create;
  FSelectionCriterias.Owner := Self;
  FSelectionCriterias.OwnsObjects := true;
  FGroupByList := TPerColumns.Create;
  FGroupByList.Owner := Self;
  FGroupByList.OwnsObjects := true;
  FOrderByList := TPerColumns.Create;
  FOrderByList.Owner := Self;
  FOrderByList.OwnsObjects := true;
  FisEmbraced := false;
end;

destructor TPerCriteria.Destroy;
begin
  FCriterias.Free;
  FSelectionCriterias.Free;
  FOrderByList.Free;
  FGroupByList.Free;
  inherited Destroy;
end;

procedure TPerCriteria.AddAndCriteria(ACriteria: TPerCriteria);
begin
  FisEmbraced := true;
  ACriteria.CriteriaType := crAND;
  FCriterias.Add(ACriteria);
end;

procedure TPerCriteria.AddBetween(AAtribute, AValue_1, AValue_2: string);
var
  lCriteria: TPerBetweenCriteria;
begin
  lCriteria := TPerBetweenCriteria.Create(AAtribute, AValue_1, AValue_2);
  FSelectionCriterias.Add(lCriteria);
end;

procedure TPerCriteria.AddEqualTo(AAttribute, AValue: string);
var
  lData: TPerEqualToCriteria;
begin
  lData := TPerEqualToCriteria.Create(AAttribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterOrEqualThan(AAtribute, AValue: string);
var
  lData: TPerLessThanCriteria;
begin
  lData := TPerLessThanCriteria.Create(AAtribute, AValue, true);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterThan(AAtribute, AValue: string);
var
  lData: TPerGreatherThanCriteria;
begin
  lData := TPerGreatherThanCriteria.Create(AAtribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGroupBy(AField: string);
var
  lData: TPerColumn;
begin
  { verificar repetidos }
  Assert(AField <> '', 'AField is blank!');
  lData := TPerColumn.Create;
  lData.Name := AField;
  FGroupByList.Add(lData);
end;

procedure TPerCriteria.AddGroupBy(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    begin
      AddGroupBy(AFields[i]);
    end;
end;

procedure TPerCriteria.AddIn(AAtribute, ASubQuery: string);
begin
end;

procedure TPerCriteria.AddLessOrEqualThan(AAtribute, AValue: string);
begin
end;

procedure TPerCriteria.AddLessThan(AAtribute, AValue: string);
begin
end;

procedure TPerCriteria.AddNotEqualTo(AAtribute, AValue: string);
begin
end;

procedure TPerCriteria.AddNotIn(AAtribute, ASubQuery: string);
begin
end;

procedure TPerCriteria.AddNotNull(AAtribute: string);
begin
end;

procedure TPerCriteria.AddOrCriteria(ACriteria: TPerCriteria);
begin
  FisEmbraced := true;
  ACriteria.CriteriaType := crOR;
  FCriterias.Add(ACriteria);
end;

procedure TPerCriteria.AddOrderBy(AField: string; ASorterAscending: boolean = 
        false);
var
  lData: TPerColumn;
begin
  Assert(AField <> '', 'AField is blank!');
  lData := TPerColumn.Create;
  lData.Ascending := ASorterAscending;
  lData.Name := AField;
  FOrderByList.Add(lData);
end;

procedure TPerCriteria.AddOrderBy(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    begin
      AddOrderBy(AFields[i], false);
    end;
end;

procedure TPerCriteria.AddOrderByAscending(AField: string);
begin
  Assert(AField <> '', 'AField is blank!');
  AddOrderBy(AField, true);
end;

procedure TPerCriteria.AddOrderByAscending(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    begin
      AddOrderByAscending(AFields[i]);
    end;
end;

procedure TPerCriteria.AddOrderByDescending(AField: string);
begin
  Assert(AField <> '', 'AField is blank!');
  AddOrderBy(AField, false);
end;

procedure TPerCriteria.AddOrderByDescending(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    begin
      AddOrderByDescending(AFields[i]);
    end;
end;

procedure TPerCriteria.ClearAll;
begin
  FCriterias.Clear;
  FOrderByList.Clear;
  FGroupByList.Clear;
end;

function TPerCriteria.GetCriterias: TPerCriteriaList;
begin
  Result := FCriterias;
end;

function TPerCriteria.GetGroupByList: TPerColumns;
begin
  Result := FGroupByList;
end;

function TPerCriteria.GetOrderByList: TPerColumns;
begin
  Result := FOrderByList;
end;

function TPerCriteria.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

function TPerCriteria.GetSelectionCriterias: TPerSelectionCriteriaList;
begin
  Result := FSelectionCriterias;
end;

function TPerCriteria.isEmbraced: Boolean;
begin
  Result := FisEmbraced;
end;

procedure TPerCriteria.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;



{
******************************* TPerCriteriaList *******************************
}
procedure TPerCriteriaList.Add(pObject: TPerCriteria; pDefDispOrdr: boolean = 
        true);
begin
  inherited Add(pObject, pDefDispOrdr);
  if Count > 0 then
    Owner.FIsEmbraced := true;
end;

function TPerCriteriaList.GetItems(Index: Integer): TPerCriteria;
begin
  Result := TPerCriteria(inherited Items[Index]);
end;

function TPerCriteriaList.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerCriteriaList.SetItems(Index: Integer; Value: TPerCriteria);
begin
  inherited Items[Index] := Value;
end;

procedure TPerCriteriaList.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{
*************************** TPerSelectionCriteriaAbs ***************************
}
constructor TPerSelectionCriteriaAbs.Create(AAttribute, AValue: string; 
        ANegative: boolean = false; AAlias: string = '');
begin
  inherited Create;
  FAttribute := AAttribute;
  FValue := AValue;
  FisNegative := ANegative;
  FAlias := AAlias;
end;

destructor TPerSelectionCriteriaAbs.Destroy;
begin
  inherited Destroy;
end;

function TPerSelectionCriteriaAbs.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

function TPerSelectionCriteriaAbs.isNegative: Boolean;
begin
  Result := FisNegative;
end;

procedure TPerSelectionCriteriaAbs.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{
************************** TPerSelectionCriteriaList ***************************
}
procedure TPerSelectionCriteriaList.Add(pObject: TPerSelectionCriteriaAbs; 
        pDefDispOrdr: boolean = true);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TPerSelectionCriteriaList.AsSQL: string;
var
  i: Integer;
begin
  Result := ' (';
  for i := 0 to Count - 1 do
    begin
      Result := Result + TPerSelectionCriteriaAbs(Items[i]).GetClause +
        ' AND ';
    end;
  Result := Result + ') ';
end;

function TPerSelectionCriteriaList.GetItems(Index: Integer): 
        TPerSelectionCriteriaAbs;
begin
  Result := TPerSelectionCriteriaAbs(inherited Items[Index]);
end;

function TPerSelectionCriteriaList.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerSelectionCriteriaList.SetItems(Index: Integer; Value: 
        TPerSelectionCriteriaAbs);
begin
  inherited Items[Index] := Value;
end;

procedure TPerSelectionCriteriaList.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{
***************************** TPerEqualToCriteria ******************************
}
function TPerEqualToCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{
*************************** TPerEqualToFieldCriteria ***************************
}
function TPerEqualToFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{
****************************** TPerExistsCriteria ******************************
}
function TPerExistsCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT EXISTS'
  else
    Result := ' EXISTS';
end;


{
*************************** TPerGreatherThanCriteria ***************************
}
function TPerGreatherThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{
************************ TPerGreatherThanFieldCriteria *************************
}
function TPerGreatherThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{
******************************** TPerInCriteria ********************************
}
function TPerInCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT IN '
  else
    Result := ' IN ';
end;


{
***************************** TPerLessThanCriteria *****************************
}
function TPerLessThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{
************************** TPerLessThanFieldCriteria ***************************
}
function TPerLessThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{
******************************* TPerLikeCriteria *******************************
}
function TPerLikeCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT LIKE '
  else
    Result := ' LIKE ';
end;


{
******************************* TPerNullCriteria *******************************
}
constructor TPerNullCriteria.Create(AAttribute: string; ANegative: boolean = 
        false; AAlias: string = '');
begin
  inherited Create(AAttribute, '', ANegative, AAlias);
end;

function TPerNullCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' IS NOT NULL '
  else
    Result := ' IS NULL ';
end;


{
***************************** TPerBetweenCriteria ******************************
}
constructor TPerBetweenCriteria.Create(AAttribute, AArg_1, AArg_2: string; 
        ANegative: boolean = false; AAlias: string = '');
begin
  inherited Create(AAttribute, AArg_1, ANegative, AAlias);
  FValue_2 := AArg_2;
end;

function TPerBetweenCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT BETWEEN '
  else
    Result := ' BETWEEN ';
end;


{
******************************* TPerSQLCriteria ********************************
}
constructor TPerSQLCriteria.Create(ASQLStm: string);
begin
  inherited Create(ASQLStm, '', false, '');
end;

function TPerSQLCriteria.GetClause: string;
begin
  Result := inherited Attribute;
end;

end.

