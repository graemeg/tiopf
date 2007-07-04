unit tiCriteria;

{$I tiDefines.inc}

interface

uses
  tiObject
  ;

type
  TCriteriaType = (crAND, crOR, crNONE);

  // forward declarations
  TPerSelectionCriteriaAbs    = class;
  TPerCriteriaList            = class;
  TPerSelectionCriteriaList   = class;
  TPerColumns                 = class;
  TPerCriteria                = class;


  TPerColumn = class(TtiObject)
  private
    FAscending: Boolean;
    FName: string;
  protected
    function    GetOwner: TPerColumns; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerColumns); reintroduce; virtual;
  public
    property    Owner: TPerColumns read GetOwner write SetOwner;
  published
    property    Ascending: Boolean read FAscending write FAscending;
    property    Name: string read FName write FName;
  end;
  
  
  TPerColumns = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TPerColumn; reintroduce;
    procedure   SetItems(i: Integer; Value: TPerColumn); reintroduce;
  protected
    function    GetOwner: TPerCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TPerColumn): integer; reintroduce;
    procedure   CopyReferences(pSource: TPerColumns);
    property    Items[Index: Integer]: TPerColumn read GetItems write SetItems;
    property    Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  
  
  TPerCriteria = class(TtiObject)
  private
    FCriterias: TPerCriteriaList;
    FCriteriaType: TCriteriaType;
    FGroupByList: TPerColumns;
    FIsEmbraced: Boolean;
    FName: string;
    FOrderByList: TPerColumns;
    FSelectionCriterias: TPerSelectionCriteriaList;
    function    GetCriterias: TPerCriteriaList;
    function    GetSelectionCriterias: TPerSelectionCriteriaList;
  protected
    function    GetOwner: TPerCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    constructor Create(pName: string); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   AddAndCriteria(ACriteria: TPerCriteria);
    procedure   AddBetween(AAttribute, AValue_1, AValue_2: string);
    procedure   AddEqualTo(AAttribute, AValue: string); overload;
    procedure   AddEqualTo(AAttribute: string; AValue: integer); overload;
    procedure   AddExists(ASubQuery: string);
    procedure   AddGreaterOrEqualThan(AAttribute, AValue: string); overload;
    procedure   AddGreaterOrEqualThan(AAttribute: string; AValue: integer); overload;
    procedure   AddGreaterThan(AAttribute, AValue: string); overload;
    procedure   AddGreaterThan(AAttribute: string; AValue: integer); overload;
    procedure   AddGroupBy(AField: string); overload;
    procedure   AddGroupBy(AFields: array of string); overload;
    procedure   AddIn(AAttribute, ASubQuery: string); overload;
    procedure   AddIn(AAttribute: string; AValueArray: array of string); overload;
    procedure   AddIn(AAttribute: string; AValueArray: array of integer); overload;
    procedure   AddLessOrEqualThan(AAttribute, AValue: string); overload;
    procedure   AddLessOrEqualThan(AAttribute: string; AValue: integer); overload;
    procedure   AddLessThan(AAttribute, AValue: string); overload;
    procedure   AddLessThan(AAttribute: string; AValue: integer); overload;
    procedure   AddLike(AAttribute, AValue: string);
    procedure   AddNotEqualTo(AAttribute, AValue: string); overload;
    procedure   AddNotEqualTo(AAttribute: string; AValue: integer); overload;
    procedure   AddNotExists(ASubQuery: string);
    procedure   AddNotIn(AAttribute, ASubQuery: string);
    procedure   AddNotLike(AAttribute, AValue: string);
    procedure   AddNotNull(AAttribute: string);
    procedure   AddNull(AAttribute: string);
    procedure   AddOrCriteria(ACriteria: TPerCriteria);
    procedure   AddOrderBy(AField: string; ASorterAscending: boolean = False); overload;
    procedure   AddOrderBy(AFields: array of string); overload;
    procedure   AddOrderByAscending(AField: string); overload;
    procedure   AddOrderByAscending(AFields: array of string); overload;
    procedure   AddOrderByDescending(AField: string); overload;
    procedure   AddOrderByDescending(AFields: array of string); overload;
    procedure   AddSQL(ASQLStm: string);
    procedure   ClearAll;
    function    GetGroupByList: TPerColumns;
    function    GetOrderByList: TPerColumns;
    function    isEmbraced: Boolean;
    function    HasCriteria: boolean;
    property    CriteriaType: TCriteriaType read FCriteriaType write FCriteriaType;
    property    Owner: TPerCriteria read GetOwner write SetOwner;

  published
    property    Criterias: TPerCriteriaList read GetCriterias;
    property    Name: string read FName;
    property    SelectionCriterias: TPerSelectionCriteriaList read GetSelectionCriterias;
  end;
  
  
  TPerCriteriaList = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TPerCriteria; reintroduce;
    procedure   SetItems(i: Integer; Value: TPerCriteria); reintroduce;
  protected
    function    GetOwner: TPerCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TPerCriteria): integer; reintroduce;
    property    Items[Index: Integer]: TPerCriteria read GetItems write SetItems;
    property    Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  

  TPerSelectionCriteriaAbs = class(TtiObject)
  private
    FFieldName: string;
    FAttribute: string;
    FisNegative: Boolean;
    FValue: string;
    function GetFieldName: string;
  protected
    function    GetOwner: TPerCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    constructor Create(AAttribute, AValue: string; ANegative: boolean = False; AFieldName: string = ''); reintroduce; virtual;
    destructor  Destroy; override;
    function    GetClause: string; virtual; abstract;
    function    isNegative: Boolean;
    property    Owner: TPerCriteria read GetOwner write SetOwner;
  published
    property    Attribute: string read FAttribute;
    property    Value: string read FValue;
    property    FieldName: string read GetFieldName write FFieldName;
  end;
  
  
  TPerSelectionCriteriaList = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TPerSelectionCriteriaAbs; reintroduce;
    procedure   SetItems(i: Integer; Value: TPerSelectionCriteriaAbs); reintroduce;
  protected
    function    GetOwner: TPerCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TPerCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TPerSelectionCriteriaAbs): integer; reintroduce;
    function    AsSQL: string;
    property    Items[Index: Integer]: TPerSelectionCriteriaAbs read GetItems write SetItems; default;
    property    Owner: TPerCriteria read GetOwner write SetOwner;
  end;
  
  
  TPerValueCriteriaAbs = class(TPerSelectionCriteriaAbs)
  end;
  
  
  TPerFieldCriteriaAbs = class(TPerSelectionCriteriaAbs)
  end;
  
  
  TPerEqualToCriteria = class(TPerValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerEqualToFieldCriteria = class(TPerFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerExistsCriteria = class(TPerValueCriteriaAbs)
  public
    constructor Create(ASubQuery: string; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  end;
  
  
  TPerGreaterThanCriteria = class(TPerValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerGreaterThanFieldCriteria = class(TPerFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerInCriteria = class(TPerValueCriteriaAbs)
  public
    ValueArray: array of string;
    function    GetClause: string; override;
  end;
  
  TPerLessThanCriteria = class(TPerValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;

  TPerLessThanFieldCriteria = class(TPerFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerLikeCriteria = class(TPerValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TPerNullCriteria = class(TPerValueCriteriaAbs)
  public
    constructor Create(AAttribute: string; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  end;
  
  
  TPerBetweenCriteria = class(TPerValueCriteriaAbs)
  private
    FValue_2: string;
  public
    constructor Create(AAttribute, AArg_1, AArg_2: string; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  published
    property    Value_2: string read FValue_2;
  end;
  
  
  TPerSQLCriteria = class(TPerSelectionCriteriaAbs)
  public
    constructor Create(ASQLStm: string); reintroduce; virtual;
    function    GetClause: string; override;
  end;

//  IFiltered  = interface
//    ['{2254A72F-11C8-410E-A285-560F74CBCCC9}']
//    function HasCriteria: boolean;
//    function GetCriteria: TPerCriteria; // property based criteria
//  end;

implementation

uses
  SysUtils
  ;
  
  
const
  cQuote = '''%s''';


{ TPerColumn }

function TPerColumn.GetOwner: TPerColumns;
begin
  Result := TPerColumns(inherited Owner);
end;

procedure TPerColumn.SetOwner(const Value: TPerColumns);
begin
  inherited Owner := Value;
end;


{ TPerColumns }

function TPerColumns.Add(const AObject: TPerColumn): integer;
begin
  result := inherited Add(AObject);
end;

procedure TPerColumns.CopyReferences(pSource: TPerColumns);
var
  i: Integer;
begin
  Assert(pSource <> nil, 'Source not assigned - CopyReferences');
  for i := 0 to pSource.Count - 1 do
    Add(pSource.Items[i]);
end;

function TPerColumns.GetItems(i: Integer): TPerColumn;
begin
  Result := TPerColumn(inherited Items[i]);
end;

function TPerColumns.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerColumns.SetItems(i: Integer; Value: TPerColumn);
begin
  inherited Items[i] := Value;
end;

procedure TPerColumns.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{ TPerCriteria }

constructor TPerCriteria.Create(pName: string);
begin
  inherited Create;
  FName           := pName;
  FCriteriaType   := crNONE;
  FisEmbraced     := False;

  FCriterias := TPerCriteriaList.Create;
  FCriterias.Owner                := Self;
  FCriterias.OwnsObjects          := true;
  FCriterias.AutoSetItemOwner     := False;
  
  FSelectionCriterias := TPerSelectionCriteriaList.Create;
  FSelectionCriterias.Owner       := Self;
  FSelectionCriterias.OwnsObjects := True;
  
  FGroupByList := TPerColumns.Create;
  FGroupByList.Owner              := Self;
  FGroupByList.OwnsObjects        := True;
  
  FOrderByList := TPerColumns.Create;
  FOrderByList.Owner              := Self;
  FOrderByList.OwnsObjects        := true;
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
  FisEmbraced := True;
  ACriteria.CriteriaType := crAND;
  FCriterias.Add(ACriteria);
end;

procedure TPerCriteria.AddBetween(AAttribute, AValue_1, AValue_2: string);
var
  lCriteria: TPerBetweenCriteria;
begin
  lCriteria := TPerBetweenCriteria.Create(AAttribute, AValue_1, AValue_2);
  FSelectionCriterias.Add(lCriteria);
end;

procedure TPerCriteria.AddEqualTo(AAttribute, AValue: string);
var
  lData: TPerEqualToCriteria;
begin
  lData := TPerEqualToCriteria.Create(AAttribute, QuotedStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddEqualTo(AAttribute: string; AValue: integer);
var
  lData: TPerEqualToCriteria;
begin
  lData := TPerEqualToCriteria.Create(AAttribute, IntToStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddExists(ASubQuery: string);
var
  lData: TPerExistsCriteria;
begin
  lData := TPerExistsCriteria.Create(ASubQuery);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterOrEqualThan(AAttribute, AValue: string);
var
  lData: TPerLessThanCriteria;
begin
  lData := TPerLessThanCriteria.Create(AAttribute, QuotedStr(AValue), true);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterOrEqualThan(AAttribute: string; AValue: integer);
var
  lData: TPerLessThanCriteria;
begin
  lData := TPerLessThanCriteria.Create(AAttribute, IntToStr(AValue), true);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterThan(AAttribute, AValue: string);
var
  lData: TPerGreaterThanCriteria;
begin
  lData := TPerGreaterThanCriteria.Create(AAttribute, QuotedStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGreaterThan(AAttribute: string; AValue: integer);
var
  lData: TPerGreaterThanCriteria;
begin
  lData := TPerGreaterThanCriteria.Create(AAttribute, IntToStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddGroupBy(AField: string);
var
  lData: TPerColumn;
begin
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
    AddGroupBy(AFields[i]);
end;

procedure TPerCriteria.AddIn(AAttribute, ASubQuery: string);
var
  lData: TPerInCriteria;
begin
  lData := TPerInCriteria.Create(AAttribute, ASubQuery);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddIn(AAttribute: string; AValueArray: array of string);
var
  lData: TPerInCriteria;
  i: integer;
begin
  lData := TPerInCriteria.Create(AAttribute, '');
  SetLength(lData.ValueArray, Length(AValueArray));

  for i := Low(AValueArray) to High(AValueArray) do
    lData.ValueArray[i] := QuotedStr(AValueArray[i]);

  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddIn(AAttribute: string; AValueArray: array of integer);
var
  lData: TPerInCriteria;
  i: integer;
begin
  lData := TPerInCriteria.Create(AAttribute, '');
  SetLength(lData.ValueArray, Length(AValueArray));

  for i := Low(AValueArray) to High(AValueArray) do
    lData.ValueArray[i] := IntToStr(AValueArray[i]);

  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddLessOrEqualThan(AAttribute, AValue: string);
var
  lData: TPerGreaterThanCriteria;
begin
  lData := TPerGreaterThanCriteria.Create(AAttribute, QuotedStr(AValue), true);
  FSelectionCriterias.Add(lData);end;

procedure TPerCriteria.AddLessThan(AAttribute, AValue: string);
var
  lData: TPerLessThanCriteria;
begin
  lData := TPerLessThanCriteria.Create(AAttribute, QuotedStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddLessOrEqualThan(AAttribute: string; AValue: integer);
var
  lData: TPerGreaterThanCriteria;
begin
  lData := TPerGreaterThanCriteria.Create(AAttribute, IntToStr(AValue), true);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddLessThan(AAttribute: string; AValue: integer);
var
  lData: TPerLessThanCriteria;
begin
  lData := TPerLessThanCriteria.Create(AAttribute, IntToStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddLike(AAttribute, AValue: string);
var
  lData: TPerLikeCriteria;
begin
  lData := TPerLikeCriteria.Create(AAttribute, QuotedStr(AValue));
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNotEqualTo(AAttribute, AValue: string);
var
  lData: TPerEqualToCriteria;
begin
  lData := TPerEqualToCriteria.Create(AAttribute, QuotedStr(AValue), True);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNotEqualTo(AAttribute: string; AValue: integer);
var
  lData: TPerEqualToCriteria;
begin
  lData := TPerEqualToCriteria.Create(AAttribute, IntToStr(AValue), True);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNotExists(ASubQuery: string);
var
  lData: TPerExistsCriteria;
begin
  lData := TPerExistsCriteria.Create(ASubQuery, True);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNotIn(AAttribute, ASubQuery: string);
begin
end;

procedure TPerCriteria.AddNotLike(AAttribute, AValue: string);
var
  lData: TPerLikeCriteria;
begin
  lData := TPerLikeCriteria.Create(AAttribute, QuotedStr(AValue), True);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNotNull(AAttribute: string);
var
  lData: TPerNullCriteria;
begin
  lData := TPerNullCriteria.Create(AAttribute, True);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddNull(AAttribute: string);
var
  lData: TPerNullCriteria;
begin
  lData := TPerNullCriteria.Create(AAttribute);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.AddOrCriteria(ACriteria: TPerCriteria);
begin
  FisEmbraced := True;
  ACriteria.CriteriaType := crOR;
  FCriterias.Add(ACriteria);
end;

procedure TPerCriteria.AddOrderBy(AField: string; ASorterAscending: boolean = False);
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
    AddOrderBy(AFields[i], false);
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
    AddOrderByAscending(AFields[i]);
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
    AddOrderByDescending(AFields[i]);
end;

procedure TPerCriteria.AddSQL(ASQLStm: string);
var
  lData: TPerSQLCriteria;
begin
  lData := TPerSQLCriteria.Create(ASQLStm);
  FSelectionCriterias.Add(lData);
end;

procedure TPerCriteria.ClearAll;
  procedure ClearList(AList: TtiObjectList);
  begin
    if assigned(AList) then
      AList.Clear;
  end;
begin
  ClearList(FCriterias);
  ClearList(FSelectionCriterias);
  ClearList(FOrderByList);
  ClearList(FGroupByList);
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

function TPerCriteria.HasCriteria: boolean;
begin
  result:= (FCriterias.Count > 0) or (FSelectionCriterias.Count > 0) or (FOrderByList.Count > 0) or (FGroupByList.Count > 0);
end;

function TPerCriteria.isEmbraced: Boolean;
begin
  Result := FisEmbraced;
end;

procedure TPerCriteria.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{ TPerCriteriaList }

function TPerCriteriaList.Add(const AObject: TPerCriteria): integer;
begin
  result := inherited Add(AObject);
  if (Count > 0) and Assigned(Owner) and (Owner is TPerCriteria) then
    Owner.FIsEmbraced := true;
end;

function TPerCriteriaList.GetItems(i: Integer): TPerCriteria;
begin
  Result := TPerCriteria(inherited Items[i]);
end;

function TPerCriteriaList.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerCriteriaList.SetItems(i: Integer; Value: TPerCriteria);
begin
  inherited Items[i] := Value;
end;

procedure TPerCriteriaList.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{ TPerSelectionCriteriaAbs }

constructor TPerSelectionCriteriaAbs.Create(AAttribute, AValue: string;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create;
  FAttribute  := AAttribute;
  FValue      := AValue;
  FisNegative := ANegative;
  FFieldName      := AFieldName;
end;

destructor TPerSelectionCriteriaAbs.Destroy;
begin
  inherited Destroy;
end;

function TPerSelectionCriteriaAbs.GetFieldName: string;
begin
  if FFieldName <> '' then
    Result := FFieldName
  else
    Result:= FAttribute;
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


{ TPerSelectionCriteriaList }

function TPerSelectionCriteriaList.Add(const AObject: TPerSelectionCriteriaAbs): integer;
begin
  result := inherited Add(AObject);
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

function TPerSelectionCriteriaList.GetItems(i: Integer): TPerSelectionCriteriaAbs;
begin
  Result := TPerSelectionCriteriaAbs(inherited Items[i]);
end;

function TPerSelectionCriteriaList.GetOwner: TPerCriteria;
begin
  Result := TPerCriteria(inherited Owner);
end;

procedure TPerSelectionCriteriaList.SetItems(i: Integer; Value:
        TPerSelectionCriteriaAbs);
begin
  inherited Items[i] := Value;
end;

procedure TPerSelectionCriteriaList.SetOwner(const Value: TPerCriteria);
begin
  inherited Owner := Value;
end;


{ TPerEqualToCriteria }

function TPerEqualToCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{ TPerEqualToFieldCriteria }

function TPerEqualToFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{ TPerExistsCriteria }

constructor TPerExistsCriteria.Create(ASubQuery: string; ANegative: boolean;
  AFieldName: string);
begin
  inherited Create('', ASubQuery, ANegative, AFieldName);
end;

function TPerExistsCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT EXISTS '
  else
    Result := ' EXISTS ';
end;


{ TPerGreaterThanCriteria }

function TPerGreaterThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{ TPerGreaterThanFieldCriteria }

function TPerGreaterThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{ TPerInCriteria }

function TPerInCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT IN '
  else
    Result := ' IN ';
end;


{ TPerLessThanCriteria }

function TPerLessThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{ TPerLessThanFieldCriteria }

function TPerLessThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{ TPerLikeCriteria }

function TPerLikeCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT LIKE '
  else
    Result := ' LIKE ';
end;


{ TPerNullCriteria }

constructor TPerNullCriteria.Create(AAttribute: string;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create(AAttribute, '', ANegative, AFieldName);
end;

function TPerNullCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' IS NOT NULL'
  else
    Result := ' IS NULL';
end;


{ TPerBetweenCriteria }

constructor TPerBetweenCriteria.Create(AAttribute, AArg_1, AArg_2: string;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create(AAttribute, AArg_1, ANegative, AFieldName);
  FValue_2 := AArg_2;
end;

function TPerBetweenCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT BETWEEN '
  else
    Result := ' BETWEEN ';
end;


{ TPerSQLCriteria }

constructor TPerSQLCriteria.Create(ASQLStm: string);
begin
  inherited Create(ASQLStm, '', false, '');
end;

function TPerSQLCriteria.GetClause: string;
begin
  Result := inherited Attribute;
end;

end.


