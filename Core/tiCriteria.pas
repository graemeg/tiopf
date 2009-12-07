unit tiCriteria;

{$I tiDefines.inc}

interface

uses
  tiObject
  ;

type
  TCriteriaType = (crAND, crOR, crNONE);

  // forward declarations
  TtiColumns                 = class;
  TtiCriteria                 = class;
  TtiCriteriaList             = class;
  TtiSelectionCriteriaAbs     = class;
  TtiSelectionCriteriaList    = class;


  ItiFiltered = interface
    ['{3B973E92-E6F2-4241-8A78-8068FC52133F}']
    function HasCriteria: boolean;
    function HasOrderBy: boolean;
    function GetCriteria: TtiCriteria;
  end;

  TtiColumn = class(TtiObject)
  private
    FAscending: Boolean;
    FName: string;
    FFieldName: string;
    function GetFieldName: string;
  protected
    function    GetOwner: TtiColumns; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiColumns); reintroduce; virtual;
  public
    property    Owner: TtiColumns read GetOwner write SetOwner;
  published
    property    Ascending: Boolean read FAscending write FAscending;
    property    Name: string read FName write FName;
    property    FieldName: string read GetFieldName write FFieldName;
  end;
  
  
  TtiColumns = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TtiColumn; reintroduce;
    procedure   SetItems(i: Integer; Value: TtiColumn); reintroduce;
  protected
    function    GetOwner: TtiCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TtiColumn): integer; reintroduce;
    procedure   CopyReferences(pSource: TtiColumns);
    property    Items[Index: Integer]: TtiColumn read GetItems write SetItems;
    property    Owner: TtiCriteria read GetOwner write SetOwner;
  end;
  
  
  TtiCriteria = class(TtiObject)
  private
    FCriterias: TtiCriteriaList;
    FCriteriaType: TCriteriaType;
    FGroupByList: TtiColumns;
    FIsEmbraced: Boolean;
    FName: string;
    FOrderByList: TtiColumns;
    FSelectionCriterias: TtiSelectionCriteriaList;
    FCriteriaAttrColMaps: TtiObjectList;
    function    GetCriterias: TtiCriteriaList;
    function    GetSelectionCriterias: TtiSelectionCriteriaList;
  protected
    function    GetOwner: TtiCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiCriteria); reintroduce; virtual;
  public
    constructor Create(pName: string); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   MapFieldNames(AClass: TtiClass);
    procedure   AddAndCriteria(ACriteria: TtiCriteria);
    procedure   AddBetween(AAttribute: string; AValue_1, AValue_2: variant);
    procedure   AddEqualTo(AAttribute: string; AValue: variant); overload;
    procedure   AddExists(ASubQuery: string);
    procedure   AddGreaterOrEqualThan(AAttribute: string; AValue: variant); overload;
    procedure   AddGreaterThan(AAttribute: string; AValue: variant); overload;
    procedure   AddGroupBy(AField: string); overload;
    procedure   AddGroupBy(AFields: array of string); overload;
    procedure   AddIn(AAttribute, ASubQuery: string); overload;
    procedure   AddIn(AAttribute: string; AValueArray: array of variant); overload;
    procedure   AddLessOrEqualThan(AAttribute: string; AValue: variant); overload;
    procedure   AddLessThan(AAttribute: string; AValue: variant); overload;
    procedure   AddLike(AAttribute, AValue: string);
    procedure   AddNotEqualTo(AAttribute: string; AValue: variant); overload;
    procedure   AddNotExists(ASubQuery: string);

    // AddNotIn() still needs to be implemented
    procedure   AddNotIn(AAttribute, ASubQuery: string); overload;
    procedure   AddNotIn(AAttribute: string; AValueArray: array of variant); overload;
    procedure   AddNotIn(AAttribute: string; AObjectList: TtiObjectList; const AFieldName: string); overload;

    procedure   AddNotLike(AAttribute, AValue: string);
    procedure   AddNotNull(AAttribute: string);
    procedure   AddNull(AAttribute: string);
    procedure   AddOrCriteria(ACriteria: TtiCriteria);
    procedure   AddOrderBy(AField: string; ASorterAscending: boolean = True); overload;
    procedure   AddOrderBy(AFields: array of string); overload;
    procedure   AddOrderByAscending(AField: string); overload;
    procedure   AddOrderByAscending(AFields: array of string); overload;
    procedure   AddOrderByDescending(AField: string); overload;
    procedure   AddOrderByDescending(AFields: array of string); overload;
    procedure   AddSQL(ASQLStm: string);

    procedure   ClearAll;
    function    GetGroupByList: TtiColumns;
    function    GetOrderByList: TtiColumns;
    function    isEmbraced: Boolean;
    function    HasCriteria: boolean;
    function    HasOrderBy: boolean;
    property    CriteriaType: TCriteriaType read FCriteriaType write FCriteriaType;
    property    Owner: TtiCriteria read GetOwner write SetOwner;
  published
    property    Criterias: TtiCriteriaList read GetCriterias;
    property    Name: string read FName;
    property    SelectionCriterias: TtiSelectionCriteriaList read GetSelectionCriterias;
  end;
  
  
  TtiCriteriaList = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TtiCriteria; reintroduce;
    procedure   SetItems(i: Integer; Value: TtiCriteria); reintroduce;
  protected
    function    GetOwner: TtiCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TtiCriteria): integer; reintroduce;
    property    Items[Index: Integer]: TtiCriteria read GetItems write SetItems;
    property    Owner: TtiCriteria read GetOwner write SetOwner;
  end;
  

  TtiSelectionCriteriaAbs = class(TtiObject)
  private
    FFieldName: string;
    FAttribute: string;
    FisNegative: Boolean;
    FValue:      variant;
    function    GetFieldName: string;
  protected
    function    GetOwner: TtiCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiCriteria); reintroduce; virtual;
  public
    constructor Create(AAttribute: string; AValue: variant; ANegative: boolean = False; AFieldName: string = ''); reintroduce; virtual;
    destructor  Destroy; override;
    function    GetClause: string; virtual; abstract;
    function    isNegative: Boolean;
    property    Owner: TtiCriteria read GetOwner write SetOwner;
  published
    property    Attribute: string read FAttribute;
    property    Value: variant read FValue;
    property    FieldName: string read GetFieldName write FFieldName;
  end;
  
  
  TtiSelectionCriteriaList = class(TtiObjectList)
  private
    function    GetItems(i: Integer): TtiSelectionCriteriaAbs; reintroduce;
    procedure   SetItems(i: Integer; Value: TtiSelectionCriteriaAbs); reintroduce;
  protected
    function    GetOwner: TtiCriteria; reintroduce; virtual;
    procedure   SetOwner(const Value: TtiCriteria); reintroduce; virtual;
  public
    function    Add(const AObject: TtiSelectionCriteriaAbs): integer; reintroduce;
    function    AsSQL: string;
    property    Items[Index: Integer]: TtiSelectionCriteriaAbs read GetItems write SetItems; default;
    property    Owner: TtiCriteria read GetOwner write SetOwner;
  end;
  
  
  TtiValueCriteriaAbs = class(TtiSelectionCriteriaAbs)
  end;
  

  TtiFieldCriteriaAbs = class(TtiSelectionCriteriaAbs)
  end;
  
  
  TtiEqualToCriteria = class(TtiValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TtiEqualToFieldCriteria = class(TtiFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  

  TtiExistsCriteria = class(TtiValueCriteriaAbs)
  public
    constructor Create(ASubQuery: string; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  end;
  
  
  TtiGreaterThanCriteria = class(TtiValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TtiGreaterThanFieldCriteria = class(TtiFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TtiInCriteria = class(TtiValueCriteriaAbs)
  public
    ValueArray: array of variant;
    function    GetClause: string; override;
  end;
  
  
  TtiLessThanCriteria = class(TtiValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;


  TtiLessThanFieldCriteria = class(TtiFieldCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TtiLikeCriteria = class(TtiValueCriteriaAbs)
  public
    function    GetClause: string; override;
  end;
  
  
  TtiNullCriteria = class(TtiValueCriteriaAbs)
  public
    constructor Create(AAttribute: string; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  end;
  
  
  TtiBetweenCriteria = class(TtiValueCriteriaAbs)
  private
    FValue_2: variant;
  public
    constructor Create(AAttribute: string; AArg_1, AArg_2: variant; ANegative: boolean = false; AFieldName: string = ''); reintroduce; virtual;
    function    GetClause: string; override;
  published
    property    Value_2: variant read FValue_2;
  end;
  
  
  TtiSQLCriteria = class(TtiSelectionCriteriaAbs)
  public
    constructor Create(ASQLStm: string); reintroduce; virtual;
    function    GetClause: string; override;
  end;


implementation

uses
  SysUtils
  ,tiAutoMap
  ,tiOPFManager
  ;

  
const
  cQuote = '''%s''';


{ TPerColumn }

function TtiColumn.GetFieldName: string;
begin
  if FFieldName = '' then
    result := FName
  else
    Result := FFieldName;
end;

function TtiColumn.GetOwner: TtiColumns;
begin
  Result := TtiColumns(inherited Owner);
end;

procedure TtiColumn.SetOwner(const Value: TtiColumns);
begin
  inherited Owner := Value;
end;


{ TPerColumns }

function TtiColumns.Add(const AObject: TtiColumn): integer;
begin
  result := inherited Add(AObject);
end;

procedure TtiColumns.CopyReferences(pSource: TtiColumns);
var
  i: Integer;
begin
  Assert(pSource <> nil, 'Source not assigned - CopyReferences');
  for i := 0 to pSource.Count - 1 do
    Add(pSource.Items[i]);
end;

function TtiColumns.GetItems(i: Integer): TtiColumn;
begin
  Result := TtiColumn(inherited Items[i]);
end;

function TtiColumns.GetOwner: TtiCriteria;
begin
  Result := TtiCriteria(inherited Owner);
end;

procedure TtiColumns.SetItems(i: Integer; Value: TtiColumn);
begin
  inherited Items[i] := Value;
end;

procedure TtiColumns.SetOwner(const Value: TtiCriteria);
begin
  inherited Owner := Value;
end;


{ TtiCriteria }

constructor TtiCriteria.Create(pName: string);
begin
  inherited Create;
  FName           := pName;
  FCriteriaType   := crNONE;
  FisEmbraced     := False;

  FCriterias := TtiCriteriaList.Create;
  FCriterias.Owner                := Self;
  FCriterias.OwnsObjects          := true;
  FCriterias.AutoSetItemOwner     := False;
  
  FSelectionCriterias := TtiSelectionCriteriaList.Create;
  FSelectionCriterias.Owner       := Self;
  FSelectionCriterias.OwnsObjects := True;
  
  FGroupByList := TtiColumns.Create;
  FGroupByList.Owner              := Self;
  FGroupByList.OwnsObjects        := True;
  
  FOrderByList := TtiColumns.Create;
  FOrderByList.Owner              := Self;
  FOrderByList.OwnsObjects        := true;

  FCriteriaAttrColMaps            := TtiAttrColMaps.Create;
  FCriteriaAttrColMaps.OwnsObjects := False;
  FCriteriaAttrColMaps.AutoSetItemOwner := False;
end;

destructor TtiCriteria.Destroy;
begin
  FCriterias.Free;
  FSelectionCriterias.Free;
  FOrderByList.Free;
  FGroupByList.Free;
  FCriteriaAttrColMaps.Free;
  inherited Destroy;
end;

procedure TtiCriteria.AddAndCriteria(ACriteria: TtiCriteria);
begin
  FisEmbraced := True;
  ACriteria.CriteriaType := crAND;
  FCriterias.Add(ACriteria);
end;

procedure TtiCriteria.AddBetween(AAttribute: string; AValue_1, AValue_2: variant);
var
  lCriteria: TtiBetweenCriteria;
begin
  lCriteria := TtiBetweenCriteria.Create(AAttribute, AValue_1, AValue_2);
  FSelectionCriterias.Add(lCriteria);
end;

procedure TtiCriteria.AddEqualTo(AAttribute: string; AValue: variant);
var
  lData: TtiEqualToCriteria;
begin
  lData := TtiEqualToCriteria.Create(AAttribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddExists(ASubQuery: string);
var
  lData: TtiExistsCriteria;
begin
  lData := TtiExistsCriteria.Create(ASubQuery);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddGreaterOrEqualThan(AAttribute: string; AValue: variant);
var
  lData: TtiLessThanCriteria;
begin
  lData := TtiLessThanCriteria.Create(AAttribute, (AValue), true);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddGreaterThan(AAttribute: string; AValue: variant);
var
  lData: TtiGreaterThanCriteria;
begin
  lData := TtiGreaterThanCriteria.Create(AAttribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddGroupBy(AField: string);
var
  lData: TtiColumn;
begin
  Assert(AField <> '', 'AField is blank!');
  lData := TtiColumn.Create;
  lData.Name := AField;
  FGroupByList.Add(lData);
end;

procedure TtiCriteria.AddGroupBy(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    AddGroupBy(AFields[i]);
end;

procedure TtiCriteria.AddIn(AAttribute, ASubQuery: string);
var
  lData: TtiInCriteria;
begin
  lData := TtiInCriteria.Create(AAttribute, ASubQuery);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddIn(AAttribute: string; AValueArray: array of variant);
var
  lData: TtiInCriteria;
  i: integer;
begin
  lData := TtiInCriteria.Create(AAttribute, '');
  SetLength(lData.ValueArray, Length(AValueArray));

  for i := Low(AValueArray) to High(AValueArray) do
    lData.ValueArray[i] := (AValueArray[i]);

  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddLessOrEqualThan(AAttribute: string; AValue: variant);
var
  lData: TtiGreaterThanCriteria;
begin
  lData := TtiGreaterThanCriteria.Create(AAttribute, AValue, true);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddLessThan(AAttribute: string; AValue: variant);
var
  lData: TtiLessThanCriteria;
begin
  lData := TtiLessThanCriteria.Create(AAttribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddLike(AAttribute, AValue: string);
var
  lData: TtiLikeCriteria;
begin
  lData := TtiLikeCriteria.Create(AAttribute, AValue);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotEqualTo(AAttribute: string; AValue: variant);
var
  lData: TtiEqualToCriteria;
begin
  lData := TtiEqualToCriteria.Create(AAttribute, AValue, True);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotExists(ASubQuery: string);
var
  lData: TtiExistsCriteria;
begin
  lData := TtiExistsCriteria.Create(ASubQuery, True);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotIn(AAttribute: string; AObjectList: TtiObjectList;
  const AFieldName: string);
var
  lData: TtiInCriteria;
  i: Integer;
  lVarArray: array of Variant;
begin
  Assert(AObjectList <> nil, ClassName + '.AddNotIn: AObjectList is nil');

  if AObjectList.Count = 0 then
    Exit; //==>

   
  for i := 0 to AObjectList.Count -1 do
    if AObjectList.Items[i].ObjectState <> posDelete then
      begin
        SetLength(lVarArray, Length(lVarArray) + 1);
        lVarArray[High(lVarArray)] := AObjectList.Items[i].PropValue[AFieldName];
      end;

  if Length(lVarArray) = 0 then
    Exit; //==>

  lData := TtiInCriteria.Create(AAttribute, '', True);

  // copy array values
  SetLength(lData.ValueArray, Length(lVarArray));
  for i := Low(lVarArray) to High(lVarArray) do
    lData.ValueArray[i] := lVarArray[i];

  FSelectionCriterias.Add(lData);

end;

procedure TtiCriteria.AddNotIn(AAttribute: string;
  AValueArray: array of variant);
var
  lData: TtiInCriteria;
  i: integer;
begin
  lData := TtiInCriteria.Create(AAttribute, '', true);
  SetLength(lData.ValueArray, Length(AValueArray));

  for i := Low(AValueArray) to High(AValueArray) do
    lData.ValueArray[i] := (AValueArray[i]);

  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotIn(AAttribute, ASubQuery: string);
var
  lData: TtiInCriteria;
begin
  lData := TtiInCriteria.Create(AAttribute, ASubQuery, true);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotLike(AAttribute, AValue: string);
var
  lData: TtiLikeCriteria;
begin
  lData := TtiLikeCriteria.Create(AAttribute, AValue, True);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNotNull(AAttribute: string);
var
  lData: TtiNullCriteria;
begin
  lData := TtiNullCriteria.Create(AAttribute, True);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddNull(AAttribute: string);
var
  lData: TtiNullCriteria;
begin
  lData := TtiNullCriteria.Create(AAttribute);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.AddOrCriteria(ACriteria: TtiCriteria);
begin
  FisEmbraced := True;
  ACriteria.CriteriaType := crOR;
  FCriterias.Add(ACriteria);
end;

procedure TtiCriteria.AddOrderBy(AField: string; ASorterAscending: boolean = True);
var
  lData: TtiColumn;
begin
  Assert(AField <> '', 'AField is blank!');
  lData := TtiColumn.Create;
  lData.Ascending := ASorterAscending;
  lData.Name := AField;
  FOrderByList.Add(lData);
end;

procedure TtiCriteria.AddOrderBy(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    AddOrderBy(AFields[i], true);
end;

procedure TtiCriteria.AddOrderByAscending(AField: string);
begin
  Assert(AField <> '', 'AField is blank!');
  AddOrderBy(AField, true);
end;

procedure TtiCriteria.AddOrderByAscending(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    AddOrderByAscending(AFields[i]);
end;

procedure TtiCriteria.AddOrderByDescending(AField: string);
begin
  Assert(AField <> '', 'AField is blank!');
  AddOrderBy(AField, false);
end;

procedure TtiCriteria.AddOrderByDescending(AFields: array of string);
var
  i: Integer;
begin
  for i := Low(AFields) to High(AFields) do
    AddOrderByDescending(AFields[i]);
end;

procedure TtiCriteria.AddSQL(ASQLStm: string);
var
  lData: TtiSQLCriteria;
begin
  lData := TtiSQLCriteria.Create(ASQLStm);
  FSelectionCriterias.Add(lData);
end;

procedure TtiCriteria.ClearAll;
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

function TtiCriteria.GetCriterias: TtiCriteriaList;
begin
  Result := FCriterias;
end;

function TtiCriteria.GetGroupByList: TtiColumns;
begin
  Result := FGroupByList;
end;

function TtiCriteria.GetOrderByList: TtiColumns;
begin
  Result := FOrderByList;
end;

function TtiCriteria.GetOwner: TtiCriteria;
begin
  Result := TtiCriteria(inherited Owner);
end;

function TtiCriteria.GetSelectionCriterias: TtiSelectionCriteriaList;
begin
  Result := FSelectionCriterias;
end;

function TtiCriteria.HasCriteria: boolean;
begin
  result:= (FCriterias.Count > 0) or (FSelectionCriterias.Count > 0); // or (FGroupByList.Count > 0);
end;

function TtiCriteria.HasOrderBy: boolean;
begin
  result:= (FOrderByList.Count > 0);
end;

function TtiCriteria.isEmbraced: Boolean;
begin
  Result := FisEmbraced;
end;

procedure TtiCriteria.MapFieldNames(AClass: TtiClass);
var i: integer;
  maps: TtiAttrColMaps;
  lVisProAttributeToFieldName: TVisProAttributeToFieldName;

  lMap:      TtiAttrColMap;
begin
  maps:= TtiAttrColMaps(FCriteriaAttrColMaps);

  // map property based critera to table based
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    AClass, maps);

  lVisProAttributeToFieldName :=
    TVisProAttributeToFieldName.Create(maps,
    AClass);
  try
    Iterate(lVisProAttributeToFieldName);

    for i := 0 to FOrderByList.count - 1 do
    begin
      lMap := maps.FindByClassAttrMap(AClass, FOrderByList.Items[i].Name);
      if assigned(lMap) then
        FOrderByList.Items[i].FieldName := lMap.DBColMap.ColName;
    end;

  finally
    lVisProAttributeToFieldName.Free;
  end;
end;

procedure TtiCriteria.SetOwner(const Value: TtiCriteria);
begin
  inherited Owner := Value;
end;


{ TtiCriteriaList }

function TtiCriteriaList.Add(const AObject: TtiCriteria): integer;
begin
  result := inherited Add(AObject);
  if (Count > 0) and Assigned(Owner) and (Owner is TtiCriteria) then
    Owner.FIsEmbraced := true;
end;

function TtiCriteriaList.GetItems(i: Integer): TtiCriteria;
begin
  Result := TtiCriteria(inherited Items[i]);
end;

function TtiCriteriaList.GetOwner: TtiCriteria;
begin
  Result := TtiCriteria(inherited Owner);
end;

procedure TtiCriteriaList.SetItems(i: Integer; Value: TtiCriteria);
begin
  inherited Items[i] := Value;
end;

procedure TtiCriteriaList.SetOwner(const Value: TtiCriteria);
begin
  inherited Owner := Value;
end;


{ TtiSelectionCriteriaAbs }

constructor TtiSelectionCriteriaAbs.Create(AAttribute: string; AValue: variant;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create;
  FAttribute    := AAttribute;
  FValue        := AValue;
  FisNegative   := ANegative;
  FFieldName    := AFieldName;
end;

destructor TtiSelectionCriteriaAbs.Destroy;
begin
  inherited Destroy;
end;

function TtiSelectionCriteriaAbs.GetFieldName: string;
begin
  if FFieldName <> '' then
    Result := FFieldName
  else
    Result:= FAttribute;
end;

function TtiSelectionCriteriaAbs.GetOwner: TtiCriteria;
begin
  Result := TtiCriteria(inherited Owner);
end;

function TtiSelectionCriteriaAbs.isNegative: Boolean;
begin
  Result := FisNegative;
end;

procedure TtiSelectionCriteriaAbs.SetOwner(const Value: TtiCriteria);
begin
  inherited Owner := Value;
end;


{ TtiSelectionCriteriaList }

function TtiSelectionCriteriaList.Add(const AObject: TtiSelectionCriteriaAbs): integer;
begin
  result := inherited Add(AObject);
end;

function TtiSelectionCriteriaList.AsSQL: string;
var
  i: Integer;
begin
  Result := ' (';
  for i := 0 to Count - 1 do
  begin
    Result := Result + TtiSelectionCriteriaAbs(Items[i]).GetClause +
      ' AND ';
  end;
  Result := Result + ') ';
end;

function TtiSelectionCriteriaList.GetItems(i: Integer): TtiSelectionCriteriaAbs;
begin
  Result := TtiSelectionCriteriaAbs(inherited Items[i]);
end;

function TtiSelectionCriteriaList.GetOwner: TtiCriteria;
begin
  Result := TtiCriteria(inherited Owner);
end;

procedure TtiSelectionCriteriaList.SetItems(i: Integer; Value:
        TtiSelectionCriteriaAbs);
begin
  inherited Items[i] := Value;
end;

procedure TtiSelectionCriteriaList.SetOwner(const Value: TtiCriteria);
begin
  inherited Owner := Value;
end;


{ TtiEqualToCriteria }

function TtiEqualToCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{ TtiEqualToFieldCriteria }

function TtiEqualToFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <> '
  else
    Result := ' = ';
end;


{ TtiExistsCriteria }

constructor TtiExistsCriteria.Create(ASubQuery: string; ANegative: boolean;
  AFieldName: string);
begin
  inherited Create('', ASubQuery, ANegative, AFieldName);
end;

function TtiExistsCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT EXISTS '
  else
    Result := ' EXISTS ';
end;


{ TtiGreaterThanCriteria }

function TtiGreaterThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{ TtiGreaterThanFieldCriteria }

function TtiGreaterThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' <= '
  else
    Result := ' > ';
end;


{ TtiInCriteria }

function TtiInCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT IN '
  else
    Result := ' IN ';
end;


{ TtiLessThanCriteria }

function TtiLessThanCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{ TtiLessThanFieldCriteria }

function TtiLessThanFieldCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' >= '
  else
    Result := ' < ';
end;


{ TtiLikeCriteria }

function TtiLikeCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT LIKE '
  else
    Result := ' LIKE ';
end;


{ TtiNullCriteria }

constructor TtiNullCriteria.Create(AAttribute: string;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create(AAttribute, '', ANegative, AFieldName);
end;

function TtiNullCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' IS NOT NULL'
  else
    Result := ' IS NULL';
end;


{ TtiBetweenCriteria }

constructor TtiBetweenCriteria.Create(AAttribute: string; AArg_1, AArg_2: variant;
    ANegative: boolean = False; AFieldName: string = '');
begin
  inherited Create(AAttribute, AArg_1, ANegative, AFieldName);
  FValue_2 := AArg_2;
end;

function TtiBetweenCriteria.GetClause: string;
begin
  if isNegative then
    Result := ' NOT BETWEEN '
  else
    Result := ' BETWEEN ';
end;


{ TtiSQLCriteria }

constructor TtiSQLCriteria.Create(ASQLStm: string);
begin
  inherited Create(ASQLStm, '', false, '');
end;

function TtiSQLCriteria.GetClause: string;
begin
  Result := inherited Attribute;
end;

end.






