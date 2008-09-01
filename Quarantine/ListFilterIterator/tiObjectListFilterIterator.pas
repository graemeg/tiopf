unit tiObjectListFilterIterator;
{: Provides a basic Iterator for TtiObjectLists. <br>

  Comparison Tokens.  Can be any of the following:
      =, <>, >, >=, <, <=, "Between" and "In"

      ToDo:
      "Between" Syntax: 'AFieldName Between(1|10)'
      "In" Syntax:  'AFieldName In(Value1|Value2|Value3)'

}

interface
uses
  SysUtils
  ,tiObject
  ,tiRTTI
  ,tiUtils
  ;

type

  {: Interface for Enumerator. }
  IListFilterIterator = interface
  ['{97218242-07AF-4D68-B254-0DA463F5EDCD}']
    function    GoToStart: Boolean;
    function    GoToEnd: Boolean;
    function    First: Boolean;
    function    Last: Boolean;
    function    Next: Boolean;
    function    Previous: Boolean;
    function    Current: TtiObject;
    function    Count: Integer;
  end;

  {: Enumerated type indicating the type of comparison that will be performed. }
  TtiComparisonKind = (ckEqualTo, ckNotEqualTo, ckGreaterThan, ckGreaterOrEqual,
    ckLessThan, ckLessOrEqual, ckBetween, ckIn, ckLike);

  {: Base iterator filter class used for comparison of values. }
  TtiIteratorFilterAbs = class(TtiObject)
  protected
    FCompareType: TtiComparisonKind;
    FCompareToken: string;                            // Comparison token.  IE: "=", "<>", ">=", etc.
    FFieldName: string;                               // FieldName of object to compare against
  public
    {: Abstract function, InFilter.  Implemented in descendant classes. }
    function    InFilter(AObject: TtiObject): Boolean; virtual; abstract;
    {: Constructor }
    constructor CreateCustom; virtual;
  end;

  {: List of @link(TtiIteratorFilterAbs) objects. }
  TtiIteratorFilterList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiIteratorFilterAbs; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiIteratorFilterAbs); reintroduce;
  public
    property    Items[i:integer] : TtiIteratorFilterAbs read GetItems write SetItems;
    procedure   Add(AObject : TtiIteratorFilterAbs); reintroduce;
  end;

  {: Integer based filter class. }
  TtiIntegerFilter = class(TtiIteratorFilterAbs)
  private
    FIntValue: Int64; // Integer value to compare against;
  public
    function    InFilter(AObject: TtiObject): Boolean; override;
    constructor CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
      ACompareValue: Integer); reintroduce;
  end;

  {: Float based filter class. }
  TtiFloatFilter = class(TtiIteratorFilterAbs)
  private
    FFloatValue: Double;
  public
    function InFilter(AObject: TtiObject): Boolean; override;
    constructor CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
      ACompareValue: Double); reintroduce;
  end;

  {: String based filter class. }
  TtiStringFilter = class(TtiIteratorFilterAbs)
  private
    FStringValue: string;
  public
    function InFilter(AObject: TtiObject): Boolean; override;
    constructor CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
      ACompareValue: string); reintroduce;
  end;

  {: Boolean based filter class }
  TtiBooleanFilter = class(TtiIteratorFilterAbs)
  private
    FBoolValue: Boolean;
  public
    function InFilter(AObject: TtiObject): Boolean; override;
    constructor CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
      ACompareValue: Boolean); reintroduce;
  end;

  {: DateTime based filter class }
  TtiDateTimeFilter = class(TtiIteratorFilterAbs)
  private
    FDateTimeValue: TDateTime;
  public
    function InFilter(AObject: TtiObject): Boolean; override;
    constructor CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
      ACompareValue: TDateTime); reintroduce;
  end;

  {: Class of Iterator. }
  TtiIteratorClass = class of TtiListFilterIterator;

  {: Base implementation of List Iterator. }
  TtiListFilterIterator = class(TtiObject, IListFilterIterator)
  private
    FStopOnFail: Boolean;
  protected
    FInternalList: TtiObjectList;
    FFilterList: TtiIteratorFilterList;
    FCurrentIdx: Integer;
    {: Parses Constructor param, AFilter and creates all filters required. }
    procedure   DoParseFilter(const AFilter: string; ASubjectList: TtiObjectList); virtual;
    {: Function to extract the object's field name from FilterElement param. }
    function    DoGetFieldName(const AFilterElement: string; const ACompareToken: string): string; virtual;
    {: Function to extract the comparison value from FilterElement param. }
    Function    DoGetComparisonValue(const AFilterElement: string; const ACompareToken: string): string; virtual;
    {: Function to extract the comparison token from the FilterElement param. }
    function    DoGetComparisonToken(const AFilterElement: string): string; virtual;
    {: Function to extract the comparison type from the FilterElement param. }
    function    DoGetComparisonType(ACompToken: string): TtiComparisonKind; virtual;
    {: Tests a single object in the external list. }
    function    DoTestSingleObject(AObject: TtiObject): Boolean; virtual;
    {: Iterates the external list and adds references to its items if filters are met. }
    procedure   IterateAndFilter(ASubjectList: TtiObjectList); virtual;
  public
    function    GoToStart:  Boolean; virtual;
    function    GoToEnd:    boolean; virtual;
    function    First:      Boolean; virtual;
    function    Last:       Boolean; virtual;
    function    Next:       Boolean; virtual;
    function    Previous:   Boolean; virtual;
    function    Current:    TtiObject; virtual;
    function    Count:      Integer; virtual;
    constructor CreateCustom(AListToCopy: TtiObjectList; const AFilter: string;
      AStopOnFirstFail: Boolean = false);
    destructor  Destroy; override;
  end;

  {: Class of ttiObjectList. }
  TtiListClass = class of TtiObjectList;
  
  {: Registration object used to register list classes to return iterators of specific
  types to avoid casting. }
  TtiIteratorRegistration = class(TtiObject)
  private
    FListClass: TtiListClass;
    FIteratorClass: TtiIteratorClass;
    procedure SetListClass(const Value: TtiListClass);
    procedure SetIteratorClass(const Value: TtiIteratorClass);
  public
    property    ListClass: TtiListClass read FListClass write SetListClass;
    property    IteratorClass: TtiIteratorClass read FIteratorClass write SetIteratorClass;
  end;

  {: List of @link(@TtiIteratorRegistration) objects. }
  TtiIteratorRegistrationList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiIteratorRegistration; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiIteratorRegistration); reintroduce;
  public
    property    Items[i:integer] : TtiIteratorRegistration read GetItems write SetItems;
    procedure   Add(AObject : TtiIteratorRegistration); reintroduce;
  end;

  {: Manager object for producing iterators (that have been registered) for different lists. }
  TtiIteratorManager = class(TtiObject)
  protected
    FRegList: TtiIteratorRegistrationList;
    function    FindIteratorRegistration(AListClass: TtiListClass): TtiIteratorRegistration;
  public
    {: Registers an interator to use. }
    procedure   RegisterIterator(AListClass: TtiListClass; AIteratorClass: TtiIteratorClass);
    {: Retrieves an interator that has been registered using RegisterIterator. }
    function    GetIteratorI(const AFilter: string; AObjectList: TtiObjectList;
      const AStopFirstFail: Boolean = False): IListFilterIterator;
    function    GetIterator(const AFilter: string; AObjectList: TtiObjectList;
      const AStopFirstFail: Boolean = False): TtiListFilterIterator;

    constructor Create; override;
    destructor  Destroy; override;
  end;

  // -----------------------------------------------------------------
  //  Global, singleton access to TtiIteratorManager object.
  // -----------------------------------------------------------------

  function  gListIteratorMgr: TtiIteratorManager;

implementation

var
  mIteratorManager: TtiIteratorManager;

function  gListIteratorMgr: TtiIteratorManager;
begin
  if mIteratorManager = nil then
    mIteratorManager := TtiIteratorManager.Create;
  result := mIteratorManager;
end;


{ TtiInteratorFilterAbs }

constructor TtiIteratorFilterAbs.CreateCustom;
begin
  inherited Create;
end;

constructor TtiIntegerFilter.CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
  ACompareValue: Integer);
begin
  inherited Create;
  FIntValue := ACompareValue;
  FCompareType := ACompareType;
  FFieldName := AFieldName;
end;

function TtiIntegerFilter.InFilter(AObject: TtiObject): Boolean;
var
  lObjValue: Int64;
begin
  lObjValue := AObject.PropValue[FFieldName];
  case FCompareType of
    ckEqualTo:              result := (lObjValue = FIntValue);
    ckNotEqualTo:           result := (lObjValue <> FIntValue);
    ckGreaterThan:          result := (lObjValue > FIntValue);
    ckGreaterOrEqual:       Result := (lObjValue >= FIntValue);
    ckLessThan:             Result := (lObjValue < FIntValue);
    ckLessOrEqual:          Result := (lObjValue <= FIntValue);
    ckIn: ;
  end;
end;

{ TtiFloatFilter }

constructor TtiFloatFilter.CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
  ACompareValue: Double);
begin
  inherited Create;
  FFloatValue := ACompareValue;
  FCompareType := ACompareType;
  FFieldName := AFieldName;
end;

function TtiFloatFilter.InFilter(AObject: TtiObject): Boolean;
var
  lObjValue: Double;
begin
  lObjValue := AObject.PropValue[FFieldName];
  case FCompareType of
    ckEqualTo:              result := (lObjValue = FFloatValue);
    ckNotEqualTo:           result := (lObjValue <> FFloatValue);
    ckGreaterThan:          result := (lObjValue > FFloatValue);
    ckGreaterOrEqual:       Result := (lObjValue >= FFloatValue);
    ckLessThan:             Result := (lObjValue < FFloatValue);
    ckLessOrEqual:          Result := (lObjValue <= FFloatValue);
    ckIn: ;
  end;
end;

{ TtiStringFilter }

constructor TtiStringFilter.CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
  ACompareValue: string);
begin
  FCompareType := ACompareType;
  FStringValue := ACompareValue;
  FFieldName := AFieldName;
end;

function TtiStringFilter.InFilter(AObject: TtiObject): Boolean;
var
  lObjValue: string;
begin
  lObjValue := AObject.PropValue[FFieldName];
  case FCompareType of
    ckEqualTo:              result := (lObjValue = FStringValue);
    ckNotEqualTo:           result := (lObjValue <> FStringValue);
    ckGreaterThan:          result := (lObjValue > FStringValue);
    ckGreaterOrEqual:       Result := (lObjValue >= FStringValue);
    ckLessThan:             Result := (lObjValue < FStringValue);
    ckLessOrEqual:          Result := (lObjValue <= FStringValue);
    ckIn: ;
    ckLike:                 Result := tiWildcardMatch(lObjValue, FStringValue);
  end;
end;

{ TtiBooleanFilter }

constructor TtiBooleanFilter.CreateCustom(AFieldName: string; ACompareType: TtiComparisonKind;
  ACompareValue: Boolean);
begin
  inherited Create;
  FBoolValue := ACompareValue;
  FCompareType := ACompareType;
  FFieldName := AFieldName;
end;

function TtiBooleanFilter.InFilter(AObject: TtiObject): Boolean;
var
  lObjValue: Boolean;
begin
  lObjValue := AObject.PropValue[FFieldName];
  case FCompareType of
    ckEqualTo:              result := (lObjValue = FBoolValue);
  else
    raise Exception.Create(ClassName + '.InFilter: Only Equals comparison allowed.');
  end;
end;

{ TtiIteratorFilterList }

procedure TtiIteratorFilterList.Add(AObject: TtiIteratorFilterAbs);
begin
  inherited Add(AObject);
end;

function TtiIteratorFilterList.GetItems(i: integer): TtiIteratorFilterAbs;
begin
  result := TtiIteratorFilterAbs(inherited GetItems(i));
end;

procedure TtiIteratorFilterList.SetItems(i: integer;
  const AValue: TtiIteratorFilterAbs);
begin
  inherited SetItems(i, AValue);
end;

{ TtiListIterator }

function TtiListFilterIterator.Count: Integer;
begin
  result := FInternalList.Count;
end;

constructor TtiListFilterIterator.CreateCustom(AListToCopy: TtiObjectList;
  const AFilter: string; AStopOnFirstFail: Boolean = false);
begin
  inherited CreateWithRefCounting;
  FCurrentIdx := -1;

  FStopOnFail := AStopOnFirstFail;

  FInternalList := TtiObjectList.Create;
  FInternalList.OwnsObjects := false;           // Using shallow copies, don't free them
  FInternalList.AutoSetItemOwner := false;      // Don't change the owner
  FFilterList := TtiIteratorFilterList.Create;

  DoParseFilter(AFilter, AListToCopy);
  IterateAndFilter(AListToCopy);
end;

function TtiListFilterIterator.Current: TtiObject;
begin
  if FCurrentIdx >= 0 then
    begin
      if (FInternalList.Count -1) >= FCurrentIdx then
        result := FInternalList.Items[FCurrentIdx]
      else
        raise Exception.Create(ClassName + '.Current: No object at current index (' +
      IntToStr(FCurrentIdx));
    end
  else
    raise Exception.Create(ClassName + '.Current: No object at current index (' +
      IntToStr(FCurrentIdx));
end;

destructor TtiListFilterIterator.Destroy;
begin
  FInternalList.Clear;
  FInternalList.Free;
  FFilterList.Free;
  inherited;
end;

function TtiListFilterIterator.DoGetComparisonToken(
  const AFilterElement: string): string;
var
  lLowerElement: string;
begin

  lLowerElement := LowerCase(AFilterElement);

  if Pos('<>', lLowerElement) > 0 then
    Result := '<>'
  else if pos('>=', lLowerElement) > 0 then
    Result := '>='
  else if pos('<=', lLowerElement) > 0 then
    Result := '<='
  else if pos('=', lLowerElement) > 0 then
    Result := '='
  else if pos('>', lLowerElement) > 0 then
    Result := '>'
  else if Pos('<', lLowerElement) > 0 then
    Result := '<'
  else if pos('between', lLowerElement) > 0 then
    Result := 'BETWEEN'
  else if pos('in', lLowerElement) > 0 then
    Result := 'IN'
  else if pos('like', lLowerElement) > 0 then
    result := 'LIKE'
  else
    raise Exception.Create(ClassName + '.DoGetComparisonKind: Token not found');

end;

function TtiListFilterIterator.DoGetComparisonType(ACompToken: string): TtiComparisonKind;
begin
  if ACompToken = '<>' then
    result := ckNotEqualTo
  else if ACompToken = '>=' then
    result := ckGreaterOrEqual
  else if ACompToken = '<=' then
    result := ckLessOrEqual
  else if ACompToken = '=' then
    result := ckEqualTo
  else if ACompToken = '>' then
    Result := ckGreaterThan
  else if ACompToken = '<' then
    result := ckLessThan
  else if LowerCase(ACompToken) = 'between' then
    result := ckBetween
  else if LowerCase(ACompToken) = 'in' then
    result := ckIn
  else if LowerCase(ACompToken) = 'like' then
    result := ckLike
 else
    raise Exception.Create(ClassName + '.DoGetComparisonKind: Token not found');

end;

function TtiListFilterIterator.DoGetComparisonValue(
  const AFilterElement: string; const ACompareToken: string): string;
var
  lPOS: Integer;
begin
  lPOS := pos(ACompareToken, AFilterElement);

  Assert(lPOS > 0, ClassName + '.DoGetFieldName: Comparison token not present.');

  Result := Trim(tiUtils.tiToken(AFilterElement, ACompareToken, 2));

end;

function TtiListFilterIterator.DoGetFieldName(const AFilterElement: string;
  const ACompareToken: string): string;
var
  lPOS: Integer;
begin
  lPOS := pos(LowerCase(ACompareToken), LowerCase(AFilterElement));

  Assert(lPOS > 0, ClassName + '.DoGetFieldName: Comparison token not present.');

  Result := Trim(tiUtils.tiToken(LowerCase(AFilterElement), LowerCase(ACompareToken), 1));

end;

procedure TtiListFilterIterator.DoParseFilter(const AFilter: string; ASubjectList: TtiObjectList);
var
  lCounter: Integer;
  lCompValue: string;
  lCompType: TtiComparisonKind;
  lCompToken: string;
  lField: string;
  lElement: string;
  lFilter: string;
  lFieldKind: TtiTypeKind;
  lSubjectObject: TtiObject;
begin

  FFilterList.Clear;
  FInternalList.Clear;

  if ASubjectList.Count = 0 then
    Exit; //==>

  lFilter := Trim(AFilter);

  lSubjectObject := ASubjectList.Items[0];

  for lCounter := 1 to tiNumToken(lFilter, ',') do
    begin
      lElement := tiToken(lFilter, ',', lCounter);
      lCompToken := DoGetComparisonToken(lElement);
      lField := DoGetFieldName(lElement, lCompToken);
      lCompValue := DoGetComparisonValue(lElement, lCompToken);
      lCompType := DoGetComparisonType(lCompToken);

      // Get the field type
      lFieldKind := lSubjectObject.PropType(lField);

      case lFieldKind of
        tiTKInteger:
          FFilterList.Add(TtiIntegerFilter.CreateCustom(lField, lCompType, StrToInt(lCompValue)));
        tiTKFloat:
          FFilterList.Add(TtiFloatFilter.CreateCustom(lField, lCompType, StrToFloat(lCompValue)));
        tiTKString:
          FFilterList.Add(TtiStringFilter.CreateCustom(lField, lCompType, lCompValue));
        tiTKDateTime:
          FFilterList.Add(TtiDateTimeFilter.CreateCustom(lField, lCompType, StrToDateTime(lCompValue)));
        tiTKBoolean:
          FFilterList.Add(TtiBooleanFilter.CreateCustom(lField, lCompType, StrToBool(lCompValue)));
        tiTKBinary:
          raise Exception.Create(ClassName + '.DoParseFilter: Binary types not supported');
      end;
    end;
end;

function TtiListFilterIterator.DoTestSingleObject(AObject: TtiObject): Boolean;
var
  lCounter: Integer;
begin
  result := false;

  for lCounter := 0 to FFilterList.Count - 1 do
    begin
      result := FFilterList.Items[lCounter].InFilter(AObject);
      if not Result then
        Exit; //==>
    end;
end;

function TtiListFilterIterator.First: Boolean;
begin
  result := false;

  if FInternalList.Count > 0 then
    begin
      FCurrentIdx := 0;
      Result := true;
    end;
end;

function TtiListFilterIterator.GoToEnd: boolean;
begin
  Result := FInternalList.Count > 0;
  if Result then FCurrentIdx := FInternalList.Count;
end;

function TtiListFilterIterator.GoToStart: Boolean;
begin
  Result := FInternalList.Count > 0;
  if Result then
    FCurrentIdx := -1;
end;

procedure TtiListFilterIterator.IterateAndFilter(ASubjectList: TtiObjectList);
var
  lCounter: Integer;
  lSuccess: Boolean;
begin
  if FFilterList.Count = 0 then
    begin
      for lCounter := 0 to ASubjectList.Count -1 do
        begin
          FInternalList.Add(ASubjectList.Items[lCounter]);
        end;
    end
  else
    begin
      for lCounter := 0 to ASubjectList.Count -1 do
        begin
          lSuccess := DoTestSingleObject(ASubjectList.Items[lCounter]);
          if lSuccess then
            begin
              FInternalList.Add(ASubjectList.Items[lCounter]);
            end
          else
            begin
              if FStopOnFail then
                Break;
            end;
        end;
    end;

  if FInternalList.Count > 0 then
    FCurrentIdx := -1;
end;

function TtiListFilterIterator.Last: Boolean;
begin
  result := false;
  if FInternalList.Count > 0 then
    begin
      FCurrentIdx := FInternalList.Count -1;
      result := True;
    end;
end;

function TtiListFilterIterator.Next: Boolean;
begin
  result := false;

  if FCurrentIdx = -1 then
    Result := FInternalList.Count > 0
  else
    Result := ((FInternalList.Count -1) >= (FCurrentIdx + 1));

  if result then
    Inc(FCurrentIdx);

end;

function TtiListFilterIterator.Previous: Boolean;
begin
  result := false;

  if (FCurrentIdx = 0) or (FCurrentIdx = -1) then
    Exit; //==>

  Dec(FCurrentIdx);

  result := True;

end;

{ TtiDateTimeFilter }

constructor TtiDateTimeFilter.CreateCustom(AFieldName: string;
  ACompareType: TtiComparisonKind; ACompareValue: TDateTime);
begin
  FCompareType := ACompareType;
  FDateTimeValue := ACompareValue;
  FFieldName := AFieldName;
end;

function TtiDateTimeFilter.InFilter(AObject: TtiObject): Boolean;
var
  lObjValue: TDateTime;
begin
  lObjValue := AObject.PropValue[FFieldName];

  case FCompareType of
    ckEqualTo:              result := (lObjValue = FDateTimeValue);
    ckNotEqualTo:           result := (lObjValue <> FDateTimeValue);
    ckGreaterThan:          result := (lObjValue > FDateTimeValue);
    ckGreaterOrEqual:       Result := (lObjValue >= FDateTimeValue);
    ckLessThan:             Result := (lObjValue < FDateTimeValue);
    ckLessOrEqual:          Result := (lObjValue <= FDateTimeValue);
    ckIn: ;
    ckLike: ;
  end;

end;

{ TtiIteratorRegistration }

procedure TtiIteratorRegistration.SetIteratorClass(
  const Value: TtiIteratorClass);
begin
  FIteratorClass := Value;
end;

procedure TtiIteratorRegistration.SetListClass(const Value: TtiListClass);
begin
  FListClass := Value;
end;

{ TtiIteratorRegistrationList }

procedure TtiIteratorRegistrationList.Add(AObject: TtiIteratorRegistration);
begin
  inherited Add(AObject);
end;

function TtiIteratorRegistrationList.GetItems(
  i: integer): TtiIteratorRegistration;
begin
  result := TtiIteratorRegistration(inherited GetItems(i));
end;

procedure TtiIteratorRegistrationList.SetItems(i: integer;
  const AValue: TtiIteratorRegistration);
begin
  inherited SetItems(i, AValue);
end;

{ TtiIteratorManager }

constructor TtiIteratorManager.Create;
begin
  inherited Create;
  FRegList := TtiIteratorRegistrationList.Create;
end;

destructor TtiIteratorManager.Destroy;
begin
  FRegList.Free;
  inherited Destroy;
end;

function TtiIteratorManager.FindIteratorRegistration(
  AListClass: TtiListClass): TtiIteratorRegistration;
var
  lCounter: Integer;
  lReg: TtiIteratorRegistration;
begin
  result := nil;

  for lCounter := 0 to FRegList.Count -1 do
    begin
      lReg := FRegList.Items[lCounter];
      if lReg.ListClass = AListClass then
        begin
          result := lReg;
          Exit;
        end;
    end;

end;

function TtiIteratorManager.GetIterator(const AFilter: string;
  AObjectList: TtiObjectList;
  const AStopFirstFail: Boolean): TtiListFilterIterator;
var
  lReg: TtiIteratorRegistration;
  lIteratorClass: TtiIteratorClass;
begin
  lReg := FindIteratorRegistration(TtiListClass(AObjectList.ClassType));

  Assert(lReg <> nil, ClassName + '.FindIteratorRegistration: Registration not found');

  lIteratorClass := lReg.IteratorClass;

  result := lIteratorClass.CreateCustom(AObjectList, AFilter, AStopFirstFail);

end;

function TtiIteratorManager.GetIteratorI(const AFilter: string;
  AObjectList: TtiObjectList; const AStopFirstFail: Boolean = False): IListFilterIterator;
var
  lReg: TtiIteratorRegistration;
  lIteratorClass: TtiIteratorClass;
begin
  lReg := FindIteratorRegistration(TtiListClass(AObjectList.ClassType));

  Assert(lReg <> nil, ClassName + '.FindIteratorRegistration: Registration not found');

  lIteratorClass := lReg.IteratorClass;

  result := lIteratorClass.CreateCustom(AObjectList, AFilter, AStopFirstFail);

end;

procedure TtiIteratorManager.RegisterIterator(AListClass: TtiListClass;
  AIteratorClass: TtiIteratorClass);
var
  lIteratorReg: TtiIteratorRegistration;
begin
  lIteratorReg := TtiIteratorRegistration.Create;
  lIteratorReg.ListClass := AListClass;
  lIteratorReg.IteratorClass := AIteratorClass;
  FRegList.Add(lIteratorReg);
end;

initialization;
finalization
  if mIteratorManager <> nil then
    mIteratorManager.Free;
end.
