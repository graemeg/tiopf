unit tiAutoMapSelect;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiObject
  ,tiCriteria
{$IFDEF DELPHI2010ORABOVE}
  ,tiGenericList
  ,tiSmartPointer
{$ENDIF}
  ;

type
  {$IFDEF DELPHI2010ORABOVE}
  TArrayOfString= array of string;
  {$ENDIF}
  {: Record wrapper around TtiCriteria with logical operator overloads to
     provide a more natural syntax "criteria and criteria and ..." }
  TtiAutoCriteria = record
  private
    FCriteria: TtiCriteria;
    FCriteriaLifetime: IInterface;
    function GetCriteria: TtiCriteria;
  public
    procedure Assign(const ASource: TtiAutoCriteria);
    class operator LogicalAnd(const ALeft, ARight: TtiAutoCriteria): TtiAutoCriteria;
    class operator LogicalOr(const ALeft, ARight: TtiAutoCriteria): TtiAutoCriteria;
    //TODO: Implement LogicalNot
    //class operator LogicalNot(const ACriteria: TtiAutoCriteria): TtiAutoCriteria;

    class function Equals(AAttribute: string; AValue: Variant): TtiAutoCriteria; static;
    class function GreaterThan(AAttribute: string; AValue: Variant): TtiAutoCriteria; static;
    class function GreaterThanOrEquals(AAttribute: string; AValue: Variant): TtiAutoCriteria; static;
    class function LessThan(AAttribute: string; AValue: Variant): TtiAutoCriteria; static;
    class function LessThanOrEquals(AAttribute: string; AValue: Variant): TtiAutoCriteria; static;
    class function Between(AAttribute: string; AValue1, AValue2: Variant): TtiAutoCriteria; static;
    class function IsIn(AAttribute: string; ASubQuery: string): TtiAutoCriteria; overload; static;
    class function IsIn(AAttribute: string; AValues: array of Variant): TtiAutoCriteria; overload; static;
    class function Like(AAttribute: string; AValue: string): TtiAutoCriteria; static;
    class function Null(AAttribute: string): TtiAutoCriteria; static;
    //TODO: Implement Exists
    //class function Exists(ASubQuery: string): TtiAutoCriteria; static;

    property Criteria: TtiCriteria read GetCriteria;
  end;

  {: Wraps a ItiFilteredSelectable and TtiCriteria so that a temporary
     criteria can used in the collection read visitor }
  TtiSelectListWrapper = class(TtiObject)
  private
    FList: TtiObjectList; // ItiFilteredSelectable
    FCriteria: TtiCriteria;
  public
    constructor Create(AList: TtiObjectList); reintroduce;
    destructor Destroy; override;
    procedure Read; override;
    property Criteria: TtiCriteria read FCriteria;
    property List: TtiObjectList read FList;
  end;

{$IFDEF DELPHI2010ORABOVE}
  ItiAutoMapSelect<T: TtiObject> = interface
  ['{6FEC2BB0-A6B5-49C1-8310-D7A368669770}']
    function Where(const ACriteria: TtiAutoCriteria): ItiAutoMapSelect<T>;
    //TODO: Implement GroupBy
    //function GroupBy(AField: string): ItiAutoMapSelect<T>; overload;
    //function GroupBy(AFields: array of string): ItiAutoMapSelect<T>; overload;
    function OrderBy(AField: string; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    function OrderBy(AFields: TArrayOfString; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    function List: TtiObjectList<T>;
    function IList: ItiSmartPointer<TtiObjectList<T>>;
  end;
{$ENDIF}

  // Auto mapping reader with fluent interface for use internal to list classes
{$IFDEF DELPHI2010ORABOVE}
  TtiAutoMapSelectInt<T: TtiObject> = class(TInterfacedObject, ItiAutoMapSelect<T>)
  private
    FCriteria: TtiAutoCriteria;
    FList: TtiObjectList<T>;
  public
    constructor Create(AList: TtiObjectList<T>); reintroduce;
    function Where(const ACriteria: TtiAutoCriteria): ItiAutoMapSelect<T>;
    //TODO: Implement GroupBy
    //function GroupBy(AField: string): ItiAutoMapSelect<T>; overload;
    //function GroupBy(AFields: array of string): ItiAutoMapSelect<T>; overload;
    function OrderBy(AField: string; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    function OrderBy(AFields: TArrayOfString; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    function List: TtiObjectList<T>;
    function IList: ItiSmartPointer<TtiObjectList<T>>;
  end;
{$ENDIF}

{$IFDEF DELPHI2010ORABOVE}
  TtiAutoMapSelectExt<T: TtiObject> = class(TInterfacedObject, ItiAutoMapSelect<T>)
  private
    FCriteria: TtiAutoCriteria;
  public
    function Where(const ACriteria: TtiAutoCriteria): ItiAutoMapSelect<T>;
    //TODO: Implement GroupBy
    //function GroupBy(AField: string): ItiAutoMapSelectExt<T>; overload;
    //function GroupBy(AFields: array of string): ItiAutoMapSelectExt<T>; overload;
    function OrderBy(AField: string; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    function OrderBy(AFields: TArrayOfString; AAscending: Boolean = true): ItiAutoMapSelect<T>; overload;
    {: This returns a new list instance that the caller must free }
    function List: TtiObjectList<T>;
    function IList: ItiSmartPointer<TtiObjectList<T>>;
  end;
{$ENDIF}

implementation

uses
  SysUtils,
  tiConstants,
  tiFilteredObjectList;

type
  TtiObjectLifetime = class(TInterfacedObject)
  private
    FObject: TObject;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
  end;

  { TtiObjectLifetime }

  constructor TtiObjectLifetime.Create(AObject: TObject);
  begin
    inherited Create;
    FObject := AObject;
  end;

  destructor TtiObjectLifetime.Destroy;
  begin
    FObject.Free;
    inherited;
  end;

{ TtiAutoCriteria }

procedure TtiAutoCriteria.Assign(const ASource: TtiAutoCriteria);
begin
  GetCriteria;
  FCriteria.Assign(ASource.Criteria);
end;

class function TtiAutoCriteria.Between(AAttribute: string;
  AValue1, AValue2: Variant): TtiAutoCriteria;
begin
  Result.Criteria.AddBetween(AAttribute, AValue1, AValue2);
end;

class function TtiAutoCriteria.Equals(AAttribute: string; AValue: Variant):
  TtiAutoCriteria;
begin
  Result.Criteria.AddEqualTo(AAttribute, AValue);
end;

function TtiAutoCriteria.GetCriteria: TtiCriteria;
begin
  if not Assigned(FCriteria) then
  begin
    FCriteria := TtiCriteria.Create('TtiAutoCriteria');
    FCriteriaLifetime := TtiObjectLifetime.Create(FCriteria);
  end;
  Result := FCriteria;
end;

class function TtiAutoCriteria.GreaterThan(AAttribute: string; AValue: Variant):
  TtiAutoCriteria;
begin
  Result.Criteria.AddGreaterThan(AAttribute, AValue);
end;

class function TtiAutoCriteria.GreaterThanOrEquals(AAttribute: string;
  AValue: Variant): TtiAutoCriteria;
begin
  Result.Criteria.AddGreaterOrEqualThan(AAttribute, AValue);
end;

class function TtiAutoCriteria.IsIn(AAttribute, ASubQuery: string):
  TtiAutoCriteria;
begin
  Result.Criteria.AddIn(AAttribute, ASubQuery);
end;

class function TtiAutoCriteria.IsIn(AAttribute: string;
  AValues: array of Variant): TtiAutoCriteria;
begin
  Result.Criteria.AddIn(AAttribute, AValues);
end;

class function TtiAutoCriteria.LessThan(AAttribute: string; AValue: Variant):
  TtiAutoCriteria;
begin
  Result.Criteria.AddLessThan(AAttribute, AValue);
end;

class function TtiAutoCriteria.LessThanOrEquals(AAttribute: string;
  AValue: Variant): TtiAutoCriteria;
begin
  Result.Criteria.AddLessOrEqualThan(AAttribute, AValue);
end;

class function TtiAutoCriteria.Like(AAttribute: string; AValue: string):
  TtiAutoCriteria;
begin
  Result.Criteria.AddLike(AAttribute, AValue);
end;

class operator TtiAutoCriteria.LogicalAnd(const ALeft, ARight: TtiAutoCriteria):
  TtiAutoCriteria;
begin
  Result.Criteria.Assign(ALeft.Criteria);
  Result.Criteria.AddAndCriteria(TtiCriteria(ARight.Criteria.Clone));
end;

class operator TtiAutoCriteria.LogicalOr(const ALeft, ARight: TtiAutoCriteria):
  TtiAutoCriteria;
begin
  Result.Criteria.Assign(ALeft.Criteria);
  Result.Criteria.AddOrCriteria(TtiCriteria(ARight.Criteria.Clone));
end;

class function TtiAutoCriteria.Null(AAttribute: string): TtiAutoCriteria;
begin
  Result.Criteria.AddNull(AAttribute);
end;

{ TtiSelectListWrapper }

constructor TtiSelectListWrapper.Create(AList: TtiObjectList);
begin
  Assert(AList.TestValid(TtiObjectList), CTIErrorInvalidObject);
  Assert(Supports(AList, ItiFilteredSelectable), 'Must support ItiFilteredSelectable');
  inherited Create;
  FList := AList;
  FCriteria := TtiCriteria.Create;
end;

destructor TtiSelectListWrapper.Destroy;
begin
  FCriteria.Free;
  inherited;
end;

procedure TtiSelectListWrapper.Read;
begin
  inherited;
end;

{ TtiAutoMapSelectInt<T> }

{$IFDEF DELPHI2010ORABOVE}
constructor TtiAutoMapSelectInt<T>.Create(AList: TtiObjectList<T>);
begin
  Assert(AList.TestValid(TtiFilteredObjectList<T>), CTIErrorInvalidObject);
  inherited Create;
  FList := AList;
end;

function TtiAutoMapSelectInt<T>.OrderBy(AField: string;
  AAscending: Boolean): ItiAutoMapSelect<T>;
begin
  FCriteria.Criteria.AddOrderBy(AField, AAscending);
  Result := Self;
end;

function TtiAutoMapSelectInt<T>.OrderBy(AFields: TArrayOfString;
  AAscending: Boolean): ItiAutoMapSelect<T>;
begin
  FCriteria.Criteria.AddOrderBy(AFields, AAscending);
  Result := Self;
end;

function TtiAutoMapSelectInt<T>.Where(const ACriteria: TtiAutoCriteria): ItiAutoMapSelect<T>;
begin
  FCriteria.Assign(ACriteria);
  Result := Self;
end;

function TtiAutoMapSelectInt<T>.List: TtiObjectList<T>;
var
  LList: TtiSelectListWrapper;
begin
  // Create a temporary criteria to be used for the read so that we don't alter
  // the criteria on the list that Select was called on. Wrapper references
  // the list to populate and has it's own criteria
  LList := TtiSelectListWrapper.Create(FList);
  try
    LList.Criteria.Assign(FCriteria.Criteria);
    LList.Read;
  finally
    LList.Free;
  end;
  // Return the now populated list
  Result := FList;
end;

function TtiAutoMapSelectInt<T>.IList: ItiSmartPointer<TtiObjectList<T>>;
begin
  result := TtiSmartPointer<TtiObjectList<T>>.Create(List);
end;
{$ENDIF}

{ TtiAutoMapSelectExt<T> }

{$IFDEF DELPHI2010ORABOVE}
function TtiAutoMapSelectExt<T>.OrderBy(AField: string;
  AAscending: Boolean): ItiAutoMapSelect<T>;
begin
  FCriteria.Criteria.AddOrderBy(AField, AAscending);
  Result := Self;
end;

function TtiAutoMapSelectExt<T>.OrderBy(AFields: TArrayOfString;
  AAscending: Boolean): ItiAutoMapSelect<T>;
begin
  FCriteria.Criteria.AddOrderBy(AFields, AAscending);
  Result := Self;
end;

function TtiAutoMapSelectExt<T>.Where(const ACriteria: TtiAutoCriteria): ItiAutoMapSelect<T>;
begin
  FCriteria.Assign(ACriteria);
  Result := Self;
end;

function TtiAutoMapSelectExt<T>.List: TtiObjectList<T>;
var
  LList: TtiFilteredObjectList<T>;
begin
  LList := TtiFilteredObjectList<T>.Create;
  try
    LList.Criteria.Assign(FCriteria.Criteria);
    LList.Read;
  except
    LList.Free;
    raise;
  end;
  Result := LList;
end;

function TtiAutoMapSelectExt<T>.IList: ItiSmartPointer<TtiObjectList<T>>;
begin
  result := TtiSmartPointer<TtiObjectList<T>>.Create(List);
end;
{$ENDIF}

end.

