unit tiGenericBase;

{$I tiDefines.inc}

interface

uses
  tiObject
  ,tiBaseObject
  ,SysUtils
  ,Generics.Collections;

type
  TtiGenericEnumerator<T: TtiObject> = record
  private
    FIndex: Integer;
    FList: TtiObjectList;
    function ObjectAsGeneric(const Value): T;
  public
    constructor Create(AList: TtiObjectList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  TtiFilteredGenericEnumerator<T: TtiObject> = class  // DO NOT CONVERT RECORD BASED, must be class based to avoide compiler error
  private
    FIndex: Integer;
    FList: TtiObjectList;
    FPredicate: TPredicate<T>;
    function ObjectAsGeneric(const Value): T;
  public
    constructor Create(AList: TtiObjectList; APredicate: TPredicate<T>);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  TGenericEnumeratorHost<T: TtiObject> = record
  private
    FEnumerator: TtiFilteredGenericEnumerator<T> ;
  public
    constructor Create(AList: TtiObjectList; APredicate: TPredicate<T>);
    function    GetEnumerator: TtiFilteredGenericEnumerator<T>; reintroduce;
  end;

implementation

{ TtiGenericEnumerator<T> }

constructor TtiGenericEnumerator<T>.Create(AList: TtiObjectList);
begin
//  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TtiGenericEnumerator<T>.GetCurrent: T;
var obj: TtiObject;
begin
  obj:= FList[FIndex];
  Result := ObjectAsGeneric(obj);
end;

function TtiGenericEnumerator<T>.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

function TtiGenericEnumerator<T>.ObjectAsGeneric(const Value): T;
begin
  Result := T(Value);
end;


{ TtiFilteredGenericEnumerator<T> }

constructor TtiFilteredGenericEnumerator<T>.Create(AList: TtiObjectList;
  APredicate: TPredicate<T>);
begin
//  inherited Create;
  FIndex := -1;
  FList:= AList;
  FPredicate:= APredicate;
end;

function TtiFilteredGenericEnumerator<T>.GetCurrent: T;
var obj: TtiObject;
begin
  obj:= FList[FIndex];
  Result := ObjectAsGeneric(obj);
end;

function TtiFilteredGenericEnumerator<T>.MoveNext: Boolean;
begin
  if FIndex >= FList.Count -1 then
    exit(false);

  repeat
    inc(FIndex);
    if FIndex >= FList.Count then
      exit(false);

    if FPredicate(GetCurrent) then
      exit(true);
  until (false);


end;

function TtiFilteredGenericEnumerator<T>.ObjectAsGeneric(const Value): T;
begin
  Result := T(Value);
end;

{ TGenericEnumeratorHost<T> }

constructor TGenericEnumeratorHost<T>.Create(AList: TtiObjectList; APredicate: TPredicate<T>);
begin
  FEnumerator:= TtiFilteredGenericEnumerator<T>.Create(AList, APredicate);
end;

function TGenericEnumeratorHost<T>.GetEnumerator: TtiFilteredGenericEnumerator<T>;
begin
  result:= TtiFilteredGenericEnumerator<T>(FEnumerator);
end;

end.
