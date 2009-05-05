unit tiGenericList;

interface

uses
  SysUtils
  ,tiGenericBase
  ,tiObject
  , tiBaseObject
  , Generics.Collections;

type

  TtiGenericObjectList<T: TtiObject> = class(TtiObjectList)
  protected
    function GetItems(i:integer): T; reintroduce;
    procedure SetItems(i:integer; const Value: T); reintroduce;

    // hack to cast generic to object and back again
    // as delphi doesn't like the
    class function GenericAsObject(const Value): TObject;
    class function ObjectAsGeneric(const Value): T;
  public
    function Add(const AObject: T): integer; reintroduce;

    function  Last : T; reintroduce;
    function  First : T; reintroduce;
    function  FirstExcludeDeleted : T; reintroduce;
    function  LastExcludeDeleted : T; reintroduce;
    procedure Insert(const AIndex: integer; const AObject: T); overload;
    procedure Insert(const AInsertBefore: T; const AObject: T); overload;
    function  GetEnumerator: TtiGenericEnumerator<T>; reintroduce;
    function  FilteredEnumerator(APredicate: TPredicate<T>): TGenericEnumeratorHost<T>;

    property Items[i:integer]: T read GetItems write SetItems;
        default;
      end;

implementation

{ TtiGenericObjectList<T> }


function TtiGenericObjectList<T>.Add(const AObject: T): integer;
var obj: TtiObject;
begin
  obj:= TtiObject(GenericAsObject(AObject));
  result:= inherited Add(obj);
  // replaces the following which gets overload errors
//   result:= inherited Add(TtiObject(AObject));
end;

function TtiGenericObjectList<T>.FilteredEnumerator(APredicate: TPredicate<T>): TGenericEnumeratorHost<T>;
begin
  result:= TGenericEnumeratorHost<T>.Create(self, APredicate);
end;

function TtiGenericObjectList<T>.First: T;
var obj: TtiObject;
begin
  obj:= inherited First;
  result:= ObjectAsGeneric(obj);
end;

function TtiGenericObjectList<T>.FirstExcludeDeleted: T;
var obj: TtiObject;
begin
  obj:= inherited FirstExcludeDeleted;
  result:= ObjectAsGeneric(obj);
end;

class function TtiGenericObjectList<T>.GenericAsObject(const Value): TObject;
begin
  Result := TObject(Value);
end;

function TtiGenericObjectList<T>.GetEnumerator: TtiGenericEnumerator<T>;
begin
  Result := TtiGenericEnumerator<T>.Create(Self);
end;

function TtiGenericObjectList<T>.GetItems(i: integer): T;
var obj: TtiObject;
begin
  obj:= inherited GetItems(i);
  result:= ObjectAsGeneric(obj);
  // replaces the following which gets "Invalid typecast" errors
  // result:= inherited Add(AObject);
end;

procedure TtiGenericObjectList<T>.Insert(const AIndex: integer;
  const AObject: T);
begin
  inherited Insert(AIndex, TtiObject(GenericAsObject(AObject)));
end;

procedure TtiGenericObjectList<T>.Insert(const AInsertBefore: T;
  const AObject: T);
begin
  inherited Insert(TtiObject(GenericAsObject(AInsertBefore)), TtiObject(GenericAsObject(AObject)));
end;

function TtiGenericObjectList<T>.Last: T;
var obj: TtiObject;
begin
  obj:= inherited Last;
  result:= ObjectAsGeneric(obj);
end;

function TtiGenericObjectList<T>.LastExcludeDeleted: T;
var obj: TtiObject;
begin
  obj:= inherited LastExcludeDeleted;
  result:= ObjectAsGeneric(obj);
end;

class function TtiGenericObjectList<T>.ObjectAsGeneric(const Value): T;
begin
  Result := T(Value);
end;

procedure TtiGenericObjectList<T>.SetItems(i: integer; const Value: T);
begin
  inherited SetItems(i, Value);
end;

end.
