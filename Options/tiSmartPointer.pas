unit tiSmartPointer;

{
This is based on the implementation of Barry Kelly's smart pointer class:
  http://blog.barrkel.com/2008/09/smart-pointers-in-delphi.html
  http://blog.barrkel.com/2008/11/reference-counted-pointers-revisited.html
  http://blog.barrkel.com/2008/11/somewhat-more-efficient-smart-pointers.html

It includes an enhancement to allow easier use when managing a new object
instance that is created using its parameterless constructor.
The smart pointer class has also been changed to call the base class
constructor and destructor for future compatibility.

Example Usage:

  TPerson = class
  public
    constructor Create(const AName: string; const AAge: Integer); reintroduce;
    procedure Birthday; // Increment Age
    property Name: string ...
    property Age: integer ...
  end;

  // Smart pointer param
  procedure ShowName(APerson: ISmartPointer<TPerson>);
  // TPerson param
  procedure ShowAge(APerson: TPerson);

var
  Person1: ItiSmartPointer<TPerson>;
  Person2: ItiSmartPointer<TPerson>;
  Person3: ItiSmartPointer<TPerson>;
  PersonObj: TPerson;
begin
  // Typical usage when creating a new object to manage
  Person1 := TtiSmartPointer<TPerson>.Create(TPerson.Create('Fred', 100));
  Person1.Birthday; // Direct member access!
  ShowName(Person1); // Pass as smart pointer
  ShowAge(Person1); // Pass as the managed object!
  //Person1 := nil; // Release early

  // Same as above but hand over to smart pointer later
  PersonObj := TPerson.Create('Wilma', 90);
  // Later
  Person2 := TtiSmartPointer<TPerson>.Create(PersonObj);
  ShowName(Person2);
  // Don't free PersonObj!

  // Smart pointer constructs the TPerson instance
  Person3 := TtiSmartPointer<TPerson>.Create(); // or Create(nil)
end;
}

interface

uses
  SysUtils
  ;

type
  ItiSmartPointer<T> = reference to function: T;

  TtiSmartPointer<T: class, constructor> = class(TInterfacedObject, ItiSmartPointer<T>)
  private
    FValue: T;
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    destructor Destroy; override;
    function Invoke: T;
  end;

  RtiSmartPointer<T: class, constructor> = record
  strict private
    FPointer: ItiSmartPointer<T>;
  public
    class operator Implicit(Value: T): RtiSmartPointer<T>;
    class operator Implicit(const Value: RtiSmartPointer<T>): ItiSmartPointer<T>;
  end;

implementation

{ TtiSmartPointer<T> }

constructor TtiSmartPointer<T>.Create;
begin
  inherited;
  FValue := T.Create;
end;

constructor TtiSmartPointer<T>.Create(AValue: T);
begin
  inherited Create;
  if AValue = nil then
    FValue := T.Create
  else
    FValue := AValue;
end;

destructor TtiSmartPointer<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TtiSmartPointer<T>.Invoke: T;
begin
  Result := FValue;
end;

{ RtiSmartPointer<T> }

class operator RtiSmartPointer<T>.Implicit(Value: T): RtiSmartPointer<T>;
begin
  Result.FPointer := TtiSmartPointer<T>.Create(Value);
end;

class operator RtiSmartPointer<T>.Implicit(const Value: RtiSmartPointer<T>): ItiSmartPointer<T>;
begin
  Result := Value.FPointer;
end;

end.

