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

{$I tiDefines.inc}

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

  type
    // A very simple garbage collector
    ItiGC = interface
      ['{53FF38BA-BCBA-45D1-AE64-80BF5C738FEE}']
      // Add object named AName to garbage collector
      // AValue is constructor call for AName
      function Add(out AName; const AValue: TObject): boolean; overload;
      function Add(const AValue: TObject): boolean; overload;
      function Remove(const AValue: TObject): boolean;
    end;

  function CreateGC: ItiGC;

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


type
    // A very simple, low-ink, un-thread-safe garbage collector
    TtiGC = class (TInterfacedObject, ItiGC)
    private
      FCapacity, FCount: integer;
      FObjects: array of TObject;
      procedure CheckCapacity;
    public
      function Add(out AName; const AValue: TObject): boolean; overload;
      function Add(const AValue: TObject): boolean; overload;
      function Remove(const AValue: TObject): boolean;
      destructor Destroy; override;
    end;

  function CreateGC: ItiGC;
  begin
    Result := TtiGC.Create;
  end;

{ TtiGC }

function TtiGC.Add(out AName; const AValue: TObject): boolean;
begin
  Result := Add(AValue);
  TObject(AName) := AValue;
end;

function TtiGC.Add(const AValue: TObject): boolean;
var
  i: integer;
  duplicate: boolean;
begin
  duplicate := false;
  for i := 0 to FCount - 1 do
    if FObjects[i] = AValue then duplicate := true;
  if not duplicate then
  begin
    CheckCapacity;
    FObjects[FCount] := AValue;
    Inc(FCount);
  end;
  Result := not duplicate;
end;

destructor TtiGC.Destroy;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
   FObjects[i].Free;
 inherited;
end;

function TtiGC.Remove(const AValue: TObject): boolean;
var
  i,hi: integer;
begin
  Result := false;
  hi := FCount - 1;
  if Assigned(AValue) then
  for i := 0 to hi do
    if AValue = FObjects[i] then
    begin
      if i < hi then
        // Move remainder of FObjects down one slot to cover removed AValue
        Move(FObjects[i+1], FObjects[i], (hi-i)*SizeOf(TObject));
      Dec(FCount);
      // Prune on lucky 7!
      if (FCount mod 7) = 0 then
      begin
        SetLength(FObjects, FCount);
        FCapacity := FCount;
      end;
      Exit(true);
    end;
end;

procedure TtiGC.CheckCapacity;
var
  delta: Integer;
begin
  if FCount = FCapacity then
  begin
    if FCapacity > 64 then
      delta := FCapacity div 4
    else
      if FCapacity > 8 then
        delta := 16
      else
        delta := 4;

    Inc(FCapacity, delta);
    SetLength(FObjects, FCapacity);
  end;
end;

end.

