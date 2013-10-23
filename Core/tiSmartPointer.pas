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
    // A very simple, low-ink garbage collector
    TtiGC = class (TInterfacedObject, ItiGC)
    private
      FObjects: array of TObject;
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
  O: TObject;
  duplicate: boolean;
begin
  duplicate := false;
  for O in FObjects do
    if O = AValue then duplicate := true;
  if not duplicate then
  begin
    i := Length(FObjects);
    SetLength(FObjects, i+1);
    FObjects[i] := AValue;
  end;
  Result := duplicate;
end;

destructor TtiGC.Destroy;
var
  O: TObject;
begin
 for O in FObjects do
   O.Free;
 inherited;
end;

function TtiGC.Remove(const AValue: TObject): boolean;
var
  i,hi: integer;
  O: TObject;
  duplicate: boolean;
begin
begin
  Result := false;
  hi := High(FObjects);
  if Assigned(AValue) then
  for i := 0 to hi do
    if AValue = FObjects[i] then
    begin
      if i < hi then
        // Move remainder of FObjects down one slot to cover removed AValue
        Move(FObjects[i+1], FObjects[i], (hi-i)*SizeOf(TObject));
      // Truncate FObjects by 1
      SetLength(FObjects, hi);
      Exit (true);
    end;
end;
end;

end.

