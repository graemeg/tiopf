unit tiNullable;

// Based on a blog post by Allen Bauer
// http://blogs.embarcadero.com/abauer/2008/09/18/38869

interface

uses Generics.Defaults, SysUtils;

type
  TtiNullable<T> = record
  private
    FValue: T;
    FHasValue: IInterface;
    function GetValue: T;
    function GetHasValue: Boolean;
  public
    constructor Create(AValue: T);
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(Default: T): T; overload;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;

    class operator NotEqual(ALeft, ARight: TtiNullable<T>): Boolean;
    class operator Equal(ALeft, ARight: TtiNullable<T>): Boolean;

    class operator Implicit(Value: TtiNullable<T>): T;
    class operator Implicit(Value: T): TtiNullable<T>;
    class operator Explicit(Value: TtiNullable<T>): T;
  end;

procedure SetFlagInterface(var Intf: IInterface);

implementation

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

const
  FlagInterfaceVTable: array[0..2] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease
  );

  FlagInterfaceInstance: Pointer = @FlagInterfaceVTable;

procedure SetFlagInterface(var Intf: IInterface);
begin
  Intf := IInterface(@FlagInterfaceInstance);
end;

{ Nullable<T> }

constructor TtiNullable<T>.Create(AValue: T);
begin
  FValue := AValue;
  SetFlagInterface(FHasValue);
end;

class operator TtiNullable<T>.Equal(ALeft, ARight: TtiNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end else
    Result := ALeft.HasValue = ARight.HasValue;
end;

class operator TtiNullable<T>.Explicit(Value: TtiNullable<T>): T;
begin
  Result := Value.Value;
end;

function TtiNullable<T>.GetHasValue: Boolean;
begin
  Result := FHasValue <> nil;
end;

function TtiNullable<T>.GetValue: T;
begin
  if not HasValue then
    raise Exception.Create('Invalid operation, Nullable type has no value');
  Result := FValue;
end;

function TtiNullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default(T);
end;

function TtiNullable<T>.GetValueOrDefault(Default: T): T;
begin
  if not HasValue then
    Result := Default
  else
    Result := FValue;
end;

class operator TtiNullable<T>.Implicit(Value: TtiNullable<T>): T;
begin
  Result := Value.Value;
end;

class operator TtiNullable<T>.Implicit(Value: T): TtiNullable<T>;
begin
  Result := TtiNullable<T>.Create(Value);
end;

class operator TtiNullable<T>.NotEqual(ALeft, ARight: TtiNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := not Comparer.Equals(ALeft.Value, ARight.Value);
  end else
    Result := ALeft.HasValue <> ARight.HasValue;
end;

end.
