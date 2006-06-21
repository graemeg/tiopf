(******************************************************************************
  mtCheck.pas Version 1.2

  A unit to simplify the writing of pre and post-conditions
  by Malcolm Groves (malcolm@madrigal.com.au)

  © 2001, Madrigal Technologies Pty Ltd. http://www.madrigal.com.au
  Feel free to use this code in your own projects. Any changes you make which you
  think may benefit other developers, please forward to malcolm@madrigal.com.au
  for inclusion.

  A paper on the use of this class can be found at:
    http://www.madrigal.com.au/papers/check/page1.htm
******************************************************************************)

unit mtCheck;

interface
uses
  Classes, SysUtils;
type
  EmtCheckFailed = class(Exception)
  end;
  TmtCheck = class
  protected
    class procedure RaiseError(const ValueDescr, TestDescr, MethodName : String); overload;
    class function BuildDescr(const ValueDescr : string; Value : Integer) : String; overload;
    class function BuildDescr(const ValueDescr : string; Value : Double) : String; overload;
    class function BuildDescr(const ValueDescr : string; const Value : String) : String; overload;
    class function BuildMethodName(const MethodName : string): String;
  public
    // boolean tests
    class procedure IsTrue(Value : Boolean; const ValueDescr : String = '';
                           const MethodName : String = '');
    class procedure IsFalse(Value : Boolean; const ValueDescr : String = '';
                            const MethodName : String = '');
    // Integer Tests
    class procedure Equals(Value : Integer; ComparisonValue : Integer;
                           const ValueDescr : String = '';
                           const ComparisonValueDescr : String = '';
                           const MethodName : String = ''); overload;
    class procedure NotEquals(Value : Integer; ComparisonValue : Integer;
                              const ValueDescr : String = '';
                              const ComparisonValueDescr : String = '';
                              const MethodName : String = ''); overload;
    class procedure IsLessThan(Value : Integer; ComparisonValue : Integer;
                               const ValueDescr : String = '';
                               const ComparisonValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsLessThanOrEqualTo(Value : Integer; ComparisonValue : Integer;
                                        const ValueDescr : String = '';
                                        const ComparisonValueDescr : String = '';
                                        const MethodName : String = ''); overload;
    class procedure IsGreaterThan(Value : Integer; ComparisonValue : Integer;
                                  const ValueDescr : String = '';
                                  const ComparisonValueDescr : String = '';
                                  const MethodName : String = ''); overload;
    class procedure IsGreaterThanOrEqualTo(Value : Integer; ComparisonValue : Integer;
                                           const ValueDescr : String = '';
                                           const ComparisonValueDescr : String = '';
                                           const MethodName : String = ''); overload;
    class procedure IsBetween(Value : Integer; LowComparisonValue : Integer;
                              HighComparisonValue : Integer;
                              const ValueDescr : String = '';
                              const LowComparisonValueDescr : String = '';
                              const HighComparisonValueDescr : String = '';
                              const MethodName : String = ''); overload;
    class procedure IsBetweenOrEqualTo(Value : Integer; LowComparisonValue : Integer;
                                       HighComparisonValue : Integer;
                                       const ValueDescr : String = '';
                                       const LowComparisonValueDescr : String = '';
                                       const HighComparisonValueDescr : String = '';
                                       const MethodName : String = ''); overload;
    class procedure IsPositive(Value : Integer; const ValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsNegative(Value : Integer; const ValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsZero(Value : Integer; const ValueDescr : String = '';
                           const MethodName : String = ''); overload;
    class procedure IsNotZero(Value : Integer; const ValueDescr : String = '';
                              const MethodName : String = ''); overload;
    // double tests
    class procedure Equals(Value : Double; ComparisonValue : Double;
                           const ValueDescr : String = '';
                           const ComparisonValueDescr : String = '';
                           const MethodName : String = ''); overload;
    class procedure NotEquals(Value : Double; ComparisonValue : Double;
                              const ValueDescr : String = '';
                              const ComparisonValueDescr : String = '';
                              const MethodName : String = ''); overload;
    class procedure IsLessThan(Value : Double; ComparisonValue : Double;
                               const ValueDescr : String = '';
                               const ComparisonValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsLessThanOrEqualTo(Value : Double; ComparisonValue : Double;
                                        const ValueDescr : String = '';
                                        const ComparisonValueDescr : String = '';
                                        const MethodName : String = ''); overload;
    class procedure IsGreaterThan(Value : Double; ComparisonValue : Double;
                                  const ValueDescr : String = '';
                                  const ComparisonValueDescr : String = '';
                                  const MethodName : String = ''); overload;
    class procedure IsGreaterThanOrEqualTo(Value : Double; ComparisonValue : Double;
                                           const ValueDescr : String = '';
                                           const ComparisonValueDescr : String = '';
                                           const MethodName : String = ''); overload;
    class procedure IsBetween(Value : Double; LowComparisonValue : Double;
                              HighComparisonValue : Double;
                              const ValueDescr : String = '';
                              const LowComparisonValueDescr : String = '';
                              const HighComparisonValueDescr : String = '';
                              const MethodName : String = ''); overload;
    class procedure IsBetweenOrEqualTo(Value : Double; LowComparisonValue : Double;
                                       HighComparisonValue : Double;
                                       const ValueDescr : String = '';
                                       const LowComparisonValueDescr : String = '';
                                       const HighComparisonValueDescr : String = '';
                                       const MethodName : String = ''); overload;
    class procedure IsPositive(Value : Double; const ValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsNegative(Value : Double; const ValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsZero(Value : Double; const ValueDescr : String = '';
                           const MethodName : String = ''); overload;
    class procedure IsNotZero(Value : Double; const ValueDescr : String = '';
                              const MethodName : String = ''); overload;
    // object tests
    class procedure IsDescendantOf(Value : TObject; ComparisonValue : TClass;
                                   const ValueDescr : String = '';
                                   const MethodName : String = ''); overload;
    class procedure IsNotDescendantOf(Value : TObject; ComparisonValue : TClass;
                                      const ValueDescr : String = '';
                                      const MethodName : String = ''); overload;
    class procedure IsInstanceOf(Value : TObject; ComparisonValue : TClass;
                                 const ValueDescr : String = '';
                                 const MethodName : String = ''); overload;
    class procedure IsNotInstanceOf(Value : TObject; ComparisonValue : TClass;
                                    const ValueDescr : String = '';
                                    const MethodName : String = ''); overload;
    class procedure IsAssigned(Value : TObject;
                               const ValueDescr : String = '';
                               const MethodName : String = ''); overload;
    class procedure IsNotAssigned(Value : TObject;
                                  const ValueDescr : String = '';
                                  const MethodName : String = ''); overload;
    class procedure IsSameInstance(Value : TObject; ComparisonValue : TObject;
                                   const ValueDescr : String = '';
                                   const ComparisonValueDescr : String = '';
                                   const MethodName : String = ''); overload;
    class procedure IsNotSameInstance(Value : TObject; ComparisonValue : TObject;
                                      const ValueDescr : String = '';
                                      const ComparisonValueDescr : String = '';
                                      const MethodName : String = ''); overload;
    // string tests
    class procedure IsNotEmpty(const Value : String;
                               const ValueDescr : String = '';
                               const Methodname : String = '');
    class procedure IsEmpty(const Value : String;
                            const ValueDescr : String = '';
                            const Methodname : String = '');
    // misc tests
    class procedure FileExists(const Filename : String;
                               const FilenameDescr : String = '';
                               const MethodName : String = '');
  end;
  Check = TmtCheck;
implementation

{ TmtCheck }

class function TmtCheck.BuildDescr(const ValueDescr: string;
  Value: Integer): String;
begin
  Result := ValueDescr + ' (' + IntToStr(Value) + ')';
end;

class function TmtCheck.BuildDescr(const ValueDescr: string;
  Value: Double): String;
begin
  Result := ValueDescr + ' (' + FloatToStr(Value) + ')';
end;

class function TmtCheck.BuildDescr(const ValueDescr,
  Value: String): String;
begin
  Result := ValueDescr + ' (' + Value + ')';
end;

class function TmtCheck.BuildMethodName(const MethodName: string): String;
begin
  if MethodName = '' then
    result := ''
  else
    Result := MethodName + ' : ';
end;

class procedure TmtCheck.Equals(Value: Integer; ComparisonValue : Integer;
                                const ValueDescr : String; const ComparisonValueDescr : String;
                                const MethodName : String);
begin
  if not (Value = ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be Equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.Equals(Value: Double; ComparisonValue : Double;
                                const ValueDescr : String; const ComparisonValueDescr : String;
                                const MethodName : String);
begin
  if not (Value = ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must not be Equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsBetween(Value, LowComparisonValue,
  HighComparisonValue: Integer; const ValueDescr, LowComparisonValueDescr,
  HighComparisonValueDescr, MethodName : String);
begin
  if not ((Value > LowComparisonValue) and (Value < HighComparisonValue)) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be between ' +
               BuildDescr(LowComparisonValueDescr, LowComparisonValue) + ' and ' +
               BuildDescr(HighComparisonValueDescr, HighComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsAssigned(Value: TObject; const ValueDescr,
  MethodName: String);
begin
  if not Assigned(Value) then
    RaiseError(ValueDescr, 'Must be Assigned', MethodName);
end;

class procedure TmtCheck.IsBetween(Value, LowComparisonValue,
  HighComparisonValue: Double; const ValueDescr, LowComparisonValueDescr,
  HighComparisonValueDescr, MethodName: String);
begin
  if not ((Value > LowComparisonValue) and (Value < HighComparisonValue)) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be between ' +
               BuildDescr(LowComparisonValueDescr, LowComparisonValue) + ' and ' +
               BuildDescr(HighComparisonValueDescr, HighComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsBetweenOrEqualTo(Value, LowComparisonValue,
  HighComparisonValue: Integer; const ValueDescr, LowComparisonValueDescr,
  HighComparisonValueDescr, MethodName: String);
begin
  if not ((Value >= LowComparisonValue) and (Value <= HighComparisonValue)) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be between or equal to ' +
               BuildDescr(LowComparisonValueDescr, LowComparisonValue) + ' and ' +
               BuildDescr(HighComparisonValueDescr, HighComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsBetweenOrEqualTo(Value, LowComparisonValue,
  HighComparisonValue: Double; const ValueDescr, LowComparisonValueDescr,
  HighComparisonValueDescr, MethodName: String);
begin
  if not ((Value >= LowComparisonValue) and (Value <= HighComparisonValue)) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be between or equal to ' +
               BuildDescr(LowComparisonValueDescr, LowComparisonValue) + ' and ' +
               BuildDescr(HighComparisonValueDescr, HighComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsDescendantOf(Value: TObject;
  ComparisonValue: TClass; const ValueDescr, MethodName: String);
begin
  if not (Value is ComparisonValue) then
    RaiseError(ValueDescr, 'Must be a descendant of ' + ComparisonValue.ClassName, MethodName);
end;

class procedure TmtCheck.IsFalse(Value: Boolean; const ValueDescr, MethodName: String);
begin
  if Value then
    RaiseError(ValueDescr, 'Must be False', MethodName);
end;

class procedure TmtCheck.IsGreaterThan(Value, ComparisonValue: Integer;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value > ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be greater than ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsGreaterThan(Value, ComparisonValue: Double;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value > ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be greater than ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsGreaterThanOrEqualTo(Value,
  ComparisonValue: Integer; const ValueDescr,
  ComparisonValueDescr, MethodName: String);
begin
  if not (Value >= ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be greater than or equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsGreaterThanOrEqualTo(Value,
  ComparisonValue: Double; const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value >= ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be greater than or equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsLessThan(Value, ComparisonValue: Integer;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value < ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be less than ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsInstanceOf(Value: TObject;
  ComparisonValue: TClass; const ValueDescr, MethodName: String);
begin
  if not (Value.ClassType = ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value.ClassName), 'Must be an instance of ' + ComparisonValue.ClassName, MethodName);
end;

class procedure TmtCheck.IsLessThan(Value, ComparisonValue: Double;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value < ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be less than ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsLessThanOrEqualTo(Value,
  ComparisonValue: Integer; const ValueDescr,
  ComparisonValueDescr, MethodName: String);
begin
  if not (Value <= ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be less than or equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsLessThanOrEqualTo(Value,
  ComparisonValue: Double; const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value <= ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be less than or equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.IsNegative(Value: Integer;
  const ValueDescr, MethodName: String);
begin
  if not (Value < 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be negative', MethodName);
end;

class procedure TmtCheck.IsNegative(Value: Double;
  const ValueDescr, MethodName: String);
begin
  if not (Value < 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be negative', MethodName);
end;

class procedure TmtCheck.IsNotZero(Value: Integer;
  const ValueDescr, MethodName: String);
begin
  if not (Value <> 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must not be equal to zero', MethodName);
end;

class procedure TmtCheck.IsNotAssigned(Value: TObject; const ValueDescr,
  MethodName: String);
begin
  if Assigned(Value) then
    RaiseError(ValueDescr, 'Must not be Assigned', MethodName);
end;

class procedure TmtCheck.IsNotSameInstance(Value, ComparisonValue: TObject;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if Value = ComparisonValue then
    RaiseError(ValueDescr, 'Must not be the same instance as ' +
               ComparisonValueDescr, MethodName);
end;

class procedure TmtCheck.IsNotZero(Value: Double;
  const ValueDescr, MethodName: String);
begin
  if not (Value <> 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must not be equal to zero', MethodName);
end;

class procedure TmtCheck.IsPositive(Value: Integer;
  const ValueDescr, MethodName: String);
begin
  if not (Value > 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be positive', MethodName);
end;

class procedure TmtCheck.IsPositive(Value: Double;
  const ValueDescr, MethodName: String);
begin
  if not (Value > 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be positive', MethodName);
end;

class procedure TmtCheck.IsSameInstance(Value, ComparisonValue: TObject;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value = ComparisonValue) then
    RaiseError(ValueDescr, 'Must be the same instance as ' +
               ComparisonValueDescr, MethodName);
end;

class procedure TmtCheck.IsTrue(Value: Boolean; const ValueDescr, MethodName: String);
begin
  if not Value then
    RaiseError(ValueDescr, 'Must be True', MethodName);
end;

class procedure TmtCheck.IsZero(Value: Integer; const ValueDescr, MethodName: String);
begin
  if not (Value = 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be equal to zero', MethodName);
end;

class procedure TmtCheck.IsZero(Value: Double; const ValueDescr, MethodName: String);
begin
  if not (Value = 0) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must be equal to zero', MethodName);
end;

class procedure TmtCheck.NotEquals(Value, ComparisonValue: Integer;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value <> ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must not be Equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.NotEquals(Value, ComparisonValue: Double;
  const ValueDescr, ComparisonValueDescr, MethodName: String);
begin
  if not (Value <> ComparisonValue) then
    RaiseError(BuildDescr(ValueDescr, Value), 'Must not be Equal to ' +
               BuildDescr(ComparisonValueDescr, ComparisonValue),
               MethodName);
end;

class procedure TmtCheck.RaiseError(const ValueDescr, TestDescr, MethodName: String);
begin
  if ValueDescr = '' then
    raise EmtCheckFailed.Create(BuildMethodName(MethodName) +  'Unnamed Value failed test : ' + TestDescr)
  else
    raise EmtCheckFailed.Create(BuildMethodName(MethodName) +  ValueDescr + ' failed test : ' + TestDescr);
end;


class procedure TmtCheck.IsNotDescendantOf(Value: TObject;
  ComparisonValue: TClass; const ValueDescr, MethodName: String);
begin
  if Value is ComparisonValue then
    RaiseError(BuildDescr(ValueDescr, Value.ClassName), 'Must not be a descendant of ' + ComparisonValue.ClassName, MethodName);
end;

class procedure TmtCheck.IsNotInstanceOf(Value: TObject;
  ComparisonValue: TClass; const ValueDescr, MethodName: String);
begin
  if Value.ClassType = ComparisonValue then
    RaiseError(BuildDescr(ValueDescr, Value.ClassName), 'Must not be an instance of ' + ComparisonValue.ClassName, MethodName);
end;

class procedure TmtCheck.FileExists(const Filename, FilenameDescr,
  MethodName: String);
begin
  IsEmpty(Filename, 'Filename', 'TmtCheck.FileExists');
  if not SysUtils.FileExists(Filename) then
    RaiseError(FilenameDescr, 'File must exist', MethodName);
end;

class procedure TmtCheck.IsEmpty(const Value, ValueDescr,
  Methodname: String);
begin
  if Value <> '' then
    RaiseError(ValueDescr, 'Must be an empty String', MethodName);
end;

class procedure TmtCheck.IsNotEmpty(const Value, ValueDescr,
  Methodname: String);
begin
  if Value = '' then
    RaiseError(ValueDescr, 'Must not be an empty String', MethodName);
end;

end.
