
{$IFDEF read_interface}

    { Dunit compatable interface for FPCunit and Free Pascal }

    {$IF Defined(VER2_0_4) or Defined(VER2_0_5)} //  or Defined(VER2_1_1)
    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: boolean; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = ''); overload; virtual;
    procedure FailNotEquals(expected, actual: string; msg: string = ''; errorAddr: Pointer = nil); virtual;
    {$ENDIF}
    procedure CheckTrue(condition: Boolean; msg: string = ''); virtual;
    procedure CheckFalse(condition: Boolean; msg: string = ''); virtual;

    class function Suite: TTest;
    {
    *** TODO  ***
    procedure CheckEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckNotEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotNull(obj :IUnknown; msg :string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; msg: string = ''); overload; virtual;

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string = '');
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload; virtual;
    }

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$IF Defined(VER2_0_4) or Defined(VER2_0_5)} //  or Defined(VER2_1_1)
procedure TtiTestCase.CheckNotEquals(expected, actual: integer; msg: string);
begin
  if (expected = actual) then
    Fail(msg + ComparisonMsg(IntToStr(expected), IntToStr(actual)));
end;

procedure TtiTestCase.CheckNotEquals(expected, actual: boolean; msg: string);
begin
  if (expected = actual) then
    Fail(msg + ComparisonMsg(BoolToStr(expected), BoolToStr(actual)));
end;

procedure TtiTestCase.CheckNotEquals(expected: extended; actual: extended;
  delta: extended; msg: string);
begin
  if (abs(expected-actual) <= delta) then
      FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, nil);
end;

procedure TtiTestCase.FailNotEquals(expected, actual: string; msg: string;
  errorAddr: Pointer);
begin
  Fail(msg + ComparisonMsg(Expected, Actual));
end;
{$ENDIF}

procedure TtiTestCase.CheckTrue(condition: Boolean; msg: string);
begin
  if (not condition) then
      FailNotEquals(BoolToStr(true, true), BoolToStr(false, true), msg, nil);
end;

procedure TtiTestCase.CheckFalse(condition: Boolean; msg: string);
begin
  if (condition) then
      FailNotEquals(BoolToStr(false, true), BoolToStr(true, true), msg, nil);
end;

class function TtiTestCase.Suite: TTest;
begin
  result := TTestSuite.Create(self);
end;


{$ENDIF read_implementation}

