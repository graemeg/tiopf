

//------------------------------------------------------------------------------
// Used for compatability with DUnit test formats!
//------------------------------------------------------------------------------
{function TtiTestCase.GetName: string;
begin
  Result := TestName;
end;
}

{$IFDEF VER2_0_2}
procedure TtiTestCase.Check(AValue: boolean; AMessage: string);
begin
  AssertTrue(AMessage, AValue);
end;


procedure TtiTestCase.CheckEquals(expected, actual: extended; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;


procedure TtiTestCase.CheckEquals(expected, actual: string; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;


procedure TtiTestCase.CheckEquals(expected, actual: extended; delta: extended;
  msg: string);
begin
  AssertEquals(msg, expected, actual, delta);
end;


procedure TtiTestCase.CheckEquals(expected, actual: integer; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;


procedure TtiTestCase.CheckEquals(expected, actual: boolean; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;


procedure TtiTestCase.CheckEquals(expected, actual: TClass; msg: string);
begin
  AssertEquals(msg, expected, actual);
end;


procedure TtiTestCase.CheckNotEquals(expected, actual: string; msg: string);
begin
  if AnsiCompareStr(Expected, Actual) = 0 then
    Fail(msg + ComparisonMsg(Expected, Actual));
end;


procedure TtiTestCase.CheckNull(obj: IUnknown; msg: string);
begin
  AssertNullIntf(msg, obj);
end;


procedure TtiTestCase.CheckNull(obj: TObject; msg: string);
begin
  AssertNull(msg, obj);
end;


procedure TtiTestCase.CheckNotNull(obj: TObject; msg: string);
begin
  AssertNotNull(msg, obj);
end;


procedure TtiTestCase.CheckIs(obj: TObject; AClass: TClass; msg: string);
begin
  Assert(AClass <> nil);
  if obj = nil then
    Fail(ComparisonMsg(AClass.ClassName, 'nil'))
  else if not obj.ClassType.InheritsFrom(AClass) then
    Fail(ComparisonMsg(AClass.ClassName, obj.ClassName));
end;


procedure TtiTestCase.CheckSame(expected, actual: TObject; msg: string);
begin
   AssertSame(msg, expected, actual);
end;
{$ENDIF}


{$IF Defined(VER2_0_4) or Defined(VER2_1_1)}
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


