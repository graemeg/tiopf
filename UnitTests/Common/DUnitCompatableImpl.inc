{%MainUnit DUnitCompatable.pp}

//------------------------------------------------------------------------------
// Used for compatability with DUnit test formats!
//------------------------------------------------------------------------------
function TtiTestCase.GetName: string;
begin
  Result := TestName;
end;


procedure TtiTestCase.Check(pValue: boolean; pMessage: string);
begin
  AssertTrue(pMessage, pValue);
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


procedure TtiTestCase.CheckIs(obj: TObject; klass: TClass; msg: string);
begin
  Assert(klass <> nil);
  if obj = nil then
    Fail(ComparisonMsg(klass.ClassName, 'nil'))
  else if not obj.ClassType.InheritsFrom(klass) then
    Fail(ComparisonMsg(klass.ClassName, obj.ClassName));
end;


procedure TtiTestCase.CheckSame(expected, actual: TObject; msg: string);
begin
   AssertSame(msg, expected, actual);
end;

