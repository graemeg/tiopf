{%MainUnit tiTestFramework.pas}

function TtiTestCase.GetName: string;
begin
  Result := TestName;
end;

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

function TtiTestCase.EqualsErrorMessage(const expected, actual: String;
  const ErrorMsg: string): String;
begin
  if (ErrorMsg <> '') then
    Result := Format(sMsgActualEqualsExpFmt, [ErrorMsg + ', ', expected, actual])
  else
    Result := Format(sActualEqualsExpFmt, [expected, actual])
end;

function TtiTestCase.NotEqualsErrorMessage(const expected, actual: String;
  const ErrorMsg: string): String;
begin
  if (ErrorMsg <> '') then
    Result := Format(sExpectedButWasAndMessageFmt, [ErrorMsg, expected, actual])
  else
    Result := Format(sExpectedButWasFmt, [expected, actual]);
end;

procedure TtiTestCase.InhibitStackTrace;
begin
  // do nothing
end;

procedure TtiTestCase.SetAllowedLeakArray(const AllowedList: array of Integer);
begin
  // do nothing
end;


