{
  Helper functions to fake the DUnit methods. This well prevent more
  IFDEF statements.
  
  NOTE:
    This unit is meant for FPCUnit only!!!  DUnit already supports these
    features.
}
unit tiFPCUnitUtils;

{$I tiDefines.inc}

interface
uses
  fpcunit
  ,TestRegistry
  ,Classes
  ;


procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass); overload;

implementation


procedure RegisterTestInSuite(rootSuite: TTestSuite; APath: string; ATestClass: TTestCaseClass);
var
  i: Integer;
  targetSuite: TTestSuite;
  currentTest: TTest;
  suiteName: String;
  pathRemainder: String;
  dotPos: Integer;
  Tests: TFPList;
begin
  if APath = '' then
  begin
    // end recursion
    rootSuite.AddTestSuiteFromClass(ATestClass);
  end
  else
  begin
    // Split the path on the dot (.)
    dotPos := Pos('.', APath);
    if (dotPos <= 0) then dotPos := Pos('\', APath);
    if (dotPos <= 0) then dotPos := Pos('/', APath);
    if (dotPos > 0) then
    begin
      suiteName := Copy(APath, 1, dotPos - 1);
      pathRemainder := Copy(APath, dotPos + 1, length(APath) - dotPos);
    end
    else
    begin
      suiteName := APath;
      pathRemainder := '';
    end;
    Tests := rootSuite.Tests;

    // Check to see if the path already exists
    targetSuite := nil;
    Tests := rootSuite.Tests;
    for i := 0 to Tests.Count -1 do
    begin
      currentTest := TTest(Tests[i]);
      if currentTest is TTestSuite then
      begin
        if (currentTest.TestName = suiteName) then
        begin
          targetSuite := TTestSuite(currentTest);
          break;
        end;
      end;
    end;

    if not Assigned(targetSuite) then
    begin
      targetSuite := TTestSuite.Create(suiteName);
      rootSuite.AddTest(targetSuite);
    end;

    RegisterTestInSuite(targetSuite, pathRemainder, ATestClass);
  end;
end;

procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass);
begin
  RegisterTestInSuite(GetTestRegistry, ASuitePath, ATestClass);
end;


initialization
  GetTestRegistry.TestName := 'tiOPF2 Unit Tests';

end.

