{
    A very basic test listener. It simply outputs the name of the
    test it is about to run. This is handly for debugging the tests
    to see the name of the test that maybe freezes up the program etc.

}
unit textprogressrunner;

{$I tiDefines.inc}

interface

uses
  Classes, SysUtils, TestFrameworkProxyIfaces;

type
    TTextProgressTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
    private
      // IStatusListener
      procedure Status(const ATest: ITestProxy; AMessage: string);
      // ITestListener
      procedure AddSuccess(Test: ITestProxy);
      procedure AddError(Error: TTestFailure);
      procedure AddFailure(Failure: TTestFailure);
      procedure AddWarning(AWarning: TTestFailure);
      procedure TestingStarts;
      procedure StartTest(Test: ITestProxy);
      procedure EndTest(Test: ITestProxy);
      procedure TestingEnds(TestResult: ITestResult);
      function  ShouldRunTest(const ATest: ITestProxy):Boolean;
      // ITestListenerX
      procedure StartSuite(Suite: ITestProxy);
      procedure EndSuite(Suite: ITestProxy);
    end;

implementation

{ TTextProgressTestListener }

procedure TTextProgressTestListener.Status(const ATest: ITestProxy; AMessage: string);
begin

end;

procedure TTextProgressTestListener.AddSuccess(Test: ITestProxy);
begin

end;

procedure TTextProgressTestListener.AddError(Error: TTestFailure);
begin

end;

procedure TTextProgressTestListener.AddFailure(Failure: TTestFailure);
begin

end;

procedure TTextProgressTestListener.AddWarning(AWarning: TTestFailure);
begin

end;

procedure TTextProgressTestListener.TestingStarts;
begin

end;

procedure TTextProgressTestListener.StartTest(Test: ITestProxy);
begin
  if Test.IsTestMethod then
    write(Test.Name);
end;

procedure TTextProgressTestListener.EndTest(Test: ITestProxy);
begin
  WriteLn();
end;

procedure TTextProgressTestListener.TestingEnds(TestResult: ITestResult);
begin

end;

function TTextProgressTestListener.ShouldRunTest(const ATest: ITestProxy): Boolean;
begin
  Result := not ATest.Excluded;
end;

procedure TTextProgressTestListener.StartSuite(Suite: ITestProxy);
begin
  WriteLn('===[ ' + Suite.Name + ' ]===');
end;

procedure TTextProgressTestListener.EndSuite(Suite: ITestProxy);
begin

end;

end.

