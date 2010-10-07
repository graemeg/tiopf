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
    TTextProgressTestListener = class(TInterfacedObject, ITestListener)
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
      procedure TestingEnds(TestResult: TTestResult);
      function  ShouldRunTest(const ATest :ITestProxy):Boolean;
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
  writeln('Testing Starts ==================================');
end;

procedure TTextProgressTestListener.StartTest(Test: ITestProxy);
begin
  if Test.IsTestMethod then
    writeln(Test.Name);
end;

procedure TTextProgressTestListener.EndTest(Test: ITestProxy);
begin

end;

procedure TTextProgressTestListener.TestingEnds(TestResult: TTestResult);
begin

end;

function TTextProgressTestListener.ShouldRunTest(const ATest: ITestProxy): Boolean;
begin

end;

end.

