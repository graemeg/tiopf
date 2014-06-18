unit tiObjectError_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFramework
  ,Classes
 ;

type

  TTestTIObjectError = class (TtiTestCase)
  published
    procedure TestAdd;
  end;

procedure RegisterTests;

implementation
uses
   SysUtils
  ,tiObject
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIObjectError);
end;


{ TTestTIObjectError }

procedure TTestTIObjectError.TestAdd;
var
  LErrors: TtiObjectErrors;
  LError: TtiObjectError;
begin
  GC.Add(LErrors, TtiObjectErrors.Create);
  CheckEquals(0, LErrors.Count, 'initially empty');
  LErrors.AddError('error 1');
  CheckEquals(1, LErrors.Count, 'error 1');
  LErrors.AddError('error 1');
  CheckEquals(1, LErrors.Count, 'error 1 re-added - fails uniqueness test');
  LErrors.AddError('error 1', 'non-empty message');
  CheckEquals(2, LErrors.Count, 'error 1 re-added with non-empty message');
  LErrors.AddError('error 1', 'non-empty message', 1);
  CheckEquals(3, LErrors.Count, 'error 1 re-added with non-empty message and non-zero code');
  LErrors.AddError('error 1', 'non-empty message', 1);
  CheckEquals(3, LErrors.Count, 'error 1 re-added with non-empty message and non-zero code - fails uniqueness');
  LErrors.AddError('error 1', 'non-empty message');
  CheckEquals(3, LErrors.Count, 'error 1 re-added with non-empty message - fails uniqueness');
end;

end.


