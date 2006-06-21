{
Version History:
  26-Apr 2002   Andrew Cumming                  Move includes to allow D6 compile
   7-Mar 2002   Grahame Grieve                  Total Rewrite of Tests
   4-Jun 2003   Peter Hinrichsen                Cloned from IndySoap to tiOPF
}

unit tiObjAbs_TST;

{$I tiDefines.inc}

{$IFNDEF CLASS_TRACKING}
  Currently, you must have CLASS_TRACKING defined to run the tests
{$ENDIF}

{$IFNDEF OBJECT_TRACKING}
  Currently, you must have OBJECT_TRACKING defined to run the tests
{$ENDIF}

interface

uses
  TestFramework,
  tiObjAbs;

type
  TtiObjAbsTestClass = class (TtiObjAbs)
  end;

  TDebugTestCases = class (TTestCase)
  published
    procedure TestDebug1;
    procedure TestDebug2;
    procedure TestDebug3;
    procedure TestDebug4;
    procedure TestDebug5;
    procedure TestDebug6;
  end;

procedure RegisterTests ;

implementation


var GTest : TtiObjAbsTestClass;

procedure RegisterTests ;
begin
  RegisterTest( 'DebugAbstractClasses', TDebugTestCases.Suite );
end;

procedure TDebugTestCases.TestDebug1;
begin
  GTest := nil;
  check(GTest = nil, 'GTest is not nil');
  check(not GTest.TestValid, 'GTest is nil and valid');
end;

procedure TDebugTestCases.TestDebug2;
begin
  GTest := TtiObjAbsTestClass(Random(20000)+2000);
  check(not GTest.TestValid, 'GTest is random and valid');
end;

procedure TDebugTestCases.TestDebug3;
begin
  GTest := TtiObjAbsTestClass.create;
  check(GTest.TestValid, 'GTest is created but not valid');
end;

procedure TDebugTestCases.TestDebug4;
begin
  GTest.free;
  check(not GTest.TestValid, 'GTest is still valid after freeing');
end;

procedure TDebugTestCases.TestDebug5;
begin
  check(IdGetThreadObjectCount > 0, 'Thread Object Counting is not working');
end;

procedure TDebugTestCases.TestDebug6;
var c,i:integer;
begin
  c := IdGetThreadObjectCount;
  for i := 0 to 100 do
    begin
    GTest := TtiObjAbsTestClass.create;
    GTest.free;
    end;
  Check(IdGetThreadObjectCount - c = 0, 'Thread Object Counting didn''t hold it''s value');
end;

end.
