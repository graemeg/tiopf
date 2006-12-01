{
Version History:
  26-Apr 2002   Andrew Cumming                  Move includes to allow D6 compile
   7-Mar 2002   Grahame Grieve                  Total Rewrite of Tests
   4-Jun 2003   Peter Hinrichsen                Cloned from IndySoap to tiOPF
}

unit tiBaseObject_TST;

{$I tiDefines.inc}

{$IFNDEF FPC}
  {$IFNDEF OBJECT_TRACKING}
    Currently, you must have OBJECT_TRACKING defined to run the tests
  {$ENDIF}
{$ENDIF}


interface
uses
  {$IFNDEF FPC}
  TestFramework,
  {$ENDIF}
  tiBaseObject
  ,tiTestFramework
 ;

type
  TtiObjAbsTestClass = class (TtiBaseObject)
  end;

  TTestTIBaseObject = class (TtiTestCase)
  published
    procedure TestDebug1;
    procedure TestDebug2;
    procedure TestDebug3;
    procedure TestDebug4;
    procedure TestDebug5;
    procedure TestDebug6;
  end;

procedure RegisterTests;


implementation
uses
  tiDUnitDependencies
 ;

var
  GTest : TtiObjAbsTestClass;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIBaseObject);
end;


procedure TTestTIBaseObject.TestDebug1;
begin
  GTest := nil;
  check(GTest = nil, 'GTest is not nil');
  check(not GTest.TestValid, 'GTest is nil and valid');
end;


procedure TTestTIBaseObject.TestDebug2;
begin
  GTest := TtiObjAbsTestClass(Random(20000)+2000);
  {$IFNDEF FPC}
  check(not GTest.TestValid, 'GTest is random and valid');
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;


procedure TTestTIBaseObject.TestDebug3;
begin
  GTest := TtiObjAbsTestClass.Create;
  check(GTest.TestValid, 'GTest is created but not valid');
end;


procedure TTestTIBaseObject.TestDebug4;
begin
  GTest.free;
  {$IFNDEF FPC}
  check(not GTest.TestValid, 'GTest is still valid after freeing');
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;


procedure TTestTIBaseObject.TestDebug5;
begin
  {$IFNDEF FPC}
  check(IdGetThreadObjectCount > 0, 'Thread Object Counting is not working');
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;


procedure TTestTIBaseObject.TestDebug6;
{$IFNDEF FPC}
var
  c,i: integer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  c := IdGetThreadObjectCount;
  for i := 0 to 100 do
    begin
    GTest := TtiObjAbsTestClass.Create;
    GTest.free;
    end;
  Check(IdGetThreadObjectCount - c = 0, 'Thread Object Counting didn''t hold it''s value');
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;


end.

