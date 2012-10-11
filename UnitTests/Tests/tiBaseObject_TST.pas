unit tiBaseObject_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFramework
  ,Classes
 ;

type

  TTestTIBaseObject = class (TtiTestCase)
  published
    procedure ValidNill;
    procedure ValidClass;
    procedure ValidPass;
  end;

procedure RegisterTests;

implementation
uses
   SysUtils
  ,tiBaseObject
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIBaseObject);
end;

type
  TtiBaseObjectForTesting = class(TtiBaseObject);

procedure TTestTIBaseObject.ValidNill;
var
  LO : TtiBaseObject;
begin
  LO := nil;
  check(LO = nil);
  check(not LO.TestValid);
  check(LO.TestValid(TtiBaseObject, True));
end;

procedure TTestTIBaseObject.ValidClass;
var
  LO : TtiBaseObject;
begin
  LO := TtiBaseObject.Create;
  try
    Check(LO.TestValid(TtiBaseObject));
    Check(not LO.TestValid(TtiBaseObjectForTesting));
  finally
    LO.Free;
  end;
  LO := TtiBaseObjectForTesting.Create;
  try
    Check(LO.TestValid(TtiBaseObjectForTesting));
  finally
    LO.Free;
  end;
end;

procedure TTestTIBaseObject.ValidPass;
var
  LO : TtiBaseObject;
begin
  LO := TtiBaseObject.Create;
  try
    check(LO.TestValid);
  finally
    LO.Free;
  end;
end;


end.


