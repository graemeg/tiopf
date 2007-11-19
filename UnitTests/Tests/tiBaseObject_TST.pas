unit tiBaseObject_TST;

{$I tiDefines.inc}

interface

uses
  {$IFNDEF FPC}
  TestFramework,
  {$ENDIF}
  tiBaseObject
  ,tiTestFramework
  ,Classes
 ;

type

  TTestTIBaseObject = class (TtiTestCase)
  published
    procedure TObjectFree;
    procedure TtiBaseObjectFree;
    procedure TObjectNillFree;
    procedure TtiBaseObjectNillFree;
    procedure ValidNill;
    procedure ValidClass;
    procedure ValidPass;
  end;

procedure RegisterTests;

implementation
uses
   SysUtils
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIBaseObject);
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

{$WARNINGS OFF}
procedure TTestTIBaseObject.TObjectFree;
var
  LO: TObject;
begin
  try
    Check(True); // To Force OnCheckCalled to be called
    LO.Free;
    Fail('Exception should have been raised');
  except
    on e: Exception do
      CheckIs(E, EInvalidPointer);
  end;
end;
{$WARNINGS ON}

procedure TTestTIBaseObject.TObjectNillFree;
var
  LO: TObject;
begin
  Check(True); // To Force OnCheckCalled to be called
  LO:= nil;
  LO.Free;
end;

{$WARNINGS OFF}
procedure TTestTIBaseObject.TtiBaseObjectFree;
var
  LO: TtiBaseObject;
begin
  try
    LO.Free;
    Fail('Exception should have been raised');
  except
    on e: Exception do
      CheckIs(E, EInvalidPointer);
  end;
end;
{$WARNINGS ON}

procedure TTestTIBaseObject.TtiBaseObjectNillFree;
var
  LO: TtiBaseObject;
begin
  Check(True); // To Force OnCheckCalled to be called
  LO:= nil;
  LO.Free;
end;

end.

