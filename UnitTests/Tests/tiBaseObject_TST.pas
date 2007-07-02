{
Version History:
  26-Apr 2002   Andrew Cumming                  Move includes to allow D6 compile
   7-Mar 2002   Grahame Grieve                  Total Rewrite of Tests
   4-Jun 2003   Peter Hinrichsen                Cloned from IndySoap to tiOPF
   2-July 2007  Peter Hinrichsen                Refactored to remove dependency on Windows.pas & GUI code
}

unit tiBaseObject_TST;

{$I tiDefines.inc}

{$IFNDEF FPC}
  {$IFNDEF OBJECT_TRACKING}
    You must have OBJECT_TRACKING defined to run the tests
  {$ENDIF}
{$ENDIF}


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
    procedure ValidFail1;
    procedure ValidFail2;
    procedure tiGetTotalObjectCount;
    procedure tiDescribeLiveObjects;
  end;

  TtiBaseObjectTest = class(TtiBaseObject);

procedure RegisterTests;


implementation
uses
   SysUtils
  ,tiDUnitDependencies
 ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIBaseObject);
end;

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
    Check(not LO.TestValid(TtiBaseObjectTest));
  finally
    LO.Free;
  end;
  LO := TtiBaseObjectTest.Create;
  try
    Check(LO.TestValid(TtiBaseObjectTest));
  finally
    LO.Free;
  end;
end;

procedure TTestTIBaseObject.ValidFail1;
var
  LO : TtiBaseObject;
begin
  LO := TtiBaseObject(Random(20000)+2000);
  {$IFNDEF FPC}
  check(not LO.TestValid);
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
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
  LO:= nil;
  LO.Free;
end;

procedure TTestTIBaseObject.ValidFail2;
var
  LO : TtiBaseObject;
begin
  LO:= TtiBaseObject.Create;
  LO.free;
  {$IFNDEF FPC}
  check(not LO.TestValid);
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;

procedure TTestTIBaseObject.tiDescribeLiveObjects;
var
  LSL: TStringList;
  LO: TtiBaseObjectTest;
begin
  LSL:= TStringList.Create;
  try
    LSL.Text:= tiBaseObject.tiDescribeLiveObjects;
    CheckEquals('', LSL.Values[TtiBaseObjectTest.ClassName]);
    LO:= TtiBaseObjectTest.Create;
    try
      LSL.Text:= tiBaseObject.tiDescribeLiveObjects;
      CheckEquals('1', LSL.Values[TtiBaseObjectTest.ClassName]);
    finally
      LO.Free;
    end;
  finally
    LSL.Free;
  end;
end;


procedure TTestTIBaseObject.tiGetTotalObjectCount;
{$IFNDEF FPC}
var
  LO : TtiBaseObject;
  LCount: integer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  LCount := tiBaseObject.tiGetTotalObjectCount;
  LO := TtiBaseObject.Create;
  try
    CheckEquals(LCount + 1, tiBaseObject.tiGetTotalObjectCount);
  finally
    LO.free;
  end;
  {$ELSE}
  check(True, 'fpcUnit has its own memory leak tests');
  {$ENDIF}
end;

end.

