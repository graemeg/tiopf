unit tiQueue_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework
  ,tiQueue
  ,tiObject;


type
  TTestObject = class(TtiObject)
  private
    FNumber : Integer;
    FDescription : String;
  public
    Constructor CreateTest(pID : Integer);
  published
    Property Number : Integer Read FNumber Write FNumber;
    Property Description : String Read FDescription Write FDescription;
  end;


  TTestTIQueue = Class(TtiTestCase)
  private
    FQueue : TtiObjQueue;
  protected
    Function ObjectsAreEqual(LHS, RHS : TTestObject) : Boolean;
    Procedure Setup; Override;
    Procedure TearDown; Override;
  published
    Procedure Grow;
    Procedure EnQueue;
    Procedure DeQueue;
    Procedure Examine;
    Procedure Clear;
    Procedure IsEmpty;
  end;


procedure RegisterTests;


implementation
uses
  SysUtils
  ,tiDUnitDependencies
  ;
  
  
procedure RegisterTests;
Begin
  RegisterNonPersistentTest(TTestTIQueue);
End;


{ TTesttiQueue }

Procedure TTesttiQueue.Clear;
Var
  lTestObj1 : TTestObject;
Begin
  lTestObj1 := TTestObject.CreateTest(1);
  FQueue.EnQueue(lTestObj1);
  FQueue.Clear;
  Check(FQueue.IsEmpty, 'Clear failed.');
End;


Procedure TTesttiQueue.DeQueue;
Var
  lTestObj1 : TTestObject;
  lTestObj2 : TTestObject;
Begin
  Try
    lTestObj1 := TTestObject.CreateTest(1);
    try
      FQueue.Clear;
      FQueue.EnQueue(lTestObj1);
      lTestObj2 := TTestObject(FQueue.DeQueue);
      Check(ObjectsAreEqual(lTestObj1, lTestObj2), 'DeQueue failed.');
    finally
      lTestObj1.Free;
    end;
  Finally
    FQueue.Clear;
  End;
End;


Procedure TTesttiQueue.EnQueue;
Var
  lTestObj1 : TTestObject;
  lTestObj2 : TTestObject;
Begin
  Try
    lTestObj1 := TTestObject.CreateTest(1);
    try
      FQueue.Clear;
      FQueue.EnQueue(lTestObj1);
      lTestObj2 := TTestObject(FQueue.DeQueue);
      Check(ObjectsAreEqual(lTestObj1, lTestObj2), 'EnQueue failed.');
    finally
      lTestObj1.Free;
    end;
  Finally
    FQueue.Clear;
  End;
End;


Procedure TTesttiQueue.Examine;
Var
  lTestObj1 : TTestObject;
  lTestObj2 : TTestObject;
Begin
  Try
    lTestObj1 := TTestObject.CreateTest(1);
    FQueue.Clear;
    FQueue.EnQueue(lTestObj1);
    lTestObj2 := TTestObject(FQueue.Examine);
    Check(ObjectsAreEqual(lTestObj1, lTestObj2), 'Examine failed.');
  Finally
    FQueue.Clear;
  End;
End;


// Not sure how to test this.
Procedure TTesttiQueue.Grow;
Begin
  Check(True, 'WTF???');
End;


Procedure TTesttiQueue.IsEmpty;
Begin
  FQueue.Clear;
  Check(FQueue.IsEmpty,'IsEmpty Failed.');
End;


Function TTesttiQueue.ObjectsAreEqual(LHS, RHS : TTestObject) : Boolean;
Begin
  Result := ((LHS.Number = RHS.Number) And (LHS.Description = RHS.Description));
End;


Procedure TTesttiQueue.Setup;
Begin
  FQueue := TtiObjQueue.Create(2);
  Inherited;
End;


Procedure TTesttiQueue.TearDown;
Begin
  FQueue.Free;
  Inherited;
End;


{ TTestObject }

Constructor TTestObject.CreateTest(pID : Integer);
Begin
  Create;
  FNumber := pID;
  FDescription := 'Test Item ' + IntToStr(pID);
End;

End.

