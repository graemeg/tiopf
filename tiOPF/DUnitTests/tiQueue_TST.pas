Unit tiQueue_TST;

Interface

Uses tiQueue, TestFramework, tiPtnVisPerObj;

Type
  TTestObject = Class(TPerObjAbs)
  Private
    FNumber : Integer;
    FDescription : String;
  Public
    Constructor CreateTest(pID : Integer);
  Published
    Property Number : Integer Read FNumber Write FNumber;
    Property Description : String Read FDescription Write FDescription;
  End;

  TTestTIQueue = Class(TTestCase)
    FQueue : TtiObjQueue;
  Protected
    Function ObjectsAreEqual(LHS, RHS : TTestObject) : Boolean;
    Procedure Setup; Override;
    Procedure TearDown; Override;
  Published
    Procedure Grow;
    Procedure EnQueue;
    Procedure DeQueue;
    Procedure Examine;
    Procedure Clear;
    Procedure IsEmpty;
  End;

Procedure RegisterTests;

Implementation

Uses
  SysUtils
//  ,tiDBConnectionSetupAbs_TST
  ,tiDUnitDependencies
  ;
  
Procedure RegisterTests;
Begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest(TTestTIQueue.Suite);
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
    FQueue.Clear;
    FQueue.EnQueue(lTestObj1);
    lTestObj2 := TTestObject(FQueue.DeQueue);
    Check(ObjectsAreEqual(lTestObj1, lTestObj2), 'DeQueue failed.');
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
    FQueue.Clear;
    FQueue.EnQueue(lTestObj1);
    lTestObj2 := TTestObject(FQueue.DeQueue);
    Check(ObjectsAreEqual(lTestObj1, lTestObj2), 'EnQueue failed.');
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

