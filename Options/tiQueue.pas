{:@summary Provides an efficient queue structure for tiPerObjAbs descendant classes.
   @desc Uses a circular array-based queue structure optimised for speed and ease of use.}

unit tiQueue;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Classes
  ,SyncObjs
  ,tiObject
 ;

type
  TtiObjQueue = class
  Private
    FList : TList;
    FCount : Integer;
    FHead : Integer;
    FTail : Integer;
    Function GetCapacity : Integer;
  Protected
    Procedure Grow;
    Procedure qError(Const AMethod : String);
  Public
    Constructor Create(ACapacity : Integer);
    Destructor Destroy; Override;
    {: Clears the Queue of all enqueued objects}
    Procedure Clear; Virtual;
    Function DeQueue : TtiObject; Virtual;
    Procedure EnQueue(AItem : TtiObject); Virtual;
    {:Examines the object at the "front" of the queue without removing it}
    Function Examine : TtiObject; Virtual;
    Function IsEmpty : Boolean; Virtual;
    {: Returns the number of items in the queue.}
    Property Count : Integer Read FCount;
    {: Returns the current capacity of the queue.}
    Property Capacity : Integer Read GetCapacity;
  End;

  {: TtiObjThreadQueue is a thread safe implementation of TtiQueue. All critical
  queue operations are protected by it's own internal locking mechanism.}
  TtiObjThreadQueue = Class(TtiObjQueue)
  Private
    FSection: TCriticalSection;
    FLockCount : Integer;
    Procedure InternalLock(AExternalLock : boolean);
    Procedure InternalUnLock(AExternalLock : boolean);
  Public
    {:Locks the queue to prevent other threads from disturbing it.}
    Procedure Lock;
    {:Unlocks the queue to allow other threads access.}
    Procedure Unlock;
    {: Creates a thread-safe queue with capacity for ACapacity objects}
    Constructor Create(ACapacity : Integer);
    Destructor Destroy; Override;
    {: Clears the Queue of all enqueued objects}
    Procedure Clear; Override;
    Function DeQueue : TtiObject; Override;
    Procedure EnQueue(AItem : TtiObject); Override;
    {:Examines the object at the "front" of the queue without removing it}
    Function Examine : TtiObject; Override;
  End;

  EQueueError = Class(Exception);

const
  msgEmptyQueue = 'Queue cannot be empty for %s operation.';


implementation
uses
  tiLog
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiObjQueue
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{: Clears the Queue of all enqueued objects}

Procedure TtiObjQueue.Clear;
Begin
  If (Count > 0) Then
  Begin
    While (FHead <> FTail) Do
    Begin
      TtiObject(FList[FHead]).Free;
      FHead := (FHead + 1) Mod FList.Count;
    End;
  End;
  FHead := 0;
  FTail := 0;
  FCount := 0;
End;

{: Creates an instance of TtiObjQueue

@param ACapacity  The initial capacity of the queue. If 0 will default to 16.
@example The following code creates an instance of a queue with a capacity of 10 objects :-
<code>
gObjectQueue := TtiObjQueue.Create(10);
</code>
}

Constructor TtiObjQueue.Create(ACapacity : Integer);
Begin
  Inherited Create;
  FList := TList.Create;
  If (ACapacity <= 1) Then
    ACapacity := 16;
  FList.Count := ACapacity;
End;

{: Gets the next object from the "front" of the queue.

@example The following code checks to see if the customer at the front of the
queue is on hold or not. If it is it is dequeued and processed accordingly.
<code>
If TCustomer(CustomerQueue.Examine).OnHold Then
Begin
  ThisCustomer := TCustomer(CustomerQueue.DeQueue);
  ProcessOnHoldCustomer(ThisCustomer);
End;
</code>
}

Function TtiObjQueue.DeQueue : TtiObject;
Begin
  If (Count = 0) Then
    qError('DeQueue');
  Result := TtiObject(FList[FHead]);
  {move the head index, making sure it's still a valid index;
   decrement the count}
  FHead := (FHead + 1) Mod FList.Count;
  Dec(FCount);
End;

{: Destroys the queue's internal list. It does NOT free any objects contained therein.}

Destructor TtiObjQueue.Destroy;
Begin
  If (Count <> 0) Then
    Clear;
  FList.Free;
  Inherited;
End;

{: Adds an object to the "back" of the queue.
@param AItem  The object to be added to the "back" of the queue.

@example The following code adds an order to the despatch queue if if has been paid.

<code>
If ThisOrder.Paid Then
  DespatchQueue.EnQueue(ThisOrder);
</code>
}

Procedure TtiObjQueue.EnQueue(AItem : TtiObject);
Begin
  FList[FTail]:= AItem;
  FTail := (FTail + 1) Mod FList.Count;
  Inc(FCount);
  If (FTail = FHead) Then
    Grow;
End;

{: Returns the item at the "front" of the queue without DeQueueing it.
 A bit like a "peek" operation.
@example The following code checks to see if the customer at the front of the
queue is on hold or not. If it is it is dequeued and processed accordingly.
<code>
If TCustomer(CustomerQueue.Examine).OnHold Then
Begin
  ThisCustomer := TCustomer(CustomerQueue.DeQueue);
  ProcessOnHoldCustomer(ThisCustomer);
End;
</code>
}

Function TtiObjQueue.Examine : TtiObject;
Begin
  If (Count = 0) Then
    qError('Examine');
  Result := TtiObject(FList[FHead]);
End;

Function TtiObjQueue.GetCapacity : Integer;
Begin
  Result := FList.Count;
End;

{: Increases the capacity of the queue. The increment for this will be 50% of
the current queue size.}
Procedure TtiObjQueue.Grow;
Var
  I : Integer;
  ToInx : Integer;
Begin
  FList.Count := (FList.Count * 3) Div 2;
  If (FHead = 0) Then
    FTail := Count
  Else
  Begin
    ToInx := FList.Count;
    For I := Pred(Count) Downto FHead Do
    Begin
      Dec(ToInx);
      FList[ToInx]:= FList[I];
    End;
    FHead := ToInx;
  End;
End;

Procedure TtiObjThreadQueue.Clear;
Begin
  InternalLock(False);
  Try
    Inherited Clear;
  Finally
    InternalUnlock(False);
  End;
End;

{: Creates a thread-safe object queue with an initial size of ACapacity objects.}
Constructor TtiObjThreadQueue.Create(ACapacity : Integer);

Begin
  Inherited;
  FSection:=TCriticalSection.Create;
  FLockCount := 0;
End;

Function TtiObjThreadQueue.DeQueue : TtiObject;
Begin
  InternalLock(False);
  Try
    Result := Inherited DeQueue;
  Finally
    InternalUnlock(False);
  End;
End;

destructor TtiObjThreadQueue.Destroy;

begin
  FreeAndNil(FSection);
  inherited;
end;

{: Adds an object to the "back" of the queue.
@param AItem  The object to be added to the "back" of the queue.
}

Procedure TtiObjThreadQueue.EnQueue(AItem : TtiObject);
Begin
  InternalLock(False);
  Try
    Inherited EnQueue(AItem);
  Finally
    InternalUnlock(False);
  End;
End;

Function TtiObjThreadQueue.Examine : TtiObject;
Begin
  InternalLock(False);
  Try
    Result := Inherited Examine;
  Finally
    InternalUnlock(False);
  End;
End;

Procedure TtiObjThreadQueue.InternalLock(AExternalLock : boolean);
Begin
  If (Not AExternalLock) And
    (FLockCount > 0) Then
  Begin
    Inc(FLockCount);
    Exit;
  End;
  FSection.Enter;
  Inc(FLockCount);
End;

Procedure TtiObjThreadQueue.InternalUnLock(AExternalLock : boolean);

Begin
  If (Not AExternalLock) And (FLockCount > 1) Then
  Begin
    Dec(FLockCount);
    Exit;
  End;
  FSection.Leave;
  Dec(FLockCount);
End;

{: Returns True if the queue is empty.}

Function TtiObjQueue.IsEmpty : Boolean;
Begin
  Result := (Count = 0);
End;


{: Locks the thread to prevent other threads from gaining access to the queue.
In practice this should never be required as all methods of TtiObjThreadQueue are
automatically thread-safe.}

Procedure TtiObjThreadQueue.Lock;
Begin
  InternalLock(True);
End;

{: Raises an exception of class EQueueError if an exception ocurred internally}

Procedure TtiObjQueue.qError(Const AMethod : String);
Begin
  Raise EQueueError.CreateFmt(msgEmptyQueue, [AMethod]);
End;

{: Unlocks the queue to allow other threads access, should the need arise.}

Procedure TtiObjThreadQueue.Unlock;
Begin
  InternalUnlock(True);
End;

End.



