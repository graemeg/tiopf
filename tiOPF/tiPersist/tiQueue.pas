{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Andrew Denton (adenton@q-range.com
  a_denton@blueyonder.co.uk)
  as part of the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Revision history:
    September 2002, Andrew Denton - Initial Version
    August 2003, Andrew Denton - Added TtiObjThreadQueue

  Purpose:
    Provides an efficient queue structure for tiPerObjAbs descendant classes.
    Uses a circular array-based queue structure.

  Classes:
    TtiObjQueue
    TtiObjThreadQueue

  ToDo:


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{:@summary Provides an efficient queue structure for tiPerObjAbs descendant classes.
   @desc Uses a circular array-based queue structure optimised for speed and ease of use.}

Unit tiQueue;

{$I tiDefines.inc}

Interface
Uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Classes
  ,Windows
  {$ENDIF MSWINDOWS}
  ,tiPtnVisPerObj
  ;

Type
  TtiObjQueue = Class
  Private
    FList : TList;
    FCount : Integer;
    FHead : Integer;
    FTail : Integer;
    Function GetCapacity : Integer;
  Protected
    Procedure Grow;
    Procedure qError(Const pMethod : String);
  Public
    Constructor Create(pCapacity : Integer);
    Destructor Destroy; Override;
    {: Clears the Queue of all enqueued objects}
    Procedure Clear; Virtual;
    Function DeQueue : TPerObjAbs; Virtual;
    Procedure EnQueue(pItem : TPerObjAbs); Virtual;
    {:Examines the object at the "front" of the queue without removing it}
    Function Examine : TPerObjAbs; Virtual;
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
    FSemaphore : THandle;
    FLockCount : Integer;
    Procedure InternalLock(pExternalLock : boolean);
    Procedure InternalUnLock(pExternalLock : boolean);
  Public
    {:Locks the queue to prevent other threads from disturbing it.}
    Procedure Lock;
    {:Unlocks the queue to allow other threads access.}
    Procedure Unlock;
    {: Creates a thread-safe queue with capacity for pCapacity objects}
    Constructor Create(pCapacity : Integer);
    Destructor Destroy; Override;
    {: Clears the Queue of all enqueued objects}
    Procedure Clear; Override;
    Function DeQueue : TPerObjAbs; Override;
    Procedure EnQueue(pItem : TPerObjAbs); Override;
    {:Examines the object at the "front" of the queue without removing it}
    Function Examine : TPerObjAbs; Override;
  End;

  EQueueError = Class(Exception);

Const
  msgEmptyQueue = 'Queue cannot be empty for %s operation.';


Implementation
Uses
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
      TPerObjAbs(FList[FHead]).Free;
      FHead := (FHead + 1) Mod FList.Count;
    End;
  End;
  FHead := 0;
  FTail := 0;
  FCount := 0;
End;

{: Creates an instance of TtiObjQueue

@param pCapacity  The initial capacity of the queue. If 0 will default to 16.
@example The following code creates an instance of a queue with a capacity of 10 objects :-
<code>
gObjectQueue := TtiObjQueue.Create(10);
</code>
}

Constructor TtiObjQueue.Create(pCapacity : Integer);
Begin
  Inherited Create;
  FList := TList.Create;
  If (pCapacity <= 1) Then
    pCapacity := 16;
  FList.Count := pCapacity;
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

Function TtiObjQueue.DeQueue : TPerObjAbs;
Begin
  If (Count = 0) Then
    qError('DeQueue');
  Result := TPerObjAbs(FList[FHead]);
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
@param pItem  The object to be added to the "back" of the queue.

@example The following code adds an order to the despatch queue if if has been paid.

<code>
If ThisOrder.Paid Then
  DespatchQueue.EnQueue(ThisOrder);
</code>
}

Procedure TtiObjQueue.EnQueue(pItem : TPerObjAbs);
Begin
  FList[FTail] := pItem;
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

Function TtiObjQueue.Examine : TPerObjAbs;
Begin
  If (Count = 0) Then
    qError('Examine');
  Result := FList[FHead];
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
      FList[ToInx] := FList[I];
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

{: Creates a thread-safe object queue with an initial size of pCapacity objects.}

Constructor TtiObjThreadQueue.Create(pCapacity : Integer);
Var
  lSemaphoreName : String;
Begin
  Inherited;
  lSemaphoreName := ClassName;
  FSemaphore := CreateSemaphore(Nil, 1, 1, PChar(lSemaphoreName));
  FLockCount := 0;
End;

Function TtiObjThreadQueue.DeQueue : TPerObjAbs;
Begin
  InternalLock(False);
  Try
    Result := Inherited DeQueue;
  Finally
    InternalUnlock(False);
  End;
End;

Destructor TtiObjThreadQueue.Destroy;
Begin
  CloseHandle(FSemaphore);
  Inherited;
End;

{: Adds an object to the "back" of the queue.
@param pItem  The object to be added to the "back" of the queue.
}

Procedure TtiObjThreadQueue.EnQueue(pItem : TPerObjAbs);
Begin
  InternalLock(False);
  Try
    Inherited EnQueue(pItem);
  Finally
    InternalUnlock(False);
  End;
End;

Function TtiObjThreadQueue.Examine : TPerObjAbs;
Begin
  InternalLock(False);
  Try
    Result := Inherited Examine;
  Finally
    InternalUnlock(False);
  End;
End;

Procedure TtiObjThreadQueue.InternalLock(pExternalLock : boolean);
Begin
  If (Not pExternalLock) And
    (FLockCount > 0) Then
  Begin
    Inc(FLockCount);
    Exit;
  End;
  If (WaitForSingleObject(FSemaphore, 60000) = WAIT_TIMEOUT) Then
  Begin
    LogError('Timed out waiting to lock ' + ClassName);
    Exit;
  End;
  Inc(FLockCount);
End;

Procedure TtiObjThreadQueue.InternalUnLock(pExternalLock : boolean);
Begin
  If (Not pExternalLock) And (FLockCount > 1) Then
  Begin
    Dec(FLockCount);
    Exit;
  End;
  ReleaseSemaphore(FSemaphore, 1, Nil);
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

Procedure TtiObjQueue.qError(Const pMethod : String);
Begin
  Raise EQueueError.CreateFmt(msgEmptyQueue, [pMethod]);
End;

{: Unlocks the queue to allow other threads access, should the need arise.}

Procedure TtiObjThreadQueue.Unlock;
Begin
  InternalUnlock(True);
End;

End.

