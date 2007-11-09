unit tiPool;

{$I tiDefines.inc}

interface
uses
  Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,pthreads
  {$ENDIF LINUX}
  ,tiObject
  ,tiBaseObject
  ,tiThread
 ;

const
  CErrorPoolUnlockByData_BadData    = 'Attempt to unlock item by passing object that is not owned by a PooledItem';
  CErrorTimedOutWaitingForSemaphore = 'Timed out waiting for PooledData.';
  CErrorFailedToUnlockPooledItem = 'Attempting to unlock PooledData which can not be found in the pool.';
  CErrorSemaphoreAvailableButNoItemsInPool = 'Semaphore was available but no items ' +
                'available in the pool. MaxPoolSize: %d, Current pool size: %d';
type

  TtiPool = class;

  TtiPooledItem = class(TtiBaseObject)
  private
    FbLocked: boolean;
    FiIndex: integer;
    FdtLastused: TDateTime;
    FOwner : TtiPool;
    FData : TtiBaseObject;
    procedure SetLocked(const AValue: boolean);
    function  GetSecInUse: integer;
  public
    constructor Create(AOwner : TtiPool); virtual;
    destructor  Destroy; override;
    property    Locked : boolean read FbLocked write SetLocked;
    property    Index : integer read FiIndex  write FiIndex;
    property    LastUsed : TDateTime read FdtLastused write FdtLastUsed;
    property    Owner : TtiPool read FOwner write FOwner;
    property    Data : TtiBaseObject read FData write FData;
    property    SecInUse : integer read GetSecInUse;
    // The list may already be locked, so count will not be accessable. Pass it in here.
    function    MustRemoveItemFromPool(AListCount: Integer): boolean; virtual;
    function    SecToTimeOut : integer;
  end;

  TtiPooledItemClass = class of TtiPooledItem;

  TtiAddPooledItemEvent = procedure(APooledItem : TtiPooledItem) of object;


  TtiThrdPoolMonitor = class(TtiSleepThread)
  private
    FPool : TtiPool;
  public
    constructor CreateExt(APool : TtiPool);
    procedure   Execute; override;
  end;


  TtiPooledItemEvent = procedure(const APooledItem : TtiPooledItem) of object;
  TtiPool = class(TtiBaseObject)
  private
    FPool : TThreadList;
    {$IFDEF MSWINDOWS}
    FSemaphore : THandle;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FSemaphore : TSemaphore;
    {$ENDIF LINUX}
    FMinPoolSize: integer;
    FMaxPoolSize: integer;
    FTimeOut: Extended;
    FThrdPoolMonitor : TtiThrdPoolMonitor;
    FWaitTime: integer;
    procedure SetMaxPoolSize(const AValue: integer);

    // ToDo: Move the semaphore operations to a class wrapper
    procedure CreatePoolSemaphore;
    procedure DestroyPoolSemaphore;
    function  LockPoolSemaphore: boolean;
    procedure UnlockPoolSemaphore;

    function  GetCount: integer;
    function  GetCountLocked: integer;
    function  FindAvailableItemInPool(const AList: TList): TtiPooledItem;
  protected
    procedure   Clear;
    function    AddItem : TtiPooledItem;
    procedure   SweepForTimeOuts; virtual;
    procedure   Remove(const APooledItem : TtiPooledItem);
    function    PooledItemClass: TtiPooledItemClass; virtual; abstract;
    procedure   AfterAddPooledItem(const APooledItem: TtiPooledItem); virtual; abstract;

  public
    constructor Create;
    destructor  Destroy; override;
    property    TimeOut    : Extended read FTimeOut     write FTimeOut;
    property    MinPoolSize : integer read FMinPoolSize write FMinPoolSize;
    property    MaxPoolSize : integer read FMaxPoolSize write SetMaxPoolSize;
    property    WaitTime   : integer read FWaitTime     write FWaitTime;

    property    Count   : integer read GetCount;
    property    CountLocked : integer read GetCountLocked;

    function    Lock : TtiBaseObject; virtual;
    procedure   UnLock(const APooledItemData : TtiBaseObject); virtual;
    procedure   ForEachPooledItem(const AMethod : TtiPooledItemEvent);

  end;


implementation
uses
   tiUtils
  ,tiLog
  ,tiConstants
  ,tiExcept
  ,SysUtils
  {$IFDEF UNIX}
  ,baseunix
  {$ENDIF UNIX}
 ;

const
  CMaxPoolSize =  9999; // Maximum number of items allowed in the pool
  CMinPoolSize =     1; // Minimum number of items to remain in the pool
  CTimeOut     =     1; // Time (minutes) before items are purged from the pool
  CWaitTime    =    60; // Time to wait for a pool item (in seconds)


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiPool
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPool.Create;
begin
  inherited create;
  FPool := TThreadList.Create;
  FMinPoolSize := CMinPoolSize;
  FMaxPoolSize := CMaxPoolSize;
  FWaitTime    := CWaitTime;
  FTimeOut    := CTimeOut;
  CreatePoolSemaphore;
  FThrdPoolMonitor := TtiThrdPoolMonitor.CreateExt(self);
end;


destructor TtiPool.Destroy;
begin
  FThrdPoolMonitor.Free;
  Clear;
  FPool.Free;
  DestroyPoolSemaphore;
  inherited;
end;


procedure TtiPool.DestroyPoolSemaphore;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
  {$IFDEF MSWINDOWS}
  CloseHandle(FSemaphore);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_destroy(FSemaphore);
  if error <> 0 then
    raise Exception.Create('Failed to destroy the semaphore');
  {$ENDIF LINUX}
end;

procedure TtiPool.Clear;
var
  lList : TList;
  i : integer;
  lPooledItem : TtiPooledItem;
begin
  lList := FPool.LockList;
  try
    for i := lList.Count - 1 downto 0 do
    begin
      lPooledItem := TObject(lList.Items[i]) as TtiPooledItem;
      Assert(lPooledItem.TestValid(TtiPooledItem), CTIErrorInvalidObject);;
      lPooledItem.Free;
      lList.Delete(i);
    end;
  finally
    FPool.UnLockList;
  end;
end;


function TtiPool.AddItem : TtiPooledItem;
var
  lPooledItem : TtiPooledItem;
  lList : TList;
  lsCount : string;
begin
  
  lPooledItem := PooledItemClass.Create(self);
  lsCount    := IntToStr(Count);

  try
    Log('Attempting to add pooled item #' + lsCount, lsConnectionPool);

    lList := FPool.LockList;
    try
      lPooledItem.Owner := Self;
      Log('Pooled item #' +
           IntToStr(lList.Count-1) +
           ' added.', lsConnectionPool);

      AfterAddPooledItem(lPooledItem);

      lList.Add(lPooledItem);
      lPooledItem.Index := lList.Count - 1;

    finally
      FPool.UnLockList;
    end;
    result := lPooledItem;

  except
    on e:exception do
    begin
      lPooledItem.Free;
      raise;
    end;
  end;
end;

function TtiPool.FindAvailableItemInPool(const AList: TList): TtiPooledItem;
var
  i: integer;
  LItem: TtiPooledItem;
begin
  // Scan the list of PooledItem to find one which is not locked.
  for i := 0 to AList.Count - 1 do
  begin
    // There was a PooledItem which was not locked available, so lock it
    // and exit.
    LItem := (TObject(AList.Items[i]) as TtiPooledItem);
    if (not LItem.Locked) and
       (not LItem.MustRemoveItemFromPool(AList.Count)) then
    begin
      Result:= LItem;
      Result.Locked := true;
      Result.Index := I;
      Log('PooledItem #' + intToStr(I) + ' Locked.', lsConnectionPool);
      Exit; //==>
    end;
  end;
  Result:= nil;
end;

function TtiPool.Lock: TtiBaseObject;
var
  LPool : TList;
  LItem : TtiPooledItem;
begin

  if not LockPoolSemaphore then
    raise EtiOPFInternalException.Create(CErrorTimedOutWaitingForSemaphore);

  Result:= nil;
  // A semaphore was available, so get a PooledItem
  LPool := FPool.LockList;

  try
    LItem:= FindAvailableItemInPool(LPool);

    // There was a semaphore available, but no PooledItem, so there is room
    // in the pool to create another.
    if (LItem = nil) and
       (LPool.Count < FMaxPoolSize) then
    begin
      LItem := AddItem;
      LItem.Locked := true;
      Log('A new PooledItem has been added to the pool.', lsConnectionPool);
      Log('PooledItem #' + intToStr(LPool.Count) + ' locked.', lsConnectionPool);
    end;

    // If we get here, the semahpore system and the pool area
    // out of sync, so raise an exception.
    if LItem = nil then
      raise EtiOPFProgrammerException.CreateFmt(
        CErrorSemaphoreAvailableButNoItemsInPool, [FMaxPoolSize, LPool.Count]);
    result:= LItem.Data;

  finally
    FPool.UnLockList;
  end;
end;


function TtiPool.LockPoolSemaphore: boolean;
begin
  // Wait for a semaphore
  {$IFDEF MSWINDOWS}
  result:= WaitForSingleObject(FSemaphore, FWaitTime * 1000) <> WAIT_TIMEOUT;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  { TODO: The timeout option is not available in POSIX semaphores. This can be
    achieved by issuing a non-blocking sem_trywait() within a loop, which
    counts the timeout value: int sem_trywait(sem_t * sem).
    i := fpgeterrno; }
  result:= sem_trywait(FSemaphore) = 0;
  {$ENDIF LINUX}
end;

procedure TtiPool.SetMaxPoolSize(const AValue: integer);
begin
  FMaxPoolSize := AValue;
  CreatePoolSemaphore;
end;


procedure TtiPool.UnLock(const APooledItemData : TtiBaseObject);
var
  i : integer;
  LList : TList;
  LItem: TtiPooledItem;
begin
  Assert(APooledItemData.TestValid, CTIErrorInvalidObject);
  LList := FPool.LockList;
  try
    for i := 0 to LList.Count-1 do
      if TtiPooledItem(LList.Items[i]).Data = APooledItemData then
      begin
        LItem:= TtiPooledItem(LList.Items[i]);
        LItem.Locked:= False;
        UnlockPoolSemaphore;
        Exit; //==>
      end;
      Raise EtiOPFProgrammerException.Create(CErrorFailedToUnlockPooledItem);
  finally
    FPool.UnLockList;
  end;
end;


function TtiPool.GetCount: integer;
var
  lList : TList;
begin
  lList := FPool.LockList;
  try
    result := lList.Count;
  finally
    FPool.UnLockList;
  end;
end;


{ TPooledItem }

constructor TtiPooledItem.Create(AOwner : TtiPool);
begin
  inherited Create;
  Index := -1;
  Locked := false;
  FOwner := AOwner;
  FdtLastused := Now;
end;


destructor TtiPooledItem.destroy;
begin
  FData.Free;
  inherited;
end;


function TtiPooledItem.GetSecInUse: integer;
begin
  result := Trunc((Now - LastUsed) * 24 * 60 * 60);
end;


function TtiPooledItem.MustRemoveItemFromPool(AListCount: Integer): boolean;
var
  lNotLocked:   Boolean;
  lTimeOut:     Boolean;
  lMinPoolSize: Boolean;
begin
  lNotLocked  := (not Locked);
  lTimeOut    := (SecToTimeOut <= 0);
  lMinPoolSize := (AListCount > Owner.MinPoolSize);
  result      := lNotLocked and lTimeOut and lMinPoolSize;
end;


function TtiPooledItem.SecToTimeOut: integer;
begin
  if Locked then
    result := cSecToTimeOutLocked
  else
    result := Trunc(Owner.TimeOut * 60) - SecInUse;
end;


procedure TtiPooledItem.SetLocked(const AValue: boolean);
begin
  if FbLocked and not AValue then
    Lastused := now;
  FbLocked := AValue;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdPoolMonitor
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiThrdPoolMonitor.CreateExt(APool: TtiPool);
begin
  Create(true);
  FreeOnTerminate := false;
  FPool := APool;
  Priority := tpLowest;
  resume;
end;


procedure TtiThrdPoolMonitor.Execute;
begin
  while SleepAndCheckTerminated(10000) do
    FPool.SweepForTimeOuts;
end;


// Scan the pool for items which have timed out and should be removed from
// the pool
procedure TtiPool.SweepForTimeOuts;
var
  lList : TList;
  i : integer;
  lPooledItem : TtiPooledItem;
  lCount : integer;
begin
  lCount := Count;
  if lCount <= MinPoolSize then
    Exit; //==>
  // TimeOut is in minutes, so convert to TDateTime
  lList := FPool.LockList;
  try
    for i := lList.Count - 1 downto 0 do begin
      lPooledItem := TtiPooledItem(lList.Items[i]);
      if lPooledItem.MustRemoveItemFromPool(lList.Count) then
      begin
        Log('Pooled item (' + ClassName + ') #' +
             IntToStr(lPooledItem.Index) +
             ' being removed from the pool.', lsConnectionPool);
        lList.Delete(i);
        lPooledItem.Free;
        Log('There are ' +
             IntToStr(lList.Count) +
             ' items left in the pool.', lsConnectionPool);
      end;
    end;
  finally
    FPool.UnLockList
  end;
end;

procedure TtiPool.UnlockPoolSemaphore;
begin
  {$IFDEF MSWINDOWS}
  ReleaseSemaphore(FSemaphore, 1, nil);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if sem_post(FSemaphore) <> 0 then
    raise Exception.Create('Failed to unlock the semaphore');
  {$ENDIF LINUX}
end;

procedure TtiPool.CreatePoolSemaphore;
begin
  {$IFDEF MSWINDOWS}
  if FSemaphore <> 0 then
    CloseHandle(FSemaphore);
  FSemaphore := CreateSemaphore(nil, FMaxPoolSize, FMaxPoolSize, nil);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  sem_destroy(FSemaphore);
  if sem_init(FSemaphore, 0, FMaxPoolSize) <> 0 then
    raise Exception.Create('Failed to create the semaphore');
  {$ENDIF LINUX}
end;


function TtiPool.GetCountLocked: integer;
var
  lList : TList;
  i : integer;
begin
  result := 0;
  lList := FPool.LockList;
  try
    for i := 0 to lList.Count - 1 do
      if TtiPooledItem(lList.Items[i]).Locked then
        Inc(Result);
  finally
    FPool.UnLockList;
  end;
end;

procedure TtiPool.ForEachPooledItem(const AMethod: TtiPooledItemEvent);
var
  lList : TList;
  i : integer;
  lPooledItem : TtiPooledItem;
begin
  lList := FPool.LockList;
  try
    for i := 0 to lList.Count - 1 do
    begin
      lPooledItem := TtiPooledItem(lList.Items[i]);
      AMethod(lPooledItem);
    end;
  finally
    FPool.UnLockList
  end;
end;

procedure TtiPool.Remove(const APooledItem: TtiPooledItem);
var
  lList : TList;
begin
  if APooledItem.Locked then
    UnLock(APooledItem);
  lList := FPool.LockList;
  try
    lList.Remove(APooledItem);
    APooledItem.Free;
  finally
    FPool.UnLockList
  end;
end;

end.





