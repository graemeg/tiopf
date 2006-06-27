unit tiPool ;

{$I tiDefines.inc}

interface
uses
  Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
//  ,cthreads
  ,pthreads
  {$ENDIF LINUX}
  ,tiObject
  ,tiBaseObject
  ,tiThread                  
  ;

const
  cErrorPoolUnlockByData_BadData    = 'Attempt to unlock item by passing object that is not owned by a PooledItem' ;
  cErrorTimedOutWaitingForSemaphore = 'Timed out waiting for a PooledItem.';

type

  TtiPool = class ;

  TPooledItem = class( TtiBaseObject )
  private
    FbLocked: boolean;
    FiIndex: integer;
    FdtLastused: TDateTime;
    FOwner : TtiPool ;
    FData : TtiBaseObject ;
    procedure SetLocked(const Value: boolean);
    function  GetSecInUse: integer;
  public
    constructor Create( pOwner : TtiPool ) ; virtual ;
    destructor  Destroy ; override ;
    property    Locked : boolean read FbLocked write SetLocked ;
    property    Index  : integer read FiIndex  write FiIndex ;
    property    LastUsed : TDateTime read FdtLastused write FdtLastUsed ;
    property    Owner : TtiPool read FOwner write FOwner ;
    property    Data : TtiBaseObject read FData write FData ;
    property    SecInUse : integer read GetSecInUse ;
    // The list may already be locked, so count will not be accessable. Pass it in here.
    function    MustRemoveItemFromPool(pListCount: Integer): boolean ; virtual ;
    function    SecToTimeOut : integer ;
  end ;

  TPooledItemClass = class of TPooledItem ;

  TAddPooledItemEvent = procedure( pPooledItem : TPooledItem ) of object ;


  TThrdPoolMonitor = class(TtiSleepThread)
  private
    FPool : TtiPool ;
  public
    constructor CreateExt( pPool : TtiPool ) ;
    procedure   Execute ; override ;
  end ;


  TtiPooledItemEvent = procedure( const pPooledItem : TPooledItem ) of object ;
  TtiPool = class( TtiObject )
  private
    FPool : TThreadList ;
    {$IFDEF MSWINDOWS}
    FSemaphore : THandle ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FSemaphore : TSemaphore ;
    {$ENDIF LINUX}
    FiMinPoolSize: integer;
    FiMaxPoolSize: integer;
    FTimeOut: real;
    FOnAddPooledItem: TAddPooledItemEvent;
    FPooledItemClass: TPooledItemClass;
    FThrdPoolMonitor : TThrdPoolMonitor ;
    FWaitTime: integer;
    procedure SetMaxPoolSize(const Value: integer);
    procedure CreatePoolSemaphore ;
    function  GetCount: integer;
    function  GetCountLocked: integer;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Clear ;
    property    TimeOut     : real    read FTimeOut      write FTimeOut ;
    property    MinPoolSize : integer read FiMinPoolSize write FiMinPoolSize ;
    property    MaxPoolSize : integer read FiMaxPoolSize write SetMaxPoolSize ;
    property    WaitTime    : integer read FWaitTime     write FWaitTime ;

    property    Count    : integer read GetCount ;
    property    CountLocked : integer read GetCountLocked ;

    property    OnAddPooledItem : TAddPooledItemEvent read FOnAddPooledItem write FOnAddPooledItem ;
    property    PooledItemClass : TPooledItemClass    read FPooledItemClass write FPooledItemClass ;

    function    AddItem : TPooledItem ;
    function    Lock : TPooledItem ;
    procedure   UnLock( pPooledItem : TPooledItem ) ;
    procedure   UnLockByData( const pData : TObject ) ;
    procedure   SweepForTimeOuts ;
    procedure   ForEachPooledItem(const pMethod : TtiPooledItemEvent);
    procedure   Remove(const pPooledItem : TPooledItem);
  end;


implementation
uses
   tiUtils
  ,tiLog
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ;

const
  cuiMaxPoolSize =  9999 ; // Maximum number of items allowed in the pool
  cuiMinPoolSize =     1 ; // Minimum number of items to remain in the pool
  cuiTimeOut     =     1 ; // Time (minutes) before items are purged from the pool
  cWaitTime      =    60 ; // Time to wait for a pool item (in seconds)


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiPool
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPool.Create;
begin
  inherited create ;
  FPool := TThreadList.Create ;
  FPooledItemClass := TPooledItem ;
  FiMinPoolSize := cuiMinPoolSize ;
  FiMaxPoolSize := cuiMaxPoolSize ;
  FWaitTime     := cWaitTime ;
  FTimeOut     := cuiTimeOut ;
  CreatePoolSemaphore ;
  FThrdPoolMonitor := TThrdPoolMonitor.CreateExt( self ) ;
end;


destructor TtiPool.Destroy;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
  FThrdPoolMonitor.Free ;
  Clear ;
  FPool.Free ;
  {$IFDEF MSWINDOWS}
  CloseHandle( FSemaphore ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_destroy( FSemaphore );
  if error <> 0 then
    raise Exception.Create('Failed to destroy the semaphore');
  {$ENDIF LINUX}
  inherited ;
end;


procedure TtiPool.Clear ;
var
  lList : TList ;
  i : integer ;
  lPooledItem : TPooledItem ;
begin
  lList := FPool.LockList ;
  try
    for i := lList.Count - 1 downto 0 do
    begin
      lPooledItem := TObject(lList.Items[i]) as TPooledItem ;
      Assert( lPooledItem.TestValid(TPooledItem), cTIInvalidObjectError ); ;
      lPooledItem.Free;
      lList.Delete(i);
    end;
  finally
    FPool.UnLockList ;
  end ;
end ;


function TtiPool.AddItem : TPooledItem ;
var
  lPooledItem : TPooledItem ;
  lList : TList ;
  lsCount : string ;
begin
  
  lPooledItem := PooledItemClass.Create( self ) ;
  lsCount     := IntToStr( Count ) ;

  try
    Log( 'Attempting to add pooled item #' + lsCount, lsConnectionPool ) ;

    lList := FPool.LockList ;
    try
      lPooledItem.Owner := Self ;
      Log( 'Pooled item #' +
           IntToStr( lList.Count-1 ) +
           ' added.', lsConnectionPool ) ;
      if Assigned( OnAddPooledItem ) then
        OnAddPooledItem( lPooledItem ) ;

      lList.Add( lPooledItem ) ;
      lPooledItem.Index := lList.Count - 1 ;

    finally
      FPool.UnLockList ;
    end ;
    result := lPooledItem ;

  except
    on e:exception do
    begin
      lPooledItem.Free ;
      raise ;
    end ;
  end ;
end;


function TtiPool.Lock: TPooledItem;
var
  lPool : TList ;
  i : integer ;
  lItem : TPooledItem ;
begin
  result := nil ;

  // Wait for a semaphore
  {$IFDEF MSWINDOWS}
  if WaitForSingleObject( FSemaphore, FWaitTime * 1000 ) = WAIT_TIMEOUT then 
    raise EtiOPFInternalException.Create(cErrorTimedOutWaitingForSemaphore) ;

  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Log('About to call sem_wait', lsConnectionPool);
  if sem_wait(FSemaphore) <> 0 then
    raise EtiOPFInternalException.Create(cErrorTimedOutWaitingForSemaphore);
  Log('Successfully passed the sem_wait call', lsConnectionPool);
  {$ENDIF LINUX}

  // A semaphore was available, so get a PooledItem
  lPool := FPool.LockList ;

  try
    // Scan the list of PooledItem to find one which is not locked.
    for i := 0 to lPool.Count - 1 do
    begin
      // There was a PooledItem which was not locked available, so lock it
      // and exit.
      lItem := ( TObject( lPool.Items[i] ) as TPooledItem ) ;
      if ( not lItem.Locked ) and
         ( not lItem.MustRemoveItemFromPool(lPool.Count)) then
      begin
        result := lItem ;
        result.Locked := true ;
        result.Index := I ;
        Log( 'PooledItem #' + intToStr( I ) + ' Locked.', lsConnectionPool ) ;
        Break ; //==>
      end ;
    end ;

    // There was a semaphore available, but no PooledItem, so there is room
    // in the pool to create another.
    if ( result = nil ) and
       ( lPool.Count < FiMaxPoolSize ) then begin
      result := AddItem ;
      result.Locked := true ;
      Log('A new PooledItem has been added to the pool.', lsConnectionPool);
      Log('PooledItem #' + intToStr(lPool.Count) + ' locked.', lsConnectionPool);
      Exit ; //==>
    end ;

    // If we get here, the semahpore system and the pool area
    // out of sync, so raise an exception.
    if Result = nil then
      LogError( 'Semaphore was available but no items ' +
                'available in the pool. MaxPoolSize: %d, Current pool size: %d' +
                'Called in TtiPool.Lock',
                [FiMaxPoolSize, lPool.Count]) ;

  finally
    FPool.UnLockList;
  end ;
end;


procedure TtiPool.SetMaxPoolSize(const Value: integer);
begin
  FiMaxPoolSize := Value;
  CreatePoolSemaphore;
end;


procedure TtiPool.UnLock( pPooledItem : TPooledItem ) ;
var
  i : integer ;
  lList : TList ;
  {$IFDEF LINUX}
  error: integer;
  {$ENDIF LINUX}
begin
  if pPooledItem = nil then
    LogError( 'Nil PooledItem passed to TtiPool.UnLock' ) ;

  if not pPooledItem.Locked then
    LogError( 'Attempting to unlock a PooledItem which is not locked.' ) ;

  lList := FPool.LockList ;
  try
    i := lList.IndexOf( pPooledItem ) ;
    if i = -1 then
      LogError( 'Attempting to unlock a PooledItem which can ' +
                'not be found in the pool. ' +
                'Called in TtiPool.SetPoolSize' ) ;
    pPooledItem.Locked := false ;
    Log( 'PooledItem #' +
         IntToStr( pPooledItem.Index ) +
         ' Unlocked.', lsConnectionPool ) ;
  finally
    FPool.UnLockList ;
  end;

  {$IFDEF MSWINDOWS}
  ReleaseSemaphore( FSemaphore, 1, nil ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_post(FSemaphore);
  if error <> 0 then
    raise Exception.Create('Failed to unlock the semaphore');
  {$ENDIF LINUX}
end;


function TtiPool.GetCount: integer;
var
  lList : TList ;
begin
  lList := FPool.LockList ;
  try
    result := lList.Count ;
  finally
    FPool.UnLockList ;
  end ;
end;


{ TPooledItem }

constructor TPooledItem.Create( pOwner : TtiPool ) ;
begin
  inherited Create ;
  Index := -1 ;
  Locked := false ;
  FOwner := pOwner ;
  FdtLastused := Now ;
end;


destructor TPooledItem.destroy;
begin
  FData.Free ;
  inherited;
end;


function TPooledItem.GetSecInUse: integer;
begin
  result := Trunc(( Now - LastUsed ) * 24 * 60 * 60 ) ;
end;


function TPooledItem.MustRemoveItemFromPool(pListCount: Integer): boolean;
var
  lNotLocked:   Boolean;
  lTimeOut:     Boolean;
  lMinPoolSize: Boolean;
begin
  lNotLocked   := ( not Locked );
  lTimeOut     := ( SecToTimeOut <= 0 );
  lMinPoolSize := ( pListCount > Owner.MinPoolSize ) ;
  result       := lNotLocked and lTimeOut and lMinPoolSize ;
end;


function TPooledItem.SecToTimeOut: integer;
begin
  if Locked then
    result := cSecToTimeOutLocked
  else
    result := Trunc( Owner.TimeOut * 60 ) - SecInUse ;
end;


procedure TPooledItem.SetLocked(const Value: boolean);
begin
  if FbLocked and not Value then
    Lastused := now ;
  FbLocked := Value;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdPoolMonitor
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TThrdPoolMonitor.CreateExt(pPool: TtiPool);
begin
  Create( true ) ;
  FreeOnTerminate := false ;
  FPool := pPool ;
  Priority := tpLowest ;
  resume ;
end ;


procedure TThrdPoolMonitor.Execute;
begin
  while SleepAndCheckTerminated(10000) do
    FPool.SweepForTimeOuts ;
end;


// Scan the pool for items which have timed out and should be removed from
// the pool
procedure TtiPool.SweepForTimeOuts;
var
  lList : TList ;
  i : integer ;
  lPooledItem : TPooledItem ;
  lCount : integer ;
begin
  lCount := Count ;
  if lCount <= MinPoolSize then
    Exit ; //==>
  // TimeOut is in minutes, so convert to TDateTime
  lList := FPool.LockList ;
  try
    for i := lList.Count - 1 downto 0 do begin
      lPooledItem := TPooledItem( lList.Items[i] ) ;
      if lPooledItem.MustRemoveItemFromPool(lList.Count) then
      begin
        Log( 'Pooled item (' + ClassName + ') #' +
             IntToStr( lPooledItem.Index ) +
             ' being removed from the pool.', lsConnectionPool ) ;
        lList.Delete( i ) ;
        lPooledItem.Free ;
        Log( 'There are ' +
             IntToStr( lList.Count ) +
             ' items left in the pool.', lsConnectionPool ) ;
      end ;
    end ;
  finally
    FPool.UnLockList
  end ;
end;


procedure TtiPool.UnLockByData(const pData: TObject);
var
  i : integer ;
  lList : TList ;
  lPooledItem : TPooledItem ;
begin
  lPooledItem := nil ;
  lList := FPool.LockList ;
  try
    for i := 0 to lList.Count - 1 do
      if TPooledItem(lList.Items[i]).Data = pData then
      begin
        lPooledItem := TPooledItem(lList.Items[i]) ;
        Break ; //==>
      end;
  finally
    FPool.UnLockList ;
  end;
  if lPooledItem = nil then
    raise EtiOPFInternalException.Create(cErrorPoolUnlockByData_BadData);
  UnLock(lPooledItem);
end;


procedure TtiPool.CreatePoolSemaphore;
begin
  {$IFDEF MSWINDOWS}
  if FSemaphore <> 0 then
    CloseHandle( FSemaphore ) ;
  FSemaphore := CreateSemaphore( nil, FiMaxPoolSize, FiMaxPoolSize, nil );
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  sem_destroy( FSemaphore );

  if sem_init(FSemaphore, 0, 1) <> 0 then
    raise Exception.Create('Failed to create the semaphore');
  {$ENDIF LINUX}
end;


function TtiPool.GetCountLocked: integer;
var
  lList : TList ;
  i : integer ;
begin
  result := 0 ;
  lList := FPool.LockList ;
  try
    for i := 0 to lList.Count - 1 do
      if TPooledItem(lList.Items[i]).Locked then
        Inc(Result);
  finally
    FPool.UnLockList ;
  end ;
end;

procedure TtiPool.ForEachPooledItem(const pMethod: TtiPooledItemEvent);
var
  lList : TList ;
  i : integer ;
  lPooledItem : TPooledItem ;
begin
  lList := FPool.LockList ;
  try
    for i := 0 to lList.Count - 1 do
    begin
      lPooledItem := TPooledItem( lList.Items[i] ) ;
      pMethod(lPooledItem);
    end ;
  finally
    FPool.UnLockList
  end ;
end;

procedure TtiPool.Remove(const pPooledItem: TPooledItem);
var
  lList : TList ;
begin
  if pPooledItem.Locked then
    UnLock(pPooledItem);
  lList := FPool.LockList ;
  try
    lList.Remove(pPooledItem);
    pPooledItem.Free;
  finally
    FPool.UnLockList
  end ;
end;

end.

