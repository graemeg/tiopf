unit tiPool_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework
  ,tiPool
  ,tiBaseObject
  ,Classes
  ,Contnrs
 ;

type

  TTestPoolItemData = class(TtiBaseObject);

  TThresPoolThread = class(TThread)
  private
    FItemCount: integer;
    FPool: TtiPool;
    FItemList : TObjectList;
    procedure LockItems;
    procedure UnLockItems;
  public
    constructor Create(ACreateSuspended: Boolean);
    destructor  Destroy; override;
    property    Pool : TtiPool read FPool write FPool;
    property    ItemCount : integer read FItemCount write FItemCount;
    procedure   Execute; override;
  end;


  TTestTiPool = class(TtiTestCase)
  private
    procedure CheckCountInPool(const APool: TtiPool; ACount, pCountLocked: integer; pTestID: string);
    procedure DoLock_Unlock(ACount: integer);
  published
    procedure CreateAndDestroy;
    procedure Lock_Unlock_1;
    procedure Lock_Unlock_10;
    procedure Lock_UnLock_Threaded;
    procedure MinPoolSize;
    procedure MaxPoolSize;
    procedure TimeOut;
  end;

  TtiPoolForTesting = class(TtiPool)
  protected
    function    PooledItemClass: TtiPooledItemClass; override;
    procedure   AfterAddPooledItem(const APooledItem: TtiPooledItem); override;
  public
    procedure SweepForTimeOuts; override;
    procedure SetTimeOut(const AValue: Extended); override;
    procedure SetWaitTime(const AValue: Word); override;
  end;

procedure RegisterTests;


implementation
uses
  tiTestDependencies
  {$IFNDEF FPC}
  ,Windows
  {$ENDIF}
  ,SysUtils
 ;

const
  cThreadCount      = 5;
  cThreadItemCount  = 5;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTiPool);
end;


{ TTestTiPool }

procedure TTestTiPool.DoLock_Unlock(ACount : integer);
var
  lPool : TtiPoolForTesting;
  LPoolData : TtiBaseObject;
  lList : TObjectList;
  i    : integer;
begin
  lPool := TtiPoolForTesting.Create(0, ACount);
  try
    lList := TObjectList.Create(false);
    try
      for i := 1 to ACount do
      begin
        LPoolData := lPool.Lock;
        CheckCountInPool(lPool,i,i, IntToStr(i));
        lList.Add(LPoolData);
      end;
      for i := 0 to ACount - 1 do
      begin
        lPool.UnLock(lList.Items[i] as TtiBaseObject);
        CheckCountInPool(lPool,ACount,ACount-i-1, IntToStr(ACount-i));
      end;
    finally
      lList.Free;
    end;
  finally
    lPool.Free;
  end;
end;

procedure TTestTiPool.Lock_Unlock_1;
begin
  DoLock_Unlock(1);
end;

procedure TTestTiPool.Lock_Unlock_10;
begin
  DoLock_Unlock(10);
end;

procedure TTestTiPool.Lock_UnLock_Threaded;
var
  lPool : TtiPoolForTesting;
  i : integer;
  lThrd : TThresPoolThread;
  lList : TObjectList;
begin
  InhibitStackTrace;
  Check(True); // To Force OnCheckCalled to be called
  lList := TObjectList.Create(true);
  try
    lPool := TtiPoolForTesting.Create(0, cThreadCount * cThreadItemCount);
    try
      for i := 1 to cThreadCount do
      begin
        lThrd := TThresPoolThread.Create(true);
        lThrd.ItemCount := cThreadItemCount;
        lThrd.Pool := lPool;
        lList.Add(lThrd);
        lThrd.Resume;
        Sleep(20);
      end;
      for i := 0 to lList.Count - 1 do
        TThread(lList.Items[i]).WaitFor;
    finally
      lPool.Free;
    end;
  finally
    lList.Free;
  end;
end;


procedure TTestTiPool.MaxPoolSize;
var
  lPool : TtiPoolForTesting;
begin
  lPool := TtiPoolForTesting.Create(0, 1);
  try
    lPool.SetWaitTime(1);
    lPool.Lock;
    try
      lPool.Lock;
      Fail(CErrorExceptionNotRaised);
    except
      on e:exception do
      begin
        CheckIs(e, Exception, 'Wrong class of exception');
        CheckFormattedMessage(CErrorTimedOutWaitingForSemaphore,
          [TtiPoolForTesting.ClassName, 0, 1, 1], e.message);
      end;
    end;
  finally
    lPool.Free;
  end;
end;

procedure TTestTiPool.MinPoolSize;
var
  lPool : TtiPoolForTesting;
  lItem1 : TtiBaseObject;
  lItem2 : TtiBaseObject;
begin
  lPool := TtiPoolForTesting.Create(1, 2);
  try
    lPool.SetTimeOut(0);
    lItem1 := lPool.Lock;
    lItem2 := lPool.Lock;
    Check(lItem1 <> lItem2, 'Items should not be same');

    lPool.UnLock(lItem1);
    lPool.UnLock(lItem2);
    Sleep(1000);
    lPool.SweepForTimeOuts;

    lItem2 := lPool.Lock;
    Check(lItem1 = lItem2, 'Items should be same');

  finally
    lPool.Free;
  end;
end;


procedure TTestTiPool.CheckCountInPool(const APool : TtiPool; ACount, pCountLocked : integer; pTestID : string);
var
  lCount : integer;
  lCountLocked : integer;
begin
  lCount := APool.Count;
  lCountLocked := APool.CountLocked;
  CheckEquals(ACount, lCount, 'Count ' + pTestID);
  CheckEquals(pCountLocked, lCountLocked, 'CountLocked ' + pTestID);
end;


procedure TTestTiPool.TimeOut;
var
  lPool : TtiPoolForTesting;
  lItem2 : TtiBaseObject;
  lItem3 : TtiBaseObject;
begin
  lPool := TtiPoolForTesting.Create(0, 3);
  try

    lPool.SetTimeOut(1/60);

    lPool.Lock;
    lPool.SweepForTimeOuts;
    CheckCountInPool(lPool, 1, 1, 'Test #1');

    lItem2 := lPool.Lock;
    lPool.SweepForTimeOuts;
    CheckCountInPool(lPool, 2, 2, 'Test #2');

    lPool.UnLock(lItem2);
    Sleep(2000);
    lPool.SweepForTimeOuts;
    CheckCountInPool(lPool, 1, 1, 'Test #3');

    lItem3 := lPool.Lock;
    lPool.SweepForTimeOuts;
    CheckCountInPool(lPool, 2, 2, 'Test #4');

    lPool.UnLock(lItem3);
    CheckCountInPool(lPool, 2, 1, 'Test #5');

    Sleep(2000);
    lPool.SweepForTimeOuts;
    CheckCountInPool(lPool, 1, 1, 'Test #6');

  finally
    lPool.Free;
  end;
end;


procedure TTestTiPool.CreateAndDestroy;
var
  lPool : TtiPoolForTesting;
begin
  Check(True); // To Force OnCheckCalled to be called
  lPool := TtiPoolForTesting.Create(0, 1);
  lPool.Free;
end;

{ TThresPoolThread }

constructor TThresPoolThread.Create(ACreateSuspended: Boolean);
begin
  inherited Create(ACreateSuspended);
  FreeOnTerminate := false;
  FItemList := TObjectList.Create(false);
end;


destructor TThresPoolThread.Destroy;
begin
  FItemList.Free;
  inherited;
end;

procedure TThresPoolThread.Execute;
begin
  LockItems;
  UnLockItems;
end;


procedure TThresPoolThread.LockItems;
var
  lItem : TtiBaseObject;
  i    : integer;
begin
  for i := 1 to FItemCount do
  begin
    lItem := Pool.Lock;
    FItemList.Add(lItem);
    Sleep(20);
  end;
end;


procedure TThresPoolThread.UnLockItems;
var
  i: integer;
begin
  for i := 0 to FItemCount - 1 do
  begin
    Pool.UnLock(FItemList.Items[i] as TtiPooledItem);
    Sleep(20);
  end;
end;


{ TtiPoolForTesting }

procedure TtiPoolForTesting.AfterAddPooledItem(const APooledItem: TtiPooledItem);
begin
  APooledItem.Data := TTestPoolItemData.Create;
end;

function TtiPoolForTesting.PooledItemClass: TtiPooledItemClass;
begin
  result:= TtiPooledItem;
end;

procedure TtiPoolForTesting.SetTimeOut(const AValue: Extended);
begin
  inherited;
end;

procedure TtiPoolForTesting.SetWaitTime(const AValue: Word);
begin
  inherited;
end;

procedure TtiPoolForTesting.SweepForTimeouts;
begin
  inherited;
end;

end.

