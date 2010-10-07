unit tiSyncObjs_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework
  ,tiObject
  ,tiSyncObjs
  ,Classes
 ;


type
  TTestTISyncObjs = class(TtiTestCase)
  private
  protected
  published
    procedure tiObjectLockList_SingleThread;
    procedure tiObjectLockList_MultiThread;
  end;


  TThreadTestObjectLockList = class(TThread)
  private
    FOLL: TtiObjectLockList;
    FO: TtiObject;
  public
    constructor Create(AOLL: TtiObjectLockList; AO: TtiObject);
    procedure   Execute; override;
  end;


procedure RegisterTests;


implementation
uses
  tiTestDependencies
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,SysUtils     // used for Sleep under Free Pascal
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTISyncObjs);
end;

{ TTestTISyncObjs }

procedure TTestTISyncObjs.tiObjectLockList_MultiThread;
var
  LOLL: TtiObjectLockList;
  LO: TtiObject;
begin
  LOLL:= TtiObjectLockList.Create;
  try
    LO:= TtiObject.Create;
    try
      LOLL.Lock(LO);
      Check(not LOLL.TryLock(LO));
      TThreadTestObjectLockList.Create(LOLL, LO);
      LOLL.Lock(LO);
    finally
      LO.Free;
    end;
  finally
    LOLL.Free;
  end;
end;


procedure TTestTISyncObjs.tiObjectLockList_SingleThread;
var
  LOLL: TtiObjectLockList;
  LO: TtiObject;
begin
  LOLL:= TtiObjectLockList.Create;
  try
    LO:= TtiObject.Create;
    try
      LOLL.Lock(LO);
      Check(not LOLL.TryLock(LO));
      LOLL.UnLock(LO);
      Check(LOLL.TryLock(LO));
    finally
      LO.Free;
    end;
  finally
    LOLL.Free;
  end;
end;


{ TThreadTestObjectLockList }

constructor TThreadTestObjectLockList.Create(AOLL: TtiObjectLockList; AO: TtiObject);
begin
  inherited Create(False);
  FreeOnTerminate:= True;
  FOLL:= AOLL;
  FO:= AO;
end;


procedure TThreadTestObjectLockList.Execute;
begin
  Sleep(2000);
  FOLL.UnLock(FO);
end;

end.

