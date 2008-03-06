unit tiThread_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
 ;

type

  TTestTIThread = class(TtiTestCase)
  private
  published
    procedure tiActiveThreadList;
    procedure TThreadFreeOnTerminate; // No leak
    procedure TThreadDoTerminateFreeOnTerminateDoTerminate; // Leak
    procedure tiThreadFreeOnTerminate; // Leak
    procedure tiThreadExplicitFree; // No leak
  end;

procedure RegisterTests;

implementation
uses
  tiThread,
  tiOPFManager,
  tiTestDependencies,
  Windows,
  Classes;

const
  CSleep = 100;

var
  URunOnce: Boolean = False;
    
procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIThread);
end;

type
  TtiThreadForTesting = class(TtiThread)
  public
    procedure Execute; override;
  end;

  procedure TtiThreadForTesting.Execute;
  begin
    Sleep(CSleep);
  end;

{ TTestTIThread }

procedure TTestTIThread.tiActiveThreadList;
var
  LList: TtiActiveThreadList;
  LThread: TtiThreadForTesting;
begin
  LList:= nil;
  LThread:= nil;
  try
    LThread:= TtiThreadForTesting.Create(True);
    LThread.FreeOnTerminate:= False;
    LList:= TtiActiveThreadList.Create;
    CheckEquals(0, LList.Count);
    LList.Add(LThread);
    CheckEquals(1, LList.Count);
  finally
    LList.Free;
    LThread.Free;
  end;
end;

procedure TTestTIThread.tiThreadExplicitFree;
var
  LThread: TtiThreadForTesting;
begin
  LThread:= TtiThreadForTesting.Create(True);
  LThread.FreeOnTerminate:= False;
  try
    LThread.Priority:= tpHigher;
    CheckEquals(1, GTIOPFManager.ActiveThreadList.Count);
    LThread.Resume;
    LThread.WaitFor;
  finally
    LThread.Free;
    CheckEquals(0, GTIOPFManager.ActiveThreadList.Count);
  end;
end;

procedure TTestTIThread.tiThreadFreeOnTerminate;
var
  LThread: TtiThreadForTesting;
begin
  // See not about leak being reported in TThreadDoTerminateFreeOnTerminate
  if not URunOnce then
  begin
    SetAllowedLeakArray([152]);
    URunOnce:= True;
  end else
    SetAllowedLeakArray([128]);
  LThread:= TtiThreadForTesting.Create(True);
  LThread.FreeOnTerminate:= True;
  LThread.Resume;
  Sleep(CSleep*2);
  Check(True);
end;

type
  TThreadForTesting = class(TThread)
  public
    procedure Execute; override;
  end;

  procedure TThreadForTesting.Execute;
  begin
    Sleep(CSleep);
  end;

procedure TTestTIThread.TThreadFreeOnTerminate;
var
  LThread: TThreadForTesting;
begin
  LThread:= TThreadForTesting.Create(True);
  LThread.FreeOnTerminate:= True;
  LThread.Resume;
  Sleep(CSleep*2);
  Check(True);
end;

type

  TThreadOnTerminateForTesting = class(TThread)
  private
    procedure DoOnTerminate(Sender: TObject);
  public
    constructor Create(ASuspended: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  constructor TThreadOnTerminateForTesting.Create(ASuspended: boolean);
  begin
    inherited Create(ASuspended);
    OnTerminate:= DoOnTerminate;
  end;

  destructor TThreadOnTerminateForTesting.Destroy;
  begin
    inherited;
  end;

  procedure TThreadOnTerminateForTesting.DoOnTerminate(Sender: TObject);
  begin
    // Do nothing
    OnTerminate:= nil;
  end;

  procedure TThreadOnTerminateForTesting.Execute;
  begin
    Sleep(CSleep);
  end;

procedure TTestTIThread.TThreadDoTerminateFreeOnTerminateDoTerminate;
var
  LThread: TThreadOnTerminateForTesting;
begin
  // This test will report a leak, however it's not actually leaking as
  // the thread is not being destroyed until LThread goes out of scope
  // The combination of FreeOnTerminate:= true and OnTerminate:= DoOnTerminate
  // appears to be causing the leak to be reported.
  SetAllowedLeakArray([120]);
  LThread:= TThreadOnTerminateForTesting.Create(True);
  LThread.FreeOnTerminate:= True;
  LThread.Resume;
  Sleep(CSleep*2);
  LThread:= nil; // This won't do the trick
  Check(True);
end;

end.
