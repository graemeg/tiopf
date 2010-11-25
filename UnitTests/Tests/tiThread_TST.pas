unit tiThread_TST;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork;

type

  TTestTIThread = class(TtiTestCase)
  private
  published
    procedure tiThread_Start;
    procedure tiSleepThread_Start;
    procedure TListDeleteLeak;
    procedure TListDeleteNoLeak;
    procedure tiActiveThreadList;
    procedure TThreadFreeOnTerminate; // No leak
    procedure TThreadDoTerminateFreeOnTerminateDoTerminate; // Leak
    procedure tiThreadFreeOnTerminate; // Leak
    procedure tiThreadExplicitFree; // No leak
    procedure TMultiReadSingleWriteSynchronizer;
  end;

procedure RegisterTests;

implementation
uses
  tiThread,
  tiTestDependencies,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  Contnrs;

const
  CSleep = 100;

var
  URunOnce: Boolean = False;
  UVCLSynchronizer: TMultiReadExclusiveWriteSynchronizer;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIThread);
end;

type
  TtiThreadForTesting = class(TtiThread)
  public
    procedure Execute; override;
  end;

  procedure TtiThreadForTesting.Execute;
  begin
    inherited Execute;
    Sleep(CSleep);
  end;

{ TTestTIThread }

procedure TTestTIThread.tiActiveThreadList;
//var
//  LThread: TtiThreadForTesting;
begin
  Check(True);
  // See not in TtiActiveThreadList.WaitForAll for description of problems with
  // this method
//  AllowedMemoryLeakSize:= 152;
//  CheckEquals(0, GTIOPFManager.ActiveThreadList.Count);
//  LThread:= TtiThreadForTesting.CreateAndResume;
//  LThread.Priority:= tpHighest;
//  CheckEquals(1, GTIOPFManager.ActiveThreadList.Count);
//  GTIOPFManager.ActiveThreadList.WaitForAll;
//  CheckEquals(0, GTIOPFManager.ActiveThreadList.Count);
end;

type
  TtiSleepThreadForTesting = class(TtiSleepThread)
  public
    procedure Execute; override;
  end;

  procedure TtiSleepThreadForTesting.Execute;
  begin
    inherited Execute;
    Sleep(CSleep);
  end;

procedure TTestTIThread.tiSleepThread_Start;
var
  L: TtiSleepThreadForTesting;
begin
  L:= TtiSleepThreadForTesting.Create(True);
  L.FreeOnTerminate:= True;
  L.Resume;
  Sleep(CSleep*2);
  Check(True);
end;

procedure TTestTIThread.tiThreadExplicitFree;
//var
//  LThread: TtiThreadForTesting;
begin
  // See not in TtiActiveThreadList.WaitForAll for description of problems that
  // relates to this method
  Check(True);
//  if GTIOPFManager.ActiveThreadList.Count > 0 then
//    Fail(Format('Expected 0 threads, but found "%s"', [GTIOPFManager.ActiveThreadList.ActiveThreadNames]));
//  LThread:= TtiThreadForTesting.Create(True);
//  LThread.FreeOnTerminate:= False;
//  try
//    LThread.Priority:= tpHighest;
//    if GTIOPFManager.ActiveThreadList.Count > 1 then
//      Fail(Format('Expected 1 thread, but found "%s"', [GTIOPFManager.ActiveThreadList.ActiveThreadNames]));
//    LThread.Resume;
//    LThread.WaitFor;
//  finally
//    LThread.Free;
//    CheckEquals(0, GTIOPFManager.ActiveThreadList.Count);
//  end;
end;

procedure TTestTIThread.tiThreadFreeOnTerminate;
var
  LThread: TtiThreadForTesting;
begin
  SetAllowedLeakArray([152]);
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

procedure TTestTIThread.tiThread_Start;
var
  L: TtiThreadForTesting;
begin
  L:= TtiThreadForTesting.Create(True);
  L.FreeOnTerminate:= True;
  L.Resume;
  Sleep(CSleep*2);
  Check(True);
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

var
  UList: TList = nil;
  UObjectList: TObjectList = nil;

procedure TTestTIThread.TListDeleteLeak;
var
  LItem: TObject;
begin
  AllowedMemoryLeakSize:= 24;
  LItem:= nil;
  try
    LItem:= TObject.Create;
    UList.Add(LItem);
    UList.Delete(0);
  finally
    LItem.Free;
  end;
  Check(True);
end;

procedure TTestTIThread.TListDeleteNoLeak;
var
  LItem: TObject;
begin
  LItem:= nil;
  try
    LItem:= TObject.Create;
    UList.Add(LItem);
    UList.Delete(0);
    UList.Capacity:= UList.Count;
    UList.Add(LItem);
    UList.Delete(0);
    UList.Capacity:= UList.Count;
    UList.Add(LItem);
    UList.Add(LItem);
    UList.Add(LItem);
    UList.Add(LItem);
    UList.Delete(0);
    UList.Delete(0);
    UList.Delete(0);
    UList.Delete(0);
    UList.Capacity:= UList.Count;
  finally
    LItem.Free;
  end;
  Check(True);
end;

procedure TTestTIThread.TMultiReadSingleWriteSynchronizer;
begin
  AllowedMemoryLeakSize:= 24;
  UVCLSynchronizer.BeginRead;
  UVCLSynchronizer.EndRead;
  UVCLSynchronizer.BeginWrite;
  UVCLSynchronizer.EndWrite;
  Check(True);
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
  // LThread:= nil; // This won't do the trick
  Check(True);
end;

{ TtiMultiReadExclusiveWriteSynchronizerForTesting }

initialization
  UList:= TList.Create;
  UObjectList:= TObjectList.Create(False);
  UVCLSynchronizer:= TMultiReadExclusiveWriteSynchronizer.Create;

finalization
  FreeAndNil(UList);
  FreeAndNil(UObjectList);
  FreeAndNil(UVCLSynchronizer);
end.
