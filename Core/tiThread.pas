unit tiThread;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiBaseObject
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,SyncObjs   // This unit must always appear after the Windows unit!
 ;

const
  cDefaultSleepResponse = 100; // Will check Terminated while sleeping every 100ms

type
  // ToDo: TtiThread should be renamed to TtiRegisteredThread and descend
  //       from TtiSleepThread
  TtiSleepThread = class(TThread)
  private
    FSleepResponse: Cardinal;
    FName: string;
  protected
    function SleepAndCheckTerminated(ASleepFor: Cardinal): boolean;
  public
    constructor Create(ASuspended: boolean); virtual;
    destructor  Destroy; override;
    procedure   SetThreadName(const AName: string);
  end;

  TtiThread = class(TtiSleepThread)
  protected
    procedure   DoOnTerminate(Sender: TObject); virtual;
  public
    constructor Create; reintroduce; overload;
    constructor Create(ACreateSuspended: Boolean); overload; override;
    constructor CreateAndResume; virtual; // See note in body of method
    destructor  Destroy; override;
  end;


  TtiActiveThreadList = class(TtiBaseObject)
  private
    FList : TObjectList;
    FCritSect : TCriticalSection;
    FOnThreadCountChange: TThreadMethod;
    FThreadCount: integer;
    procedure DoThreadCountChange(AThreadCount: Integer; const AThread: TThread);
    procedure SetOnThreadCountChange(const AValue: TThreadMethod);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(const AThread : TtiThread);
    procedure   Remove(const AThread : TtiThread);
    procedure   Terminate;
    property    OnThreadCountChange: TThreadMethod read FOnThreadCountChange write SetOnThreadCountChange;
    property    Count: integer read FThreadCount;
  end;

{$IFDEF MSWINDOWS}
procedure SetIdeDebuggerThreadName(AThreadID: DWORD; AThreadName: PChar);
{$ENDIF}


implementation
uses
  tiOPFManager
  {$IFDEF MSWINDOWS}
  ,tiWin32
  {$ENDIF MSWINDOWS}
  ,tiUtils
  ,SysUtils    // Used by FPC for the Sleep method.
 ;


{$IFDEF MSWINDOWS}
procedure SetIdeDebuggerThreadName(AThreadID: DWORD; AThreadName: PChar);
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := AThreadName;
  ThreadNameInfo.FThreadID := $FFFFFFFF; //ThreadID;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord), @ThreadNameInfo);
  except
  end;
end;
{$ELSE}
procedure SetIdeDebuggerThreadName(AThreadID: Integer; const AThreadName: string);
begin
  // In .NET there is a method... thread.setname or something
  // In Linux - who knows
end;
{$ENDIF}


{ TtiThread }

constructor TtiThread.CreateAndResume;
begin
  // When overriding CreateAndResume, don't call inherited as any code
  // after the inherited call may execute after the thread starts running.
  // Have never seen an error caused by this, but the code smells of trouble.
  Create(true);
  resume;
end;


constructor TtiThread.Create(ACreateSuspended: Boolean);
begin
  inherited Create(ACreateSuspended);
  FreeOnTerminate := true;
  OnTerminate := DoOnTerminate;
  gTIOPFManager.ActiveThreadList.Add(Self);
end;


destructor TtiThread.Destroy;
begin                         
  {$IFDEF MSWINDOWS}
  tiWin32CoUnInitialize; // You never know, perhpas COM was used in this thread.
  {$ENDIF}
  gTIOPFManager.ActiveThreadList.Remove(Self);
  inherited;
end;


procedure TtiThread.DoOnTerminate(Sender: TObject);
begin
  // Implement in the concrete
end;

constructor TtiThread.Create;
begin
  Create(true);
end;

procedure TtiSleepThread.SetThreadName(const AName: string);
begin
  //Assert(GetCurrentThreadId = Self.ThreadID); //<-- always fail
  FName := AName; // Need to store the name in the heap
  SetIdeDebuggerThreadName(Self.ThreadID, PChar(FName));
end;


{ TtiActiveThreadList }

procedure TtiActiveThreadList.Add(const AThread: TtiThread);
begin
  FCritSect.Enter;
  try
    FList.Add(AThread);
    DoThreadCountChange(FList.Count, AThread);
  finally
    FCritSect.Leave;
  end;
end;


constructor TtiActiveThreadList.Create;
begin
  inherited;
  FList := TObjectList.Create(false);
  FCritSect := TCriticalSection.Create;
  FThreadCount:= 0;
end;

destructor TtiActiveThreadList.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;

procedure TtiActiveThreadList.DoThreadCountChange(AThreadCount: integer; const AThread: TThread);
begin
  if FThreadCount <> AThreadCount then
  begin
    FThreadCount:= AThreadCount;
    if Assigned(FOnThreadCountChange) then
      TThread.Synchronize(AThread, FOnThreadCountChange);
  end;
end;

procedure TtiActiveThreadList.Remove(const AThread: TtiThread);
var
  LIndex: integer;
begin
  FCritSect.Enter;
  try
    LIndex:= FList.IndexOf(AThread);
    if LIndex <> -1 then
    begin
      FList.Delete(LIndex);
      DoThreadCountChange(FList.Count, AThread);
    end;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiActiveThreadList.SetOnThreadCountChange(const AValue: TThreadMethod);
begin
  FCritSect.Enter;
  try
    FOnThreadCountChange := AValue;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiActiveThreadList.Terminate;
var
  i : integer;
begin
  FCritSect.Enter;
  try
    for i := FList.Count - 1 downto 0 do
      (FList.Items[i] as TtiThread).Terminate;
  finally
    FCritSect.Leave;
  end;
end;




{ TtiSleepThread }

constructor TtiSleepThread.Create(ASuspended: boolean);
begin
  inherited;
  FSleepResponse:= cDefaultSleepResponse;
end;


function TtiSleepThread.SleepAndCheckTerminated(ASleepFor: Cardinal): boolean;
var
  LStart: DWord;
begin
  LStart := tiGetTickCount;
  while ((tiGetTickCount - LStart) <= ASleepFor) and (not Terminated) do
    Sleep(FSleepResponse);
  Result := not Terminated;
end;


destructor TtiSleepThread.Destroy;
begin
  // If it's a low priority thread, it may never finish running so it can
  // be destroyed. (Destroy calls WaitFor)
  // This became an issue with Delphi 6 apps hanging on shutdown.
  if Priority < tpNormal then
    Priority := tpNormal;
  inherited;
end;


end.


