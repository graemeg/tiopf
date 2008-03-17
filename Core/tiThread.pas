unit tiThread;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiBaseObject
  ,tiObject
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,SyncObjs
  ,SysUtils   // This unit must always appear after the Windows unit!
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

  TtiThreadList = class(TObjectList)
  protected
    function    GetItem(Index: integer): TtiThread; reintroduce;
    procedure   SetItem(Index: integer; const Value: TtiThread); reintroduce;
  public
    {: Performs the class method AMethod on every thread in the list.}
    procedure   ForEach(AMethod: TObjForEachMethod); overload;
    {: Performs the method AMethod on every thread in the list.}
    procedure   ForEach(AMethod: TObjForEachMethodRegular); overload;
    property    Items[Index:integer]: TtiThread read GetItem write SetItem;
    procedure   WaitForAll;
    procedure   ResumeAll;
  end;

  TtiActiveThreadList = class(TtiBaseObject)
  private
    FList: TtiThreadList;
    FCritSect : TCriticalSection;
    FOnThreadCountChange: TThreadMethod;
    FThreadCount: integer;
    procedure DoThreadCountChange(AThreadCount: Integer; const AThread: TThread);
    procedure SetOnThreadCountChange(const AValue: TThreadMethod);
    function GetThreadCount: integer;
    function GetActiveThreadNames: string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(const AThread : TtiThread);
    procedure   Remove(const AThread : TtiThread);
    procedure   Terminate;
    {: Performs the class method AMethod on every thread in the list.}
    procedure   ForEach(AMethod: TObjForEachMethod); overload;
    {: Performs the method AMethod on every thread in the list.}
    procedure   ForEach(AMethod: TObjForEachMethodRegular); overload;
    property    OnThreadCountChange: TThreadMethod read FOnThreadCountChange write SetOnThreadCountChange;
    property    Count: integer read GetThreadCount;
    property    ActiveThreadNames: string read GetActiveThreadNames;
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
  FName := AName; // Need to store the name in the heap
  SetIdeDebuggerThreadName(Self.ThreadID, PChar(FName));
end;

{ TtiThreadList }

procedure TtiThreadList.ForEach(AMethod: TObjForEachMethod);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    AMethod(Items[i]);
end;

procedure TtiThreadList.ForEach(AMethod: TObjForEachMethodRegular);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    AMethod(Items[i]);
end;

function TtiThreadList.GetItem(Index: integer): TtiThread;
begin
  result := TtiThread(inherited GetItem(Index));
end;

procedure TtiThreadList.ResumeAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Resume;
end;

procedure TtiThreadList.SetItem(Index: integer; const Value: TtiThread);
begin
  inherited SetItem(Index, Value);
end;

procedure TtiThreadList.WaitForAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].WaitFor;
end;

{ TtiActiveThreadList }

procedure TtiActiveThreadList.Add(const AThread: TtiThread);
begin
  FCritSect.Enter;
  try
    FList.Add(AThread);
    FList.Capacity:= FList.Count; // To suppress a memory leak being reported by DUnit2
    DoThreadCountChange(FList.Count, AThread);
  finally
    FCritSect.Leave;
  end;
end;


constructor TtiActiveThreadList.Create;
begin
  inherited;
  FList := TtiThreadList.Create;
  FList.OwnsObjects := False;
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

procedure TtiActiveThreadList.ForEach(AMethod: TObjForEachMethod);
begin
  FCritSect.Enter;
  try
    FList.ForEach(AMethod);
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiActiveThreadList.ForEach(AMethod: TObjForEachMethodRegular);
begin
  FCritSect.Enter;
  try
    FList.ForEach(AMethod);
  finally
    FCritSect.Leave;
  end;
end;

function TtiActiveThreadList.GetActiveThreadNames: string;
var
  i: Integer;
begin
  FCritSect.Enter;
  try
    result:= '';
    for i := 0 to FList.Count-1 do
    begin
      if result <> '' then
        result:= result + CrLf;
      result:= result + FList.Items[i].ClassName;
    end;
  finally
    FCritSect.Leave;
  end;
end;

function TtiActiveThreadList.GetThreadCount: integer;
begin
  FCritSect.Enter;
  try
    result:= FThreadCount;
  finally
    FCritSect.Leave;
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
      FList.Capacity:= FList.Count; // To suppress a memory leak being reported by DUnit2
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
      FList.Items[i].Terminate;
  finally
    FCritSect.Leave;
  end;
end;


{ TtiSleepThread }

constructor TtiSleepThread.Create(ASuspended: boolean);
begin
  inherited Create(ASuspended);
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
  inherited Destroy;
end;

end.

