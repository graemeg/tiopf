unit tiThread;

interface
uses
  Classes
  ,tiObjAbs
  ,Contnrs
  ,SyncObjs
  ;

type

  TtiThread = class( TThread )
  private
  protected
    procedure   DoOnTerminate(Sender : TObject); virtual ;
  public
    constructor Create(CreateSuspended: Boolean); overload; virtual ;
    constructor Create ; overload ; virtual ;
    constructor CreateAndResume ; virtual ; // See note in body of method
    destructor  Destroy ; override ;
  end ;

  TtiActiveThreadList = class( TtiObjAbs )
  private
    FList : TObjectList ;
    FCritSect : TCriticalSection ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   Add(const pThrd : TtiThread ) ;
    procedure   Remove(const pThrd : TtiThread ) ;
    procedure   Terminate ;
    function    RunningThreadCount : integer ;
  end ;

implementation
uses
  tiPersist
  ;

{ TtiThread }

constructor TtiThread.CreateAndResume;
begin
  // When overriding CreateAndResume, don't call inherited as any code
  // after the inherited call may execute after the thread starts running.
  // Have never seen an error caused by this, but the code smells of trouble.
  Create(true);
  resume;
end;

constructor TtiThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
  OnTerminate := DoOnTerminate ;
  gtiPerMgr.ActiveThreadList.Add(Self);
end;

destructor TtiThread.Destroy;
begin
  gtiPerMgr.ActiveThreadList.Remove(Self);
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

{ TtiActiveThreadList }

procedure TtiActiveThreadList.Add(const pThrd: TtiThread);
begin
  FCritSect.Enter;
  try
    FList.Add(pThrd);
  finally
    FCritSect.Leave;
  end;
end;

constructor TtiActiveThreadList.Create;
begin
  inherited;
  FList := TObjectList.Create(false) ;
  FCritSect := TCriticalSection.Create ;
end;

destructor TtiActiveThreadList.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;

procedure TtiActiveThreadList.Remove(const pThrd: TtiThread);
begin
  FCritSect.Enter;
  try
    FList.Remove(pThrd);
  finally
    FCritSect.Leave;
  end;
end;

function TtiActiveThreadList.RunningThreadCount: integer;
begin
  FCritSect.Enter;
  try
    result := FList.Count ;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiActiveThreadList.Terminate;
var
  i : integer ;
begin
  FCritSect.Enter;
  try
    for i := FList.Count - 1 downto 0 do
      (FList.Items[i] as TtiThread ).Terminate;
  finally
    FCritSect.Leave;
  end;
end;

end.
