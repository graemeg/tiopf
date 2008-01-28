unit tiGeneratorThread;

{$I tiDefines.inc}

interface

uses
  tiVisitor,
  Classes;

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);


implementation

uses
  tiOPFManager,
  {$ifdef fpc}
  testregistry,
  {$endif}
  SysUtils,
  SyncObjs;

type

  TGeneratorThread = class(TThread)
  private
    FNextOID: TtiVisited;
    FVistorName: string;
    FOnNextOID: TNotifyEvent;
    FDatabase: string;
    FPerLayer: string;
  protected
    procedure DoOnNextOID(Sender: TObject);
    procedure DoOnTerminate;
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

const
  NUM_THREADS = 3; // 3 is the minimum for testing, 10 will give a more indepth test
  NUM_CALLS = 5;   // 5 is the minimum for testing, 100 will give a more indepth test

var
  Threads : array[1.. NUM_THREADS] of TGeneratorThread;
  UThreadError: string;
  UOIDs: TStrings;
  UCritSect: TCriticalSection;
  UTerminateThreads: boolean;
  UThreadCount: integer;

procedure RunTest(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);
var
  f: integer;
  thread: TGeneratorThread;
begin
  UThreadError:= '';
  UTerminateThreads:= false;
  try
    UThreadCount:= NUM_THREADS;
    for f := 1 to NUM_THREADS do
    begin
      thread:= TGeneratorThread.Create(true);
      Threads[f]:= thread;
      thread.FVistorName:= AVistorName;
      thread.FDatabase:= ADatabase;
      thread.FPerLayer:= APerLayer;
      thread.FNextOID:= ANextOIDClass.Create;
      thread.FOnNextOID:= AOnNextOID;
      thread.Resume;
    end;

  finally
    for f := 1 to 600 do
    begin
      if (UThreadCount = 0) then
        break;
      Sleep(100);
    end;
  end;
  Sleep(100);

  if UThreadError <> '' then
    raise Exception.Create(UThreadError);
end;

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);
begin
  UCritSect:= TCriticalSection.create;
  try
    UOIDs:= TStringList.Create;
    try
      RunTest(ANextOIDClass, AVistorName, ADatabase, APerLayer, AOnNextOID);
    finally
      FreeAndNil( UOIDs );
    end;
  finally
    FreeAndNil( UCritSect );
  end;
end;

{ TGeneratorThread }

destructor TGeneratorThread.Destroy;
begin
  FNextOID.Free;
  inherited;
end;

procedure TGeneratorThread.DoOnNextOID(Sender: TObject);
begin
  UCritSect.Enter;
  try
    FOnNextOID(Sender);
  finally
    UCritSect.Leave;
  end;
end;

procedure TGeneratorThread.DoOnTerminate;
begin
  UCritSect.Enter;
  try
    dec(UThreadCount);
  finally
    UCritSect.Leave;
  end;

end;

procedure TGeneratorThread.Execute;
var f: integer;
begin
  inherited;
  try
    FreeOnTerminate:= true;
    try
      for f := 1 to NUM_CALLS do
      begin
        if UTerminateThreads then
          exit;
        
        gTIOPFManager.VisitorManager.Execute(FVistorName, FNextOID, FDatabase, FPerLayer);
        DoOnNextOID(FNextOID);
      end;
    except
      on e: Exception do
      begin
        UThreadError:= e.Message;
        UTerminateThreads:= true;
      end;
    end;
  finally
    DoOnTerminate;
  end;
end;

end.


