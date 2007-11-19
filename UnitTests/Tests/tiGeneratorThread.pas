unit tiGeneratorThread;

interface

uses
  TestFramework, SysUtils, tiOID, tiQuery, Classes, tiOPFManager,
  SyncObjs, tiVisitor;

type
  TGeneratorThread = class(TThread)
  private
  protected
    procedure DoOnNextOID(Sender: TObject);
    procedure DoOnTerminate;
    procedure Execute; override;
  public
    NextOID: TtiVisited;
    VistorName: string;
    OnNextOID: TNotifyEvent;
    Database: string;
    PerLayer: string;
    destructor Destroy; override;
  end;

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);


implementation

uses tiConstants, tiOIDInteger;

const
  NUM_THREADS = 3; // 3 is the minimum for testing, 10 will give a more indepth test
  NUM_CALLS = 5;   // 5 is the minimum for testing, 100 will give a more indepth test

var
  Threads : array[1.. NUM_THREADS] of TGeneratorThread;
    FThreadError: string;
    FOIDs: TStrings;
    FCritSect: TCriticalSection;
    TerminateThreads: boolean;
    ThreadCount: integer;

procedure RunTest(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);
var
  f: integer;
  thread: TGeneratorThread;
begin
  FThreadError:= '';
  TerminateThreads:= false;
  try
    ThreadCount:= NUM_THREADS;
    for f := 1 to NUM_THREADS do
    begin
      thread:= TGeneratorThread.Create(true);
      Threads[f]:= thread;
      thread.VistorName:= AVistorName;
      thread.Database:= ADatabase;
      thread.PerLayer:= APerLayer;
      thread.NextOID:= ANextOIDClass.Create;
      thread.OnNextOID:= AOnNextOID;
      thread.Resume;
    end;

  finally
    for f := 1 to 600 do
    begin
      if (ThreadCount = 0) then
        break;
      Sleep(100);
    end;
  end;
  Sleep(100);

  if FThreadError <> '' then
    raise Exception.Create(FThreadError);
end;

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string; AOnNextOID: TNotifyEvent);
begin
  FCritSect:= TCriticalSection.create;
  try
    FOIDs:= TStringList.Create;
    try
      RunTest(ANextOIDClass, AVistorName, ADatabase, APerLayer, AOnNextOID);
    finally
      FreeAndNil( FOIDs );
    end;
  finally
    FreeAndNil( FCritSect );
  end;
end;

{ TGeneratorThread }

destructor TGeneratorThread.Destroy;
begin
  inherited;
end;

procedure TGeneratorThread.DoOnNextOID(Sender: TObject);
begin
  FCritSect.Enter;
  try
    OnNextOID(Sender);
  finally
    FCritSect.Leave;
  end;
end;

procedure TGeneratorThread.DoOnTerminate;
begin
  FCritSect.Enter;
  try
    dec(ThreadCount);
  finally
    FCritSect.Leave;
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
        if TerminateThreads then
          exit;
        
        gTIOPFManager.VisitorManager.Execute(VistorName, NextOID, Database, PerLayer);
        DoOnNextOID(NextOID);
      end;
    except
      on e: Exception do
      begin
        FThreadError:= e.Message;
        TerminateThreads:= true;
      end;
    end;
  finally
    DoOnTerminate;
  end;
end;

end.

