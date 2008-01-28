unit tiGeneratorThread;

{$I tiDefines.inc}

interface

uses
  tiVisitor,
  Classes;

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string;
  AOnNextOID: TNotifyEvent; const ANumThreads: Byte);


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
  NUM_CALLS = 5;   // 5 is the minimum for testing, 100 will give a more indepth test

var
  UThreads : array of TGeneratorThread;
  UThreadError: string;
  UOIDs: TStrings;
  UCritSect: TCriticalSection;
  UTerminateThreads: boolean;
  UThreadCount: integer;

procedure RunTest(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string;
  AOnNextOID: TNotifyEvent; const ANumThreads: Byte);
var
  i: integer;
  LThread: TGeneratorThread;
begin
  UThreadError:= '';
  UTerminateThreads:= false;
  try
    SetLength(UThreads, ANumThreads);
    UThreadCount:= ANumThreads;
    for i := 0 to ANumThreads-1 do
    begin
      LThread:= TGeneratorThread.Create(true);
      UThreads[i]:= LThread;
      LThread.FVistorName:= AVistorName;
      LThread.FDatabase:= ADatabase;
      LThread.FPerLayer:= APerLayer;
      LThread.FNextOID:= ANextOIDClass.Create;
      LThread.FOnNextOID:= AOnNextOID;
      LThread.Resume;
    end;

  finally
    for i := 1 to 600 do
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

procedure TestOIDGenerator(ANextOIDClass: TtiVisitedClass; const AVistorName, ADatabase, APerLayer: string;
  AOnNextOID: TNotifyEvent; const ANumThreads: Byte);
begin
  UCritSect:= TCriticalSection.create;
  try
    UOIDs:= TStringList.Create;
    try
      RunTest(ANextOIDClass, AVistorName, ADatabase, APerLayer, AOnNextOID, ANumThreads);
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


