unit tiSyncObjs;

{$I tiDefines.inc}

interface

uses
  tiBaseObject
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows      // Graeme: This must appear before SyncObjs for Free Pascal!
  {$ENDIF}
  ,tiObject
  ,tiExcept
  ,SyncObjs
  ,Classes  // TThreadList
 ;

const
  cDefaultReTryLockFrequency = 5000; // ms

type
  TtiObjectLockList = class(TtiBaseObject)
  private
    FList: TObjectList;
    FCritSect: TCriticalSection;
    FRetryLockFrequency: Word;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Lock(AObject: TtiObject);
    procedure   UnLock(AObject: TtiObject);
    function    TryLock(AObject: TtiObject): Boolean;
    property    RetryLockFrequence: Word Read FRetryLockFrequency Write FRetryLockFrequency;
  end;

  EtiOPFLockTimedOut = class(EtiOPFException);

function  tiWaitForMutex(const AMutexName: string): Boolean; overload;
function  tiWaitForMutex(const AMutexName: string; ASecondsToWait: Word): Boolean; overload;
procedure tiReleaseMutex(const AMutexName: string);
procedure tiFreeThreadList(var AThreadList: TThreadList; const AOwnsContent: boolean);


implementation
uses
  tiUtils
  ,SysUtils    // used for Sleep under Free Pascal
 ;

var
  uMutexes : TThreadList;

type

  TtiMutex = class(TtiBaseObject)
  private
    FMutexName: string;
    FMutexHandle: THandle;
    {$IFDEF UNIX}
    FCriticalSection: TRTLCriticalSection;
//    InitializeMutex(pCritSec: TRTLCriticalSection);
    {$ENDIF}
  public
    constructor Create(const AMutexName: string);
    destructor  Destroy; override;
    property    MutexName: string read FMutexName;
    property    MutexHandle: THandle read FMutexHandle;
  end;


function _MutexName(const AMutexName: string): string;
begin
  Result := tiStrTran(AMutexName, ' ', '');
  Result := tiStrTran(Result,  ':', '');
  Result := tiStrTran(Result,  '/', '');
end;


function tiWaitForMutex(const AMutexName: string): Boolean;
begin
  Result:= tiWaitForMutex(AMutexName, 3*60);
end;


function tiWaitForMutex(const AMutexName: string; ASecondsToWait: Word): Boolean;
var
  lMutexName : string;
  lMutex : TtiMutex;
begin
  lMutexName := _MutexName(AMutexName);
  lMutex := TtiMutex.Create(lMutexName);
  {$IFDEF MSWINDOWS}
  if WaitForSingleObject(lMutex.MutexHandle, ASecondsToWait*1000) = Wait_Object_0 then
  begin
    Result := True;
    uMutexes.Add(lMutex); // Locks the list
  end
  else begin
    Result := False;
    lMutex.Free;
  end;
  {$ENDIF}
  {$IFDEF UNIX}
    {$Warning This needs to be completed! }
//  pthread_mutex_lock
//   pthread_cond_wait and pthread_cond_signal seems to be a Linux replacement
//   for the Windows WaitForSingleObject() call.
  {$ENDIF}
end;


procedure tiReleaseMutex(const AMutexName: string);
var
  LMutex : TtiMutex;
  LMutexName : string;
  i : Integer;
  LList: TList;
begin
  LList := uMutexes.LockList;
  try
  LMutexName := _MutexName(AMutexName);
  for i := LList.Count - 1 downto 0 do
    if LMutexName = (TObject(LList.Items[i]) as TtiMutex).MutexName then
    begin
      LMutex := TObject(LList.Items[i]) as TtiMutex;
      LMutex.Free;
      LList.Delete(i);
      Break;
    end;
  finally
    uMutexes.UnlockList;
  end;
end;


{ TtiMutex }

constructor TtiMutex.Create(const AMutexName: string);
begin
  inherited Create;
  FMutexName := AMutexName;
  {$IFDEF MSWINDOWS}
  FMutexHandle := CreateMutex(nil, False, PChar(FMutexName));
  {$ENDIF}
  {$IFDEF UNIX}
    {$Warning This needs to be completed! }
//  InitializeMutex(FCriticalSection);
(*
var
  MAttr : TMutexAttribute;

begin
  Result:=pthread_mutexattr_init(@MAttr);
  if Result=0 then
    try
    Result:=pthread_mutexattr_settype(@MAttr, PTHREAD_MUTEX_RECURSIVE);
    if Result=0 then
       Result:=pthread_mutex_init(@lpCriticalSection,@MAttr);
    finally
      pthread_mutexattr_destroy(@MAttr);
    end;
*)
//  pthread_mutex_init
  {$ENDIF}
end;


destructor TtiMutex.Destroy;
begin
  {$IFDEF MSWINDOWS}
  ReleaseMutex(FMutexHandle);
  CloseHandle(FMutexHandle);
  {$ENDIF}
  {$IFDEF UNIX}
    {$Warning This needs to be implemented! }
//    pthread_mutex_unlock
//    pthread_mutex_destroy
  {$ENDIF}
  inherited;
end;


{ TtiObjectLockList }

constructor TtiObjectLockList.Create;
begin
  inherited;
  FList := TObjectList.Create(False);
  FCritSect := TCriticalSection.Create;
  FRetryLockFrequency := cDefaultReTryLockFrequency;
end;


destructor TtiObjectLockList.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;


procedure TtiObjectLockList.Lock(AObject: TtiObject);
begin
  while not TryLock(AObject) do
    Sleep(FReTryLockFrequency)
end;


function TtiObjectLockList.TryLock(AObject: TtiObject): Boolean;
begin
  FCritSect.Enter;
  try
    if FList.IndexOf(AObject) = -1 then
    begin
      FList.Add(AObject);
      Result := true;
    end
    else
      Result := False;
  finally
    FCritSect.Leave;
  end;
end;


procedure TtiObjectLockList.UnLock(AObject: TtiObject);
begin
  FCritSect.Enter;
  try
    FList.Remove(AObject);
  finally
    FCritSect.Leave;
  end;
end;

procedure tiFreeThreadList(var AThreadList: TThreadList; const AOwnsContent: boolean);
var
  LList: TList;

begin

  if Assigned(AThreadList) and AOwnsContent then
  begin
    LList := AThreadList.LockList;

    try

      while LList.Count > 0 do
      begin
        TObject(LList[0]).Free;
        LList.Delete(0);
      end;

    finally
      AThreadList.UnlockList;
    end;

  end;

  FreeAndNil(AThreadList);
end;

initialization
  uMutexes := TThreadList.Create;

finalization
  tiFreeThreadList(uMutexes, true);

end.

