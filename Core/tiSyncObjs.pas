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
  ,SyncObjs
  ;

const
  cDefaultReTryLockFrequency = 5000 ; // ms

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


function  tiWaitForMutex(const pMutexName: string): Boolean ; overload;
function  tiWaitForMutex(const pMutexName: string; ASecondsToWait: Word): Boolean ; overload;
procedure tiReleaseMutex(const pMutexName: string);


implementation
uses
  tiUtils
  {$IFDEF LINUX}
  ,cthreads
  {$ENDIF}
  ,SysUtils    // used for Sleep under Free Pascal
  ;

var
  uMutexes : TObjectList ;

type

  TtiMutex = class( TtiBaseObject )
  private
    FMutexName: string;
    FMutexHandle: THandle;
    {$IFDEF LINUX}
    FCriticalSection: TRTLCriticalSection;
//    InitializeMutex(pCritSec: TRTLCriticalSection);
    {$ENDIF}
  public
    constructor Create( const pMutexName: string );
    destructor  Destroy; override ;
    property    MutexName: string read FMutexName;
    property    MutexHandle: THandle read FMutexHandle ;
  end ;


function _MutexName(const pMutexName: string): string;
begin
  Result := tiStrTran(pMutexName, ' ', '');
  Result := tiStrTran(Result,  ':', '');
  Result := tiStrTran(Result,  '/', '');
end;


function tiWaitForMutex(const pMutexName: string): Boolean ;
begin
  Result:= tiWaitForMutex(pMutexName, 3*60);
end ;


function tiWaitForMutex(const pMutexName: string; ASecondsToWait: Word): Boolean ;
var
  lMutexName : string ;
  lMutex : TtiMutex ;
begin
  lMutexName := _MutexName(pMutexName);
  lMutex := TtiMutex.Create(lMutexName) ;
  {$IFDEF MSWINDOWS}
  if WaitForSingleObject(lMutex.MutexHandle, ASecondsToWait*1000) = Wait_Object_0 then
  begin
    Result := True;
    uMutexes.Add(lMutex);
  end
  else begin
    Result := False;
    lMutex.Free;
  end;
  {$ENDIF}
  {$IFDEF LINUX}
    {$Warning This needs to be completed! }
//  pthread_mutex_lock
  {$ENDIF}
end ;


procedure tiReleaseMutex(const pMutexName: string);
var
  lMutex : TtiMutex ;
  lMutexName : string ;
  i : Integer ;
begin
  lMutexName := _MutexName(pMutexName);
  for i := 0 to uMutexes.Count - 1 do
    if lMutexName = ( uMutexes.Items[i] as TtiMutex ).MutexName then
    begin
      lMutex := uMutexes.Items[i] as TtiMutex ;
      uMutexes.Remove(lMutex);
    end;
end;


{ TtiMutex }

constructor TtiMutex.Create(const pMutexName: string);
begin
  inherited Create;
  FMutexName := pMutexName ;
  {$IFDEF MSWINDOWS}
  FMutexHandle := CreateMutex(nil, False, PChar(FMutexName));
  {$ENDIF}
  {$IFDEF LINUX}
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
  {$IFDEF LINUX}
    {$Warning This needs to be completed! }
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


initialization
  uMutexes := TObjectList.Create(True) ;


finalization
  uMutexes.Free;


end.
