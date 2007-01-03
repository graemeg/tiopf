{ Windows specific functions used by tiUtils. }
unit tiWin32;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Windows
 ;

  procedure tiWin32RunEXEAndWait(AEXE : string);
  function  tiWin32FileGetAttr(const AFileName : string): integer;
  function  tiWin32FileSetAttr(const AFileName: string; pAttr: Integer): Integer;
  function  tiWin32FindFirstFile(const APath: string; var  ASearchRec: TSearchRec): Integer;
  function  tiWin32CoCreateGUID : String;
  procedure tiWin32CoInitialize;
  procedure tiWin32ForceCoInitialize;
  procedure tiWin32CoUnInitialize;
  function  tiWin32HasCoInitializeBeenCalled: Boolean;
  function  tiWin32GetTickCount: Cardinal;
  function  tiWin32GetUserName: string;
  function  tiWin32GetComputerName: string;



implementation
uses
   tiBaseObject
  ,tiUtils
  ,ComObj
  ,ActiveX
  ,Classes
  ,SyncObjs
 ;

type

  TtiCoInitializeManager = class(TtiBaseObject)
  private
    FList: TtiIntegerList;
    FCritSect: TCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;
    function    HasBeenCalled: Boolean;
    procedure   CoInitialize;
    procedure   CoUnInitialize;
    procedure   ForceCoInitialize;
  end;

var
  UTICoInitializeManager : TtiCoInitializeManager;

//{$ifdef DELPHI6ORABOVE}
//  {$WARN SYMBOL_PLATFORM OFF}
//{$endif}


procedure tiWin32RunEXEAndWait(AEXE : string);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  { Don't remove the IFDEF even though we use FPC in Delphi Mode. GetStartupInfo
    is defined differently to Delphi.  34 minutes after reporting this bug, it
    was confirmed fixed.:-) I am leaving this IFDEF for now, until the next
    FPC version is released, in a few days. - Graeme [2006-07-17] }
  GetStartupInfo({$IFDEF FPC}@{$ENDIF}SI);

  CreateProcess(
    nil, PChar(AEXE), nil, nil,
    False, 0, nil, nil, SI, PI);
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end;


function tiWin32FileGetAttr(const AFileName : string): integer;
begin
  result := fileGetAttr(AFileName);
end;


function tiWin32FileSetAttr(const AFileName: string; pAttr: Integer): Integer;
begin
  result := FileSetAttr(AFileName, pAttr);
end;


function tiWin32FindFirstFile(const APath: string; var  ASearchRec: TSearchRec): Integer;
begin
  result := FindFirst(APath, faAnyFile-faVolumeID-faSYSFile-faDirectory, ASearchRec);
end;


function tiWin32CoCreateGUID : string;
var
  lGuid : TGUID;
begin
  tiWin32CoInitialize;
  CoCreateGuid(lGUID);
  Result := GuidToString(lGUID);
end;


// S_OK - Indicates the library was initialized successfully.
// S_FALSE - Indicates that the library is already initialized or that it could not release default allocator.
// E_OUTOFMEMORY - Indicates that it was unable to initialize because the system is out of memory.
// E_INVALIDARG - Indicates the argument is invalid.
// E_UNEXPECTED - Indicates an unexpected error occurred.

procedure tiWin32CoInitialize;
begin
  if UTICoInitializeManager = nil then
    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.CoInitialize;
end;

procedure tiWin32ForceCoInitialize;
begin
  if UTICoInitializeManager = nil then
    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.ForceCoInitialize;
end;

procedure tiWin32CoUnInitialize;
begin
  if UTICoInitializeManager = nil then
    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.CoUnInitialize;
end;


function  tiWin32HasCoInitializeBeenCalled: Boolean;
begin
  if UTICoInitializeManager = nil then
    UTICoInitializeManager := TtiCoInitializeManager.Create;
  Result:= UTICoInitializeManager.HasBeenCalled;
end;


function tiWin32GetTickCount: Cardinal;
begin
  Result := GetTickCount;
end;


function tiWin32GetUserName: string;
var
  userNameBuffer: array[0..255] of char;
  sizeBuffer: DWord;
begin
  SizeBuffer := 256;
  getUserName(userNameBuffer, sizeBuffer);
  result := string(userNameBuffer);
end;


function tiWin32GetComputerName: string;
var
  computerNameBuffer: array[0..255] of char;
  sizeBuffer: DWord;
begin
  SizeBuffer := 256;
  getComputerName(computerNameBuffer, sizeBuffer);
  result := string(computerNameBuffer);
end;


{ TtiCoInitializeManager }

procedure TtiCoInitializeManager.CoInitialize;
var
  LCurrentThreadID: DWord;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  FCritSect.Enter;
  try
    if FList.IndexOf(LCurrentThreadID) = -1 then
    begin
      ActiveX.CoInitialize(nil);
      FList.Add(LCurrentThreadID);
    end;
  finally
    FCritSect.Leave
  end;
end;

procedure TtiCoInitializeManager.ForceCoInitialize;
var
  LCurrentThreadID: DWord;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  FCritSect.Enter;
  try
    ActiveX.CoInitialize(nil);
    if FList.IndexOf(LCurrentThreadID) = -1 then
      FList.Add(LCurrentThreadID);
  finally
    FCritSect.Leave
  end;
end;

procedure TtiCoInitializeManager.CoUnInitialize;
var
  LCurrentThreadID: DWord;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  FCritSect.Enter;
  try
    if FList.IndexOf(LCurrentThreadID) <> -1 then
    begin
      ActiveX.CoUnInitialize;
      FList.Remove(LCurrentThreadID);
    end;
  finally
    FCritSect.Leave;
  end;
end;

constructor TtiCoInitializeManager.Create;
begin
  inherited Create;
  FList:= TtiIntegerList.Create;
  FCritSect:= TCriticalSection.Create;
end;

destructor TtiCoInitializeManager.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;

function TtiCoInitializeManager.HasBeenCalled: Boolean;
var
  LThreadID : integer;
begin
  FCritSect.Enter;
  try
    LThreadID := GetCurrentThreadID;
    Result:= FList.IndexOf(LThreadID) <> -1;
  finally
    FCritSect.Leave;
  end;
end;

initialization

finalization
  FreeAndNil(UTICoInitializeManager);

end.
