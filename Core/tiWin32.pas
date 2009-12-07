{ Windows specific functions used by tiUtils. }
unit tiWin32;

{$I tiDefines.inc}

interface
uses
  SysUtils,
  Windows;

  procedure tiWin32RunEXEAndWait(const AEXE : string);
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
  function  tiWin32GetAppConfigDir(Global: Boolean): string;
  function  tiWin32AuthenticateWithDomain(const AUserName, ADomain, APassword: string): Boolean;

  function  tiWin32GetCommonAppDir: string;
  function  tiWin32GetUserLocalAppDir: string;
  function  tiWin32GetCurrentUserPersonalDir: string;

  {$IFNDEF FPC}
  //Start a process, wait until timeout reached then kill a process
  function tiWin32RunProcessWithTimeout(const AProcessCommandLine: string;
        const AParams: string = '';
        const AProcessCurrentDirectory: string = '';
        const ATimeoutIntervalSecs: Cardinal = 0): DWORD;


  function tiWin32KillProcess(const AEXEName: String): Integer;
  {$ENDIF}

implementation
uses
  tiBaseObject,
  tiUtils,
  tiConstants,
  tiExcept,
  ComObj,
  ActiveX,
  Classes,
  SyncObjs,
  ShlObj,
  Messages
  {$IFNDEF FPC}
  ,Tlhelp32
  {$ENDIF};


const
  CSIDL_LOCAL_APPDATA = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming)}
  CSIDL_COMMON_APPDATA = $0023 { %USERPROFILE%\All Users\Application Data };
  CSIDL_FLAG_CREATE   = $8000; { (force creation of requested folder if it doesn't exist yet)}
  CSIDL_PERSONAL = $0005;

  CErrorCanNotExecuteApplication = 'Can not execute application "%s". Error code "%d". Error message "%s"';

  {$IFDEF FPC}
  // Graeme [2007-11-27]: This constant is missing from FPC 2.2.0. I created
  //   a bug report so we should be able to remove it after the next FPC release.
  LOGON32_LOGON_NETWORK = 3;
  {$ENDIF}

type
  PFNSHGetFolderPath = function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;

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
  UTICoInitializeManager: TtiCoInitializeManager = nil;
  SHGetFolderPath: PFNSHGetFolderPath = nil;
  CFGDLLHandle: THandle = 0;

procedure _InitDLL;
var
  LProcAddress: Pointer;
begin
  LProcAddress:= nil;
  CFGDLLHandle := LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
  begin
    LProcAddress := GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    if (LProcAddress = nil) then
    begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle := 0;
    end
    else
      SHGetFolderPath := PFNSHGetFolderPath(LProcAddress);
  end;

  if (LProcAddress = nil) then
  begin
    CFGDLLHandle := LoadLibrary('shfolder.dll');
    if (CFGDLLHandle <> 0) then
    begin
      LProcAddress := GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      if (LProcAddress=Nil) then
      begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle := 0;
      end
      else
        ShGetFolderPath := PFNSHGetFolderPath(LProcAddress);
    end;
  end;

  if (@ShGetFolderPath = nil) then
    raise Exception.Create('Could not determine SHGetFolderPath function');
end;

function _GetSpecialDir(ID: Integer): string;
var
  APath: Array[0..MAX_PATH] of ansichar;
  APtr: PAnsiChar;
begin
  Result := '';
  if (CFGDLLHandle = 0) then
    _InitDLL;
  if Assigned(SHGetFolderPath) then
  begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0]) = S_OK then
    begin
      APtr    := PAnsiChar(@APath[0]);
      Result  := IncludeTrailingPathDelimiter(APtr);
    end;
  end;
end;

function tiWin32GetCommonAppDir: string;
begin
  result := _GetSpecialDir(CSIDL_COMMON_APPDATA);
end;

function tiWin32GetUserLocalAppDir: string;
begin
  result := _GetSpecialDir(CSIDL_LOCAL_APPDATA);
end;

function tiWin32GetCurrentUserPersonalDir: string;
begin
  result := _GetSpecialDir(CSIDL_PERSONAL);
end;

procedure tiWin32RunEXEAndWait(const AEXE: string);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  GetStartupInfo(SI);
  CreateProcess(
    nil, PChar(AEXE), nil, nil,
    False, 0, nil, nil, SI, PI);
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end;

{$IFNDEF FPC}
function tiWin32RunProcessWithTimeout(const AProcessCommandLine,
  AParams, AProcessCurrentDirectory: string;
  const ATimeoutIntervalSecs: Cardinal): DWORD;

var
  SI: TStartupInfo;
  LProcessInfo: TProcessInformation;
  SA: TSecurityAttributes;
  LPProcessCurrentDirectory: PChar;
  LCommandLine: string;
  LWaitResult: DWORD;
const
  // WAIT_ABANDONED, 0x00000080L, 128
  // WAIT_OBJECT_0, 0x00000000L, 0
  // WAIT_TIMEOUT, 0x00000102L, 258
  // WAIT_FAILED, 0xFFFFFFFF, High(DWord), 4294967295
  CExitWithoutTimeout = WAIT_OBJECT_0;
  CInheritParentHandlesFalse = false;

  procedure _RaiseLastSystemError(const ACommandLine: string);
  var
    LErrorCode: DWord;
    LErrorMessage: string;
  begin
    LErrorCode:= GetLastError;
    LErrorMessage:= sysErrorMessage(LErrorCode);
    raise EtiOPFFileSystemException.CreateFmt(CErrorCanNotExecuteApplication,
      [LCommandLine, LErrorCode, LErrorMessage]);
  end;

begin

  LCommandLine := '"' + AProcessCommandLine + '" ' + AParams;

  if AProcessCurrentDirectory = '' then
    LPProcessCurrentDirectory := nil
  else
    LPProcessCurrentDirectory := PChar(tiRemoveTrailingSlash(AProcessCurrentDirectory));

  GetStartupInfo(SI);
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength:= Sizeof(SA);
  SA.bInheritHandle:= true;

  try
    // CreateProcess docs are here: http://msdn.microsoft.com/en-us/library/ms682425.aspx
    if not CreateProcess(
      nil, PChar(LCommandLine), nil, nil,
      cInheritParentHandlesFalse, CREATE_NO_WINDOW, nil,
      LPProcessCurrentDirectory, SI, LProcessInfo) then
      _RaiseLastSystemError(LCommandLine);

    LWaitResult := WaitForSingleObject(LProcessInfo.hProcess, ATimeoutIntervalSecs * 1000);

    if  LWaitResult = cExitWithoutTimeout then
      GetExitCodeProcess(LProcessInfo.hProcess, Result)
    else
    begin
      Result := LWaitResult;
      if not TerminateProcess(LProcessInfo.hProcess, 0) then
      begin
        Sleep(1000);
        if not TerminateProcess(LProcessInfo.hProcess, 0) then
          _RaiseLastSystemError(LCommandLine);
      end;
    end;

  finally
    CloseHandle(LProcessInfo.hProcess);
    CloseHandle(LProcessInfo.hThread);
  end;
end;
{$ENDIF}

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
  Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
  UTICoInitializeManager.CoInitialize;
end;

procedure tiWin32ForceCoInitialize;
begin
  Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
  UTICoInitializeManager.ForceCoInitialize;
end;

// ToDo: This may leak as TtiThread's destructor calls tiWin32CoUnInitiliaze,
//       and there is no thread protection around the creation of TtiCoInitializeManager.
//       Fix.
procedure tiWin32CoUnInitialize;
begin
  Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
  UTICoInitializeManager.CoUnInitialize;
end;


function  tiWin32HasCoInitializeBeenCalled: Boolean;
begin
  Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
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

function tiWin32GetAppConfigDir(Global: Boolean): string;
begin
  if Global then
    Result := _GetSpecialDir(CSIDL_COMMON_APPDATA) + tiApplicationName
  else
  begin
    Result := _GetSpecialDir(CSIDL_LOCAL_APPDATA) + tiApplicationName;
  end;
  if (Result = '') then
    Result := tiGetEXEPath;
end;

function tiWin32AuthenticateWithDomain(const AUserName, ADomain, APassword: string): Boolean;
var
  LUserHandle: THandle;
begin
  Result := False;
  try
    if LogonUser(PChar(AUserName), pChar(ADomain), PChar(APassword),
        LOGON32_LOGON_NETWORK, LOGON32_PROVIDER_DEFAULT, LUserHandle) then
      Result := True;
  except
    // Swallow failure
  end;
end;

{$IFNDEF FPC}
function tiWin32KillProcess(const AEXEName: String): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  LContinueLoop: BOOL;
  LSnapshotHandle: THandle;
  LProcessEntry32: TProcessEntry32;
  LEXEName: string;
begin
  Result := 0;
  LSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  LProcessEntry32.dwSize := SizeOf(LProcessEntry32);
  LContinueLoop := Process32First(LSnapshotHandle, LProcessEntry32);
  LEXEName := ExtractFileName(AEXEName);

  while Integer(LContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(LProcessEntry32.szExeFile)) =
      UpperCase(LEXEName)) or (UpperCase(LProcessEntry32.szExeFile) =
      UpperCase(LEXEName))) then
      Result := Integer(TerminateProcess(
                        OpenProcess(PROCESS_TERMINATE,
                                    BOOL(0),
                                    LProcessEntry32.th32ProcessID),
                                    0));
     LContinueLoop := Process32Next(LSnapshotHandle, LProcessEntry32);
  end;
  CloseHandle(LSnapshotHandle);
end;
{$ENDIF}

//var
//  LhWindowHandle: HWND;
//  LProcessID: INTEGER;
//  LhProcessHandle: THandle;
//  LDWResult: DWORD;
//begin
//
//  LhWindowHandle := FindWindow(PAnsiChar(AEXEName), nil);
//
//  SendMessageTimeout(LhWindowHandle, WM_CLOSE, 0, 0,
//    SMTO_ABORTIFHUNG or SMTO_NORMAL, 5000, LDWResult);
//
//  if isWindow(LhWindowHandle) then
//  begin
//    // PostMessage(LhWindowHandle, WM_QUIT, 0, 0);
//
//    { Get the process identifier for the window}
//    GetWindowThreadProcessID(LhWindowHandle, @LProcessID);
//    if LProcessID <> 0 then
//    begin
//      { Get the process handle }
//      LhProcessHandle := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION,
//        False, LProcessID);
//      if LhProcessHandle <> 0 then
//      begin
//        { Terminate the process }
//        TerminateProcess(LhProcessHandle, 0);
//        CloseHandle(LhProcessHandle);
//      end;
//    end;
//  end;
//  Result := 0;
//end;

{ TtiCoInitializeManager }

procedure TtiCoInitializeManager.CoInitialize;
var
  LCurrentThreadID: TThreadID;
begin
  FCritSect.Enter;
  try
    LCurrentThreadID:= GetCurrentThreadID;
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
  LCurrentThreadID: TThreadID;
begin
  FCritSect.Enter;
  try
    LCurrentThreadID:= GetCurrentThreadID;
    ActiveX.CoInitialize(nil);
    if FList.IndexOf(LCurrentThreadID) = -1 then
      FList.Add(LCurrentThreadID);
  finally
    FCritSect.Leave
  end;
end;

procedure TtiCoInitializeManager.CoUnInitialize;
var
  LCurrentThreadID: TThreadID;
begin
  FCritSect.Enter;
  try
    LCurrentThreadID:= GetCurrentThreadID;
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
  LThreadID: TThreadID;
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
  // ToDo: Just because we are linking tiWin32 does not mean we are going
  //       to use COM. TtiCoInitializeManager should be in it's own tiCOM.pas
  //       unit. (TtiThreads call tiCoUnitialize. This should be tidied up.)
  UTICoInitializeManager := TtiCoInitializeManager.Create;

finalization
  FreeAndNil(UTICoInitializeManager);

end.
