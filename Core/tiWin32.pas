{ Windows specific functions used by tiUtils. }
unit tiWin32;

{$I tiDefines.inc}
{$WARN SYMBOL_PLATFORM OFF} // This file is Win32 specific, so it's OK to have platform dependencies

interface
uses
  SysUtils,
  Windows;

  procedure tiWin32RunEXEAndWait(const AEXE : string; AInheritParentStartInfo: boolean = true); overload;
  procedure tiWin32RunEXEAndWait(const AEXEPathAndName, AParameters, ACurrentDir : string;
      AInheritParentStartInfo: boolean = true); overload;
  function tiCreateProcess(const AProcessName: string;
      const ACommandLineParams: string;
      var AProcessInfo: TProcessInformation): Boolean;
  function  tiWin32ThreadRunning(const AThreadHandle: THandle): Boolean;
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
type

  TtiCreateProcessParams = record
    CommandLine: string;
    CommandLineParams: string;
    WorkingDirectory: string;
    TimeoutAfterSecs: cardinal;
    wShowWindow: WORD; // See STARTUPINFO structure and ShowWindow function
  end;

  TtiCreateProcessResult = record
    ErrorCode: Cardinal;
    WaitResult: DWord;
  end;

  //Start a process, wait until timeout reached then kill a process
  function tiWin32RunProcessWithTimeout(const AParams: TtiCreateProcessParams;
        out AResults: TtiCreateProcessResult): Boolean;

//  function tiWin32RunProcessWithTimeout(const AProcessCommandLine: string;
//        out AResults: TtiCreateProcessResult;
//        const AParams: string = '';
//        const AProcessCurrentDirectory: string = '';
//        const ATimeoutIntervalSecs: Cardinal = INFINITE): DWORD;


  function tiWin32KillProcess(const AEXEName: String): Integer;
  {$ENDIF}

implementation
uses
  tiBaseObject,
  tiUtils,
  tiConstants,
  tiExcept,
  tiLog,
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

  CErrorCanNotExecuteApplication = 'System error executing application "%s". Error code "%d". Error message "%s"';

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
      Result  := IncludeTrailingPathDelimiter(string(APtr));
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

procedure tiWin32RunEXEAndWait(const AEXE: string;
    AInheritParentStartInfo: boolean = true);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  if AInheritParentStartInfo then
    GetStartupInfo(SI)
  else
  begin
    //Don't inherit the parents startup info, in this case just use
    //default values for the startup info (blank).
    ZeroMemory(@SI, SizeOf(TStartupInfo));
    SI.cb := SizeOf(TStartupInfo);
  end;

  CreateProcess(
    nil, PChar(AEXE), nil, nil,
    False, 0, nil, nil, SI, PI);
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end;

procedure tiWin32RunEXEAndWait(const AEXEPathAndName, AParameters, ACurrentDir : string;
    AInheritParentStartInfo: boolean = true );
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  LExePathNameAndParameters: string;
begin
  LEXEPathNameAndParameters :=
    Trim(AEXEPathAndName + ' ' + AParameters);

  if AInheritParentStartInfo then
    GetStartupInfo(SI)
  else
  begin
    //Don't inherit the parents startup info, in this case just use
    //default values for the startup info (blank).
    ZeroMemory(@SI, SizeOf(TStartupInfo));
    SI.cb := SizeOf(TStartupInfo);
  end;

  CreateProcess(
    nil,                              // lpApplicationName
    PChar(LEXEPathNameAndParameters), // lpCommandLine
    nil,                              // lpProcessAttributes
    nil,                              // lpThreadAttributes
    False,                            // bInheritHandles
    0,                                // dwCreationFlags
    nil,                              // lpEnvironment
    PChar(ACurrentDir),               // lpCurrentDirectory
    SI,                               // lpStartupInfo
    PI);                              // lpProcessInformation
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end;

function tiCreateProcess(const AProcessName: string;
  const ACommandLineParams: string;
  var AProcessInfo: TProcessInformation): Boolean;
var
  LStartupInfo: TStartupInfo;
const
  CInheritParentHandlesFalse = false;
begin
  GetStartupInfo(LStartupInfo);
  Result := CreateProcess(
      PChar(AProcessName), PChar(ACommandLineParams), nil, nil,
      cInheritParentHandlesFalse, 0, nil,
      nil, LStartupInfo, AProcessInfo);
end;

{$IFNDEF FPC}
function tiWin32RunProcessWithTimeout(
  const AParams: TtiCreateProcessParams;
  out AResults: TtiCreateProcessResult): Boolean;

var
  SI: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LPProcessCurrentDirectory: PChar;
  LCommandLine: string;
  LTimeOutAfterMS: Cardinal;
  LCreateOK: Boolean;
const
  // WAIT_ABANDONED, 0x00000080L, 128
  // WAIT_OBJECT_0, 0x00000000L, 0
  // WAIT_TIMEOUT, 0x00000102L, 258
  // WAIT_FAILED, 0xFFFFFFFF, High(DWord), 4294967295
  CExitWithoutTimeout = WAIT_OBJECT_0;
  CInheritParentHandlesFalse = false;

  procedure _RaiseLastSystemError(const ACommandLine: string);
  var
    LError: DWord;
    LErrorMessage: string;
  begin
    LError:= GetLastError;
    LErrorMessage:= sysErrorMessage(LError);
    raise EtiOPFFileSystemException.CreateFmt(CErrorCanNotExecuteApplication,
      [LCommandLine, LError, LErrorMessage]);
  end;

begin
  Result := false;
  AResults.ErrorCode := 0;
  AResults.WaitResult := 0;

  LCommandLine := '"' + AParams.CommandLine + '" ' + AParams.CommandLineParams;

  if AParams.WorkingDirectory = '' then
    LPProcessCurrentDirectory := nil
  else
    LPProcessCurrentDirectory := PChar(tiRemoveTrailingSlash(AParams.WorkingDirectory));

  // GetStartupInfo uses startup info of our process. As we might not have
  // control over that it can cause unexpected behaviour such as stin/stdout
  // not working. Instead we set the startup info explicitly
  //GetStartupInfo(SI);
  ZeroMemory(@SI, SizeOf(TStartupInfo));
  SI.cb := SizeOf(TStartupInfo);
  if AParams.wShowWindow <> 0 then
  begin
    SI.dwFlags := SI.dwFlags or STARTF_USESHOWWINDOW;
    SI.wShowWindow := AParams.wShowWindow;
  end;

  try
    // CreateProcess docs are here: http://msdn.microsoft.com/en-us/library/ms682425.aspx
    Log(Format('About to create process %s', [LCommandLine]), lsUserInfo);
    LCreateOK := CreateProcess(
        nil,                        // Application name
        PChar(LCommandLine),        // Command line (Includes application name if application name param value is nil).
        nil,                        // Process security attributes
        nil,                        // Thread security attributes
        cInheritParentHandlesFalse, // Inherit handles
        CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS+CREATE_NO_WINDOW+SYNCHRONIZE, // Creation flags
        nil,                        // Environment block for the new process. (Uses environment of calling process if value is nil).
        LPProcessCurrentDirectory,  // Current directory
        SI,                         // Startup info
        LProcessInfo                // Process info
    );

    if LCreateOK then
    begin
      Log(Format('Process created successfully %s', [LCommandLine]), lsUserInfo);

      // Let the process run for TimeoutIntervalSecs.
      if AParams.TimeoutAfterSecs > 0 then
        LTimeOutAfterMS := AParams.TimeoutAfterSecs * 1000
      else
        LTimeOutAfterMS := INFINITE;

      Log(Format('Waiting up to %u seconds for process to finish.', [LTimeOutAfterMS div 1000]), lsUserInfo);
      AResults.WaitResult := WaitForSingleObject(LProcessInfo.hProcess, LTimeOutAfterMS);

      // Check the status of WaitForSingleObject
      if AResults.WaitResult = WAIT_OBJECT_0 then // WAIT_OBJECT_0, 0x00000000L, 0
      begin
        // Program has exited. Get exit code.
        if GetExitCodeProcess(LProcessInfo.hProcess, AResults.ErrorCode) then
        begin
          Log(Format('Process exited with error code %u.', [AResults.ErrorCode]), lsUserInfo);
          Result := AResults.ErrorCode = 0;
        end else
        begin
          Log(Format('Could not get exit code for process %s.', [LCommandLine]));
          _RaiseLastSystemError(LCommandLine);
        end;
      end else
      if AResults.WaitResult = WAIT_TIMEOUT then // WAIT_TIMEOUT, 0x00000102L, 258
      begin
        // Program has timed-out. Wait for a second then terminate that sucker!
        Sleep(1000);
        Log('Process timed out. Attempting to kill it...', lsUserInfo);
        if TerminateProcess(LProcessInfo.hProcess, 0) then
        begin
          Log('Process terminated successfully.', lsUserInfo)
        end else
        begin
          // Raise an exception if we're stuck with some invincible process that can't be killed.
          Log('Process could not be killed.', lsError);
          _RaiseLastSystemError(LCommandLine);
        end;
      end else
      if AResults.WaitResult = WAIT_ABANDONED then // WAIT_ABANDONED, 0x00000080L, 128
      begin
        Log('Wait abaondoned.', lsError);
        _RaiseLastSystemError(LCommandLine);
      end else
      if AResults.WaitResult = WAIT_FAILED then // WAIT_FAILED, 0xFFFFFFFF, High(DWord), 4294967295
      begin
        Log('Wait failed.', lsError);
        _RaiseLastSystemError(LCommandLine);
      end else
      begin
        Log(Format('Invalid wait status result %u', [AResults.WaitResult]), lsError);
        _RaiseLastSystemError(LCommandLine);
      end;

    end else
    begin
      // Raise an exception if the process couldn't be created.
      Log(Format('Error creating process %s', [LCommandLine]), lsError);
      _RaiseLastSystemError(LCommandLine);
    end;

  finally
    CloseHandle(LProcessInfo.hProcess);
    CloseHandle(LProcessInfo.hThread);
  end;
end;
{$ENDIF}

function tiWin32ThreadRunning(const AThreadHandle: THandle): Boolean;
begin
  {$IFDEF MSWINDOWS}
  result := WaitForSingleObject(AThreadHandle, 0) = WAIT_OBJECT_0;
  {$ENDIF}
  {$IFDEF UNIX}
    {$Warning This needs to be completed! }
  {$ENDIF}
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
  result := FindFirst(APath, faAnyFile-faDirectory, ASearchRec);
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
  // Due to finalization order the manager may have already been freed.
  //Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
  if Assigned(UTICoInitializeManager) then
    UTICoInitializeManager.CoUnInitialize;
end;


function  tiWin32HasCoInitializeBeenCalled: Boolean;
begin
  // Due to finalization order the manager may have already been freed.
  //Assert(UTICoInitializeManager.TestValid, CTIErrorInvalidObject);
  Result := Assigned(UTICoInitializeManager) and UTICoInitializeManager.HasBeenCalled;
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
