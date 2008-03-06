{ Windows specific functions used by tiUtils. }
unit tiWin32;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Windows
 ;

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

implementation
uses
   tiBaseObject
  ,tiUtils
  ,ComObj
  ,ActiveX
  ,Classes
  ,SyncObjs
 ;


const
  CSIDL_LOCAL_APPDATA = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming)}
  CSIDL_COMMON_APPDATA = $0023 { %USERPROFILE%\All Users\Application Data };
  CSIDL_FLAG_CREATE   = $8000; { (force creation of requested folder if it doesn't exist yet)}

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
  APath: Array[0..MAX_PATH] of char;
begin
  Result := '';
  if (CFGDLLHandle = 0) then
    _InitDLL;
  if Assigned(SHGetFolderPath) then
  begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0]) = S_OK then
      Result := IncludeTrailingPathDelimiter(StrPas(@APath[0]));
  end;
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
//  if UTICoInitializeManager = nil then
//    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.CoInitialize;
end;

procedure tiWin32ForceCoInitialize;
begin
//  if UTICoInitializeManager = nil then
//    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.ForceCoInitialize;
end;

procedure tiWin32CoUnInitialize;
begin
//  if UTICoInitializeManager = nil then
//    UTICoInitializeManager := TtiCoInitializeManager.Create;
  UTICoInitializeManager.CoUnInitialize;
end;


function  tiWin32HasCoInitializeBeenCalled: Boolean;
begin
//  if UTICoInitializeManager = nil then
//    UTICoInitializeManager := TtiCoInitializeManager.Create;
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


{ TtiCoInitializeManager }

procedure TtiCoInitializeManager.CoInitialize;
var
  LCurrentThreadID: DWord;
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
  LCurrentThreadID: DWord;
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
  LCurrentThreadID: DWord;
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
  UTICoInitializeManager := TtiCoInitializeManager.Create;

finalization
  FreeAndNil(UTICoInitializeManager);

end.
