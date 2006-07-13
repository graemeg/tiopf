{ Windows specific functions used by tiUtils. }
unit tiWin32;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Windows
  ;

  procedure tiWin32RunEXEAndWait( pStrEXE : string ) ;
  function  tiWin32FileGetAttr( const pFileName : string ) : integer ;
  function  tiWin32FileSetAttr(const pFileName: string; pAttr: Integer): Integer;
  function  tiWin32FindFirstFile(const Path: string; var  F: TSearchRec): Integer;
  function  tiWin32CoCreateGUID : String ;
  procedure tiWin32CoInitialize;
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
  end;

var
  uTICoInitializeManager : TtiCoInitializeManager ;

//{$ifdef DELPHI6ORABOVE}
//  {$WARN SYMBOL_PLATFORM OFF}
//{$endif}


procedure tiWin32RunEXEAndWait( pStrEXE : string ) ;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  GetStartupInfo(SI);
  CreateProcess(
    nil, PChar(pStrEXE), nil, nil,
    False, 0, nil, nil, SI, PI);
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end ;


function tiWin32FileGetAttr( const pFileName : string ) : integer ;
begin
  result := fileGetAttr( pFileName ) ;
end;


function tiWin32FileSetAttr(const pFileName: string; pAttr: Integer): Integer;
begin
  result := FileSetAttr( pFileName, pAttr);
end;


function tiWin32FindFirstFile(const Path: string; var  F: TSearchRec): Integer;
begin
  result := FindFirst(Path, faAnyFile-faVolumeID-faSYSFile-faDirectory, F);
end ;


function tiWin32CoCreateGUID : string ;
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

procedure tiWin32CoInitialize ;
begin
  if uTICoInitializeManager = nil then
    uTICoInitializeManager := TtiCoInitializeManager.Create ;
  uTICoInitializeManager.CoInitialize;
end;


procedure tiWin32CoUnInitialize ;
begin
  if uTICoInitializeManager = nil then
    uTICoInitializeManager := TtiCoInitializeManager.Create ;
  uTICoInitializeManager.CoUnInitialize;
end;


function  tiWin32HasCoInitializeBeenCalled: Boolean;
begin
  if uTICoInitializeManager = nil then
    uTICoInitializeManager := TtiCoInitializeManager.Create ;
  Result:= uTICoInitializeManager.HasBeenCalled;
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
      ActiveX.CoInitialize( nil );
      FList.Add(LCurrentThreadID);
    end;
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
  LThreadID : integer ;
begin
  FCritSect.Enter;
  try
    LThreadID := GetCurrentThreadID ;
    Result:= FList.IndexOf(LThreadID) <> -1;
  finally
    FCritSect.Leave;
  end;
end;

initialization

finalization
  FreeAndNil(uTICoInitializeManager);

end.
