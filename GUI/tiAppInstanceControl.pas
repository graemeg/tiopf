unit tiAppInstanceControl;

{ Based on code found at http://delphi.about.com/library/code/ncaa100703a.htm}

{$i tiDefines.inc}

interface

uses
  Windows,
  SysUtils;

function tiRestoreIfRunning(const AClientExePath: string;
    const AMaxInstances: integer = 1): boolean;
procedure tiSetRunningInstanceMainFormHandle(const AMainFormHandle: THandle);
procedure SendMsgToRunningInstanceMainForm(const AMsg: String);
procedure RemoveRunningInstance;

implementation

uses
  Forms,
  Messages;

type
  PInstanceInfo = ^TInstanceInfo;
  TInstanceInfo = packed record
    PreviousMainFormHandle: THandle;
    RunCounter: integer;
  end;

var
  UMappingHandle: THandle;
  UInstanceInfo: PInstanceInfo;
  UMappingName: string;
  URemoveThisAppInstance: boolean = False;

function tiRestoreIfRunning(const AClientExePath: string;
  const AMaxInstances: integer = 1): boolean;
var
  LCreated: boolean;
begin
  Result := True;

  UMappingName := StringReplace(AClientExePath, '\', '', [rfReplaceAll, rfIgnoreCase]);

  UMappingHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0,
      SizeOf(TInstanceInfo), PChar(UMappingName));
  if UMappingHandle = 0 then
    RaiseLastOSError;
  LCreated := GetLastError <> ERROR_ALREADY_EXISTS;

  UInstanceInfo := MapViewOfFile(UMappingHandle, FILE_MAP_ALL_ACCESS,
      0, 0, SizeOf(TInstanceInfo));
  if UInstanceInfo = nil then
    RaiseLastOSError;

  if LCreated then
  begin
    UInstanceInfo^.PreviousMainFormHandle := 0;
    UInstanceInfo^.RunCounter := 1;
    URemoveThisAppInstance := True;
    Result := False;
  end
  else
  begin
    // Restore existing instance if too many now running
    if UInstanceInfo^.RunCounter >= AMaxInstances then
    begin
      if UInstanceInfo^.PreviousMainFormHandle <> 0 then
      begin
        if IsIconic(UInstanceInfo^.PreviousMainFormHandle) then
          ShowWindow(UInstanceInfo^.PreviousMainFormHandle, SW_RESTORE);
        SetForegroundWindow(UInstanceInfo^.PreviousMainFormHandle);
      end;
    end
    else
    begin
      Inc(UInstanceInfo^.RunCounter);
      URemoveThisAppInstance := True;
      Result := False;
    end;
  end;
end;

procedure tiSetRunningInstanceMainFormHandle(const AMainFormHandle: THandle);
begin
  if UInstanceInfo <> nil then
    UInstanceInfo^.PreviousMainFormHandle := AMainFormHandle;
end;

procedure SendMsgToRunningInstanceMainForm(const AMsg: string);
var
  LCopyDataStruct : TCopyDataStruct;
begin
  if UInstanceInfo <> nil then
  begin
    if UInstanceInfo^.PreviousMainFormHandle <> 0 then
    begin
      LCopyDataStruct.dwData := 0; //string
      LCopyDataStruct.cbData := 1 + Length(AMsg);
      LCopyDataStruct.lpData := PChar(AMsg);
      SendMessage(UInstanceInfo^.PreviousMainFormHandle, WM_COPYDATA, Integer(Application.Handle), Integer(@LCopyDataStruct));
    end;
  end;
end;

procedure RemoveRunningInstance;
begin
  if UInstanceInfo <> nil then
  begin
    if URemoveThisAppInstance then
    begin
      UInstanceInfo^.PreviousMainFormHandle := 0;

      if UInstanceInfo^.RunCounter > 0 then
        Dec(UInstanceInfo^.RunCounter)
      else
        UInstanceInfo^.RunCounter := 0;

      URemoveThisAppInstance := false;
    end;
  end;
end;

initialization

finalization
  RemoveRunningInstance;

  if Assigned(UInstanceInfo) then
  begin
    UnmapViewOfFile(UInstanceInfo);
    UInstanceInfo := nil;
  end;
  if UMappingHandle <> 0 then
  begin
    CloseHandle(UMappingHandle);
    UMappingHandle := 0;
  end;

end.

