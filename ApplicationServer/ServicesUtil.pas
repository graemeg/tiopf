unit ServicesUtil;

{$I tiDefines.inc}

interface

uses
  Windows, WinSvc, SvcMgr;

//--------------------------------------------------------------------------------------------------
// Missing WinSvc.h declarations
//--------------------------------------------------------------------------------------------------

const
  SERVICE_CONFIG_DESCRIPTION     = 1;
  {$EXTERNALSYM SERVICE_CONFIG_DESCRIPTION}
  SERVICE_CONFIG_FAILURE_ACTIONS = 2;
  {$EXTERNALSYM SERVICE_CONFIG_FAILURE_ACTIONS}

  SC_ACTION_NONE          = 0;
  {$EXTERNALSYM SC_ACTION_NONE}
  SC_ACTION_RESTART       = 1;
  {$EXTERNALSYM SC_ACTION_RESTART}
  SC_ACTION_REBOOT        = 2;
  {$EXTERNALSYM SC_ACTION_REBOOT}
  SC_ACTION_RUN_COMMAND   = 3;
  {$EXTERNALSYM SC_ACTION_RUN_COMMAND}

type
  PServiceDescriptionA = ^TServiceDescriptionA;
  PServiceDescriptionW = ^TServiceDescriptionW;
  PServiceDescription = ^TServiceDescriptionA;
  _SERVICE_DESCRIPTIONA = record
    lpDescription: PChar;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONA}
  _SERVICE_DESCRIPTIONW = record
    lpDescription: PWideChar;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONW}
  _SERVICE_DESCRIPTION = _SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM _SERVICE_DESCRIPTION}
  SERVICE_DESCRIPTIONA = _SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM SERVICE_DESCRIPTIONA}
  SERVICE_DESCRIPTIONW = _SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM SERVICE_DESCRIPTIONW}
  SERVICE_DESCRIPTION = _SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM SERVICE_DESCRIPTION}
  TServiceDescriptionA = _SERVICE_DESCRIPTIONA;
  TServiceDescriptionW = _SERVICE_DESCRIPTIONW;
  TServiceDescription = TServiceDescriptionA;

  _SC_ACTION_TYPE = Cardinal;
  {$EXTERNALSYM _SC_ACTION_TYPE}
  SC_ACTION_TYPE = _SC_ACTION_TYPE;
  {$EXTERNALSYM SC_ACTION_TYPE}

  PScAction = ^TScAction;
  _SC_ACTION = record
    Type_: SC_ACTION_TYPE;
    Delay: DWORD;
  end;
  {$EXTERNALSYM _SC_ACTION}
  SC_ACTION = _SC_ACTION;
  {$EXTERNALSYM SC_ACTION}
  TScAction = _SC_ACTION;

  PServiceFailureActionsA = ^TServiceFailureActionsA;
  PServiceFailureActionsW = ^TServiceFailureActionsW;
  PServiceFailureActions = ^TServiceFailureActionsA;
  _SERVICE_FAILURE_ACTIONSA = record
    dwResetPeriod: DWORD;
    lpRebootMsg: PChar;
    lpCommand: PChar;
    cActions: DWORD;
    lpsaActions: PScAction;
  end;
  {$EXTERNALSYM _SERVICE_FAILURE_ACTIONSA}
  _SERVICE_FAILURE_ACTIONSW = record
    dwResetPeriod: DWORD;
    lpRebootMsg: PWideChar;
    lpCommand: PWideChar;
    cActions: DWORD;
    lpsaActions: PScAction;
  end;
  {$EXTERNALSYM _SERVICE_FAILURE_ACTIONSW}
  _SERVICE_FAILURE_ACTIONS = _SERVICE_FAILURE_ACTIONSA;
  {$EXTERNALSYM _SERVICE_FAILURE_ACTIONS}
  SERVICE_FAILURE_ACTIONSA = _SERVICE_FAILURE_ACTIONSA;
  {$EXTERNALSYM SERVICE_FAILURE_ACTIONSA}
  SERVICE_FAILURE_ACTIONSW = _SERVICE_FAILURE_ACTIONSW;
  {$EXTERNALSYM SERVICE_FAILURE_ACTIONSW}
  TServiceFailureActionsA = _SERVICE_FAILURE_ACTIONSA;
  TServiceFailureActionsW = _SERVICE_FAILURE_ACTIONSW;
  TServiceFailureActions = TServiceFailureActionsA;

  TChangeServiceConfig2 = function (hService: SC_HANDLE; dwInfoLevel: DWORD; lpInfo: Pointer): BOOL; stdcall;

function ChangeServiceConfig2(hService: SC_HANDLE; dwInfoLevel: DWORD; lpInfo: Pointer): BOOL;

//--------------------------------------------------------------------------------------------------
// Service description
//--------------------------------------------------------------------------------------------------

// Call the function from TService.AfterInstall event
function SetServiceDescription(Service: TService; const Description: string): Boolean;

//--------------------------------------------------------------------------------------------------
// Service failure actions
//--------------------------------------------------------------------------------------------------

function SetServiceFailureActions(Service: TService; const Actions: TServiceFailureActions): Boolean;

implementation

//--------------------------------------------------------------------------------------------------

function ChangeServiceConfig2(hService: SC_HANDLE; dwInfoLevel: DWORD; lpInfo: Pointer): BOOL;
var
  AdvapiHandle: HMODULE;
  _ChangeServiceConfig2: TChangeServiceConfig2;
begin
  Result := False;
  AdvapiHandle := GetModuleHandle(advapi32);
  if AdvapiHandle <> 0 then
  begin
    @_ChangeServiceConfig2 := GetProcAddress(AdvapiHandle, 'ChangeServiceConfig2A');
    if Assigned(_ChangeServiceConfig2) then
      Result := _ChangeServiceConfig2(hService, dwInfoLevel, lpInfo);
  end;
end;

//--------------------------------------------------------------------------------------------------

function SetServiceDescription(Service: TService; const Description: string): Boolean;
var
  SvcMgr, Svc: SC_HANDLE;
  ServiceDescr: TServiceDescription;
begin
  Result := False;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr <> 0 then
  begin
    Svc := OpenService(SvcMgr, PChar(Service.Name), STANDARD_RIGHTS_REQUIRED or SERVICE_CHANGE_CONFIG);
    if Svc <> 0 then
    begin
      ServiceDescr.lpDescription := PChar(Description);
      Result := ChangeServiceConfig2(Svc, SERVICE_CONFIG_DESCRIPTION, @ServiceDescr);
      CloseServiceHandle(Svc);
    end;
    CloseServiceHandle(SvcMgr);
  end;
end;

//--------------------------------------------------------------------------------------------------

function SetServiceFailureActions(Service: TService; const Actions: TServiceFailureActions): Boolean;
var
  SvcMgr, Svc: SC_HANDLE;
  Access: DWORD;
  I: Integer;
  P: PScAction;
begin
  Result := False;
  Access := STANDARD_RIGHTS_REQUIRED or SERVICE_CHANGE_CONFIG;
  P := Actions.lpsaActions;
  if P <> nil then
    for I := 1 to Actions.cActions do
    begin
      if P^.Type_ = SC_ACTION_REBOOT then
        Access := Access or SERVICE_START;
      Inc(P);
    end;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr <> 0 then
  begin
    Svc := OpenService(SvcMgr, PChar(Service.Name), Access);
    if Svc <> 0 then
    begin
      Result := ChangeServiceConfig2(Svc, SERVICE_CONFIG_FAILURE_ACTIONS, @Actions);
      CloseServiceHandle(Svc);
    end;
    CloseServiceHandle(SvcMgr);
  end;
end;

//--------------------------------------------------------------------------------------------------

end.
