{

 antonio.bakula@zg.hinet.hr

 }
unit ServiceManager;

interface

uses Windows, Sysutils, WinSvc;


{
 this class requires following access rights :

 SERVICE_CHANGE_CONFIG
 SERVICE_START
 SERVICE_STOP
 SC_MANAGER_CONNECT
 SC_MANAGER_ENUMERATE_SERVICE
 SC_MANAGER_QUERY_LOCK_STATUS

 In one word user must be member of NT administrators

 }

type
  TServiceManager = class (TObject)
  private
    FServiceHandle: SC_Handle;
    FServiceManagerHandle: Int64;
    FServiceName: string;
    procedure SetServiceName(Value: string);
    function  GetIsRunning: boolean;
    function  GetIsInstalled: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   SetServiceDescription(description: String);
    procedure   StartService;
    procedure   StopService;
    property    ServiceName: string read FServiceName write SetServiceName;
    property    IsRunning : boolean read GetIsRunning ;
    property    IsInstalled : boolean read GetIsInstalled ;
  end;


  LPSERVICE_DESCRIPTIONA = ^SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM LPSERVICE_DESCRIPTIONA}
  _SERVICE_DESCRIPTIONA = record
    lpDescription: LPSTR;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONA}
  SERVICE_DESCRIPTIONA = _SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM SERVICE_DESCRIPTIONA}
  TServiceDescriptionA = SERVICE_DESCRIPTIONA;
  PServiceDescriptionA = LPSERVICE_DESCRIPTIONA;

//
// Service description string
//

  LPSERVICE_DESCRIPTIONW = ^SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM LPSERVICE_DESCRIPTIONW}
  _SERVICE_DESCRIPTIONW = record
    lpDescription: LPWSTR;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONW}
  SERVICE_DESCRIPTIONW = _SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM SERVICE_DESCRIPTIONW}
  TServiceDescriptionW = SERVICE_DESCRIPTIONW;
  PServiceDescriptionW = LPSERVICE_DESCRIPTIONW;

{$IFDEF UNICODE}
  SERVICE_DESCRIPTION = SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM SERVICE_DESCRIPTION}
  LPSERVICE_DESCRIPTION = LPSERVICE_DESCRIPTIONW;
  {$EXTERNALSYM LPSERVICE_DESCRIPTION}
  TServiceDescription = TServiceDescriptionW;
  PServiceDescription = PServiceDescriptionW;
{$ELSE}
  SERVICE_DESCRIPTION = SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM SERVICE_DESCRIPTION}
  LPSERVICE_DESCRIPTION = LPSERVICE_DESCRIPTIONA;
  {$EXTERNALSYM LPSERVICE_DESCRIPTION}
  TServiceDescription = TServiceDescriptionA;
  PServiceDescription = PServiceDescriptionA;
{$ENDIF}


LPVOID = Pointer;
{$EXTERNALSYM LPVOID}

const
  SERVICE_CONFIG_DESCRIPTION     = 1;
  {$EXTERNALSYM SERVICE_CONFIG_DESCRIPTION}


implementation


{
******************************* TServiceManager ********************************
}
constructor TServiceManager.Create;
var
  sMachine: string;
begin
  FServiceName := '';
  FServiceHandle := 0;

  sMachine := ''; // localhost is used when empty
  FServiceManagerHandle := OpenSCManager(PChar(sMachine), Nil, SC_MANAGER_CONNECT);


end;

destructor TServiceManager.Destroy;
begin

  if FServiceHandle <> 0 then
    CloseServiceHandle(FServiceHandle);

  CloseServiceHandle(FServiceManagerHandle);

  inherited;
end;


function TServiceManager.GetIsInstalled: boolean;
begin
  if FServiceName = '' then
    raise exception.create( 'TServiceManager.GetIsRunning: ServiceName not assigned' ) ;
  result := FServiceHandle <> 0 ;
end;

function TServiceManager.GetIsRunning: boolean;
begin
  if FServiceName = '' then
    raise exception.create( 'TServiceManager.GetIsRunning: ServiceName not assigned' ) ;
  result := FServiceHandle <> 0 ;
end;

procedure TServiceManager.SetServiceDescription(description: String);
type
TFnc_ChangeServiceConfig2 = function(hService: SC_HANDLE; dwInfoLevel: DWORD;
                                     lpInfo: LPVOID): BOOL; stdcall;
var
  srvDesc: TServiceDescription;
  csc2: TFnc_ChangeServiceConfig2;
  dllHandle: THandle;
begin

  if FServiceHandle = 0 then begin
    Raise Exception.Create('TServiceManager.SetServiceDescription: Service not selected');
    Exit;
  end;

  srvDesc.lpDescription := pChar(Description);

  { this ChangeServiceConfigA doesn't exist on NT 4 and lover}
  dllHandle := 0;
  try
    dllHandle := LoadLibrary('advapi32');
    @csc2 := GetProcAddress(dllHandle, 'ChangeServiceConfig2A')
  except
    csc2 := nil;
  end;

  if Assigned(csc2) then begin
    try
      csc2(FServiceHandle, SERVICE_CONFIG_DESCRIPTION, @srvDesc);
    except

    end;
  end;

  try
    FreeLibrary(dllHandle);
  except

  end;



end;

procedure TServiceManager.SetServiceName(Value: string);
begin

  if Value = FServiceName then
    Exit;

  FServiceName := Value;

  if FServiceHandle <> 0 then
    CloseServiceHandle(FServiceHandle);

  FServiceHandle := OpenService(FServiceManagerHandle, PChar(FServiceName),
                    SERVICE_CHANGE_CONFIG or SERVICE_START or SERVICE_STOP);

end;

procedure TServiceManager.StartService;
var
  dummy: PChar;
begin

  if FServiceHandle = 0 then begin
    Raise Exception.Create('TServiceManager.StartService: Service not selected');
    Exit;
  end;

  winsvc.StartService(FServiceHandle, 0, dummy);

end;

procedure TServiceManager.StopService;
var
  stat: TServiceStatus;
begin

  if FServiceHandle = 0 then begin
    Raise Exception.Create('TServiceManager.StopService: Service not selected');
    Exit;
  end;

  ControlService(FServiceHandle, SERVICE_CONTROL_STOP, stat);
end;



end.
