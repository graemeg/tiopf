unit tiDUnitINI;

interface
uses
   INIFiles
  ,tiBaseObject
 ;

const
  cErrorCanNotSetINIFileToReadWrite = 'INI file <%s> is read-only and can not be set to read-write.';


type

  TDUntiLocalSettings = class(TtiBaseObject)
  private
    FINIFile: TINIFile;
    function  GetAppConfigDir_nonGlobal: string;
    procedure SetAppConfigDir_nonGlobal(const AValue: string);
    function  GetAppConfigDir_Global: string;
    procedure SetAppConfigDir_Global(const AValue: string);

    function  GetTempDir: string;
    procedure SetTempDir(const AValue: string);
    function  GetWindowsSysDir: string;
    procedure SetWindowsSysDir(const AValue: string);
    function  GetUserName: string;
    procedure SetUserName(const AValue: string);
    function  GetComputerName: string;
    procedure SetComputerName(const AValue: string);
  public
    constructor Create;
    destructor  Destroy; override;

    function  ReadString(const ASection: string; AIdent: string): string;

    property AppConfigDir_nonGlobal: string read GetAppConfigDir_nonGlobal write SetAppConfigDir_nonGlobal;
    property AppConfigDir_Global: string read GetAppConfigDir_Global write SetAppConfigDir_Global;
    property TempDir: string read GetTempDir write SetTempDir;
    property WindowsSysDir: string read GetWindowsSysDir write SetWindowsSysDir;
    property UserName: string read GetUserName write SetUserName;
    property ComputerName: string read GetComputerName write SetComputerName;

    class    function  IsDataMissing: boolean;
    class    function  FileName: string;
    class    procedure CreateDefaultFile;

  end;

  // INI file that is deployed with tiOPF and stores which persistence layers are to be tested
function GDUnitINICommon : TINIFile;

implementation
uses
   tiUtils
  ,tiExcept
  ,SysUtils
 ;

const
  CINIMachineSettings = 'MachineSettings';
  CAppConfigDir_nonGlobal = 'AppConfigDir_nonGlobal';
  CAppConfigDir_Global = 'AppConfigDir_Global';
  CTempDir = 'TempDir';
  CWindowsSysDir = 'WindowsSysDir';
  CUserName = 'UserName';
  CComputerName = 'ComputerName';
  CDefaultStringValue = ''; // Must be empty string because it's checked in other locations. Tidy up.

var
  UDUnitINICommon : TINIFile;

function _CreateINIFile(const AFileName: string) : TINIFile;
var
  LDir: String;
begin
  LDir:= ExtractFilePath(AFileName);
  if not DirectoryExists(LDir) then
    tiForceDirectories(LDir);
  if FileExists(AFileName) then
  begin
    if tiIsFileReadOnly(AFileName) then
      tiSetFileReadOnly(AFileName,false);
    if tiIsFileReadOnly(AFileName) then
      raise EtiOPFUserException.CreateFmt(cErrorCanNotSetINIFileToReadWrite,
                 [AFileName]);
  end;
  Result := TINIFile.Create(AFileName);
end;

function GDUnitINICommon : TINIFile;
var
  LFileName : string;
begin
  if UDUnitINICommon = nil then
  begin
    LFileName := tiGetEXEPath + PathDelim + 'DUnitTIOPF.ini';
    UDUnitINICommon := _CreateINIFile(LFileName);
  end;
  result := UDUnitINICommon;
end;


{ TDUntiLocalSettings }

constructor TDUntiLocalSettings.Create;
begin
  inherited Create;
  FINIFile := _CreateINIFile(FileName);
end;

class procedure TDUntiLocalSettings.CreateDefaultFile;
var
  LO: TDUntiLocalSettings;
begin
  LO:= Create;
  try
    LO.AppConfigDir_nonGlobal;
    LO.AppConfigDir_Global;
    LO.TempDir;
    LO.WindowsSysDir;
    LO.UserName;
    LO.ComputerName;
  finally
    LO.Free;
  end;
end;

destructor TDUntiLocalSettings.Destroy;
begin
  FINIFile.Free;
  inherited;
end;

class function TDUntiLocalSettings.FileName: string;
begin
  Result := tiGetAppConfigDir;
  Result:= Copy(Result, 1, tiPosR(PathDelim, Result)-1);
  Result:= Result + PathDelim + 'DUnitTIOPF\DUnitTIOPF.ini';
end;

function TDUntiLocalSettings.GetAppConfigDir_Global: string;
begin
  Result:= ReadString(CINIMachineSettings, CAppConfigDir_Global);
end;

function TDUntiLocalSettings.GetAppConfigDir_nonGlobal: string;
begin
  Result:= ReadString(CINIMachineSettings, CAppConfigDir_nonGlobal);
end;

function TDUntiLocalSettings.GetComputerName: string;
begin
  Result:= ReadString(CINIMachineSettings, CComputerName);
end;

class function TDUntiLocalSettings.IsDataMissing: boolean;
var
  LO: TDUntiLocalSettings;
begin
  LO:= Create;
  try
    result:=
      (LO.AppConfigDir_nonGlobal = CDefaultStringValue) or
      (LO.AppConfigDir_Global = CDefaultStringValue) or
      (LO.TempDir = CDefaultStringValue) or
      (LO.WindowsSysDir = CDefaultStringValue) or
      (LO.UserName = CDefaultStringValue) or
      (LO.ComputerName = CDefaultStringValue);
  finally
    LO.Free;
  end;
end;

function TDUntiLocalSettings.GetTempDir: string;
begin
  Result:= ReadString(CINIMachineSettings, CTempDir);
end;

function TDUntiLocalSettings.GetUserName: string;
begin
  Result:= ReadString(CINIMachineSettings, CUserName);
end;

function TDUntiLocalSettings.GetWindowsSysDir: string;
begin
  Result:= ReadString(CINIMachineSettings, CWindowsSysDir);
end;

function TDUntiLocalSettings.ReadString(
  const ASection: string; AIdent: string): string;
begin
  Result:= FINIFile.ReadString(ASection, AIdent, CDefaultStringValue);
  if Result = '' then
    FINIFile.WriteString(ASection, AIdent, CDefaultStringValue);
end;

procedure TDUntiLocalSettings.SetAppConfigDir_Global(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CAppConfigDir_Global, AValue);
end;

procedure TDUntiLocalSettings.SetAppConfigDir_nonGlobal(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CAppConfigDir_nonGlobal, AValue);
end;

procedure TDUntiLocalSettings.SetComputerName(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CComputerName, AValue);
end;

procedure TDUntiLocalSettings.SetTempDir(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CTempDir, AValue);
end;

procedure TDUntiLocalSettings.SetUserName(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CUserName, AValue);
end;

procedure TDUntiLocalSettings.SetWindowsSysDir(const AValue: string);
begin
  FINIFile.WriteString(CINIMachineSettings, CWindowsSysDir, AValue);
end;

initialization

finalization
  UDUnitINICommon.Free;
  
end.

