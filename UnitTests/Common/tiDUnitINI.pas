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
var
  LFileName : string;
begin
  inherited Create;
  LFileName := tiGetAppConfigDir;
  LFileName:= Copy(LFileName, 1, tiPosR(PathDelim, LFileName)-1);
  LFileName:= LFileName + PathDelim + 'DUnitTIOPF\DUnitTIOPF.ini';
  FINIFile := _CreateINIFile(tiFixPathDelim(LFileName));
end;

destructor TDUntiLocalSettings.Destroy;
begin
  FINIFile.Free;
  inherited;
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
  Result:= FINIFile.ReadString(ASection, AIdent, '');
  if Result = '' then
    FINIFile.WriteString(ASection, AIdent, '');
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

