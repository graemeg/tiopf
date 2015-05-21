unit tiDUnitINI;

{$I tiDefines.inc}

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
    function  GetAppDataDirPrivate: string;
    function  GetAppDataDirPublic: string;
    function  GetTempDir: string;
    function  GetWindowsSysDir: string;
    function  GetUserName: string;
    function  GetComputerName: string;
  public
    constructor Create;
    destructor  Destroy; override;

    function  ReadString(const ASection: string; AIdent: string): string;

    property AppDataDirPrivate: string read GetAppDataDirPrivate;
    property AppDataDirPublic: string read GetAppDataDirPublic;
    property TempDir: string read GetTempDir;
    property WindowsSysDir: string read GetWindowsSysDir;
    property UserName: string read GetUserName;
    property ComputerName: string read GetComputerName;

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
  CAppDataDirPrivate = 'AppDataDirPrivate';
  CAppDataDirPublic = 'AppDataDirPublic';
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
    LFileName := tiRemoveTrailingSlash(tiGetAppDataDirPrivate) + PathDelim + 'DUnitTIOPF.ini';
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
    LO.AppDataDirPrivate;
    LO.AppDataDirPublic;
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
  Result := tiRemoveTrailingSlash(tiGetAppDataDirPrivate) + PathDelim + 'DUnitTIOPF.ini';
end;

function TDUntiLocalSettings.GetAppDataDirPublic: string;
begin
  Result:= ReadString(CINIMachineSettings, CAppDataDirPublic);
end;

function TDUntiLocalSettings.GetAppDataDirPrivate: string;
begin
  Result:= ReadString(CINIMachineSettings, CAppDataDirPrivate);
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
      (LO.AppDataDirPrivate = CDefaultStringValue) or
      (LO.AppDataDirPublic = CDefaultStringValue) or
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

initialization

finalization
  UDUnitINICommon.Free;
  
end.

