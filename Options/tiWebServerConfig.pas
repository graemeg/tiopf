unit tiWebServerConfig;

interface
uses
   tiBaseObject
  ,tiINI
 ;

type

  TtiWebServerConfig = class(TtiBaseObject)
  private
    FINI: TtiINIFile;
    function GetPort: integer;
  protected
    function  GetRegistryValue(const AName, ADefault: string): string; virtual;
    function  GetINIFileName: string; virtual;
    property  INI: TtiINIFile Read FINI;

    function  GetWebServiceDiaplayName: string; virtual;
    function  GetWebServiceShortName: string; virtual;
    function  GetLogPathToSharedFiles: string; virtual;
    function  GetPathToCGIBin: string; virtual;
    function  GetPathToStaticPages: string; virtual;
    function  GetCIGExtensionLogging: boolean;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure   RegisterLog; virtual;
    property    LogPathToSharedFiles: string Read GetLogPathToSharedFiles;
    property    CGIExtensionLogging: boolean read GetCIGExtensionLogging;

    property    WebServiceShortName: string Read GetWebServiceShortName;
    property    WebServiceDiaplayName: string Read GetWebServiceDiaplayName;

    property    PathToStaticPages: string Read GetPathToStaticPages;
    property    PathToCGIBin: string Read GetPathToCGIBin;

    property    Port: integer read GetPort;

  end;

implementation
uses
   tiWebServerConstants
  ,tiLog
  ,tiLogToFile
  ,tiUtils
  ,SysUtils
  ,Registry
  ,Windows
 ;

const
  // Logging
  cINILog = 'Log';
  cINILog_PathToSharedFiles = 'PathToSharedFiles';
  cINILog_DefaultPathToSharedFiles = 'C:\TechInsite\Log';
  cINILog_CGIExtensionLogging = 'CGIExtensionLogging';

  cINIService = 'Web Server';
  cINIService_ShortName = 'ShortName';
  cINIService_DisplayName  = 'DiaplayName';
  cINIService_ShortNameDefault = 'tiDBWebServer';
  cINIService_DisplayNameDefault  = 'TechInsite Web Server';
  cINIService_PathToStaticPages = 'PathToStaticPages';
  cINIService_PathToCGIBin = 'PathToCGIBin';
  cINIService_DefaultPathToStaticPages = 'StaticPages';
  cINIService_DefaultPathToCGIBin = 'CGI-Bin';

  cINIService_IdentPort = 'Port';
  cINIService_DefaultPort = 80;

{ TtiWebServerConfig }

constructor TtiWebServerConfig.Create;
begin
  inherited;
  FINI:= TtiINIFile.Create(GetINIFileName);
end;

destructor TtiWebServerConfig.Destroy;
begin
  FINI.Free;
  inherited;
end;

function TtiWebServerConfig.GetINIFileName: string;
begin
  Result:= GetRegistryValue('Shared INI File Location', '\TechInsite\Shared\Config.ini');
  if not DirectoryExists(ExtractFilePath(Result)) then
    ForceDirectories(ExtractFilePath(Result));
end;

function TtiWebServerConfig.GetRegistryValue(const AName, ADefault: string): string;
var
  LRegistry: TRegistry;
const
  cKey   = 'SOFTWARE\TechInsite\Shared';
begin
  LRegistry := TRegistry.Create(HKEY_LOCAL_MACHINE);
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    LRegistry.OpenKey(cKey, True);
    Result := LRegistry.ReadString(AName);
    if Result = '' then
    begin
      Result:= ExtractFileDrive(ParamStr(0)) + ADefault;
      LRegistry.WriteString(AName, Result);
    end;
  finally
    LRegistry.Free;
  end;
end;


function TtiWebServerConfig.GetLogPathToSharedFiles: string;
begin
  Result:= INI.ReadString(cINILog, cINILog_PathToSharedFiles, cINILog_DefaultPathToSharedFiles);
  Result:= tiAddTrailingSlash(Result) + tiExtractFileNameOnly(ParamStr(0));
  ExpandFileName(Result);
end;

function TtiWebServerConfig.GetCIGExtensionLogging: boolean;
begin
  Result:= INI.ReadBool(cINILog, cINILog_CGIExtensionLogging, False);
end;

function TtiWebServerConfig.GetWebServiceDiaplayName;
begin
  Result:= FINI.ReadString(cINIService, cINIService_DisplayName, cINIService_DisplayNameDefault);
end;

function TtiWebServerConfig.GetWebServiceShortName: string;
begin
  Result:= FINI.ReadString(cINIService, cINIService_ShortName, cINIService_ShortNameDefault);
end;

procedure TtiWebServerConfig.RegisterLog;
begin
  gLog.RegisterLog(TtiLogToFile.CreateWithDateInFileName(LogPathToSharedFiles));
end;

function TtiWebServerConfig.GetPathToCGIBin: string;
begin
  Result:= INI.ReadString(cINIService, cINIService_PathToCGIBin, '');
  if Result = '' then
  begin
    Result:= tiGetEXEPath + PathDelim + cINIService_DefaultPathToCGIBin;
    INI.WriteString(cINIService, cINIService_PathToCGIBin, Result);
  end;
end;

function TtiWebServerConfig.GetPathToStaticPages: string;
begin
  Result:= INI.ReadString(cINIService, cINIService_PathToStaticPages, '');
  if Result = '' then
  begin
    Result:= tiGetEXEPath + PathDelim + cINIService_DefaultPathToStaticPages;
    INI.WriteString(cINIService, cINIService_PathToStaticPages, Result);
  end;
end;

function TtiWebServerConfig.GetPort: integer;
begin
  Result:= INI.ReadInteger(cINIService, cINIService_IdentPort, cINIService_DefaultPort);
end;

end.

