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
    function GetSendBugReportEmailOnCGIFailure: boolean;
    function GetLogToApplicationSubDirectory: boolean;
    function GetLogFullHTTPRequest: boolean;
  protected
    function  GetRegistryKey: string; virtual;
    function  GetRegistryValue(const AName, ADefault: string): string; virtual;
    function  GetINIFileName: string; virtual;
    property  INI: TtiINIFile Read FINI;

    function  GetWebServiceDisplayName: string; virtual;
    function  GetWebServiceShortName: string; virtual;
    function  GetLogPathToSharedFiles: string; virtual;
    function  GetPathToBin: string; virtual;
    function  GetPathToCGIBin: string; virtual;
    function  GetPathToStaticPages: string; virtual;
    function  GetPathToPassThrough: string; virtual;
    function  GetAppServerSeverityToLog: string;
    function  GetCGIExtensionLogging: boolean;
    function  GetCGIExtensionSeverityToLog: string;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure   RegisterLog; virtual;
    property    LogPathToSharedFiles: string Read GetLogPathToSharedFiles;
    property    LogToApplicationSubDirectory: boolean read GetLogToApplicationSubDirectory;
    property    LogFullHTTPRequest: boolean read GetLogFullHTTPRequest;
    property    AppServerSeverityToLog: string read GetAppServerSeverityToLog;
    property    CGIExtensionLogging: boolean read GetCGIExtensionLogging;
    property    CGIExtensionSeverityToLog: string read GetCGIExtensionSeverityToLog;

    property    WebServiceShortName: string Read GetWebServiceShortName;
    property    WebServiceDisplayName: string Read GetWebServiceDisplayName;

    property    PathToStaticPages: string Read GetPathToStaticPages;
    property    PathToBin: string Read GetPathToBin;
    property    PathToCGIBin: string Read GetPathToCGIBin;
    property    PathToPassThrough: string Read GetPathToPassThrough;

    property    Port: integer read GetPort;

    property    SendBugReportEmailOnCGIFailure: boolean read GetSendBugReportEmailOnCGIFailure;
    function    INIFileName: string;
    function    INIFilePath: string;
    function    ApplicationINISectionName: string;
  end;

implementation
uses
   tiLog
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
  cINILog_CGIExtensionSeverityToLog = 'CGIExtensionSeverityToLog';
  CINILog_LogToApplicationSubDirectory = 'LogToApplicationSubDirectory';
  CINILog_LogFullHTTPRequest = 'LogFullHTTPRequest';
  cINILog_AppServerSeverityToLog = 'AppServerSeverityToLog';
  CINILog_DefaultLogFullHTTPRequest = false;

  cINIService = 'Web Server';
  cINIService_ShortName = 'ShortName';
  cINIService_DisplayName  = 'DiaplayName';
  cINIService_ShortNameDefault = 'tiDBWebServer';
  cINIService_DisplayNameDefault  = 'TechInsite Web Server';
  cINIService_PathToStaticPages = 'PathToStaticPages';
  cINIService_PathToBin = 'PathToBin';
  cINIService_PathToCGIBin = 'PathToCGIBin';
  cINIService_PathToPassThrough = 'PathToPassThrough';
  cINIService_DefaultPathToStaticPages = 'StaticPages';
  cINIService_DefaultPathToBin = 'Bin';
  cINIService_DefaultPathToCGIBin = 'CGI-Bin';
  cINIService_DefaultPathToPassThrough = 'PathToPassThrough';
  CINILog_DefaultLogToApplicationSubDirectory = true;
  cINIService_SendBugReportEmailOnCGIFailure = 'SendBugReportEmailOnCGIFailure';
  cINIService_SendBugReportEmailOnCGIFailureDefault = true;

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

function TtiWebServerConfig.INIFilePath: string;
begin
  result:= ExtractFilePath(INIFileName);
end;

function TtiWebServerConfig.ApplicationINISectionName: string;
begin
  result := tiRemoveExtension(ExtractFileName(tiGetModuleFileName));
end;

function TtiWebServerConfig.GetRegistryKey: string;
begin
  result:= 'SOFTWARE\TechInsite\Shared';
end;

function TtiWebServerConfig.GetRegistryValue(const AName, ADefault: string): string;
var
  LRegistry: TRegistry;
begin
  LRegistry := TRegistry.Create(HKEY_LOCAL_MACHINE);
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    LRegistry.OpenKey(GetRegistryKey, True);
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

function TtiWebServerConfig.GetLogFullHTTPRequest: boolean;
begin
  Result:= INI.ReadBool(cINILog, cINILog_LogFullHTTPRequest, cINILog_DefaultLogFullHTTPRequest);
end;

function TtiWebServerConfig.GetLogPathToSharedFiles: string;
begin
  Result:= INI.ReadString(cINILog, cINILog_PathToSharedFiles, cINILog_DefaultPathToSharedFiles);
  if LogToApplicationSubDirectory then
    Result:= tiAddTrailingSlash(Result) + tiExtractFileNameOnly(ParamStr(0));
  ExpandFileName(Result);
end;

function TtiWebServerConfig.GetLogToApplicationSubDirectory: boolean;
begin
  Result:= INI.ReadBool(cINILog, cINILog_LogToApplicationSubDirectory, cINILog_DefaultLogToApplicationSubDirectory);
end;

function TtiWebServerConfig.GetAppServerSeverityToLog: string;
begin
  Result := INI.ReadString(cINILog, cINILog_AppServerSeverityToLog, '');
end;

function TtiWebServerConfig.GetCGIExtensionLogging: boolean;
begin
  Result:= INI.ReadBool(cINILog, cINILog_CGIExtensionLogging, False);
end;

function TtiWebServerConfig.GetCGIExtensionSeverityToLog: string;
begin
  Result := INI.ReadString(cINILog, cINILog_CGIExtensionSeverityToLog, '');
end;

function TtiWebServerConfig.GetWebServiceDisplayName;
begin
  Result:= FINI.ReadString(cINIService, cINIService_DisplayName, cINIService_DisplayNameDefault);
end;

function TtiWebServerConfig.GetWebServiceShortName: string;
begin
  Result:= FINI.ReadString(cINIService, cINIService_ShortName, cINIService_ShortNameDefault);
end;

function TtiWebServerConfig.INIFileName: string;
begin
  result := INI.FileName;
end;

procedure TtiWebServerConfig.RegisterLog;
begin
  gLog.RegisterLog(TtiLogToFile.CreateWithDateInFileName(LogPathToSharedFiles));
  if AppServerSeverityToLog <> '' then
    gLog.SevToLogAsString := AppServerSeverityToLog
  else
    gLog.SevToLog := [lsError, lsWarning, lsUserInfo, lsNormal];
end;

function TtiWebServerConfig.GetPathToBin: string;
begin
  Result:= INI.ReadString(cINIService, cINIService_PathToBin, '');
  if Result = '' then
  begin
    Result:= tiGetEXEPath + PathDelim + cINIService_DefaultPathToBin;
    INI.WriteString(cINIService, cINIService_PathToBin, Result);
  end;
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

function TtiWebServerConfig.GetPathToPassThrough: string;
begin
  Result:= INI.ReadString(cINIService, cINIService_PathToPassThrough, '');
  if Result = '' then
  begin
    Result:= tiGetEXEPath + PathDelim + cINIService_DefaultPathToPassThrough;
    INI.WriteString(cINIService, cINIService_PathToPassThrough, Result);
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

function TtiWebServerConfig.GetSendBugReportEmailOnCGIFailure: boolean;
begin
  Result:= INI.ReadBool(cINIService, cINIService_SendBugReportEmailOnCGIFailure,
      cINIService_SendBugReportEmailOnCGIFailureDefault);
end;

end.

