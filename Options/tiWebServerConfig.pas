unit tiWebServerConfig;

{$I tiDefines.inc}

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
    function  GetINIFileName: string; virtual;
    property  INI: TtiINIFile Read FINI;

    function  GetWebServiceDisplayName: string; virtual;
    function  GetWebServiceShortName: string; virtual;
    function  GetLogPathToSharedFiles: string; virtual;
    function  GetPathToCGIBin: string; virtual;
    function  GetPathToStaticPages: string; virtual;
    function  GetPathToPassThrough: string; virtual;
    function  GetCGIExtensionLogging: boolean;
    function  GetCGIExtensionSeverityToLog: string;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure   RegisterLog; virtual;
    property    LogPathToSharedFiles: string Read GetLogPathToSharedFiles;
    property    LogToApplicationSubDirectory: boolean read GetLogToApplicationSubDirectory;
    property    LogFullHTTPRequest: boolean read GetLogFullHTTPRequest;
    property    CGIExtensionLogging: boolean read GetCGIExtensionLogging;
    property    CGIExtensionSeverityToLog: string read GetCGIExtensionSeverityToLog;

    property    WebServiceShortName: string Read GetWebServiceShortName;
    property    WebServiceDisplayName: string Read GetWebServiceDisplayName;

    property    PathToStaticPages: string Read GetPathToStaticPages;
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
 ;

const
  // Logging
  cINILog = 'Log';
  cINILog_PathToSharedFiles = 'PathToSharedFiles';
  cINILog_CGIExtensionLogging = 'CGIExtensionLogging';
  cINILog_CGIExtensionSeverityToLog = 'CGIExtensionSeverityToLog';
  CINILog_LogToApplicationSubDirectory = 'LogToApplicationSubDirectory';
  CINILog_LogFullHTTPRequest = 'LogFullHTTPRequest';
  CINILog_DefaultLogFullHTTPRequest = false;

  cINIService = 'Web Server';
  cINIService_ShortName = 'ShortName';
  cINIService_DisplayName  = 'DiaplayName';
  cINIService_ShortNameDefault = 'tiDBWebServer';
  cINIService_DisplayNameDefault  = 'TechInsite Web Server';
  cINIService_PathToStaticPages = 'PathToStaticPages';
  cINIService_PathToCGIBin = 'PathToCGIBin';
  cINIService_PathToPassThrough = 'PathToPassThrough';
  cINIService_DefaultPathToStaticPages = 'StaticPages';
  cINIService_DefaultPathToCGIBin = 'cgi-bin';    // DO NOT CHANGE THE CASE
  cINIService_DefaultPathToPassThrough = 'PathToPassThrough';
  CINILog_DefaultLogToApplicationSubDirectory = true;
  cINIService_SendBugReportEmailOnCGIFailure = 'SendBugReportEmailOnCGIFailure';
  cINIService_SendBugReportEmailOnCGIFailureDefault = true;

  cINIService_IdentPort = 'Port';
  cINIService_DefaultPort = 80;

var
  cINILog_DefaultPathToSharedFiles: string;

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
var
  d: string;
begin
  d := tiGetCommonAppDataDir('');
  Result:= tiFixPathDelim(tiAddTrailingSlash(d) + 'tiopf\webserver.ini');
  Log('Webserver config file: <' + Result + '>', lsDebug);
  if not DirectoryExists(d) then
    tiForceDirectories(d);
end;

function TtiWebServerConfig.INIFilePath: string;
begin
  result:= ExtractFilePath(INIFileName);
end;

function TtiWebServerConfig.ApplicationINISectionName: string;
begin
  result := tiRemoveExtension(ExtractFileName(tiGetModuleFileName));
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

function TtiWebServerConfig.GetCGIExtensionLogging: boolean;
begin
  Result:= INI.ReadBool(cINILog, cINILog_CGIExtensionLogging, False);
end;

function TtiWebServerConfig.GetCGIExtensionSeverityToLog: string;
begin
  Result := INI.ReadString(cINILog, cINILog_CGIExtensionSeverityToLog, '');
end;

function TtiWebServerConfig.GetWebServiceDisplayName: string;
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


initialization
  cINILog_DefaultPathToSharedFiles := tiFixPathDelim(tiGetTempDir + '\TechInsite\Log');

end.

