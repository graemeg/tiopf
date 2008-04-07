#define PATH_TO_DEPLOY      "..\_Deploy"

#define AppServerNameLong  "tiOPF - Address book application server"
#define Application_Name_Long  "tiOPF - Address book application server"
#define Application_Name_Short "AddressBookApplicationServer"

[Setup]
AppName={#Application_Name_Short}
AppVerName={#Application_Name_Long}
AppPublisher=TechInsite
AppPublisherURL=http://www.tiopf.com
AppSupportURL=http://www.tiopf.com
AppUpdatesURL=http://www.tiopf.com
DefaultDirName=C:\tiOPF Address Book Demo
DefaultGroupName=tiOPF\{#Application_Name_Long}
Compression=lzma/fast
SolidCompression=yes
OutputDir=..\_Deploy
OutputBaseFilename={#Application_Name_Short}_Setup
Uninstallable=yes
UninstallFilesDir={app}

[Files]
Source: ..\_Deploy\Version.txt; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookApplicationServer.exe; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookApplicationServerGUI.exe; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\ADRS.FDB; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\StaticPages\default.htm; DestDir: {app}\Application Server\StaticPages; Flags: ignoreversion touch
Source: ..\_Deploy\StartApplicationServer.bat; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\StopApplicationServer.bat; DestDir: {app}\Application Server; Flags: ignoreversion touch
Source: ..\_Deploy\LauncherServer.exe; DestDir: {app}\Application Server\CGI-Bin; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookUIHTML.exe; DestDir: {app}\Application Server\CGI-Bin; Flags: ignoreversion touch
Source: ..\_Deploy\AddressBook_Setup.zip; DestDir: {app}\Application Server\StaticPages; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookUIModalForms.exe; DestDir: {app}\Application Server\StaticPages\AddressBook; Flags: ignoreversion touch

Source: ..\_Deploy\AdrsBookUIModalForms.exe; DestDir: {app}\Client; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookUIConsole.exe; DestDir: {app}\Client; Flags: ignoreversion touch
Source: ..\_Deploy\DUnitAdrsBookGUI.exe; DestDir: {app}\Client; Flags: ignoreversion touch
Source: ..\_Deploy\DUnitAdrsBookText.exe; DestDir: {app}\Client; Flags: ignoreversion touch
Source: ..\_Deploy\Adrs.XMLLight; DestDir: {app}\Client; Flags: ignoreversion touch
Source: ..\_Deploy\ADRS.FDB; DestDir: {app}\Client; Flags: ignoreversion touch

[UninstallDelete]
Type: filesandordirs; Name: {app}\Application Server\Log

[Code]

#include AddBackslash(SourcePath) + "\ServiceControlLibrary.iss"

const
  CServiceName        = 'AddressBookApplicationServer';
  CServiceDisplayName = 'tiOPF - Address book application server';
  CServiceEXEName     = '{app}\Application Server\AdrsBookApplicationServer.exe';

var
  LAppServerWasRunning: boolean;

procedure DoStopAllServices;
begin

  LAppServerWasRunning:= IsServiceRunning(CServiceName);
  if LAppServerWasRunning then
    DoStopService(CServiceName, CServiceDisplayName);
end;

procedure DoInstallAllServices;
begin
  DoInstallDelphiService(ExpandConstant(CServiceEXEName), CServiceName, CServiceDisplayName);
end;

procedure DoStartAllServices;
begin
  if LAppServerWasRunning then
    DoStartService(CServiceName, CServiceDisplayName);
end;

procedure DoUnInstallAllServices;
begin
  DoUninstallDelphiService(ExpandConstant(CServiceEXEName), CServiceName, CServiceDisplayName);
end;

#include AddBackslash(SourcePath) + "\ServiceControlExecute.iss"
