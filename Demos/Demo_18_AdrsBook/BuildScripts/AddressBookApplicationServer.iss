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
DefaultDirName=C:\{#Application_Name_Long}
DefaultGroupName=tiOPF\{#Application_Name_Long}
Compression=lzma/fast
SolidCompression=yes
OutputDir=..\_Deploy
OutputBaseFilename={#Application_Name_Short}_Setup
Uninstallable=yes
UninstallFilesDir={app}


[Files]
Source: ..\_Deploy\Version.txt; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookApplicationServer.exe; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookApplicationServerGUI.exe; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookUIModalForms.exe; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\DUnitAdrsBookGUI.exe; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\DUnitAdrsBookText.exe; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\Adrs.XMLLight; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\ADRS.FDB; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\StaticPages\default.htm; DestDir: {app}\StaticPages; Flags: ignoreversion touch
Source: ..\_Deploy\StartApplicationServer.bat; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\StopApplicationServer.bat; DestDir: {app}; Flags: ignoreversion touch
Source: ..\_Deploy\LauncherServer.exe; DestDir: {app}\CGI-Bin; Flags: ignoreversion touch
Source: ..\_Deploy\AddressBook_Setup.zip; DestDir: {app}\StaticPages; Flags: ignoreversion touch
Source: ..\_Deploy\AdrsBookUIModalForms.exe; DestDir: {app}\StaticPages\AddressBook; Flags: ignoreversion touch

[UninstallDelete]
Type: filesandordirs; Name: {app}\Log

[Code]

#include AddBackslash(SourcePath) + "\ServiceControlLibrary.iss"

const
  CServiceName        = 'AddressBookApplicationServer';
  CServiceDisplayName = 'tiOPF - Address book application server';
  CServiceEXEName     = '{app}\AdrsBookApplicationServer.exe';

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
