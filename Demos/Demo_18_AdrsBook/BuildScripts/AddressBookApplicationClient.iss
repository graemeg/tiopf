#define PATH_TO_DEPLOY      "..\_Deploy"

#define AppServerNameLong  "tiOPF - Address book"
#define Application_Name_Long  "tiOPF - Address book"
#define Application_Name_Short "AddressBook"

[Setup]
AppName={#Application_Name_Short}
AppVerName={#Application_Name_Long}
AppPublisher=TechInsite
AppPublisherURL=http://www.tiopf.com
AppSupportURL=http://www.tiopf.com
AppUpdatesURL=http://www.tiopf.com
DefaultDirName=C:\tiOPF Address Book Demo\Client - Remote
DefaultGroupName=tiOPF\{#Application_Name_Long}
Compression=lzma/fast
SolidCompression=yes
OutputDir=..\_Deploy
OutputBaseFilename={#Application_Name_Short}_Setup
Uninstallable=yes
UninstallFilesDir={app}


[Files]
Source: ..\_Deploy\LauncherClient.exe; DestDir: {app}; Flags: ignoreversion touch

;[Icons]
;Name: {group}\Address book (remote); Filename: {app}\Remote Client\LauncherClient.exe
;Name: {userdesktop}\Address book (remote); Filename: {app}\Remote Client\LauncherClient.exe
;Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Address book (remote); Filename: {app}\Remote Client\LauncherClient.exe
;Name: {app}\Address book (remote); Filename: {app}\Remote Client\LauncherClient.exe

[Run]
Filename: {app}\LauncherClient.exe; Description: Run Address Book (Remote); Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: {app}\Log
Type: files; Name: {app}\AdrsBookUIModalForms.exe
