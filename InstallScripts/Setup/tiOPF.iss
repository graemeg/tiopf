#define PATH_TO_WORKING_TIOPF2 "%path_to_working_tiopf2%"
#define PATH_TO_WORKING_3RDPARTY "%path_to_working_3rdparty%"
#define VERSION_NUMBER "%version_number%"
#define PATH_TO_WORKING_DEPLOY "%path_to_working_deploy%"
#define OUTPUT_BASE_FILENAME "%output_base_filename%"

#define PATH_TO_DOC "{app}\tiOPF2\Doc"
#define FILE_NAME_INSTALLING "Installing tiOPF2.chm"

;#define PATH_TO_WORKING_TIOPF2 "C:\Temp\tiOPFBuild\tiOPF2"
;#define PATH_TO_WORKING_3RDPARTY "C:\Temp\tiOPFBuild\3rdParty"
;#define VERSION_NUMBER "2.0.0.0"
;#define PATH_TO_WORKING_DEPLOY "C:\Temp\tiOPFBuild\_Deploy"
;#define OUTPUT_BASE_FILENAME "tiOPF-Setup-2.0.0.0"

[_ISXTool]
EnableISX=true

[Setup]
AppName=tiOPF
DefaultDirName=C:\tiOPF
OutputBaseFilename={#OUTPUT_BASE_FILENAME}
OutputDir={#PATH_TO_WORKING_DEPLOY}
AppVerName=tiOPF {#VERSION_NUMBER}
LicenseFile={#PATH_TO_WORKING_TIOPF2}\InstallScripts\Setup\License.txt
AppPublisher=TechInsite
AppPublisherURL=http://www.techinsite.com.au
AppSupportURL=http://www.techinsite.com.au
AppUpdatesURL=http://www.techinsite.com.au
;WizardSmallImageFile={#DEPLOYPATH}\..\Images\SetupWizardSmallImage.bmp
;WizardImageFile={#DEPLOYPATH}\..\Images\SetupWizardImage.bmp

; These are constant for all installs
DisableProgramGroupPage=yes
Compression=lzma/normal
SolidCompression=yes
DisableStartupPrompt=yes
Uninstallable=yes
UninstallFilesDir={app}\Uninstall
;UninstallDisplayIcon={app}\OPDMSAppService_Setup.exe

[types]
Name: "tiOPF2And3rdParty"; Description: "tiOPF2 and third party code"
Name: "tiOPF2Only"; Description: "tiOPF2 source only"
Name: "Custom"; Description: "Custom"; flags: iscustom

[Components]
Name: "tiOPF2";          Description: "Main tiOPF2 files"; types: tiOPF2Only tiOPF2And3rdParty Custom
Name: "TrdParty";        Description: "Optional 3rd party code (you may already have these)"; types: tiOPF2And3rdParty Custom
Name: "TrdParty\DUnit";  Description: "DUnit (so the unit tests will build)"; types: tiOPF2And3rdParty Custom
Name: "TrdParty\FastMM"; Description: "FastMM (leak detection for the unit tests)"; types: tiOPF2And3rdParty Custom
Name: "TrdParty\Indy";   Description: "Indy (used by the remote persistence layer)"; types: tiOPF2And3rdParty Custom
Name: "TrdParty\JCL";    Description: "JCL (gives unit name and line number when a unit test fails)"; types: tiOPF2And3rdParty Custom

[Files]
Source: {#PATH_TO_WORKING_TIOPF2}\Compilers\*.*; DestDir: {app}\tiOPF2\Compilers; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\Core\*.*; DestDir: {app}\tiOPF2\Core; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\GUI\*.*; DestDir: {app}\tiOPF2\GUI; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\Options\*.*; DestDir: {app}\tiOPF2\Options; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\UnitTests\*.*; DestDir: {app}\tiOPF2\UnitTests; Flags: recursesubdirs; components: tiOPF2
Source: "{#PATH_TO_WORKING_DEPLOY}\{#FILE_NAME_INSTALLING}" ; DestDir: {#PATH_TO_DOC}; components: tiOPF2

Source: {#PATH_TO_WORKING_3RDPARTY}\DUnit\*.*; DestDir: {app}\tiOPF2\3rdParty\DUnit; Flags: recursesubdirs; components: TrdParty\DUnit
Source: {#PATH_TO_WORKING_3RDPARTY}\FastMM\*.*; DestDir: {app}\tiOPF2\3rdParty\FastMM; Flags: recursesubdirs; components: TrdParty\FastMM
Source: {#PATH_TO_WORKING_3RDPARTY}\Indy\*.*; DestDir: {app}\tiOPF2\3rdParty\Indy; Flags: recursesubdirs; components: TrdParty\Indy
Source: {#PATH_TO_WORKING_3RDPARTY}\JCL\*.*; DestDir: {app}\tiOPF2\3rdParty\JCL; Flags: recursesubdirs; components: TrdParty\JCL

[Dirs]
Name: {app}\tiOPF2\Compilers\Delphi7\_dcu
Name: {app}\tiOPF2\Compilers\Delphi2006\_dcu
Name: {app}\tiOPF2\UnitTests\Text\_dcu
Name: {app}\tiOPF2\UnitTests\GUI\_dcu
Name: {app}\tiOPF2\_bin

[Icons]
Name: {app}\Uninstall tiOPF2; Filename: {uninstallexe}
Name: {app}\tiOPF2\Doc\tiOPF2 Online Help; Filename: http://opensoft.homeip.net/tiopf/
Name: {app}\tiOPF2\Doc\tiOPF2 Concepts; Filename: http://www.techinsite.com.au/tiOPF/Doc/Default.htm

[Run]
Filename: "{#PATH_TO_DOC}\{#FILE_NAME_INSTALLING}"; Description: "Read '{#FILE_NAME_INSTALLING}'"; Flags: postinstall shellexec skipifsilent
