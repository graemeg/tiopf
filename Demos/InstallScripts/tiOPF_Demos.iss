#define PATH_TO_TIOPF2_DEMOS "%path_to_tiopf2_demos%"
#define PATH_TO_TIOPF2 "%path_to_tiopf2%"
#define VERSION_NUMBER "%version_number%"
#define PATH_TO_DEPLOY "%path_to_deploy%"
#define OUTPUT_BASE_FILENAME "%output_base_filename%"

[_ISXTool]
EnableISX=true

[Setup]
AppName=tiOPF2 Demos
DefaultDirName=C:\tiOPF\tiOPF2_Demos
OutputBaseFilename={#OUTPUT_BASE_FILENAME}
OutputDir={#PATH_TO_DEPLOY}
AppVerName=tiOPF2 Demos {#VERSION_NUMBER}
LicenseFile={#PATH_TO_TIOPF2}\InstallScripts\Setup\License.txt
AppPublisher=tiOPF Project
AppPublisherURL=http://tiopf.sourceforge.net
AppSupportURL=http://tiopf.sourceforge.net
AppUpdatesURL=http://tiopf.sourceforge.net
;WizardSmallImageFile={#DEPLOYPATH}\..\Images\SetupWizardSmallImage.bmp
;WizardImageFile={#DEPLOYPATH}\..\Images\SetupWizardImage.bmp

; These are constant for all installs
DisableProgramGroupPage=yes
Compression=lzma/normal
SolidCompression=yes
DisableStartupPrompt=yes
Uninstallable=yes
UninstallFilesDir={app}\Uninstall

[Files]
Source: {#PATH_TO_TIOPF2_DEMOS}\Common\*.*; DestDir: {app}\Common; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_01_LoadPersistenceLayerIfDef\*.*; DestDir: {app}\Demo_01_LoadPersistenceLayerIfDef; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_01_LoadPersistenceLayerUses\*.*; DestDir: {app}\Demo_01_LoadPersistenceLayerUses; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_02_CreateDatabase\*.*; DestDir: {app}\Demo_02_CreateDatabase; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_03_ConnectToDatabase\*.*; DestDir: {app}\Demo_03_ConnectToDatabase; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_04_CreateTable\*.*; DestDir: {app}\Demo_04_CreateTable; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_05_Collection\*.*; DestDir: {app}\Demo_05_Collection; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_06_EditDataInGUI\*.*; DestDir: {app}\Demo_06_EditDataInGUI; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_06_OnetoMany\*.*; DestDir: {app}\Demo_06_OnetoMany; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_07_CollectionWithFilter\*.*; DestDir: {app}\Demo_07_CollectionWithFilter; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\Demo_08_Inheritance\*.*; DestDir: {app}\Demo_08_Inheritance; Flags: recursesubdirs
Source: {#PATH_TO_TIOPF2_DEMOS}\DemoDB.bdsgroup; DestDir: {app}
Source: {#PATH_TO_TIOPF2_DEMOS}\DemoDB.bpg; DestDir: {app}


[Dirs]
Name: {app}\_dcu
Name: {app}\_bin

[Icons]
Name: {app}\Uninstall tiOPF2 Demos; Filename: {uninstallexe}
