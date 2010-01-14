#define PATH_TO_WORKING_TIOPF3 "%path_to_working_tiopf3%"
#define PATH_TO_WORKING_3RDPARTY "%path_to_working_3rdparty%"
#define VERSION_NUMBER "%version_number%"
#define PATH_TO_WORKING_DEPLOY "%path_to_working_deploy%"
#define OUTPUT_BASE_FILENAME "%output_base_filename%"

;#define PATH_TO_WORKING_TIOPF3 "C:\Sourceforge\TechInsite\tiOPF3\Trunk"
;#define PATH_TO_WORKING_3RDPARTY "C:\Sourceforge\TechInsite\tiOPF3\Trunk\3rdParty"
;#define VERSION_NUMBER "x.x.x.x"
;#define PATH_TO_WORKING_DEPLOY "C:\Sourceforge\TechInsite\tiOPF3\Trunk\_Deploy"
;#define OUTPUT_BASE_FILENAME "tiOPF-Setup-x.x.x.x"

[_ISXTool]
EnableISX=true

[Setup]
AppName=tiOPF
;DefaultDirName={commondocs}\tiOPF
DefaultDirName={userdocs}\tiOPF
OutputBaseFilename={#OUTPUT_BASE_FILENAME}
OutputDir={#PATH_TO_WORKING_DEPLOY}
AppVerName=tiOPF {#VERSION_NUMBER}
LicenseFile={#PATH_TO_WORKING_TIOPF3}\InstallScripts\Setup\License.txt
AppPublisher=TechInsite
AppPublisherURL=http://www.tiopf.com
AppSupportURL=http://www.tiopf.com
AppUpdatesURL=http://www.tiopf.com
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

[Types]
Name: tiOPF3All; Description: tiOPF3, Demos and third party code
Name: tiOPF3Only; Description: tiOPF3 source only
Name: Custom; Description: Custom; flags: iscustom

[Components]
Name: tiOPF3; Description: Main tiOPF3 files; types: tiOPF3Only tiOPF3All Custom
Name: tiOPF3Demos; Description: tiOPF3 demo projects; types: tiOPF3All Custom
Name: TrdParty; Description: Optional 3rd party code (you may already have these); types: tiOPF3All Custom
Name: TrdParty\DUnit; Description: DUnit (so the unit tests will build); types: tiOPF3All Custom
Name: TrdParty\FastMM; Description: FastMM (leak detection for the unit tests); types: tiOPF3All Custom
Name: TrdParty\Indy; Description: Indy (used by the remote persistence layer); types: tiOPF3All Custom
Name: TrdParty\JCL; Description: JCL (gives unit name and line number when a unit test fails); types: tiOPF3All Custom

[Files]
Source: {#PATH_TO_WORKING_TIOPF3}\Compilers\*.*; DestDir: {app}\tiOPF3\Compilers; Flags: recursesubdirs; components: tiOPF3
Source: {#PATH_TO_WORKING_tiOPF3}\Core\*.*; DestDir: {app}\tiOPF3\Core; Flags: recursesubdirs; components: tiOPF3
Source: {#PATH_TO_WORKING_tiOPF3}\GUI\*.*; DestDir: {app}\tiOPF3\GUI; Flags: recursesubdirs; components: tiOPF3
Source: {#PATH_TO_WORKING_tiOPF3}\Options\*.*; DestDir: {app}\tiOPF3\Options; Flags: recursesubdirs; components: tiOPF3
Source: {#PATH_TO_WORKING_tiOPF3}\UnitTests\*.*; DestDir: {app}\tiOPF3\UnitTests; Flags: recursesubdirs; components: tiOPF3
Source: {#PATH_TO_WORKING_tiOPF3}\ApplicationServer\*.*; DestDir: {app}\tiOPF3\ApplicationServer; Flags: recursesubdirs; components: tiOPF3

Source: {#PATH_TO_WORKING_3RDPARTY}\DUnit\*.*; DestDir: {app}\tiOPF3\3rdParty\DUnit; Flags: recursesubdirs; components: TrdParty\DUnit
Source: {#PATH_TO_WORKING_3RDPARTY}\FastMM\*.*; DestDir: {app}\tiOPF3\3rdParty\FastMM; Flags: recursesubdirs; components: TrdParty\FastMM
Source: {#PATH_TO_WORKING_3RDPARTY}\Indy\*.*; DestDir: {app}\tiOPF3\3rdParty\Indy; Flags: recursesubdirs; components: TrdParty\Indy
Source: {#PATH_TO_WORKING_3RDPARTY}\JCL\*.*; DestDir: {app}\tiOPF3\3rdParty\JCL; Flags: recursesubdirs; components: TrdParty\JCL

Source: {#PATH_TO_WORKING_tiOPF3}\Demos\_bin\*.*; DestDir: {app}\tiOPF3\Demos\_bin; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Common\*.*; DestDir: {app}\tiOPF3\Demos\Common; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_01_ReadMe\*.*; DestDir: {app}\tiOPF3\Demos\Demo_01_ReadMe; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_02_LoadPersistenceLayerIfDef\*.*; DestDir: {app}\tiOPF3\Demos\Demo_02_LoadPersistenceLayerIfDef; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_03_LoadPersistenceLayerUses\*.*; DestDir: {app}\tiOPF3\Demos\Demo_03_LoadPersistenceLayerUses; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_04_CreateDatabase\*.*; DestDir: {app}\tiOPF3\Demos\Demo_04_CreateDatabase; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_05_ConnectToDatabase\*.*; DestDir: {app}\tiOPF3\Demos\Demo_05_ConnectToDatabase; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_06_CreateTable\*.*; DestDir: {app}\tiOPF3\Demos\Demo_06_CreateTable; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_07_VisitorBasics\*.*; DestDir: {app}\tiOPF3\Demos\Demo_07_VisitorBasics; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_08_Collection\*.*; DestDir: {app}\tiOPF3\Demos\Demo_08_Collection; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_09_EditDataInGUI\*.*; DestDir: {app}\tiOPF3\Demos\Demo_09_EditDataInGUI; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_10_CollectionWithFilter\*.*; DestDir: {app}\tiOPF3\Demos\Demo_10_CollectionWithFilter; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_10a_CollectionWithCriteria\*.*; DestDir: {app}\tiOPF3\Demos\Demo_10a_CollectionWithCriteria; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_11_OnetoMany\*.*; DestDir: {app}\tiOPF3\Demos\Demo_11_OnetoMany; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_12_Inheritance\*.*; DestDir: {app}\tiOPF3\Demos\Demo_12_Inheritance; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_13_OneToOne\*.*; DestDir: {app}\tiOPF3\Demos\Demo_13_OneToOne; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_14_OrdinalTypes\*.*; DestDir: {app}\tiOPF3\Demos\Demo_14_OrdinalTypes; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_15_LookupList\*.*; DestDir: {app}\tiOPF3\Demos\Demo_15_LookupList; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_16_Dataset\*.*; DestDir: {app}\tiOPF3\Demos\Demo_16_Dataset; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_17_MastApp\*.*; DestDir: {app}\tiOPF3\Demos\Demo_17_MastApp; Flags: recursesubdirs; components: tiOPF3Demos
Source: {#PATH_TO_WORKING_tiOPF3}\Demos\Demo_18_AdrsBook\*.*; DestDir: {app}\tiOPF3\Demos\Demo_18_AdrsBook; Flags: recursesubdirs; components: tiOPF3Demos

[Dirs]
Name: {app}\tiOPF3\Compilers\Delphi7\_dcu
Name: {app}\tiOPF3\Compilers\Delphi2006\_dcu
Name: {app}\tiOPF3\UnitTests\Text\_dcu
Name: {app}\tiOPF3\UnitTests\GUI\_dcu
Name: {app}\tiOPF3\_bin
Name: {app}\tiOPF3\Demos\_dcu

[Icons]
Name: {app}\Uninstall tiOPF3; Filename: {uninstallexe}
Name: {app}\tiOPF3\Doc\Installing the tiOPF; Filename: http://tiopf.sourceforge.net/Doc/Installing/
Name: {app}\tiOPF3\Doc\tiOPF3 Quick Start; Filename: http://tiopf.sourceforge.net/Doc/QuickStart/
Name: {app}\tiOPF3\Doc\tiOPF3 Concepts Manual; Filename: http://tiopf.sourceforge.net/Doc/Concepts/
Name: {app}\tiOPF3\Doc\tiOPF3 Online Help; Filename: http://opensoft.homeip.net/tiopf/

[Run]
Filename: http://tiopf.sourceforge.net/Doc/Installing/; Description: Read Installing the tiOPF; Flags: postinstall shellexec skipifsilent
