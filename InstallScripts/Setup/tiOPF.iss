#define PATH_TO_WORKING_TIOPF2 "%path_to_working_tiopf2%"
#define PATH_TO_WORKING_3RDPARTY "%path_to_working_3rdparty%"
#define VERSION_NUMBER "%version_number%"
#define PATH_TO_WORKING_DEPLOY "%path_to_working_deploy%"
#define OUTPUT_BASE_FILENAME "%output_base_filename%"

;#define PATH_TO_WORKING_TIOPF2 "C:\Sourceforge\TechInsite\tiOPF2\Trunk"
;#define PATH_TO_WORKING_3RDPARTY "C:\Sourceforge\TechInsite\tiOPF2\Trunk\3rdParty"
;#define VERSION_NUMBER "x.x.x.x"
;#define PATH_TO_WORKING_DEPLOY "C:\Sourceforge\TechInsite\tiOPF2\Trunk\_Deploy"
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
LicenseFile={#PATH_TO_WORKING_TIOPF2}\InstallScripts\Setup\License.txt
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
Name: tiOPF2All; Description: tiOPF2, Demos and third party code
Name: tiOPF2Only; Description: tiOPF2 source only
Name: Custom; Description: Custom; flags: iscustom

[Components]
Name: tiOPF2; Description: Main tiOPF2 files; types: tiOPF2Only tiOPF2All Custom
Name: tiOPF2Demos; Description: tiOPF2 demo projects; types: tiOPF2All Custom
Name: TrdParty; Description: Optional 3rd party code (you may already have these); types: tiOPF2All Custom
Name: TrdParty\DUnit; Description: DUnit (so the unit tests will build); types: tiOPF2All Custom
Name: TrdParty\FastMM; Description: FastMM (leak detection for the unit tests); types: tiOPF2All Custom
Name: TrdParty\Indy; Description: Indy (used by the remote persistence layer); types: tiOPF2All Custom
Name: TrdParty\JCL; Description: JCL (gives unit name and line number when a unit test fails); types: tiOPF2All Custom

[Files]
Source: {#PATH_TO_WORKING_TIOPF2}\Compilers\*.*; DestDir: {app}\tiOPF2\Compilers; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\Core\*.*; DestDir: {app}\tiOPF2\Core; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\GUI\*.*; DestDir: {app}\tiOPF2\GUI; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\Options\*.*; DestDir: {app}\tiOPF2\Options; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\UnitTests\*.*; DestDir: {app}\tiOPF2\UnitTests; Flags: recursesubdirs; components: tiOPF2
Source: {#PATH_TO_WORKING_TIOPF2}\ApplicationServer\*.*; DestDir: {app}\tiOPF2\ApplicationServer; Flags: recursesubdirs; components: tiOPF2

Source: {#PATH_TO_WORKING_3RDPARTY}\DUnit\*.*; DestDir: {app}\tiOPF2\3rdParty\DUnit; Flags: recursesubdirs; components: TrdParty\DUnit
Source: {#PATH_TO_WORKING_3RDPARTY}\FastMM\*.*; DestDir: {app}\tiOPF2\3rdParty\FastMM; Flags: recursesubdirs; components: TrdParty\FastMM
Source: {#PATH_TO_WORKING_3RDPARTY}\Indy\*.*; DestDir: {app}\tiOPF2\3rdParty\Indy; Flags: recursesubdirs; components: TrdParty\Indy
Source: {#PATH_TO_WORKING_3RDPARTY}\JCL\*.*; DestDir: {app}\tiOPF2\3rdParty\JCL; Flags: recursesubdirs; components: TrdParty\JCL

Source: {#PATH_TO_WORKING_TIOPF2}\Demos\_bin\*.*; DestDir: {app}\tiOPF2\Demos\_bin; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Common\*.*; DestDir: {app}\tiOPF2\Demos\Common; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_01_ReadMe\*.*; DestDir: {app}\tiOPF2\Demos\Demo_01_ReadMe; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_02_LoadPersistenceLayerIfDef\*.*; DestDir: {app}\tiOPF2\Demos\Demo_02_LoadPersistenceLayerIfDef; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_03_LoadPersistenceLayerUses\*.*; DestDir: {app}\tiOPF2\Demos\Demo_03_LoadPersistenceLayerUses; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_04_CreateDatabase\*.*; DestDir: {app}\tiOPF2\Demos\Demo_04_CreateDatabase; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_05_ConnectToDatabase\*.*; DestDir: {app}\tiOPF2\Demos\Demo_05_ConnectToDatabase; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_06_CreateTable\*.*; DestDir: {app}\tiOPF2\Demos\Demo_06_CreateTable; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_07_VisitorBasics\*.*; DestDir: {app}\tiOPF2\Demos\Demo_07_VisitorBasics; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_08_Collection\*.*; DestDir: {app}\tiOPF2\Demos\Demo_08_Collection; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_09_EditDataInGUI\*.*; DestDir: {app}\tiOPF2\Demos\Demo_09_EditDataInGUI; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_10_CollectionWithFilter\*.*; DestDir: {app}\tiOPF2\Demos\Demo_10_CollectionWithFilter; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_10a_CollectionWithCriteria\*.*; DestDir: {app}\tiOPF2\Demos\Demo_10a_CollectionWithCriteria; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_11_OnetoMany\*.*; DestDir: {app}\tiOPF2\Demos\Demo_11_OnetoMany; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_12_Inheritance\*.*; DestDir: {app}\tiOPF2\Demos\Demo_12_Inheritance; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_13_OneToOne\*.*; DestDir: {app}\tiOPF2\Demos\Demo_13_OneToOne; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_14_OrdinalTypes\*.*; DestDir: {app}\tiOPF2\Demos\Demo_14_OrdinalTypes; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_15_LookupList\*.*; DestDir: {app}\tiOPF2\Demos\Demo_15_LookupList; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_16_Dataset\*.*; DestDir: {app}\tiOPF2\Demos\Demo_16_Dataset; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_17_MastApp\*.*; DestDir: {app}\tiOPF2\Demos\Demo_17_MastApp; Flags: recursesubdirs; components: tiOPF2Demos
Source: {#PATH_TO_WORKING_TIOPF2}\Demos\Demo_18_AdrsBook\*.*; DestDir: {app}\tiOPF2\Demos\Demo_18_AdrsBook; Flags: recursesubdirs; components: tiOPF2Demos

[Dirs]
Name: {app}\tiOPF2\Compilers\Delphi7\_dcu
Name: {app}\tiOPF2\Compilers\Delphi2006\_dcu
Name: {app}\tiOPF2\UnitTests\Text\_dcu
Name: {app}\tiOPF2\UnitTests\GUI\_dcu
Name: {app}\tiOPF2\_bin
Name: {app}\tiOPF2\Demos\_dcu

[Icons]
Name: {app}\Uninstall tiOPF2; Filename: {uninstallexe}
Name: {app}\tiOPF2\Doc\Installing the tiOPF; Filename: http://tiopf.sourceforge.net/Doc/Installing/
Name: {app}\tiOPF2\Doc\tiOPF2 Quick Start; Filename: http://tiopf.sourceforge.net/Doc/QuickStart/
Name: {app}\tiOPF2\Doc\tiOPF2 Concepts Manual; Filename: http://tiopf.sourceforge.net/Doc/Concepts/
Name: {app}\tiOPF2\Doc\tiOPF2 Online Help; Filename: http://opensoft.homeip.net/tiopf/

[Run]
Filename: http://tiopf.sourceforge.net/Doc/Installing/; Description: Read Installing the tiOPF; Flags: postinstall shellexec skipifsilent
