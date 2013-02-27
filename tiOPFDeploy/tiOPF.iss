[Setup]
; These get set from FinalBuilder to the temp build directory
AppName=tiOPF
DefaultDirName=C:\TechInsite
OutputBaseFilename=tiOPF
AppVerName=tiOPF 1.001
OutputDir=C:\Temp\tiOPFInstall
SourceDir=C:\Temp\TechInsite\
LicenseFile=C:\Temp\TechInsite\Deploy\INNOSetup\license.txt

; These are constant for all installs
DisableProgramGroupPage=yes
compression=zip/9
Uninstallable=no
DisableStartupPrompt=yes

[Files]
Source: "*.*"; DestDir: "{app}"; Flags: recursesubdirs

