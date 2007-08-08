set pasdoc="C:\Program Files\Pasdoc\pasdoc.exe"
set source_directory=..\..
set target_directory=Doc

cd %source_directory%
if not exist %target_directory% md %target_directory%
%pasdoc% -S InstallScripts\Pasdoc\FilesToDocument.txt -O html -I Core -E %target_directory% --staronly

pause
