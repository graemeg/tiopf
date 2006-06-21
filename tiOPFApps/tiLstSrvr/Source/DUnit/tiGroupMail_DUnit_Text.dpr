program tiGroupMail_DUnit_Text;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TextTestRunner,
  tiGroupMailDUnitDependencies in 'tiGroupMailDUnitDependencies.pas',
  tiGroupMailDependencies in '..\tiGroupMail\tiGroupMailDependencies.pas';

begin
  tiGroupMailDUnitDependencies.RegisterTests ;
  TextTestRunner.RunRegisteredTests;
end.
