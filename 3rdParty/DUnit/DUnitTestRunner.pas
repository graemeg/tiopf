unit DUnitTestRunner;

interface

uses
  SysUtils,
  TextTestRunner
  {$IFDEF MSWINDOWS}
  ,GUITestRunner
  {$ENDIF}
  ;

procedure RunRegisteredTests;

implementation

procedure RunRegisteredTests;
begin
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end;

end.
