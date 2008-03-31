program DUnitAdrsBookGUI;

uses
  FastMM4,
  tiBaseObject,
  tiLogToGUI,
  Forms,
  TestFramework,
  GUITestRunner,
  AdrsBook_TST in 'AdrsBook_TST.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

