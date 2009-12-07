program DUnitAdrsBookGUI;

uses
  FastMM4,
  GUITestRunner,
  Forms,
  DUnitAdrsBookDependencies in 'DUnitAdrsBookDependencies.pas';

{$R *.RES}

begin
  Application.Initialize;
  DUnitAdrsBookDependencies.RegisterTests;
  DUnitAdrsBookDependencies.ConnectToDatabase;
  GUITestRunner.RunRegisteredTests;
end.

