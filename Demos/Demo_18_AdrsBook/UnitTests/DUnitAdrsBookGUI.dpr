program DUnitAdrsBookGUI;

uses
  FastMM4,
  GUITestRunner,
  Forms,
  Adrs_Dependencies,
  DUnitAdrsBookDependencies in 'DUnitAdrsBookDependencies.pas';

{$R *.RES}

begin
  Application.Initialize;
  DUnitAdrsBookDependencies.RegisterTests;
  Adrs_Dependencies.ConnectToDatabase;
  GUITestRunner.RunRegisteredTests;
end.

