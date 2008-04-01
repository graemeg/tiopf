program DUnitAdrsBookGUI;

uses
  FastMM4,
  GUITestRunner,
  Forms,
  Adrs_SrvAutoMap,
  DUnitAdrsBookDependencies in 'DUnitAdrsBookDependencies.pas ';

{$R *.RES}

begin
  Application.Initialize;

  // ToDo: Ask which persistence mechanism
  Adrs_SrvAutoMap.RegisterMappings;

  DUnitAdrsBookDependencies.RegisterTests;
  DUnitAdrsBookDependencies.ConnectToDatabase;
  GUITestRunner.RunRegisteredTests;
end.

