program DUnitAdrsBookGUI;

uses
  FastMM4,
  GUITestRunner,
  Forms,
  Adrs_SrvAutoGenSQL,
  DUnitAdrsBookDependencies in 'DUnitAdrsBookDependencies.pas ';

{$R *.RES}

begin
  Application.Initialize;

  // ToDo: Ask which persistence mechanism
  Adrs_SrvAutoGenSQL.RegisterMappings;

  DUnitAdrsBookDependencies.RegisterTests;
  DUnitAdrsBookDependencies.ConnectToDatabase;
  GUITestRunner.RunRegisteredTests;
end.

