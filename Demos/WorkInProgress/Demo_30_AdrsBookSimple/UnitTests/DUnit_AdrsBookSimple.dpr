program DUnit_AdrsBookSimple;

uses
  FastMM4,
  tiBaseObject,
  tiLogToGUI,
  Forms,
  TestFramework,
  GUITestRunner,
  Adrs_Dependencies,
  AdrsBook_TSTSetup in 'AdrsBook_TSTSetup.pas',
  AdrsBook_TST in 'AdrsBook_TST.pas',
  Adrs_Svr in '..\BOM\Adrs_Svr.pas',
  Adrs_BOM in '..\BOM\Adrs_BOM.pas',
  Adrs_Constants in '..\BOM\Adrs_Constants.pas';

{$R *.RES}

begin
  ConnectToDatabase;
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

