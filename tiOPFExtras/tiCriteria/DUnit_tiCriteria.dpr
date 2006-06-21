program DUnit_tiCriteria;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  tiCriteria in 'tiCriteria.pas',
  tiPtnVisCriteria in 'tiPtnVisCriteria.pas',
  tiCriteria_TST in 'tiCriteria_TST.pas';

{$R *.res}

begin
  tiCriteria_TST.RegisterTests ;
  GUITestRunner.RunRegisteredTests;
end.
