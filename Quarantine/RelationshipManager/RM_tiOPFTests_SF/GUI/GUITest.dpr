program GUITest;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {frmMainForm},
  BOMUnit in '..\Core\BOMUnit.pas',
  Customer_VisitorsUnit in '..\Core\Customer_VisitorsUnit.pas',
  Order_VisitorsUnit in '..\Core\Order_VisitorsUnit.pas',
  RelationshipManagerUnit in '..\Core\RelationshipManagerUnit.pas',
  AssociationObjectVisitorsUnit in '..\Core\AssociationObjectVisitorsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
