program Demo_Inheritance;

uses
  tiLogToGUI,
  Forms,
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in 'Client_BOM.pas',
  FClientAbsEdit in 'FClientAbsEdit.pas' {FormClientAbsEdit},
  FClientPersonEdit in 'FClientPersonEdit.pas' {FormClientPersonEdit},
  FClientCompanyEdit in 'FClientCompanyEdit.pas' {FormClientCompanyEdit},
  FMainInheritance in 'FMainInheritance.pas' {FormMainInheritance},
  WhichPersistenceMechanism in '..\Common\WhichPersistenceMechanism.pas',
  Client_AutoMap_Svr in 'Client_AutoMap_Svr.pas',
  Client_HardCodedVisitors_Svr in 'Client_HardCodedVisitors_Svr.pas',
  Client_DBIndependentVisitors_Svr in 'Client_DBIndependentVisitors_Svr.pas',
  FtiDialogAbs in '..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin

  if not TFormConnectToDatabase.Execute then
    Exit; //==>

  if not WhichPersistenceMechanism.Execute(
    Client_AutoMap_Svr.RegisterMappings,
    Client_HardCodedVisitors_Svr.RegisterVisitors,
    Client_DBIndependentVisitors_Svr.RegisterVisitors) then
    Exit; //==>

  Application.Initialize;
  Application.CreateForm(TFormMainInheritance, FormMainInheritance);
  Application.Run;

end.
