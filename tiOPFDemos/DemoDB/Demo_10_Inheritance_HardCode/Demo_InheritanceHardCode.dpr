program Demo_InheritanceHardCode;

uses
  tiLog,
  Forms,
  FMainInheritance in '..\Demo_10_Inheritance_Common\FMainInheritance.pas' {FormMainInheritance},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FClientAbsEdit in '..\Demo_10_Inheritance_Common\FClientAbsEdit.pas' {FormClientAbsEdit},
  Client_BOM in '..\Demo_10_Inheritance_Common\Client_BOM.pas',
  Client_Svr in 'Client_Svr.pas',
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FClientCompanyEdit in '..\Demo_10_Inheritance_Common\FClientCompanyEdit.pas' {FormClientCompanyEdit},
  FClientPersonEdit in '..\Demo_10_Inheritance_Common\FClientPersonEdit.pas' {FormClientPersonEdit};

{$R *.res}

begin
  SetupLogForClient;
  if not TFormConnectToDatabase.Execute(True) then
    Exit ; //==>

  Application.CreateForm(TFormMainInheritance, FormMainInheritance);
  Application.Run;
end.
