program Demo_InheritanceAutoMap;

uses
  tiLog,
  Forms,
  FMainInheritance in '..\Demo_10_Inheritance_Common\FMainInheritance.pas' {FormMainInheritance},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in '..\Demo_10_Inheritance_Common\Client_BOM.pas',
  FClientAbsEdit in '..\Demo_10_Inheritance_Common\FClientAbsEdit.pas' {FormClientAbsEdit},
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FClientPersonEdit in '..\Demo_10_Inheritance_Common\FClientPersonEdit.pas' {FormClientPersonEdit},
  FClientCompanyEdit in '..\Demo_10_Inheritance_Common\FClientCompanyEdit.pas' {FormClientCompanyEdit};

{$R *.res}

begin
  SetupLogForClient;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  Client_BOM.RegisterMappings;
  
  Application.Initialize;
  Application.CreateForm(TFormMainInheritance, FormMainInheritance);
  Application.Run;
end.
