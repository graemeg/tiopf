program Demo_OneToManyAutoMap;

uses
  Forms,
  tiLog,
  FMainOneToMany in '..\Demo_09_OneToMany_Common\FMainOneToMany.pas' {FormMainOneToMany},
  Client_BOM in '..\Demo_09_OneToMany_Common\Client_BOM.pas',
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FClientEdit in '..\Demo_09_OneToMany_Common\FClientEdit.pas' {FormClientEdit},
  FPhoneNumberEdit in '..\Demo_09_OneToMany_Common\FPhoneNumberEdit.pas' {FormPhoneNumberEdit},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase};

{$R *.res}

begin
  SetupLogForClient;
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

end.
