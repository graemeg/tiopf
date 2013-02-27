program Demo_OneToManyHardCoded;

uses
  tiLog,
  Forms,
  tiPersist,
  cTIPersist,
  SysUtils,
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  Client_Svr in 'Client_Svr.pas',
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  Client_BOM in '..\Demo_09_OneToMany_Common\Client_BOM.pas',
  FClientEdit in '..\Demo_09_OneToMany_Common\FClientEdit.pas' {FormClientEdit},
  FPhoneNumberEdit in '..\Demo_09_OneToMany_Common\FPhoneNumberEdit.pas' {FormPhoneNumberEdit},
  FMainOneToMany in '..\Demo_09_OneToMany_Common\FMainOneToMany.pas' {FormMainOneToMany},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase};

{$R *.res}

begin
  SetupLogForClient;
  if not TFormConnectToDatabase.Execute(True) then
    Exit ; //==>

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

end.
