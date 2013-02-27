program Demo_OneToOne;

uses
  Forms,
  tiLog,
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  Client_BOM in 'Client_BOM.pas',
  FMainOneToOne in 'FMainOneToOne.pas' {FormMainInheritance};

{$R *.res}

begin
  SetupLogForClient;
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TFormMainInheritance, FormMainInheritance);
  Application.Run;

end.
