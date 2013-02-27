program Demo_EditDataInGUI;

uses
  tiLog,
  Forms,
  FMainEditDataInGUI in 'FMainEditDataInGUI.pas' {Form2},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in 'Client_BOM.pas',
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase};

{$R *.res}

begin

  SetupLogForClient ;

  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
