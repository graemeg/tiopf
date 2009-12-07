program Demo_EditDataInGUI;

uses
  tiLogToGUI,
  Forms,
  FMainEditDataInGUI in 'FMainEditDataInGUI.pas' {Form2},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in 'Client_BOM.pas',
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FtiDialogAbs in '..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin

  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.

