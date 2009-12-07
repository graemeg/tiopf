program Demo_OneToOne;

uses
  Forms,
  tiLogToGUI,
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  Client_BOM in 'Client_BOM.pas',
  FMainOneToOne in 'FMainOneToOne.pas' {FormMainInheritance},
  FtiDialogAbs in '..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin

  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TFormMainInheritance, FormMainInheritance);
  Application.Run;

end.

