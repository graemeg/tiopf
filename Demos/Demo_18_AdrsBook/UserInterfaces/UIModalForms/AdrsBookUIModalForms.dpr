program AdrsBookUIModalForms;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FPersonEdit in 'FPersonEdit.pas' {FormPersonEdit},
  FAdrsEdit in 'FAdrsEdit.pas' {FormAdrsEdit},
  FEAdrsEdit in 'FEAdrsEdit.pas' {FormEAdrsEdit},
  FAdrsAbs in 'FAdrsAbs.pas' {FormAdrsAbs},
  FtiPerEditDialog in '..\..\..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FtiDialogAbs in '..\..\..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
