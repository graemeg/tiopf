program AdrsBookUIModalForms;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FtiPerEditDialog in '..\..\..\GUI\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FPersonEdit in 'FPersonEdit.pas' {FormPersonEdit},
  FAdrsEdit in 'FAdrsEdit.pas' {FormAdrsEdit},
  FEAdrsEdit in 'FEAdrsEdit.pas' {FormEAdrsEdit},
  FAdrsAbs in 'FAdrsAbs.pas' {FormAdrsAbs};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
