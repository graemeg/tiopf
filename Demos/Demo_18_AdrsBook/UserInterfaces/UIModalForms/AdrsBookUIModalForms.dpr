program AdrsBookUIModalForms;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FPersonEdit in 'FPersonEdit.pas' {FormPersonEdit},
  FAdrsEdit in 'FAdrsEdit.pas' {FormAdrsEdit},
  FEAdrsEdit in 'FEAdrsEdit.pas' {FormEAdrsEdit},
  FAdrsAbs in 'FAdrsAbs.pas' {FormAdrsAbs},
  FtiPerEditDialog in '..\..\..\..\GUI\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin
  Application.Initialize;
  {$ifdef DELPHI2007ORABOVE}
  Application.MainFormOnTaskbar := True;
  {$endif}
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
