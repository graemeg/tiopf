program AdrsBookUIModalForms;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FPersonEdit in 'FPersonEdit.pas' {FormPersonEdit},
  FAdrsEdit in 'FAdrsEdit.pas' {FormAdrsEdit},
  FEAdrsEdit in 'FEAdrsEdit.pas' {FormEAdrsEdit},
  FAdrsAbs in 'FAdrsAbs.pas' {FormAdrsAbs},
  FtiPerEditDialog in '..\..\..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FtiDialogAbs in '..\..\..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs},
  AdrsType_BOM in '..\..\BOM\AdrsType_BOM.pas',
  Adrs_BOM in '..\..\BOM\Adrs_BOM.pas',
  Adrs_Dependencies in '..\..\BOM\Adrs_Dependencies.pas';

{$R *.res}

begin
  Application.Initialize;
  {$ifdef DELPHI2007ORABOVE}
  Application.MainFormOnTaskbar := True;
  {$endif}
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
