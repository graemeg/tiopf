program tiDBParamMgr;

uses
  tiEncryptSimple,
  Forms,
  FMain in 'FMain.pas' {Form2},
  FEdit in 'FEdit.pas' {FormEdit};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
