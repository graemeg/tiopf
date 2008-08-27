program Demo_GUIWithMediators;

uses
  Forms,
  frmMain in 'frmMain.pas' {Form1},
  Model_View in 'Model_View.pas',
  Model in 'Model.pas',
  Constants in 'Constants.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
