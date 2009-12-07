program Demo_AdrsBook_MGM;

uses
  Forms,
  formmain in 'formmain.pas' {MainFrm},
  model in 'model.pas',
  views in 'views.pas',
  contactmanager in 'contactmanager.pas',
  formcontactmaint in 'formcontactmaint.pas' {ContactMaintFrm},
  tiOIDGUID;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
