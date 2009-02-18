program demo_addressbook;

uses
  Forms,
  frmMain in 'frmMain.pas' {Form1},
  contactmanager in 'contactmanager.pas',
  model in 'model.pas',
  frmEditContact in 'frmEditContact.pas',
  frmEditAddress in 'frmEditAddress.pas' {EditAddressForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
