program tiXmlRpcObjectLockingClient;

uses
  Forms,
  tiXmlRpcObjectLockingClientMainForm in 'tiXmlRpcObjectLockingClientMainForm.pas' {XmlRpcObjectLockingClientMainForm},
  tiObjectLockingService in 'tiObjectLockingService.pas',
  tiXmlRpcObjectLocking in 'tiXmlRpcObjectLocking.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TXmlRpcObjectLockingClientMainForm, XmlRpcObjectLockingClientMainForm);
  Application.Run;
end.
