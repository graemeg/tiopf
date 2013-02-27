program tiXmlRpcObjectLockingServer;

uses
  Forms,
  tiXmlRpcObjectLockingServerMainForm in 'tiXmlRpcObjectLockingServerMainForm.pas' {XMLRPCServerForm},
  tiXmlRpcObjectLocking in 'tiXmlRpcObjectLocking.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TXMLRPCServerForm, XMLRPCServerForm);
  Application.Run;
end.
