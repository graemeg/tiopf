unit tiXmlRpcObjectLockingClientMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  xmlrpctypes, xmlrpcclient,

  tiXmlRpcObjectLocking;

type
  TXmlRpcObjectLockingClientMainForm = class(TForm)
    HostNameEdit: TEdit;
    Label1: TLabel;
    ObjectIdentityEdit: TEdit;
    Label2: TLabel;
    Button1: TButton;
    ObjectLockLabel: TLabel;
    ObjectLockEdit: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    FObjectLockingClient: TtiXmlRpcObjectLockingClient;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;  
  end;

var
  XmlRpcObjectLockingClientMainForm: TXmlRpcObjectLockingClientMainForm;

implementation

{$R *.DFM}

{ TXmlRpcObjectLockingClientMainForm }

procedure TXmlRpcObjectLockingClientMainForm.AfterConstruction;
begin
  inherited;
  FObjectLockingClient := TtiXmlRpcObjectLockingClient.Create;
end;

procedure TXmlRpcObjectLockingClientMainForm.BeforeDestruction;
begin
  FObjectLockingClient.Free;
  inherited;
end;

procedure TXmlRpcObjectLockingClientMainForm.Button1Click(Sender: TObject);
begin
  FObjectLockingClient.HostName := HostNameEdit.Text;
  ObjectLockEdit.Text := FObjectLockingClient.AcquireLock(ObjectIdentityEdit.Text);
end;

end.
