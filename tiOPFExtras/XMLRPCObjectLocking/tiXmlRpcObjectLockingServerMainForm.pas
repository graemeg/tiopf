{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Yi Chen, Darren Camp and
  Tom Tuddenham.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiXmlRpcObjectLockingServerMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  tiXmlRpcObjectLocking, ActnList;


type
  TXMLRPCServerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    StartAction: TAction;
    StopAction: TAction;
    procedure StartActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
  private
    FObjectLockingServer: TtiXmlRpcObjectLockingServer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  XMLRPCServerForm: TXMLRPCServerForm;

implementation

{$R *.DFM}

{ TXMLRPCServerForm } 

procedure TXMLRPCServerForm.AfterConstruction;
begin
  inherited;
  FObjectLockingServer := TtiXmlRpcObjectLockingServer.Create;
end;

procedure TXMLRPCServerForm.BeforeDestruction;
begin
  FObjectLockingServer.Free;
  inherited;
end;

{ TXMLRPCServerForm - events }

procedure TXMLRPCServerForm.StartActionExecute(Sender: TObject);
begin
  FObjectLockingServer.Start;
end;

procedure TXMLRPCServerForm.StopActionExecute(Sender: TObject);
begin
  FObjectLockingServer.Stop;
end;

procedure TXMLRPCServerForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action = StartAction then
    StartAction.Enabled := not FObjectLockingServer.Started
  else if Action = StopAction then
    StopAction.Enabled := FObjectLockingServer.Started;
end;

end.
 
