{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpClientCreationWizardForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ImgList, ActnList, Buttons, ExtCtrls,

  KWizard, RouteMapNodes,

  tiPersist, tiPtnVisPerObj,

  pbpBusinessClasses, pbpUtils, pbpClientFrame,
  pbpClientIdentityRecordsFrame;

type
  TClientCreationWizardForm = class(TForm)
    Wizard: TKWizard;
    KWizardRouteMapNodes1: TKWizardRouteMapNodes;
    ClientPersonalDetailsPage: TKWizardInteriorPage;
    CreateWizardPage: TKWizardInteriorPage;
    ClientFrame: TClientFrame;
    KWizardInteriorPage2: TKWizardInteriorPage;
    ClientIdentityRecordsFrame: TClientIdentityRecordsFrame;
    NotesRichEdit: TRichEdit;
    Label1: TLabel;
    IdleTimer: TTimer;
    procedure CreateWizardPageFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure WizardCancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IdleTimerTimer(Sender: TObject);
  private
    FData: TClient;
    function GetData: TClient;
    procedure SetData(const Value: TClient);
  public
    procedure Execute(Client: TClient = nil);
    procedure UpdateData;
    property Data: TClient read GetData write SetData;
  end;

var
  ClientCreationWizardForm: TClientCreationWizardForm;

implementation

uses pbpMainForm;

{$R *.DFM}

{ TCreateClientDialogForm }

procedure TClientCreationWizardForm.Execute(Client: TClient = nil);
begin
  MainForm.TreeViewAdapter.CurrentChildForm := Self;

  if Client = nil then
  begin
    Client := TClient.CreateNew;
    gTiPerMgr.VisMgr.Execute('read.defaultValues', Client);
    Client.ObjectState := posCreate;
    Data := Client;
  end
  else
  begin
    Data := Client;
  end;
end;

function TClientCreationWizardForm.GetData: TClient;
begin
  UpdateData;
  Result := FData;
end;

procedure TClientCreationWizardForm.CreateWizardPageFinishButtonClick(
  Sender: TObject; var Stop: Boolean);
begin
  Stop := True;

  ClientFrame.UpdateData;

  if FData.ObjectState = posCreate then
    Pawnbroker.Clients.Add(FData);

  Pawnbroker.ClientIdentityRecordTypes.Save;
  FData := GetData;
  FData.Save;
  FData.Changed;

  Close;
end;

procedure TClientCreationWizardForm.SetData(const Value: TClient);
begin
  FData := Value;
  ClientFrame.Data := FData;
  ClientIdentityRecordsFrame.Data := FData;
  NotesRichEdit.Text := FData.Notes;
end;

procedure TClientCreationWizardForm.WizardCancelButtonClick(
  Sender: TObject);
begin
  if MessageDlg('Cancel modifying this client?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Assert(Assigned(Data));
    if Data.ObjectState = posCreate then
    begin
      Data.Free;
    end
    else
    begin
{ TODO -oPBPPRO -cDEFECT :
Code here to send message to contract form that client has been
invalidated (will need to re-read the client in prior to closing). }
    end;
    Close;
  end;
end;


procedure TClientCreationWizardForm.UpdateData;
begin
  ClientFrame.UpdateData;
  ClientIdentityRecordsFrame.UpdateData;
  FData.Notes := NotesRichEdit.Text;
end;

{ TClientCreationWizardForm - events }

procedure TClientCreationWizardForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{ TODO -oPBPRO -cSMELL : Not keen on wizard knowing about main form }
  MainForm.TreeViewAdapter.CurrentChildForm := MainForm.TreeViewAdapter.BackStack.Pop;
  Action := caFree;
end;


procedure TClientCreationWizardForm.IdleTimerTimer(Sender: TObject);
begin
{ TODO -oPBPRO -cSMELL : 
This is sooo wrong. I'm not keen on having a timer on the form controlling
the user interface like this, but it will have to do for now }
  case CaseObject(Wizard.ActivePage, [ClientPersonalDetailsPage]) of
    0: { SelectClientPage }
      begin
        if ClientFrame.Valid then
          ClientPersonalDetailsPage.EnabledButtons := ClientPersonalDetailsPage.EnabledButtons + [bkNext]
        else
          ClientPersonalDetailsPage.EnabledButtons := ClientPersonalDetailsPage.EnabledButtons - [bkNext];
      end;
  end;
end;

end.
