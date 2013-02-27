{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

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
unit pbpClientForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Buttons, ExtCtrls, StdCtrls, Grids, DBGrids, DBCtrls, ComCtrls, Mask, ToolWin,

  jclStrings,

  TB2Item, TB2Dock, TB2Toolbar, tiListViewPlus, tiReadOnly,

  tiPersist, tiListView, tiPtnVisPerObj,

  pbpUtils, pbpBusinessClasses, pbpTypes, pbpClientCreationWizardForm, pbpResources;

type
  TClientForm = class(TForm, IUnknown, ISupportsToolbar2000, IPerObjAbsListener)
    Bevel1: TBevel;
    Label13: TLabel;
    ActionList: TActionList;
    NewClientAction: TAction;
    ClientDetailsPanel: TPanel;
    ClientNameEdit: TEdit;
    Label3: TLabel;
    ClientNumberEdit: TEdit;
    Label15: TLabel;
    AddressPanel: TPanel;
    IdentityRecordPanel: TPanel;
    UndesirablePanel: TPanel;
    UndesirableReasonRichEdit: TRichEdit;
    Label1: TLabel;
    DateOfBirthEdit: TEdit;
    Label14: TLabel;
    Label17: TLabel;
    UndesirableCodeEdit: TEdit;
    Label5: TLabel;
    UndesirableEdit: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    PhoneWorkEdit: TEdit;
    PhoneHomeEdit: TEdit;
    Label12: TLabel;
    Label11: TLabel;
    EmailAddressEdit: TEdit;
    PhoneMobileEdit: TEdit;
    TBDock: TTBDock;
    ClientFormToolbar: TTBToolbar;
    TBItem5: TTBItem;
    TBItem1: TTBItem;
    EditAction: TAction;
    Label2: TLabel;
    ClientIdentityRecordListView: TtiListViewPlus;
    StreetEdit: TEdit;
    SuburbEdit: TEdit;
    StateEdit: TEdit;
    PostCodeEdit: TEdit;
    Label21: TLabel;
    Label20: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    NotesRichEdit: TRichEdit;
    Label8: TLabel;
    procedure SaveClientActionExecute(Sender: TObject);
    procedure IdentificationListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure NewClientActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure ClientIdentityRecordListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
  private
    FData: TClient;
    FReadOnly: Boolean;
    function GetData: TClient;
    procedure SetData(const Value: TClient);
    procedure UpdateControls;
  protected
    procedure DoVerbRefresh;
    procedure PerObjAbsListener_Changed(Sender: TObject);
    procedure IPerObjAbsListener.Changed = PerObjAbsListener_Changed;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetToolbarParent(Parent: TWinControl);
    property Data: TClient read GetData write SetData;
  end;

var
  ClientForm: TClientForm;

implementation

uses pbpMainForm;

{$R *.DFM}

{ TClientForm }

procedure TClientForm.AfterConstruction;
begin
  inherited;

  // Setup action list
  ActionList.Images := GetResourceImageList24x24;
  NewClientAction.ImageIndex := GLYFX_24_NEW;
  EditAction.ImageIndex      := GLYFX_24_EDIT;
  // Setup toolbar
  ClientFormToolbar.Images := GetResourceImageList24x24;

  ClientIdentityRecordListView.LV.Color := IdentityRecordPanel.Color;
end;

function TClientForm.GetData: TClient;
begin
  Result := FData;
end;

procedure TClientForm.SetData(const Value: TClient);
begin
  if Assigned(FData) then
  begin
    FData.RemoveListener(Self);
  end;

  FData := Value;

  if FData <> nil then
  begin
    if FData.ObjectState = posPK then
      FData.Read;
    FData.AddListener(Self);
  end;

  UpdateControls;
end;

procedure TClientForm.SaveClientActionExecute(Sender: TObject);
begin
  Assert(FData <> nil);
  Assert((FData.ObjectState in [posCreate, posUpdate]) or (FData.IdentityRecords.Dirty));
  if FData.Owner = nil then
    PawnBroker.Clients.Add(FData);
  FData.IdentityRecords.Save;
  FData.Save;
  FData.IdentityRecords.Dirty := False;
end;

procedure TClientForm.IdentificationListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  Assert(pData is TClientIdentityRecord);
  ClientIdentityRecord := (pData as TClientIdentityRecord);
  if ClientIdentityRecord.IdentityRecordType <> nil then
  begin
    pResult := Format('%s', [(pData as TClientIdentityRecord).IdentityRecordType.Name]);
  end
  else
  begin
    pResult := '';
  end;
end;

procedure TClientForm.DoVerbRefresh;
var
  Client: TClient;
begin
  inherited;
  Client := FData;
  SetData(nil);
  gTiPerMgr.VisMgr.Execute('read.refresh', Client);
  SetData(Client);
end;

procedure TClientForm.SetToolbarParent(Parent: TWinControl);
begin
  if Parent <> nil then
    ClientFormToolbar.Parent := Parent
  else
    ClientFormToolbar.Parent := TBDock;
end;

{ TClientForm - actions }

procedure TClientForm.NewClientActionExecute(Sender: TObject);
var
  WizardForm: TClientCreationWizardForm;
begin
  WizardForm := TClientCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;

procedure TClientForm.EditActionExecute(Sender: TObject);
var
  WizardForm: TClientCreationWizardForm;
begin
  MainForm.TreeViewAdapter.BackStack.Push(Self);
  WizardForm := TClientCreationWizardForm.Create(Application);
  WizardForm.Execute(Self.Data);
end;

procedure TClientForm.PerObjAbsListener_Changed(Sender: TObject);
begin
  UpdateControls;
end;

procedure TClientForm.UpdateControls;
begin
  if FData = nil then
  begin
    ClientNumberEdit.Clear;
    DateOfBirthEdit.Clear;
    ClientNameEdit.Clear;

    StreetEdit.Clear;
    SuburbEdit.Clear;
    StateEdit.Clear;
    PostCodeEdit.Clear;

    PhoneHomeEdit.Text := '';
    PhoneWorkEdit.Text := '';
    PhoneMobileEdit.Text := '';
    EmailAddressEdit.Text := '';

//    PhotoImage.Picture.Clear;
    ClientIdentityRecordListView.Data := nil;

    UndesirableEdit.Text := '';
    UndesirableCodeEdit.Text := '';
    UndesirableReasonRichEdit.Text := '';

    NotesRichEdit.Lines.Clear;
  end
  else
  begin
    if FData.ObjectState = posPK then
      FData.Read;

    // ClientDetailsPanel
    ClientNumberEdit.Text := Format('%d', [FData.ClientNumber]);
    ClientNameEdit.Text := FData.Caption;
    DateOfBirthEdit.Text := DateTimeToStr(FData.DateOfBirth);

    // PhoneNumbersPanel
    PhoneHomeEdit.Text := FData.PhoneHome;
    PhoneWorkEdit.Text := FData.PhoneWork;
    PhoneMobileEdit.Text := FData.PhoneMobile;
    EmailAddressEdit.Text := FData.EmailAddress;

    // AddressPanel
    if Assigned(FData.CurrentAddress) then
    begin
      StreetEdit.Text := FData.CurrentAddress.Street;
      SuburbEdit.Text := FData.CurrentAddress.Suburb;
      StateEdit.Text := FData.CurrentAddress.State;
      PostCodeEdit.Text := FData.CurrentAddress.PostCode;
    end
    else
    begin
      StreetEdit.Clear;
      SuburbEdit.Clear;
      StateEdit.Clear;
      PostCodeEdit.Clear;
    end;

    // IdentityRecordPanel
    ClientIdentityRecordListView.Data := FData.IdentityRecords.List;
    ClientIdentityRecordListView.Refresh;

    // UndesirablePanel
    UndesirableEdit.Text := BooleanToStr(FData.Undesirable);
    UndesirableCodeEdit.Text := FData.UndesirableCode;
    UndesirableReasonRichEdit.Text := FData.UndesirableReason;

    // NotesPanel
    NotesRichEdit.Text := FData.Notes;
  end;
end;

procedure TClientForm.BeforeDestruction;
begin
  Data := nil;
  inherited;
end;

procedure TClientForm.ClientIdentityRecordListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  Assert(pData is TClientIdentityRecord);
  ClientIdentityRecord := (pData as TClientIdentityRecord);
  if ClientIdentityRecord.IdentityRecordType <> nil then
  begin
    pResult := Format('%s', [(pData as TClientIdentityRecord).IdentityRecordType.Name]);
  end
  else
  begin
    pResult := '';
  end;
end;

end.

