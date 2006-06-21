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
unit pbpClientIdentityRecordsFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ActnList, StdCtrls, Buttons, Comctrls,

  tiListView, tiListViewPlus,

  pbpUtils, pbpBusinessClasses;

type
  TClientIdentityRecordsFrame = class(TFrame)
    Label4: TLabel;
    Bevel1: TBevel;
    AddClientIdentityRecordButton: TBitBtn;
    ReplaceClientIdentityRecordButton: TBitBtn;
    DeleteClientIdentityRecordButton: TBitBtn;
    IdentityRecordDetailsEdit: TEdit;
    IdentityRecordTypeComboBox: TComboBox;
    Label13: TLabel;
    Label8: TLabel;
    ActionList: TActionList;
    AddClientIdentityRecordAction: TAction;
    ReplaceClientIdentityRecordAction: TAction;
    DeleteClientIdentityRecordAction: TAction;
    ClientIdentityRecordListView: TtiListViewPlus;
    procedure AddClientIdentityRecordActionExecute(Sender: TObject);
    procedure IdentityRecordTypeComboBoxDropDown(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DeleteClientIdentityRecordActionExecute(Sender: TObject);
    procedure ClientIdentityRecordListViewFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure ClientIdentityRecordListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ReplaceClientIdentityRecordActionExecute(Sender: TObject);
    procedure ClientIdentityRecordListViewDblClick(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
  private
    FData: TClient;
    FSelectedClientIdentityRecord: TClientIdentityRecord;
    procedure SetData(const Value: TClient);
    procedure SetSelectedClientIdentityRecord(
      const Value: TClientIdentityRecord);
    { Private declarations }
  public
    procedure AfterConstruction; override;
    procedure UpdateData;
    property Data: TClient read FData write SetData;
    property SelectedClientIdentityRecord: TClientIdentityRecord
      read FSelectedClientIdentityRecord write SetSelectedClientIdentityRecord;
  end;

implementation

{$R *.DFM}

{ TClientIdentityRecordsFrame }

procedure TClientIdentityRecordsFrame.AfterConstruction;
var
  ListColumn: TtiListColumn;
begin
  inherited;
  ListColumn := ClientIdentityRecordListView.ListColumns.Add;
  ListColumn.Derived := True;
  ListColumn.DisplayLabel := 'Type';
  ListColumn.OnDeriveColumn := ClientIdentityRecordListViewListColumns0DeriveColumn;

  ListColumn := ClientIdentityRecordListView.ListColumns.Add;
  ListColumn.Derived := False;
  ListColumn.DisplayLabel := 'Details';
  ListColumn.FieldName := 'Details';

  ListColumn := ClientIdentityRecordListView.ListColumns.Add;
  ListColumn.Derived := False;
  ListColumn.DisplayLabel := 'In use';
  ListColumn.FieldName := 'InUse';
end;

procedure TClientIdentityRecordsFrame.SetData(const Value: TClient);
begin
  FData := Value;                                               
  SelectedClientIdentityRecord := nil;
  ClientIdentityRecordListView.Data := FData.IdentityRecords.List;
end;

procedure TClientIdentityRecordsFrame.SetSelectedClientIdentityRecord(
  const Value: TClientIdentityRecord);
begin
  FSelectedClientIdentityRecord := Value;
  if FSelectedClientIdentityRecord <> nil then
  begin
    if FSelectedClientIdentityRecord.IdentityRecordType <> nil then
      IdentityRecordTypeComboBox.Text := FSelectedClientIdentityRecord.IdentityRecordType.Name
    else
      IdentityRecordTypeComboBox.Text := '';
    IdentityRecordDetailsEdit.Text := FSelectedClientIdentityRecord.Details;
  end
  else
  begin
    IdentityRecordTypeComboBox.Text := '';
    IdentityRecordDetailsEdit.Text := '';
  end;
end;

{ TClientIdentityRecordsFrame - events }

procedure TClientIdentityRecordsFrame.ClientIdentityRecordListViewListColumns0DeriveColumn(
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

procedure TClientIdentityRecordsFrame.ClientIdentityRecordListViewDblClick(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  Assert(ClientIdentityRecordListView.SelectedData <> nil);
  SelectedClientIdentityRecord := ClientIdentityRecordListView.SelectedData as TClientIdentityRecord;
end;

procedure TClientIdentityRecordsFrame.ClientIdentityRecordListViewFilterData(
  pData: TPersistent; var pbInclude: Boolean);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  ClientIdentityRecord := pData as TClientIdentityRecord;
  pbInclude := not ClientIdentityRecord.Deleted;
end;

procedure TClientIdentityRecordsFrame.IdentityRecordTypeComboBoxDropDown(
  Sender: TObject);
var
  Counter: Integer;
begin
  IdentityRecordTypeComboBox.Items.Clear;
  for Counter := 0 to PawnBroker.ClientIdentityRecordTypes.Count-1 do
  begin
    IdentityRecordTypeComboBox.Items.Add(
      PawnBroker.ClientIdentityRecordTypes.Items[Counter].Caption);
  end;
end;

procedure TClientIdentityRecordsFrame.UpdateData;
begin

end;

{ TClientIdentityRecordsFrame - events - actions }

procedure TClientIdentityRecordsFrame.ActionListUpdate(
  Action: TBasicAction; var Handled: Boolean);
begin
  case CaseObject(Action,
    [ AddClientIdentityRecordAction,
      ReplaceClientIdentityRecordAction,
      DeleteClientIdentityRecordAction ]) of

    0 : AddClientIdentityRecordAction.Enabled :=
          Assigned(FData) and
          (IdentityRecordTypeComboBox.Text <> '') and
          (IdentityRecordDetailsEdit.Text <> '');

    1 : ReplaceClientIdentityRecordAction.Enabled :=
          Assigned(FData) and
          Assigned(ClientIdentityRecordListView.SelectedData) and
          (((ClientIdentityRecordListView.SelectedData) as TClientIdentityRecord).InUse = False) and
          (IdentityRecordTypeComboBox.Text <> '') and
          (IdentityRecordDetailsEdit.Text <> '');

    2 : DeleteClientIdentityRecordAction.Enabled :=
          Assigned(FData) and
          Assigned(SelectedClientIdentityRecord) and
          (SelectedClientIdentityRecord.InUse = False);
  end;
end;

procedure TClientIdentityRecordsFrame.AddClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
begin
  Assert(Assigned(FData));
  Assert(IdentityRecordTypeComboBox.Text <> '');
  Assert(IdentityRecordDetailsEdit.Text <> '');

  IdentityRecord := TClientIdentityRecord.CreateNew;
  IdentityRecord.IdentityRecordType :=
    PawnBroker.ClientIdentityRecordTypes.FindOrCreateByName(IdentityRecordTypeComboBox.Text);
  IdentityRecord.Details := IdentityRecordDetailsEdit.Text;
  FData.IdentityRecords.Add(IdentityRecord);
  FData.IdentityRecords.Dirty := True;

//  Data.ClientIdentityRecordProxies.Add(TClientIdentityRecordProxy.Create(IdentityRecord));

  ClientIdentityRecordListView.Refresh;

  (* Return focus to identity record controls *)
  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;
end;

procedure TClientIdentityRecordsFrame.DeleteClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
begin
  Assert(Assigned(FData));
  Assert(Assigned(SelectedClientIdentityRecord));

  if SelectedClientIdentityRecord.InUse = True then
    raise Exception.Create('You cannot delete a client identity record that has been used elsewhere');

  SelectedClientIdentityRecord.Deleted := True;
  FData.IdentityRecords.Dirty := True;

  ClientIdentityRecordListView.Refresh;

  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;
end;

procedure TClientIdentityRecordsFrame.ReplaceClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
begin
  Assert(Assigned(FData));
  Assert(Assigned(ClientIdentityRecordListView.SelectedData));
  Assert(IdentityRecordTypeComboBox.Text <> '');
  Assert(IdentityRecordDetailsEdit.Text <> '');

  IdentityRecord :=
    ClientIdentityRecordListView.SelectedData as TClientIdentityRecord;

  if IdentityRecord.InUse = True then
    raise Exception.Create('You cannot replace a client identity record that has been used elsewhere');

  IdentityRecord.IdentityRecordType :=
    PawnBroker.ClientIdentityRecordTypes.FindOrCreateByName(IdentityRecordTypeComboBox.Text);
  IdentityRecord.Details := IdentityRecordDetailsEdit.Text;
  IdentityRecord.Dirty := True;
  FData.IdentityRecords.Dirty := True;

  ClientIdentityRecordListView.Refresh;

  (* Return focus to identity record controls *)
  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;

end;



end.
