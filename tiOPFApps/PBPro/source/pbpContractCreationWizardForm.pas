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
unit pbpContractCreationWizardForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ActnList, ComCtrls, ToolWin, Mask, DBCtrls, ExtCtrls,
  ImgList,

  jclStrings,

  ovcbase, ovcef, ovcpb, ovcnf,

  tiPersist, tiPtnVisPerObj, tiListView, tiListViewCtrls,tiPerAwareCtrls,

  KWizard, RouteMapNodes,

  pbpBusinessClasses, pbpUtils,

  pbpEvents, pbpContractPrint;

type
  IContractCreationWizardListener = interface(IEventListener)
    ['{2BADF722-DBD9-47EB-A710-C46C63D7EC5C}']
    procedure WizardCancelled;
  end;

  TContractCreationWizardForm = class(TForm)
    Wizard: TKWizard;
    SelectClientPage: TKWizardInteriorPage;
    KWizardRouteMapNodes1: TKWizardRouteMapNodes;
    ClientIdentityPage: TKWizardInteriorPage;
    PawnedItemsPage: TKWizardInteriorPage;
    FinishPage: TKWizardInteriorPage;
    ActionList: TActionList;
    ClientIdentityRecordListBox: TtiListViewListBox;
    Label12: TLabel;
    Label25: TLabel;
    Label16: TLabel;
    Label19: TLabel;
    Label10: TLabel;
    SummaryContractItemsListView: TtiListView;
    AddClientIdentityRecordAction: TAction;
    ReplaceClientIdentityRecordAction: TAction;
    DeleteClientIdentityRecordAction: TAction;
    ClientNumberEdit: TEdit;
    ClientNameEdit: TEdit;
    Name: TLabel;
    StreetEdit: TEdit;
    SuburbEdit: TEdit;
    StateEdit: TEdit;
    PostCodeEdit: TEdit;
    EmailAddressEdit: TEdit;
    PhoneMobileEdit: TEdit;
    PhoneWorkEdit: TEdit;
    PhoneHomeEdit: TEdit;
    Label21: TLabel;
    Label20: TLabel;
    Label14: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label9: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    Label7: TLabel;
    NewClientButton: TBitBtn;
    NewClientAction: TAction;
    ImageList: TImageList;
    FindClientButton: TBitBtn;
    FindClientAction: TAction;
    IdentityRecordDetailsEdit: TEdit;
    IdentityRecordTypeComboBox: TComboBox;
    Label8: TLabel;
    Label13: TLabel;
    AddClientIdentityRecordButton: TBitBtn;
    ReplaceClientIdentityRecordButton: TBitBtn;
    DeleteClientIdentityRecordButton: TBitBtn;
    Label4: TLabel;
    CategoryComboBox: TComboBox;
    QuantityEdit: TEdit;
    DescriptionEdit: TEdit;
    ManufacturerComboBox: TComboBox;
    ModelNumberEdit: TEdit;
    SerialNumberEdit: TEdit;
    ValueEdit: TEdit;
    Label6: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    ContractItemNotesRichEdit: TRichEdit;
    Label27: TLabel;
    AddPawnedItemButton: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ContractItemsListView: TtiListView;
    Label5: TLabel;
    AddContractItemAction: TAction;
    Label28: TLabel;
    SummaryClientNumberEdit: TEdit;
    SummaryClientNameEdit: TEdit;
    SummaryContractNotesRichEdit: TRichEdit;
    NotesLabel: TLabel;
    SummaryStreetEdit: TEdit;
    SummarySuburbEdit: TEdit;
    SummaryStateEdit: TEdit;
    SummaryPostCodeEdit: TEdit;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    SummaryPhoneHomeEdit: TEdit;
    SummaryPhoneWorkEdit: TEdit;
    SummaryPhoneMobileEdit: TEdit;
    SummaryEmailAddressEdit: TEdit;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    ReplaceContractItemAction: TAction;
    DeleteContractItemAction: TAction;
    SummaryClientIdentityRecordListView: TtiListView;
    Label39: TLabel;
    UndesirableNoticeLabel: TLabel;
    SubtotalNumericField: TOvcNumericField;
    ContractFeeNumericField: TOvcNumericField;
    InterestValueNumericField: TOvcNumericField;
    InterestRateNumericField: TOvcNumericField;
    RedemptionValueNumericField: TOvcNumericField;
    UndesirableRichEdit: TRichEdit;
    Label29: TLabel;
    Label40: TLabel;
    UndesirableCodeEdit: TEdit;
    UndesirableCheckBox: TCheckBox;
    ClientFinderListView: TtiListView;
    Label41: TLabel;
    ContractNumberEdit: TEdit;
    Label42: TLabel;
    ContractPeriodEdit: TEdit;
    Label43: TLabel;
    StatusEdit: TEdit;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Label44: TLabel;
    SummaryContractNumberEdit: TEdit;
    Label45: TLabel;
    SummaryContractPeriodEdit: TEdit;
    Label46: TLabel;
    SummaryStatusEdit: TEdit;
    PrintDialog: TPrintDialog;
    procedure FinishPageFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure SelectClientPageExitPage(Sender: TObject;
      const Page: TKWizardCustomPage);
    procedure ClientIdentityRecordListBoxCheck(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure ClientIdentityRecordListBoxGetChecked(pData: TPersistent;
      var pbChecked: Boolean);
    procedure ClientIdentificationListBoxListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ActionListUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure FinishPageEnterPage(Sender: TObject;
      const Page: TKWizardCustomPage);
    procedure ContractItemsListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ContractItemsListViewListColumns5DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ReplaceClientIdentityRecordActionExecute(Sender: TObject);
    procedure AddClientIdentityRecordActionExecute(Sender: TObject);
    procedure ClientNameEditExit(Sender: TObject);
    procedure ClientFinderListViewDblClick(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure NewClientActionExecute(Sender: TObject);
    procedure ClientFinderListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FindClientActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdentityRecordTypeComboBoxDropDown(Sender: TObject);
    procedure DeleteClientIdentityRecordActionExecute(Sender: TObject);
    procedure WizardFinishButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClientIdentityRecordListBoxFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure AddContractItemActionExecute(Sender: TObject);
    procedure ManufacturerComboBoxDropDown(Sender: TObject);
    procedure CategoryComboBoxDropDown(Sender: TObject);
    procedure ContractItemsListViewDblClick(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure ReplaceContractItemActionExecute(Sender: TObject);
    procedure DeleteContractItemActionExecute(Sender: TObject);
    procedure ContractItemsListViewFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure WizardCancelButtonClick(Sender: TObject);
    procedure ClientFinderListViewExit(Sender: TObject);
    procedure FinishPageExitPage(Sender: TObject;
      const Page: TKWizardCustomPage);
    procedure SummaryClientIdentityRecordListViewFilterData(
      pData: TPersistent; var pbInclude: Boolean);
  private
    FData: TContract;
    FEventListenerList: TEventListenerList;
    FClientFinder_FamilyName: string;
    FClientFinder_GivenNames: string;
    FSelectedClientIdentityRecord: TClientIdentityRecord;
    FSelectedContractItem: TContractItem;
    procedure DoActivateContract;
    procedure DoUpdateContract;
    procedure SetSelectedContractItem(const Value: TContractItem);
    procedure HandleClientIdentityRecordListBoxDblClick(Sender: TObject);
    function FindClientByName(AFamilyName, AGivenNames: string): TClient;
    function FindOrCreateClientIdentityRecordTypeByName(AName: string): TClientIdentityRecordType;
    function FindOrCreateContractItemCategoryByName(AName: string): TContractItemCategory;
    function FindOrCreateManufacturerByName(AName: string): TManufacturer;
    function FormatClientName(AClient: TClient): string;
    procedure SetData(const Value: TContract);
    function GetClientFinderVisible: Boolean;
    function GetClientFinderHeight: Integer;


    function GetClient: TClient;
    function GetClientAddress: TClientAddress;
    procedure HandleClientFinderListViewFilterData_ExactMatch(pData: TPersistent;
      var pbInclude: Boolean);
    procedure HandleClientFinderListViewFilterData_PartialMatch(
      pData: TPersistent; var pbInclude: Boolean);
    procedure RefreshContractControls;
    procedure SetClient(const Value: TClient);
    procedure SetClientAddress(const Value: TClientAddress);
    procedure SetClientFinderVisible(const Value: Boolean);
    procedure SetSelectedClientIdentityRecord(
      const Value: TClientIdentityRecord);
  public
    procedure Execute(Contract: TContract = nil);
    procedure AddListener(Listener: IContractCreationWizardListener);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure RemoveListener(Listener: IContractCreationWizardListener);
    property Data: TContract read FData write SetData;
    property Client: TClient read GetClient write SetClient;
    property ClientAddress: TClientAddress read GetClientAddress write SetClientAddress;
    property ClientFinderVisible: Boolean read GetClientFinderVisible write SetClientFinderVisible;
    property SelectedClientIdentityRecord: TClientIdentityRecord read FSelectedClientIdentityRecord write SetSelectedClientIdentityRecord;
    property SelectedContractItem: TContractItem read FSelectedContractItem write SetSelectedContractItem;
  end;

var
  ContractCreationWizardForm: TContractCreationWizardForm;

implementation

uses pbpMainForm;

{$R *.DFM}

const
  CLIENT_NAME_TEMPLATE = '<family name>, <given names>';

procedure TContractCreationWizardForm.AddListener(
  Listener: IContractCreationWizardListener);
begin
  FEventListenerList.Add(Listener);
end;

procedure TContractCreationWizardForm.DoActivateContract;
begin
  Assert(Data <> nil);
  Assert(Data.ObjectState = posCreate);

  TContractTransactionGateway.CommitTransaction(FData, -Data.LoanValue, '', cttActivate);
  Data.ContractState := csActive;
  Data.Dirty := True;
  Data.Save;
  Data.Dirty := False;
  Pawnbroker.Contracts.Add(Data);
end;

procedure TContractCreationWizardForm.DoUpdateContract;
begin
  Assert(Data <> nil);
  Assert(Data.ObjectState <> posCreate);
  Data.Dirty := True;
  Data.Save;
  Data.Dirty := False;
  Data.Changed;
end;

procedure TContractCreationWizardForm.SetSelectedContractItem(const Value: TContractItem);
begin
  FSelectedContractItem := Value;
  if Assigned(FSelectedContractItem) then
  begin
    if FSelectedContractItem.Category <> nil then
      CategoryComboBox.Text        := FSelectedContractItem.Category.Name
    else
      CategoryComboBox.Text        := '';
    QuantityEdit.Text              := IntToStr(FSelectedContractItem.Quantity);
    DescriptionEdit.Text           := FSelectedContractItem.Description;
    if FSelectedContractItem.Manufacturer <> nil then
      ManufacturerComboBox.Text    := FSelectedContractItem.Manufacturer.Name
    else
      ManufacturerComboBox.Text    := '';
    ModelNumberEdit.Text           := FSelectedContractItem.ModelNumber;
    SerialNumberEdit.Text          := FSelectedContractItem.SerialNumber;
    ValueEdit.Text                 := Format('%f', [FSelectedContractItem.Value]);
    ContractItemNotesRichEdit.Text := FSelectedContractItem.Notes;
  end
  else
  begin
    CategoryComboBox.Text     := '';
    DescriptionEdit.Text      := '';
    ManufacturerComboBox.Text := '';
    ModelNumberEdit.Text      := '';
    QuantityEdit.Text         := '';
    SerialNumberEdit.Text     := '';
    ValueEdit.Text            := '';
    ContractItemNotesRichEdit.Lines.Clear;
  end;
end;

procedure TContractCreationWizardForm.HandleClientIdentityRecordListBoxDblClick(
  Sender: TObject);
begin
  Assert(ClientIdentityRecordListBox.SelectedData <> nil);
  SelectedClientIdentityRecord := ClientIdentityRecordListBox.SelectedData as TClientIdentityRecord;
end;

procedure TContractCreationWizardForm.HandleClientFinderListViewFilterData_ExactMatch(
  pData: TPersistent; var pbInclude: Boolean);
var
  Client: TClient;
begin
  Client := pData as TClient;

  pbInclude :=
    (AnsiCompareText(FClientFinder_FamilyName, Client.FamilyName) = 0) and
    (AnsiCompareText(FClientFinder_GivenNames, Client.GivenNames) = 0);
end;

procedure TContractCreationWizardForm.HandleClientFinderListViewFilterData_PartialMatch(
  pData: TPersistent; var pbInclude: Boolean);
var
  Client: TClient;
begin
  Client := pData as TClient;

  pbInclude :=
    ( (FClientFinder_FamilyName = '') or (Pos(Uppercase(FClientFinder_FamilyName), Uppercase(Client.FamilyName)) = 1) ) and
    ( (FClientFinder_GivenNames = '') or (Pos(Uppercase(FClientFinder_GivenNames), Uppercase(Client.GivenNames)) = 1) );
end;

procedure TContractCreationWizardForm.SetData(const Value: TContract);
begin
  FData := Value;
  if FData <> nil then
  begin
    Client := FData.Client;
    ContractItemsListView.Data := FData.Items.List;
  end
  else
  begin
    Client := nil;
    ContractItemsListView.Data := nil;
  end;
  RefreshContractControls;
end;

{ TContractNewItemForm - events - pages }

procedure TContractCreationWizardForm.SelectClientPageExitPage(Sender: TObject;
  const Page: TKWizardCustomPage);
begin
  if Data.Client = nil then
  begin
    MessageDlg('Please select a client before proceeding', mtWarning, [mbOK], 0);
    Abort;
  end;
end;

procedure TContractCreationWizardForm.FinishPageFinishButtonClick(
  Sender: TObject; var Stop: Boolean);
begin
  Assert(Assigned(Data));
  Assert(Assigned(Data.Client));

  Stop := True;

  Data.Notes := SummaryContractNotesRichEdit.Text;

  Pawnbroker.ClientIdentityRecordTypes.Save;
  Pawnbroker.ContractItemCategories.Save;
  Pawnbroker.Manufacturers.Save;
  Data.Client.Save;
  if Data.ContractState = csNew then
    DoActivateContract
  else
    DoUpdateContract;
{ TODO -oTT -cReview :
All dialog interactions should go through some central controller.}
  if PrintDialog.Execute then
  begin
    with TContractPrintDataModule.Create(nil) do
    begin
      try
        Data := Self.Data;
        Print(PrintDialog.Copies);
      finally
        Free;
      end;
    end;
  end;

  Close;
{ TODO -oTT -cRequired : Need to automatically prompt for printing of contract at this point }
end;

procedure TContractCreationWizardForm.ClientIdentityRecordListBoxCheck(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
var
  Checked: Boolean;
  IdentityRecord: TClientIdentityRecord;
  IdentityRecordProxy: TClientIdentityRecord;
begin
  IdentityRecord := pData as TClientIdentityRecord;
  IdentityRecordProxy := Data.ClientIdentityRecordProxies.Find(IdentityRecord.OID.AsString);
  Checked := Assigned(IdentityRecordProxy) and (not IdentityRecordProxy.Deleted);

  case Checked of
    False:
      begin
        if Assigned(IdentityRecordProxy) then
        begin
          IdentityRecordProxy.Deleted := False
        end
        else
        begin
          IdentityRecordProxy := TClientIdentityRecordProxy.Create(IdentityRecord);
          IdentityRecordProxy.ObjectState := posCreate;
          Data.ClientIdentityRecordProxies.Add(IdentityRecordProxy);
        end;
      end;

    True:
      begin
        if IdentityRecordProxy.ObjectState = posCreate then
        begin
          Data.ClientIdentityRecordProxies.Remove(IdentityRecordProxy);
        end;
      end;
  end;
  Data.ClientIdentityRecordProxies.Dirty := True;
end;

procedure TContractCreationWizardForm.ClientIdentityRecordListBoxGetChecked(
  pData: TPersistent; var pbChecked: Boolean);
var
  IdentityRecord: TClientIdentityRecord;
  IdentityRecordProxy: TClientIdentityRecord;
begin
  Assert(Data.Client <> nil);
  IdentityRecord := pData as TClientIdentityRecord;
  IdentityRecordProxy := Data.ClientIdentityRecordProxies.Find(IdentityRecord.OID.AsString);
  pbChecked := Assigned(IdentityRecordProxy) and (not IdentityRecordProxy.Deleted);
end;

procedure TContractCreationWizardForm.ClientIdentificationListBoxListColumns0DeriveColumn(
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

procedure TContractCreationWizardForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  case CaseObject(Action,

    [ AddClientIdentityRecordAction,
      ReplaceClientIdentityRecordAction,
      DeleteClientIdentityRecordAction,

      AddContractItemAction,
      ReplaceContractItemAction,
      DeleteContractItemAction ]) of

    0 : AddClientIdentityRecordAction.Enabled :=
          Assigned(Data) and
          (IdentityRecordTypeComboBox.Text <> '') and
          (IdentityRecordDetailsEdit.Text <> '');

    1 : ReplaceClientIdentityRecordAction.Enabled :=
          Assigned(Data) and
          Assigned(ClientIdentityRecordListBox.SelectedData) and
          (((ClientIdentityRecordListBox.SelectedData) as TClientIdentityRecord).InUse = False) and
          (IdentityRecordTypeComboBox.Text <> '') and
          (IdentityRecordDetailsEdit.Text <> '');

    2 : DeleteClientIdentityRecordAction.Enabled :=
          Assigned(Data) and
          Assigned(SelectedClientIdentityRecord) and
          (SelectedClientIdentityRecord.InUse = False);

    3 : AddContractItemAction.Enabled :=
          Assigned(Data);

    4 : ReplaceContractItemAction.Enabled :=
          Assigned(Data) and
          Assigned(ContractItemsListView.SelectedData);

    5 : DeleteContractItemAction.Enabled :=
          Assigned(Data) and
          Assigned(SelectedContractItem);
  end;

  case CaseObject(Wizard.ActivePage, [SelectClientPage, PawnedItemsPage]) of
    0: { SelectClientPage }
      begin
        if (Data <> nil) and (Data.Client <> nil) and
          (ClientNameEdit.Text <> CLIENT_NAME_TEMPLATE) and (ClientNameEdit.Text <> '') then
        begin
          SelectClientPage.EnabledButtons := SelectClientPage.EnabledButtons + [bkNext];
        end
        else
        begin
          SelectClientPage.EnabledButtons := SelectClientPage.EnabledButtons - [bkNext];
        end;
      end;

    1: { PawnedItemsPage }
      begin
        if (Data <> nil) and (Data.Items.Count > 0) and
          (ClientNameEdit.Text <> CLIENT_NAME_TEMPLATE) and (ClientNameEdit.Text <> '') then
        begin
          PawnedItemsPage.EnabledButtons := PawnedItemsPage.EnabledButtons + [bkNext];
        end
        else
        begin
          PawnedItemsPage.EnabledButtons := PawnedItemsPage.EnabledButtons - [bkNext];
        end;
      end;
    
  end;
end;

procedure TContractCreationWizardForm.FinishPageEnterPage(Sender: TObject;
  const Page: TKWizardCustomPage);
begin
  Assert(Assigned(Data));
  Assert(Assigned(Data.Client));

  Data.Client := GetClient; // refresh the client object, sync from user interface

  SummaryContractNumberEdit.Text := Data.ContractNumberAsString;
  SummaryContractPeriodEdit.Text := Format('%s to %s',
    [FormatDateTime('mmmm-d-yy', Data.StartDate),
     FormatDateTime('mmmm-d-yy', Data.EndDate)]);
  SummaryStatusEdit.Text := Data.ContractStateAsString;

  SummaryClientNumberEdit.Text := IntToStr(Data.Client.ClientNumber);
  SummaryClientNameEdit.Text   := FormatClientName(Data.Client);

  Data.ClientAddress := GetClientAddress;
  if Assigned(Data.ClientAddress) then
  begin
    SummaryStreetEdit.Text   := Data.ClientAddress.Street;
    SummarySuburbEdit.Text   := Data.ClientAddress.Suburb;
    SummaryStateEdit.Text    := Data.ClientAddress.State;
    SummaryPostCodeEdit.Text := Data.ClientAddress.PostCode;
  end
  else
  begin
    SummaryStreetEdit.Clear;
    SummarySuburbEdit.Clear;
    SummaryStateEdit.Clear;
    SummaryPostCodeEdit.Clear;
  end;

  SummaryPhoneHomeEdit.Text    := Data.Client.PhoneHome;
  SummaryPhoneMobileEdit.Text  := Data.Client.PhoneMobile;
  SummaryPhoneWorkEdit.Text    := Data.Client.PhoneWork;
  SummaryEmailAddressEdit.Text := Data.Client.EmailAddress;


  SummaryClientIdentityRecordListView.Data := Data.ClientIdentityRecordProxies.List;
  SummaryClientIdentityRecordListView.Refresh;

  SummaryContractItemsListView.Data := Data.Items.List;
  SummaryContractItemsListView.Refresh;

  SubtotalNumericField.AsFloat    := FData.Items.SubTotal;
  ContractFeeNumericField.AsFloat := FData.ContractFee;
  InterestRateNumericField.AsFloat := FData.InterestRate;
  InterestValueNumericField.AsFloat := FData.InterestValue;
  RedemptionValueNumericField.AsFloat := FData.RedemptionValue;

  SummaryContractNotesRichEdit.Clear;
  SummaryContractNotesRichEdit.Text := Data.Notes;

  SummaryClientIdentityRecordListView.LV.Color := FinishPage.Color;
  SummaryContractItemsListView.LV.Color := FinishPage.Color;
end;

procedure TContractCreationWizardForm.ContractItemsListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  Assert(pData is TContractItem);
  if (pData as TContractItem).Category <> nil then
    pResult := (pData as TContractItem).Category.Name
  else
    pResult := '<none>';
end;

procedure TContractCreationWizardForm.ContractItemsListViewListColumns5DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  Assert(pData is TContractItem);
  if (pData as TContractItem).Manufacturer <> nil then
    pResult := (pData as TContractItem).Manufacturer.Name
  else
    pResult := '<none>';
end;

procedure TContractCreationWizardForm.AfterConstruction;
begin
  inherited;
  FEventListenerList := TEventListenerList.Create;
  ContractItemsListView.LV.TabStop := False;
{ TODO -oTT -cRequired : Need to have code here to copy record into edit controls }
//  ContractItemsListView.LV.OnDblClick := EditClientIdentityRecordActionExecute;

  // Setup SelectClientPage

  ClientFinderVisible := False;
  Client := nil;

  // Setup ClientIdentityPage
  ClientIdentityRecordListBox.LV.OnDblClick := HandleClientIdentityRecordListBoxDblClick;
  SelectedClientIdentityRecord := nil;

  // Setup PawnedItemsPage
  ValueEdit.OnKeyPress := TEditKeyPressUtility.OnEditKeyPress_DisallowNonNumerics;
  QuantityEdit.OnKeyPress := TEditKeyPressUtility.OnEditKeyPress_DisallowNonIntegers;
  SelectedContractItem := nil;
end;

procedure TContractCreationWizardForm.BeforeDestruction;
begin
  FEventListenerList.Free;
  inherited;
end;

procedure TContractCreationWizardForm.RemoveListener(
  Listener: IContractCreationWizardListener);
begin
  FEventListenerList.Remove(Listener);
end;

function TContractCreationWizardForm.GetClient: TClient;
begin
{ TODO -oTT -cRefactor :
Really should decouple the selected client from the contract until the very
last minute, using the GetClient method to update the client with values
set in the user interface }
  Result := nil;
  if Assigned(Data) then
  begin
    Result := Data.Client;
    if Assigned(Result) then
    begin
      Data.Client.PhoneHome    := PhoneHomeEdit.Text;
      Data.Client.PhoneMobile  := PhoneMobileEdit.Text;
      Data.Client.PhoneWork    := PhoneWorkEdit.Text;
      Data.Client.EmailAddress := EmailAddressEdit.Text;
      Data.Client.Undesirable  := UndesirableCheckBox.Checked;
      Data.Client.UndesirableCode := UndesirableCodeEdit.Text;
      Data.Client.UndesirableReason := UndesirableCodeEdit.Text;
      Data.Client.Dirty := True;
    end;
  end;
end;

function TContractCreationWizardForm.GetClientAddress: TClientAddress;
var
  Counter: Integer;
  Address: TClientAddress;
begin
  Result := nil;

  for Counter := Data.Client.Addresses.Count-1 downto 0 do
  begin
    Address := Data.Client.Addresses.Items[Counter];
    if (AnsiCompareText(Address.Street,   StreetEdit.Text) = 0) and
       (AnsiCompareText(Address.Suburb,   SuburbEdit.Text) = 0) and
       (AnsiCompareText(Address.State,    StateEdit.Text) = 0) and
       (AnsiCompareText(Address.PostCode, PostCodeEdit.Text) = 0) then
    begin
      Result := Address;
      Break;
    end;
  end;

  if not Assigned(Result) then
  begin
{ TODO -oTT -cRequired : Should put a test in here to make sure no-one adds a blank address }
    Address := TClientAddress.CreateNew;
    Address.Street := StreetEdit.Text;
    Address.Suburb := SuburbEdit.Text;
    Address.State  := StateEdit.Text;
    Address.PostCode := PostCodeEdit.Text;
    Data.Client.Addresses.Add(Address);
    Data.Client.Addresses.Dirty := True;
    Result := Address;
  end;
end;

function TContractCreationWizardForm.GetClientFinderVisible: Boolean;
begin
  Result := ClientFinderListView.Visible;
end;



procedure TContractCreationWizardForm.RefreshContractControls;
begin
  if Assigned(Data) then
  begin
    ContractNumberEdit.Text := Format('%d', [FData.ContractNumber]);
    if FData.ExtensionNumber > 0 then
      ContractNumberEdit.Text := ContractNumberEdit.Text + ' E' + IntToStr(FData.ExtensionNumber);
    ContractPeriodEdit.Text := Format('%s to %s',
      [FormatDateTime('mmmm-d-yy', FData.StartDate),
      FormatDateTime('mmmm-d-yy', FData.EndDate)]);
    StatusEdit.Text := ContractStateToStr(FData.ContractState);
  end
  else
  begin
    ContractNumberEdit.Text := '';
    ContractPeriodEdit.Text := '';
    StatusEdit.Text         := '';
  end;
end;

procedure TContractCreationWizardForm.SetClientFinderVisible(
  const Value: Boolean);
begin
  case Value of
    False:
    begin
      ClientFinderListView.Visible := False;
      ClientFinderListView.ApplyFilter := False;
    end;

    True:
    begin
      ClientFinderListView.Left   := ClientNameEdit.Left;
      ClientFinderListView.Top    := ClientNameEdit.Top + ClientNameEdit.Height;
      ClientFinderListView.Width  := ClientNameEdit.Width;
      ClientFinderListView.Height := GetClientFinderHeight;

      ClientFinderListView.OnFilterData := HandleClientFinderListViewFilterData_PartialMatch;

      ClientFinderListView.Data := PawnBroker.Clients.List;
      ClientFinderListView.Visible := True;
      ClientFinderListView.ApplyFilter := True;
      ClientFinderListView.Refresh;
      if ClientFinderListView.LV.Items.Count > 0 then
        ClientFinderListView.SelectedData := TObject(ClientFinderListView.LV.Items[0].Data) as TClient;
      ClientFinderListView.LV.SetFocus;
//      ClientFinderListView.LV
    end;
  end;
end;

procedure TContractCreationWizardForm.SetClient(const Value: TClient);

  procedure DeleteClientIdentityRecordProxies;
  var
    Counter: Integer;
  begin
    Assert(Data <> nil);

    for Counter := Pred(Data.ClientIdentityRecordProxies.Count) downto 0 do
    begin
      Data.ClientIdentityRecordProxies.Items[Counter].Deleted := True;
    end;
    Data.ClientIdentityRecordProxies.Clear;
  end;

  procedure RefreshClientControls;
  begin
    if Assigned(Value) then
    begin
      ClientNumberEdit.Text := Format('%d', [Value.ClientNumber]);

      if (Value.FamilyName <> '') and (Value.GivenNames <> '') then
      begin
        ClientNameEdit.Text := Format('%s, %s', [Value.FamilyName, Value.GivenNames])
      end
      else
      begin
        ClientNameEdit.Text := '';
        if Value.FamilyName <> '' then
          ClientNameEdit.Text := Value.FamilyName
        else if Value.GivenNames <> '' then
          ClientNameEdit.Text := Value.GivenNames;
      end;

      PhoneHomeEdit.Text := Value.PhoneHome;
      PhoneMobileEdit.Text := Value.PhoneMobile;
      PhoneWorkEdit.Text := Value.PhoneWork;
      EmailAddressEdit.Text := Value.EmailAddress;

      UndesirableCheckBox.Checked := Value.Undesirable;
      UndesirableCodeEdit.Text := Value.UndesirableCode;
      UndesirableRichEdit.Text := Value.UndesirableReason;
    end
    else
    begin
      ClientNumberEdit.Text := '';
      ClientNameEdit.Text := '';
      PhoneHomeEdit.Text := '';
      PhoneMobileEdit.Text := '';
      PhoneWorkEdit.Text := '';
      EmailAddressEdit.Text := '';

      UndesirableCodeEdit.Text := '';
      UndesirableRichEdit.Lines.Clear;
    end;

    UndesirableNoticeLabel.Visible := Assigned(Client) and Client.Undesirable;
  end;

begin
{ TODO -oTT -cReview : This works, but is a bit messy. Tidy it up Tuddenham }


  if Assigned(Data) then
  begin

    if Assigned(Value) and Value.Undesirable and (Data.Client <> Value) and 
     (MessageDlg(
        FormatClientName(
          Value) + ' is marked as UNDESIRABLE' + #13 +
          'Are you sure you want to select this client?',
           mtWarning, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Abort;
    end;

    if Data.Client <> Value then // New client selected
    begin
      DeleteClientIdentityRecordProxies;
    end;

    if (Data.Client <> nil) and (Data.Client.ObjectState = posCreate) then
    begin
      DeleteClientIdentityRecordProxies;
      Data.Client.Free;
      Data.Client := nil;
    end;

    if Value <> nil then
    begin
      Assert(Data <> nil);

      if Value.ObjectState = posPK then
        Value.Read;
      Data.Client := Value;

      ClientAddress := Data.Client.CurrentAddress;

      ClientIdentityRecordListBox.Data := Data.Client.IdentityRecords.List;
    end
    else
    begin
      if Data <> nil then
        Data.Client := nil;

      ClientAddress := nil;

      ClientIdentityRecordListBox.Data := nil;
    end;
  end;
  
  RefreshClientControls;
end;

procedure TContractCreationWizardForm.SetClientAddress(
  const Value: TClientAddress);
begin
  StreetEdit.Clear;
  SuburbEdit.Clear;
  StateEdit.Clear;
  PostCodeEdit.Clear;

  if Value <> nil then
  begin
    Assert(Data <> nil);
    Assert(Data.Client <> nil);

    StreetEdit.Text := Value.Street;
    SuburbEdit.Text := Value.Suburb;
    StateEdit.Text := Value.State;
    PostCodeEdit.Text := Value.PostCode;
  end;
end;

procedure TContractCreationWizardForm.SetSelectedClientIdentityRecord(
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

{ TContractCreationWizardForm - events }

procedure TContractCreationWizardForm.FormShow(Sender: TObject);
begin
  ClientNameEdit.SetFocus;
end;

{ TContractCreationWizardForm - events - actions }

procedure TContractCreationWizardForm.AddClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
  IdentityRecordProxy: TClientIdentityRecordProxy;
begin
  Assert(Data <> nil);
  Assert(Data.Client <> nil);
  Assert(IdentityRecordTypeComboBox.Text <> '');
  Assert(IdentityRecordDetailsEdit.Text <> '');

  IdentityRecord := TClientIdentityRecord.CreateNew;
  IdentityRecord.IdentityRecordType :=
    FindOrCreateClientIdentityRecordTypeByName(IdentityRecordTypeComboBox.Text);
  IdentityRecord.Details := IdentityRecordDetailsEdit.Text;
  Data.Client.IdentityRecords.Add(IdentityRecord);
  Data.Client.IdentityRecords.Dirty := True;

  IdentityRecordProxy := TClientIdentityRecordProxy.Create(IdentityRecord);
  IdentityRecordProxy.ObjectState := posCreate;

  Data.ClientIdentityRecordProxies.Add(IdentityRecordProxy);
  Data.ClientIdentityRecordProxies.Dirty := True;

  ClientIdentityRecordListBox.Refresh;

  // Return focus to identity record controls
  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;
end;

procedure TContractCreationWizardForm.AddContractItemActionExecute(
  Sender: TObject);
var
  ContractItem: TContractItem;
begin
  Assert(Data <> nil);

  ContractItem := TContractItem.CreateNew;

  ContractItem.Category     := FindOrCreateContractItemCategoryByName(CategoryComboBox.Text);
  ContractItem.Quantity     := StrToIntDef(QuantityEdit.Text, 0);
  ContractItem.Description  := DescriptionEdit.Text;
  ContractItem.Manufacturer := FindOrCreateManufacturerByName(ManufacturerComboBox.Text);
  ContractItem.ModelNumber  := ModelNumberEdit.Text;
  ContractItem.Notes        := ContractItemNotesRichEdit.Text;
  ContractItem.SerialNumber := SerialNumberEdit.Text;

  if ValueEdit.Text <> '' then
    ContractItem.Value      := StrToFloat(ValueEdit.Text)
  else
    ContractItem.Value      := 0;

  Data.Items.Add(ContractItem);
  Data.Items.Dirty := True;

  ContractItemsListView.Refresh;

  // Return focus to contract item edit controls
  SelectedContractItem := nil;
  CategoryComboBox.SetFocus;
end;


procedure TContractCreationWizardForm.DeleteClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
begin
  Assert(Data <> nil);
  Assert(Data.Client <> nil);
  Assert(Assigned(SelectedClientIdentityRecord));

  if SelectedClientIdentityRecord.InUse = True then
    raise Exception.Create('You cannot delete a client identity record that has been used elsewhere');

  SelectedClientIdentityRecord.Deleted := True;
  Data.Client.IdentityRecords.Dirty := True;
  Data.ClientIdentityRecordProxies.Dirty := True;

  ClientIdentityRecordListBox.Refresh;

  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;
end;

procedure TContractCreationWizardForm.DeleteContractItemActionExecute(
  Sender: TObject);
var
  ContractItem: TContractItem;
begin
  Assert(Data <> nil);
  Assert(Assigned(SelectedContractItem));

  if SelectedContractItem.ObjectState = posCreate then
  begin
    Data.Items.Remove(SelectedContractItem);
  end
  else
  begin
    SelectedContractItem.Deleted := True;
  end;

  Data.Items.Dirty := True;
  ContractItemsListView.Refresh;

  // Return focus to contract item edit controls
  SelectedContractItem := nil;
  CategoryComboBox.SetFocus;  
end;

procedure TContractCreationWizardForm.ReplaceClientIdentityRecordActionExecute(
  Sender: TObject);
var
  IdentityRecord: TClientIdentityRecord;
begin
  Assert(Assigned(Data));
  Assert(Assigned(Data.Client));
  Assert(Assigned(ClientIdentityRecordListBox.SelectedData));
  Assert(IdentityRecordTypeComboBox.Text <> '');
  Assert(IdentityRecordDetailsEdit.Text <> '');

  IdentityRecord :=
    ClientIdentityRecordListBox.SelectedData as TClientIdentityRecord;

  if IdentityRecord.InUse = True then
    raise Exception.Create('You cannot replace a client identity record that has been used elsewhere');

  IdentityRecord.IdentityRecordType :=
    FindOrCreateClientIdentityRecordTypeByName(IdentityRecordTypeComboBox.Text);
  IdentityRecord.Details := IdentityRecordDetailsEdit.Text;
  
  Data.Client.IdentityRecords.Dirty := True;
  Data.ClientIdentityRecordProxies.Dirty := True;

  ClientIdentityRecordListBox.Refresh;

  // Return focus to identity record controls
  SelectedClientIdentityRecord := nil;
  IdentityRecordTypeComboBox.SetFocus;
end;

procedure TContractCreationWizardForm.ReplaceContractItemActionExecute(
  Sender: TObject);
var
  ContractItem: TContractItem;
begin
  Assert(Assigned(Data));
  Assert(Assigned(ContractItemsListView.SelectedData));

  ContractItem := ContractItemsListView.SelectedData as TContractItem;

  ContractItem.Category     := FindOrCreateContractItemCategoryByName(CategoryComboBox.Text);
  ContractItem.Quantity     := StrToIntDef(QuantityEdit.Text, 0);
  ContractItem.Description  := DescriptionEdit.Text;
  ContractItem.Manufacturer := FindOrCreateManufacturerByName(ManufacturerComboBox.Text);
  ContractItem.ModelNumber  := ModelNumberEdit.Text;
  ContractItem.Notes        := ContractItemNotesRichEdit.Text;
  ContractItem.SerialNumber := SerialNumberEdit.Text;
  ContractItem.Dirty        := True;

  if ValueEdit.Text <> '' then
    ContractItem.Value      := StrToFloat(ValueEdit.Text)
  else
    ContractItem.Value      := 0;

  Data.Items.Dirty := True;

  ContractItemsListView.Refresh;

  // Return focus to contract item edit controls
  SelectedContractItem := nil;
  CategoryComboBox.SetFocus;
end;

{ TContractCreationWizardForm - events - SelectClientPage }

{ TContractCreationWizardForm - events - ClientIdentityPage }

procedure TContractCreationWizardForm.ClientNameEditExit(Sender: TObject);
var
  ClientGivenNames: string;
  ClientFamilyName: string;
  ClientNames: TStringList;
  TempClient: TClient;
begin
{ TODO -oTT -cDevNote :
Need to check if client entered is exact match.. if it is then just
automatically select that client - otherwise show the drop down box with
all partial matches }
  if Pos(',', ClientNameEdit.Text) <> 0 then
  begin
    ClientFamilyName := Trim(StrLeft(ClientNameEdit.Text, Pos(',', ClientNameEdit.Text)-1));
    ClientGivenNames := Trim(StrRestOf(ClientNameEdit.Text, Pos(',', ClientNameEdit.Text)+1));
  end
  else
  begin
    if StrLastPos(AnsiSpace, ClientNameEdit.Text) <> 0 then
    begin
      ClientFamilyName := Trim(StrRestOf(ClientNameEdit.Text, StrLastPos(AnsiSpace, ClientNameEdit.Text)+1));
      ClientGivenNames := Trim(StrLeft(ClientNameEdit.Text, StrLastPos(AnsiSpace, ClientNameEdit.Text)-1));
    end
    else
    begin
      // if there is only one name in the edit control, assume it is a family name
      ClientFamilyName := Trim(ClientNameEdit.Text);
      ClientGivenNames := '';
    end;
  end;

  if (ClientFamilyName <> '') or (ClientGivenNames <> '') then
  begin
    if Assigned(Client) and (Client.ObjectState = posCreate) then
    begin
      Client.FamilyName := ClientFamilyName;
      Client.GivenNames := ClientGivenNames;
{ TODO -oTT -cSMELL : Refresh the client controls. Need an explicit mapping of controls to object }
      ClientNameEdit.Text := FormatClientName(Client);
    end
    else
    begin
      TempClient := FindClientByName(ClientFamilyName, ClientGivenNames);
      if not Assigned(TempClient) then
      begin
        FClientFinder_FamilyName := ClientFamilyName;
        FClientFinder_GivenNames := ClientGivenNames;
        ClientFinderVisible := True;
      end
      else
      begin
        Client := TempClient;
      end;
    end;
  end;  
end;

procedure TContractCreationWizardForm.ClientFinderListViewDblClick(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  Client := pData as TClient;
  ClientFinderVisible := False;
  StreetEdit.SetFocus;
end;

procedure TContractCreationWizardForm.NewClientActionExecute(
  Sender: TObject);
begin
  Assert(Data <> nil);
  Client := TClient.CreateNew;
  ClientNameEdit.Text := CLIENT_NAME_TEMPLATE;
  ClientNameEdit.SelectAll;
  ClientNameEdit.SetFocus;
end;

procedure TContractCreationWizardForm.ClientFinderListViewKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Assert(Data <> nil);
    Client := ClientFinderListView.SelectedData as TClient;
    ClientFinderVisible := False;

    StreetEdit.SetFocus;
  end;
end;

procedure TContractCreationWizardForm.FindClientActionExecute(
  Sender: TObject);
begin
  FClientFinder_FamilyName := '';
  FClientFinder_GivenNames := '';
  ClientFinderVisible := True;
end;

procedure TContractCreationWizardForm.IdentityRecordTypeComboBoxDropDown(
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

function TContractCreationWizardForm.FindClientByName(AFamilyName,
  AGivenNames: string): TClient;
begin
  ClientFinderVisible := False;
  FClientFinder_FamilyName := AFamilyName;
  FClientFinder_GivenNames := AGivenNames;

  ClientFinderListView.OnFilterData := HandleClientFinderListViewFilterData_ExactMatch;

  ClientFinderListView.Data := PawnBroker.Clients.List;
  ClientFinderListView.ApplyFilter := True;

  ClientFinderListView.Height := 0;
  try
    ClientFinderListView.Refresh;
  finally
    ClientFinderListView.Visible := False;
    ClientFinderListView.Height := GetClientFinderHeight;
    ClientFinderListView.LV.Align := alClient;
  end;


  if ClientFinderListView.Items.Count = 1 then
  begin
    Result := TObject(ClientFinderListView.Items[0].Data) as TClient;
  end
  else
  begin
    Result := nil;
  end;
  ClientFinderListView.ApplyFilter := False;
end;

function TContractCreationWizardForm.FindOrCreateClientIdentityRecordTypeByName(
  AName: string): TClientIdentityRecordType;
begin
{ TODO -oPBPRO -cSMELL :
The code that was here has been moved. Code that calls this can now probably just call the
PawnBroker.ClientIdentityRecordTypes list directly }
  Result := PawnBroker.ClientIdentityRecordTypes.FindOrCreateByName(AName);
end;

function TContractCreationWizardForm.FindOrCreateContractItemCategoryByName(
  AName: string): TContractItemCategory;
begin
  Result := nil;

  if AName <> '' then
  begin
    Result := PawnBroker.ContractItemCategories.FindByProps(['Name'], [AName], False);
    if Result = nil then
    begin
      Result := TContractItemCategory.CreateNew;
      Result.Name := AName;
      PawnBroker.ContractItemCategories.Add(Result);
      PawnBroker.ContractItemCategories.Dirty := True;
{ TODO -oTT -cDevNote : Defer saving until the contract is finalised }
//    Result.Save;
    end
  end;
end;

function TContractCreationWizardForm.FindOrCreateManufacturerByName(
  AName: string): TManufacturer;
begin
  Result := nil;

  if (AName <> '') then
  begin
    Result := PawnBroker.Manufacturers.FindByProps(['Name'], [AName], False);

    if Result = nil then
    begin
      Result := TManufacturer.CreateNew;
      Result.Name := AName;
      PawnBroker.Manufacturers.Add(Result);
      PawnBroker.Manufacturers.Dirty := True;
{ TODO -oTT -cDevNote : Defer saving until the contract is finalised }
  //    Result.Save;
    end;
  end;
end;

function TContractCreationWizardForm.FormatClientName(
  AClient: TClient): string;
begin
  Assert(AClient <> nil);
  Result := '';
  if AClient.FamilyName <> '' then
  begin
    Result := AClient.FamilyName;
  end;

  if AClient.GivenNames <> '' then
  begin
    if Result = '' then
      Result := AClient.GivenNames
    else
      Result := Result + ', ' + AClient.GivenNames;
  end;
end;


procedure TContractCreationWizardForm.WizardFinishButtonClick(
  Sender: TObject);
begin
  ShowMessage('Not implemented yet');
{ TODO -oTT -cRequired : Post all changes to database }
  Close;
end;

procedure TContractCreationWizardForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{ TODO -oTT -cReview : Not keen on wizard knowing about main form }
  MainForm.TreeViewAdapter.CurrentChildForm := MainForm.TreeViewAdapter.BackStack.Pop;
//  if MainForm.TreeViewAdapter.CurrentChildForm = Self then
//    MainForm.TreeViewAdapter.CurrentChildForm := nil;
{ TODO -oTT -cReview : 
Honestly, this should just pop the last viewed form off the stack and make 
it enabled. }    
  Action := caFree;

end;

procedure TContractCreationWizardForm.ClientIdentityRecordListBoxFilterData(
  pData: TPersistent; var pbInclude: Boolean);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  ClientIdentityRecord := pData as TClientIdentityRecord;
  pbInclude := not ClientIdentityRecord.Deleted;
end;

procedure TContractCreationWizardForm.ManufacturerComboBoxDropDown(
  Sender: TObject);
var
  Counter: Integer;
  Manufacturer: TManufacturer;
begin
  ManufacturerComboBox.Clear;
  for Counter := 0 to PawnBroker.Manufacturers.Count-1 do
  begin
    Manufacturer := PawnBroker.Manufacturers.Items[Counter];
    ManufacturerComboBox.Items.Add(Manufacturer.Name);
  end;
end;

procedure TContractCreationWizardForm.CategoryComboBoxDropDown(
  Sender: TObject);
var
  Category: TContractItemCategory;
  Counter: Integer;
begin
  CategoryComboBox.Clear;
  for Counter := 0 to PawnBroker.ContractItemCategories.Count-1 do
  begin
    Category := PawnBroker.ContractItemCategories.Items[Counter];
    CategoryComboBox.Items.Add(Category.Name);
  end;
end;

procedure TContractCreationWizardForm.ContractItemsListViewDblClick(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  SelectedContractItem := pData as TContractItem;
end;


procedure TContractCreationWizardForm.ContractItemsListViewFilterData(
  pData: TPersistent; var pbInclude: Boolean);
var
  ContractItem: TContractItem;
begin
  ContractItem := pData as TContractItem;
  pbInclude := not (ContractItem.ObjectState = posDelete);
end;

procedure TContractCreationWizardForm.WizardCancelButtonClick(
  Sender: TObject);
begin
  if MessageDlg('Cancel this contract?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Assert(Assigned(Data));
    if Data.ObjectState = posCreate then
    begin
      Data.ClientIdentityRecordProxies.Clear;
      Data.Client := nil;
      Data.Free;
    end
    else
    begin
      { TODO -oPBPPRO -cDEFECT : 
Code here to send message to contract form that contract has been 
invalidated (will need to re-read the contract in prior to closing). }
    end;
    Close;
  end;  
end;

procedure TContractCreationWizardForm.ClientFinderListViewExit(
  Sender: TObject);
begin
  ClientFinderVisible := False;
end;

function TContractCreationWizardForm.GetClientFinderHeight: Integer;
begin
  Result := Trunc((SelectClientPage.ClientHeight - (ClientNameEdit.Top + ClientNameEdit.Height)) * 0.75);
end;

procedure TContractCreationWizardForm.Execute(Contract: TContract = nil);
begin
  if Contract = nil then
  begin
    Contract := TContract.CreateNew;
    gTiPerMgr.VisMgr.Execute('read.defaultValues', Contract);
    Contract.ObjectState := posCreate;
    Data := Contract;
    MainForm.TreeViewAdapter.CurrentChildForm := Self;
  end
  else
  begin
    Data := Contract;
    MainForm.TreeViewAdapter.CurrentChildForm := Self;
  end;
end;

procedure TContractCreationWizardForm.FinishPageExitPage(Sender: TObject;
  const Page: TKWizardCustomPage);
begin
  Assert(Assigned(Data));
  Data.Notes := SummaryContractNotesRichEdit.Text;
end;

procedure TContractCreationWizardForm.SummaryClientIdentityRecordListViewFilterData(
  pData: TPersistent; var pbInclude: Boolean);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  ClientIdentityRecord := pData as TClientIdentityRecord;
  pbInclude := not ClientIdentityRecord.Deleted;
end;






end.
