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
unit pbpContractForm;

interface

uses
  Windows, ActiveX, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Mask, DBCtrls, Db, IBCustomDataSet, IBQuery, Comctrls, IBTable, ExtCtrls, StdCtrls,
  ImgList, ActnList, Menus, DBLookup, Buttons,

  TB2Item, TB2Dock, TB2Toolbar,

  ovcbase, ovcef, ovcpb, ovcnf,

  jclStrings,

  tiListView, tiPerAwareCtrls, tiReadOnly, tiPtnVisPerObj, tiPersist, tiListViewCtrls,
  tiListViewPlus,

  pbpUtils, pbpTypes,

  pbpBusinessClasses, pbpContractPartPaymentForm, pbpContractAdditionalAmountForm,
  pbpContractCreationWizardForm, pbpResources;


type
  TContractEditFormMode = (cefmUnknown, cefmContract, cefmContracts);

  TContractForm = class(TForm, IUnknown, ISupportsToolbar2000, IPerObjAbsListener)
    ToolbarActionList: TActionList;
    NewContractAction: TAction;
    PrintAction: TAction;
    PawnedItemsPanel: TPanel;
    ContractItemsListView: TtiListView;
    SummaryPanel: TPanel;
    Label15: TLabel;
    Label16: TLabel;
    Label19: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    NotesRichEdit: TRichEdit;
    Label2: TLabel;
    ContractDetailsPanel: TPanel;
    TransactionListView: TtiListView;
    StatusEdit: TEdit;
    ContractPeriodEdit: TEdit;
    Label6: TLabel;
    Label4: TLabel;
    ContractNumberEdit: TEdit;
    ClientDetailsPanel: TPanel;
    ClientNumberEdit: TEdit;
    ClientNameEdit: TEdit;
    Name: TLabel;
    Label1: TLabel;
    Label9: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    PhoneMobileEdit: TEdit;
    PhoneWorkEdit: TEdit;
    PhoneHomeEdit: TEdit;
    SubtotalPanel: TPanel;
    Label25: TLabel;
    Label26: TLabel;
    Label7: TLabel;
    EmailAddressEdit: TEdit;
    ExtendContractAction: TAction;
    Label8: TLabel;
    RedeemContractAction: TAction;
    ExpireContractAction: TAction;
    PartPaymentAction: TAction;
    AdditionalAmountAction: TAction;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label5: TLabel;
    TBDock: TTBDock;
    ContractFormToolbar: TTBToolbar;
    TBItem5: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    PrintTBItem: TTBItem;
    PrintPreviewTBItem: TTBItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBItem10: TTBItem;
    TBItem11: TTBItem;
    TBItem12: TTBItem;
    TBItem13: TTBItem;
    TBItem14: TTBItem;
    PrintPreviewAction: TAction;
    ExitAction: TAction;
    TBItem1: TTBItem;
    EditAction: TAction;
    ControlsActionList: TActionList;
    ToggleMoreLessContractDetailsAction: TAction;
    ToggleMoreLessClientDetailsAction: TAction;
    StreetEdit: TEdit;
    SuburbEdit: TEdit;
    StateEdit: TEdit;
    PostCodeEdit: TEdit;
    Label14: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    ToggleMoreLessClientDetailsButton: TSpeedButton;
    ToggleIsShowingMoreButton: TSpeedButton;
    IdentityRecordAddAction: TAction;
    IdentityRecordReplaceAction: TAction;
    IdentityRecordDeleteAction: TAction;
    ClientIdentityRecordListView: TtiListView;
    PrintDialog: TPrintDialog;
    ContractFeeNumericField: TOvcNumericField;
    InterestRateNumericField: TOvcNumericField;
    InterestValueNumericField: TOvcNumericField;
    RedemptionValueNumericField: TOvcNumericField;
    LoanValueNumericField: TOvcNumericField;
    CurrentPaymentsNumericField: TOvcNumericField;
    SubtotalNumericField: TOvcNumericField;
    PreviousPaymentsNumericField: TOvcNumericField;
    procedure ToggleMoreLessContractDetailsActionExecute(Sender: TObject);
    procedure ToolbarActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ToggleMoreLessClientDetailsActionExecute(Sender: TObject);
    procedure PawnedItemsPanelResize(Sender: TObject);
    procedure ExtendContractActionExecute(Sender: TObject);
    procedure TransactionListViewListColumns4DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ContractItemsListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ContractItemsListViewListColumns6DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure RedeemContractActionExecute(Sender: TObject);
    procedure ExpireContractActionExecute(Sender: TObject);
    procedure PartPaymentActionExecute(Sender: TObject);
    procedure AdditionalAmountActionExecute(Sender: TObject);
    procedure ClientIdentificationListBoxGetChecked(pData: TPersistent;
      var pbChecked: Boolean);
    procedure ClientIdentificationListBoxListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ClientIdentificationListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure NewContractActionExecute(Sender: TObject);
    procedure CommitChangesActionExecute(Sender: TObject);
    procedure PrintActionExecute(Sender: TObject);
    procedure PrintPreviewActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure ClientIdentityRecordListViewListColumns0DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure EditActionExecute(Sender: TObject);
  private
    FData: TContract;
    FTreeNode: TTreeNode;
    FEditing: Boolean;
    FClientFinder_GivenNames: string;
    FClientFinder_FamilyName: string;
    procedure ClientAddressControls_Refresh;
    function GetClientDetailsIsShowingMore: Boolean;
    function GetContractDetailsIsShowingMore: Boolean;
    function  GetData : TContract;
    function  GetValid: boolean;
    procedure ObjectControlsRefresh;
    procedure SetContractDetailsIsShowingMore(const Value: Boolean);
    procedure SetData(const Value: TContract);
    procedure SetClientAddress(const Value: TClientAddress);
    procedure SetClientDetailsIsShowingMore(const Value: Boolean);
    procedure UpdateCaption;
  protected
    procedure DoActivateContract;
    procedure DoAdditionalAmount;
    procedure DoVerbExit;
    procedure DoExpireContract;
    procedure DoExtendContract;
    procedure DoPartPayment;
    procedure DoSaveChanges;
    procedure DoRedeemContact;
    procedure DoVerbNewItem;
    procedure DoVerbPrint;
    procedure DoVerbPreview;
    function GetVerbState(Index: Integer): Integer;
    procedure PerObjAbsListener_Changed(Sender: TObject);
    procedure IPerObjAbsListener.Changed = PerObjAbsListener_Changed;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetToolbarParent(Parent: TWinControl);
  published
    // These published properties are required by the TtiTreeViewPlus
    property Data : TContract read GetData write SetData ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid    : boolean   read GetValid ;
  end;

var
  ContractForm: TContractForm;

implementation

uses pbpContractPrint, pbpMainForm;

{$R *.DFM}

const
  // TransactionMessageFormat = '[%transaction type, message, till change, instructions]'
  TransactionMessageFormat = '[%s]' + #13 + #13 +
                             '%s' + #13 + #13 +
                             'Change to till: %m' + #13 + #13 +
                             '%s';


  DEFAULT_MARGIN = 10;

{ TContractEditForm }

procedure TContractForm.AfterConstruction;
begin
  inherited;
  // Setup action list
  ToolbarActionList.Images := GetResourceImageList24x24;
  NewContractAction.ImageIndex  := GLYFX_24_NEW;
  EditAction.ImageIndex         := GLYFX_24_EDIT;
  PrintAction.ImageIndex        := GLYFX_24_PRINT;
  PrintPreviewAction.ImageIndex := GLYFX_24_PREVIEW;

{ TODO -oPBPRO -cDEFECT : Need bitmaps for contract actions (extend, redeem, etc) }
  ExtendContractAction.ImageIndex   := -1;
  RedeemContractAction.ImageIndex   := -1;
  PartPaymentAction.ImageIndex      := -1;
  ExpireContractAction.ImageIndex   := -1;
  AdditionalAmountAction.ImageIndex := -1;

  ContractFormToolbar.Images := GetResourceImageList24x24;
  
{ TODO -oTT:2002-07-14 -cReview : Ultimately this should be done via a user configuration object }
  SetContractDetailsIsShowingMore(False);
  SetClientDetailsIsShowingMore(True);

  ContractItemsListView.LV.Color := PawnedItemsPanel.Color;

  UpdateCaption;
end;


function TContractForm.GetData: TContract;
begin
  Result := FData;
end;

function TContractForm.GetClientDetailsIsShowingMore: Boolean;
begin
  Result := ClientDetailsPanel.Height > ToggleMoreLessClientDetailsButton.Top + ToggleMoreLessClientDetailsButton.Height + BOTTOM_MARGIN;
end;

function TContractForm.GetValid: boolean;
begin
  if FData.InheritsFrom(TContractList) then
    Result := True
  else
  { TODO -oTT -cRequired : Need to put test code for contract's state. Might use a visitor, hey :) }
    Result := True;
end;

procedure TContractForm.SetClientDetailsIsShowingMore(const Value: Boolean);
begin
  case Value of
    False:
      begin
        ClientDetailsPanel.Height := ToggleMoreLessClientDetailsButton.Top + ToggleMoreLessClientDetailsButton.Height + BOTTOM_MARGIN;
      end;
    True:
      begin
        ClientDetailsPanel.Height := ClientIdentityRecordListView.Top + ClientIdentityRecordListView.Height + BOTTOM_MARGIN;
      end;
  end;
end;

procedure TContractForm.SetData(const Value: TContract);
begin
  if Assigned(FData) then
  begin
    FData.RemoveListener(Self);
  end;

  FData := Value;

  if FData <> nil then
  begin

    if (FData.Client <> nil) and (FData.Client.ObjectState = posPK) then
      FData.Client.Read;

    if FData.ObjectState = posPK then
      FData.Read;

    UpdateCaption;
    FData.AddListener(Self);
  end;

  ObjectControlsRefresh;
end;

procedure TContractForm.DoVerbNewItem;
var
  WizardForm: TContractCreationWizardForm;
begin
  MainForm.TreeViewAdapter.BackStack.Push(Self);

  WizardForm := TContractCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;

procedure TContractForm.DoVerbPreview;
begin
  Assert(Self.Data <> nil);
  Assert(Self.Data.Dirty <> True);

  with TContractPrintDataModule.Create(nil) do
  begin
    try
      Data := Self.Data;
      Preview;
    finally
      Free;
    end;
  end;
end;

procedure TContractForm.DoVerbPrint;
begin
  Assert(Self.Data <> nil);
  Assert(Self.Data.Dirty <> True);

  
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
end;

function TContractForm.GetContractDetailsIsShowingMore: Boolean;
begin
  Result := ContractDetailsPanel.Height > ToggleIsShowingMoreButton.Top + ToggleIsShowingMoreButton.Height + BOTTOM_MARGIN;
end;

procedure TContractForm.SetContractDetailsIsShowingMore(
  const Value: Boolean);
begin
  case Value of
    False : begin
              TransactionListView.Visible := False;
              ContractDetailsPanel.Height := ToggleIsShowingMoreButton.Top + ToggleIsShowingMoreButton.Height + BOTTOM_MARGIN;
              Self.Update;
            end;
    True  : begin
              TransactionListView.Visible := True;
              ContractDetailsPanel.Height := TransactionListView.Top + TransactionListView.Height + BOTTOM_MARGIN;
            end;
  end;
end;



procedure TContractForm.ObjectControlsRefresh;
begin
  if FData <> nil then
  begin
    // Contract header
    ContractNumberEdit.Text := Format('%d', [FData.ContractNumber]);
    if FData.ExtensionNumber > 0 then
      ContractNumberEdit.Text := ContractNumberEdit.Text + ' E' + IntToStr(FData.ExtensionNumber);
    ContractPeriodEdit.Text := Format('%s to %s',
      [FormatDateTime('mmmm-d-yy', FData.StartDate),
      FormatDateTime('mmmm-d-yy', FData.EndDate)]);
    StatusEdit.Text := ContractStateToStr(FData.ContractState);
    TransactionListView.Data := FData.Transactions.List;
    TransactionListView.Refresh;

    // Client details
    if FData.Client = nil then
    begin
      ClientNumberEdit.Text := '';
      ClientNameEdit.Text := '';
      PhoneHomeEdit.Text := '';
      PhoneMobileEdit.Text := '';
      PhoneWorkEdit.Text := '';
      EmailAddressEdit.Text := '';
      ClientIdentityRecordListView.Data := nil;
    end
    else
    begin
      ClientNumberEdit.Text := Format('%d', [FData.Client.ClientNumber]);
      ClientNameEdit.Text := Format('%s, %s', [FData.Client.FamilyName, FData.Client.GivenNames]);

      PhoneHomeEdit.Text := FData.Client.PhoneHome;
      PhoneMobileEdit.Text := FData.Client.PhoneMobile;
      PhoneWorkEdit.Text := FData.Client.PhoneWork;
      EmailAddressEdit.Text := FData.Client.EmailAddress;

      if FData.Client.Undesirable then
      begin
        ClientNumberEdit.Font.Color := clRed;
        ClientNameEdit.Font.Color := clRed;
      end
      else
      begin
        ClientNumberEdit.Font.Color := clBlack;
        ClientNameEdit.Font.Color := clBlack;
      end;
      ClientIdentityRecordListView.Data := FData.ClientIdentityRecordProxies.List;
      ClientIdentityRecordListView.Refresh;
    end;

    // Contract items
    ContractItemsListView.Data := FData.Items.List;
    ContractItemsListView.Refresh;
    SubtotalNumericField.AsFloat := FData.Items.SubTotal;
    PreviousPaymentsNumericField.AsFloat := FData.PreviousPaymentsTotal;

    // Contract summary
    NotesRichEdit.Text := FData.Notes;
    NotesRichEdit.ReadOnly := True;
    NotesRichEdit.Color := SummaryPanel.Color;

    LoanValueNumericField.AsFloat       := FData.LoanValue;
    ContractFeeNumericField.AsFloat     := FData.ContractFee;
    InterestRateNumericField.AsFloat    := FData.InterestRate;
    InterestValueNumericField.AsFloat   := FData.InterestValue;
    CurrentPaymentsNumericField.AsFloat := FData.CurrentPaymentsTotal;
    RedemptionValueNumericField.AsFloat := FData.RedemptionValue;
  end
  else
  begin
    // Contract header
    ContractNumberEdit.Text := '';
    ContractPeriodEdit.Text := '';
    StatusEdit.Text         := '';
    TransactionListView.Data := nil;
    // Client details
    ClientNumberEdit.Text := '';
    ClientNameEdit.Text := '';
    PhoneHomeEdit.Text := '';
    PhoneMobileEdit.Text := '';
    PhoneWorkEdit.Text := '';
    EmailAddressEdit.Text := '';
    ClientIdentityRecordListView.Data := nil;
    // Contract items
    ContractItemsListView.Data := nil;
    SubtotalNumericField.Text := '';
    PreviousPaymentsNumericField.Text := '';
    // Contract summary
    NotesRichEdit.Text := '';
    NotesRichEdit.ReadOnly := True;
    NotesRichEdit.Color := SummaryPanel.Color;
    LoanValueNumericField.Text := '';
    ContractFeeNumericField.Text  := '';
    InterestRateNumericField.Text := '';
    InterestValueNumericField.Text := '';
    CurrentPaymentsNumericField.Text := '';
    RedemptionValueNumericField.Text := '';
  end;

  ClientAddressControls_Refresh;
  // It's necessary to set the color here, otherwise it will change back to the
  // default color each time you move between contracts (behaviour is in
  // TechInsite control).
  ClientIdentityRecordListView.LV.Color :=
    (ClientIdentityRecordListView.Parent as TPanel).Color;
end;

{ TContractForm - events - actions }

procedure TContractForm.ToolbarActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
 case CaseObject(Action,
   [
     NewContractAction,

     PrintAction,
     PrintPreviewAction,

     ExtendContractAction,
     RedeemContractAction,
     ExpireContractAction,
     PartPaymentAction,
     AdditionalAmountAction
     ] ) of

    0  : NewContractAction.Enabled      := True;

    1  : PrintAction.Enabled            := (FData <> nil);
    2  : PrintPreviewAction.Enabled     := (FData <> nil);

    3  : ExtendContractAction.Enabled   := (FData <> nil) and (FData.ContractState = csActive);
    4  : RedeemContractAction.Enabled   := (FData <> nil) and (FData.ContractState = csActive);
    5  : ExpireContractAction.Enabled   := (FData <> nil) and (FData.ContractState = csActive);
    6  : PartPaymentAction.Enabled      := (FData <> nil) and (FData.ContractState = csActive);
    7  : AdditionalAmountAction.Enabled := (FData <> nil) and (FData.ContractState = csActive);
  end;
end;

procedure TContractForm.ToggleMoreLessClientDetailsActionExecute(
  Sender: TObject);
begin
  SetClientDetailsIsShowingMore(not GetClientDetailsIsShowingMore);
  case GetClientDetailsIsShowingMore of
    False : ToggleMoreLessClientDetailsAction.Caption := 'More';
    True  : ToggleMoreLessClientDetailsAction.Caption := 'Less';
  end;
{ TODO -oTT -cComment : 
Explicitly setting the caption of the the toggle button here because it is not
automatically refreshing the caption. }  
  ToggleMoreLessClientDetailsButton.Caption := ToggleMoreLessClientDetailsAction.Caption;
end;

procedure TContractForm.ToggleMoreLessContractDetailsActionExecute(
  Sender: TObject);
begin
  SetContractDetailsIsShowingMore(not GetContractDetailsIsShowingMore);
  case GetContractDetailsIsShowingMore of
    False : ToggleMoreLessContractDetailsAction.Caption := 'More';
    True  : ToggleMoreLessContractDetailsAction.Caption := 'Less';
  end;
end;

procedure TContractForm.PawnedItemsPanelResize(Sender: TObject);
begin
  ContractItemsListView.Top := 8;
  ContractItemsListView.Height := PawnedItemsPanel.Height - SubtotalPanel.Height - 16 ;
  SubtotalPanel.Top := ContractItemsListView.Height +  ContractItemsListView.Top + 2;
end;

procedure TContractForm.DoActivateContract;
begin
  Assert(FData <> nil);
  Assert(FData.ObjectState = posCreate);

  TContractTransactionGateway.CommitTransaction(FData, -FData.LoanValue, '', cttActivate);
  FData.ContractState := csActive;
  FData.Dirty := True;
  FData.Save;
  ObjectControlsRefresh;
end;

procedure TContractForm.DoAdditionalAmount;
var
  MessageStr: string;
  PaymentDetails: string;
  PaymentValue: Currency;
begin
  with TContractAdditionalAmountForm.Create(nil) do
  begin
    try
      if Execute then
      begin
         PaymentDetails := 'Additional amount';
         PaymentValue := - Amount;
         MessageStr := Format(TransactionMessageFormat, [
           'Additional Amount',
           'You are about to add an additional loan amount to an existing contract.' + #13 +
           'This will effect both interest and redemption values for the contract.',
            PaymentValue,
           'Click OK to commit transaction, Cancel to abort.']);
        if MessageDlg(MessageStr, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
        begin
          TContractTransactionGateway.AdditionalAmount(FData, PaymentValue, PaymentDetails);
          ObjectControlsRefresh;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TContractForm.DoVerbExit;
begin
  Close;
end;

procedure TContractForm.DoExpireContract;
var
  MessageStr: string;
  PaymentDetails: string;
begin
  PaymentDetails := 'Contract expired';

  MessageStr := Format(TransactionMessageFormat, [
    'Expire Contract',
    'You are about to expire this contract.',
    0.00,
    'Click OK to commit transaction, Cancel to abort.']);

  if MessageDlg(MessageStr, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
  begin
    TContractTransactionGateway.ExpireContract(FData, PaymentDetails);
    ObjectControlsRefresh;
  end;
end;

procedure TContractForm.DoExtendContract;
var
  MessageStr: string;
  PaymentValue: Currency;
  PaymentDetails: string;
begin
  PaymentValue := FData.InterestValue + PawnBroker.SystemValues.ContractFee;
  PaymentDetails := Format('Extended from E%d to E%d', [
    FData.ExtensionNumber, FData.ExtensionNumber + 1]);

  MessageStr := Format(TransactionMessageFormat, [
    'Extend Contract',
    'You are about to extend this contract.',
    PaymentValue,
    'Click OK to commit transaction, Cancel to abort.']);

  if MessageDlg(MessageStr, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
  begin
    TContractTransactionGateway.ExtendContract(FData, PaymentValue, PaymentDetails);
    ObjectControlsRefresh;
  end;
end;

procedure TContractForm.DoPartPayment;
var
  MessageStr: string;
begin
  with TContractPartPaymentForm.Create(nil) do
  begin
    try
      if Execute then
      begin
        Assert(PaymentValue >= 0, 'Payment is less than zero');
        Assert(PaymentValue <= FData.RedemptionValue,
          'Payment is greater than redemption value');

        MessageStr :=
          '[Part Payment Transaction]' +
           #13 + #13 +
          'You are about to add ' + Format('%m', [PaymentValue]) + ' to the till.' +
           #13;

        if PaymentValue = FData.RedemptionValue then
        begin
          MessageStr := MessageStr + #13 +
            'Warning! This payment value is the same as the contract''s redemption value.' + #13 +
            'If you are redeeming this contract it will not change to the ''Redeemed'' status' +
            'until you explicitly apply the ''Redeem'' action to this contract.'+ #13;
        end;

        MessageStr := MessageStr + #13 +
            'Select ''OK'' to confirm transaction, or ''Cancel'' to abort.';

        if MessageDlg(MessageStr, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
        begin
          TContractTransactionGateway.CommitTransaction(FData, PaymentValue, PaymentDetails, cttPartPayment, PaymentType);
          ObjectControlsRefresh;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TContractForm.DoSaveChanges;
begin
  Assert(FData <> nil);
  Assert(FEditing = True);

  FData.Save;
end;

procedure TContractForm.DoRedeemContact;
var
  MessageStr: string;
  PaymentValue: Currency;
  PaymentDetails: string;
begin
  PaymentValue := FData.RedemptionValue;
  PaymentDetails := 'Contract redeemed';

  MessageStr := Format(TransactionMessageFormat, [
    'Redeem Contract',
    'You are about to redeem this contract.',
    PaymentValue,
    'Click OK to commit transaction, Cancel to abort.']);

  if MessageDlg(MessageStr, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
  begin
    TContractTransactionGateway.RedeemContract(FData, PaymentValue, PaymentDetails);
    ObjectControlsRefresh;
  end;
end;

procedure TContractForm.TransactionListViewListColumns4DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  Assert(pData is TContractTransaction);
  pResult := ContractTransactionTypeToStr((pData as TContractTransaction).TransactionType);
end;

procedure TContractForm.ContractItemsListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  Assert(pData is TContractItem);
  if (pData as TContractItem).Category <> nil then
    pResult := (pData as TContractItem).Category.Name
  else
    pResult := '<none>';
end;

procedure TContractForm.ContractItemsListViewListColumns6DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  Assert(pData is TContractItem);
  if (pData as TContractItem).Manufacturer <> nil then
    pResult := (pData as TContractItem).Manufacturer.Name
  else
    pResult := '<none>';
end;

procedure TContractForm.RedeemContractActionExecute(Sender: TObject);
begin
  DoRedeemContact;
end;

procedure TContractForm.ExpireContractActionExecute(Sender: TObject);
begin
  DoExpireContract;
end;

procedure TContractForm.PartPaymentActionExecute(Sender: TObject);
begin
  DoPartPayment;

//  with TPartPaymentForm.Create(nil) do
//  begin
//    try
//      Data := FData;
//      if Execute then
//      begin
//        FData.Save;
//        FData.Transactions.Save;
//        ObjectControlsRefresh;
//        Assert(FData.Dirty <> True);
//      end;
//    finally
//      Free;
//    end;
//  end;

end;

procedure TContractForm.AdditionalAmountActionExecute(Sender: TObject);
begin
  DoAdditionalAmount;
end;

procedure TContractForm.ClientIdentificationListBoxGetChecked(
  pData: TPersistent; var pbChecked: Boolean);
var
  Data: TClientIdentityRecord;
begin
  Data := pData as TClientIdentityRecord;
  pbChecked := Data.Selected;
end;

procedure TContractForm.ClientIdentificationListBoxListColumns0DeriveColumn(
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

{ TContractForm - events - actions }

procedure TContractForm.ExtendContractActionExecute(Sender: TObject);
begin
  DoExtendContract;
end;

procedure TContractForm.ClientAddressControls_Refresh;
begin
  if (FData <> nil) and (FData.Client <> nil) then
    SetClientAddress(FData.ClientAddress)
  else
    SetClientAddress(nil);
end;

procedure TContractForm.SetClientAddress(const Value: TClientAddress);
begin
  StreetEdit.Clear;
  SuburbEdit.Clear;
  StateEdit.Clear;
  PostCodeEdit.Clear;

  if Value <> nil then
  begin
    StreetEdit.Text := Value.Street;
    SuburbEdit.Text := Value.Suburb;
    StateEdit.Text := Value.State;
    PostCodeEdit.Text := Value.PostCode;
  end;
end;

function TContractForm.GetVerbState(Index: Integer): Integer;
begin

end;

procedure TContractForm.ClientIdentificationListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
var
  ClientIdentityRecord: TClientIdentityRecord;
begin
  Assert(pData is TClientIdentityRecord);
  ClientIdentityRecord := (pData as TClientIdentityRecord);
  if ClientIdentityRecord.IdentityRecordType <> nil then
  begin
    pResult := Format('%s', [ClientIdentityRecord.IdentityRecordType.Name]);
  end
  else
  begin
    pResult := '';
  end;
end;

procedure TContractForm.NewContractActionExecute(Sender: TObject);
begin
  DoVerbNewItem;
end;

procedure TContractForm.CommitChangesActionExecute(
  Sender: TObject);
begin
  DoActivateContract;
end;

procedure TContractForm.PrintActionExecute(Sender: TObject);
begin
  DoVerbPrint;
end;

procedure TContractForm.PrintPreviewActionExecute(Sender: TObject);
begin
  DoVerbPreview;
end;

procedure TContractForm.ExitActionExecute(Sender: TObject);
begin
  DoVerbExit;
end;

procedure TContractForm.UpdateCaption;
begin
  Caption := 'Contract';
  if Data <> nil then
  begin
    Caption := Caption + AnsiSpace + AnsiSpace + Data.ContractNumberAsString;
    if Data.Client <> nil then
    begin
      Caption := Caption + ' : Client ' + Format('%s, %s', [FData.Client.FamilyName, FData.Client.GivenNames]);
    end;
  end;
end;

procedure TContractForm.ClientIdentityRecordListViewListColumns0DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
var
  ClientIdentityRecord: TClientIdentityRecordProxy;
begin
  Assert(FData <> nil);
  ClientIdentityRecord := pData as TClientIdentityRecordProxy;
  if Assigned(ClientIdentityRecord.IdentityRecordType) then
    pResult := ClientIdentityRecord.IdentityRecordType.Caption
  else
    pResult := '<not defined>'
end;

procedure TContractForm.EditActionExecute(Sender: TObject);
var
  WizardForm: TContractCreationWizardForm;
begin
  MainForm.TreeViewAdapter.BackStack.Push(Self);
  WizardForm := TContractCreationWizardForm.Create(Application);
  WizardForm.Execute(Self.Data);
end;

procedure TContractForm.SetToolbarParent(Parent: TWinControl);
begin
  if Parent <> nil then
    ContractFormToolbar.Parent := Parent
  else
    ContractFormToolbar.Parent := TBDock;
end;

procedure TContractForm.PerObjAbsListener_Changed(Sender: TObject);
begin
  ObjectControlsRefresh;
end;

procedure TContractForm.BeforeDestruction;
begin
  Data := nil;
  inherited;
end;

end.
