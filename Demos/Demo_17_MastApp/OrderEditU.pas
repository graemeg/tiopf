unit OrderEditU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, tiObject, tiMemoReadOnly, MastApp_BOM, Grids, DBGrids, DB,
  tiDataset;

type
  TfrmOrder = class(TFormTIPerEditDialog)
    Panel1: TPanel;
    memoErrors: TtiMemoReadOnly;
    editTaxRate: TtiPerAwareFloatEdit;
    editItemsTotal: TtiPerAwareFloatEdit;
    comboCustomer: TtiPerAwareComboBoxDynamic;
    editOID: TtiPerAwareEdit;
    editCustAddress1: TtiPerAwareEdit;
    editCustAddress2: TtiPerAwareEdit;
    editCustCity: TtiPerAwareEdit;
    editCustState: TtiPerAwareEdit;
    editCustZip: TtiPerAwareEdit;
    editShipToContact: TtiPerAwareEdit;
    editShipToAddress1: TtiPerAwareEdit;
    editShipToAddress2: TtiPerAwareEdit;
    editShipToCity: TtiPerAwareEdit;
    editShipToState: TtiPerAwareEdit;
    editShipToZip: TtiPerAwareEdit;
    comboEmployee: TtiPerAwareComboBoxDynamic;
    editPurchaseOrder: TtiPerAwareEdit;
    editTaxDue: TtiPerAwareFloatEdit;
    editFreight: TtiPerAwareFloatEdit;
    editAmountPaid: TtiPerAwareFloatEdit;
    editAmountDue: TtiPerAwareFloatEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    editCustNo: TtiPerAwareEdit;
    comboTerms: TtiPerAwareComboBoxStatic;
    comboPayment: TtiPerAwareComboBoxStatic;
    comboShipping: TtiPerAwareComboBoxStatic;
    dscItems: TDataSource;
    gridItems: TDBGrid;
    editSaleDate: TtiPerAwareDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure comboCustomerChange(Sender: TObject);
    procedure comboEmployeeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FDataset_CalcFields(DataSet: TDataSet);
    procedure gridItemsEditButtonClick(Sender: TObject);
    procedure FDataset_AfterPost(DataSet: TDataSet);
    procedure editTaxRateExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCustomer: TCustomer;
    FItemsDataset: TTiDataset;
    procedure SetCustomer(const Value: TCustomer);
    function  Order: TOrder;
    procedure CopyCustomerDetails;
    procedure SetupControls;
    procedure PickPartNumber;
    procedure CalculateTotals;
    procedure RefreshTotals;
  protected
    procedure SetData(const Value: TtiObject); override ;
    function  FormIsValid : boolean ; override ;
    property  Customer: TCustomer read FCustomer write SetCustomer;
  public
    { Public declarations }
  end;

var
  frmOrder: TfrmOrder;

implementation

uses modSharedU, PartLookupU;

{$R *.dfm}


procedure TfrmOrder.comboCustomerChange(Sender: TObject);
begin
  inherited;
  Customer:= TCustomer(comboCustomer.Value) ;
  Order.CustNo := Customer.oid;
  CopyCustomerDetails;
  Databuffer.Dirty:= true;
end;

procedure TfrmOrder.comboEmployeeChange(Sender: TObject);
begin
  inherited;
  Order.EmpNo := TEmployee(comboEmployee.Value).OID ;
  Databuffer.Dirty:= true;
end;

procedure TfrmOrder.CopyCustomerDetails;
begin
  editShipToContact.Value:= Customer.Contact;
  editShipToAddress1.Value:= Customer.Address1;
  editShipToAddress2.Value:= Customer.Address2;
  editShipToCity.Value:= Customer.City;
  editShipToState.Value:= Customer.State;
  editShipToZip.Value:= Customer.Zip;
  
  Order.ShipToCountry:= Customer.Country; // doesn't have a edit box, very strange
end;

procedure TfrmOrder.editTaxRateExit(Sender: TObject);
begin
  inherited;
  RefreshTotals;
end;

procedure TfrmOrder.gridItemsEditButtonClick(Sender: TObject);
begin
  inherited;
  PickPartNumber;
end;

procedure TfrmOrder.FDataset_AfterPost(DataSet: TDataSet);
begin
  inherited;
  CalculateTotals;
end;

procedure TfrmOrder.FDataset_CalcFields(DataSet: TDataSet);
begin
  inherited;
  FItemsDataset.FieldByName('Description').AsString:= TOrderItem(FItemsDataset.GetActiveItem).Description;
  FItemsDataset.FieldByName('ListPrice').AsCurrency:= TOrderItem(FItemsDataset.GetActiveItem).ListPrice;
  FItemsDataset.FieldByName('TotalPrice').AsCurrency:= TOrderItem(FItemsDataset.GetActiveItem).TotalPrice;
end;

procedure TfrmOrder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FItemsDataset.CheckBrowseMode;
end;

procedure TfrmOrder.FormCreate(Sender: TObject);
begin
  inherited;
  SetupControls;
end;

procedure TfrmOrder.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FCustomer);
end;

function TfrmOrder.FormIsValid: boolean;
begin
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TfrmOrder.RefreshTotals;
begin
  editItemsTotal.Refresh;
  editTaxRate.Refresh;
  editTaxDue.Refresh;
  editAmountPaid.Refresh;
  editAmountDue.Refresh;
end;

procedure TfrmOrder.CalculateTotals;
begin
  Order.CalculateTotals;
  RefreshTotals;
end;

procedure TfrmOrder.PickPartNumber;
var partNo: integer;
begin
  partNo:= FItemsDataset.FieldByName('PartNo').AsInteger;
  if TfrmPartLookup.Execute(partNo) then
  begin
    FItemsDataset.Edit;
    FItemsDataset.FieldByName('PartNo').AsInteger:= partNo;
  end;

end;

procedure TfrmOrder.SetupControls;
  procedure SetupField(AField: TField; AFieldName: string; ASize: integer; ACalculated: boolean);
  begin
//    FDataset_.Fields.Add(AField);
    AField.FieldName:= AFieldName;
    AField.Size:= ASize;
    AField.Calculated:= ACalculated;
    AField.DataSet:= FItemsDataset;   // VITAL
  end;
  procedure SetupCombo(ACombo: TtiPerAwareComboBoxStatic; AEnumStrings: array of string);
  begin
    TComboBox(ACombo).Sorted:= true; // this doesn't work!
    AddEnumStrings(ACombo.items, AEnumStrings);
  end;
begin
  // create this at runtime so that TTiDataset doesn't need to be installed.
  FItemsDataset:= TTiDataset.Create(self);
  FItemsDataset.AfterPost := FDataset_AfterPost;
  FItemsDataset.AfterDelete := FDataset_AfterPost;
  FItemsDataset.OnCalcFields := FDataset_CalcFields;

  SetupField(TFloatField.Create(FItemsDataset), 'PartNo', 0, false);
  TFloatField(FItemsDataset.FieldByName('PartNo')).DisplayFormat := '#,##0';

  SetupField(TStringField.Create(FItemsDataset), 'Description', 30, true);
  SetupField(TIntegerField.Create(FItemsDataset), 'Quantity', 0, false);
  SetupField(TFloatField.Create(FItemsDataset), 'Discount', 0, false);
  SetupField(TCurrencyField.Create(FItemsDataset), 'ListPrice', 0, true);
  SetupField(TCurrencyField.Create(FItemsDataset), 'TotalPrice', 0, true);

  dscItems.DataSet := FItemsDataset;

  FCustomer := TCustomer.Create;
  comboCustomer.List := modShared.Customers.List;
  comboEmployee.List := modShared.Employees.List;
  SetupCombo(comboTerms, cTerms);
  SetupCombo(comboPayment, cPayment);
  SetupCombo(comboShipping, cShipping);
end;

function TfrmOrder.Order: TOrder;
begin
  result:= TOrder(DataBuffer);
end;

procedure TfrmOrder.SetCustomer(const Value: TCustomer);
begin
  if assigned(Value) then
  begin
    FCustomer.Assign(Value);
    if Value <> comboCustomer.Value then
      comboCustomer.Value:= Value;

    editCustNo.Value:= Customer.OID.AsString;

    editCustAddress1.Value:= FCustomer.Address1;
    editCustAddress2.Value:= FCustomer.Address2;
    editCustCity.Value:=     FCustomer.City;
    editCustState.Value:=    FCustomer.State;
    editCustZip.Value:=      FCustomer.Zip;
  end;
end;

procedure TfrmOrder.SetData(const Value: TtiObject);
begin
  inherited;

  editOID.Value := DataBuffer.OID.AsString ;
  Customer:= TCustomer(modShared.Customers.Find(TOrder(Value).CustNo));

  comboEmployee.Value:= modShared.Employees.Find(Order.EmpNo);

  editSaleDate.LinkToData(DataBuffer, 'SaleDate');
  editShipToContact.LinkToData(DataBuffer, 'ShipToContact');
  editShipToAddress1.LinkToData(DataBuffer, 'ShipToAddress1');
  editShipToAddress2.LinkToData(DataBuffer, 'ShipToAddress2');
  editShipToCity.LinkToData(DataBuffer, 'ShipToCity');
  editShipToState.LinkToData(DataBuffer, 'ShipToState');
  editShipToZip.LinkToData(DataBuffer, 'ShipToZip');
//  editShipToCountry.LinkToData(DataBuffer, 'ShipToCountry');  // not implimented in original MastApp
//  editShipToPhone.LinkToData(DataBuffer, 'ShipToPhone');
  editPurchaseOrder.LinkToData(DataBuffer, 'PurchaseOrder');
  editItemsTotal.LinkToData(DataBuffer, 'ItemsTotal');
  editTaxRate.LinkToData(DataBuffer, 'TaxRate');
  editTaxDue.LinkToData(DataBuffer, 'TaxDue');

  editFreight.LinkToData(DataBuffer, 'Freight');
  editAmountPaid.LinkToData(DataBuffer, 'AmountPaid');
  editAmountDue.LinkToData(DataBuffer, 'AmountDue');

  comboTerms.LinkToData(DataBuffer, 'TermsString');
  comboPayment.LinkToData(DataBuffer, 'PaymentMethodString');
  comboShipping.LinkToData(DataBuffer, 'ShipViaString');

  Order.OrderItems.Read;

  FItemsDataset.LinkObject(Order.OrderItems, TOrderItem);
  FItemsDataset.Open;


end;

end.
