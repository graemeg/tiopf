unit CustomerListU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseListU, ActnList, StdCtrls, ExtCtrls, tiFocusPanel, tiVTListView,
  Buttons, tiObject, MastApp_BOM, tiVirtualTrees;

type
  TfrmCustomerList = class(TfrmBaseList)
    lvOrders: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVItemArrive(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemLeave(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure lvOrdersItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure lvOrdersItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure lvOrdersItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
  private
    FOrders: TOrders;
    procedure RefreshOrderList(AData: TtiObject);
  protected
     procedure SetColumns; override;
    function CreateNewItem: TtiObject; override;
    function EditItem(AItem: TtiObject): boolean; override;
  public
    { Public declarations }
  end;

var
  frmCustomerList: TfrmCustomerList;

implementation

uses CustomerEditU, OrderEditU, modSharedU, tiGUIUtils;

{$R *.dfm}

{ TfrmCustomerList }

function TfrmCustomerList.CreateNewItem: TtiObject;
begin
  result:= TCustomer.CreateNew;
end;

function TfrmCustomerList.EditItem(AItem: TtiObject): boolean;
begin
  Result:= TfrmCustomer.Execute(AItem);
end;

procedure TfrmCustomerList.RefreshOrderList(AData: TtiObject);
begin
  lvOrders.Data := nil;
  FOrders.Clear;
  FOrders.Criteria.ClearAll;
  
  // note, we need to convert back to an integer as we are using Integer OID
  FOrders.Criteria.AddEqualTo('CustNo', StrToInt( AData.OID.AsString));
  FOrders.Read;
  lvOrders.Data := FOrders;
end;

procedure TfrmCustomerList.FormCreate(Sender: TObject);
begin
  inherited;
  FOrders:= TOrders.create;
end;

procedure TfrmCustomerList.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FOrders);
end;

procedure TfrmCustomerList.LVItemArrive(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  RefreshOrderList(AData);
end;

procedure TfrmCustomerList.LVItemLeave(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  lvOrders.Data := nil;
end;

procedure TfrmCustomerList.lvOrdersItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if tiPerObjAbsConfirmAndDelete(AData) then
    lvOrders.Refresh;
end;

procedure TfrmCustomerList.lvOrdersItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  if TfrmOrder.Execute(AData) then
  begin
    LV.SelectedData.Dirty:= true;
    lvOrders.Refresh;
  end;
end;

procedure TfrmCustomerList.lvOrdersItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if assigned(lvOrders.Data) then
  begin
    modShared.CreateNewOrder(LV.SelectedData.OID);
    RefreshOrderList(LV.SelectedData);
  end;
end;

procedure TfrmCustomerList.SetColumns;
begin
  inherited;
  LV.AddColumn('CustNo', vttkString, 'Cust No', 75);
  LV.AddColumn('Company', vttkString, 'Company', 150);
  LV.AddColumn('Phone', vttkString, 'Phone', 100);
  LV.AddColumn('LastInvoiceDate', vttkDate, 'Last Invoice', 75);

  lvOrders.AddColumn('OrderNo', vttkInt, 'Order No', 75);
  lvOrders.AddColumn('SaleDate', vttkDate, 'Sale Date', 75);
//  lvOrders.AddColumn('ShipDate', vttkDate, 'Ship Date', 75);
  lvOrders.AddColumn('AmountPaid', vttkCurrency, 'AmountPaid', 75);
  lvOrders.AddColumn('AmountDue', vttkCurrency, 'AmountDue', 75);
end;

end.
