unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tiPerAwareCtrls, ExtCtrls, tiFocusPanel, tiListView, StdCtrls,
  tiObject,
  ComCtrls,
  BOMUnit,
  RelationshipManagerUnit;

type
  TfrmMainForm = class(TForm)
    lblCustomers: TLabel;
    lblOrders: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnReadCustomers: TButton;
    btnInsertCustomer: TButton;
    btnSaveCustomers: TButton;
    btnInsertOrder: TButton;
    btnDeleteCustomer: TButton;
    btnDeleteOrder: TButton;
    lstCustomers: TListBox;
    lstOrders: TListBox;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnInsertCustomerClick(Sender: TObject);
    procedure btnSaveCustomersClick(Sender: TObject);
    procedure btnReadCustomersClick(Sender: TObject);
    procedure btnInsertOrderClick(Sender: TObject);
    procedure btnDeleteOrderClick(Sender: TObject);
    procedure lstCustomersClick(Sender: TObject);
    procedure btnDeleteCustomerClick(Sender: TObject);
  private
    { Private declarations }
    DBName: string;
    ListStrategy: TListStrategy;
    procedure UpdateCustomersView;
    procedure UpdateOrdersView(ACustomer: TCustomer);
    function GetSelectedCustomer: TCustomer;
    function GetSelectedOrder: TOrder;
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses
  tiOPFManager,
  tiQueryIBX,
  tiOIDGUID,
  tiDialogs,
  Customer_VisitorsUnit,
  Order_VisitorsUnit,
  AssociationObjectVisitorsUnit;

{$R *.dfm}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  RegisterCustomerVisitors;
  RegisterOrderVisitors;
  RegisterAssociationVisitors;

  CustomerList := TCustomerList.Create;
  OrderList := TOrderList.Create;
  ListStrategy := TMyCustomListStrategy.Create;
  GetRelationshipManager.ListStrategy := ListStrategy;
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CustomerList);
  FreeAndNil(OrderList);
  FreeAndNil(ListStrategy);
end;

function TfrmMainForm.GetSelectedCustomer: TCustomer;
begin
  result := nil;
  if lstCustomers.ItemIndex > -1 then
    result := TCustomer(lstCustomers.Items.Objects[lstCustomers.ItemIndex]);
end;

function TfrmMainForm.GetSelectedOrder: TOrder;
begin
  result := nil;
  if lstOrders.ItemIndex > -1 then
    result := TOrder(lstOrders.Items.Objects[lstOrders.ItemIndex]);
end;

procedure TfrmMainForm.lstCustomersClick(Sender: TObject);
var
  Customer: TCustomer;
begin
  Customer := GetSelectedCustomer;
  if Assigned(Customer) then
    UpdateOrdersView(Customer);
end;

procedure TfrmMainForm.btnConnectClick(Sender: TObject);
begin
  //Define Path to your test database here
  //DBName := 'server1:C:\Program Files\Firebird\Firebird_1_5\data\RM_TIOPF.FDB';
  DBName := '';
  Assert(DBName <> '','Path to DB not defined');
  gTIOPFManager.ConnectDatabase(
    DBName,
    'SYSDBA',
    'masterkey',
    '', // Additional params as Name-Value pairs
    'IBX') ;
  tiAppMessage('Database connection pool for "' + DBName + '" loaded.');
end;

procedure TfrmMainForm.btnDisconnectClick(Sender: TObject);
begin
  gTIOPFManager.DisconnectDatabase(
    DBName,
    'IBX') ;
  tiAppMessage('Disconnected from "' + DBName + '"');
end;

procedure TfrmMainForm.btnInsertCustomerClick(Sender: TObject);
var
  tmpCustomer: TCustomer;
begin
  tmpCustomer := TCustomer.CreateNew;
  tmpCustomer.Name := 'CUST'+IntToStr(1+Random(200));
  CustomerList.Add(tmpCustomer);

  UpdateCustomersView;
end;

procedure TfrmMainForm.btnDeleteCustomerClick(Sender: TObject);
var
  Customer: TCustomer;
begin
  Customer := GetSelectedCustomer;
  if Assigned(Customer) then
    begin
      Customer.Deleted := True;

      UpdateCustomersView;
    end;
end;

procedure TfrmMainForm.btnDeleteOrderClick(Sender: TObject);
var
  Order: TOrder;
//  Customer: TCustomer;
begin
  Order := GetSelectedOrder;
  if Assigned(Order) then
    begin
//      Customer := Order.Customer;
//      Customer.RemoveOrder(Order);
//      UpdateOrdersView(Customer);
      Order.Deleted := true;
      UpdateOrdersView(GetSelectedCustomer);
  end;
end;

procedure TfrmMainForm.btnInsertOrderClick(Sender: TObject);
var
  Customer: TCustomer;
  Order: TOrder;
begin
  Customer := GetSelectedCustomer;
  if Assigned(Customer) then
    begin
      Order := TOrder.CreateNew;
      Order.OrderDescription := 'ART'+IntToStr(1+Random(200));
      Order.Quantity := 1+Random(100);
      Customer.AddOrder(Order);
      UpdateOrdersView(Customer);
    end;
end;

procedure TfrmMainForm.btnReadCustomersClick(Sender: TObject);
begin
  CustomerList.Clear;
  CustomerList.Read;
  OrderList.Clear;
  OrderList.Read;
  GetRelationshipManager.Read;

  UpdateCustomersView;
end;

procedure TfrmMainForm.btnSaveCustomersClick(Sender: TObject);
begin
  CustomerList.Save;
  OrderList.Save;
  GetRelationshipManager.Save;
end;

procedure TfrmMainForm.UpdateCustomersView;
var
  tmpCustomer: TCustomer;
  i: Integer;
begin
  lstCustomers.Clear;

  for i := 0 to pred(CustomerList.Count) do
  begin
    if CustomerList[i].Deleted = false then
      begin
        tmpCustomer := CustomerList[i];
        lstCustomers.AddItem(tmpCustomer.OID.AsString + ' - ' +
         tmpCustomer.Name,tmpCustomer);
      end;
  end;

  UpdateOrdersView(GetSelectedCustomer);
end;

procedure TfrmMainForm.UpdateOrdersView(ACustomer: TCustomer);
var
  tmpOrder: TOrder;
  i: Integer;
begin
  lstOrders.Clear;

  if Assigned(ACustomer) then
    begin
      with ACustomer.Orders do
        begin
          for i := 0 to pred(Count) do
          begin
            tmpOrder := Items[i];
            lstOrders.AddItem(tmpOrder.OID.AsString + ' - ' +
             tmpOrder.OrderDescription + ' - Quantity:' + IntToStr(tmpOrder.Quantity),
             tmpOrder);
          end;
        end;
    end;
end;

end.
