unit modSharedU;

interface

uses
  SysUtils, Classes, tiObject, MastApp_BOM, tiOID;

type
  TmodShared = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FCustomers: TCustomers;
    FVendors: TVendors;
    FParts: TParts;
    FEmployees: TEmployees;
//    FOrders: TOrders;
    { Private declarations }
  public
    property Customers: TCustomers read FCustomers;
    property Vendors: TVendors read FVendors;
    property Parts: TParts read FParts;
    property Employees: TEmployees read FEmployees;
//    property Orders: TOrders read FOrders;
    procedure EditCustomers;
    procedure EditVendors;
    procedure EditParts;
    procedure CreateNewOrder(ACustNo: TtiOID = nil);
  end;

var
  modShared: TmodShared;

implementation

{$R *.dfm}

uses
  tiOPFManager
  ,tiConstants
  ,tiGUIUtils
, CustomerEditU, CustomerListU, BaseListU, VendorListU, PartsListU,
  OrderEditU;

procedure TmodShared.CreateNewOrder(ACustNo: TtiOID = nil);
var newOrder: TOrder;
begin
  newOrder := TOrder.CreateNew;
  try
    if Assigned(ACustNo) then
      newOrder.CustNo.Assign(ACustNo);

    if TfrmOrder.Execute(newOrder) then
    begin
      newOrder.Save();
    end;

  finally
    newOrder.Free;
  end; 
end;

procedure TmodShared.DataModuleCreate(Sender: TObject);
begin
  gTIOPFManager.DefaultPersistenceLayerName:= 'ADOAccess';
//old  gTIOPFManager.ConnectDatabase('Data\MastApp.mdb', '', '');
  gTIOPFManager.ConnectDatabase('..\_bin\Data\MastApp.mdb', '', '');

  RegisterAutoMappings;  // auto mapped
//  RegisterVisitors;      // visitors after automapped
//  RegisterLateAutoMappings;


  FCustomers:= TCustomers.Create;
  FVendors:= TVendors.Create;
  FParts:= TParts.create;
  FEmployees:= TEmployees.Create;

  Customers.Read();
  Customers.SortByProps(['Company']);
  Vendors.Read();
  Vendors.SortByProps(['VendorName']);
  Parts.Read;
  Employees.Read;
  Employees.SortByProps(['LastName', 'FirstName']);
end;

procedure TmodShared.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FCustomers);
  FreeAndNil(FVendors);
  FreeAndNil(FParts);
  FreeAndNil(FEmployees);
//  FreeAndNil(FOrders);
end;

procedure TmodShared.EditCustomers;
begin
  TfrmCustomerList.Execute(Customers);
end;

procedure TmodShared.EditParts;
begin
  TfrmPartsList.Execute(Parts);
end;

procedure TmodShared.EditVendors;
begin
  TfrmVendorList.Execute(Vendors);
end;

end.
