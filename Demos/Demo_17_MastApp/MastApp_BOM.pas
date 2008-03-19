unit MastApp_BOM;

interface

uses
   SysUtils,
   tiObject
  ,tiOID
  ,tiQuery
//  ,tiOIDGUID // To force linking GUID OIDs. Must be included in application at least once.
  ,tiOIDInteger
  ,tiQueryADOAccess // peristance layer
  ,tiFilteredObjectList
  , Classes
  ;

type
  TTerms = (termsUnknown, termsPrepaid, termsNet, termsCOD ) ;
  TPayment = (payUnknown, payCash, payCheck, payVisa, payMC, payAmEx, payCredit);
  TShipping = (swhipUnknown, shipUPS, shipUsMail, shipFedEx, shipDHL, shipEmery);

const
  cTerms : array[TTerms] of string = ('', 'Prepaid', 'Net 30', 'COD' );
  cPayment : array[TPayment] of string = ('', 'Cash', 'Check', 'Visa', 'MC', 'AmEx', 'Credit');
  cShipping : array[TShipping] of string = ('', 'UPS', 'US Mail', 'Fed Ex', 'DHL', 'Emery');

type

  TCustomer = class ;
  TCustomers = class ;

  TVendor = class;
  TVendors = class;

  TPart = class;
  TParts = class;

  TEmployee = class;
  TEmployees = class;

  TOrderItem = class;
  TOrderItems = class;

  TOrder = class;
  TOrders = class;

  TMASTObjectList = class(TtiObjectList)
  public
    procedure Read; override;
    procedure Save; override;
  end;

  TMASTFilteredObjectList = class(TtiFilteredObjectList)
  public
    procedure Read; override;
    procedure Save; override;
  end;

  TCustomers = class( TMASTObjectList )
  private
  protected
    function    GetItems(i: integer): TCustomer ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TCustomer); reintroduce ;
  public
    property    Items[i:integer] : TCustomer read GetItems write SetItems ;
    procedure   Add( pObject : TCustomer   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TCustomer = class( TtiObject )
  private
    FCompany: string;
    FAddress1: string;
    FAddress2: string;
    FAddress3: string;
    FLastInvoiceDate: TDateTime;
    FTaxRate: double;
    FFax: string;
    FZip: string;
    FState: string;
    FCountry: string;
    FCity: string;
    FContact: string;
    FPhone: string;
    procedure SetCompany(const Value: string);
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetContact(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetFax(const Value: string);
    procedure SetLastInvoiceDate(const Value: TDateTime);
    procedure SetState(const Value: string);
    procedure SetTaxRate(const Value: double);
    procedure SetZip(const Value: string);
    procedure SetPhone(const Value: string);
    function GetCustNo: integer;
  protected
    function    GetCaption: string; override;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;

    function    GetOwner: TCustomers; reintroduce ;
    procedure   SetOwner(const Value: TCustomers ); reintroduce ;
    property    Owner       : TCustomers             read GetOwner      write SetOwner ;
  published
//    property OID;
    property CustNo: integer read GetCustNo;
    property Company: string read FCompany write SetCompany;
    property Address1: string read FAddress1 write SetAddress1;
    property Address2: string read FAddress2 write SetAddress2;
    property City: string read FCity write SetCity;
    property State: string read FState write SetState;
    property Zip: string read FZip write SetZip;
    property Country: string read FCountry write SetCountry;
    property Phone: string read FPhone write SetPhone;
    property Fax: string read FFax write SetFax;
    property TaxRate: double read FTaxRate write SetTaxRate;
    property Contact: string read FContact write SetContact;
    property LastInvoiceDate: TDateTime read FLastInvoiceDate write SetLastInvoiceDate;
  end ;

// vendors

  TVendors = class( TMASTObjectList )
  private
  protected
    function    GetItems(i: integer): TVendor ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TVendor); reintroduce ;
  public
    property    Items[i:integer] : TVendor read GetItems write SetItems ;
    procedure   Add( pObject : TVendor   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TVendor = class( TtiObject )
  private
    FVendorName: string;
    FPreferred: boolean;
    FFax: string;
    FZip: string;
    FState: string;
    FPhone: string;
    FAddress2: string;
    FCountry: string;
    FAddress1: string;
    FCity: string;
    FParts: TParts;
    procedure SetVendorName(const Value: string);
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetFax(const Value: string);
    procedure SetPhone(const Value: string);
    procedure SetPreferred(const Value: boolean);
    procedure SetState(const Value: string);
    procedure SetZip(const Value: string);
  protected
    function    GetCaption: string; override;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;

    function    GetOwner: TVendors; reintroduce ;
    procedure   SetOwner(const Value: TVendors ); reintroduce ;
    property    Owner       : TVendors             read GetOwner      write SetOwner ;
  published
    property VendorName: string read FVendorName write SetVendorName;
    property Address1: string read FAddress1 write SetAddress1;
    property Address2: string read FAddress2 write SetAddress2;
    property City: string read FCity write SetCity;
    property State: string read FState write SetState;
    property Zip: string read FZip write SetZip;
    property Country: string read FCountry write SetCountry;
    property Phone: string read FPhone write SetPhone;
    property Fax: string read FFax write SetFax;
    property Preferred: boolean read FPreferred write SetPreferred;
    property Parts: TParts read FParts;
  end ;

 TParts = class( TMASTObjectList )
  private
  protected
    function    GetItems(i: integer): TPart ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPart); reintroduce ;
  public
    property    Items[i:integer] : TPart read GetItems write SetItems ;
    procedure   Add( pObject : TPart   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TPart = class( TtiObject )
  private
    FCost: Currency;
    FOnHand: double;
    FListPrice: Currency;
    FDescription: string;
    FOnOrder: double;
    FVendorNo: TtiOID;
    procedure SetCost(const Value: Currency);
    procedure SetDescription(const Value: string);
    procedure SetListPrice(const Value: Currency);
    procedure SetOnHand(const Value: double);
    procedure SetOnOrder(const Value: double);
    procedure SetVendorNo(const Value: TtiOID);
    function GetVendorNo: TtiOID;
    function GetPartNo: integer;

  protected
    function    GetCaption: string; override;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;

    function    GetOwner: TParts; reintroduce ;
    procedure   SetOwner(const Value: TParts ); reintroduce ;
    property    Owner       : TParts             read GetOwner      write SetOwner ;
  published
//    property    OID;
    property    PartNo: integer read GetPartNo;   // resurfaces OID so we can get at it in the lists
    property    VendorNo: TtiOID read GetVendorNo write SetVendorNo;
    property    Description: string read FDescription write SetDescription;
    property    OnHand: double read FOnHand write SetOnHand;  // database is double for no apparant reason.  Int would make more sense
    property    OnOrder: double read FOnOrder write SetOnOrder;
    property    Cost: Currency read FCost write SetCost;
    property    ListPrice: Currency read FListPrice write SetListPrice;
  end ;

  TEmployees = class( TMASTObjectList )
  private
  protected
    function    GetItems(i: integer): TEmployee ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TEmployee); reintroduce ;
  public
    property    Items[i:integer] : TEmployee read GetItems write SetItems ;
    procedure   Add( pObject : TEmployee   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TEmployee = class( TtiObject )
  private
    FLastName: string;
    FHireDate: TDateTime;
    FPhoneExt: string;
    FSalary: Currency;
    FFirstName: string;
   protected
    function    GetCaption: string; override;

  public
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
  published
    property    OID;
    property  LastName: string read FLastName write FLastName;
    property  FirstName: string read FFirstName write FFirstName;
    property  PhoneExt: string read FPhoneExt write FPhoneExt;
    property  HireDate: TDateTime read FHireDate write FHireDate;
    property  Salary: Currency read FSalary write FSalary;
  end ;

  TOrderItems = class( TMASTObjectList )
  private
  protected
    function    GetItems(i: integer): TOrderItem ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TOrderItem); reintroduce ;
    function    GetOID: TtiOID; override;
  public
    property    Items[i:integer] : TOrderItem read GetItems write SetItems ;  default;
    procedure   Add( pObject : TOrderItem   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TOrderItem = class( TtiObject )
  private
    FQuantity: integer;
    FDiscount: double;
    function GetOrderNo: TtiOID;
    procedure SetOrderNo(const Value: TtiOID);
//    function GetPartNo: TtiOID;
//    procedure SetPartNo(const Value: TtiOID);
    function GetDescription: string;
    function GetPart: TPart;
    procedure SetPartNo(const Value: double);
    function GetListPrice: Currency;
    function GetTotalPrice: Currency;
//    procedure SetPartNumberAsString(const Value: string);
//    function GetPartNumberAsString: string;
   protected
     FOrderNo: TtiOID;
//     FPartNo: TtiOID;
     FPartNo: double;
     FPart: Tpart;
    function    GetCaption: string; override;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create ; override ;  
    destructor  Destroy ; override ;
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
    property Part: TPart read GetPart;
    property Description: string read GetDescription;
    property ListPrice: Currency read GetListPrice;
    property TotalPrice: Currency read GetTotalPrice;
  published
//    property    OID;
    property OrderNo: TtiOID read GetOrderNo write SetOrderNo;
    // need the following to provide db access to PartNo

    property PartNo: double read FPartNo write SetPartNo; // read GetPartNo write SetPartNo;
    property Quantity: integer read FQuantity write FQuantity;
    property Discount: double read FDiscount write FDiscount;
  end ;


  TOrders = class( TMASTFilteredObjectList )
  private
//    FWhere: TtiQueryParams;
  protected
    function    GetItems(i: integer): TOrder ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TOrder); reintroduce ;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    property    Items[i:integer] : TOrder read GetItems write SetItems ;
    procedure   Add( pObject : TOrder   ; pDefDispOrdr : boolean = true ) ; reintroduce ;

    // no longer required, as we are now using filtered object lists
//    property    Where: TtiQueryParams read FWhere;
//    property    FilterCustNo: TtiOID read GetFilterCustNo write SetFilterCustNo;  xx
  published
  end ;

  TOrder = class( TtiObject )
  private
    FCustNo: TtiOID;
    FEmpNo: TtiOID;
    FTerms: TTerms;
    FAmountPaid: Currency;
    FTaxRate: Double;
    FShipToPhone: string;
    FShipToAddress2: string;
    FShipToCountry: string;
    FShipToAddress1: string;
    FPaymentMethod: TPayment;
    FShipToCity: string;
    FShipToContact: string;
    FPurchaseOrder: string;
    FShipDate: TDateTime;
    FSaleDate: TDateTime;
    FItemsTotal: Currency;
    FShipVia: TShipping;
    FFreight: Currency;
    FShipToZip: string;
    FShipToState: string;
    FOrderItems: TOrderItems;
    function GetCustNo: TtiOID;
    function GetEmpNo: TtiOID;
    procedure SetCustNo(const Value: TtiOID);
    procedure SetEmpNo(const Value: TtiOID);
    function GetAmountDue: Currency;
    function GetOrderNo: integer;
    function GetTaxDue: Currency;
    function GetPaymentMethodString: string;
    function GetShipViaString: string;
    function GetTermsString: string;
    procedure SetPaymentMethodString(const Value: string);
    procedure SetShipViaString(const Value: string);
    procedure SetTermsString(const Value: string);
    procedure AddItem(AObject: TtiObject);
  protected
    function    GetCaption: string; override;
    procedure   AssignClassProps(ASource: TtiObject); override;    
  public
    function    IsValid( const pErrors : TtiObjectErrors ) : boolean ; override ;
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   CalculateTotals;
    procedure   Save; override;
  published
//     property OID;
     property OrderNo: integer read GetOrderNo;   // resurfaces OID so we can get at it in the lists
     property CustNo: TtiOID read GetCustNo write SetCustNo;
     property SaleDate: TDateTime read FSaleDate write FSaleDate;
     property ShipDate: TDateTime read FShipDate write FShipDate;
     property EmpNo: TtiOID read GetEmpNo write SetEmpNo;
     property ShipToContact: string read FShipToContact write FShipToContact;

     property ShipToAddress1: string read FShipToAddress1 write FShipToAddress1;
     property ShipToAddress2: string read FShipToAddress2 write FShipToAddress2;
     property ShipToCity: string read FShipToCity write FShipToCity;
     property ShipToState: string read FShipToState write FShipToState;
     property ShipToZip: string read FShipToZip write FShipToZip;
     property ShipToCountry: string read FShipToCountry write FShipToCountry;
     property ShipToPhone: string read FShipToPhone write FShipToPhone;
     property PurchaseOrder: string read FPurchaseOrder write FPurchaseOrder;

     property Terms: TTerms read FTerms write FTerms;
     property TermsString: string read GetTermsString write SetTermsString;

     property PaymentMethod: TPayment read FPaymentMethod write FPaymentMethod;
     property PaymentMethodString: string read GetPaymentMethodString write SetPaymentMethodString;

     property ShipVia: TShipping read FShipVia write FShipVia;
     property ShipViaString: string read GetShipViaString write SetShipViaString; 

     property ItemsTotal: Currency read FItemsTotal write FItemsTotal;
     property TaxRate: Double read FTaxRate write FTaxRate;
     property TaxDue: Currency read GetTaxDue;
     property Freight: Currency read FFreight write FFreight;
     property AmountPaid: Currency read FAmountPaid write FAmountPaid;
     property AmountDue: Currency read GetAmountDue;
     property OrderItems: TOrderItems read FOrderItems;
  end ;

procedure RegisterAutoMappings;

procedure AddEnumStrings(AStrings: tstrings; AEnumStrings: array of string);

implementation

uses
  tiOPFManager
  ,tiAutomap
  ;

procedure AddEnumStrings(AStrings: tstrings; AEnumStrings: array of string);
var i: integer;
begin
  AStrings.clear;
  for i := low(AEnumStrings) to high(AEnumStrings) do
    AStrings.Add(AEnumStrings[i]);
end;

function StringToOrd(AValue: string; AEnumStrings: array of string): integer;
var i: integer;
begin
  for i := low(AEnumStrings) to high(AEnumStrings) do
  begin
    if SameText(AValue, AEnumStrings[i]) then
    begin
      result:= i;
      exit;
    end;
  end;

  result:= 0;  // default value
end;

procedure RegisterAutoMappings ;
begin
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'OID', 'CustNo', [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Company', 'Company' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Address1', 'Addr1' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Address2', 'Addr2' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'City', 'City' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'State', 'State' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Zip', 'Zip' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Country', 'Country' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Phone', 'Phone' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'FAX', 'FAX' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'TaxRate', 'TaxRate' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'Contact', 'Contact' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCustomer, 'Customer', 'LastInvoiceDate', 'LastInvoiceDate' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TCustomers, TCustomer);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'OID', 'VendorNo', [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'VendorName', 'VendorName' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'Address1', 'Address1' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'Address2', 'Address2' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'City', 'City' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'State', 'State' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'Zip', 'Zip' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'Country', 'Country' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'Phone', 'Phone' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TVendor, 'Vendors', 'FAX', 'FAX' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TVendors, TVendor);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'OID', 'EmpNo', [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'LastName', 'LastName' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'FirstName', 'FirstName' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'PhoneExt', 'PhoneExt' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'HireDate', 'HireDate' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEmployee, 'Employee', 'Salary', 'Salary' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEmployees, TEmployee);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'OID', 'OrderNo', [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'CustNo', 'CustNo' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'SaleDate', 'SaleDate' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipDate', 'ShipDate' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'EmpNo', 'EmpNo' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToContact', 'ShipToContact' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToAddress1', 'ShipToAddr1' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToAddress2', 'ShipToAddr2' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToCity', 'ShipToCity' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToState', 'ShipToState' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToZip', 'ShipToZip' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToCountry', 'ShipToCountry' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipToPhone', 'ShipToPhone' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ShipVIA', 'ShipVIA' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'PurchaseOrder', 'PO' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'Terms', 'Terms' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'PaymentMethod', 'PaymentMethod' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'ItemsTotal', 'ItemsTotal' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'TaxRate', 'TaxRate' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'Freight', 'Freight' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrder, 'Orders', 'AmountPaid', 'AmountPaid' );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TOrders, TOrder);

  // this gives the write visitors
//  //                                              Class,  Table,    Property,     Column
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'OID',         'PartNo',  [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'VendorNo',   'VendorNo' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'Description', 'Description'   );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'OnHand',      'OnHand' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'OnOrder',     'OnOrder' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'Cost',        'Cost' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPart,  'Parts', 'ListPrice',   'ListPrice' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TParts, TPart);

//                                                Class,      Table,    Property,     Column
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrderItem, 'Items', 'OID', 'ItemNo', [pktDB] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrderItem, 'Items', 'Owner.OID', 'OrderNo', [pktFK] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrderItem, 'Items', 'PartNo', 'PartNo' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrderItem, 'Items', 'Quantity', 'Qty' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TOrderItem, 'Items', 'Discount', 'Discount' ) ;
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TOrderItems, TOrderItem);
end;




{ TCustomer }

procedure TCustomer.AssignClassProps(ASource: TtiObject);
begin
// do nothing here as OID is already assigned
end;

constructor TCustomer.Create;
begin
  inherited;
  FCompany:= '';
  FAddress1:= '';
  FAddress2:= '';
  FAddress3:= '';
  FLastInvoiceDate:= 0;
  FTaxRate:= 0;
  FFax:= '';
  FZip:= '';
  FState:= '';
  FCountry:= '';
  FCity:= '';
  FContact:= '';
  FPhone:= '';
end;

destructor TCustomer.Destroy;
begin
  inherited;
end;

function TCustomer.GetCaption: string;
begin
  result:= Company + ', ' + Contact;
end;

function TCustomer.GetCustNo: integer;
begin
  result:= OID.AsVariant;
end;

function TCustomer.GetOwner: TCustomers;
begin
  result := TCustomers( inherited GetOwner );
end;

function TCustomer.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if Company = '' then
    pErrors.AddError( 'Company', 'Company Name is missing' ) ;

  if Contact = '' then
    pErrors.AddError( 'Contact', 'Contact Name is missing' ) ;

  if (Address1 = '') and (Address2 = '') then
    pErrors.AddError( 'Address', 'Address is missing' ) ;

  if (City = '') then
    pErrors.AddError( 'City', 'City is missing' ) ;

  if (Country = '') then
    pErrors.AddError( 'Country', 'Country is missing' ) ;

  result := pErrors.Count = 0 ;
end;

procedure TCustomer.SetAddress1(const Value: string);
begin
  FAddress1 := Value;
  
end;

procedure TCustomer.SetAddress2(const Value: string);
begin
  FAddress2 := Value;
  
end;

procedure TCustomer.SetCity(const Value: string);
begin
  FCity := Value;
  
end;

procedure TCustomer.SetContact(const Value: string);
begin
  FContact := Value;
  
end;

procedure TCustomer.SetCountry(const Value: string);
begin
  FCountry := Value;
  
end;

procedure TCustomer.SetCompany(const Value: string);
begin
  FCompany := Value;
  
end;

procedure TCustomer.SetFax(const Value: string);
begin
  FFax := Value;
  
end;

procedure TCustomer.SetLastInvoiceDate(const Value: TDateTime);
begin
  FLastInvoiceDate := Value;
  
end;

procedure TCustomer.SetOwner(const Value: TCustomers);
begin
  inherited SetOwner(value);
  
end;

procedure TCustomer.SetPhone(const Value: string);
begin
  FPhone := Value;
  
end;

procedure TCustomer.SetState(const Value: string);
begin
  FState := Value;
  
end;

procedure TCustomer.SetTaxRate(const Value: double);
begin
  FTaxRate := Value;
  
end;

procedure TCustomer.SetZip(const Value: string);
begin
  FZip := Value;
  
end;

{ TCustomers }

procedure TCustomers.Add(pObject: TCustomer; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TCustomers.GetItems(i: integer): TCustomer;
begin
  result := TCustomer( inherited GetItems( i )) ;
end;

procedure TCustomers.SetItems(i: integer; const Value: TCustomer);
begin
  inherited SetItems( i, Value ) ;
end;

{ TVendor }

procedure TVendor.AssignClassProps(ASource: TtiObject);
begin
  FParts.Assign(TVendor(ASource).Parts);
end;

constructor TVendor.Create;
begin
  inherited;
  FParts := TParts.Create;
  FParts.Owner := Self ;
end;

destructor TVendor.Destroy;
begin
  FParts.Free;
  inherited;
end;

function TVendor.GetCaption: string;
begin
  result:= VendorName;
end;

function TVendor.GetOwner: TVendors;
begin
  result := TVendors( inherited GetOwner );
end;

function TVendor.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if VendorName = '' then
    pErrors.AddError( 'VendorName', 'Vendor Name is missing' ) ;

  if (Address1 = '') and (Address2 = '') then
    pErrors.AddError( 'Address', 'Address is missing' ) ;

  if (City = '') then
    pErrors.AddError( 'City', 'City is missing' ) ;

  if (Country = '') then
    pErrors.AddError( 'Country', 'Country is missing' ) ;

  result := pErrors.Count = 0 ;
end;

procedure TVendor.SetAddress1(const Value: string);
begin
  FAddress1 := Value;
end;

procedure TVendor.SetAddress2(const Value: string);
begin
  FAddress2 := Value;
end;

procedure TVendor.SetCity(const Value: string);
begin
  FCity := Value;
end;

procedure TVendor.SetCountry(const Value: string);
begin
  FCountry := Value;
end;

procedure TVendor.SetFax(const Value: string);
begin
  FFax := Value;
end;

procedure TVendor.SetOwner(const Value: TVendors);
begin
  inherited SetOwner(value);
end;


procedure TVendor.SetPhone(const Value: string);
begin
  FPhone := Value;
end;

procedure TVendor.SetPreferred(const Value: boolean);
begin
  FPreferred := Value;
end;

procedure TVendor.SetState(const Value: string);
begin
  FState := Value;
end;

procedure TVendor.SetVendorName(const Value: string);
begin
  FVendorName := Value;  
end;

procedure TVendor.SetZip(const Value: string);
begin
  FZip := Value;
end;

{ TVendors }

procedure TVendors.Add(pObject: TVendor; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TVendors.GetItems(i: integer): TVendor;
begin
  result := TVendor( inherited GetItems( i )) ;
end;

procedure TVendors.SetItems(i: integer; const Value: TVendor);
begin
  inherited SetItems( i, Value ) ;
end;

{ TPart }

procedure TPart.AssignClassProps(ASource: TtiObject);
begin
  VendorNo.Assign(TPart(ASource).VendorNo);
end;

constructor TPart.Create;
begin
  inherited;
  {$IFDEF OID_AS_INT64}
  FVendorNo := cNullOIDInteger;
  {$ENDIF}
end;

destructor TPart.Destroy;
begin
  {$IFNDEF OID_AS_INT64}
    FVendorNo.Free;
  {$ENDIF}

  inherited;
end;

function TPart.GetCaption: string;
begin
  result:=  OID.AsString + ': ' + copy(Description, 1, 20);
end;

function TPart.GetOwner: TParts;
begin
  result:= TParts( inherited Owner);
end;

function TPart.GetPartNo: integer;
begin
  result:= OID.AsVariant;
end;

function TPart.GetVendorNo: TtiOID;
begin
  {$IFNDEF OID_AS_INT64}
    if FVendorNo = nil then
      FVendorNo := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
  {$ENDIF}
  result:= FVendorNo;
end;

function TPart.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if Description = '' then
    pErrors.AddError( 'Description', 'Description is missing' ) ;

  if Onhand < 0 then
    pErrors.AddError( 'Onhand', 'Onhand must be 0 or greater' ) ;

  if OnOrder < 0 then
    pErrors.AddError( 'OnOrder', 'OnOrder must be 0 or greater' ) ;

  if Cost < 0 then
    pErrors.AddError( 'Cost', 'Cost must be 0 or greater' ) ;

  if ListPrice < 0 then
    pErrors.AddError( 'ListPrice', 'ListPrice must be 0 or greater' ) ;

  result := pErrors.Count = 0 ;
end;

procedure TPart.SetCost(const Value: Currency);
begin
  FCost := Value;
  
end;

procedure TPart.SetDescription(const Value: string);
begin
  FDescription := Value;
  
end;

procedure TPart.SetListPrice(const Value: Currency);
begin
  FListPrice := Value;
  
end;

procedure TPart.SetOnHand(const Value: double);
begin
  FOnHand := Value;
  
end;

procedure TPart.SetOnOrder(const Value: double);
begin
  FOnOrder := Value;
  
end;

procedure TPart.SetOwner(const Value: TParts);
begin
  inherited SetOwner(value);
  
end;

procedure TPart.SetVendorNo(const Value: TtiOID);
begin
  FVendorNo.Assign(Value);
end;

{procedure TPart.SetVendorNo(const Value: TtiOID);
begin
  FVendorNo := Value;
end;

 TParts }

procedure TParts.Add(pObject: TPart; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TParts.GetItems(i: integer): TPart;
begin
  result := TPart( inherited GetItems( i )) ;
end;

procedure TParts.SetItems(i: integer; const Value: TPart);
begin
  inherited SetItems( i, Value ) ;
end;

{ TEmployees }

procedure TEmployees.Add(pObject: TEmployee; pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TEmployees.GetItems(i: integer): TEmployee;
begin
  result := TEmployee( inherited GetItems( i )) ;
end;

procedure TEmployees.SetItems(i: integer; const Value: TEmployee);
begin
  inherited SetItems( i, Value ) ;
end;

{ TEmployee }

function TEmployee.GetCaption: string;
begin
  result:= LastName + ', ' + FirstName;
end;

function TEmployee.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if LastName = '' then
    pErrors.AddError( 'LastName', 'LastName is missing' ) ;

  if FirstName = '' then
    pErrors.AddError( 'FirstName', 'FirstName is missing' ) ;

  if HireDate <= 0 then
    pErrors.AddError( 'HireDate', 'HireDate is missing' ) ;

  if Salary <= 0 then
    pErrors.AddError( 'Salary', 'Salary must be greater than 0' ) ;

  result := pErrors.Count = 0 ;
end;

{ TOrderItems }

procedure TOrderItems.Add(pObject: TOrderItem; pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TOrderItems.GetItems(i: integer): TOrderItem;
begin
  result := TOrderItem( inherited GetItems( i )) ;
end;

function TOrderItems.GetOID: TtiOID;
begin
  result:= Owner.OID;
end;

procedure TOrderItems.SetItems(i: integer; const Value: TOrderItem);
begin
  inherited SetItems( i, Value ) ;
end;

{ TOrderItem }

procedure TOrderItem.AssignClassProps(ASource: TtiObject);
begin
  OrderNo.Assign(TOrderItem(ASource).OrderNo);
end;

constructor TOrderItem.Create;
begin
  inherited;
  FQuantity:= 1;
end;

destructor TOrderItem.Destroy;
begin
  FreeAndNil(Fpart);
  inherited;
end;

function TOrderItem.GetCaption: string;
begin
  result:= OID.AsString;
end;

function TOrderItem.GetDescription: string;
begin
  result:= Part.Description;
end;

function TOrderItem.GetListPrice: Currency;
begin
  result:= Part.ListPrice;
end;

function TOrderItem.GetOrderNo: TtiOID;
begin
  {$IFNDEF OID_AS_INT64}
    if FOrderNo = nil then
      FOrderNo := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
  {$ENDIF}
  result:= FOrderNo;
end;

function TOrderItem.GetPart: TPart;
begin
  if not assigned(FPart) then
  begin
    FPart:= TPart.Create;
//    FPart.OID.Assign(PartNo);
  end;
  if FPart.ObjectState = posEmpty then
  begin
    FPart.OID.AsString:= FloatToStr(FPartNo);
    FPart.Read();
  end;

  result:= FPart;
end;

function TOrderItem.GetTotalPrice: Currency;
begin
  Result:= ListPrice * Quantity * (1-Discount / 100);
end;

//function TOrderItem.GetPartNo: TtiOID;
//begin
//  {$IFNDEF OID_AS_INT64}
//    if FPartNo = nil then
//      FPartNo := GTIOPFManager.OIDFactory.CreateOID;
//  {$ENDIF}
//  result:= FPartNo;
//end;

//function TOrderItem.GetPartNumberAsString: string;
//begin
//  result:= PartNo.AsString;
//end;

function TOrderItem.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if OrderNo.IsNull	 then
    pErrors.AddError( 'OrderNo', 'Order No is missing' ) ;

//  if PartNo.IsNull	 then
  if PartNo <= 0 then
    pErrors.AddError( 'PartNo', 'Part No is missing' ) ;

  if Quantity < 0 then
    pErrors.AddError( 'Quantity', 'Quantity must be zero or greater' ) ;

  if Discount < 0 then
    pErrors.AddError( 'Discount', 'Discount must be zero or greater' ) ;

  result := pErrors.Count = 0 ;

end;

procedure TOrderItem.SetOrderNo(const Value: TtiOID);
begin
  OrderNo.Assign(value);
end;

procedure TOrderItem.SetPartNo(const Value: double);
begin
  if FPartNo <> Value then
  begin
    FPartNo := Value;
    if assigned(FPart) then
      FPart.ObjectState:= posEmpty;
  end;
end;

//procedure TOrderItem.SetPartNo(const Value: TtiOID);
//begin
//  FPartNo.Assign(value);
//end;

//procedure TOrderItem.SetPartNumberAsString(const Value: string);
//begin
//  if value <> PartNo.AsString then
//  begin
//    PartNo.AsString:= Value;
//    if assigned(FPart) then
//      FPart.ObjectState:= posEmpty;
//
//  end;
//end;

{ TOrderItems }

procedure TOrders.Add( pObject : TOrder   ; pDefDispOrdr : boolean = true ) ;
begin
  inherited Add(pObject, pDefDispOrdr);
end;

procedure TOrders.AssignClassProps(ASource: TtiObject);
begin

end;

constructor TOrders.Create;
begin
  inherited;
end;

destructor TOrders.Destroy;
begin
  inherited;
end;

function TOrders.GetItems(i: integer): TOrder;
begin
  result := TOrder( inherited GetItems( i )) ;
end;

procedure TOrders.SetItems(i: integer; const Value: TOrder);
begin
  inherited SetItems( i, Value ) ;
end;

{ TOrder }

procedure TOrder.AssignClassProps(ASource: TtiObject);
begin
  CustNo.Assign(TOrder(ASource).CustNo);
  EmpNo.Assign(TOrder(ASource).EmpNo);
  OrderItems.Assign(TOrder(ASource).OrderItems);
end;

procedure TOrder.AddItem(AObject : TtiObject);
begin
  FItemsTotal:= FItemsTotal + TOrderItem(AObject).TotalPrice;
end;

procedure TOrder.CalculateTotals;
begin
  FItemsTotal:= 0;

  OrderItems.ForEach(AddItem);
end;

constructor TOrder.Create;
begin
  inherited;
  FOrderItems:= TOrderItems.create;
  FOrderItems.Owner:= self;
  SaleDate:= Date;
end;

destructor TOrder.Destroy;
begin
  FOrderItems.free;
  inherited;
end;

function TOrder.GetAmountDue: Currency;
begin
  result:= ItemsTotal + Freight + TaxDue - AmountPaid;
end;

function TOrder.GetCaption: string;
begin
  result:= OID.AsString;
end;

function TOrder.GetCustNo: TtiOID;
begin
  {$IFNDEF OID_AS_INT64}
    if FCustNo = nil then
      FCustNo := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
  {$ENDIF}
  result:= FCustNo;
end;

function TOrder.GetEmpNo: TtiOID;
begin
  {$IFNDEF OID_AS_INT64}
    if FEmpNo = nil then
      FEmpNo := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
  {$ENDIF}
  result:= FEmpNo;
end;

function TOrder.GetOrderNo: integer;
begin
  result:= OID.AsVariant;
end;

function TOrder.GetPaymentMethodString: string;
begin
  result:= cPayment[FPaymentMethod]
end;

function TOrder.GetShipViaString: string;
begin
  result:= cShipping[FShipVia];
end;

function TOrder.GetTaxDue: Currency;
begin
  result:= TaxRate / 100 * ItemsTotal
end;

function TOrder.GetTermsString: string;
begin
  Result:= cTerms[FTerms];
end;

function TOrder.IsValid(const pErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid( pErrors ) ;

  if CustNo.IsNull	 then
    pErrors.AddError( 'CustNo', 'Customer No is missing' ) ;

// add others as required

  result := pErrors.Count = 0 ;
end;

procedure TOrder.Save;
begin
  inherited;
end;

procedure TOrder.SetCustNo(const Value: TtiOID);
begin
  CustNo.Assign(value);
end;

procedure TOrder.SetEmpNo(const Value: TtiOID);
begin
  EmpNo.Assign(value);
end;

procedure TOrder.SetPaymentMethodString(const Value: string);
begin
  FPaymentMethod:= tpayment( StringToOrd(Value, cPayment) );
end;

procedure TOrder.SetShipViaString(const Value: string);
begin
  FShipVia:= TShipping( StringToOrd(Value, cShipping));
end;

procedure TOrder.SetTermsString(const Value: string);
begin
  FTerms:= TTerms(StringToOrd(Value, cTerms));
end;

{ TMASTObjectList }

procedure TMASTObjectList.Read;
begin
  inherited;
end;

procedure TMASTObjectList.Save;
begin
  inherited;
end;

{ TMASTFilteredObjectList }

procedure TMASTFilteredObjectList.Read;
begin
  inherited;
end;

procedure TMASTFilteredObjectList.Save;
begin
  inherited;
end;

end.
