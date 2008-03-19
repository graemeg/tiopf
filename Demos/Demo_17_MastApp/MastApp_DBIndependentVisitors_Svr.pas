unit MastApp_DBIndependentVisitors_Svr;

interface

uses
   tiVisitorDBAutoGen, tirtti,
   MastApp_BOM;

type
// using filtered object lists, so none of this is necessary


// customers
//  TVisCustomer_Read = class( TVisDBAutoGenRead )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure Init           ; override ;
//    procedure SetupParams    ; override ;
//    procedure MapRowToObject ; override ;
//    procedure Final          ; override ;
//  end ;
//
//  TVisCustomer_Create = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisCustomer_Update = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisCustomer_Delete = class( TVisDBAutoGenDelete )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//// vendors
//  TVisVendor_Read = class( TVisDBAutoGenRead )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure Init           ; override ;
//    procedure SetupParams    ; override ;
//    procedure MapRowToObject ; override ;
//  end ;
//
//  TVisVendor_Create = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisVendor_Update = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisVendor_Delete = class( TVisDBAutoGenDelete )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;

// Parts
//  TVisPart_Read = class( TVisDBAutoGenRead )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure Init           ; override ;
//    procedure SetupParams    ; override ;
//    procedure MapRowToObject ; override ;
//  end ;

//  TVisPart_Create = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisPart_Update = class( TVisDBAutoGenUpdate )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;
//
//  TVisPart_Delete = class( TVisDBAutoGenDelete )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure SetupParams    ; override ;
//  end ;

//  TVisOrder_Read = class( TVisDBAutoGenRead )
//  protected
//    function  AcceptVisitor : boolean ; override ;
//    procedure Init           ; override ;
//    procedure SetupParams    ; override ;
//    procedure MapRowToObject ; override ;
//  end ;

//procedure RegisterVisitors;

implementation
uses
   tiOPFManager
  ,tiObject
  ,tiQuery
  ,SysUtils
  ;

//procedure RegisterVisitors;
//begin
//
//  // The registration order is important
////  GTIOPFManager.RegReadVisitor(TVisCustomer_Read);
////  GTIOPFManager.RegReadVisitor(TVisVendor_Read);
//  GTIOPFManager.RegReadVisitor(TVisPart_Read);
//  GTIOPFManager.RegReadVisitor(TVisOrder_Read);
//
////  GTIOPFManager.RegSaveVisitor(TVisVendor_Update);
////  GTIOPFManager.RegSaveVisitor(TVisCustomer_Update);
////  GTIOPFManager.RegSaveVisitor(TVisPart_Update);
//
////  GTIOPFManager.RegSaveVisitor(TVisVendor_Delete);
////  GTIOPFManager.RegSaveVisitor(TVisCustomer_Delete);
////  GTIOPFManager.RegSaveVisitor(TVisPart_Delete);
//
////  GTIOPFManager.RegSaveVisitor(TVisCustomer_Create);
////  GTIOPFManager.RegSaveVisitor(TVisVendor_Create);
////  GTIOPFManager.RegSaveVisitor(TVisPart_Delete);
//end;

//{ TVisCustomer_Read }
//
//function TVisCustomer_Read.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TCustomers ) and
//            ( Visited.ObjectState = posEmpty ) ;
//end;
//
//procedure TVisCustomer_Read.Final;
//begin
//  // Do nothing as TVisPhoneNumber_Read will set ObjectState to posClean
//end;
//
//procedure TVisCustomer_Read.Init;
//begin
//  TableName:= 'Customer';
//end;
//
//procedure TVisCustomer_Read.MapRowToObject;
//var
//  LCustomer : TCustomer ;
//begin
//  LCustomer := TCustomer.Create ;
//  LCustomer.OID.AssignFromTIQuery('CustNo',Query);
//  LCustomer.Company := Query.FieldAsString['Company'];
//  LCustomer.Address1 := Query.FieldAsString['Addr1'];
//  LCustomer.Address2 := Query.FieldAsString['Addr2'];
//  LCustomer.City := Query.FieldAsString['City'];
//  LCustomer.State := Query.FieldAsString['State'];
//  LCustomer.Zip := Query.FieldAsString['Zip'];
//  LCustomer.Country := Query.FieldAsString['Country'];
//  LCustomer.Phone := Query.FieldAsString['Phone'];
//  LCustomer.Fax := Query.FieldAsString['Fax'];
//
//  LCustomer.TaxRate := Query.FieldAsFloat['TaxRate'];
//  LCustomer.Contact := Query.FieldAsString['Contact'];
//  LCustomer.LastInvoiceDate := Query.FieldAsDateTime['LastInvoiceDate'];
//
//  LCustomer.ObjectState := posClean ;
//  TCustomers(Visited).Add(LCustomer);
//end;
//
//procedure TVisCustomer_Read.SetupParams;
//begin
//  // Do nothing
//end;
//
//{ TVisCustomer_Create }
//
//function TVisCustomer_Create.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TCustomer ) and
//            ( Visited.ObjectState = posCreate ) ;
//end;
//
//procedure TVisCustomer_Create.SetupParams;
//var
//  LData : TCustomer ;
//begin
//  LData := Visited as TCustomer ;
//  TableName:= 'Customer';
//  QueryType:= qtInsert;
//  QueryParams.SetValueAsString('CustNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('Company', LData.Company);
//  QueryParams.SetValueAsString('Addr1', LData.Address1);
//  QueryParams.SetValueAsString('Addr2', LData.Address2);
//  QueryParams.SetValueAsString('City', LData.City);
//  QueryParams.SetValueAsString('State', LData.State);
//  QueryParams.SetValueAsString('Country', LData.Country);
//  QueryParams.SetValueAsString('Zip', LData.Zip);
//  QueryParams.SetValueAsString('Phone', LData.Phone);
//  QueryParams.SetValueAsString('Fax', LData.Fax);
//  QueryParams.SetValueAsString('Contact', LData.Contact);
//  QueryParams.SetValueAsFloat('TaxRate', LData.TaxRate);
//  QueryParams.SetValueAsDateTime('LastInvoiceDate', LData.LastInvoiceDate);
//end;
//
//{ TVisCustomer_Update }
//
//function TVisCustomer_Update.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TCustomer ) and
//            ( Visited.ObjectState = posUpdate ) ;
//end;
//
//procedure TVisCustomer_Update.SetupParams;
//var
//  LData : TCustomer ;
//begin
//  LData := Visited as TCustomer ;
//  TableName:= 'Customer';
//  QueryType:= qtUpdate;
//  QueryWhere.SetValueAsString('CustNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('Company', LData.Company);
//  QueryParams.SetValueAsString('Addr1', LData.Address1);
//  QueryParams.SetValueAsString('Addr2', LData.Address2);
//  QueryParams.SetValueAsString('City', LData.City);
//  QueryParams.SetValueAsString('State', LData.State);
//  QueryParams.SetValueAsString('Country', LData.Country);
//  QueryParams.SetValueAsString('Zip', LData.Zip);
//  QueryParams.SetValueAsString('Phone', LData.Phone);
//  QueryParams.SetValueAsString('Fax', LData.Fax);
//  QueryParams.SetValueAsString('Contact', LData.Contact);
//  QueryParams.SetValueAsFloat('TaxRate', LData.TaxRate);
//  QueryParams.SetValueAsDateTime('LastInvoiceDate', LData.LastInvoiceDate);
//end;
//
//{ TVisCustomer_Delete }
//
//function TVisCustomer_Delete.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TCustomer ) and
//            ( Visited.ObjectState = posDelete ) ;
//end;
//
//procedure TVisCustomer_Delete.SetupParams;
//var
//  LData : TCustomer ;
//begin
//  LData := Visited as TCustomer ;
//  QueryType:= qtDelete;
//  TableName:= 'Customer';
//  QueryWhere.SetValueAsString('CustNo', LData.OID.AsString);
//end;
//
//{ TVisVendor_Read }
//
//function TVisVendor_Read.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TVendors ) and
//            ( Visited.ObjectState = posEmpty ) ;
//end;
//
//procedure TVisVendor_Read.Init;
//begin
//  TableName:= 'Vendors';
//end;
//
//procedure TVisVendor_Read.MapRowToObject;
//var
//  LVendor : TVendor ;
//begin
//  LVendor := TVendor.Create ;
//  LVendor.OID.AssignFromTIQuery('VendorNo',Query);
//  LVendor.VendorName := Query.FieldAsString['VendorName'];
//  LVendor.Address1 := Query.FieldAsString['Address1'];
//  LVendor.Address2 := Query.FieldAsString['Address2'];
//  LVendor.City := Query.FieldAsString['City'];
//  LVendor.State := Query.FieldAsString['State'];
//  LVendor.Zip := Query.FieldAsString['Zip'];
//  LVendor.Country := Query.FieldAsString['Country'];
//  LVendor.Phone := Query.FieldAsString['Phone'];
//  LVendor.Fax := Query.FieldAsString['Fax'];
//
//  LVendor.Preferred := Query.FieldAsBoolean['Preferred'];
//
//  LVendor.ObjectState := posClean ;
//  TVendors(Visited).Add(LVendor);
//end;
//
//procedure TVisVendor_Read.SetupParams;
//begin
//
//end;
//
//{ TVisVendor_Create }
//
//function TVisVendor_Create.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TVendor ) and
//            ( Visited.ObjectState = posCreate ) ;
//end;
//
//procedure TVisVendor_Create.SetupParams;
//var
//  LData : TVendor ;
//begin
//  LData := Visited as TVendor ;
//  TableName:= 'Vendors';
//  QueryType:= qtInsert;
//  QueryParams.SetValueAsString('VendorNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('VendorName', LData.VendorName);
//  QueryParams.SetValueAsString('Address1', LData.Address1);
//  QueryParams.SetValueAsString('Address2', LData.Address2);
//  QueryParams.SetValueAsString('City', LData.City);
//  QueryParams.SetValueAsString('State', LData.State);
//  QueryParams.SetValueAsString('Country', LData.Country);
//  QueryParams.SetValueAsString('Zip', LData.Zip);
//  QueryParams.SetValueAsString('Phone', LData.Phone);
//  QueryParams.SetValueAsString('Fax', LData.Fax);
//end;
//
//{ TVisVendor_Update }
//
//function TVisVendor_Update.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TVendor ) and
//            ( Visited.ObjectState = posUpdate ) ;
//end;
//
//procedure TVisVendor_Update.SetupParams;
//var
//  LData : TVendor ;
//begin
//  LData := Visited as TVendor ;
//  TableName:= 'Vendors';
//  QueryType:= qtUpdate;
//  QueryWhere.SetValueAsString('VendorNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('VendorName', LData.VendorName);
//  QueryParams.SetValueAsString('Address1', LData.Address1);
//  QueryParams.SetValueAsString('Address2', LData.Address2);
//  QueryParams.SetValueAsString('City', LData.City);
//  QueryParams.SetValueAsString('State', LData.State);
//  QueryParams.SetValueAsString('Country', LData.Country);
//  QueryParams.SetValueAsString('Zip', LData.Zip);
//  QueryParams.SetValueAsString('Phone', LData.Phone);
//  QueryParams.SetValueAsString('Fax', LData.Fax);
//end;
//
//{ TVisVendor_Delete }
//
//function TVisVendor_Delete.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TVendor ) and
//            ( Visited.ObjectState = posDelete ) ;
//end;
//
//procedure TVisVendor_Delete.SetupParams;
//var
//  LData : TVendor ;
//begin
//  LData := Visited as TVendor ;
//  QueryType:= qtDelete;
//  TableName:= 'Vendors';
//  QueryWhere.SetValueAsString('VendorNo', LData.OID.AsString);
//end;

{ TVisPart_Read }

function TVisPart_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TParts ) and
            ( Visited.ObjectState = posEmpty ) ;
end;

procedure TVisPart_Read.Init;
begin
  TableName:= 'Parts';
end;

procedure TVisPart_Read.MapRowToObject;
var
  LPart : TPart ;
begin
  LPart := TPart.Create ;
  LPart.OID.AssignFromTIQuery('PartNo',Query);
  LPart.VendorNo.AssignFromTIQuery('VendorNo',Query);

  LPart.Description := Query.FieldAsString['Description'];
  LPart.OnHand := Query.FieldAsFloat['OnHand'];
  LPart.OnOrder := Query.FieldAsFloat['OnOrder'];
  LPart.Cost := Query.FieldAsFloat['Cost'];
  LPart.ListPrice := Query.FieldAsFloat['ListPrice'];

  LPart.ObjectState := posClean ;
  TParts(Visited).Add(LPart);
end;

procedure TVisPart_Read.SetupParams;
begin
  if assigned(Visited.Owner) then
    QueryParams.SetValueAsString('VendorNo', Visited.Owner.OID.AsString);

end;

{ TVisPart_Create }

//function TVisPart_Create.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TPart ) and
//            ( Visited.ObjectState = posCreate ) ;
//end;
//
//procedure TVisPart_Create.SetupParams;
//var
//  LData : TPart ;
//begin
//  LData := Visited as TPart ;
//  TableName:= 'Parts';
//  QueryType:= qtInsert;
//  QueryParams.SetValueAsString('PartNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('VendorNo', LData.VendorNo.AsString);
//
//  QueryParams.SetValueAsString('Description', LData.Description);
//  QueryParams.SetValueAsFloat('OnHand', LData.OnHand);
//  QueryParams.SetValueAsFloat('OnOrder', LData.OnOrder);
//  QueryParams.SetValueAsFloat('Cost', LData.Cost);
//  QueryParams.SetValueAsFloat('ListPrice', LData.ListPrice);
//
//
//end;
//
//{ TVisPart_Update }
//
//function TVisPart_Update.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TPart ) and
//            ( Visited.ObjectState = posUpdate ) ;
//end;
//
//procedure TVisPart_Update.SetupParams;
//var
//  LData : TPart ;
//begin
//  LData := Visited as TPart ;
//  TableName:= 'Parts';
//  QueryType:= qtUpdate;
//  QueryParams.SetValueAsString('PartNo', LData.OID.AsString);
//  QueryParams.SetValueAsString('VendorNo', LData.VendorNo.AsString);
//  QueryParams.SetValueAsString('Description', LData.Description);
//  QueryParams.SetValueAsFloat('OnHand', LData.OnHand);
//  QueryParams.SetValueAsFloat('OnOrder', LData.OnOrder);
//  QueryParams.SetValueAsFloat('Cost', LData.Cost);
//  QueryParams.SetValueAsFloat('ListPrice', LData.ListPrice);
//end;
//
//{ TVisPart_Delete }
//
//function TVisPart_Delete.AcceptVisitor: boolean;
//begin
//  result := ( Visited is TPart ) and
//            ( Visited.ObjectState = posDelete ) ;
//end;
//
//procedure TVisPart_Delete.SetupParams;
//var
//  LData : TPart ;
//begin
//  LData := Visited as TPart ;
//  QueryType:= qtDelete;
//  TableName:= 'Parts';
//  QueryParams.SetValueAsString('PartNo', LData.OID.AsString);
//end;

{ TVisOrder_Read }

function TVisOrder_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TOrders ) and
            ( Visited.ObjectState = posEmpty ) ;
end;

procedure TVisOrder_Read.Init;
begin
  TableName:= 'Orders';
end;

procedure TVisOrder_Read.MapRowToObject;
var
  LOrder : TOrder ;
begin
  LOrder := TOrder.Create ;

  LOrder.OID.AssignFromTIQuery('OrderNo',Query);
  LOrder.CustNo.AssignFromTIQuery('CustNo',Query);
  LOrder.EmpNo.AssignFromTIQuery('EmpNo',Query);
  LOrder.SaleDate:= Query.FieldAsDateTime['SaleDate'];
  LOrder.ShipDate:= Query.FieldAsDateTime['ShipDate'];
  LOrder.ShipToContact:= Query.FieldAsString['ShipToContact'];
  LOrder.ShipToAddress1:= Query.FieldAsString['ShipToAddr1'];
  LOrder.ShipToAddress2:= Query.FieldAsString['ShipToAddr2'];
  LOrder.ShipToCity:= Query.FieldAsString['ShipToCity'];
  LOrder.ShipToState:= Query.FieldAsString['ShipToState'];
  LOrder.ShipToZip:= Query.FieldAsString['ShipToZip'];
  LOrder.ShipToCountry:= Query.FieldAsString['ShipToCountry'];
  LOrder.ShipToPhone:= Query.FieldAsString['ShipToPhone'];
  LOrder.PurchaseOrder:= Query.FieldAsString['PO'];
  LOrder.ItemsTotal:= Query.FieldAsFloat['ItemsTotal'];
  LOrder.TaxRate:= Query.FieldAsFloat['TaxRate'];
  LOrder.Freight:= Query.FieldAsFloat['Freight'];
  LOrder.AmountPaid:= Query.FieldAsFloat['AmountPaid'];

  LOrder.ShipViaString:= Query.FieldAsString['ShipVia'];
  LOrder.TermsString:= Query.FieldAsString['Terms'];
  LOrder.PaymentMethodString:= Query.FieldAsString['PaymentMethod'];

  LOrder.ObjectState := posClean ;
  TOrders(Visited).Add(LOrder);
end;

procedure TVisOrder_Read.SetupParams;
begin
  // use the where clause from the Torders to filter
  QueryParams.Assign(TOrders(Visited).Where);
end;


end.
