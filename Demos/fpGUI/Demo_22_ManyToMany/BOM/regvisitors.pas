unit regvisitors;

{$mode objfpc}{$H+}

interface


procedure RegisterVisitors;


implementation

uses
  tiOPFManager
  ,customer_vis
  ,order_vis
  ;

procedure RegisterVisitors;
begin
  { NOTE: The most reliable order of registering visitors is
    Read, Delete, Update, Create }

  gTIOPFManager.RegisterVisitor('customer_readlist', TVisCustomer_ReadList);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Delete);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Update);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Create);

  gTIOPFManager.RegisterVisitor('order_readlist', TVisOrder_ReadList);
//  gTIOPFManager.RegisterVisitor('customer_save', TVisOrder_Delete);
//  gTIOPFManager.RegisterVisitor('customer_save', TVisOrder_Update);
//  gTIOPFManager.RegisterVisitor('customer_save', TVisOrder_Create);

end;

end.

