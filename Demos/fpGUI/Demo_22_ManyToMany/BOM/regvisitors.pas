unit regvisitors;

{$mode objfpc}{$H+}

interface

uses
  customer_vis;

procedure RegisterVisitors;


implementation

uses
  tiOPFManager
  ;

procedure RegisterVisitors;
begin
  { NOTE: The most reliable order of registering visitors is
    Read, Delete, Update, Create }

  gTIOPFManager.RegisterVisitor('customer_readlist', TVisCustomer_ReadList);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Delete);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Update);
  gTIOPFManager.RegisterVisitor('customer_save', TVisCustomer_Create);

end;

end.

