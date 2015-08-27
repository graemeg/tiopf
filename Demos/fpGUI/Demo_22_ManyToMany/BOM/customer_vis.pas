unit customer_vis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiVisitorDB;

type

  TVisCustomer_ReadList = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;


  TVisCustomer_Create = class( TtiVisitorUpdate )
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;


  TVisCustomer_Delete = class( TtiVisitorUpdate )
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;


  TVisCustomer_Update = class(TVisCustomer_Create)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
  end;


implementation

uses
  tiLog
  ,tiObject
  ,customer
  ;

{ TVisCustomer_ReadList }

function TVisCustomer_ReadList.AcceptVisitor: boolean;
begin
  Result := (Visited is TCustomerList) and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisCustomer_ReadList.Init;
begin
  inherited Init;
end;

procedure TVisCustomer_ReadList.SetupParams;
begin
  inherited SetupParams;
end;

procedure TVisCustomer_ReadList.MapRowToObject;
begin
  inherited MapRowToObject;
end;

{ TVisCustomer_Create }

function TVisCustomer_Create.AcceptVisitor: boolean;
begin
  Result:= (Visited is TCustomer) and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisCustomer_Create.Init;
begin
  inherited Init;
end;

procedure TVisCustomer_Create.SetupParams;
begin
  inherited SetupParams;
end;

{ TVisCustomer_Delete }

function TVisCustomer_Delete.AcceptVisitor: boolean;
begin
  Result := (Visited is TCustomer) and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisCustomer_Delete.Init;
begin
  inherited Init;
end;

procedure TVisCustomer_Delete.SetupParams;
begin
  inherited SetupParams;
end;

{ TVisCustomer_Update }

function TVisCustomer_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TCustomer) and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisCustomer_Update.Init;
begin
  inherited Init;
end;

end.

