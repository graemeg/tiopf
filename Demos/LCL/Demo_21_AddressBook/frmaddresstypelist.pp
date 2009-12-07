unit frmAddressTypeList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  model, tiModelMediator, Grids;

type

  { TAddressTypeListForm }

  TAddressTypeListForm = class(TForm)
    BAdd: TButton;
    BEdit: TButton;
    BDelete: TButton;
    BClose: TButton;
    GAddressTypes: TStringGrid;
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
  private
    FMediator: TtiModelMediator;
    FData: TAddressTypeList;
    function EditType(C: TAddressType): Boolean;
    procedure SetData(const AValue: TAddressTypeList);
    procedure SetupMediators;
  public
    property Data: TAddressTypeList read FData write SetData;
  end;

var
  AddressTypeListForm: TAddressTypeListForm;

procedure ShowAddressTypes(const AList: TAddressTypeList);

implementation

uses
  tiBaseMediator,contactmanager,tiListMediators;

Resourcestring
  SType    = 'Edit address type';
  SNewName = 'Type a new name for the address Type';

procedure ShowAddressTypes(const AList: TAddressTypeList);

var
  frm: TAddressTypeListForm;
begin
  frm := TAddressTypeListForm.Create(nil);
  try
    frm.Data:=AList;
    frm.ShowModal;
  finally;
    frm.Free;
  end;
end;


{ TAddressTypeListForm }

Function TAddressTypeListForm.EditType(C : TAddressType) : Boolean;

Var
  S : String;

begin
  S:=C.Name;
  Result:=InputQuery(SType,SNewName,S);
  Result:=Result and (S<>'');
  If Result then
    C.Name := S;
end;

procedure TAddressTypeListForm.BEditClick(Sender: TObject);

var
  A : TAddressType;

begin
  A:= TAddressType(TtiStringGridMediatorView(FMediator.FindByComponent(gAddressTypes).Mediator).SelectedObject);
  if Assigned(A) then
    if EditType(A) then
    begin
      // we can save address type here
    end;

end;

procedure TAddressTypeListForm.BAddClick(Sender: TObject);

var
  A: TAddressType;

begin
  A:=TAddressType.Create;
  if EditType(A) then
    begin
    // we can save country here
    gcontactmanager.AddressTypeList.Add(A);
    end
  else
    A.Free;
end;

procedure TAddressTypeListForm.BDeleteClick(Sender: TObject);

var
  A: TAddressType;
  M : TtiMediatorView;

begin
  M:=FMediator.FindByComponent(gAddressTypes).Mediator;
  A := TAddressType(TtiStringGridMediatorView(M).SelectedObject);
  if Assigned(A) then
    begin
    gContactManager.AddressTypeList.Extract(A);
    M.ObjectToGui;
    A.Deleted:=True;
    end;
end;

procedure TAddressTypeListForm.SetData(const AValue: TAddressTypeList);
begin
  FData:=AValue;
  SetupMediators;
end;

procedure TAddressTypeListForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddComposite('Name(110)', GAddressTypes);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

initialization
  {$I frmaddresstypelist.lrs}

end.

