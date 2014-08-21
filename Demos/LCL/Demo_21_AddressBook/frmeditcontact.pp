unit frmEditContact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  model,ComCtrls, tiModelMediator, tiListMediators, EditBtn;

type

  { TContactEditForm }

  TContactEditForm = class(TForm)
    BAdd: TButton;
    BCancel: TButton;
    Button2: TButton;
    BEdit: TButton;
    BSave: TButton;
    EFirstName: TEdit;
    ELastName: TEdit;
    EEmail: TEdit;
    EMobile: TEdit;
    Label1: TLabel;
    LEFirstName: TLabel;
    LELastName: TLabel;
    LEEmail: TLabel;
    LEMobile: TLabel;
    LVAddresses: TListView;
    LMComments: TLabel;
    MComments: TMemo;
    EDateOfBirth: TDateEdit;
    Button1: TButton;
    Label2: TLabel;
    chkIsConfirmed: TCheckBox;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BAddClick(Sender: TObject);
  private
    FData: TContact;
    FMediator: TtiModelMediator;
    FAdrsMediator: TtiModelMediator;
    procedure SetData(const AValue: TContact);
    procedure SetupMediators;
  public
    property  Data: TContact read FData write SetData;
  end;


function EditContact(AData: TContact): Boolean;


var
  ContactEditForm: TContactEditForm;


implementation

{$R *.lfm}

uses
  frmEditAddress, tiDialogs;


function EditContact(AData: TContact): Boolean;
var
  frm: TContactEditForm;
begin
  frm:= TContactEditForm.Create(nil);
  try
    frm.Data:=AData;
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;


{ TContactEditForm }

procedure TContactEditForm.BEditClick(Sender: TObject);
Var
  A : TAddress;
begin
  A := TAddress(TtiListViewMediatorView(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
  if Assigned(A) then
    if EditAddress(A) then
      begin
      // do nothing
      end;
end;

procedure TContactEditForm.Button1Click(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TContactEditForm.BAddClick(Sender: TObject);
var
  A: TAddress;
begin
  A := TAddress.CreateNew;
  if Assigned(A) then
  begin
    if EditAddress(A) then
      FData.AddressList.Add(A)
    else
      A.Free;
  end;
end;

procedure TContactEditForm.BDeleteClick(Sender: TObject);
Var
  A : TAddress;
begin
  A := TAddress(TtiListViewMediatorView(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
  if Assigned(A) then
    A.Deleted:=True;
end;

procedure TContactEditForm.SetData(const AValue: TContact);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TContactEditForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddProperty('FirstName', EFirstName);
    FMediator.AddProperty('LastName', ELastName);
    FMediator.AddProperty('EMail', EEmail);
    FMediator.AddProperty('Mobile', EMobile);
    FMediator.AddProperty('Comments', MComments);
    FMediator.AddProperty('DateOfBirth', EDateOfBirth);
    FMediator.AddProperty('IsConfirmed', chkIsConfirmed);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;

  if not Assigned(FAdrsMediator) then
  begin
    FAdrsMediator := TtiModelMediator.Create(self);
    FAdrsMediator.AddComposite({'AddressType.Name;}'AddressType4GUI(50,"Type");Nr;Street;Telephone1', lvAddresses);
  end;
  FAdrsMediator.Subject := FData.AddressList;
  FAdrsMediator.Active := True;
end;


end.

