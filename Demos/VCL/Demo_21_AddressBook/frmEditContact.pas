unit frmEditContact;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  model, ComCtrls, tiModelMediator, tiListMediators;

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
    dtpDOB: TDateTimePicker;
    lblDOB: TLabel;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FData: TContact;
    FMediator: TtiModelMediator;
    FAdrsMediator: TtiModelMediator;
    FMemento : TContactMemento;
    procedure SetData(const AValue: TContact);
    Procedure SetupMediators;
  public
    property Data: TContact read FData write SetData;
    property Memento : TContactMemento read FMemento Write FMemento;
  end;


function EditContact(AData: TContact): Boolean;


var
  ContactEditForm: TContactEditForm;


implementation

{$R *.dfm}

uses
  frmEditAddress;


function EditContact(AData: TContact): Boolean;
var
  frm: TContactEditForm;
begin
  frm:= TContactEditForm.Create(nil);
  try
    frm.Data := AData;
    result := (frm.ShowModal = mrOK);
    if not result then
    begin
      frm.Data.BeginUpdate;
      frm.Data.Memento := frm.Memento;
      frm.Data.EndUpdate;
    end;
  finally
    frm.Free;
  end;
end;


{ TContactEditForm }

procedure TContactEditForm.BEditClick(Sender: TObject);
Var
  A: TAddress;
begin
  A := TAddress(TtiListViewMediatorView(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
  if Assigned(A) then
    if EditAddress(A) then
    begin
      // do nothing
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
  FreeAndNil(FMemento);
  FMemento := FData.Memento;
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
    FMediator.AddProperty('DateOfBirth', dtpDOB);
    FMediator.AddProperty('Comments', MComments);
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


procedure TContactEditForm.FormDestroy(Sender: TObject);
begin
  FAdrsMediator.Active := False;
  FMediator.Active := False;
end;

end.