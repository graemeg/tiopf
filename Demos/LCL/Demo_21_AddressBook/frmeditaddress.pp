unit frmEditAddress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, model, tiFormMediator;

type

  { TEditAddressForm }

  TEditAddressForm = class(TForm)
    BCancel: TButton;
    BSave: TButton;
    CBType: TComboBox;
    CBCity: TComboBox;
    ENumber: TEdit;
    EStreet: TEdit;
    ETelephone1: TEdit;
    ETelephone2: TEdit;
    EFax: TEdit;
    LCBType: TLabel;
    LENumber: TLabel;
    LEStreet: TLabel;
    LCBCity: TLabel;
    LETelephone1: TLabel;
    LEFax: TLabel;
    LETelephone2: TLabel;
  private
    FData: TAddress;
    { private declarations }
    FMediator: TFormMediator;
    procedure SetData(const AValue: TAddress);
    procedure SetupMediators;
  public
    property  Data: TAddress read FData write SetData;
  end;

function EditAddress(AData: TAddress): Boolean;

var
  EditAddressForm: TEditAddressForm;

implementation

uses
  contactmanager;


function EditAddress(AData: TAddress): Boolean;

var
  frm: TEditAddressForm;

begin
  frm:= TEditAddressForm.Create(nil);
  try
    frm.Data:=AData;
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{ TEditAddressForm }

procedure TEditAddressForm.SetData(const AValue: TAddress);
begin
  FData:=AValue;
  SetupMediators;
end;

procedure TEditAddressForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('AddressType', cbType).ValueList := gContactManager.AddressTypeList;
    FMediator.AddProperty('Nr', ENumber);
    FMediator.AddProperty('Street', EStreet);
    FMediator.AddProperty('City', cbCity).ValueList := gContactManager.CityList;
    FMediator.AddProperty('Telephone1', ETelePhone1);
    FMediator.AddProperty('Telephone2', ETelePhone2);
    FMediator.AddProperty('Fax', EFax);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

initialization
  {$I frmeditaddress.lrs}

end.

