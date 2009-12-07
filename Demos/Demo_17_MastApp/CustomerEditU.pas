unit CustomerEditU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, tiObject, tiMemoReadOnly;

type
  TfrmCustomer = class(TFormTIPerEditDialog)
    Panel1: TPanel;
    editFax: TtiPerAwareEdit;
    editAddress2: TtiPerAwareEdit;
    editAddress1: TtiPerAwareEdit;
    editCompany: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    editPhone: TtiPerAwareEdit;
    editCountry: TtiPerAwareEdit;
    editZip: TtiPerAwareEdit;
    editContact: TtiPerAwareEdit;
    editCity: TtiPerAwareEdit;
    editTax: TtiPerAwareFloatEdit;
    editState: TtiPerAwareEdit;
  private
  protected
    procedure SetData(const Value: TtiObject); override ;
    function  FormIsValid : boolean ; override ;  
  public
    { Public declarations }
  end;

var
  frmCustomer: TfrmCustomer;

implementation

{$R *.dfm}

{ TdlgCustomer }

function TfrmCustomer.FormIsValid: boolean;
begin
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TfrmCustomer.SetData(const Value: TtiObject);
begin
  inherited;
  editContact.LinkToData(DataBuffer, 'Contact');
  editCompany.LinkToData(DataBuffer, 'Company');
  editAddress1.LinkToData(DataBuffer, 'Address1');
  editAddress2.LinkToData(DataBuffer, 'Address2');
  editCity.LinkToData(DataBuffer, 'City');
  editState.LinkToData(DataBuffer, 'State');
  editZip.LinkToData(DataBuffer, 'Zip');
  editCountry.LinkToData(DataBuffer, 'Country');
  editPhone.LinkToData(DataBuffer, 'Phone');
  editFax.LinkToData(DataBuffer, 'Fax');
  editTax.LinkToData(DataBuffer, 'TaxRate');
end;

end.
