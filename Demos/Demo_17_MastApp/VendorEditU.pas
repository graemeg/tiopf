unit VendorEditU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, tiObject, tiMemoReadOnly;

type
  TfrmVendor = class(TFormTIPerEditDialog)
    Panel1: TPanel;
    editFax: TtiPerAwareEdit;
    editAddress2: TtiPerAwareEdit;
    editAddress1: TtiPerAwareEdit;
    editVendorName: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    editPhone: TtiPerAwareEdit;
    editCountry: TtiPerAwareEdit;
    editZip: TtiPerAwareEdit;
    editCity: TtiPerAwareEdit;
    editState: TtiPerAwareEdit;
    checkPreferred: TtiPerAwareCheckBox;
  private
  protected
    procedure SetData(const Value: TtiObject); override ;
    function  FormIsValid : boolean ; override ;  
  public
    { Public declarations }
  end;

var
  frmVendor: TfrmVendor;

implementation

{$R *.dfm}


function TfrmVendor.FormIsValid: boolean;
begin
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TfrmVendor.SetData(const Value: TtiObject);
begin
  inherited;
  editVendorName.LinkToData(DataBuffer, 'VendorName');
  editAddress1.LinkToData(DataBuffer, 'Address1');
  editAddress2.LinkToData(DataBuffer, 'Address2');
  editCity.LinkToData(DataBuffer, 'City');
  editState.LinkToData(DataBuffer, 'State');
  editZip.LinkToData(DataBuffer, 'Zip');
  editCountry.LinkToData(DataBuffer, 'Country');
  editPhone.LinkToData(DataBuffer, 'Phone');
  editFax.LinkToData(DataBuffer, 'Fax');
  checkPreferred.LinkToData(DataBuffer, 'Preferred');
end;

end.
