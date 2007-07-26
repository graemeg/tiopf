unit PartEditU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, tiObject, tiMemoReadOnly;

type
  TfrmPart = class(TFormTIPerEditDialog)
    Panel1: TPanel;
    editDescription: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    editCost: TtiPerAwareFloatEdit;
    editListPrice: TtiPerAwareFloatEdit;
    comboVendor: TtiPerAwareComboBoxDynamic;
    editOID: TtiPerAwareEdit;
    procedure FormCreate(Sender: TObject);
    procedure comboVendorChange(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TtiObject); override ;
    function  FormIsValid : boolean ; override ;
  public
    { Public declarations }
  end;

var
  frmPart: TfrmPart;

implementation

uses modSharedU, MastApp_BOM;

{$R *.dfm}


procedure TfrmPart.comboVendorChange(Sender: TObject);
begin
  inherited;
  TPart(DataBuffer).VendorNo := TPart(comboVendor.Value).OID ;
end;

procedure TfrmPart.FormCreate(Sender: TObject);
begin
  inherited;
  comboVendor.List:= modShared.Vendors.List;
end;

function TfrmPart.FormIsValid: boolean;
begin
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TfrmPart.SetData(const Value: TtiObject);
begin
  inherited;

  editOID.Value := DataBuffer.OID.AsString ;
  comboVendor.Value:= modShared.Vendors.Find(TPart(Value).VendorNo);

//  comboVendor.Enabled:= Value.ObjectState in [posEmpty, posPK, posCreate];

  editDescription.LinkToData(DataBuffer, 'Description');
  editCost.LinkToData(DataBuffer, 'Cost');
  editListPrice.LinkToData(DataBuffer, 'ListPrice');
end;

end.
