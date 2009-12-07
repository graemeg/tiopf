program MastAppOpf;

uses
  Forms,
  MastApp_BOM in 'MastApp_BOM.pas',
  OrderEditU in 'OrderEditU.pas' {frmOrder},
  modSharedU in 'modSharedU.pas' {modShared: TDataModule},
  MainForm in 'MainForm.pas' {frmMain},
  VendorListU in 'VendorListU.pas' {frmVendorList},
  CustomerEditU in 'CustomerEditU.pas' {frmCustomer},
  PartsListU in 'PartsListU.pas' {frmPartsList},
  CustomerListU in 'CustomerListU.pas' {frmCustomerList},
  VendorEditU in 'VendorEditU.pas' {frmVendor},
  PartEditU in 'PartEditU.pas' {frmPart},
  PartLookupU in 'PartLookupU.pas' {frmPartLookup},
  BaseListU in 'BaseListU.pas' {frmBaseList},
  FtiPerEditDialog in '..\..\..\GUI\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmodShared, modShared);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
