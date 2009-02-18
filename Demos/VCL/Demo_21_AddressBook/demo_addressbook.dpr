program demo_addressbook;

uses
  Forms,
  frmMain in 'frmMain.pas' {Form1},
  contactmanager in 'contactmanager.pas',
  model in 'model.pas',
  frmEditContact in 'frmEditContact.pas',
  frmEditAddress in 'frmEditAddress.pas' {EditAddressForm},
  frmCityList in 'frmCityList.pas' {CityListForm},
  frmeditcity in 'frmEditCity.pas' {EditCityForm},
  frmAddressTypeList in 'frmAddressTypeList.pas' {AddressTypeListForm},
  frmCountryList in 'frmCountryList.pas' {CountryListForm},
  frmeditcountry in 'frmEditCountry.pas' {EditCountryForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
