program demo_addressbook;

uses
{  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,   }
  Forms,
  SysUtils,
  frmMain in 'frmMain.pas' {frmDemoMain},
  contactmanager in 'contactmanager.pas',
  model in 'model.pas',
  frmEditContact in 'frmEditContact.pas',
  frmEditAddress in 'frmEditAddress.pas' {EditAddressForm},
  frmCityList in 'frmCityList.pas' {CityListForm},
  frmeditcity in 'frmEditCity.pas' {EditCityForm},
  frmAddressTypeList in 'frmAddressTypeList.pas' {AddressTypeListForm},
  frmCountryList in 'frmCountryList.pas' {CountryListForm},
  frmeditcountry in 'frmEditCountry.pas' {EditCountryForm},
  ContactDisplay in 'ContactDisplay.pas';

{$R *.res}

begin
  { Let default to the ISO international date format }
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  Application.Initialize;
  Application.Title := 'MGM Demo Addressbook';
  Application.CreateForm(TfrmDemoMain, frmDemoMain);
  Application.Run;
end.
