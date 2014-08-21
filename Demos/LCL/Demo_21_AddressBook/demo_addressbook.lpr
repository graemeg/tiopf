program demo_addressbook;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, frmmain, model, contactmanager,
  frmEditContact, frmEditAddress, frmCityList, frmEditCity, frmCountryList,
  frmEditCountry, frmAddressTypeList, ContactDisplay, DisplayHelpers;

begin
  { Lets default to the ISO international date format }
  ShortDateFormat := 'yyyy-mm-dd';

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

