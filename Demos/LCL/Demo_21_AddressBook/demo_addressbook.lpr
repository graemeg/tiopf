program demo_addressbook;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, frmmain, model, contactmanager,
  frmeditcontact, frmEditAddress, frmCityList, frmeditcity, frmCountryList,
  frmeditcountry, frmAddressTypeList, ContactDisplay, DisplayHelpers;

begin
  { Let default to the ISO international date format }
  ShortDateFormat := 'yyyy-mm-dd';

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

