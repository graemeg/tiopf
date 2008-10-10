program demo_addressbook;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, frmmain, model, contactmanager, tiopfLCL,
  frmeditcontact, frmEditAddress, frmCityList, frmeditcity, frmCountryList,
  frmeditcountry, frmAddressTypeList;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

