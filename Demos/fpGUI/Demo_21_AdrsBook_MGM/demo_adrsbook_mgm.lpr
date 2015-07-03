program demo_adrsbook_mgm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, frmmain, frmcontactmaint, frmCityList, frmcitymaint,
  frmcountrylist, frmaddressmaint, contact_views;



procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  fpgApplication.ShowHint := True;

  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  Randomize;
  MainProc;
end.


