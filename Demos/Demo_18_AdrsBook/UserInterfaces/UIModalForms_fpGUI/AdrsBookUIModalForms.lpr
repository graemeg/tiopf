program AdrsBookUIModalForms;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fpg_base, fpg_main,
  frm_main, Adrs_BOM, Adrs_Dependencies, Adrs_Singleton;

procedure MainProc;
var
  frm: TFormMain;
begin
  fpgApplication.Initialize;
  frm := TFormMain.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


