program Demo_AdrsBook_MGM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  tiOIDGUID, formmain, model, contactmanager, tiOPFGUI, views, formcontactmaint,
  formcitylist, formcitymaint;

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

