program demo_02;

{$mode objfpc}{$H+}

(*
  Adding a {$DEFINE LINK_???} to your project, either in the LPR
  or the Project | Options | Compiler Options | Other dialog
  or in the tiOPFfpGUI.lpk package's Compiler Options | Other dialog,
  the specified persistence layers will be linked and loaded.

  By default when tiOPF is used with FPC, the following persistence
  layers are automatically linked in:
    * CSV
    * TAB
    * XMLLight
    * SqlDB_IB  (Interbase & Firebird)

  When you run this demo, a dialog will show listing
  the loaded persistence layers. (Note, the dialog will
  say the database is not connected - wich is correct.)

  Take a look at tiOPFManager about line 215 you will see how
  this the defines are implemented.
*)

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, DemoDBUtils, fpg_base, fpg_main, fpg_form;

type
  TMainForm = class(TfpgForm)
  private
    procedure   FormShow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowConnectedDatabases;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'tiOPF Demo 02';
  WindowPosition := wpUser;
  SetPosition(100, 100, 300, 200);
  OnShow := @FormShow;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
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


