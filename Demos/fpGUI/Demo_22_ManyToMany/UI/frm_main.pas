unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(521, 216, 609, 311);
  WindowTitle := 'MainForm';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;
  ShowHint := True;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
