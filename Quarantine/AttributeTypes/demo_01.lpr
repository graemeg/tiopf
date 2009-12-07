program demo_01;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpg_main, fpg_form, fpg_button, model, tiDialogs, tiObject,
  tiVTFObject;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Button1: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    FPerson: TPerson;
    procedure Button1Clicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.Button1Clicked(Sender: TObject);
const
  s = '   %s = %s';
var
  i: integer;
  v: TtiFieldAbs;
begin
  tiShowString(FPerson.AsDebugString);
  writeln('Attributes:');
  for i := 0 to FPerson.FieldList.Count-1 do
  begin
    v := FPerson.FieldList[i];
    writeln('   ' + v.FieldName + ' = ' + v.AsString);
  end;

  writeln('-----');

  // one way of doing it
  writeln(Format(s, ['fname', FPerson.FieldItem['FirstName'].AsString]));
  writeln(Format(s, ['lname', FPerson.FieldItem['LastName'].AsString]));
  // or via the defaut property
  writeln(Format(s, ['age', FPerson['Age'].AsString]));
  writeln(Format(s, ['dob', FPerson['DateOfBirth'].AsString]));

end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPerson := TPerson.Create;
end;

destructor TMainForm.Destroy;
begin
  FPerson.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 300, 250);
  WindowTitle := 'MainForm';

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(68, 76, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    OnClick :=@Button1Clicked;
  end;

  {@VFD_BODY_END: MainForm}
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

