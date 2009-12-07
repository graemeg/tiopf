unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Model, tiModelMediator;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edtName: TEdit;
    Label2: TLabel;
    ageTrackBar: TTrackBar;
    Label3: TLabel;
    memName: TMemo;
    cbGender: TComboBox;
    Label4: TLabel;
    btnChgViaCode: TButton;
    btnShowModel: TButton;
    btnClose: TButton;
    edtAge: TEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnShowModelClick(Sender: TObject);
    procedure btnChgViaCodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { The object we will be working with. }
    FPerson: TPerson;
    { Form Mediator }
    FMediator: TtiModelMediator;

    procedure SetupMediators;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnShowModelClick(Sender: TObject);
begin
  ShowMessage(FPerson.AsDebugString);
end;

{ The controls will automatically update as well! }
procedure TForm1.btnChgViaCodeClick(Sender: TObject);
begin
  FPerson.Name    := 'John Doe';
  FPerson.Age     := 23;
  FPerson.Gender  := genFemale;
end;

procedure TForm1.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.Name := 'DemoModelMediator';
    FMediator.AddProperty('Name', edtName);
    FMediator.AddProperty('Age', AgeTrackBar);
    FMediator.AddProperty('Age', edtAge);
    FMediator.AddProperty('Name', memName);
    FMediator.AddProperty('GenderGUI', cbGender);
  end;
  FMediator.Subject := FPerson;
  FMediator.Active := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: TGender;
begin
  { The Data Object being observed }
  FPerson       := TPerson.Create;
  FPerson.Name  := 'Graeme Geldenhuys';
  FPerson.Age   := 32;

  { this could also be done in a custom mediator }
  for i := Low(TGender) to High(TGender) do
    cbGender.Items.Add(cGender[i]);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetupMediators;
end;

end.