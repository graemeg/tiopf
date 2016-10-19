unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, ExtCtrls,
  Model, tiModelMediator;

type
  TForm1 = class(TForm)
    btnAddViaCode: TButton;
    btnChange: TButton;
    btnShowModel: TButton;
    btnDelete: TButton;
    btnQuit: TButton;
    chkShowDeleted: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lstName1: TListBox;
    lvName1: TListView;
    Panel1: TPanel;
    grdName1: TStringGrid;
    procedure btnAddViaCodeClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnShowModelClick(Sender: TObject);
    procedure chkShowDeletedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPersonList: TPersonList;       // The subject of our mediator.
    FMediator: TtiModelMediator;
    procedure   SetupMediators;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  tiBaseMediator, tiListMediators, tiDialogs;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPersonList := GeneratePersonList;
end;

procedure TForm1.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnShowModelClick(Sender: TObject);
begin
  tiShowString(FPersonList.AsDebugString);
end;

procedure TForm1.chkShowDeletedChange(Sender: TObject);
var
  med: TtiMediatorView;
begin
  med := FMediator.FindByComponent(grdName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;

  med := FMediator.FindByComponent(lvName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;

  med := FMediator.FindByComponent(lstName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;
end;

procedure TForm1.btnAddViaCodeClick(Sender: TObject);
var
  lData: TPerson;
begin
  lData := TPerson.Create;
  lData.Name := 'I am new';
  lData.Age := 44;
  FPersonList.Add(lData);
end;

procedure TForm1.btnChangeClick(Sender: TObject);
begin
  { The BeginUpdate/EndUpdate will let the Item notify its observers
    only once, even though two change where made.
    Note:
    This is for observers to the Item, not the List that the Item belongs to! }
  FPersonList.Items[1].BeginUpdate;
  FPersonList.Items[1].Name := 'I have changed via code';
  FPersonList.Items[1].Age  := 99;
  FPersonList.Items[1].EndUpdate;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var
  med: TtiMediatorView;
  i: integer;
begin
  med := FMediator.FindByComponent(grdName1).Mediator;
  if not Assigned(TtiStringGridMediatorView(med).SelectedObject) then
  begin
    tiAppError('You need to select a StringGrid item first');
    Exit;
  end;
  TtiStringGridMediatorView(med).SelectedObject.Deleted := True;
  FPersonList.NotifyObservers;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetupMediators;
  FPersonList.NotifyObservers;
end;

procedure TForm1.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.Name := 'DemoFormMediator';
    FMediator.AddComposite('Caption(150,"Name",<);Age(50,"Age",>);GenderGUI(80,"Gender",|)', grdName1);
    FMediator.AddComposite('Caption(150,"Name",<);Age(55,"Age",>);GenderGUI(65,"Gender",|)', lvName1);
    { In the following line of code 'Name' refers to the TPerson.Name property.
      We could also have left the ADisplayNames property empty, which meant it
      would then default to TPerson.Caption }
    FMediator.AddComposite('Name', lstName1);
  end;
  FMediator.Subject := FPersonList;
  FMediator.Active := True;
end;


initialization
  RegisterFallbackListMediators;

end.

