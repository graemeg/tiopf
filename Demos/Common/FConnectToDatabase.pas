unit FConnectToDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, Menus, ActnList, Buttons;

type
  TFormConnectToDatabase = class(TFormPickDatabase)
    btnConnect: TButton;
    btnCancel: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure ConnectToDatabase;
    procedure CreateDatabase;
    function DatabaseExists: boolean;
    { Private declarations }
  public
    class function Execute(const ASQLDatabaseOnly: boolean = false): boolean;
  end;

implementation
uses
  tiOPFManager
  ,tiConstants
 ;

{$R *.DFM}

procedure TFormConnectToDatabase.Button1Click(Sender: TObject);
begin
  try
    GTIOPFManager.DefaultPersistenceLayerName:= PersistenceLayerName;
    if not DatabaseExists then
      CreateDatabase;
    ConnectToDatabase;
    ModalResult:= mrOK;
  except
    on e:exception do
      ShowMessage(e.Message);
  end;
end;

function TFormConnectToDatabase.DatabaseExists: boolean;
begin
  Assert(GTIOPFManager.DefaultPerLayer.TestValid, CTIErrorInvalidObject);
  result:= GTIOPFManager.DefaultPerLayer.DatabaseExists(DatabaseName, UserName, Password);
end;

procedure TFormConnectToDatabase.ConnectToDatabase;
begin
  GTIOPFManager.ConnectDatabase(
    DatabaseName,
    UserName,
    Password);
end;

procedure TFormConnectToDatabase.CreateDatabase;
begin
  Assert(GTIOPFManager.DefaultPerLayer.TestValid, CTIErrorInvalidObject);
  GTIOPFManager.DefaultPerLayer.CreateDatabase(DatabaseName, UserName, Password);
end;

procedure TFormConnectToDatabase.btnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

class function TFormConnectToDatabase.Execute(
  const ASQLDatabaseOnly: boolean = false): boolean;
var
  LForm: TFormConnectToDatabase;
  i: integer;
begin
  LForm:= Create(nil);
  try
    for i := 0 to LForm.SingleUserPersistenceLayers.Count - 1 do
      (LForm.SingleUserPersistenceLayers.Items[i] as TAction).Enabled:= not ASQLDatabaseOnly;
    result:= LForm.ShowModal = mrOK;
  finally
    LForm.Free;
  end;
end;

end.
