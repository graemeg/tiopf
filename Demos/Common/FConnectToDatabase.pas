unit FConnectToDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, Menus,
  ActnList, Buttons, tiSpeedButton;

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
    class function Execute(const ASQLDatabaseOnly: boolean = false; const ADataDirDepth: integer = 3): boolean;
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
    gTIOPFManager.DefaultPerLayerName:= PersistenceLayerName;
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
  Assert(gTIOPFManager.DefaultPerLayer.TestValid, CTIErrorInvalidObject);
  result:= gTIOPFManager.DefaultPerLayer.DatabaseExists(DatabaseName, UserName, Password);
end;

procedure TFormConnectToDatabase.ConnectToDatabase;
begin
  gTIOPFManager.ConnectDatabase(
    DatabaseName,
    UserName,
    Password);
end;

procedure TFormConnectToDatabase.CreateDatabase;
begin
  Assert(gTIOPFManager.DefaultPerLayer.TestValid, CTIErrorInvalidObject);
  gTIOPFManager.DefaultPerLayer.CreateDatabase(DatabaseName, UserName, Password);
end;

procedure TFormConnectToDatabase.btnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

class function TFormConnectToDatabase.Execute(
  const ASQLDatabaseOnly: boolean = false;
  const ADataDirDepth: integer = 3): boolean;
var
  LForm: TFormConnectToDatabase;
  i: integer;
begin
  LForm:= Create(nil);
  try
    LForm.DataDirDepth:= ADataDirDepth;
    for i := 0 to LForm.SingleUserPersistenceLayers.Count - 1 do
      (LForm.SingleUserPersistenceLayers.Items[i] as TAction).Enabled:= not ASQLDatabaseOnly;
    result:= LForm.ShowModal = mrOK;
  finally
    LForm.Free;
  end;
end;

end.
