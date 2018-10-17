unit FMainCreateDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, Menus, ActnList, Buttons;

type
  TFormMainCreateDatabase = class(TFormPickDatabase)
    btnDatabaseExists: TButton;
    btnCreateDatabase: TButton;
    procedure btnDatabaseExistsClick(Sender: TObject);
    procedure btnCreateDatabaseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMainCreateDatabase: TFormMainCreateDatabase;

implementation
uses
  tiOPFManager
  ,tiPersistenceLayers
 ;
  
{$R *.DFM}

procedure TFormMainCreateDatabase.btnDatabaseExistsClick(Sender: TObject);
var
  LPerLayer: TtiPersistenceLayer;
begin
  LPerLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(PersistenceLayerName);
  Assert(LPerLayer<>nil, '<' + PersistenceLayerName + '> not registered');
  if LPerLayer.DatabaseExists(DatabaseName, UserName, Password)
  then
    ShowMessage('Database <' + DatabaseName + '> exists.')
  else
    ShowMessage('Database <' + DatabaseName + '> does not exist.');
end;

procedure TFormMainCreateDatabase.btnCreateDatabaseClick(Sender: TObject);
var
  LPerLayer: TtiPersistenceLayer;
begin
  LPerLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(PersistenceLayerName);
  Assert(LPerLayer<>nil, '"' + PersistenceLayerName + '" not registered');
  LPerLayer.CreateDatabase(DatabaseName, UserName, Password);
  ShowMessage('Database "' + DatabaseName + '" has been created.');
end;

end.
