unit FMainCreateDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls;

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
  tiPersist
  ;
{$R *.DFM}

procedure TFormMainCreateDatabase.btnDatabaseExistsClick(Sender: TObject);
begin
  gTIPerMgr.LoadPersistenceLayer( PersistenceLayerName ) ;
  try
    if gTIPerMgr.DefaultPerLayer.DatabaseExists(
      DatabaseName, UserName, Password )
    then
      ShowMessage( 'Database <' + DatabaseName +
                   '> exists.' )
    else
      ShowMessage( 'Database <' + DatabaseName +
                   '> does not exist.' );
  finally
    gTIPerMgr.UnLoadPersistenceFramework ;
  end ;
end;

procedure TFormMainCreateDatabase.btnCreateDatabaseClick(Sender: TObject);
begin
  gTIPerMgr.LoadPersistenceLayer( PersistenceLayerName ) ;
  try
    gTIPerMgr.DefaultPerLayer.CreateDatabase(
      DatabaseName, UserName, Password ) ;
    ShowMessage( 'Database <' + DatabaseName +
                 '> has been created.' ) ;
  finally
    gTIPerMgr.UnLoadPersistenceFramework ;
  end ;
end;

end.
