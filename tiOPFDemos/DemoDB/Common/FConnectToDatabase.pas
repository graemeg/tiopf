unit FConnectToDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls;

type
  TFormConnectToDatabase = class(TFormPickDatabase)
    btnConnect: TButton;
    btnCancel: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute( pSQLDatabaseOnly : boolean = false ; pDataDirDepth : integer = 3 ) : boolean ;
  end;

implementation
uses
  tiPersist
  ,cTIPersist
  ;
  
{$R *.DFM}

procedure TFormConnectToDatabase.Button1Click(Sender: TObject);
begin
  try
    gTIPerMgr.LoadPersistenceLayer(paePersistenceLayer.Value);
    // Should do better than this. Perhaps a CanCreateDatabase
    // class method on the TtiDatabase
    if not SameText( paePersistenceLayer.Value, cTIPersistRemote ) then
    begin
      if not gTIPerMgr.DefaultPerLayer.DatabaseExists(
              paeDatabaseName.Value,
              paeUserName.Value,
              paePassword.Value ) then
       gTIPerMgr.DefaultPerLayer.CreateDatabase(
              paeDatabaseName.Value,
              paeUserName.Value,
              paePassword.Value ) ;
    end ;
    gTIPerMgr.LoadDatabaseLayer(
      paePersistenceLayer.Value,
      paeDatabaseName.Value,
      paeUserName.Value,
      paePassword.Value ) ;
    ModalResult := mrOK ;
  except
    on e:exception do
    begin
      ShowMessage( e.Message ) ;
      ModalResult := mrCancel ;
    end ;
  end ;
end;

procedure TFormConnectToDatabase.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

class function TFormConnectToDatabase.Execute( pSQLDatabaseOnly : boolean = false ; pDataDirDepth : integer = 3 ) : boolean;
var
  lForm : TFormConnectToDatabase ;
begin
  lForm := Create(nil) ;
  try
    lForm.DataDirDepth := pDataDirDepth ;
    if pSQLDatabaseOnly then
    begin
      lForm.btnSetupForXML.Enabled := false ;
      lForm.btnSetupForCSV.Enabled := false ;
    end ;
    result := lForm.ShowModal = mrOK ;
  finally
    lForm.Free ;
  end ;
end;

end.
