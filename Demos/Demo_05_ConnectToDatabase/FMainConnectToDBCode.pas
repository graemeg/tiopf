unit FMainConnectToDBCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, ActnList, Menus, Buttons;

type
  TFormMainConnectToDBCode = class(TFormPickDatabase)
    btnConnectToDatabase: TButton;
    btnDisconnectFromDatabase: TButton;
    btnShowWhatsConnected: TButton;
    procedure btnConnectToDatabaseClick(Sender: TObject);
    procedure btnShowWhatsConnectedClick(Sender: TObject);
    procedure btnDisconnectFromDatabaseClick(Sender: TObject);
  private
  protected
  public
    { Public declarations }
  end;

var
  FormMainConnectToDBCode: TFormMainConnectToDBCode;

implementation
uses
  tiOPFManager
  ,tiUtils
  ,tiDialogs
  ,tiPersistenceLayers
  ,DemoDBUtils
 ;

{$R *.DFM}

procedure TFormMainConnectToDBCode.btnConnectToDatabaseClick(Sender: TObject);
begin
  GTIOPFManager.ConnectDatabase(
    DatabaseName,
    UserName,
    Password,
    '', // Additional params as Name-Value pairs
    PersistenceLayerName);
  tiAppMessage('Database connection pool for "' + DatabaseName + '" loaded.');
end;

procedure TFormMainConnectToDBCode.btnShowWhatsConnectedClick(Sender: TObject);
begin
  ShowConnectedDatabases;
end;

procedure TFormMainConnectToDBCode.btnDisconnectFromDatabaseClick(Sender: TObject);
begin
  GTIOPFManager.DisconnectDatabase(
    DatabaseName,
    PersistenceLayerName);
  tiAppMessage('Disconnected from "' + DatabaseName + '"');
end;

end.
