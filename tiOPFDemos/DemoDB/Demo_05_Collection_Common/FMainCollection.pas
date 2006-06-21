unit FMainCollection;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiMemoReadOnly
  ;
  

type
  TFormCollection = class(TForm)
    btnInsertRow: TButton;
    btnDeleteRow: TButton;
    btnClear: TButton;
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    paeClientID: TtiPerAwareEdit;
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    tiMemoReadOnly1: TtiMemoReadOnly;
    btnReadList: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure btnReadListClick(Sender: TObject);
  private
    FClients : TClients ;
    procedure CreateTable;
    procedure DropTable;
    function  TableExists: boolean ;
  public
    { Public declarations }
  end;

var
  FormCollection: TFormCollection;

implementation
uses
  tiQuery
  ,tiPersist
  ,tiDBConnectionPool
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDAbs
  ,tiPerObjOIDGUID
  ,tiPtnVisPerObj
  ;

{$R *.dfm}

// Create table
procedure TFormCollection.CreateTable;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',               qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',       qfkString, 200 ) ;
    lTableMetaData.AddField( 'Client_ID',          qfkString,   9 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
end;

// Drop table
procedure TFormCollection.DropTable;
begin
  gTIPerMgr.DropTable( 'Client' );
end;

// Does a table exist?
function TFormCollection.TableExists;
var
  lDBMetaData : TtiDBMetaData ;
  lPooledDB   : TPooledDB ;
  lDatabase   : TtiDatabase ;
begin
  lDBMetaData := TtiDBMetaData.Create ;
  try
    lPooledDB := gTIPerMgr.DefaultDBConnectionPool.Lock ;
    try
      lDatabase := lPooledDB.Database ;
      lDatabase.ReadMetaDataTables(lDBMetaData);
      result := lDBMetaData.FindByTableName('Client') <> nil;
    finally
      gTIPerMgr.DefaultDBConnectionPool.UnLock(lPooledDB);
    end ;
  finally
    lDBMetaData.Free;
  end ;
end;

procedure TFormCollection.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  FClients := TClients.Create ;
  // Drop and re-create to be sure we start with the correct structure
  if TableExists then
    DropTable;
  CreateTable ;
end;

// Insert a row
procedure TFormCollection.btnInsertRowClick(Sender: TObject);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew ;
  lClient.OID.AsString := paeOID.Value ;
  lClient.ClientName := paeClientName.Value ;
  lClient.ClientID   := paeClientID.Value ;
  FClients.Add(lClient);
end;

// Clear GUI and get a new OID
procedure TFormCollection.btnClearClick(Sender: TObject);
var
  lOID : TOID;
begin
  lOID := gTIPerMgr.OIDFactory.CreateOID;
  try
    lOID.GetNextValue(gTIPerMgr.DefaultDBConnectionName,
                      gTIPerMgr.DefaultPerLayerName);
    paeOID.Value := lOID.AsString ;
    paeClientName.Value := 'TEST ' + DateTimeToStr(Now) ;
    paeClientID.Value := IntToStr(GetTickCount) ;
  finally
    lOID.Free;
  end ;
end;

// Delete a row
procedure TFormCollection.btnDeleteRowClick(Sender: TObject);
var
  lClient : TPerObjAbs ;
begin
  lClient := FClients.Find(paeOID.Value) ;
  if lClient <> nil then
    lClient.Deleted := true ;
end;

procedure TFormCollection.aSaveExecute(Sender: TObject);
begin
  FClients.Save ;
end;

procedure TFormCollection.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormCollection.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormCollection.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
end;

procedure TFormCollection.btnReadListClick(Sender: TObject);
begin
  FClients.Clear ;
  FClients.Read ;
end;

end.
