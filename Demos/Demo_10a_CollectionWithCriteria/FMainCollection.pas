unit FMainCollection;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, tiVirtualTrees
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiVTListView, tiObject
 ;


type
  TFormCollection = class(TForm)
    ActionList1: TActionList;
    aSave: TAction;
    LV: TtiVTListView;
    Panel2: TPanel;
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    paeClientID: TtiPerAwareEdit;
    btnInsertRow: TButton;
    btnDeleteRow: TButton;
    Button2: TButton;
    Button1: TButton;
    btnReadList: TButton;
    EditFilter: TEdit;
    Button3: TButton;
    LabelFilter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure btnDeleteRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnReadListClick(Sender: TObject);
    procedure LVFilterData(pData: TtiObject; var pInclude: Boolean);
    procedure LVItemArrive(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure Button3Click(Sender: TObject);
  private
    FClients: TClients;
    procedure CreateTable;
    procedure DropTable;
    function  TableExists: boolean;
    procedure LVDeriveOID(const pVT: TtiCustomVirtualTree;
                           const pData: TtiObject;
                           const ptiListColumn: TtiVTColumn;
                           var   pResult: string);
  public
    { Public declarations }
  end;

var
  FormCollection: TFormCollection;

implementation
uses
   tiQuery
  ,tiOIDGUID 
  ,tiOPFManager
  ,tiDBConnectionPool
  ,tiOID
  ,tiDialogs
  ,tiConstants   
 ;

{$R *.dfm}

// Create table
procedure TFormCollection.CreateTable;
var
  LTableMetaData: TtiDBMetaDataTable;
begin
  LTableMetaData:= TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LTableMetaData.AddInstance('OID',               qfkString,  36); // Using GUID OIDs
    LTableMetaData.AddInstance('Client_Name',       qfkString, 200);
    LTableMetaData.AddInstance('Client_ID',          qfkString,   9);
    GTIOPFManager.CreateTable(LTableMetaData);
  finally
    LTableMetaData.Free;
  end;
end;

// Drop table
procedure TFormCollection.DropTable;
begin
  GTIOPFManager.DropTable('Client');
end;

// Does a table exist?
function TFormCollection.TableExists;
var
  LDBMetaData: TtiDBMetaData;
  LDatabase  : TtiDatabase;
begin
  LDBMetaData:= TtiDBMetaData.Create;
  try
    LDatabase:= GTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataTables(LDBMetaData);
      result:= LDBMetaData.FindByTableName('Client') <> nil;
    finally
      GTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LDBMetaData.Free;
  end;
end;

procedure TFormCollection.FormCreate(Sender: TObject);
begin
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  FClients:= TClients.Create;
  // Drop and re-create to be sure we start with the correct structure
  if TableExists then
  begin
    DropTable;
    CreateTable;
  end;
  LV.AddColumn(LVDeriveOID, 'OID', 270);
  LV.AddColumn('ClientName', vttkString, 'Client name', 200);
  LV.AddColumn('ClientID',   vttkString, 'Client ID', 80);
  LV.Data:= FClients;
end;

// Insert a row
procedure TFormCollection.btnInsertRowClick(Sender: TObject);
var
  LClient: TClient;
begin
  LClient:= TClient.CreateNew;
  FClients.Add(LClient);
  LV.Refresh(LClient);
end;

// Delete a row
procedure TFormCollection.btnDeleteRowClick(Sender: TObject);
begin
  if LV.SelectedData <> nil then
    LV.SelectedData.Deleted:= true;
  LV.Refresh;
end;

procedure TFormCollection.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TFormCollection.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormCollection.LVDeriveOID(const pVT: TtiCustomVirtualTree;
  const pData: TtiObject; const ptiListColumn: TtiVTColumn;
  var pResult: string);
begin
  Assert(pData.IsValid, CTIErrorInvalidObject);
  pResult:= pData.OID.AsString;
end;

procedure TFormCollection.LVFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  Assert(pData.IsValid, CTIErrorInvalidObject);
  pInclude:= not pData.Deleted;
end;

procedure TFormCollection.LVItemArrive(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  LClient: TClient;
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  LClient:= pData as TClient;
  paeOID.Value:= LClient.OID.AsString;
  paeClientName.Value:= LClient.ClientName;
  paeClientID.Value:= LClient.ClientID;
end;

procedure TFormCollection.Button2Click(Sender: TObject);
begin
  tiMessageDlg(FClients.AsDebugString, ['OK']);
end;

procedure TFormCollection.Button3Click(Sender: TObject);
begin
  FClients.Clear;
  FClients.Criteria.ClearAll;

  // needed for Db Independant mapping
  // note Automapping can use property name instead of field name
  // DB Independnand can only use field name at the moment
  FClients.Criteria.AddLike('Client_Name', EditFilter.Text + '%');


  FClients.Read;
  LV.Refresh;
end;

procedure TFormCollection.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
end;

procedure TFormCollection.btnReadListClick(Sender: TObject);
begin
  FClients.Clear;
  FClients.Criteria.ClearAll;  
  FClients.Read;
  LV.Refresh;
end;

end.
