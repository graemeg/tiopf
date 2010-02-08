unit FMainEditDataInGUI;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, ComCtrls, tiVTListView, tiObject, tiVirtualTrees, Variants;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    Button3: TButton;
    aUndo: TAction;
    LV: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aUndoExecute(Sender: TObject);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure LVFilterData(pData: TtiObject; var pInclude: Boolean);
  private
    FClients: TClients;
    procedure CreateTables;
    procedure DropTables;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  tiQuery
  ,tiOPFManager
  ,tiDBConnectionPool
  ,tiOID
  ,tiOIDGUID
  ,FClientEdit
  ,tiDialogs
  ,tiGUIUtils
 ;

{$R *.dfm}

// Create table
procedure TForm2.CreateTables;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Client_Name',       qfkString, 200);
    lTableMetaData.AddField('Client_ID',         qfkString,   9);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
end;

// Drop table
procedure TForm2.DropTables;
begin
  try GTIOPFManager.DropTable('Client') except end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  LV.AddColumn('ClientID',   vttkString, 'Client ID', 80);
  LV.AddColumn('ClientName', vttkString, 'Client name', 200);

  FClients:= TClients.Create;

  if tiAppConfirmation('Do you want to drop and re-create the tables?') then
  begin
    DropTables;
    CreateTables;
  end else
    FClients.Read;

  LV.Data:= FClients;
  
end;

// Insert a row
procedure TForm2.btnInsertRowClick(Sender: TObject);
begin
end;

// Clear GUI and get a new OID

// Delete a row
procedure TForm2.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

procedure TForm2.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
  aUndo.Enabled:= aSave.Enabled;
end;

procedure TForm2.aUndoExecute(Sender: TObject);
begin
  LV.Data:= nil;
  FClients.Clear;
  FClients.Read;
  LV.Data:= FClients;
end;

procedure TForm2.LVItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
begin
  if TFormClientEdit.Execute(pData) then
    pVT.Refresh(pData);
end;

procedure TForm2.LVItemInsert(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  lClient: TClient;
begin
  lClient:= TClient.CreateNew;
  if TFormClientEdit.Execute(lClient) then
  begin
    FClients.Add(lClient);
    pVT.Refresh(lClient);
  end
  else
    lClient.Free;
end;

procedure TForm2.LVItemDelete(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(pData as TClient) then
    pVT.Refresh;
end;

procedure TForm2.LVFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  pInclude:= not pData.Deleted;
end;

end.
