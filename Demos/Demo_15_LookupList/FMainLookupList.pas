unit FMainLookupList;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiObject, ActnList,
  Client_BOM, tiListView, ComCtrls, tiVirtualTrees, tiVTListView,
  Variants, tiVTAbstract;

type
  TFormMainLookupList = class(TForm)
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    Button3: TButton;
    aRead: TAction;
    LV: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure LVFilterData(pData: TtiObject; var pbInclude: Boolean);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
  private
    FClients: TClients;
    procedure CreateTables;
    procedure DropTables;
  public
    { Public declarations }
  end;

var
  FormMainLookupList: TFormMainLookupList;

implementation
uses
  tiQuery
//  ,tiPersist
  ,tiDBConnectionPool
//  ,tiPtnVisPerObj_Cli
  ,tiOPFManager
  ,tiOID
  ,tiOIDGUID
  ,FClientEdit
  ,tiDialogs
  ,tiGUIUtils
 ;

{$R *.dfm}

// Create table
procedure TFormMainLookupList.CreateTables;
  procedure _AddClientSource(const pDisplayText: string);
  var
    lClientSource: TClientSource;
  begin
    lClientSource:= TClientSource.CreateNew;
    lClientSource.DisplayText:= pDisplayText;
    gClientSources.Add(lClientSource);
  end;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client_Source';
    lTableMetaData.AddInstance('OID',          qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddInstance('Display_Text',  qfkString, 20);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;


  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lTableMetaData.AddInstance('OID',           qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddInstance('Client_Name',   qfkString, 200);
    lTableMetaData.AddInstance('Client_Source', qfkString,  36);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

  _AddClientSource('Unknown');
  _AddClientSource('Yellow pages');
  _AddClientSource('Referal');
  _AddClientSource('Radio');
  _AddClientSource('TV');
  _AddClientSource('News paper');
  gClientSources.Save;

end;

// Drop table
procedure TFormMainLookupList.DropTables;
begin
  try GTIOPFManager.DropTable('Client') except end;
  try GTIOPFManager.DropTable('Client_Source') except end;
end;

procedure TFormMainLookupList.FormCreate(Sender: TObject);
begin
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  LV.AddColumn('ClientSourceAsGUIString', vttkString, 'Client source', 80);
  LV.AddColumn('ClientName',     vttkString, 'Client name', 200);





  FClients:= TClients.Create;

  if tiAppConfirmation('Do you want to drop and re-create the tables?') then
  begin
    DropTables;
    CreateTables;
  end;

  gClientSources; // This will force the pre-loading of ClientSources

  aReadExecute(nil);
  
end;

// Insert a row
procedure TFormMainLookupList.btnInsertRowClick(Sender: TObject);
begin
end;

// Clear GUI and get a new OID

// Delete a row
procedure TFormMainLookupList.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TFormMainLookupList.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormMainLookupList.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainLookupList.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
  aRead.Enabled:= aSave.Enabled;
end;

procedure TFormMainLookupList.aReadExecute(Sender: TObject);
begin
  LV.Data:= nil;
  FClients.Clear;
  FClients.Read;
  LV.Data:= FClients;
end;


procedure TFormMainLookupList.LVFilterData(pData: TtiObject; var pbInclude: Boolean);
begin
  pbInclude:= not pData.Deleted;
end;

procedure TFormMainLookupList.LVItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(AData) then
    lv.Refresh;
end;

procedure TFormMainLookupList.LVItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
var
  LClient: TClient;
begin
  LClient:= TClient.CreateNew;
  if TFormClientEdit.Execute(LClient) then
  begin
    FClients.Add(LClient);
    LV.Refresh(LClient);
  end
  else
    LClient.Free;
end;

procedure TFormMainLookupList.LVItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  TFormClientEdit.Execute(AData);
end;

end.
