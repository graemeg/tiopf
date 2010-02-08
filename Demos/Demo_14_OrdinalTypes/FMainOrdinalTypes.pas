unit FMainOrdinalTypes;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, tiListView, ComCtrls, tiVirtualTrees, tiVTListView, tiObject,
  Variants, tiVTAbstract;

type
  TFormMainOrdinalTypes = class(TForm)
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
    procedure LVFilterData(AData: TtiObject; var pInclude: Boolean);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
  private
    FClients: TClients;
    procedure CreateTables;
    procedure DropTables;
  public
    { Public declarations }
  end;

var
  FormMainOrdinalTypes: TFormMainOrdinalTypes;

implementation
uses
  tiQuery
//  ,tiPersist
  ,tiDBConnectionPool
//  ,tiPtnVisPerObj_Cli
  ,tiOID
  ,tiOIDGUID
  ,tiOPFManager
//  ,tiPtnVisPerObj
  ,FClientEdit
  ,tiGUIUtils
  ,tiDialogs
 ;


{$R *.dfm}

// Create table
procedure TFormMainOrdinalTypes.CreateTables;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lTableMetaData.AddInstance('OID',          qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddInstance('Client_Name',  qfkString, 200);
    lTableMetaData.AddInstance('Sex',          qfkString,   7);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
end;

// Drop table
procedure TFormMainOrdinalTypes.DropTables;
begin
  try GTIOPFManager.DropTable('Client') except end;
end;

procedure TFormMainOrdinalTypes.FormCreate(Sender: TObject);
begin
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  LV.AddColumn('ClientName',     vttkString, 'Client name', 200);
  LV.AddColumn('SexAsGUIString', vttkString, 'Sex', 80);

  FClients:= TClients.Create;

  if tiAppConfirmation('Do you want to drop and re-create the tables?') then
  begin
    DropTables;
    CreateTables;
  end;
  aReadExecute(nil);
  
end;

// Insert a row
procedure TFormMainOrdinalTypes.btnInsertRowClick(Sender: TObject);
begin
end;

// Clear GUI and get a new OID

// Delete a row
procedure TFormMainOrdinalTypes.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TFormMainOrdinalTypes.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormMainOrdinalTypes.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainOrdinalTypes.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
//  aRead.Enabled:= aSave.Enabled;
end;

procedure TFormMainOrdinalTypes.aReadExecute(Sender: TObject);
begin
  LV.Data:= nil;
  FClients.Clear;
  FClients.Read;
  LV.Data:= FClients;
end;

procedure TFormMainOrdinalTypes.LVFilterData(AData: TtiObject;
  var pInclude: Boolean);
begin
  pInclude:= not AData.Deleted;
end;

procedure TFormMainOrdinalTypes.LVItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(AData) then
    lv.Refresh;
end;

procedure TFormMainOrdinalTypes.LVItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  TFormClientEdit.Execute(AData);
end;

procedure TFormMainOrdinalTypes.LVItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
var
  LClient: TClient;
begin
  LClient:= TClient.CreateNew;
  if TFormClientEdit.Execute(LClient) then
  begin
    FClients.Add(LClient);
    lv.Refresh(LClient);
  end
  else
    LClient.Free;
end;



end.
