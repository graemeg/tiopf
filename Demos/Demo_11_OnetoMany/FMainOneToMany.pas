unit FMainOneToMany;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, ComCtrls, tiVirtualTrees, tiVTListView, tiObject
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
 ;
  

type
  TFormMainOneToMany = class(TForm)
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    btnRead: TButton;
    aRead: TAction;
    Label1: TLabel;
    Label2: TLabel;
    paeClientCount: TtiPerAwareFloatEdit;
    paePhoneNumberCount: TtiPerAwareFloatEdit;
    btnInsert: TButton;
    lvClients: TtiVTListView;
    lvPhoneNumbers: TtiVTListView;
    aInsertClients: TAction;
    aShowObjects: TAction;
    procedure FormCreate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
    procedure lvClientsItemArrive(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvClientsItemLeave(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvPhoneNumbersCanInsert(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode; var pCanPerformAction: Boolean);
    procedure lvClientsItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvClientsItemInsert(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvClientsItemDelete(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvPhoneNumbersItemDelete(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure lvPhoneNumbersItemEdit(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure lvPhoneNumbersItemInsert(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure aShowObjectsExecute(Sender: TObject);
    procedure aInsertClientsExecute(Sender: TObject);
  private
    FClients: TClients;
    procedure CreateTables;
    procedure DropTables;
  public
    { Public declarations }
  end;

var
  FormMainOneToMany: TFormMainOneToMany;

implementation
uses
   tiQuery
  ,tiOPFManager
  ,tiConstants
  ,tiUtils
  ,FClientEdit
  ,FPhoneNumberEdit
  ,tiDialogs
  ,tiGUIUtils
 ;

{$R *.dfm}

// Create table
procedure TFormMainOneToMany.CreateTables;
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

  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Phone_Number';
    lTableMetaData.AddField('OID',               qfkString, 36); // Using GUID OIDs
    lTableMetaData.AddField('Client_OID',        qfkString, 36);
    lTableMetaData.AddField('Number_Type',       qfkString, 20);
    lTableMetaData.AddField('Number_Text',       qfkString, 19);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

end;

// Drop table
procedure TFormMainOneToMany.DropTables;
begin
  try GTIOPFManager.DropTable('Client') except end;
  try GTIOPFManager.DropTable('Phone_Number') except end;
end;

procedure TFormMainOneToMany.FormCreate(Sender: TObject);
begin
  if tiAppConfirmation('Do you want to drop and recreate the tables required ' +
    'for this demo?' + CrLf(2) +
    '(Select "Yes" if this is the first time you have run the demo.)') then
  begin
    DropTables;
    CreateTables;
  end;
  
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  lvClients.AddColumn('ClientID',   vttkString, 'Client ID', 80);
  lvClients.AddColumn('ClientName', vttkString, 'Client name', 200);

  lvPhoneNumbers.AddColumn('NumberType', vttkString, 'Number type', 80);
  lvPhoneNumbers.AddColumn('NumberText', vttkString, 'Number', 200);

  paeClientCount.Value     := 10;
  paePhoneNumberCount.Value:= 2;

  FClients:= TClients.Create;

  aReadExecute(nil);

end;

procedure TFormMainOneToMany.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TFormMainOneToMany.aShowObjectsExecute(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

procedure TFormMainOneToMany.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormMainOneToMany.lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  pInclude:= not pData.Deleted;
end;

procedure TFormMainOneToMany.lvClientsItemArrive(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  lvPhoneNumbers.Data:= (pData as TClient).PhoneNumbers;
end;

procedure TFormMainOneToMany.lvClientsItemDelete(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(pData) then
    lvClients.Refresh;
end;

procedure TFormMainOneToMany.lvClientsItemEdit(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  TFormClientEdit.Execute(pData);
end;

procedure TFormMainOneToMany.lvClientsItemInsert(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  LClient: TClient;
begin
  LClient:= TClient.CreateNew;
  if TFormClientEdit.Execute(LClient) then
  begin
    FClients.Add(LClient);
    lvClients.Refresh(LClient);
  end
  else
    LClient.Free;
end;

procedure TFormMainOneToMany.lvClientsItemLeave(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  lvPhoneNumbers.Data:= nil;
end;

procedure TFormMainOneToMany.lvPhoneNumbersCanInsert(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode; var pCanPerformAction: Boolean);
begin
  pCanPerformAction:= lvClients.SelectedData <> nil;
end;

procedure TFormMainOneToMany.lvPhoneNumbersItemDelete(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(pData) then
    lvPhoneNumbers.Refresh;
end;

procedure TFormMainOneToMany.lvPhoneNumbersItemEdit(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  TFormPhoneNumberEdit.Execute(pData);
end;

procedure TFormMainOneToMany.lvPhoneNumbersItemInsert(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  LPhoneNumber: TPhoneNumber;
  LClient: TClient;
begin
  LClient:= lvClients.SelectedData as TClient;
  LPhoneNumber:= TPhoneNumber.CreateNew;
  if TFormPhoneNumberEdit.Execute(LPhoneNumber) then
  begin
    LClient.PhoneNumbers.Add(LPhoneNumber);
    lvPhoneNumbers.Refresh(LPhoneNumber);
  end
  else
    LPhoneNumber.Free;
end;

procedure TFormMainOneToMany.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
end;

procedure TFormMainOneToMany.aInsertClientsExecute(Sender: TObject);
var
  i, j: integer;
  lClient: TClient;
  lPhoneNumber: TPhoneNumber;
begin
  for i:= 1 to Trunc(paeClientCount.Value) do
  begin
    lClient:= TClient.CreateNew;
    lClient.ClientName:= IntToStr(i);
    lClient.ClientID  := lClient.ClientName;
    FClients.Add(lClient);
    for j:= 1 to Trunc(paePhoneNumberCount.Value) do
    begin
      lPhoneNumber:= TPhoneNumber.CreateNew;
      lPhoneNumber.NumberType:= lClient.ClientName + '.' + IntToStr(j);
      lPhoneNumber.NumberText:= lPhoneNumber.NumberType;
      lClient.PhoneNumbers.Add(lPhoneNumber);
    end;
  end;
  FClients.Save;
  lvClients.Refresh;
end;

procedure TFormMainOneToMany.aReadExecute(Sender: TObject);
begin
  lvClients.Data:= nil;
  FClients.Clear;
  FClients.Read;
  lvClients.Data:= FClients;
end;

end.
