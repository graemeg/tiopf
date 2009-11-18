unit FMainOneToMany;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LResources,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Grids,
  Client_BOM, ComCtrls, tiObject, tiModelMediator;

const
  // Database file name
  cDatabase = 'testdb.xml';

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
    paeClientCount: TEdit;
    paePhoneNumberCount: TEdit;
    btnInsert: TButton;
    lvClients: TStringGrid;
    lvPhoneNumbers: TStringGrid;
    aInsertClients: TAction;
    aShowObjects: TAction;
    procedure FormCreate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
    //procedure lvClientsItemArrive(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvClientsItemLeave(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvPhoneNumbersCanInsert(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode; var pCanPerformAction: Boolean);
    //procedure lvClientsItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvClientsItemInsert(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvClientsItemDelete(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvPhoneNumbersItemDelete(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvPhoneNumbersItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    //procedure lvPhoneNumbersItemInsert(pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
    procedure aShowObjectsExecute(Sender: TObject);
    procedure aInsertClientsExecute(Sender: TObject);
    procedure lvClientsSelection(Sender: TObject; aCol, aRow: Integer);
  private
    FClients: TClients;
    FClientMediator: TtiModelMediator;
    FPhoneMediator: TtiModelMediator;
    procedure CreateTables;
    procedure DropTables;
    procedure SetupMediators;
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
  ,tiListMediators
 ;


// Create table
procedure TFormMainOneToMany.CreateTables;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  gTIOPFManager.DefaultPerLayer.CreateDatabase(cDatabase, '', '');
  gTIOPFManager.TestThenConnectDatabase(cDatabase, '', '');

  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'client';
    lTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Client_Name',       qfkString, 200);
    lTableMetaData.AddField('Client_ID',         qfkString,   9);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'phone_number';
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
  if FileExists(cDatabase) then
    DeleteFile(cDatabase);
  //try GTIOPFManager.DropTable('Client') except end;
  //try GTIOPFManager.DropTable('Phone_Number') except end;
end;

procedure TFormMainOneToMany.SetupMediators;
begin
  if not Assigned(FClientMediator) then
  begin
    FClientMediator := TtiModelMediator.Create(self);
    FClientMediator.AddComposite('ClientID(80);ClientName(200)', lvClients);
  end;
  FClientMediator.Subject := FClients;
  FClientMediator.Active := True;

  if not Assigned(FPhoneMediator) then
  begin
    FPhoneMediator := TtiModelMediator.Create(self);
    FPhoneMediator.AddProperty('NumberType(80);NumberText(200)', lvPhoneNumbers);
  end;
  if FClients.Count > 0 then
  begin
    FPhoneMediator.Subject := TClient(FClients[0]).PhoneNumbers;
    FPhoneMediator.Active := True;
  end;
end;

procedure TFormMainOneToMany.FormCreate(Sender: TObject);
begin
  if tiAppConfirmation('Do you want to drop and recreate the tables required ' +
    'for this demo?' + tiLineEnd(2) +
    '(Select "Yes" if this is the first time you have run the demo.)') then
  begin
    DropTables;
    CreateTables;
  end
  else
    gTIOPFManager.TestThenConnectDatabase(cDatabase, '', '');

  
  Caption := 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  paeClientCount.Text      := '10';
  paePhoneNumberCount.Text := '2';

  FClients:= TClients.Create;
  aReadExecute(nil);
  SetupMediators;
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
  FPhoneMediator.Active := False;
  FClientMediator.Active := False;
  FClients.Free;
end;

procedure TFormMainOneToMany.lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  pInclude:= not pData.Deleted;
end;

//procedure TFormMainOneToMany.lvClientsItemArrive(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //lvPhoneNumbers.Data:= (pData as TClient).PhoneNumbers;
//end;

//procedure TFormMainOneToMany.lvClientsItemDelete(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //if tiPerObjAbsConfirmAndDelete(pData) then
    //lvClients.Refresh;
//end;

//procedure TFormMainOneToMany.lvClientsItemEdit(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //Assert(pData.TestValid, CTIErrorInvalidObject);
  //TFormClientEdit.Execute(pData);
//end;

//procedure TFormMainOneToMany.lvClientsItemInsert(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//var
  //LClient: TClient;
//begin
  //LClient:= TClient.CreateNew;
  //if TFormClientEdit.Execute(LClient) then
  //begin
    //FClients.Add(LClient);
    //lvClients.Refresh(LClient);
  //end
  //else
    //LClient.Free;
//end;

//procedure TFormMainOneToMany.lvClientsItemLeave(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //lvPhoneNumbers.Data:= nil;
//end;

//procedure TFormMainOneToMany.lvPhoneNumbersCanInsert(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode; var pCanPerformAction: Boolean);
//begin
  //pCanPerformAction:= lvClients.SelectedData <> nil;
//end;

//procedure TFormMainOneToMany.lvPhoneNumbersItemDelete(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //if tiPerObjAbsConfirmAndDelete(pData) then
    //lvPhoneNumbers.Refresh;
//end;

//procedure TFormMainOneToMany.lvPhoneNumbersItemEdit(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//begin
  //Assert(pData.TestValid, CTIErrorInvalidObject);
  //TFormPhoneNumberEdit.Execute(pData);
//end;

//procedure TFormMainOneToMany.lvPhoneNumbersItemInsert(pVT: TtiCustomVirtualTree;
  //pData: TtiObject; pItem: PVirtualNode);
//var
  //LPhoneNumber: TPhoneNumber;
  //LClient: TClient;
//begin
  //LClient:= lvClients.SelectedData as TClient;
  //LPhoneNumber:= TPhoneNumber.CreateNew;
  //if TFormPhoneNumberEdit.Execute(LPhoneNumber) then
  //begin
    //LClient.PhoneNumbers.Add(LPhoneNumber);
    //lvPhoneNumbers.Refresh(LPhoneNumber);
  //end
  //else
    //LPhoneNumber.Free;
//end;

procedure TFormMainOneToMany.ActionList1Update(AAction: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
end;

procedure TFormMainOneToMany.aInsertClientsExecute(Sender: TObject);
var
  i, j: integer;
  lClient: TClient;
  lPhoneNumber: TPhoneNumber;
begin
  for i:= 1 to StrToInt(paeClientCount.Text) do
  begin
    lClient:= TClient.CreateNew;
    lClient.ClientName:= 'ClientName' + IntToStr(i);
    lClient.ClientID  := 'ID' +  IntToStr(i);
    FClients.Add(lClient);
    for j:= 1 to StrToInt(paePhoneNumberCount.Text) do
    begin
      lPhoneNumber:= TPhoneNumber.CreateNew;
      lPhoneNumber.NumberType:= 'Type' + IntToStr(i) + '.' + IntToStr(j);
      lPhoneNumber.NumberText:= 'NumberText' + IntToStr(j);
      lClient.PhoneNumbers.Add(lPhoneNumber);
    end;
  end;
  FClients.Save;
  lvClients.Refresh;
end;

procedure TFormMainOneToMany.lvClientsSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  FPhoneMediator.Active := False;
  FPhoneMediator.Subject := TClient(FClients[lvClients.Row-1]).PhoneNumbers;
  FPhoneMediator.Active := True;
end;

procedure TFormMainOneToMany.aReadExecute(Sender: TObject);
begin
  FClients.Clear;
  FClients.Read;
end;

initialization
  {$I FMainOneToMany.lrs}
  RegisterFallBackListmediators;

end.