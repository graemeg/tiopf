unit FMainInheritance;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, tiPerAwareCtrls, ExtCtrls, tiFocusPanel,
  ComCtrls, tiVTListView, Variants, Client_BOM, tiVirtualTrees, tiObject,
  tiVTAbstract;

type
  TFormMainInheritance = class(TForm)
    paeClientCount: TtiPerAwareFloatEdit;
    btnInsert: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    aRead: TAction;
    Button2: TButton;
    Button1: TButton;
    btnRead: TButton;
    lvClients: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aReadExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);

    procedure Button2Click(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure lvClientsItemDelete(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure lvClientsItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvClientsItemInsert(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
  private
    FClients: TClients;
  public
    procedure CreateTables;
    procedure DropTables;
  end;

var
  FormMainInheritance: TFormMainInheritance;

implementation
uses
   tiQuery
  ,tiOPFManager
  ,tiConstants
  ,tiDialogs
  ,FClientAbsEdit
  ,tiUtils
  ,tiGUIUtils
 ;

{$R *.dfm}

procedure TFormMainInheritance.FormCreate(Sender: TObject);
begin
  paeClientCount.Value:= 10;
  Caption:= 'Connected to ' + GTIOPFManager.DefaultDBConnectionName;
  lvClients.AddColumn('ClientType', vttkString, 'Client Type', 80);
  lvClients.AddColumn('ClientID', vttkString, 'Client ID', 80);
  lvClients.AddColumn('Caption',  vttkString, 'Client name', 250);

  FClients:= TClients.Create;
  if tiAppConfirmation('Do you want to drop and re-create the tables?') then
  begin
    DropTables;
    CreateTables;
  end else
    aReadExecute(nil);

  lvClients.Data:= FClients;
end;

procedure TFormMainInheritance.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormMainInheritance.lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  pInclude:= not pData.Deleted;
end;

procedure TFormMainInheritance.lvClientsItemDelete(
  pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  if tiPerObjAbsConfirmAndDelete(pData) then
    lvClients.Refresh;
end;

procedure TFormMainInheritance.lvClientsItemEdit(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  LClient: TClientAbs;
begin
  LClient:= pData as TClientAbs;
  if EditClient(LClient) then
    lvClients.Refresh(LClient);
end;

procedure TFormMainInheritance.lvClientsItemInsert(
  pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
var
  LClient: TClientAbs;
  LToCreate: string;
const
  cCompany = '&Company';
  cPerson = '&Person';
  cCancel = 'C&ancel';
begin
  // Better as a factory...
  LToCreate:=
    tiMessageDlg('Do you want to create a company clent or a person client?',
                  [cCompany, cPerson, cCancel]);
  if LToCreate = cCancel then
    Exit; //==>
  if LToCreate = cPerson then
    LClient:= TClientPerson.CreateNew
  else if LToCreate = cCompany then
    LClient:= TClientCompany.CreateNew
  else
  begin
    raise Exception.CreateFmt('Invalid class to create "%s"', [LToCreate]);
  end;

  LClient.Owner:= FClients;
  FClients.Add(LClient);
  if EditClient(LClient) then
    lvClients.Refresh(LClient)
  else
    LClient.Free;
end;

procedure TFormMainInheritance.aSaveExecute(Sender: TObject);
begin
  FClients.Save;
end;

procedure TFormMainInheritance.aReadExecute(Sender: TObject);
begin
  lvClients.Data:= nil;
  FClients.Clear;
  FClients.Read;
  lvClients.Data:= FClients;
end;

procedure TFormMainInheritance.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aSave.Enabled:= FClients.Dirty;
end;

procedure TFormMainInheritance.CreateTables;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client_Abs';
    lTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Client_ID',         qfkString,   9);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client_Company';
    lTableMetaData.AddField('OID',                 qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Company_Name',         qfkString, 200);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client_Person';
    lTableMetaData.AddField('OID',                 qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Given_Name',          qfkString,  40);
    lTableMetaData.AddField('Family_Name',         qfkString,  40);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;

end;

procedure TFormMainInheritance.DropTables;
begin
  try GTIOPFManager.DropTable('Client_Abs') except end;
  try GTIOPFManager.DropTable('Client_Company') except end;
  try GTIOPFManager.DropTable('Client_Person') except end;
end;

procedure TFormMainInheritance.Button2Click(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

procedure TFormMainInheritance.btnInsertClick(Sender: TObject);
var
  i: integer;
  lClientCompany: TClientCompany;
  lClientPerson: TClientPerson;
begin
  i:= 1;
  while i <= Trunc(paeClientCount.Value) do
  begin
    lClientCompany            := TClientCompany.CreateNew;
    lClientCompany.ClientID   := IntToStr(i);
    lClientCompany.CompanyName:= lClientCompany.ClientID;
    FClients.Add(lClientCompany);

    lClientPerson           := TClientPerson.CreateNew;
    lClientPerson.ClientID  := IntToStr(i+1);
    lClientPerson.GivenName := lClientPerson.ClientID;
    lClientPerson.FamilyName:= lClientPerson.ClientID;
    FClients.Add(lClientPerson);
    Inc(i, 2);
  end;
  FClients.Save;
  lvClients.Refresh;
end;

end.
