unit FMainInheritance;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, tiPerAwareCtrls, ExtCtrls, tiFocusPanel,
  tiListView, ComCtrls
  {$IFDEF DELPHI6ORABOVE}
  Variants,
  {$ENDIF}
  ,Client_BOM
  ;

type
  TFormMainInheritance = class(TForm)
    lvClient: TtiListView;
    paeClientCount: TtiPerAwareFloatEdit;
    btnInsert: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    aRead: TAction;
    Button2: TButton;
    Button1: TButton;
    btnRead: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aReadExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure lvClientItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvClientItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvClientItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure Button2Click(Sender: TObject);
    procedure lvClientFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure btnInsertClick(Sender: TObject);
  private
    FClients : TClients ;
  public
    procedure CreateTables;
    procedure DropTables;
  end;

var
  FormMainInheritance: TFormMainInheritance;

implementation
uses
  tiQuery
  ,tiPersist
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDGUID
  ,tiDialogs
  ,FClientAbsEdit
  ,tiUtils
  ;

{$R *.dfm}

procedure TFormMainInheritance.FormCreate(Sender: TObject);
begin
  paeClientCount.Value := 1000 ;
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  lvClient.AddColumn('ClientType', lvtkString, 'Client Type', 80);
  lvClient.AddColumn('ClientID', lvtkString, 'Client ID', 80);
  lvClient.AddColumn('Caption',  lvtkString, 'Client name', 200 );

  FClients := TClients.Create ;
  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end else
    aReadExecute(nil);

  lvClient.Data := FClients.List ;
end;

procedure TFormMainInheritance.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainInheritance.aSaveExecute(Sender: TObject);
begin
  FClients.Save ;
end;

procedure TFormMainInheritance.aReadExecute(Sender: TObject);
begin
  lvClient.Data := nil;
  FClients.Clear ;
  FClients.Read ;
  lvClient.Data := FClients.List ;
end;

procedure TFormMainInheritance.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
end;

procedure TFormMainInheritance.CreateTables;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client_Abs' ;
    lTableMetaData.AddField( 'OID',               qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_ID',         qfkString,   9 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client_Company' ;
    lTableMetaData.AddField( 'OID',                 qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Company_Name',         qfkString, 200 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client_Person' ;
    lTableMetaData.AddField( 'OID',                 qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Given_Name',          qfkString,  40 ) ;
    lTableMetaData.AddField( 'Family_Name',         qfkString,  40 ) ;
    lTableMetaData.AddField( 'Name_Title',          qfkString,  12 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

end;

procedure TFormMainInheritance.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client_Abs' ) except end ;
  try gTIPerMgr.DropTable( 'Client_Company' ) except end ;
  try gTIPerMgr.DropTable( 'Client_Person' ) except end ;
end;

procedure TFormMainInheritance.lvClientItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClientAbs ;
  lToCreate : string ;
const
  cCompany = '&Company' ;
  cPerson = '&Person' ;
  cCancel = 'C&ancel' ;
begin
  // Better as a factory...
  lToCreate :=
    tiMessageDlg( 'Do you want to create a company clent or a person client?',
                  [cCompany, cPerson, cCancel] ) ;
  if lToCreate = cCancel then
    Exit ; //==>
  if lToCreate = cPerson then
    lClient := TClientPerson.CreateNew
  else if lToCreate = cCompany then
    lClient := TClientCompany.CreateNew
  else
  begin
    lClient := nil ;
    tiFmtException('Invalid class to create <' + lToCreate + '>',
                   ClassName, 'lvClientItemInsert' ) ;
  end ;
  FClients.Add(lClient);
  if EditClient(lClient) then
    lClient.Save
  else
    FClients.Remove(lClient);
end;

procedure TFormMainInheritance.lvClientItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClientAbs ;
begin
  lClient := pData as TClientAbs ;
  if EditClient(lClient) then
    lClient.Save ;
end;

procedure TFormMainInheritance.lvClientItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClientAbs ;
begin
  lClient := pData as TClientAbs ;
  if tiPerObjAbsConfirmAndDelete(lClient) then
    lClient.Save ;
end;

procedure TFormMainInheritance.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainInheritance.lvClientFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TClientAbs ).Deleted ;
end;

procedure TFormMainInheritance.btnInsertClick(Sender: TObject);
var
  i : integer ;
  lClientCompany : TClientCompany ;
  lClientPerson : TClientPerson ;
begin
  for i := 1 to Trunc(paeClientCount.Value) do
  begin
    lClientCompany             := TClientCompany.CreateNew ;
    lClientCompany.ClientID    := IntToStr(i);
    lClientCompany.CompanyName := lClientCompany.ClientID ;
    FClients.Add(lClientCompany);

    lClientPerson            := TClientPerson.CreateNew ;
    lClientPerson.ClientID   := IntToStr(i);
    lClientPerson.NameTitle  := 'MR' ;
    lClientPerson.GivenName  := lClientPerson.ClientID ;
    lClientPerson.FamilyName := lClientPerson.ClientID ;
    FClients.Add(lClientPerson);
  end ;
  FClients.Save ;
  lvClient.Refresh ;
end;

end.
