unit FMainOneToMany;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, tiListView, ComCtrls
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
    lvClient: TtiListView;
    lvPhoneNumber: TtiListView;
    Label1: TLabel;
    Label2: TLabel;
    paeClientCount: TtiPerAwareFloatEdit;
    paePhoneNumberCount: TtiPerAwareFloatEdit;
    btnInsert: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure lvClientItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvClientItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvClientItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvClientFilterData(pData: TPersistent; var pbInclude: Boolean);
    procedure lvPhoneNumberItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvPhoneNumberItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvPhoneNumberItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvClientItemLeave(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvClientItemArive(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvPhoneNumberCanInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem;
      var pbCanPerformAction: Boolean);
    procedure btnInsertClick(Sender: TObject);
  private
    FClients : TClients ;
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
  ,tiPersist
  ,tiDBConnectionPool
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDAbs
  ,tiPerObjOIDGUID
  ,tiPtnVisPerObj
  ,FClientEdit
  ,FPhoneNumberEdit
  ,tiDialogs
  ;

{$R *.dfm}

// Create table
procedure TFormMainOneToMany.CreateTables;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',               qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',       qfkString, 200 ) ;
    lTableMetaData.AddField( 'Client_ID',         qfkString,   9 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Phone_Number' ;
    lTableMetaData.AddField( 'OID',               qfkString, 36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_OID',        qfkString, 36 ) ;
    lTableMetaData.AddField( 'Number_Type',       qfkString, 20 ) ;
    lTableMetaData.AddField( 'Number_Text',       qfkString, 19 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

end;

// Drop table
procedure TFormMainOneToMany.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client' ) except end ;
  try gTIPerMgr.DropTable( 'Phone_Number' ) except end ;
end;

procedure TFormMainOneToMany.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  lvClient.AddColumn('ClientID',   lvtkString, 'Client ID', 80);
  lvClient.AddColumn('ClientName', lvtkString, 'Client name', 200 );

  lvPhoneNumber.AddColumn('NumberType', lvtkString, 'Number type', 80 );
  lvPhoneNumber.AddColumn('NumberText', lvtkString, 'Number', 200);

  paeClientCount.Value      := 1000 ;
  paePhoneNumberCount.Value := 2 ;

  FClients := TClients.Create ;

  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end ;
  aReadExecute(nil);

end;

// Insert a row
procedure TFormMainOneToMany.btnInsertRowClick(Sender: TObject);
begin
end;

// Clear GUI and get a new OID

// Delete a row
procedure TFormMainOneToMany.aSaveExecute(Sender: TObject);
begin
  FClients.Save ;
end;

procedure TFormMainOneToMany.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainOneToMany.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainOneToMany.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
end;

procedure TFormMainOneToMany.aReadExecute(Sender: TObject);
begin
  lvClient.Data := nil;
  FClients.Clear ;
  FClients.Read ;
  lvClient.Data := FClients.List ;
end;

procedure TFormMainOneToMany.lvClientItemEdit(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
begin
  TFormClientEdit.Execute(pData as TPerObjAbs);
end;

procedure TFormMainOneToMany.lvClientItemInsert(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew ;
  if TFormClientEdit.Execute(lClient) then
    FClients.Add(lClient)
  else
    lClient.Free ;
end;

procedure TFormMainOneToMany.lvClientItemDelete(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete(pData as TClient) then
    lvPhoneNumber.Refresh ;
end;

procedure TFormMainOneToMany.lvClientFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end;

procedure TFormMainOneToMany.lvPhoneNumberItemDelete(pLV: TtiCustomListView;pData: TPersistent; pItem: TListItem);
begin
  tiPerObjAbsConfirmAndDelete(pData as TPhoneNumber);
end;

procedure TFormMainOneToMany.lvPhoneNumberItemEdit(pLV: TtiCustomListView;pData: TPersistent; pItem: TListItem);
begin
  TFormPhoneNumberEdit.Execute(pData as TPerObjAbs);
end;

procedure TFormMainOneToMany.lvPhoneNumberItemInsert(pLV: TtiCustomListView;pData: TPersistent; pItem: TListItem);
var
  lPhoneNumber : TPhoneNumber ;
  lClient : TClient ;
begin
  lClient := lvClient.SelectedData as TClient ;
  lPhoneNumber := TPhoneNumber.CreateNew ;
  if TFormPhoneNumberEdit.Execute(lPhoneNumber) then
    lClient.PhoneNumbers.Add(lPhoneNumber)
  else
    lPhoneNumber.Free ;
end;

procedure TFormMainOneToMany.lvClientItemLeave(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  lvPhoneNumber.Data := nil ;
end;

procedure TFormMainOneToMany.lvClientItemArive(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  lvPhoneNumber.Data := ( pData as TClient ).PhoneNumbers.List ;
end;

procedure TFormMainOneToMany.lvPhoneNumberCanInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem; var pbCanPerformAction: Boolean);
begin
  pbCanPerformAction := lvClient.SelectedData <> nil ;
end;

procedure TFormMainOneToMany.btnInsertClick(Sender: TObject);
var
  i, j : integer ;
  lClient : TClient ;
  lPhoneNumber : TPhoneNumber ;
begin
  for i := 1 to Trunc(paeClientCount.Value) do
  begin
    lClient := TClient.CreateNew ;
    lClient.ClientName := IntToStr(i);
    lClient.ClientID   := lClient.ClientName;
    FClients.Add(lClient);
    for j := 1 to Trunc(paePhoneNumberCount.Value) do
    begin
      lPhoneNumber := TPhoneNumber.CreateNew ;
      lPhoneNumber.NumberType := lClient.ClientName + '.' + IntToStr(j);
      lPhoneNumber.NumberText := lPhoneNumber.NumberType;
      lClient.PhoneNumbers.Add(lPhoneNumber);
    end ;
  end ;
  FClients.Save ;
  lvClient.Refresh ;
end;

end.
