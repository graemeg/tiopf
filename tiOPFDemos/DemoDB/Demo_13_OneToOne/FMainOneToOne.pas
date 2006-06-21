unit FMainOneToOne;

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
  private
    FClients : TClients ;
    procedure lvClientListDeriveAdrs(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
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
  ,FClientEdit
  ,tiUtils
  ;

{$R *.dfm}

procedure TFormMainInheritance.FormCreate(Sender: TObject);
var
  lCol : TtiListColumn ;
begin

  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  lvClient.AddColumn('ClientName',  lvtkString, 'Client name', 200 );
  lCol := lvClient.ListColumns.Add ;
  lCol.Derived := true ;
  lCol.Width := 100 ;
  lCol.DisplayLabel := 'Address' ;
  lCol.OnDeriveColumn := lvClientListDeriveAdrs ;

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
  FClients[0].Read;
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
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',               qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',       qfkString,  200 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Adrs' ;
    lTableMetaData.AddField( 'OID',       qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Adrs_Text', qfkString, 240 ) ;
    lTableMetaData.AddField( 'Locality',  qfkString,  46 ) ;
    lTableMetaData.AddField( 'State',     qfkString,   3 ) ;
    lTableMetaData.AddField( 'Post_Code', qfkString,   4 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
  
end;

procedure TFormMainInheritance.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client' ) except end ;
  try gTIPerMgr.DropTable( 'Adrs' ) except end ;
end;

procedure TFormMainInheritance.lvClientItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew;
  FClients.Add(lClient);
  if TFormClientEdit.Execute(lClient) then
    lClient.Save
  else
    FClients.Remove(lClient);
end;

procedure TFormMainInheritance.lvClientItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := pData as TClient ;
  if TFormClientEdit.Execute(lClient) then
    lClient.Save ;
end;

procedure TFormMainInheritance.lvClientItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := pData as TClient ;
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
  pbInclude := not ( pData as TClient ).Deleted ;
end;

procedure TFormMainInheritance.lvClientListDeriveAdrs(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  pResult := ( pData as TClient ).Adrs.AsOneLine ;
end;

end.
