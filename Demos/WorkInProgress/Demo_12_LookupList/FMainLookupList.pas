unit FMainLookupList;

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
  TFormMainLookupList = class(TForm)
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    Button3: TButton;
    aRead: TAction;
    LV: TtiListView;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVFilterData(pData: TPersistent; var pbInclude: Boolean);
  private
    FClients : TClients ;
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
  ,tiPersist
  ,tiDBConnectionPool
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDAbs
  ,tiPerObjOIDGUID
  ,tiPtnVisPerObj
  ,FClientEdit
  ,tiDialogs
  ;

{$R *.dfm}

// Create table
procedure TFormMainLookupList.CreateTables;
  procedure _AddClientSource(const pDisplayText : string ) ;
  var
    lClientSource : TClientSource ;
  begin
    lClientSource := TClientSource.CreateNew ;
    lClientSource.DisplayText := pDisplayText ;
    gClientSources.Add(lClientSource);
  end ;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client_Source' ;
    lTableMetaData.AddField( 'OID',          qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Display_Text',  qfkString, 20 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;


  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',           qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',   qfkString, 200 ) ;
    lTableMetaData.AddField( 'Client_Source', qfkString,  36 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

  _AddClientSource( 'Unknown' ) ;
  _AddClientSource( 'Yellow pages' ) ;
  _AddClientSource( 'Referal' ) ;
  _AddClientSource( 'Radio' ) ;
  _AddClientSource( 'TV' ) ;
  _AddClientSource( 'News paper' ) ;
  gClientSources.Save ;

end;

// Drop table
procedure TFormMainLookupList.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client' ) except end ;
  try gTIPerMgr.DropTable( 'Client_Source' ) except end ;
end;

procedure TFormMainLookupList.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  LV.AddColumn('ClientName',     lvtkString, 'Client name', 200 );
  LV.AddColumn('ClientSourceAsGUIString', lvtkString, 'Client source', 80 );

  FClients := TClients.Create ;

  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end ;

  gClientSources ; // This will force the pre-loading of ClientSources

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
  FClients.Save ;
end;

procedure TFormMainLookupList.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainLookupList.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainLookupList.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
  aRead.Enabled := aSave.Enabled;
end;

procedure TFormMainLookupList.aReadExecute(Sender: TObject);
begin
  LV.Data := nil;
  FClients.Clear ;
  FClients.Read ;
  LV.Data := FClients.List ;
end;

procedure TFormMainLookupList.LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lData : TClient ;
begin
  lData := pData as TClient ;
  if TFormClientEdit.Execute(lData) then
    lData.Save;
end;

procedure TFormMainLookupList.LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew ;
  if TFormClientEdit.Execute(lClient) then
  begin
    FClients.Add(lClient);
    lClient.Save;
  end 
  else
    lClient.Free ;
end;

procedure TFormMainLookupList.LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lData : TClient ;
begin
  lData := pData as TClient ;
  if tiPerObjAbsConfirmAndDelete(lData) then
    lData.Save ;
end;

procedure TFormMainLookupList.LVFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end;

end.
