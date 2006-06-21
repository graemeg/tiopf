unit FMainOrdinalTypes;

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
  TFormMainOrdinalTypes = class(TForm)
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
  FormMainOrdinalTypes: TFormMainOrdinalTypes;

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
procedure TFormMainOrdinalTypes.CreateTables;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',          qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',  qfkString, 200 ) ;
    lTableMetaData.AddField( 'Sex',          qfkString,   7 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
end;

// Drop table
procedure TFormMainOrdinalTypes.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client' ) except end ;
end;

procedure TFormMainOrdinalTypes.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  LV.AddColumn('ClientName',     lvtkString, 'Client name', 200 );
  LV.AddColumn('SexAsGUIString', lvtkString, 'Sex', 80 );

  FClients := TClients.Create ;

  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end ;
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
  FClients.Save ;
end;

procedure TFormMainOrdinalTypes.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainOrdinalTypes.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TFormMainOrdinalTypes.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
  aRead.Enabled := aSave.Enabled;
end;

procedure TFormMainOrdinalTypes.aReadExecute(Sender: TObject);
begin
  LV.Data := nil;
  FClients.Clear ;
  FClients.Read ;
  LV.Data := FClients.List ;
end;

procedure TFormMainOrdinalTypes.LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lData : TClient ;
begin
  lData := pData as TClient ;
  if TFormClientEdit.Execute(lData) then
    lData.Save;
end;

procedure TFormMainOrdinalTypes.LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
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

procedure TFormMainOrdinalTypes.LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lData : TClient ;
begin
  lData := pData as TClient ;
  if tiPerObjAbsConfirmAndDelete(lData) then
    lData.Save ;
end;

procedure TFormMainOrdinalTypes.LVFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end;

end.
