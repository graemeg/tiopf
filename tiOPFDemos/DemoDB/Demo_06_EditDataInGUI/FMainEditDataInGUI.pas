unit FMainEditDataInGUI;

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
  TForm2 = class(TForm)
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    Button2: TButton;
    Button3: TButton;
    aUndo: TAction;
    LV: TtiListView;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aUndoExecute(Sender: TObject);
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
  Form2: TForm2;

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
procedure TForm2.CreateTables;
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
end;

// Drop table
procedure TForm2.DropTables;
begin
  try gTIPerMgr.DropTable( 'Client' ) except end ;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  LV.AddColumn('ClientID',   lvtkString, 'Client ID', 80);
  LV.AddColumn('ClientName', lvtkString, 'Client name', 200 );

  FClients := TClients.Create ;

  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end else
    FClients.Read ;

  LV.Data := FClients.List ;
  
end;

// Insert a row
procedure TForm2.btnInsertRowClick(Sender: TObject);
begin
end;

// Clear GUI and get a new OID

// Delete a row
procedure TForm2.aSaveExecute(Sender: TObject);
begin
  FClients.Save ;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  tiShowPerObjAbs(FClients, true);
end;

procedure TForm2.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
  aUndo.Enabled := aSave.Enabled;
end;

procedure TForm2.aUndoExecute(Sender: TObject);
begin
  LV.Data := nil;
  FClients.Clear ;
  FClients.Read ;
  LV.Data := FClients.List ;
end;

procedure TForm2.LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
begin
  TFormClientEdit.Execute(pData as TPerObjAbs);
end;

procedure TForm2.LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew ;
  if TFormClientEdit.Execute(lClient) then
    FClients.Add(lClient)
  else
    lClient.Free ;
end;

procedure TForm2.LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;pItem: TListItem);
begin
  tiPerObjAbsConfirmAndDelete(pData as TClient);
end;

procedure TForm2.LVFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end;

end.
