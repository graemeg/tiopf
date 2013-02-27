unit FMainInsertData;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiMemoReadOnly
  ;

type
  TForm2 = class(TForm)
    btnInsertRow: TButton;
    btnDeleteRow: TButton;
    btnClear: TButton;
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    paeClientID: TtiPerAwareEdit;
    tiMemoReadOnly1: TtiMemoReadOnly;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertRowClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteRowClick(Sender: TObject);
  private
    procedure CreateTable;
    procedure DropTable;
    function  TableExists: boolean ;
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
  ,tiDataSet_BOM
  ;

{$R *.dfm}

// Create table
procedure TForm2.CreateTable;
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
procedure TForm2.DropTable;
begin
  gTIPerMgr.DropTable( 'Client' );
end;

// Does a table exist?
function TForm2.TableExists;
var
  lDBMetaData : TtiDBMetaData ;
  lPooledDB   : TPooledDB ;
  lDatabase   : TtiDatabase ;
begin
  lDBMetaData := TtiDBMetaData.Create ;
  try
    lPooledDB := gTIPerMgr.DefaultDBConnectionPool.Lock ;
    try
      lDatabase := lPooledDB.Database ;
      lDatabase.ReadMetaDataTables(lDBMetaData);
      result := lDBMetaData.FindByTableName('Client') <> nil;
    finally
      gTIPerMgr.DefaultDBConnectionPool.UnLock(lPooledDB);
    end ;
  finally
    lDBMetaData.Free;
  end ;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := 'Connected to ' + gTIPerMgr.DefaultDBConnectionName ;
  // Drop and re-create to be sure we start with the correct structure
  if TableExists then
    DropTable;
  CreateTable ;
end;

// Insert a row
procedure TForm2.btnInsertRowClick(Sender: TObject);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString( 'OID', paeOID.Value );
    lParams.SetValueAsString( 'Client_Name', paeClientName.Value);
    lParams.SetValueAsString( 'Client_ID', paeClientID.Value );
    gTIPerMgr.InsertRow('Client', lParams);
  finally
    lParams.Free ;
  end ;
end;

// Clear GUI and get a new OID
procedure TForm2.btnClearClick(Sender: TObject);
var
  lOID : TOID;
begin
  lOID := gTIPerMgr.OIDFactory.CreateOID;
  try
    lOID.GetNextValue(gTIPerMgr.DefaultDBConnectionName,
                      gTIPerMgr.DefaultPerLayerName);
    paeOID.Value := lOID.AsString ;
    paeClientName.Value := '' ;
    paeClientID.Value := '' ;
  finally
    lOID.Free;
  end ;
end;

// Delete a row
procedure TForm2.btnDeleteRowClick(Sender: TObject);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString( 'OID', paeOID.Value );
    gTIPerMgr.DeleteRow('Client', lParams);
  finally
    lParams.Free ;
  end ;
end;

end.
