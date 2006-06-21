unit FMainCreateTable;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiMemoReadOnly
  ;


type
  TFormMainCreateTable = class(TForm)
    btnCreateTable: TButton;
    btnDropTable: TButton;
    btnShowMetaData: TButton;
    btnTableExists: TButton;
    tiMemoReadOnly1: TtiMemoReadOnly;
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnDropTableClick(Sender: TObject);
    procedure btnShowMetaDataClick(Sender: TObject);
    procedure btnTableExistsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMainCreateTable: TFormMainCreateTable;

implementation
uses
  tiQuery
  ,tiPersist
  ,tiDBConnectionPool
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDGUID
  ;

{$R *.dfm}

// Create table
procedure TFormMainCreateTable.btnCreateTableClick(Sender: TObject);
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lTableMetaData.AddField( 'OID',               qfkString,  36 ) ; // Using GUID OIDs
    lTableMetaData.AddField( 'Client_Name',       qfkString, 200 ) ;
    lTableMetaData.AddField( 'ACN',               qfkString,   9 ) ;
    gTIPerMgr.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
  ShowMessage('Table ''Client'' created');
end;

// Drop table
procedure TFormMainCreateTable.btnDropTableClick(Sender: TObject);
begin
  gTIPerMgr.DropTable( 'Client' );
  ShowMessage('Table ''Client'' dropped');
end;

// Show table metadata
procedure TFormMainCreateTable.btnShowMetaDataClick(Sender: TObject);
var
  lTableMetaData : TtiDBMetaDataTable ;
  lPooledDB      : TPooledDB ;
  lDatabase      : TtiDatabase ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := 'Client' ;
    lPooledDB := gTIPerMgr.DefaultDBConnectionPool.Lock ;
    try
      lDatabase := lPooledDB.Database ;
      lDatabase.ReadMetaDataFields(lTableMetaData);
      tiShowPerObjAbs(lTableMetaData);
    finally
      gTIPerMgr.DefaultDBConnectionPool.UnLock(lPooledDB);
    end ;
  finally
    lTableMetaData.Free;
  end ;
end;

// Does a table exist?
procedure TFormMainCreateTable.btnTableExistsClick(Sender: TObject);
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
      if lDBMetaData.FindByTableName('Client') <> nil then
        ShowMessage( 'Table <Client> exists' )
      else
        ShowMessage( 'Table <Client> does not exist' ) ;
    finally
      gTIPerMgr.DefaultDBConnectionPool.UnLock(lPooledDB);
    end ;
  finally
    lDBMetaData.Free;
  end ;
end;

end.
