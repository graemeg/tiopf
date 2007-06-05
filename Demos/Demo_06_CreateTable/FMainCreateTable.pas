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
    lblConnectedTo: TLabel;
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnDropTableClick(Sender: TObject);
    procedure btnShowMetaDataClick(Sender: TObject);
    procedure btnTableExistsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  ,tiOPFManager
  ,tiObject
  ,tiDBConnectionPool
  ,tiOIDGUID
  ,tiDialogs
 ;

{$R *.dfm}

// Create table
procedure TFormMainCreateTable.btnCreateTableClick(Sender: TObject);
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddField('Client_Name',       qfkString, 200);
    lTableMetaData.AddField('ACN',               qfkString,   9);
    gTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
  ShowMessage('Table ''Client'' created');
end;

// Drop table
procedure TFormMainCreateTable.btnDropTableClick(Sender: TObject);
begin
  gTIOPFManager.DropTable('Client');
  ShowMessage('Table ''Client'' dropped');
end;

// Show table metadata
procedure TFormMainCreateTable.btnShowMetaDataClick(Sender: TObject);
var
  lTableMetaData: TtiDBMetaDataTable;
  lPooledDB     : TPooledDB;
  lDatabase     : TtiDatabase;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lPooledDB:= gTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      lDatabase:= lPooledDB.Database;
      lDatabase.ReadMetaDataFields(lTableMetaData);
      tiShowMessage(lTableMetaData.AsDebugString);
    finally
      gTIOPFManager.DefaultDBConnectionPool.UnLock(lPooledDB);
    end;
  finally
    lTableMetaData.Free;
  end;
end;

// Does a table exist?
procedure TFormMainCreateTable.btnTableExistsClick(Sender: TObject);
var
  lDBMetaData: TtiDBMetaData;
  lPooledDB  : TPooledDB;
  lDatabase  : TtiDatabase;
begin
  lDBMetaData:= TtiDBMetaData.Create;
  try
    lPooledDB:= gTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      lDatabase:= lPooledDB.Database;
      lDatabase.ReadMetaDataTables(lDBMetaData);
      if lDBMetaData.FindByTableName('Client') <> nil then
        ShowMessage('Table <Client> exists')
      else
        ShowMessage('Table <Client> does not exist');
    finally
      gTIOPFManager.DefaultDBConnectionPool.UnLock(lPooledDB);
    end;
  finally
    lDBMetaData.Free;
  end;
end;

procedure TFormMainCreateTable.FormCreate(Sender: TObject);
begin
  lblConnectedTo.Caption:= 'Connected to: ' + gTIOPFManager.DefaultDBConnectionName;
end;

end.
