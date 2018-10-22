unit FMainCreateTable;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Variants;

type
  TFormMainCreateTable = class(TForm)
    btnCreateTable: TButton;
    btnDropTable: TButton;
    btnShowMetaData: TButton;
    btnTableExists: TButton;
    Info: TLabel;
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
  LTableMetaData: TtiDBMetaDataTable;
begin
  LTableMetaData:= TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LTableMetaData.AddInstance('OID',               qfkString,  36); // Using GUID OIDs
    LTableMetaData.AddInstance('Client_Name',       qfkString, 200);
    LTableMetaData.AddInstance('ACN',               qfkString,   9);
    GTIOPFManager.CreateTable(LTableMetaData);
  finally
    LTableMetaData.Free;
  end;
  ShowMessage('Table ''Client'' created');
end;

// Drop table
procedure TFormMainCreateTable.btnDropTableClick(Sender: TObject);
begin
  GTIOPFManager.DropTable('Client');
  ShowMessage('Table ''Client'' dropped');
end;

// Show table metadata
procedure TFormMainCreateTable.btnShowMetaDataClick(Sender: TObject);
var
  LTableMetaData: TtiDBMetaDataTable;
  LDatabase     : TtiDatabase;
begin
  LTableMetaData:= TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LDatabase:= GTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataFields(LTableMetaData);
      tiShowMessage(LTableMetaData.AsDebugString);
    finally
      GTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LTableMetaData.Free;
  end;
end;

// Does a table exist?
procedure TFormMainCreateTable.btnTableExistsClick(Sender: TObject);
var
  LDBMetaData: TtiDBMetaData;
  LDatabase  : TtiDatabase;
begin
  LDBMetaData:= TtiDBMetaData.Create;
  try
    LDatabase:= GTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataTables(LDBMetaData);
      if LDBMetaData.FindByTableName('Client') <> nil then
        ShowMessage('Table <Client> exists')
      else
        ShowMessage('Table <Client> does not exist');
    finally
      GTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LDBMetaData.Free;
  end;
end;

procedure TFormMainCreateTable.FormCreate(Sender: TObject);
begin
  lblConnectedTo.Caption:= 'Connected to: ' + GTIOPFManager.DefaultDBConnectionName;
end;

end.
