unit FMainOneToMany;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, ComCtrls, tiVirtualTrees, tiVTListView, tiObject, Grids, DBGrids,
  DB, tiDataset
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
    Label1: TLabel;
    Label2: TLabel;
    paeClientCount: TtiPerAwareFloatEdit;
    paePhoneNumberCount: TtiPerAwareFloatEdit;
    btnInsert: TButton;
    aInsertClients: TAction;
    aShowObjects: TAction;
    DatasetClients_: TTiDataset;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DatasetClients_ClientId: TStringField;
    DatasetClients_ClientName: TStringField;
    NestedDataset_PhoneNumbers: TTiNestedDataset;
    DataSource2: TDataSource;
    DBGrid2: TDBGrid;
    DatasetClients_PhoneNumbers: TDataSetField;
    NestedDataset_PhoneNumbersNumberType: TStringField;
    NestedDataset_PhoneNumbersNumberText: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure aReadExecute(Sender: TObject);
    procedure lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
    procedure aShowObjectsExecute(Sender: TObject);
    procedure aInsertClientsExecute(Sender: TObject);
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
  ,tiOPFManager
  ,tiConstants
  ,tiUtils
  ,tiDialogs
  ,tiGUIUtils
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
    GTIOPFManager.CreateTable( lTableMetaData ) ;
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
    GTIOPFManager.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;

end;

// Drop table
procedure TFormMainOneToMany.DropTables;
begin
  try GTIOPFManager.DropTable( 'Client' ) except end ;
  try GTIOPFManager.DropTable( 'Phone_Number' ) except end ;
end;

procedure TFormMainOneToMany.FormCreate(Sender: TObject);
begin
  if tiAppConfirmation('Do you want to drop and recreate the tables required ' +
    'for this demo?' + CrLf(2) +
    '(Select "Yes" if this is the first time you have run the demo.)') then
  begin
    DropTables;
    CreateTables ;
  end;

  Caption := 'Connected to ' + GTIOPFManager.DefaultDBConnectionName ;

  paeClientCount.Value      := 10 ;
  paePhoneNumberCount.Value := 2 ;

  FClients := TClients.Create ;

  DatasetClients_.LinkObject(FClients, TClient);

  NestedDataset_PhoneNumbers.DataSetField:= DatasetClients_PhoneNumbers;
  NestedDataset_PhoneNumbers.ObjectClass:= TPhoneNumber;

//  TiNestedDataset1.
  aReadExecute(nil);


end;

procedure TFormMainOneToMany.aSaveExecute(Sender: TObject);
begin
  FClients.Save ;
end;

procedure TFormMainOneToMany.aShowObjectsExecute(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

procedure TFormMainOneToMany.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainOneToMany.lvClientsFilterData(pData: TtiObject; var pInclude: Boolean);
begin
  Assert(pData.TestValid, CTIErrorInvalidObject);
  pInclude := not pData.Deleted ;
end;

procedure TFormMainOneToMany.ActionList1Update(Action: TBasicAction;var Handled: Boolean);
begin
  aSave.Enabled := FClients.Dirty ;
end;

procedure TFormMainOneToMany.aInsertClientsExecute(Sender: TObject);
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
  DatasetClients_.Refresh;
end;

procedure TFormMainOneToMany.aReadExecute(Sender: TObject);
begin
  FClients.Clear ;
  FClients.Read ;
  if DatasetClients_.Active then
    DatasetClients_.Refresh
  else
    DatasetClients_.Open; 

end;

end.
