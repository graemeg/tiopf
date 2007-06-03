unit FMainOneToOne;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, tiPerAwareCtrls, ExtCtrls, ComCtrls
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,Client_BOM, tiVTListView, tiObject, tiFocusPanel, tiVirtualTrees
  ;

type
  TFormMainInheritance = class(TForm)
    ActionList1: TActionList;
    aSave: TAction;
    aRead: TAction;
    Button2: TButton;
    Button1: TButton;
    btnRead: TButton;
    lvClient: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aReadExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);

    procedure Button2Click(Sender: TObject);
    procedure lvClientFilterData(pData: TtiObject; var pInclude: Boolean);
    procedure lvClientItemEdit(pVT: TtiCustomVirtualTree; pData: TtiObject;
      pItem: PVirtualNode);
    procedure lvClientItemInsert(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
    procedure lvClientItemDelete(pVT: TtiCustomVirtualTree;
      pData: TtiObject; pItem: PVirtualNode);
  private
    FClients : TClients ;
    procedure lvClientListDeriveAdrs(const pVT : TtiCustomVirtualTree ;
                                     const pData : TtiObject ;
                                     const ptiListColumn : TtiVTColumn ;
                                     var   pResult : string );
  public
    procedure CreateTables;
    procedure DropTables;
  end;

var
  FormMainInheritance: TFormMainInheritance;

implementation
uses
  tiQuery
  ,tiOPFManager
  ,tiDialogs
  ,tiConstants
  ,FClientEdit
  ,tiUtils
  ,tiGUIUtils
  ;

{$R *.dfm}

procedure TFormMainInheritance.FormCreate(Sender: TObject);
begin

  Caption := 'Connected to ' + gTIOPFManager.DefaultDBConnectionName ;
  lvClient.AddColumn('ClientName',  vttkString, 'Client name', 200 );
  lvClient.AddColumn(lvClientListDeriveAdrs, 'Address', 200);

  FClients := TClients.Create ;
  if tiAppConfirmation('Do you want to drop and re-create the tables?' ) then
  begin
    DropTables;
    CreateTables ;
  end else
    aReadExecute(nil);

  lvClient.Data := FClients ;
end;

procedure TFormMainInheritance.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormMainInheritance.lvClientListDeriveAdrs(
  const pVT: TtiCustomVirtualTree; const pData: TtiObject;
  const ptiListColumn: TtiVTColumn; var pResult: string);
begin
  Assert(pData.TestValid(TClient), cTIInvalidObjectError);
  pResult := ( pData as TClient ).Adrs.AsOneLine ;
end;

procedure TFormMainInheritance.lvClientFilterData(pData: TtiObject;
  var pInclude: Boolean);
begin
  Assert(pData.TestValid, cTIInvalidObjectError);
  pInclude := not pData.Deleted ;
end;

procedure TFormMainInheritance.lvClientItemDelete(
  pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
var
  lClient : TClient ;
begin
  lClient := pData as TClient ;
  if tiPerObjAbsConfirmAndDelete(lClient) then
    lvClient.Refresh;
end;

procedure TFormMainInheritance.lvClientItemEdit(pVT: TtiCustomVirtualTree;
  pData: TtiObject; pItem: PVirtualNode);
var
  LClient : TClient ;
begin
  Assert(pData.TestValid(TClient), cTIInvalidObjectError);
  LClient := pData as TClient ;
  if TFormClientEdit.Execute(LClient) then
    lvClient.Refresh(LClient);
end;

procedure TFormMainInheritance.lvClientItemInsert(
  pVT: TtiCustomVirtualTree; pData: TtiObject; pItem: PVirtualNode);
var
  lClient : TClient ;
begin
  lClient := TClient.CreateNew;
  LClient.Owner:= FClients;
  if TFormClientEdit.Execute(lClient) then
  begin
    FClients.Add(LClient);
    lvClient.Refresh(LClient);
  end
  else
    LClient.Free;
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
  lvClient.Data := FClients ;
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
    gTIOPFManager.CreateTable( lTableMetaData ) ;
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
    gTIOPFManager.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
  
end;

procedure TFormMainInheritance.DropTables;
begin
  try gTIOPFManager.DropTable( 'Client' ) except end ;
  try gTIOPFManager.DropTable( 'Adrs' ) except end ;
end;

procedure TFormMainInheritance.Button2Click(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

end.
