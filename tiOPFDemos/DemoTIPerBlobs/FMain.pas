unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DemoTIPerBlobs_BOM, ComCtrls, tiPerAwareCtrls, ExtCtrls,
  tiListView, ActnList, Grids, DBGrids, Db, IBDatabase, IBCustomDataSet,
  IBTable, DBCtrls, Mask, IBQuery, tiFocusPanel ;

type
  TFormMain = class(TForm)
    PC: TPageControl;
    tsSetupDB: TTabSheet;
    tsViewEditData: TTabSheet;
    btnCreateTables: TButton;
    btnLoadData: TButton;
    btnDropTables: TButton;
    LV: TtiListView;
    Panel1: TPanel;
    paeDescription: TtiPerAwareEdit;
    paeImage: TtiPerAwareImageEdit;
    Button1: TButton;
    ActionList1: TActionList;
    aSave: TAction;
    StatusBar1: TStatusBar;
    procedure btnCreateTablesClick(Sender: TObject);
    procedure btnDropTablesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadDataClick(Sender: TObject);
    procedure LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemLeave(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemArive(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure aSaveExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure paeDescriptionChange(Sender: TObject);
    procedure paeImageChange(Sender: TObject);
    procedure LVFilterData(pData: TPersistent; var pbInclude: Boolean);
  private
    FDemoImags : TDemoImages ;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiPersist
  ,DemoTIPerBlobs_Srv
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ;

{$R *.DFM}

procedure TFormMain.btnCreateTablesClick(Sender: TObject);
begin
  CreateImageDemoTable ;
end;

procedure TFormMain.btnDropTablesClick(Sender: TObject);
begin
  LV.Data := nil ;
  DropImageDemoTable ;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  PC.ActivePage := tsSetupDB;
  FDemoImags := TDemoImages.Create ;
  LV.AddColumn( 'Description', lvtkString, 'Description' ) ;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FDemoImags.Free ;
end;

procedure TFormMain.btnLoadDataClick(Sender: TObject);
begin
  LV.Data := nil ;
  FDemoImags.Clear ;
  FDemoImags.ReadPK ;
  LV.Data := FDemoImags.List ;
end;

procedure TFormMain.LVItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TDemoImage ;
begin
  lData := TDemoImage.CreateNew ;
  FDemoImags.Add(lData);
  pLV.Refresh ;
end;

procedure TFormMain.LVItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  tiPerObjAbsConfirmAndDelete( pData as TDemoImage ) ;
end;

procedure TFormMain.LVItemLeave(pLV: TtiCustomListView; pData: TPersistent;  pItem: TListItem);
begin
  paeDescription.Data := nil ;
  paeDescription.FieldName := '';
  paeImage.Data := nil ;
  paeImage.FieldName := '';
end;

procedure TFormMain.LVItemArive(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
var
  lData : TDemoImage ;
begin
  lData := pData as TDemoImage ;
  if lData.ObjectState = posPK then
    lData.Read ;
  paeDescription.LinkToData( lData, 'Description' ) ;
  paeImage.LinkToData( lData, 'Image' ) ;
end;

procedure TFormMain.aSaveExecute(Sender: TObject);
begin
  FDemoImags.Save ;
end;

procedure TFormMain.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  Handled := true ;
  aSave.Enabled := FDemoImags.Dirty ;
end;

procedure TFormMain.paeDescriptionChange(Sender: TObject);
begin
  if LV.SelectedData = nil then
    Exit ; //==>
  ( LV.SelectedData as TDemoImage ).Dirty := true ;
  LV.Refresh(false);
end;

procedure TFormMain.paeImageChange(Sender: TObject);
begin
  if LV.SelectedData = nil then
    Exit ; //==>
  ( LV.SelectedData as TDemoImage ).Dirty := true ;
end;

procedure TFormMain.LVFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TDemoImage ).Deleted ;
end;

end.
