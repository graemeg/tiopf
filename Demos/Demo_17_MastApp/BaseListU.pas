unit BaseListU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  ExtCtrls, ActnList, tiFocusPanel, tiVTListView, tiObject, tiVirtualTrees,
  StdCtrls, Buttons, MastApp_BOM;

type
  TfrmBaseList = class(TForm)
    LV: TtiVTListView;
    ActionList1: TActionList;
    aSave: TAction;
    aUndo: TAction;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVFilterData(AData: TtiObject; var pInclude: Boolean);
    procedure aSaveExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aSaveUpdate(Sender: TObject);
  private

  protected
    FEditingData: TMASTObjectList;
    FOriginalData: TMASTObjectList;

    procedure SetColumns; virtual;
    function CreateNewItem: TtiObject; virtual;
    function EditItem(AItem: TtiObject): boolean; virtual;

    procedure SaveChanges;
  public
    { Public declarations }
    class procedure Execute(ADataList: TMASTObjectList);
  end;

implementation

{$R *.dfm}

uses
  tiOPFManager
  ,tiConstants
  ,tiGUIUtils
;

procedure TfrmBaseList.aSaveExecute(Sender: TObject);
begin
  SaveChanges;
end;

procedure TfrmBaseList.aSaveUpdate(Sender: TObject);
begin
  TAction(sender).Enabled:= FEditingData.Dirty;
end;

procedure TfrmBaseList.aUndoExecute(Sender: TObject);
begin
  LV.Data := nil;
  FEditingData.Clear ;
  FEditingData.Assign(FOriginalData) ;
  LV.Data := FEditingData;
end;

function TfrmBaseList.CreateNewItem: TtiObject;
begin
  result:= nil;
end;

function TfrmBaseList.EditItem(AItem: TtiObject): boolean;
begin
  result:= false;
end;

class procedure TfrmBaseList.Execute(ADataList: TMASTObjectList);
var form: TfrmBaseList;
begin
  form:= self.create(Application);

  form.SetColumns;

  form.FOriginalData:= ADataList;
  form.FEditingData.Assign(ADataList);

  form.lv.Data:= form.FEditingData;

  if form.ShowModal = mrOk then
  begin
    form.SaveChanges;
  end;
end;

procedure TfrmBaseList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmBaseList.FormCreate(Sender: TObject);
begin
  FEditingData:= TMASTObjectList.Create;
end;

procedure TfrmBaseList.FormDestroy(Sender: TObject);
begin
  FEditingData.free;
end;

procedure TfrmBaseList.LVFilterData(AData: TtiObject; var pInclude: Boolean);
begin
  pInclude := not aData.Deleted ;
end;

procedure TfrmBaseList.LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
begin
  if tiPerObjAbsConfirmAndDelete(AData) then
    pVT.Refresh;
end;

procedure TfrmBaseList.LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
begin
  if EditItem(AData) then
    pVT.Refresh(AData);
end;

procedure TfrmBaseList.SaveChanges;
begin
  FEditingData.Save();
  FOriginalData.Clear;
  FOriginalData.Read;
end;

procedure TfrmBaseList.SetColumns;
begin

end;

procedure TfrmBaseList.LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
var
  item : TtiObject ;
begin
  item := CreateNewItem ;

  if EditItem(item) then
  begin
    FEditingData.Add(item);
    pVT.Refresh(item);
  end
  else
    item.Free ;
end;

end.
