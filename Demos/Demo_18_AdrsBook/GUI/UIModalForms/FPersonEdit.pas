unit FPersonEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, ExtCtrls, tiFocusPanel,
  tiPerAwareCtrls, tiObject, tiVTAbstract, tiVTListView, tiVirtualTrees,
  FAdrsAbs;

type
  TFormPersonEdit = class(TFormAdrsAbs)
    paeTitle: TtiPerAwareEdit;
    paeFirstName: TtiPerAwareEdit;
    paeLastName: TtiPerAwareEdit;
    LVAdrs: TtiVTListView;
    LVEAdrs: TtiVTListView;
    procedure LVAdrsItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVAdrsItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVAdrsItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVEAdrsItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVEAdrsItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVEAdrsItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure LVAdrsFilterData(AData: TtiObject; var pInclude: Boolean);
  private
  protected
    procedure SetData(const AValue: TtiObject); override;
  public
    { Public declarations }
  end;

implementation
uses
  Adrs_BOM,
  FEAdrsEdit,
  FAdrsEdit,
  tiGUIUtils;

{$R *.dfm}

procedure TFormPersonEdit.FormCreate(Sender: TObject);
begin
  inherited;
  LVEAdrs.AddColumn('AdrsTypeAsString', vttkString, 'Address Type', 100);
  LVEAdrs.AddColumn('Text', vttkString, 'Text', 200);

  LVAdrs.AddColumn('AdrsTypeAsString', vttkString, 'Address Type', 100);
  LVAdrs.AddColumn('AsSingleLine', vttkString, 'Address', 200);

end;

procedure TFormPersonEdit.LVAdrsFilterData(AData: TtiObject;
  var pInclude: Boolean);
begin
  inherited;
  pInclude:= not AData.Deleted;
end;

procedure TFormPersonEdit.LVAdrsItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  if tiObjectConfirmDelete(AData) then
  begin
    AData.Deleted:= True;
    LVAdrs.Refresh;
  end;
end;

procedure TFormPersonEdit.LVAdrsItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if TFormAdrsEdit.Execute(AData) then
    LVEAdrs.Refresh(AData);
end;

procedure TFormPersonEdit.LVAdrsItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
var
  LData: TAdrs;
begin
  inherited;
  LData:= TAdrs.CreateNew;
  LData.Owner:= (DataBuffer as TPerson).AddressList;
  if TFormAdrsEdit.Execute(LData) then
  begin
    (DataBuffer as TPerson).AddressList.Add(LData);
    LVAdrs.Refresh(LData);
  end;
end;

procedure TFormPersonEdit.LVEAdrsItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if tiObjectConfirmDelete(AData) then
  begin
    AData.Deleted:= True;
    LVEAdrs.Refresh;
  end;
end;

procedure TFormPersonEdit.LVEAdrsItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if TFormEAdrsEdit.Execute(AData) then
    LVEAdrs.Refresh(AData);
end;

procedure TFormPersonEdit.LVEAdrsItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
var
  LData: TEAdrs;
begin
  inherited;
  LData:= TEAdrs.CreateNew;
  LData.Owner:= (DataBuffer as TPerson).EAddressList;
  if TFormEAdrsEdit.Execute(LData) then
  begin
    (DataBuffer as TPerson).EAddressList.Add(LData);
    LVEAdrs.Refresh(LData);
  end;
end;

procedure TFormPersonEdit.SetData(const AValue: TtiObject);
begin
  inherited SetData(AValue);
  paeTitle.LinkToData(Databuffer, 'Title');
  paeFirstName.LinkToData(DataBuffer, 'FirstName');
  paeLastName.LinkToData(DataBuffer, 'LastName');

  LVAdrs.Data:= (DataBuffer as TPerson).AddressList;
  LVEAdrs.Data:= (DataBuffer as TPerson).EAddressList;

end;

end.
