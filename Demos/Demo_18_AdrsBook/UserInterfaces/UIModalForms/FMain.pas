unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, tiFocusPanel, tiVTAbstract, tiVTListView, Adrs_BOM,
  StdCtrls, tiObject, tiVirtualTrees;

type
  TFormMain = class(TForm)
    LV: TtiVTListView;
    Panel1: TPanel;
    btnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    procedure Refresh;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiOPFManager,
  Adrs_Dependencies,
  Adrs_Singleton,
  FPersonEdit,
  tiGUIUtils,
  tiGUIINI;

{$R *.dfm}

procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GGUIINI.ReadFormState(Self);

  Adrs_Dependencies.ConnectToDatabase;

  Caption:= Caption + ' (' +
        ConnectionDetailsAsString + ')';

  LV.AddColumn('Title', vttkString, 'Title', 70);
  LV.AddColumn('FirstName', vttkString, 'First Name', 120);
  LV.AddColumn('LastName', vttkString, 'Last Name', 120);

  Refresh;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  GGUIINI.WriteFormState(Self);
end;

procedure TFormMain.LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
var
  LData: TPerson;
begin
  LData:= AData as TPerson;
  if tiObjectConfirmDelete(LData) then
  begin
    LData.Deleted:= True;
    LData.Save;
    GAdrsBook.PersonList.FreeDeleted;
    LV.Refresh;
  end;
end;

procedure TFormMain.LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
begin
  if TFormPersonEdit.Execute(AData) then
  begin
    (AData as TPerson).Save;
    LV.Refresh(AData);
  end;
end;

procedure TFormMain.LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject;
  AItem: PVirtualNode);
var
  LData: TPerson;
begin
  LData:= TPerson.CreateNew;
  LData.Owner:= GAdrsBook.PersonList;
  if TFormPersonEdit.Execute(LData) then
  begin
    LData.Save;
    GAdrsBook.PersonList.Add(LData);
    LV.Refresh(LData);
  end else
    LData.Free;
end;

procedure TFormMain.Refresh;
begin
  LV.Data:= nil;
  FreeAndNilAdrsBook;
  LV.Data:= GAdrsBook.PersonList;
end;

end.
