unit FPersonList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiFormMgrForm, StdCtrls, ExtCtrls, tiFocusPanel, tiVTListView,
  tiObject, tiVirtualTrees;

const
  cFormCaption = 'View a list of people';

type
  TFormPersonList = class(TFormTIFormMgrForm)
    LV: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure LVItemDelete(pVT: TtiCustomVirtualTree; AData: TtiObject; AItem: PVirtualNode);
    procedure LVItemEdit(pVT: TtiCustomVirtualTree; AData: TtiObject; AItem: PVirtualNode);
    procedure LVItemInsert(pVT: TtiCustomVirtualTree; AData: TtiObject; AItem: PVirtualNode);
  private
    procedure DoPersonEditSave(const AData: TtiObject);
    procedure DoPersonInsertSave(const AData: TtiObject);
    procedure DoPersonInsertCancel(const AData: TtiObject);

  public
    { Public declarations }
  end;

procedure Execute;

implementation
uses
  tiApplicationMenuSystem
  ,Adrs_Manager
  ,tiConstants
  ,tiDialogs
  ,FPerson
  ,Adrs_BOM
 ;

{$R *.dfm}

procedure Execute;
begin
  gAMS.FormMgr.ShowForm(TFormPersonList);
end;

procedure TFormPersonList.DoPersonEditSave(const AData: TtiObject);
begin
  Assert(AData.TestValid(TPerson), CTIErrorInvalidObject);
  AData.Save;
end;

procedure TFormPersonList.DoPersonInsertCancel(const AData: TtiObject);
begin
  Assert(AData.TestValid(TPerson), CTIErrorInvalidObject);
  AData.Free;
end;

procedure TFormPersonList.DoPersonInsertSave(const AData: TtiObject);
begin
  Assert(AData.TestValid(TPerson), CTIErrorInvalidObject);
  GAdrsManager.AdrsBook.People.Add(AData as TPerson);
  AData.Save;
  LV.Refresh(AData);
end;

procedure TFormPersonList.FormCreate(Sender: TObject);
begin
  inherited;
  FormCaption:= cFormCaption;
  ButtonsVisible:= btnVisReadOnly;

  LV.AddColumn('FirstName', vttkString, 'First Name', 80);
  LV.AddColumn('LastName',  vttkString, 'Last Name', 80);
  LV.AddColumn('Initials',  vttkString, 'Initials', 80);
  LV.AddColumn('Title',     vttkString, 'Title', 80);
  LV.Data:= GAdrsManager.AdrsBook.People;

end;

procedure TFormPersonList.LVItemDelete(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  if tiAppConfirmation('Are you sure you want to delete "' +
                       AData.Caption + '"?') then
  begin
    AData.Read;
    GAdrsManager.AdrsBook.People.Extract(AData);
    AData.Deleted:= True;
    AData.Save;
    AData.Free;
    LV.Refresh;
  end;
end;

procedure TFormPersonList.LVItemEdit(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  AData.Read;
  gAMS.FormMgr.ShowFormModal(TFormPerson, AData, DoPersonEditSave);
end;

procedure TFormPersonList.LVItemInsert(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
var
  LData: TPerson;
begin
  inherited;
  LData:= TPerson.CreateNew;
  gAMS.FormMgr.ShowFormModal(TFormPerson, LData, DoPersonInsertSave, DoPersonInsertCancel);
end;

end.
