unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tiListView, tiListViewPlus, tiPerAwareCtrls, Buttons,
  Adrs_Cli, ActnList, Menus, Adrs_BOM, ComCtrls, ToolWin, tiButtons,
  ImgList, Contnrs, tiSplitter, tiObject, tiFormMgr;

type



  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    ActionList1: TActionList;
    aExit: TAction;
    tiToolBar1: TtiToolBar;
    ToolButton3: TToolButton;
    IL: TImageList;
    N7: TMenuItem;
    aDaysWorkList: TAction;
    aDaysWorkList1: TMenuItem;
    SB: TStatusBar;
    SP: TtiSplitterPanel;
    SpeedButton1: TSpeedButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    aFormPrevious: TAction;
    aFormNext: TAction;
    aFormClose: TAction;
    ToolButton10: TToolButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    pnlCntr: TPanel;
    tmrCloseForm: TTimer;
    aFind: TAction;
    aNewPerson: TAction;
    aNewCompany: TAction;
    Edit1: TMenuItem;
    FindF31: TMenuItem;
    N1: TMenuItem;
    Newperson1: TMenuItem;
    Newcompany1: TMenuItem;
    Help1: TMenuItem;
    aHelpAbout: TAction;
    aHelpShowLogFile: TAction;
    About1: TMenuItem;
    Showlogfile1: TMenuItem;
    imgWallpaper: TImage;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure pmClientPopup(Sender: TObject);
    procedure aDaysWorkListExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aFormPreviousExecute(Sender: TObject);
    procedure aFormNextExecute(Sender: TObject);
    procedure aFormCloseExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure tmrCloseFormTimer(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aNewPersonExecute(Sender: TObject);
    procedure aNewCompanyExecute(Sender: TObject);
    procedure aHelpAboutExecute(Sender: TObject);
    procedure aHelpShowLogFileExecute(Sender: TObject);
  private
    FSelectedClients: TObjectList;
    FFirstCall: boolean;
    procedure CloseActiveForm(var Message: TMessage); message TI_CLOSEACTIVEFORM;
    procedure HintToStatusBar(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
   tiSingleInstanceApp
  ,tiGUIINI
  ,tiDialogs
  ,tiOPFManager
  ,tiLog
  ,tiUtils
  ,FWorkList
  ,FtiAdrsListChild_Person
  ,FtiAdrsListChild_Company
 ;

{$R *.DFM}

const
  cBorder = 8;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SP.Panel1.Color:= clAppWorkSpace;
  SP.Panel1.Font.Color:= clWhite;
  SP.Panel2.Color:= clNavy;
  gFormMgr.ParentPnl:= pnlCntr;
  tiSaveWindowHandle(Self);
  Application.OnHint:= HintToStatusBar;
  Caption:= Caption + ' - ' + GTIOPFManager.DefaultDBConnectionName;
  SP.Align:= alClient;
  FSelectedClients:= TObjectList.Create(false);
  gINI.ReadFormState(self);
  FFirstCall:= true;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  gFormMgr.CloseAllForms;
  FSelectedClients.Free;
  gINI.WriteFormState(self);
end;

procedure TFormMain.aExitExecute(Sender: TObject);
begin
  if tiAppConfirmation('Are you sure you want to close ' + Caption) then
    Close;
end;

procedure TFormMain.pmClientPopup(Sender: TObject);
//var
//  i: integer;
//  lMenuItem: TMenuItem;
begin
{
  for i:= pmClient.Items.Count - 1 downto 3 do
    pmClient.Items.Delete(i);
  lMenuItem:= TMenuItem.Create(pmClient);
  lMenuItem.Caption:= '-';
  pmClient.Items.Add(lMenuItem);
  for i:= FSelectedClients.Count - 1 downto 0 do
  begin
    lMenuItem:= TMenuItem.Create(pmClient);
    lMenuItem.Caption:=
      IntToStr(FSelectedClients.Count - i) +
      '. ' +
      TtiObject(FSelectedClients.Items[i]).Caption;
    lMenuItem.OnClick:= DoSelectedClientEditClick;
    lMenuItem.Tag:= i;
    pmClient.Items.Add(lMenuItem);
  end;
}  
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if FFirstCall then
  begin
    FFirstCall:= false;
    aDaysWorkListExecute(nil);
  end;
end;

procedure TFormMain.HintToStatusBar(Sender: TObject);
begin
  SB.Panels[0].Text:= GetLongHint(Application.Hint);
end;

procedure TFormMain.aFormPreviousExecute(Sender: TObject);
begin
  gFormMgr.ShowPrevForm;
end;

procedure TFormMain.aFormNextExecute(Sender: TObject);
begin
  gFormMgr.ShowNextForm;
end;

procedure TFormMain.aFormCloseExecute(Sender: TObject);
begin
  gFormMgr.CloseForm(gFormMgr.ActiveForm);
end;

procedure TFormMain.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  Handled:= true;
  aFormPrevious.Enabled:= gFormMgr.PrevForm <> nil;
  aFormNext.Enabled    := gFormMgr.NextForm <> nil;
  aFormClose.Enabled   := gFormMgr.ActiveForm <> nil;

end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnlCntr.SetBounds(
    cBorder, cBorder,
    SP.Panel2.ClientWidth - cBorder*2,
    SP.Panel2.ClientHeight - cBorder*2);
  imgWallpaper.SetBounds(
    (pnlCntr.ClientWidth - imgWallPaper.Width) div 2,
    (pnlCntr.ClientHeight - imgWallPaper.Height) div 2,
    imgWallPaper.Width,
    imgWallPaper.Height
);
end;

procedure TFormMain.CloseActiveForm(var Message: TMessage);
begin
  tmrCloseForm.Enabled:= true;
end;

procedure TFormMain.tmrCloseFormTimer(Sender: TObject);
begin
  tmrCloseForm.Enabled:= false;
  aFormCloseExecute(nil);
end;

procedure TFormMain.aDaysWorkListExecute(Sender: TObject);
begin
  gFormMgr.ShowForm(TFormWorkList);
end;

procedure TFormMain.aFindExecute(Sender: TObject);
begin
  gFormMgr.ShowForm(TFormWorkList);
end;

procedure TFormMain.aNewPersonExecute(Sender: TObject);
begin
  FtiAdrsListChild_Person.EditNewPerson;
end;

procedure TFormMain.aNewCompanyExecute(Sender: TObject);
begin
  FtiAdrsListChild_Company.EditNewCompany;
end;

procedure TFormMain.aHelpAboutExecute(Sender: TObject);
begin
  tiAppMessage(Caption);
end;

procedure TFormMain.aHelpShowLogFileExecute(Sender: TObject);
begin
  tiEditFile(gLog.LogFileName);
end;

end.
