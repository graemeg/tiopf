unit tiApplicationMenuSystem;

interface
uses
  // Delphi units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ImgList, StdCtrls, ExtCtrls, ComCtrls, ActnList, Buttons,
  // TB2 and TBX units
  TBX, TB2Dock, TB2Toolbar, TB2Item, TB2Common, TB2ExtItems, TBXMDI,
  TBXExtItems, TBXSwitcher, TBXLists, TBXDkPanels, TBXToolPals, TBXStatusBars,
  TB2MDI, TBXStripesTheme, TBXOfficeXPTheme,
  // DX units
  dxWinXPBar, dxCore, dxContainer,
  // Help units
  ehshelprouter, ehsbase, ehswhatsthis,
  // tiOPF units
  tiObjAbs, FtiFormMgrForm, tiRoundedPanel
  ;

const
//cA-XXX-Nm = '';     cA-XXX-Cptn = '';     cA-XXX-Hnt = '';
  cAViewAppToolBarNm     = 'ViewApplicationToolBar'; cAViewAppToolBarCptn     = 'Application toolbar'; cAViewAppToolBarHnt     = 'View the application toolbar';
  cAViewHelpToolbarNm    = 'ViewHelpToolbar';        cAViewHelpToolbarCptn    = 'Help toolbar';        cAViewHelpToolbarHnt    = 'View the help toolbar';
  cAViewMenuSidebarNm    = 'ViewMenuSidebar';        cAViewMenuSidebarCptn    = 'Menu sidebar';        cAViewMenuSidebarHnt    = 'View the menu sidebar';
  cAViewProgressWindowNm = 'ViewProgressWindow';     cAViewProgressWindowCptn = 'Progress window';     cAViewProgressWindowHnt = 'View the progress window';

  cAFileExitNm           = 'FileExit';               cAFileExitCptn           = 'E&xit';               cAFileExitHnt           = 'Exit the application';

  cAWindowPreviousNm     = 'WindowPrevious';         cAWindowPreviousCptn     = 'Previous window';     cAWindowPreviousHnt     = 'Show the previous window';
  cAWindowNextNm         = 'WindowNext';             cAWindowNextCptn         = 'Next window';         cAWindowNextHnt         = 'Show the next window';
  cAWindowWorkListNm     = 'WindowWorkList';         cAWindowWorkListCptn     = 'Work list';           cAWindowWorkListHnt     = 'Show work list';
  cAWindowCloseNm        = 'WindowClose';            cAWindowCloseCptn        = 'Close';               cAWindowCloseHnt        = 'Close this window';

  cAHelpContentsNm       = 'HelpContentsForm';       cAHelpContentsCptn       = 'Help contents' ;      cAHelpContentsHnt       = 'Show the OPDMS help table of contents' ;
  cAHelpActiveFormNm     = 'HelpActiveForm';         cAHelpActiveFormCptn     = 'Help current form';   cAHelpActiveFormHnt     = 'Get help on the current form' ;
  cAHelpWhatsThisNm      = 'HelpWhatsThis' ;         cAHelpWhatsThisCptn      = 'What''s this?' ;      cAHelpWhatsThisHnt      = 'Click on an item to show what it does' ;
  cAHelpAboutNm          = 'HelpAbout';              cAHelpAboutCptn          = 'About OPDMS'   ;      cAHelpAboutHnt          = 'About the OPDMS' ;

type

   TtiApplicationMenuSystem = class(TtiObjAbs)
  private
    FAL : TActionList ;
    FMainForm: TForm;
    FWorkListFormClass: TFormTIFormMgrFormClass ;
    FHelpFileName: string;
    FDefHelpContext: integer;

    FpnlBorder: TPanel;
    FpnlParent: TtiRoundedPanel;
    //FpnlParent: TPanel;
    FimgWallPaper: TImage;

    FTBDockTop: TTBXDock;
    FMainMenuBar : TTBXToolbar;

    FLastTBXSubItem : TTBXSubmenuItem;
    FLastBoolBar :    TTBXToolbar;
    FLastMenuSidebarGroup: TdxWinXPBar;

    FtbImageList16      : TtbImageList;
    FtbImageList24      : TtbImageList;
    FImageListHot16     : TImageList;
    FImageListHot24     : TImageList;
    FImageListDisabled16: TImageList;
    FImageListDisabled24: TImageList;

    FtbxApplication: TTBXToolbar;
    FtbxHelp: TTBXToolbar;
    FStatusPanelHint : TTBXStatusPanel ;
    FtbxHelpMenu : TTBXSubmenuItem ;

    FdpMenuSidebar: TTBXDockablePanel;
    FdpProgressBar: TTBXDockablePanel;
    FdxContainer  : TdxContainer;
    FdxPageScroller: TTBXPageScroller;

    FHelpRouter : THelpRouter;
    FWhatsThis  : TWhatsThis;

    FActionWindowPrevious  : TAction;
    FActionWindowNext      : TAction;
    FActionWindowClose     : TAction;
    FAViewAppToolBarNm     : TAction;
    FAViewHelpToolbarNm    : TAction;
    FAViewMenuSidebarNm    : TAction;
    FAViewProgressWindowNm : TAction;
    FHelpAvailable         : Boolean;

    procedure CreateDockSystem;
    procedure CreateContainerPanels;
    procedure CreateMainMenuBar;
    procedure CreateApplicationToolBar;
    procedure CreateMenuSideBar;
    procedure CreateHelpToolBar;
    procedure CreateInternalActions;
    procedure CreateHelpSystem;

    procedure DoToolButtonFormPopup(Sender: TTBCustomItem;FromLink: Boolean);
    procedure DoSelectActiveForm(Sender: TObject);
    procedure OnMainFormReSize(Sender: TObject);
    procedure HintToStatusBar(Sender: TObject);
    procedure ALUpdate(pAction: TBasicAction; var pHandled: Boolean);

    procedure DoFileExit(Sender: TObject);

    procedure DoToggleApplicationToolbarVisible(Sender: TObject);
    procedure DoToggleHelpToolbarVisible(Sender: TObject);
    procedure DoToggleMenuSidebarVisible(Sender: TObject);
    procedure DoToggleProgressWindowVisible(Sender: TObject);

    procedure DoWindowPrevious(Sender: TObject);
    procedure DoWindowNext(Sender: TObject);
    procedure DoWindowWorkList(Sender: TObject);
    procedure DoWindowClose(Sender: TObject);

    procedure DoHelpContents(Sender: TObject);
    procedure DoHelpActiveForm(Sender: TObject);
    procedure DoHelpWhatsThis(Sender: TObject);
    procedure DoHelpAbout(Sender: TObject);
    procedure DoOnViewMenuPopup(Sender: TTBCustomItem; FromLink: Boolean);
    function  DoOnWhatsThisGetActiveWindow : TWinControl ;
    procedure WhatsThisGetContext(pSender, pHelpItem: TObject; pIsMenu: Boolean;
                                  var pHContext: THelpContext; pX, pY: Integer);
    function  GetHelpContext(const pHelpItem : TObject): THelpContext ;
    function  GetParentPnl: TPanel;

   public
     constructor Create(const pMainForm : TForm;
                        const pWorkListFormClass : TFormTIFormMgrFormClass;
                        const pHelpFileName: string ;
                        pDefHelpContext : integer
                       );

     property    MainForm : TForm read FMainForm ;
     property    MainMenuBar: TTBXToolbar read FMainMenuBar ;
     property    DPProgressBar : TTBXDockablePanel read FdpProgressBar;
     property    DPMenuSideBar : TTBXDockablePanel read FdpMenuSidebar ;
     property    TBXHelpMenu : TTBXSubmenuItem read FtbxHelpMenu;
     property    ParentPnl : TPanel read GetParentPnl ;
     property    HelpAvailable: Boolean read FHelpAvailable Write FHelpAvailable;

     procedure   SetWallpaperImageResName(const pResName : string);
     procedure   HelpJump(const pHelpTopic : string); overload ;
     procedure   HelpJump(pHelpContext : integer); overload ;
     procedure   HelpJump; overload ;

     function    AddAction(const pName : string ;
                           const pCaption : string ; const pHint : string ;
                           pImageIndex : integer ; const pEvent : TNotifyEvent ;
                           pHelpContext : integer = 0 ) : TAction ; overload ;
     function    AddAction16(const pName : string ;
                             const pCaption : string ; const pHint : string ;
                             const pResName : string ; const pEvent : TNotifyEvent ;
                             pHelpContext : integer = 0 ) : TAction ;
     function    AddAction24(const pName : string ;
                             const pCaption : string ; const pHint : string ;
                             const pResName : string ; const pEvent : TNotifyEvent ;
                             pHelpContext : integer = 0 ) : TAction ;
     function    FindAction(const pName : string): TAction;

     function    AddMainMenuItem(const pCaption: string; pIndex : integer = -1 ): TTBXSubmenuItem ;
     function    AddMenuItem(const pAction : TAction; const pTBXSubItem : TTBXSubMenuItem = nil ): TTBXItem;
     function    AddMenuItemSeparator: TTBXSeparatorItem;

     function    AddToolBar(const pCaption: string): TTBXToolbar;
     function    AddDropDownToolButton(const pAction : TAction): TTBXSubmenuItem;
     function    AddToolButton(const pAction : TAction): TTBXItem;
     function    AddToolBarSeparator: TTBXSeparatorItem;

     function    AddMenuSidebarGroup(const pCaption: string): TdxWinXPBar;
     function    AddMenuSidebarItem(const pAction: TAction): TdxWinXPBarItem;

   end ;


implementation
uses
  tiFormMgr
  ,tiImageMgr
  ,tiThreadProgress
  ,tiUtils
  ,tiResources
  ,tiDialogs
  ,FAbout
  ;

const
  cBorder = 4 ;

{ TtiApplicationMenuSystem }

function TtiApplicationMenuSystem.AddMainMenuItem(const pCaption: string ; pIndex : integer = -1 ):TTBXSubmenuItem;
var
  lIndex : integer ;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(FMainMenuBar <> nil, 'FMainMenuBar not assigned');
  if pIndex = -1 then
    lIndex := FMainMenuBar.Items.Count
  else
    lIndex := pIndex ;
  FLastTBXSubItem := TTBXSubmenuItem.Create(FMainForm);
  FMainMenuBar.Items.Insert(lIndex, FLastTBXSubItem);
  FLastTBXSubItem.Images := FtbImageList16;
  FLastTBXSubItem.Caption := pCaption;
  result := FLastTBXSubItem;
end;

function TtiApplicationMenuSystem.AddMenuItem(const pAction: TAction; const pTBXSubItem : TTBXSubMenuItem = nil ): TTBXItem;
var
  lTBXSubMenuItem : TTBXSubmenuItem ;
begin
  result := TTBXItem.Create(FMainForm);
  if pTBXSubItem <> nil then
    lTBXSubMenuItem := pTBXSubItem
  else
    lTBXSubMenuItem := FLastTBXSubItem ;
  lTBXSubMenuItem.Add(result);
  result.Action := pAction;
end;

function TtiApplicationMenuSystem.AddMenuItemSeparator: TTBXSeparatorItem;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(FLastTBXSubItem <> nil, 'FLastTBXSubItem not assigend');
  result := TTBXSeparatorItem.Create(FMainForm);
  FLastTBXSubItem.Add(result);
end;

function TtiApplicationMenuSystem.AddToolBar(const pCaption: string): TTBXToolbar;
begin
  Assert(FMainForm <> nil, 'pMainForm not assigned' ) ;
  Assert(FtbImageList24 <> nil, 'pImageList not assigned');
  FLastBoolBar:= TTBXToolbar.Create(MainForm);
  FLastBoolBar.Parent := FTBDockTop;
  FLastBoolBar.Caption := pCaption;
  FLastBoolBar.Images := FtbImageList24;
  FLastBoolBar.OnDockChanged    := OnMainFormReSize ;
  FLastBoolBar.OnVisibleChanged := OnMainFormReSize ;
  FLastBoolBar.ShowHint := true ;
  result := FLastBoolBar;
end;

function TtiApplicationMenuSystem.AddToolBarSeparator: TTBXSeparatorItem;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(FLastBoolBar <> nil, 'FLastTBXSubItem not assigend');
  result := TTBXSeparatorItem.Create(FMainForm) ;
  FLastBoolBar.Items.Add(result);
end;

function TtiApplicationMenuSystem.AddDropDownToolButton(const pAction: TAction): TTBXSubmenuItem;
begin
  result := TTBXSubmenuItem.Create(MainForm);
  FLastBoolBar.Items.Add(result);
  result.Action := pAction;
  result.DisplayMode := nbdmTextOnlyInMenus;
end;

constructor TtiApplicationMenuSystem.Create(const pMainForm: TForm;
                                            const pWorkListFormClass : TFormTIFormMgrFormClass;
                                            const pHelpFileName : string ;
                                            pDefHelpContext : integer
                                            );

begin
  Assert(pMainForm <> nil, 'pMainForm not assigned' ) ;
  Assert(not Assigned(pMainForm.OnResize), 'pMainForm.OnResize assigned');
  inherited Create;
  FMainForm := pMainForm ;
  FMainForm.OnResize := OnMainFormResize;
  FWorkListFormClass := pWorkListFormClass;
  FHelpFileName := pHelpFileName;
  FDefHelpContext := pDefHelpContext ;

  FAL := TActionList.Create(MainForm) ;
  FAL.OnUpdate := ALUpdate ;

  FImageListHot16     := TImageList.Create(FMainForm);
  FImageListDisabled16:= TImageList.Create(FMainForm);
  FImageListHot24     := TImageList.Create(FMainForm);
  FImageListHot24.Height := 24 ;
  FImageListHot24.Width  := 24 ;
  FImageListDisabled24:= TImageList.Create(FMainForm);
  FImageListDisabled24.Height := 24 ;
  FImageListDisabled24.Width  := 24 ;

  FtbImageList16                := TTBImageList.Create(FMainForm);
  FtbImageList16.DisabledImages := FImageListDisabled16;
  FtbImageList16.HotImages      := FImageListHot16;
  FtbImageList24                := TTBImageList.Create(FMainForm);
  FtbImageList24.Height := 24 ;
  FtbImageList24.Width  := 24 ;
  FtbImageList24.DisabledImages := FImageListDisabled24;
  FtbImageList24.HotImages      := FImageListHot24;

  gTIImageListMgr.ILNormal16   := FtbImageList16 ;
  gTIImageListMgr.ILHot16      := FImageListHot16 ;
  gTIImageListMgr.ILDisabled16 := FImageListDisabled16 ;
  gTIImageListMgr.ILNormal24   := FtbImageList24 ;
  gTIImageListMgr.ILHot24      := FImageListHot24 ;
  gTIImageListMgr.ILDisabled24 := FImageListDisabled24 ;
  gTIImageListMgr.LoadTIOPFImages ;

  CreateDockSystem;
  CreateContainerPanels;
  CreateInternalActions;
  CreateMainMenuBar;
  CreateHelpToolBar;
  CreateApplicationToolBar;
  CreateMenuSidebar;
  CreateHelpSystem;
end;


procedure TtiApplicationMenuSystem.CreateApplicationToolBar;
var
  ltbPrev : TTBXSubmenuItem;
  ltbNext : TTBXSubmenuItem;
begin
  FtbxApplication := AddToolBar('Navigation');
  FtbxApplication.OnDockChanged := OnMainFormResize;
  FtbxApplication.OnVisibleChanged := OnMainFormResize;
  ltbPrev := AddDropDownToolButton(FindAction(cAWindowPreviousNm));
  ltbPrev.DropdownCombo := True;
  ltbPrev.OnPopup := DoToolButtonFormPopup;
  ltbNext := AddDropDownToolButton(FindAction(cAWindowNextNm));
  ltbNext.DropdownCombo := True;
  ltbNext.OnPopup := DoToolButtonFormPopup;
  AddToolBarSeparator;
  AddToolButton(FindAction(cAWindowWorkListNm));
  AddToolButton(FindAction(cAWindowCloseNm));
  AddToolButton(FindAction(caFileExitNm));
end;

procedure TtiApplicationMenuSystem.CreateHelpToolBar;
begin
  FtbxHelp := AddToolBar('Help');
  FtbxHelp.OnDockChanged := OnMainFormReSize;
  FtbxHelp.OnVisibleChanged := OnMainFormReSize;
  AddToolButton(FindAction(caHelpWhatsThisNm));
  AddToolButton(FindAction(caHelpActiveFormNm));
end;

procedure TtiApplicationMenuSystem.CreateMainMenuBar;
var
  lTBXItem : TTBXItem ;
begin
  FMainMenuBar := TTBXToolbar.Create(FMainForm);
  FMainMenuBar.Parent := FTBDockTop;
  FMainMenuBar.Left := 0;
  FMainMenuBar.Top := 0;
  FMainMenuBar.Caption := 'Menu Bar';
  FMainMenuBar.CloseButton := False;
  FMainMenuBar.DockPos := 0;
  FMainMenuBar.Font.Charset := DEFAULT_CHARSET;
  FMainMenuBar.Font.Color := clWindowText;
  FMainMenuBar.Font.Height := -11;
  FMainMenuBar.Font.Name := 'Tahoma';
  FMainMenuBar.Font.Style := [];
  FMainMenuBar.FullSize := True;
  FMainMenuBar.MenuBar := True;
  FMainMenuBar.ParentFont := False;
  FMainMenuBar.ProcessShortCuts := True;
  FMainMenuBar.ShrinkMode := tbsmWrap;
  FMainMenuBar.TabOrder := 0;
  FMainMenuBar.HelpContext := FDefHelpContext;
  FMainMenuBar.Images := FtbImageList16;
  FMainMenuBar.OnDockChanged := OnMainFormReSize ;
  FMainMenuBar.OnVisibleChanged := OnMainFormReSize;

  AddMainMenuItem('&File');
  AddMenuItem(FindAction(cAFileExitNm));

  AddMainMenuItem('&View');
  FLastTBXSubItem.OnPopup := DoOnViewMenuPopup ;
  lTBXItem := AddMenuItem(FindAction(cAViewAppToolBarNm));
  lTBXItem.ImageIndex := -1 ;
  lTBXItem := AddMenuItem(FindAction(cAViewHelpToolBarNm));
  lTBXItem.ImageIndex := -1 ;
  lTBXItem := AddMenuItem(FindAction(cAViewMenuSideBarNm));
  lTBXItem.ImageIndex := -1 ;
  lTBXItem := AddMenuItem(FindAction(cAViewProgressWindowNm));
  lTBXItem.ImageIndex := -1 ;

  AddMainMenuItem('&Window');
  AddMenuItem(FindAction(cAWindowPreviousNm));
  AddMenuItem(FindAction(cAWindowNextNm));
  AddMenuItemSeparator;
  AddMenuItem(FindAction(cAWindowWorkListNm));
  AddMenuItem(FindAction(cAWindowCloseNm));

  FtbxHelpMenu := AddMainMenuItem('&Help');
  AddMenuItem(FindAction(cAHelpContentsNm));
  AddMenuItem(FindAction(caHelpActiveFormNm));
  AddMenuItem(FindAction(caHelpWhatsThisNm));
  AddMenuItem(FindAction(caHelpAboutNm));

end;

procedure TtiApplicationMenuSystem.DoToolButtonFormPopup(
  Sender: TTBCustomItem; FromLink: Boolean);
var
  lsl : TStringList ;
  i : integer ;
  lItem : TTBXItem ;
begin
  lsl := TStringList.Create ;
  try
    gFormMgr.AssignFormList(lsl);
    Sender.Clear;
    for i := 0 to lsl.Count - 1 do
      if lsl.Objects[i] <> gFormMgr.ActiveForm then
      begin
        lItem := TTBXItem.Create(Sender);
        Sender.Add(lItem);
        lItem.Caption := lsl.Strings[i];
        lItem.OnClick := DoSelectActiveForm ;
        lItem.Tag := Integer( lsl.Objects[i] );
      end ;
  finally
    lsl.Free;
  end ;
end;

procedure TtiApplicationMenuSystem.DoSelectActiveForm(Sender: TObject);
begin
  if not ( Sender is TTBCustomItem ) then
    Exit ; //==>
  gFormMgr.BringToFront(TFormTIFormMgrForm(TTBCustomItem(Sender).Tag));
end;


function TtiApplicationMenuSystem.AddToolButton(const pAction: TAction): TTBXItem;
begin
  result := TTBXItem.Create(MainForm);
  FLastBoolBar.Items.Add(result);
  result.Action := pAction;
  result.DisplayMode := nbdmTextOnlyInMenus;
end;

procedure TtiApplicationMenuSystem.CreateDockSystem;
var
  ltbMultiDockTop: TTBXMultiDock;
  ltbDockLeft: TTBXDock;
  ltbMultiDockLeft: TTBXMultiDock;
  ltbdBottom: TTBXDock;
  ltbxStatusBar: TTBXStatusBar;
  ltbMultiDockBottom: TTBXMultiDock;
  ltbDockRight: TTBXDock;
  ltbMultiDockRight: TTBXMultiDock;
  lPnlProgress: TtiRoundedPanel;
begin

  ltbxStatusBar:= TTBXStatusBar.Create(FMainForm);
  ltbxStatusBar.Parent := FMainForm ;
  ltbxStatusBar.Font.Charset := DEFAULT_CHARSET;
  ltbxStatusBar.Font.Color := clBlack;
  ltbxStatusBar.Font.Height := -11;
  ltbxStatusBar.Font.Name := 'Tahoma';
  ltbxStatusBar.Font.Style := [];
  ltbxStatusBar.HelpContext := FDefHelpContext;
  FStatusPanelHint := ltbxStatusBar.Panels.Add ;
  FStatusPanelHint.Size   := 220 ;
  ltbxStatusBar.Panels.Add.StretchPriority := 1 ;
  ltbxStatusBar.ParentShowHint := False;
  ltbxStatusBar.ShowHint := True;
  ltbxStatusBar.UseSystemFont := True;

  FtbDockTop:= TTBXDock.Create(FMainForm);
  FtbDockTop.Parent := FMainForm;
  FtbDockTop.Left := 0;
  FtbDockTop.Top := 0;
  FtbDockTop.Width := 792;
  FtbDockTop.Height := 9;
  FtbDockTop.Color := clAppWorkSpace ;
  FtbDockTop.HelpContext := FDefHelpContext;

  ltbMultiDockTop:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockTop.Parent := FMainForm ;
  ltbMultiDockTop.Left := 0;
  ltbMultiDockTop.Top := 9;
  ltbMultiDockTop.Width := 792;
  ltbMultiDockTop.Height := 7;
  ltbMultiDockTop.FixAlign := True;
  ltbMultiDockTop.HelpContext := FDefHelpContext;

  ltbDockLeft:= TTBXDock.Create(FMainForm);
  ltbDockLeft.Parent := FMainForm ;
  ltbDockLeft.Left := 0;
  ltbDockLeft.Top := 16;
  ltbDockLeft.Width := 9;
  ltbDockLeft.Height := 573;
  ltbDockLeft.Position := dpLeft;
  ltbDockLeft.Color := clAppWorkSpace ;
  ltbDockLeft.HelpContext := FDefHelpContext;

  ltbMultiDockLeft:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockLeft.Parent := FMainForm ;
  ltbMultiDockLeft.Left := 9;
  ltbMultiDockLeft.Top := 16;
  ltbMultiDockLeft.Width := 180;
  ltbMultiDockLeft.Height := 573;
  ltbMultiDockLeft.FixAlign := True;
  ltbMultiDockLeft.Position := dpLeft;
  ltbMultiDockLeft.HelpContext := FDefHelpContext;

  FdpMenuSidebar:= TTBXDockablePanel.Create(FMainForm);
  FdpMenuSidebar.Parent := ltbMultiDockLeft;
  FdpMenuSidebar.Left := 0;
  FdpMenuSidebar.Top := 0;
  FdpMenuSidebar.MinClientHeight := 64;
  FdpMenuSidebar.MinClientWidth := 80;
  FdpMenuSidebar.Caption := 'Menu side bar';
  FdpMenuSidebar.DockedWidth := 176;
  FdpMenuSidebar.DockPos := 32;
  FdpMenuSidebar.FloatingWidth := 160;
  FdpMenuSidebar.FloatingHeight := 320;
  FdpMenuSidebar.Font.Charset := DEFAULT_CHARSET;
  FdpMenuSidebar.Font.Color := clWindowText;
  FdpMenuSidebar.Font.Height := -11;
  FdpMenuSidebar.Font.Name := 'Tahoma';
  FdpMenuSidebar.Font.Style := [];
  FdpMenuSidebar.ParentFont := False;
  FdpMenuSidebar.SupportedDocks := [dkMultiDock];
  FdpMenuSidebar.TabOrder := 0;
  FdpMenuSidebar.HelpContext := FDefHelpContext;
  FdpMenuSidebar.OnDockChanged := OnMainFormReSize;
  FdpMenuSidebar.OnVisibleChanged:= OnMainFormReSize;

  ltbdBottom:= TTBXDock.Create(FMainForm);
  ltbdBottom.Parent := FMainForm ;
  ltbdBottom.Left := 0;
  ltbdBottom.Top := 661;
  ltbdBottom.Width := 792;
  ltbdBottom.Height := 9;
  ltbdBottom.FixAlign := True;
  ltbdBottom.Position := dpBottom;
  ltbdBottom.Color := clAppWorkSpace ;
  ltbdBottom.HelpContext := FDefHelpContext;

  ltbMultiDockBottom:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockBottom.Parent := FMainForm ;
  ltbMultiDockBottom.Left := 0;
  ltbMultiDockBottom.Top := 589;
  ltbMultiDockBottom.Width := 792;
  ltbMultiDockBottom.Height := 62;
  ltbMultiDockBottom.FixAlign := True;
  ltbMultiDockBottom.Position := dpBottom;
  ltbMultiDockBottom.HelpContext := FDefHelpContext;

  FdpProgressBar:= TTBXDockablePanel.Create(FMainForm);
  FdpProgressBar.Parent := ltbMultiDockBottom;
  FdpProgressBar.Left := 0;
  FdpProgressBar.Top := 0;
  FdpProgressBar.BorderSize := 1;
  FdpProgressBar.Caption := '';
  FdpProgressBar.Color := clWhite;
  FdpProgressBar.DockedWidth := 123;
  FdpProgressBar.DockedHeight := 68;
  FdpProgressBar.DockPos := 0;
  FdpProgressBar.FloatingWidth := 128;
  FdpProgressBar.FloatingHeight := 128;
  FdpProgressBar.SupportedDocks := [dkMultiDock];
  FdpProgressBar.TabOrder := 0;
  FdpProgressBar.CloseButtonWhenDocked := false;
  FdpProgressBar.HelpContext := FDefHelpContext;
  FdpProgressBar.OnDockChanged := OnMainFormReSize;
  FdpProgressBar.OnVisibleChanged:= OnMainFormReSize;

  lPnlProgress:= TtiRoundedPanel.Create(FMainForm);
  lPnlProgress.Parent:= FdpProgressBar;
  lPnlProgress.Align:= alClient;
  lPnlProgress.Color := clWhite;
  lPnlProgress.BorderColor:= clSkyBlue;
  lPnlProgress.HelpContext := FDefHelpContext;
  lPnlProgress.CornerRadius := 5 ;

  ltbDockRight:= TTBXDock.Create(FMainForm);
  ltbDockRight.Parent := FMainForm ;
  ltbDockRight.Left := 776;
  ltbDockRight.Top := 16;
  ltbDockRight.Width := 9;
  ltbDockRight.Height := 573;
  ltbDockRight.FixAlign := True;
  ltbDockRight.Position := dpRight;
  ltbDockRight.Color := clAppWorkSpace ;
  ltbDockRight.HelpContext := FDefHelpContext;

  ltbMultiDockRight:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockRight.Parent := FMainForm ;
  ltbMultiDockRight.Left := 785;
  ltbMultiDockRight.Top := 16;
  ltbMultiDockRight.Width := 7;
  ltbMultiDockRight.Height := 573;
  ltbMultiDockRight.FixAlign := True;
  ltbMultiDockRight.Position := dpRight;
  ltbMultiDockRight.HelpContext := FDefHelpContext;

  TBXSetTheme('OfficeXP');

  //dpProgressBar.Height                 := 72 ; // Will show 4 progress bars
  dpProgressBar.Height                 := 46 ; // Will show 2 progress bars
  gFormThreadProgress.BorderStyle      := bsNone ;
  gFormThreadProgress.Parent           := lPnlProgress ;
  gFormThreadProgress.PanelBevelInner  := bvNone ;
  gFormThreadProgress.PanelBevelOuter  := bvNone ;
  gFormThreadProgress.Align            := alClient ;
  gFormThreadProgress.Color            := clWhite ;
  gFormThreadProgress.ProgressBarColor := clSilver ; //clAppWorkSpace ;
  gFormThreadProgress.ColumnCount      := 2 ;
  gFormThreadProgress.Width            := FdpProgressBar.ClientWidth ;
  gFormThreadProgress.HelpContext      := FDefHelpContext;

  Application.OnHint := HintToStatusBar ;

  FtbDockTop.OnResize := OnMainFormReSize;
  ltbMultiDockTop.OnResize := OnMainFormReSize;
  ltbDockLeft.OnResize := OnMainFormReSize;
  ltbMultiDockLeft.OnResize := OnMainFormReSize;
  ltbdBottom.OnResize := OnMainFormReSize;
  ltbMultiDockBottom.OnResize := OnMainFormReSize;
  ltbDockRight.OnResize := OnMainFormReSize;
  ltbMultiDockRight.OnResize := OnMainFormReSize;
  
end;

procedure TtiApplicationMenuSystem.CreateMenuSidebar;
begin
  FdxPageScroller:= TTBXPageScroller.Create(MainForm);
  FdxPageScroller.Parent := FDPMenuSidebar;
  FdxPageScroller.Left := 0;
  FdxPageScroller.Top := 0;
  FdxPageScroller.Width := 176;
  FdxPageScroller.Height := 487;
  FdxPageScroller.Align := alClient;
  FdxPageScroller.AutoRange := True;
  FdxPageScroller.Color := clSkyBlue;
  FdxPageScroller.DoubleBuffered := True;
  FdxPageScroller.Margin := 10;
  FdxPageScroller.ParentColor := False;
  FdxPageScroller.TabOrder := 0;
  FdxPageScroller.Color := FpnlBorder.Color ;
  FdxPageScroller.HelpContext := FDefHelpContext;

  FdxContainer:= TdxContainer.Create(MainForm);
  FdxContainer.Parent := FdxPageScroller ;
  FdxContainer.Left := 0;
  FdxContainer.Top := 0;
  FdxContainer.Width := 176;
  FdxContainer.Height := 742;
  FdxContainer.Align := alTop;
  FdxContainer.AutoSize := True;
  FdxContainer.BorderWidth := 5;
  FdxContainer.Caption := 'dxContainer';
  FdxContainer.Color := clSkyBlue;
  FdxContainer.ParentColor := False;
  FdxContainer.Color    := FpnlBorder.Color ;
  FdxContainer.HelpContext := FDefHelpContext;

end;

function TtiApplicationMenuSystem.AddMenuSidebarGroup(const pCaption: string): TdxWinXPBar;
begin
  FLastMenuSidebarGroup:= TdxWinXPBar.Create(MainForm);
  FLastMenuSidebarGroup.Parent := FDXContainer ;
  FLastMenuSidebarGroup.Align := alTop;
  FLastMenuSidebarGroup.Font.Charset := DEFAULT_CHARSET;
  FLastMenuSidebarGroup.Font.Color := 15159552;
  FLastMenuSidebarGroup.Font.Height := -11;
  FLastMenuSidebarGroup.Font.Name := 'Tahoma';
  FLastMenuSidebarGroup.Font.Style := [];
  FLastMenuSidebarGroup.ParentFont := False;
  FLastMenuSidebarGroup.ParentShowHint := False;
  FLastMenuSidebarGroup.ShowHint := True;
  FLastMenuSidebarGroup.Caption := pCaption;
  FLastMenuSidebarGroup.HeaderFont.Charset := DEFAULT_CHARSET;
  FLastMenuSidebarGroup.HeaderFont.Color := clBlack;
  FLastMenuSidebarGroup.HeaderFont.Height := -11;
  //FLastMenuSidebarGroup.HeaderFont.Size := 9;
  FLastMenuSidebarGroup.HeaderFont.Name := 'Tahoma';
  FLastMenuSidebarGroup.HeaderFont.Style := [fsBold];
  FLastMenuSidebarGroup.ImageList := FtbImageList16;
  FLastMenuSidebarGroup.ShowHint  := true ;
  result := FLastMenuSidebarGroup ;
end;

function TtiApplicationMenuSystem.AddMenuSidebarItem(const pAction: TAction): TdxWinXPBarItem;
begin
  result := FLastMenuSidebarGroup.Items.Add;
  result.Action := pAction ;
  result.Hint := pAction.Hint;
end;

procedure TtiApplicationMenuSystem.CreateContainerPanels;
begin
  FpnlBorder:= TPanel.Create(FMainForm);
  FpnlBorder.Parent := FMainForm;
  FpnlBorder.BevelOuter := bvNone;
  FpnlBorder.Color := clSkyBlue;
  FpnlBorder.Align := alClient ;
  FpnlBorder.HelpContext := FDefHelpContext;

  FpnlParent:= TtiRoundedPanel.Create(FMainForm);
  FpnlParent.Parent := FPnlBorder;
  FpnlParent.Left := 8;
  FpnlParent.Top := 8;
  FpnlParent.HelpContext := FDefHelpContext;
  FpnlParent.Color := clSkyBlue;
  FpnlParent.BorderColor := clWhite ;

  FimgWallPaper:= TImage.Create(FMainForm);
  FimgWallPaper.Parent := FpnlParent ;
  FimgWallPaper.Left := 20;
  FimgWallPaper.Top := 32;
  FimgWallPaper.AutoSize := True;
  FimgWallPaper.HelpContext := FDefHelpContext;

  gFormMgr.ParentPnl    := ParentPnl ;
  gFormMgr.BorderColor  := FpnlBorder.Color ;

end ;

procedure TtiApplicationMenuSystem.OnMainFormReSize(Sender: TObject);
begin
  FpnlParent.SetBounds(
    cBorder, cBorder,
    FpnlBorder.ClientWidth - cBorder*2,
    FpnlBorder.ClientHeight - cBorder*2 ) ;
  FimgWallpaper.SetBounds(
    ( FpnlBorder.ClientWidth - FimgWallPaper.Width ) div 2,
    ( FpnlBorder.ClientHeight - FimgWallPaper.Height ) div 2,
    FimgWallPaper.Width,
    FimgWallPaper.Height
 );
end;

procedure TtiApplicationMenuSystem.SetWallpaperImageResName(const pResName: string);
begin
  gTIImageListMgr.LoadBMPFromRes(pResName, FImgWallPaper.Picture.Bitmap);
end;

procedure TtiApplicationMenuSystem.HintToStatusBar(Sender: TObject);
begin
  FStatusPanelHint.Caption := '  ' + GetLongHint(Application.Hint);
end;

procedure TtiApplicationMenuSystem.WhatsThisGetContext(pSender, pHelpItem: TObject;
  pIsMenu: Boolean; var pHContext: THelpContext; pX, pY: Integer);
  function _GetTBXToolBarHelpContext( const HelpItem : TObject ) : THelpContext ;
  var
     lV: TTBItemViewer;
  begin
    lV := nil ;
    result := 0 ;
    if HelpItem is TTBXToolBar then
      lV := TTBXToolbar(HelpItem).View.ViewerFromPoint(TTBXToolBar(HelpItem).ScreenToClient(point(pX,pY)))
    else if (HelpItem is TTBXPopupWindow) then
      lV := TTBXPopupWindow(HelpItem).View.ViewerFromPoint(TTBXPopupWindow(HelpItem).ScreenToClient(point(pX,pY)))
    else
      tiFmtException('Invalid class <' + HelpItem.ClassName + '>', ClassName, '_GetTBXToolBarHelpContext' ) ;
    if ( lV <> nil ) and
       ( lV.Item <> nil ) and
       ( lV.Item.Action <> nil ) then
      result := ( lV.Item.Action as TAction ).HelpContext ;
  end ;

  function _GetDXWinXPBarHelpContext( const HelpItem :TdxWinXPBar ) : THelpContext ;
  var
    lPoint : TPoint ;
    lY     : integer ;
    i      : integer ;
  begin
    result := 0 ;
    lPoint := HelpItem.ScreenToClient(Point(pX,pY));
    lY     := lPoint.Y ;
    i      := ( HelpItem.Height - 35 ) div lY ;
    if i >= HelpItem.Items.Count then
      i := HelpItem.Items.Count - 1 ;
    if HelpItem.Items[i].Action <> nil then
      result := ( HelpItem.Items[i].Action as TAction ).HelpContext ;
  end ;

begin
  if ( pHelpItem is TTBXToolBar ) or
     ( pHelpItem is TTBXPopupWindow ) then
  begin
    pHContext := _GetTBXToolBarHelpContext( pHelpItem );
    Exit ; //==>
  end ;

  if ( pHelpItem is TdxWinXPBar ) then
  begin
    pHContext := _GetDXWinXPBarHelpContext( pHelpItem as TdxWinXPBar );
    Exit ; //==>
  end ;

  pHContext := GetHelpContext(pHelpItem);

end ;

function TtiApplicationMenuSystem.GetHelpContext(const pHelpItem: TObject): THelpContext;
  function _GetHelpContext(const pHelpItem: TControl): integer;
  begin
    if pHelpItem.HelpContext <> 0 then
      result := pHelpItem.HelpContext
    else if ( pHelpItem.Action <> nil ) and
            ( pHelpItem.Action is TAction ) and
            (( pHelpItem.Action as TAction ).HelpContext <> 0 ) then
      result := (pHelpItem.Action as TAction).HelpContext
    else if pHelpItem.Parent <> nil then
      result := _GetHelpContext(pHelpItem.Parent)
    else if (pHelpItem.Owner <> nil ) and
       (pHelpItem.Owner is TControl) then
      result := _GetHelpContext(pHelpItem.Owner as TControl)
    else
      result := FDefHelpContext;
  end;
begin
  if pHelpItem is TControl then
    result := _GetHelpContext(pHelpItem as TControl)
  else
    result := 0 ;
end;

function TtiApplicationMenuSystem.AddAction(const pName, pCaption, pHint: string;
  pImageIndex: integer; const pEvent: TNotifyEvent ;
  pHelpContext : integer = 0 ): TAction;
begin
  Assert(pName <> '', 'pName not assigned');
  Assert(pCaption<>'', 'pCaption not assigned');
  Assert(pHint<>'', 'pHint not assigned');
  Assert(pImageIndex >= -1, 'pImageIndex < -1');
  Assert(Assigned(pEvent), 'pEvent not assigned');
  Assert(FindAction(pName)=nil, 'Action name <' + pName + '> not unique');
  result             := TAction.Create( FAL ) ;
  result.ActionList  := FAL ;
  result.Caption     := pCaption ;
  result.OnExecute   := pEvent ;
  result.ImageIndex  := pImageIndex ;
  result.Hint        := pHint;
  result.Name        := pName ;
  if pHelpContext <> 0 then
    result.HelpContext := pHelpContext
  else
    result.HelpContext := FDefHelpContext;
end;

function TtiApplicationMenuSystem.AddAction16(const pName : string ;
  const pCaption : string ; const pHint : string ;
  const pResName : string ; const pEvent : TNotifyEvent ;
  pHelpContext : integer = 0 ) : TAction ;
var
  lImageIndex : integer ;
begin
  lImageIndex := gTIImageListMgr.ImageIndex16(pResName);
  result :=AddAction(pName, pCaption, pHint, lImageIndex, pEvent, pHelpContext);
end ;

function TtiApplicationMenuSystem.AddAction24(const pName : string ;
  const pCaption : string ; const pHint : string ;
  const pResName : string ; const pEvent : TNotifyEvent ;
  pHelpContext : integer = 0 ) : TAction ;
var
  lImageIndex : integer ;
begin
  lImageIndex := gTIImageListMgr.ImageIndex24(pResName);
  result :=AddAction(pName, pCaption, pHint, lImageIndex, pEvent, pHelpContext);
end ;

procedure TtiApplicationMenuSystem.ALUpdate(pAction: TBasicAction; var pHandled: Boolean);
begin
  FActionWindowNext.Enabled     := gFormMgr.NextForm <> nil ;
  FActionWindowPrevious.Enabled := gFormMgr.PrevForm <> nil ;
  FActionWindowClose.Enabled    := gFormMgr.ActiveForm <> nil ;
end;

function TtiApplicationMenuSystem.FindAction(const pName: string): TAction;
var
  i : integer ;
begin
  result := nil;  
  for i := 0 to FAL.ActionCount - 1 do
    if SameText( pName, FAL.Actions[i].Name ) then
    begin
      result := FAL.Actions[i] as TAction;
      Exit ; //==>
    end;
end;

procedure TtiApplicationMenuSystem.CreateInternalActions;
var
  lAction : TAction ;
begin

  AddAction24(cAFileExitNm, cAFileExitCptn, cAFileExitHnt, cResTI_Exit, DoFileExit );

  FAViewAppToolBarNm     := AddAction(cAViewAppToolBarNm,     cAViewAppToolBarCptn,     cAViewAppToolBarHnt,     FDefHelpContext, DoToggleApplicationToolbarVisible);
  FAViewHelpToolbarNm    := AddAction(cAViewHelpToolbarNm,    cAViewHelpToolbarCptn,    cAViewHelpToolbarHnt,    FDefHelpContext, DoToggleHelpToolbarVisible ) ;
  FAViewMenuSidebarNm    := AddAction(cAViewMenuSidebarNm,    cAViewMenuSidebarCptn,    cAViewMenuSidebarHnt,    FDefHelpContext, DoToggleMenuSidebarVisible ) ;
  FAViewProgressWindowNm := AddAction(cAViewProgressWindowNm, cAViewProgressWindowCptn, cAViewProgressWindowHnt, FDefHelpContext, DoToggleProgressWindowVisible ) ;

  FActionWindowPrevious := AddAction24(cAWindowPreviousNm, cAWindowPreviousCptn, cAWindowPreviousHnt, cResTI_ArrowLeft,        DoWindowPrevious);
  FActionWindowNext     := AddAction24(cAWindowNextNm,     cAWindowNextCptn,     cAWindowNextHnt,     cResTI_ArrowRight,       DoWindowNext);
  FActionWindowClose    := AddAction24(cAWindowCloseNm,    cAWindowCloseCptn,    cAWindowCloseHnt,    cResTI_CloseWindow,      DoWindowClose);
  AddAction24(cAWindowWorkListNm, cAWindowWorkListCptn,   cAWindowWorkListHnt,  cResTI_WorkList,      DoWindowWorkList);

  lAction := AddAction24(cAHelpActiveFormNm,      cAHelpActiveFormCptn,      cAHelpActiveFormHnt,      cResTI_Help,               DoHelpActiveForm);
  lAction.ShortCut := ShortCut(VK_F1, []);
  lAction := AddAction24(cAHelpWhatsThisNm, cAHelpWhatsThisCptn, cAHelpWhatsThisHnt, cResTI_HelpWhatsThis,      DoHelpWhatsThis);
  lAction.ShortCut := ShortCut(VK_F1, [ssShift]);
  AddAction24(cAHelpAboutNm,     cAHelpAboutCptn,     cAHelpAboutHnt,     cResTI_HelpAbout,          DoHelpAbout);

  AddAction24(cAHelpContentsNm,      cAHelpContentsCptn,      cAHelpContentsHnt,      cResTI_Help,               DoHelpContents);
end;

procedure TtiApplicationMenuSystem.DoToggleApplicationToolbarVisible(Sender: TObject);
var
  lVisible : boolean ;
begin
  lVisible := not FtbxApplication.Visible ;
  FtbxApplication.Visible := lVisible ;
  FindAction(cAViewAppToolBarNm).Checked := lVisible ;
end ;

procedure TtiApplicationMenuSystem.DoToggleHelpToolbarVisible(Sender: TObject);
var
  lVisible : boolean ;
begin
  lVisible := not FtbxHelp.Visible ;
  FtbxHelp.Visible := lVisible ;
  FindAction(cAViewHelpToolbarNm).Checked := lVisible ;
end ;

procedure TtiApplicationMenuSystem.DoToggleMenuSidebarVisible(Sender: TObject);
var
  lVisible : boolean ;
begin
  lVisible := not FdpMenuSidebar.Visible ;
  FdpMenuSidebar.Visible := lVisible ;
  FindAction(cAViewMenuSidebarNm).Checked := lVisible ;
end ;

procedure TtiApplicationMenuSystem.DoToggleProgressWindowVisible(Sender: TObject);
var
  lVisible : boolean ;
begin
  lVisible := not FdpProgressBar.Visible ;
  FdpProgressBar.Visible := lVisible ;
  FindAction(cAViewProgressWindowNm).Checked := lVisible ;
end ;

procedure TtiApplicationMenuSystem.DoOnViewMenuPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  FAViewAppToolBarNm.Checked     := FtbxApplication.Visible ;
  FAViewHelpToolbarNm.Checked    := FtbxHelp.Visible;
  FAViewMenuSidebarNm.Checked    := FdpMenuSidebar.Visible ;
  FAViewProgressWindowNm.Checked := FdpProgressBar.Visible ;
end;

procedure TtiApplicationMenuSystem.DoWindowClose(Sender: TObject);
begin
  gFormMgr.CloseForm(gFormMgr.ActiveForm);
end;

procedure TtiApplicationMenuSystem.DoWindowNext(Sender: TObject);
begin
  gFormMgr.ShowNextForm ;
end;

procedure TtiApplicationMenuSystem.DoWindowPrevious(Sender: TObject);
begin
  gFormMgr.ShowPrevForm ;
end;

procedure TtiApplicationMenuSystem.DoWindowWorkList(Sender: TObject);
begin
  if Assigned(FWorkListFormClass) then
    gFormMgr.ShowForm(FWorkListFormClass);
end;

procedure TtiApplicationMenuSystem.DoFileExit(Sender: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to exit the application?' ) then
    MainForm.Close ;
end;

procedure TtiApplicationMenuSystem.DoHelpAbout(Sender: TObject);
begin
  FAbout.Execute;
end;

procedure TtiApplicationMenuSystem.DoHelpContents(Sender: TObject);
begin
  FHelpRouter.HelpContent ;
end;

procedure TtiApplicationMenuSystem.DoHelpWhatsThis(Sender: TObject);
begin
  FWhatsThis.ContextHelp;
end;

procedure TtiApplicationMenuSystem.CreateHelpSystem;
begin

  if not FileExists( FHelpFileName ) then
  begin
    FHelpAvailable := False ;
    FindAction( cAHelpActiveFormNm ).Visible := false ;
    FindAction( cAHelpWhatsThisNm ).Visible := false ;
    FindAction( cAHelpContentsNm ).Visible := false;
    FtbxHelp.Visible := false ;
    FAViewHelpToolbarNm.Visible := false ;
    tiAppError( 'Unable to find the help file <' + FHelpFileName +'>' + Cr +
                'Help system will not be available' ) ;
  end else
  begin
    FHelpAvailable := True ;
    FHelpRouter:= THelpRouter.Create(MainForm);
    FHelpRouter.HelpType := htHTMLhelp;
    FHelpRouter.ShowType := stMain;
    FHelpRouter.Helpfile := FHelpFileName;
    FHelpRouter.ValidateID := False;

    FWhatsThis := TWhatsThis.Create(MainForm);
    FWhatsThis.F1Action := goContext; //goContext, goDefault, goTOC
    FWhatsThis.Options := [wtMenuRightClick{, wtNoContextHelp, wtNoContextMenu, wtInheritFormContext}];
    // (wtDirectHelp, wtMenuRightClick, wtNoContextHelp,
    //                     wtNoContextMenu, wtPopupOnForm, wtInheritFormContext,
    //                     wtUseTag, wtUseAppKey)

    FWhatsThis.PopupCaption := 'What'#39's this?';
    FWhatsThis.PopupHelpContext := 0;
    FWhatsThis.OnGetContext := WhatsThisGetContext;
    FWhatsThis.OnGetActiveWindow := DoOnWhatsThisGetActiveWindow ;
    FWhatsThis.Active := True;
    FWhatsThis.Loaded;
  end ;

end;

procedure TtiApplicationMenuSystem.HelpJump(const pHelpTopic: string);
begin
  FHelpRouter.HelpJump(pHelpTopic);
end;

procedure TtiApplicationMenuSystem.HelpJump(pHelpContext: integer);
begin
  Application.helpcommand(HELP_CONTEXT, pHelpContext);
end;

procedure TtiApplicationMenuSystem.HelpJump;
begin
  if ( gFormMgr.ActiveForm <> nil ) and
     ( gFormMgr.ActiveForm.HelpContext <> 0 ) then
    HelpJump(gFormMgr.ActiveForm.HelpContext)
  else
    HelpJump(FDefHelpContext);
end;

function TtiApplicationMenuSystem.DoOnWhatsThisGetActiveWindow: TWinControl;
begin
  if gFormMgr.ActiveForm <> nil then
    result := gFormMgr.ActiveForm
  else
    result := nil ;
end;

procedure TtiApplicationMenuSystem.DoHelpActiveForm(Sender: TObject);
begin
  HelpJump;
end;

function TtiApplicationMenuSystem.GetParentPnl: TPanel;
begin
  result := TPanel(FpnlParent);
end;

end.
