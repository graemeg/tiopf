unit tiApplicationMenuSystem;

{$I tiDefines.inc}

interface
uses
  // Delphi units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ImgList, StdCtrls, ExtCtrls, ComCtrls, ActnList, Buttons,
  // TB2 and TBX units
  TBX, TB2Dock, TB2Toolbar, TB2Item, TB2Common, TB2ExtItems, TBXMDI,
  TBXExtItems, TBXSwitcher, TBXLists, TBXDkPanels, TBXToolPals, TBXStatusBars,
  TB2MDI, TBXOfficeXPTheme,
  // DX units
  dxWinXPBar, dxCore, dxContainer,
  // Help units
  ehswhatsthis,
  // HTML Viewer units
  Htmlview,
  // tiOPF units
  tiUtils, tiBaseObject, FtiFormMgrForm, tiRoundedPanel, tiAnimatedGIF,
  tiGUIUtils
 ;

const
//cA-XXX-Nm = '';     cA-XXX-Cptn = '';     cA-XXX-Hnt = '';
  cAViewAppToolBarNm     = 'ViewApplicationToolBar'; cAViewAppToolBarCptn     = 'Application toolbar'; cAViewAppToolBarHnt     = 'View the application toolbar';
  cAViewHelpToolbarNm    = 'ViewHelpToolbar';        cAViewHelpToolbarCptn    = 'Help toolbar';        cAViewHelpToolbarHnt    = 'View the help toolbar';
  cAViewMenuSidebarNm    = 'ViewMenuSidebar';        cAViewMenuSidebarCptn    = 'Menu sidebar';        cAViewMenuSidebarHnt    = 'View the menu sidebar';
  cAViewProgressWindowNm = 'ViewProgressWindow';     cAViewProgressWindowCptn = 'Progress window';     cAViewProgressWindowHnt = 'View the progress window';

  cAWindowPreviousMenuNm    = 'PreviousPageMenu';   cAWindowPreviousCptn     = 'Previous page';       cAWindowPreviousHnt     = 'Show the previous page';
  cAWindowPreviousToolbarNm = 'PreviousPageToolbar';
  cAWindowNextMenuNm        = 'NextPageMenu';       cAWindowNextCptn         = 'Next page';           cAWindowNextHnt         = 'Show the next page';
  cAWindowNextToolbarNm     = 'NextPageToolbar';
  cAFileExitMenuNm       = 'FileExitMenu';           cAFileExitCptn           = 'E&xit';               cAFileExitHnt           = 'Exit the application';
  cAFileExitToolbarNm    = 'FileExitToolbar';

  cAHelpContentsNm       = 'HelpContentsForm';       cAHelpContentsCptn       = 'Help contents';       cAHelpContentsHnt       = 'Show the OPDMS help table of contents';
  cAHelpActiveFormMenuNm    = 'HelpActiveFormMenu';  cAHelpActiveFormCptn     = 'Help current form';   cAHelpActiveFormHnt     = 'Get help on the current form';
  cAHelpActiveFormToolbarNm = 'HelpActiveFormToolbar';
  cAHelpWhatsThisMenuNm     = 'HelpWhatsThisMenu';   cAHelpWhatsThisCptn      = 'What''s this?';       cAHelpWhatsThisHnt      = 'Click on an item to show what it does';
  cAHelpWhatsThisToolbarNm  = 'HelpWhatsThisToolbar';
  cAHelpContactNm        = 'HelpContact';            cAHelpContactCptn        = 'Contact %s';          cAHelpContactHnt        = 'Send an email to %s';
  cAHelpAboutNm          = 'HelpAbout';              cAHelpAboutCptn          = 'About OPDMS'  ;       cAHelpAboutHnt          = 'About the OPDMS';

  cMessageWindowHint     = 'Click here to hide this message|';

  CMenuSideBarWidth = 176;

type

  // Encapsulates an animated GIF image displayed on a toolbar to indicate that
  // the application is processing a request (e.g. network traffic) similar
  // to the Firefox or IE browsers whilst they are loading documents.
  // The animated GIF image is "right aligned" on the toolbar. The TBX toolbar
  // does not support this out of the box so this is implented using a simple
  // work-around by inserting a spacing blank label control before the image
  // and adjusting its width to fill the remaining toolbar space.
  TtiApplicationBusyToolbarImage = class(TtiBaseObject)
  private
    FAnimatedGIFSpaceLabel: TLabel;
    FAnimatedGIF: TtiAnimatedGIF;
    FToolbar: TTBXToolbar;
    FVisible: Boolean;
    FAnimationEnabled: boolean;
    FOnBusyHint: TtiHintShowEvent;
    FOnNotBusyHint: TtiHintShowEvent;
    FAnimationSpeed: integer;
    FResourceName: string;

    function  GetAnimationEnabled: Boolean;
    procedure SetAnimationEnabled(const AValue: Boolean);

    procedure SetOnBusyHint(const AValue: TtiHintShowEvent);
    procedure SetNotOnBusyHint(const AValue: TtiHintShowEvent);

    function  GetResourceName: string;
    procedure SetResourceName(const AValue: string);

    function  GetAnimationSpeed: Integer;
    procedure SetAnimationSpeed(const AValue: Integer);

    procedure SetVisible(const AValue: Boolean);

    procedure AnimateGIF(AAnimationEnabled: boolean);
  public
    constructor Create(Toolbar: TTBXToolbar); virtual;
    procedure Resize;
    // Is the given control one used in the application busy toolbar image.
    function IsBusyItem(pToolbarItem: TTBCustomItem): Boolean;
    property ResourceName: string read GetResourceName write SetResourceName;
    property OnBusyHint: TtiHintShowEvent read FOnBusyHint write SetOnBusyHint;
    property OnNotBusyHint: TtiHintShowEvent read FOnNotBusyHint write SetNotOnBusyHint;
    property AnimationEnabled: Boolean read GetAnimationEnabled write SetAnimationEnabled;
    property AnimationSpeed: Integer read GetAnimationSpeed write SetAnimationSpeed;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TMessageOnHotSpotClickEvent = procedure(const ASrc: string) of object;

  TtiApplicationMenuSystemDisplaySetings = class
  private
    FBackgroundColor: TColor;
    FSidebarGroupBodyColor: TColor;
    FSidebarGroupGradientFrom: TColor;
    FSidebarGroupGradientTo: TColor;
  public
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property SidebarGroupBodyColor: TColor read FSidebarGroupBodyColor write FSidebarGroupBodyColor;
    property SidebarGroupGradientFrom: TColor read FSidebarGroupGradientFrom write FSidebarGroupGradientFrom;
    property SidebarGroupGradientTo: TColor read FSidebarGroupGradientTo write FSidebarGroupGradientTo;
  end;

  TtiAMSOnGetShowModalMenuEvent = procedure(const AMenuItem: TTBXSubmenuItem;
      var AShow: Boolean) of object;

  TtiApplicationMenuSystem = class(TtiBaseObject)
  private
    FFormMgr: TtiFormMgr;
    FAL : TActionList;
    FMainForm: TForm;
    FWorkListFormClass: TtiFormMgrFormClass;
    FHelpFileName: string;
    FDefHelpContext: integer;
    FContactName: string;
    FContactEmailAddress: string;
    FDefaultDisplaySettings: TtiApplicationMenuSystemDisplaySetings;
    FDisplaySettings: TtiApplicationMenuSystemDisplaySetings; // Reference
    FOnGetShowModalMenu: TtiAMSOnGetShowModalMenuEvent;

    FlblCaption : TLabel;
    FpnlCaption: TPanel;
    FpnlBorder: TtiRoundedPanel;
    FpnlParent: TtiRoundedPanel;

    FTBDockTop: TTBXDock;
    FMainMenuBar : TTBXToolbar;
    FStatusBar: TTBXStatusBar;

    FLastTBXSubItem : TTBXSubmenuItem;
    FLastToolBar :    TTBXToolbar;
    FLastMenuSidebarGroup: TdxWinXPBar;

    FtbImageList16     : TtbImageList;
    FtbImageList24     : TtbImageList;
    FImageListHot16    : TImageList;
    FImageListHot24    : TImageList;
    FImageListDisabled16: TImageList;
    FImageListDisabled24: TImageList;

    FtbdBottom: TTBXDock;
    FtbMultiDockBottom: TTBXMultiDock;
    FtbxApplication: TTBXToolbar;
    FtbxHelp: TTBXToolbar;
    FStatusPanelHint : TTBXStatusPanel;
    FStatusPanelMessage : TTBXStatusPanel;

    FtbxFileMenu    : TTBXSubmenuItem;
//    FtbxViewMenu    : TTBXSubmenuItem;
    FtbxWindowMenu  : TTBXSubmenuItem;
    FtbxHelpMenu    : TTBXSubmenuItem;
    FtbxContextMenu : TTBXSubmenuItem;
    FtbxPrevForm    : TTBXSubmenuItem;
    FtbxNextForm    : TTBXSubmenuItem;

    FApplicationBusyToolbarImage: TtiApplicationBusyToolbarImage;

    FdpMenuSidebar: TTBXDockablePanel;
    FdpMessageBar: TTBXDockablePanel;
    FdxPageScroller: TTBXPageScroller;
    FdxContainer : TdxContainer;
    FdxWinXPBarContext  : TdxWinXPBar;
    FPnlEmptyMessage: TtiRoundedPanel;
    FPnlProgress: TtiRoundedPanel;
    FPnlMessageText: TtiRoundedPanel;
    FhtmlMessage: THTMLViewer;

    FWhatsThis : TWhatsThis;

    FActionWindowPreviousMenu : TAction;
    FActionWindowNextMenu     : TAction;
    FActionWindowPreviousToolbar : TAction;
    FActionWindowNextToolbar     : TAction;
    FAViewAppToolBarNm    : TAction;
    FAViewHelpToolbarNm   : TAction;
    FAViewMenuSidebarNm   : TAction;
    FAViewProgressWindowNm : TAction;
    FHelpAvailable        : Boolean;
    FNavigationControlsEnabled: Boolean;
    FUpdateCount: Integer;
    FBruteForceNoFlicker: TtiBruteForceNoFlicker;
    FAboutFormClass : TFormClass;
    FhtmlMessageOnHotSpotClickEvent: TMessageOnHotSpotClickEvent;
    FEscapeContextAction: TAction;

    procedure CreateDockSystem;
    procedure CreateContainerPanels;
    procedure CreateMainMenuBar;
    procedure CreateApplicationToolBar;
    procedure CreateMenuSideBar;
    procedure CreateHelpToolBar;
    procedure CreateInternalActions;
    procedure CreateHelpSystem;
    procedure CreateContextMenuItems;

    procedure DoToolButtonFormPopup(Sender: TTBCustomItem;FromLink: Boolean);
    procedure DoSelectActiveForm(Sender: TObject);
    procedure OnMainFormReSize(Sender: TObject);
    procedure HintToStatusBar(Sender: TObject);
    procedure OnMainMenuBarResize(Sender: TObject);

    procedure DoFileExit(Sender: TObject);

    procedure DoToggleApplicationToolbarVisible(Sender: TObject);
    procedure DoToggleHelpToolbarVisible(Sender: TObject);
    procedure DoToggleMenuSidebarVisible(Sender: TObject);
    procedure DoToggleProgressWindowVisible(Sender: TObject);

    procedure DoWindowPrevious(Sender: TObject);
    procedure DoWindowNext(Sender: TObject);

    procedure DoHelpContents(Sender: TObject);
    procedure DoHelpActiveForm(Sender: TObject);
    procedure DoHelpWhatsThis(Sender: TObject);
    procedure DoHelpContact(Sender: TObject);
    procedure DoHelpAbout(Sender: TObject);
    //procedure DoOnViewMenuPopup(Sender: TTBCustomItem; FromLink: Boolean);
    function  DoOnWhatsThisGetActiveWindow : TWinControl;
    procedure WhatsThisGetContext(pSender, pHelpItem: TObject; pIsMenu: Boolean;
                                  var pHContext: THelpContext; pX, pY: Integer);
    function  GetHelpContext(const pHelpItem : TObject): THelpContext;
    function  GetParentPnl: TPanel;
    procedure SetFormMessage(const AMessage: string; pMessageType: TtiUserFeedbackMessageType);
    procedure SetFormErrorMessage(const AMessage: string);
    procedure SetNavigationControlsEnabled(const AValue: Boolean);
    function  GetFormCaption: TCaption;
    procedure SetFormCaption(const AValue: TCaption);
    procedure DoOnShowForm(const pForm: TtiFormMgrForm);
    procedure DoBeginUpdate(Sender: TObject);
    procedure DoEndUpdate(Sender: TObject);
    procedure DoLBLMessageClick(Sender: TObject);
    procedure DoLblMessageOnHotSpotClick(ASender: TObject; const ASRC: string;
        var AHandled: boolean);
    function  GetMenuSideBarWidth: integer;
    procedure SetMenuSideBarWidth(const AValue: integer);
    function GetStatusPannelMessage: string;
    procedure SetStatusPannelMessage(const AValue: string);
    procedure SetFormInfoMessage(const AMessage: string);
    procedure DoProgressVisibleChange(const AVisible: Boolean);
    procedure ArrangeMessagePanels;
   public
     constructor Create(const AMainForm: TForm;
                        const AWorkListFormClass: TtiFormMgrFormClass;
                        const AHelpFileName: string;
                        const ADefHelpContext: Integer;
                        const AAboutFormClass: TFormClass;
                        const ALblMessageOnHotSpotClickEvent: TMessageOnHotSpotClickEvent;
                        const AContactName: string = '';
                        const AContactEmailAddress: string = '';
                        const ADisplaySettings: TtiApplicationMenuSystemDisplaySetings = nil
                      );
     destructor  Destroy; override;
     property    MainForm : TForm read FMainForm;
     property    MainMenuBar: TTBXToolbar read FMainMenuBar;
     property    DPMessageBar : TTBXDockablePanel read FdpMessageBar;
     property    DPMenuSideBar : TTBXDockablePanel read FdpMenuSidebar;
     property    TBXHelpMenu : TTBXSubmenuItem read FtbxHelpMenu;
     property    StatusBar: TTBXStatusBar read FStatusBar;
     property    ParentPnl : TPanel read GetParentPnl;
     property    HelpAvailable: Boolean read FHelpAvailable Write FHelpAvailable;
     property    HelpFileName: string read FHelpFileName;
     property    FormMgr: TtiFormMgr read FFormMgr;
     property    ApplicationBusyToolbarImage: TtiApplicationBusyToolbarImage read FApplicationBusyToolbarImage;


     procedure   HelpJump(const pHelpTopic : string); overload;
     procedure   HelpJump(pHelpContext : integer); overload;
     procedure   HelpJump; overload;
     property    NavigationControlsEnabled: Boolean read FNavigationControlsEnabled Write SetNavigationControlsEnabled;
     property    FormCaption: TCaption read GetFormCaption Write SetFormCaption;
     property    MenuSideBarWidth: integer read GetMenuSideBarWidth write SetMenuSideBarWidth default CMenuSideBarWidth;

     function    AddAction(const AName : string;
                           const pCaption : string; const pHint : string;
                           pImageIndex : integer; const pEvent : TNotifyEvent;
                           pHelpContext : integer = 0): TAction; overload;
     function    AddAction16(const AName : string;
                             const pCaption : string; const pHint : string;
                             const pResName : string; const pEvent : TNotifyEvent;
                             pHelpContext : integer = 0): TAction;
     function    AddAction24(const AName : string;
                             const pCaption : string; const pHint : string;
                             const pResName : string; const pEvent : TNotifyEvent;
                             pHelpContext : integer = 0): TAction;
     function    FindAction(const AName : string): TAction; // TODO: We can remove FindAction by exposing the private Action fields as properties

{$IFDEF DELPHI2010ORABOVE}
     function    FindFormInstance<T: TForm>: T;
{$ENDIF}

     // ToDo: AddActionUpdateHandler should add the handler to a list, which
     //       is scanned in the FAL.OnUpdate. Will also require a
     //       RemoveActionUpdateHandler method
     procedure   AddActionUpdateHandler(const AHandler: TActionEvent);

     function    AddMainMenuItem(const pCaption: string; AIndex : integer = -1): TTBXSubmenuItem;
     function    AddMenuItem(const pAction : TAction; const pTBXSubItem : TTBXSubMenuItem = nil): TTBXItem;
     function    AddMenuItemSeparator: TTBXSeparatorItem; overload;
     function    AddMenuItemSeparator(const ATBXSubMenuItem: TTBXSubMenuItem): TTBXSeparatorItem; overload;

     function    AddToolBar(const pCaption: string): TTBXToolbar;
     function    AddDropDownToolButton(const pAction : TAction): TTBXSubmenuItem;
     function    AddToolButton(const pAction: TAction; pDisplayMode: TTBItemDisplayMode): TTBXItem;
     function    AddToolBarSeparator: TTBXSeparatorItem;

     function    AddMenuSidebarGroup(const ACaption: string; const AName: string = ''): TdxWinXPBar;
     function    AddMenuSidebarItem(const pAction: TAction; const pSideBarGroup: TdxWinXPBar = nil): TdxWinXPBarItem;

     procedure   ClearContextActions;
     procedure   AddContextAction(const AAction: TAction);
     procedure   AddContextActions(const AList: TList);
     procedure   MouseToContextMenuSideBar;
     procedure   SetEscapeKeyEnabled(const AValue: boolean);

     property    ContextMenuSideBar: TdxWinXPBar read FdxWinXPBarContext;
     property    dxContainer : TdxContainer read FdxContainer;
     property    FormErrorMessage: string Write SetFormErrorMessage;
     property    FormInfoMessage: string Write SetFormInfoMessage;
     property    StatusPanelMessage: string read GetStatusPannelMessage write SetStatusPannelMessage;
     property    OnGetShowModalMenu: TtiAMSOnGetShowModalMenuEvent read FOnGetShowModalMenu write FOnGetShowModalMenu;
   end;

function  gAMS: TtiApplicationMenuSystem;
procedure CreateAMS(
  const AMainForm : TForm;
  const AWorkListFormClass : TtiFormMgrFormClass;
  const AHelpFileName: string;
  const ADefHelpContext : Integer;
  const AFormAboutClass: TFormClass;
  const ALblMessageOnHotSpotClickEvent: TMessageOnHotSpotClickEvent;
  const AContactName: string = '';
  const AContactEmailAddress: string = '';
  const ADisplaySettings: TtiApplicationMenuSystemDisplaySetings = nil);

implementation
uses
  Types
  ,HTMLHelpViewer
  ,tiImageMgr
  ,tiThreadProgress
  ,tiResources
  ,tiConstants
  ,tiGUIConstants
//  ,FAbout
  ,tiExcept
  ,tiDialogs
  // HTML Viewer
  ,HTMLUn2
  ,URLSubs
 ;

const
  CBorder = 4;
  CApplicationBusyToolbarImageRightSpace = 4; // Space on right of toolbar image

var
  uAMS: TtiApplicationMenuSystem;

function  gAMS: TtiApplicationMenuSystem;
begin
  Assert(uAMS.TestValid(TtiApplicationMenuSystem), CTIErrorInvalidObject);
  Result := uAMS;
end;

procedure CreateAMS(
  const AMainForm : TForm;
  const AWorkListFormClass : TtiFormMgrFormClass;
  const AHelpFileName: string;
  const ADefHelpContext : Integer;
  const AFormAboutClass: TFormClass;
  const ALblMessageOnHotSpotClickEvent: TMessageOnHotSpotClickEvent;
  const AContactName: string = '';
  const AContactEmailAddress: string = '';
  const ADisplaySettings: TtiApplicationMenuSystemDisplaySetings = nil);
begin
  Assert(uAMS = nil, 'AMS already created');
  uAMS := TtiApplicationMenuSystem.Create(
    AMainForm,
    AWorkListFormClass,
    AHelpFileName,
    ADefHelpContext,
    AFormAboutClass,
    ALblMessageOnHotSpotClickEvent,
    AContactName,
    AContactEmailAddress,
    ADisplaySettings);
end;

{ TtiApplicationBusyToolbarImage }

constructor TtiApplicationBusyToolbarImage.Create(Toolbar: TTBXToolbar);
begin
  Assert(Toolbar <> nil, 'Toolbar not assigned');
  inherited Create;

  FVisible := False;
  FToolbar := Toolbar;

  FAnimatedGIFSpaceLabel := TLabel.Create(FToolbar);
  FAnimatedGIFSpaceLabel.Name := tiGetUniqueComponentName('AnimatedGIFSpaceLabel');
  FAnimatedGIFSpaceLabel.AutoSize := False;
  FAnimatedGIFSpaceLabel.Transparent := True;
  FAnimatedGIFSpaceLabel.Height := 16;
  FAnimatedGIFSpaceLabel.Caption := '';
  AnimateGIF(False);

end;

procedure TtiApplicationBusyToolbarImage.AnimateGIF(AAnimationEnabled: boolean);
begin
  if not Assigned(FAnimatedGIF) then
  begin
    FAnimatedGIF := TtiAnimatedGIF.Create(FToolbar);
    FAnimatedGIF.Name := tiGetUniqueComponentName('AnimatedGIF');
    FAnimatedGIF.AutoSize := True;
    FAnimatedGIF.ShowHint := True;
    FAnimatedGIF.AnimationSpeed:= FAnimationSpeed;
    FAnimatedGIF.ResourceName:= FResourceName;
    FAnimatedGIF.Transparent := true;
  end;
  FAnimatedGIF.AnimationEnabled:= AAnimationEnabled;
  if Visible then
    FAnimatedGIF.Parent := FToolbar;
  if AAnimationEnabled then
    FAnimatedGIF.OnHint:= FOnBusyHint
  else
    FAnimatedGIF.OnHint:= FOnNotBusyHint;
end;

function TtiApplicationBusyToolbarImage.GetAnimationEnabled: Boolean;
begin
  Result := FAnimationEnabled;
end;

procedure TtiApplicationBusyToolbarImage.SetAnimationEnabled(const AValue: Boolean);
begin
  if AValue <> FAnimationEnabled then
  begin
    AnimateGIF(AValue);
    FAnimationEnabled:= AValue;
  end;
end;

function TtiApplicationBusyToolbarImage.GetAnimationSpeed: Integer;
begin
  Result := FAnimationSpeed;
end;

procedure TtiApplicationBusyToolbarImage.SetAnimationSpeed(const AValue: Integer);
begin
  FAnimationSpeed:= AValue;
  FAnimatedGIF.AnimationSpeed := AValue;
end;

procedure TtiApplicationBusyToolbarImage.SetNotOnBusyHint(const AValue: TtiHintShowEvent);
begin
  FOnNotBusyHint:= AValue;
  if Not AnimationEnabled then
    FAnimatedGIF.OnHint := AValue;
end;

procedure TtiApplicationBusyToolbarImage.SetOnBusyHint(const AValue: TtiHintShowEvent);
begin
  FOnBusyHint:= AValue;
  if AnimationEnabled then
    FAnimatedGIF.OnHint := AValue;
end;

function TtiApplicationBusyToolbarImage.GetResourceName: string;
begin
  result:= FResourceName;
end;

procedure TtiApplicationBusyToolbarImage.SetResourceName(const AValue: string);
begin
  FResourceName:= AValue;
  FAnimatedGIF.ResourceName := AValue;
end;

procedure TtiApplicationBusyToolbarImage.SetVisible(const AValue: Boolean);
begin
  if AValue <> FVisible then begin
    FVisible := AValue;
    if FVisible then begin
      // Assigning parent adds the controls to the end (right) of the toolbar.
      FAnimatedGIFSpaceLabel.Parent := FToolbar;
      FAnimatedGIF.Parent := FToolbar;
    end else begin
      FAnimatedGIFSpaceLabel.Parent := nil;
      FAnimatedGIF.Parent := nil;
    end;
  end;
end;

procedure TtiApplicationBusyToolbarImage.Resize;
var
  ToolbarOuterSpaceWidth: Integer;
  ToolbarHandleWidth: Integer;
  ToolbarWidth: Integer;
begin
  if FVisible then begin
    ToolbarOuterSpaceWidth := 4;

    case FToolbar.DragHandleStyle of
      dhSingle: ToolbarHandleWidth := 6;
      dhDouble: ToolbarHandleWidth := 10;
    else
      ToolbarHandleWidth := 0;
    end;

    // Work-around for bug where toolbar width is not set correctly when the
    // form is resized and toolbar is Stretch or FullSize or MenuBar. Use the
    // parent control (generally the dock or form) width instead.
    if FToolbar.Stretch or FToolbar.FullSize or FToolbar.MenuBar then
      ToolbarWidth := (FToolbar.Parent.ClientWidth - FToolbar.Left)
    else
      ToolbarWidth := FToolbar.Width;

    // Size the space label to fill all room in toolbar other than image
    // and other toolbar controls.
    FAnimatedGIFSpaceLabel.Width := (ToolbarWidth - ToolbarOuterSpaceWidth - ToolbarHandleWidth) - FAnimatedGIFSpaceLabel.Left - (FAnimatedGIF.Width + CApplicationBusyToolbarImageRightSpace);

    // Always force a toolbar refresh.
    FToolbar.Realign;
  end;
end;

function TtiApplicationBusyToolbarImage.IsBusyItem(pToolbarItem: TTBCustomItem): Boolean;
begin
  if pToolbarItem is TTBControlItem then
    Result := ((pToolbarItem as TTBControlItem).Control = FAnimatedGIFSpaceLabel) or
              ((pToolbarItem as TTBControlItem).Control = FAnimatedGIF)
  else
    Result := False;
end;

{ TtiApplicationMenuSystem }

function TtiApplicationMenuSystem.AddMainMenuItem(const pCaption: string; AIndex : integer = -1):TTBXSubmenuItem;
var
  lIndex : integer;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(FMainMenuBar <> nil, 'FMainMenuBar not assigned');
  if AIndex = -1 then
    lIndex := FMainMenuBar.Items.Count
  else
    lIndex := AIndex;
  FLastTBXSubItem := TTBXSubmenuItem.Create(FMainForm);
  FLastTBXSubItem.Name := tiGetUniqueComponentName(tiToComponentName('mi' + pCaption));
  FMainMenuBar.Items.Insert(lIndex, FLastTBXSubItem);
  FLastTBXSubItem.Images := FtbImageList16;
  FLastTBXSubItem.Caption := pCaption;
  result := FLastTBXSubItem;
end;

function TtiApplicationMenuSystem.AddMenuItem(const pAction: TAction; const pTBXSubItem : TTBXSubMenuItem = nil): TTBXItem;
var
  lTBXSubMenuItem : TTBXSubmenuItem;
begin
  result := TTBXItem.Create(FMainForm);
  if pTBXSubItem <> nil then
    lTBXSubMenuItem := pTBXSubItem
  else
    lTBXSubMenuItem := FLastTBXSubItem;
  lTBXSubMenuItem.Add(result);
  result.Action := pAction;
end;

function TtiApplicationMenuSystem.AddMenuItemSeparator(
  const ATBXSubMenuItem: TTBXSubMenuItem): TTBXSeparatorItem;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(ATBXSubMenuItem <> nil, 'FLastTBXSubItem not assigend');
  result := TTBXSeparatorItem.Create(FMainForm);
  ATBXSubMenuItem.Add(result);
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
  Assert(FMainForm <> nil, 'pMainForm not assigned');
  Assert(FtbImageList24 <> nil, 'pImageList not assigned');
  FLastToolBar:= TTBXToolbar.Create(MainForm);
  FLastToolBar.Name := tiGetUniqueComponentName(tiToComponentName('tb' + pCaption));
  FLastToolBar.Parent := FTBDockTop;
  FLastToolBar.Caption := pCaption;
  FLastToolBar.Images := FtbImageList24;
  FLastToolBar.OnDockChanged   := OnMainFormReSize;
  FLastToolBar.OnVisibleChanged := OnMainFormReSize;
  FLastToolBar.ShowHint := true;
  FLastToolBar.DockMode := dmCannotFloatOrChangeDocks;
  FLastToolBar.DragHandleStyle := dhNone;
  result := FLastToolBar;
end;

function TtiApplicationMenuSystem.AddToolBarSeparator: TTBXSeparatorItem;
begin
  Assert(FMainForm <> nil, 'FMainForm not assigned');
  Assert(FLastToolBar <> nil, 'FLastTBXSubItem not assigend');
  result := TTBXSeparatorItem.Create(FMainForm);
  FLastToolBar.Items.Add(result);
end;

function TtiApplicationMenuSystem.AddDropDownToolButton(const pAction: TAction): TTBXSubmenuItem;
begin
  result := TTBXSubmenuItem.Create(MainForm);
  FLastToolBar.Items.Add(result);
  result.Action := pAction;
  result.DisplayMode := nbdmTextOnlyInMenus;
end;

constructor TtiApplicationMenuSystem.Create(
  const AMainForm : TForm;
  const AWorkListFormClass : TtiFormMgrFormClass;
  const AHelpFileName: string;
  const ADefHelpContext : Integer;
  const AAboutFormClass: TFormClass;
  const ALblMessageOnHotSpotClickEvent: TMessageOnHotSpotClickEvent;
  const AContactName: string = '';
  const AContactEmailAddress: string = '';
  const ADisplaySettings: TtiApplicationMenuSystemDisplaySetings = nil);
begin
  Assert(AMainForm <> nil, 'pMainForm not assigned');
  Assert(not Assigned(AMainForm.OnResize), 'pMainForm.OnResize assigned');
  inherited Create;
  FMainForm := AMainForm;
  FWorkListFormClass := AWorkListFormClass;
  FHelpFileName := AHelpFileName;
  FDefHelpContext := ADefHelpContext;
  FContactName := AContactName;
  FContactEmailAddress := AContactEmailAddress;
  if Assigned(ADisplaySettings) then
    FDisplaySettings := ADisplaySettings
  else
  begin
    FDefaultDisplaySettings := TtiApplicationMenuSystemDisplaySetings.Create;
    FDefaultDisplaySettings.BackgroundColor := clSkyBlue;
    FDefaultDisplaySettings.SidebarGroupBodyColor := $00F7DFD6;
    FDefaultDisplaySettings.SidebarGroupGradientFrom := clWhite;
    FDefaultDisplaySettings.SidebarGroupGradientTo := $00F7D7C6;
    FDisplaySettings := FDefaultDisplaySettings;
  end;

  FAL := TActionList.Create(MainForm);

  FImageListHot16    := TImageList.Create(FMainForm);
  FImageListDisabled16:= TImageList.Create(FMainForm);
  FImageListHot24    := TImageList.Create(FMainForm);
  FImageListHot24.Height := 24;
  FImageListHot24.Width := 24;
  FImageListDisabled24:= TImageList.Create(FMainForm);
  FImageListDisabled24.Height := 24;
  FImageListDisabled24.Width := 24;

  FtbImageList16               := TTBImageList.Create(FMainForm);
  FtbImageList16.DisabledImages := FImageListDisabled16;
  FtbImageList16.HotImages     := FImageListHot16;
  FtbImageList24               := TTBImageList.Create(FMainForm);
  FtbImageList24.Height := 24;
  FtbImageList24.Width := 24;
  FtbImageList24.DisabledImages := FImageListDisabled24;
  FtbImageList24.HotImages     := FImageListHot24;

  gTIImageListMgr.OwnsImageLists := false;
  gTIImageListMgr.ILNormal16  := FtbImageList16;
  gTIImageListMgr.ILHot16     := FImageListHot16;
  gTIImageListMgr.ILDisabled16 := FImageListDisabled16;
  gTIImageListMgr.ILNormal24  := FtbImageList24;
  gTIImageListMgr.ILHot24     := FImageListHot24;
  gTIImageListMgr.ILDisabled24 := FImageListDisabled24;
  gTIImageListMgr.LoadTIOPFImages;

  FFormMgr := TtiFormMgr.Create;
  FFormMgr.OnShowForm := DoOnShowForm;
  FFormMgr.OnBeginUpdate:= DoBeginUpdate;
  FFormMgr.OnEndUpdate:= DoEndUpdate;

  FUpdateCount := 0;
  FAboutFormClass := AAboutFormClass;

  CreateDockSystem;
  CreateContainerPanels;
  CreateInternalActions;
  CreateMainMenuBar;
  CreateHelpToolBar;
  CreateApplicationToolBar;
  CreateMenuSidebar;
  CreateHelpSystem;
  CreateContextMenuItems;
  FMainForm.OnResize := OnMainFormResize;
  FhtmlMessageOnHotSpotClickEvent:= ALblMessageOnHotSpotClickEvent;
  FEscapeContextAction:= nil;

end;

procedure TtiApplicationMenuSystem.DoOnShowForm(const pForm : TtiFormMgrForm);
var
  lList: TList;
begin
  if pForm <> nil then
  begin
    lList:= TList.Create;
    try
      // The FormMgr should know about the application menu system or the other
      // way around so we can remove this code from FMain
      pForm.AssignActions(lList);
      AddContextActions(lList);
      FormCaption := pForm.FormCaption;

      // A gastly hack to handle FormLinks (which has a white background)
      if pForm.Color <> clWhite then
        ParentPnl.Color := pForm.Color
      else
        ParentPnl.Color := FDisplaySettings.BackgroundColor;

    finally
      lList.Free;
    end;
    NavigationControlsEnabled := not pForm.IsModal;
  end else
    ClearContextActions;
end;

procedure TtiApplicationMenuSystem.CreateApplicationToolBar;
begin
  FtbxApplication := AddToolBar('Navigation');
  FtbxApplication.OnDockChanged := OnMainFormResize;
  FtbxApplication.OnVisibleChanged := OnMainFormResize;

  FtbxPrevForm := AddDropDownToolButton(FindAction(cAWindowPreviousToolbarNm));
  FtbxPrevForm.DropdownCombo := True;
  FtbxPrevForm.OnPopup := DoToolButtonFormPopup;
  FtbxPrevForm.DisplayMode := nbdmImageAndText;

  FtbxNextForm := AddDropDownToolButton(FindAction(cAWindowNextToolbarNm));
  FtbxNextForm.DropdownCombo := True;
  FtbxNextForm.OnPopup := DoToolButtonFormPopup;
  FtbxNextForm.DisplayMode := nbdmImageAndText;

  AddToolBarSeparator;
  AddToolButton(FindAction(caFileExitToolbarNm), nbdmImageAndText);

end;

procedure TtiApplicationMenuSystem.CreateHelpToolBar;
begin
  FtbxHelp := AddToolBar('Help');
  FtbxHelp.OnDockChanged := OnMainFormReSize;
  FtbxHelp.OnVisibleChanged := OnMainFormReSize;
  AddToolButton(FindAction(caHelpWhatsThisToolbarNm), nbdmTextOnlyInMenus);
  AddToolButton(FindAction(caHelpActiveFormToolbarNm), nbdmTextOnlyInMenus);
end;

procedure TtiApplicationMenuSystem.CreateMainMenuBar;
begin
  FMainMenuBar := TTBXToolbar.Create(FMainForm);
  FMainMenuBar.Name := tiGetUniqueComponentName('tbMainAMS');
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
  FMainMenuBar.ShrinkMode := tbsmNone;
  FMainMenuBar.TabOrder := 0;
  FMainMenuBar.HelpContext := FDefHelpContext;
  FMainMenuBar.Images := FtbImageList16;
  FMainMenuBar.OnDockChanged := OnMainFormReSize;
  FMainMenuBar.OnVisibleChanged := OnMainFormReSize;
  FMainMenuBar.DockMode := dmCannotFloatOrChangeDocks;
  FMainMenuBar.DragHandleStyle := dhNone;
  FMainMenuBar.OnResize := OnMainMenuBarResize;

  FtbxFileMenu := AddMainMenuItem('&File');
  AddMenuItem(FindAction(cAFileExitMenuNm));

//  FtbxViewMenu := AddMainMenuItem('&View');
//  FLastTBXSubItem.OnPopup := DoOnViewMenuPopup;
//  lTBXItem := AddMenuItem(FindAction(cAViewAppToolBarNm));
//  lTBXItem.ImageIndex := -1;
//  lTBXItem := AddMenuItem(FindAction(cAViewHelpToolBarNm));
//  lTBXItem.ImageIndex := -1;
//  lTBXItem := AddMenuItem(FindAction(cAViewMenuSideBarNm));
//  lTBXItem.ImageIndex := -1;
//  lTBXItem := AddMenuItem(FindAction(cAViewProgressWindowNm));
//  lTBXItem.ImageIndex := -1;

  FtbxWindowMenu := AddMainMenuItem('&Window');
  AddMenuItem(FindAction(cAWindowPreviousMenuNm));
  AddMenuItem(FindAction(cAWindowNextMenuNm));
  AddMenuItemSeparator;

  FtbxHelpMenu := AddMainMenuItem('&Help');
  AddMenuItem(FindAction(cAHelpContentsNm));
  AddMenuItem(FindAction(caHelpActiveFormMenuNm));
  AddMenuItem(FindAction(caHelpWhatsThisMenuNm));
  if (FContactName <> '') and (FContactEmailAddress <> '') then
    AddMenuItem(FindAction(caHelpContactNm));
  if Assigned(FAboutFormClass) then
    AddMenuItem(FindAction(caHelpAboutNm));

  FApplicationBusyToolbarImage := TtiApplicationBusyToolbarImage.Create(FMainMenuBar);
end;

procedure TtiApplicationMenuSystem.DoToolButtonFormPopup(Sender: TTBCustomItem; FromLink: Boolean);
  procedure _AddFormToMenu(const pCaption: string; const AIndex: Integer);
  var
    lItem : TTBXItem;
  begin
    lItem := TTBXItem.Create(Sender);
    Sender.Add(lItem);
    lItem.Caption := pCaption;
    lItem.OnClick := DoSelectActiveForm;
    lItem.Tag := AIndex;
  end;
var
  lsl : TStringList;
  i : integer;
begin
  lsl := TStringList.Create;
  try
    FormMgr.AssignFormList(lsl);
    Sender.Clear;
    if Sender = FtbxPrevForm then
      for i := 0 to lsl.Count - 1 do
        _AddFormToMenu(lsl.Strings[i], i)
    else
      for i := lsl.Count - 1 downto 0 do
        _AddFormToMenu(lsl.Strings[i], i)

  finally
    lsl.Free;
  end;
end;

procedure TtiApplicationMenuSystem.DoSelectActiveForm(Sender: TObject);
begin
  if not (Sender is TTBCustomItem) then
    Exit; //==>
  FormMgr.BringToFront(FormMgr.Forms[(Sender as TTBCustomItem).Tag], false);
end;


function TtiApplicationMenuSystem.AddToolButton(const pAction: TAction; pDisplayMode: TTBItemDisplayMode): TTBXItem;
begin
  result := TTBXItem.Create(MainForm);
  FLastToolBar.Items.Add(result);
  result.Action := pAction;
  result.DisplayMode := pDisplayMode;
end;

procedure TtiApplicationMenuSystem.CreateDockSystem;
var
  ltbMultiDockTop: TTBXMultiDock;
  ltbDockLeft: TTBXDock;
  ltbMultiDockLeft: TTBXMultiDock;
  ltbDockRight: TTBXDock;
  ltbMultiDockRight: TTBXMultiDock;
const
  cMessagePnlHeight = 80;
begin

  FStatusBar:= TTBXStatusBar.Create(FMainForm);
  FStatusBar.Name := tiGetUniqueComponentName('sbAMS');
  FStatusBar.Parent := FMainForm;
  FStatusBar.Font.Charset := DEFAULT_CHARSET;
  FStatusBar.Font.Color := clBlack;
  FStatusBar.Font.Height := -11;
  FStatusBar.Font.Name := 'Tahoma';
  FStatusBar.Font.Style := [];
  FStatusBar.HelpContext := FDefHelpContext;
  FStatusPanelHint := FStatusBar.Panels.Add;
  FStatusPanelHint.Size  := 220;
  FStatusPanelMessage:= FStatusBar.Panels.Add;
  FStatusPanelMessage.StretchPriority := 1;
  FStatusBar.ParentShowHint := False;
  FStatusBar.ShowHint := True;
  FStatusBar.UseSystemFont := True;

  FtbDockTop:= TTBXDock.Create(FMainForm);
  FtbDockTop.Name := tiGetUniqueComponentName('tbDockTop');
  FtbDockTop.Parent := FMainForm;
  FtbDockTop.Left := 0;
  FtbDockTop.Top := 0;
  FtbDockTop.Width := 792;
  FtbDockTop.Height := 9;
  FtbDockTop.Color := clAppWorkSpace;
  FtbDockTop.HelpContext := FDefHelpContext;

  ltbMultiDockTop:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockTop.Name := tiGetUniqueComponentName('tbMultiDockTop');
  ltbMultiDockTop.Parent := FMainForm;
  ltbMultiDockTop.Left := 0;
  ltbMultiDockTop.Top := 9;
  ltbMultiDockTop.Width := 792;
  ltbMultiDockTop.Height := 7;
  ltbMultiDockTop.FixAlign := True;
  ltbMultiDockTop.HelpContext := FDefHelpContext;

  ltbDockLeft:= TTBXDock.Create(FMainForm);
  ltbDockLeft.Name := tiGetUniqueComponentName('tbDockLeft');
  ltbDockLeft.Parent := FMainForm;
  ltbDockLeft.Left := 0;
  ltbDockLeft.Top := 16;
  ltbDockLeft.Width := 9;
  ltbDockLeft.Height := 573;
  ltbDockLeft.Position := dpLeft;
  ltbDockLeft.Color := clAppWorkSpace;
  ltbDockLeft.HelpContext := FDefHelpContext;

  ltbMultiDockLeft:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockLeft.Name := tiGetUniqueComponentName('tbMultiDockLeft');
  ltbMultiDockLeft.Parent := FMainForm;
  ltbMultiDockLeft.Left := 9;
  ltbMultiDockLeft.Top := 16;
  ltbMultiDockLeft.Width := 180;
  ltbMultiDockLeft.Height := 573;
  ltbMultiDockLeft.FixAlign := True;
  ltbMultiDockLeft.Position := dpLeft;
  ltbMultiDockLeft.HelpContext := FDefHelpContext;

  FdpMenuSidebar:= TTBXDockablePanel.Create(FMainForm);
  FdpMenuSidebar.Name := tiGetUniqueComponentName('dpMenuSidebar');
  FdpMenuSidebar.Caption := '';
  FdpMenuSidebar.Parent := ltbMultiDockLeft;
  FdpMenuSidebar.Left := 0;
  FdpMenuSidebar.Top := 0;
  FdpMenuSidebar.MinClientHeight := 64;
  FdpMenuSidebar.MinClientWidth := 80;
  FdpMenuSidebar.ShowCaptionWhenDocked := false;
  FdpMenuSidebar.DockedWidth := CMenuSideBarWidth;
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
  FdpMenuSidebar.CloseButtonWhenDocked := false;
  FdpMenuSidebar.DockMode := dmCannotFloatOrChangeDocks;

  FtbdBottom:= TTBXDock.Create(FMainForm);
  FtbdBottom.Name := tiGetUniqueComponentName('tbdBottom');
  FtbdBottom.Parent := FMainForm;
  FtbdBottom.Left := 0;
  FtbdBottom.Top := 661;
  FtbdBottom.Width := 792;
  FtbdBottom.Height := cMessagePnlHeight;
  FtbdBottom.FixAlign := True;
  FtbdBottom.Position := dpBottom;
  FtbdBottom.Color := clAppWorkSpace;
  FtbdBottom.HelpContext := FDefHelpContext;

  FtbMultiDockBottom:= TTBXMultiDock.Create(FMainForm);
  FtbMultiDockBottom.Name := tiGetUniqueComponentName('tbMultiDockBottom');
  FtbMultiDockBottom.Parent := FMainForm;
  FtbMultiDockBottom.Left := 0;
  FtbMultiDockBottom.Top := 589;
  FtbMultiDockBottom.Width := 792;
  FtbMultiDockBottom.Height := cMessagePnlHeight;
  FtbMultiDockBottom.FixAlign := True;
  FtbMultiDockBottom.Position := dpBottom;
  FtbMultiDockBottom.HelpContext := FDefHelpContext;

  FdpMessageBar:= TTBXDockablePanel.Create(FMainForm);
  FdpMessageBar.Name := tiGetUniqueComponentName('dpMessageBar');
  FdpMessageBar.Caption := '';
  FdpMessageBar.Parent := FtbMultiDockBottom;
  FdpMessageBar.Left := 0;
  FdpMessageBar.Top := 0;
  FdpMessageBar.BorderSize := 1;
  FdpMessageBar.Caption := '';
  FdpMessageBar.ShowCaption := False;
  FdpMessageBar.ShowCaptionWhenDocked := False;
  FdpMessageBar.Color := clWhite;
  FdpMessageBar.DockedWidth := 123;
  FdpMessageBar.DockedHeight := cMessagePnlHeight;
  FdpMessageBar.DockPos := 0;
  FdpMessageBar.FloatingWidth := 128;
  FdpMessageBar.FloatingHeight := 128;
  FdpMessageBar.SupportedDocks := [dkMultiDock];
  FdpMessageBar.TabOrder := 0;
  FdpMessageBar.HelpContext := FDefHelpContext;
  FdpMessageBar.OnDockChanged := OnMainFormReSize;
  FdpMessageBar.OnVisibleChanged:= OnMainFormReSize;
  FdpMessageBar.CloseButtonWhenDocked := false;
  FdpMessageBar.DockMode := dmCannotFloatOrChangeDocks;

  FPnlProgress:= TtiRoundedPanel.Create(FMainForm);
  FPnlProgress.Name := tiGetUniqueComponentName('PnlProgress');
  FPnlProgress.Caption := '';
  FPnlProgress.Visible := False;
  FPnlProgress.Parent:= FdpMessageBar;
  FPnlProgress.Align:= alClient;
  FPnlProgress.Color := clPaleBlue;
  FPnlProgress.BorderColor:= FDisplaySettings.BackgroundColor;
  FPnlProgress.HelpContext := FDefHelpContext;
  FPnlProgress.CornerRadius := 5;

  FPnlEmptyMessage := TtiRoundedPanel.Create(FMainForm);
  FPnlEmptyMessage.Name := tiGetUniqueComponentName('PnlEmptyMessage');
  FPnlEmptyMessage.Caption := '';
  FPnlEmptyMessage.Visible := False;
  FPnlEmptyMessage.Parent := FdpMessageBar;
  FPnlEmptyMessage.Align := alClient;
  FPnlEmptyMessage.Color := clWhite;
  FPnlEmptyMessage.BorderColor:= FDisplaySettings.BackgroundColor;
  FPnlEmptyMessage.HelpContext := FDefHelpContext;
  FPnlEmptyMessage.CornerRadius := 5;

  FPnlMessageText:= TtiRoundedPanel.Create(FMainForm);
  FPnlMessageText.Name := tiGetUniqueComponentName('PnlMessageText');
  FPnlMessageText.Caption := '';
  FPnlMessageText.Visible := False;
  FPnlMessageText.Parent:= FdpMessageBar;
  FPnlMessageText.Align:= alClient;
  FPnlMessageText.Color := clYellow;
  FPnlMessageText.BorderColor:= FDisplaySettings.BackgroundColor;
  FPnlMessageText.HelpContext := FDefHelpContext;
  FPnlMessageText.CornerRadius := 5;

  FhtmlMessage:= THTMLViewer.Create(FPnlMessageText);
  FhtmlMessage.Name := tiGetUniqueComponentName('htmlApplicationMessage');
  FhtmlMessage.Parent := FPnlMessageText;
  FhtmlMessage.Align  := alClient;
  FhtmlMessage.DefFontSize := 8;
  FhtmlMessage.MarginHeight := 0;
  FhtmlMessage.MarginWidth := 0;
  FhtmlMessage.BorderStyle := htNone;
  FhtmlMessage.DefFontName := FPnlMessageText.Font.Name;
  FhtmlMessage.OnHotSpotClick := DoLblMessageOnHotSpotClick;

  ltbDockRight:= TTBXDock.Create(FMainForm);
  ltbDockRight.Name := tiGetUniqueComponentName('tbDockRight');
  ltbDockRight.Parent := FMainForm;
  ltbDockRight.Left := 776;
  ltbDockRight.Top := 16;
  ltbDockRight.Width := 9;
  ltbDockRight.Height := 573;
  ltbDockRight.FixAlign := True;
  ltbDockRight.Position := dpRight;
  ltbDockRight.Color := clAppWorkSpace;
  ltbDockRight.HelpContext := FDefHelpContext;

  ltbMultiDockRight:= TTBXMultiDock.Create(FMainForm);
  ltbMultiDockRight.Name := tiGetUniqueComponentName('tbMultiDockRight');
  ltbMultiDockRight.Parent := FMainForm;
  ltbMultiDockRight.Left := 785;
  ltbMultiDockRight.Top := 16;
  ltbMultiDockRight.Width := 7;
  ltbMultiDockRight.Height := 573;
  ltbMultiDockRight.FixAlign := True;
  ltbMultiDockRight.Position := dpRight;
  ltbMultiDockRight.HelpContext := FDefHelpContext;

  TBXSetTheme('OfficeXP');

  DPMessageBar.Height                 := cMessagePnlHeight; // 46 Will show 2 progress bars
  gFormThreadProgress.BorderStyle     := bsNone;
  gFormThreadProgress.Parent          := FPnlProgress;
  gFormThreadProgress.Visible         := False;
  gFormThreadProgress.PanelBevelInner := bvNone;
  gFormThreadProgress.PanelBevelOuter := bvNone;
  gFormThreadProgress.Align           := alClient;
  gFormThreadProgress.Color           := clPaleBlue;
  gFormThreadProgress.ProgressBarColor := clSilver; //clAppWorkSpace;
  gFormThreadProgress.ColumnCount     := 0;
  gFormThreadProgress.Width           := FdpMessageBar.ClientWidth;
  gFormThreadProgress.HelpContext     := FDefHelpContext;
  gFormThreadProgress.OnVisibleChange := DoProgressVisibleChange;

  Application.OnHint := HintToStatusBar;
  Application.ShowHint:= True;

  FtbDockTop.OnResize := OnMainFormReSize;
  ltbMultiDockTop.OnResize := OnMainFormReSize;
  ltbDockLeft.OnResize := OnMainFormReSize;
  ltbMultiDockLeft.OnResize := OnMainFormReSize;
  FtbdBottom.OnResize := OnMainFormReSize;
  FtbMultiDockBottom.OnResize := OnMainFormReSize;
  ltbDockRight.OnResize := OnMainFormReSize;
  ltbMultiDockRight.OnResize := OnMainFormReSize;

end;

procedure TtiApplicationMenuSystem.CreateMenuSidebar;
begin
  FdxPageScroller:= TTBXPageScroller.Create(MainForm);
  FdxPageScroller.Name := tiGetUniqueComponentName('dxPageScroller');
  FdxPageScroller.Parent := FDPMenuSidebar;
  FdxPageScroller.Left := 0;
  FdxPageScroller.Top := 0;
  FdxPageScroller.Width := 176;
  FdxPageScroller.Height := 487;
  FdxPageScroller.Align := alClient;
  FdxPageScroller.AutoRange := True;
  FdxPageScroller.Color:= FDisplaySettings.BackgroundColor;
  FdxPageScroller.DoubleBuffered := True;
  FdxPageScroller.Margin := 10;
  FdxPageScroller.ParentColor := False;
  FdxPageScroller.TabOrder := 0;
  FdxPageScroller.Color := FpnlBorder.Color;
  FdxPageScroller.HelpContext := FDefHelpContext;

  FdxContainer:= TdxContainer.Create(MainForm);
  FdxContainer.Name := tiGetUniqueComponentName('dxContainer');
  FdxContainer.Parent := FdxPageScroller;
  FdxContainer.Left := 0;
  FdxContainer.Top := 0;
  FdxContainer.Width := 176;
  FdxContainer.Height := 742;
  FdxContainer.Align := alClient;
  FdxContainer.AutoSize := True;
  FdxContainer.BorderWidth := 5;
  FdxContainer.Caption := 'dxContainer';
  FdxContainer.Color:= FDisplaySettings.BackgroundColor;
  FdxContainer.ParentColor := False;
  FdxContainer.Color   := FpnlBorder.Color;
  FdxContainer.HelpContext := FDefHelpContext;

end;

function TtiApplicationMenuSystem.AddMenuSidebarGroup(
  const ACaption: string; const AName: string): TdxWinXPBar;
var
  LName: string;
begin
  if AName <> '' then
    LName := AName
  else
    LName := 'SidebarGroup';
  FLastMenuSidebarGroup:= TdxWinXPBar.Create(FDXContainer);
  FLastMenuSidebarGroup.Name := tiGetUniqueComponentNameFromParent(FDXContainer, LName);
  FLastMenuSidebarGroup.Parent := FDXContainer;
  FLastMenuSidebarGroup.Align := alTop;
  FLastMenuSidebarGroup.Font.Charset := DEFAULT_CHARSET;
  FLastMenuSidebarGroup.Font.Color := 15159552;
  FLastMenuSidebarGroup.Font.Height := -11;
  FLastMenuSidebarGroup.Font.Name := 'Tahoma';
  FLastMenuSidebarGroup.Font.Style := [];
  FLastMenuSidebarGroup.ParentFont := False;
  FLastMenuSidebarGroup.ParentShowHint := False;
  FLastMenuSidebarGroup.ShowHint := True;
  FLastMenuSidebarGroup.Caption := ACaption;
  FLastMenuSidebarGroup.HeaderFont.Charset := DEFAULT_CHARSET;
  FLastMenuSidebarGroup.HeaderFont.Color := clBlack;
  FLastMenuSidebarGroup.HeaderFont.Height := -11;
  //FLastMenuSidebarGroup.HeaderFont.Size := 9;
  FLastMenuSidebarGroup.HeaderFont.Name := 'Tahoma';
  FLastMenuSidebarGroup.HeaderFont.Style := [fsBold];
  FLastMenuSidebarGroup.ImageList := FtbImageList16;
  FLastMenuSidebarGroup.ShowHint := true;
  FLastMenuSidebarGroup.BodyColor := FDisplaySettings.SidebarGroupBodyColor;
  FLastMenuSidebarGroup.GradientFrom := FDisplaySettings.SidebarGroupGradientFrom;
  FLastMenuSidebarGroup.GradientTo := FDisplaySettings.SidebarGroupGradientTo;
  result := FLastMenuSidebarGroup;
end;

function TtiApplicationMenuSystem.AddMenuSidebarItem(const pAction: TAction;
                                                     const pSideBarGroup: TdxWinXPBar = nil): TdxWinXPBarItem;
var
  lSideBarGroup: TdxWinXPBar;
begin
  if pSideBarGroup <> nil then
    lSideBarGroup := pSideBarGroup
  else
    lSideBarGroup := FLastMenuSidebarGroup;
  result := lSideBarGroup.Items.Add;
  result.Name := tiToComponentName('sbi' + pAction.Caption);
  result.Action := pAction;
  // Side bar items don't use accelerator chars so change && to &
  Result.Caption := tiDecodeNonAcceleratorInCaption(pAction.Caption);
//  result.Hint := pAction.Hint;
end;

procedure TtiApplicationMenuSystem.CreateContainerPanels;
begin

  FpnlBorder:= TtiRoundedPanel.Create(FMainForm);
  FpnlBorder.Name := tiGetUniqueComponentName('pnlBorderAMS');
  FpnlBorder.Caption := '';
  FpnlBorder.Parent := FMainForm;
  FpnlBorder.Color := FDisplaySettings.BackgroundColor;
  FpnlBorder.Align := alClient;
  FpnlBorder.HelpContext := FDefHelpContext;
  FpnlBorder.Color := FDisplaySettings.BackgroundColor;
  FpnlBorder.BorderColor := clWhite;

  FpnlCaption:= TPanel.Create(FMainForm);
  FpnlCaption.Name := tiGetUniqueComponentName('pnlCaptionAMS');
  FpnlCaption.Caption := '';
  FpnlCaption.Parent := FpnlBorder;
  FpnlCaption.BevelOuter := bvNone;
  FpnlCaption.Color := FDisplaySettings.BackgroundColor;
  FpnlCaption.Align := alTop;
  FpnlCaption.Height := 33;
  FpnlCaption.HelpContext := FDefHelpContext;

  FlblCaption := TLabel.Create(FMainForm);
  FlblCaption.Name := tiGetUniqueComponentName('lblCaptionAMS');
  FlblCaption.Parent := FpnlCaption;
  FlblCaption.Caption := '';
  FlblCaption.Top := 8;
  FlblCaption.Left := 8;
  FlblCaption.Font.Size := 10;
  FlblCaption.Font.Color := clBlack;
  FlblCaption.Font.Style := [fsBold];
  FlblCaption.Font.Name := 'Tahoma';

  FpnlParent:= TtiRoundedPanel.Create(FMainForm);
  FpnlParent.Name := tiGetUniqueComponentName('pnlParentAMS');
  FpnlParent.Caption := '';
  FpnlParent.Parent := FPnlBorder;
  FPnlParent.Align := alClient;
  FpnlParent.HelpContext := FDefHelpContext;
  FpnlParent.Color := FDisplaySettings.BackgroundColor;
  FpnlParent.BorderColor := clWhite;
  FpnlParent.CornerRadius := 8; 

  FormMgr.ParentPnl   := ParentPnl;
  FormMgr.OnFormMessage := SetFormMessage;
end;

procedure TtiApplicationMenuSystem.OnMainFormReSize(Sender: TObject);
begin
  ArrangeMessagePanels;
end;

procedure TtiApplicationMenuSystem.HintToStatusBar(Sender: TObject);
begin
  FStatusPanelHint.Caption := '  ' + GetLongHint(Application.Hint);
end;

{$IFDEF DELPHI2010ORABOVE}
function TtiApplicationMenuSystem.FindFormInstance<T>: T;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is T then
    begin
      result := Screen.Forms[i] as T;
      break;
    end;
end;
{$ENDIF}

procedure TtiApplicationMenuSystem.WhatsThisGetContext(pSender, pHelpItem: TObject;
  pIsMenu: Boolean; var pHContext: THelpContext; pX, pY: Integer);
  function _GetTBXToolBarHelpContext(const HelpItem : TObject): THelpContext;
  var
     lV: TTBItemViewer;
  begin
    result := 0;
    if HelpItem is TTBXToolBar then
      lV := TTBXToolbar(HelpItem).View.ViewerFromPoint(TTBXToolBar(HelpItem).ScreenToClient(point(pX,pY)))
    else if (HelpItem is TTBXPopupWindow) then
      lV := TTBXPopupWindow(HelpItem).View.ViewerFromPoint(TTBXPopupWindow(HelpItem).ScreenToClient(point(pX,pY)))
    else
      raise EtiOPFProgrammerException.Create('Invalid class <' + HelpItem.ClassName + '>');
    if (lV <> nil) and
       (lV.Item <> nil) and
       (lV.Item.Action <> nil) then
      result := (lV.Item.Action as TAction).HelpContext;
  end;

  function _GetDXWinXPBarHelpContext(const HelpItem :TdxWinXPBar): THelpContext;
  var
    lPoint : TPoint;
    lY    : integer;
    i     : integer;
  begin
    result := 0;
    lPoint := HelpItem.ScreenToClient(Point(pX,pY));
    lY    := lPoint.Y;
    i     := (HelpItem.Height - 35) div lY;
    if i >= HelpItem.Items.Count then
      i := HelpItem.Items.Count - 1;
    if HelpItem.Items[i].Action <> nil then
      result := (HelpItem.Items[i].Action as TAction).HelpContext;
  end;

begin
  if (pHelpItem is TTBXToolBar) or
     (pHelpItem is TTBXPopupWindow) then
  begin
    pHContext := _GetTBXToolBarHelpContext(pHelpItem);
    Exit; //==>
  end;

  if (pHelpItem is TdxWinXPBar) then
  begin
    pHContext := _GetDXWinXPBarHelpContext(pHelpItem as TdxWinXPBar);
    Exit; //==>
  end;

  pHContext := GetHelpContext(pHelpItem);

end;

function TtiApplicationMenuSystem.GetHelpContext(const pHelpItem: TObject): THelpContext;
  function _GetHelpContext(const pHelpItem: TControl): integer;
  begin
    if pHelpItem.HelpContext <> 0 then
      result := pHelpItem.HelpContext
    else if (pHelpItem.Action <> nil) and
            (pHelpItem.Action is TAction) and
            ((pHelpItem.Action as TAction).HelpContext <> 0) then
      result := (pHelpItem.Action as TAction).HelpContext
    else if pHelpItem.Parent <> nil then
      result := _GetHelpContext(pHelpItem.Parent)
    else if (pHelpItem.Owner <> nil) and
       (pHelpItem.Owner is TControl) then
      result := _GetHelpContext(pHelpItem.Owner as TControl)
    else
      result := FDefHelpContext;
  end;
begin
  if pHelpItem is TControl then
    result := _GetHelpContext(pHelpItem as TControl)
  else
    result := 0;
end;

function TtiApplicationMenuSystem.GetMenuSideBarWidth: integer;
begin
  result:= FdpMenuSidebar.DockedWidth;
end;

function TtiApplicationMenuSystem.AddAction(const AName, pCaption, pHint: string;
  pImageIndex: integer; const pEvent: TNotifyEvent;
  pHelpContext : integer = 0): TAction;
begin
  Assert(AName <> '', 'AName not assigned');
  Assert(pCaption<>'', 'pCaption not assigned');
  Assert(pHint<>'', 'pHint not assigned');
  Assert(pImageIndex >= -1, 'pImageIndex < -1');
  Assert(Assigned(pEvent), 'pEvent not assigned');
  Assert(FindAction(AName)=nil, 'Action name <' + AName + '> not unique');
  result            := TAction.Create(FAL);
  result.ActionList := FAL;
  result.Caption    := pCaption;
  result.OnExecute  := pEvent;
  result.ImageIndex := pImageIndex;
  result.Hint       := pHint;
  result.Name       := AName;
  if pHelpContext <> 0 then
    result.HelpContext := pHelpContext
  else
    result.HelpContext := FDefHelpContext;
end;

function TtiApplicationMenuSystem.AddAction16(const AName : string;
  const pCaption : string; const pHint : string;
  const pResName : string; const pEvent : TNotifyEvent;
  pHelpContext : integer = 0): TAction;
var
  lImageIndex : integer;
begin
  lImageIndex := gTIImageListMgr.ImageIndex16(pResName);
  result :=AddAction(AName, pCaption, pHint, lImageIndex, pEvent, pHelpContext);
end;

function TtiApplicationMenuSystem.AddAction24(const AName : string;
  const pCaption : string; const pHint : string;
  const pResName : string; const pEvent : TNotifyEvent;
  pHelpContext : integer = 0): TAction;
var
  lImageIndex : integer;
begin
  lImageIndex := gTIImageListMgr.ImageIndex24(pResName);
  result :=AddAction(AName, pCaption, pHint, lImageIndex, pEvent, pHelpContext);
end;

procedure TtiApplicationMenuSystem.AddActionUpdateHandler(
  const AHandler: TActionEvent);
begin
  Assert(Assigned(AHandler), 'AHandler not assigned');
  Assert(not Assigned(FAL.OnUpdate), 'FAL.OnUpdate Assigned');
  FAL.OnUpdate:= AHandler;
end;

function TtiApplicationMenuSystem.FindAction(const AName: string): TAction;
var
  i : integer;
begin
  result := nil;  
  for i := 0 to FAL.ActionCount - 1 do
    if SameText(AName, FAL.Actions[i].Name) then
    begin
      result := FAL.Actions[i] as TAction;
      Exit; //==>
    end;
end;

procedure TtiApplicationMenuSystem.CreateInternalActions;
var
  lAction : TAction;
begin

  AddAction16(cAFileExitMenuNm, cAFileExitCptn, cAFileExitHnt, cResTI_Exit, DoFileExit);
  AddAction24(cAFileExitToolbarNm, cAFileExitCptn, cAFileExitHnt, cResTI_Exit, DoFileExit);

  FAViewAppToolBarNm    := AddAction(cAViewAppToolBarNm,     cAViewAppToolBarCptn,     cAViewAppToolBarHnt,     FDefHelpContext, DoToggleApplicationToolbarVisible);
  FAViewHelpToolbarNm   := AddAction(cAViewHelpToolbarNm,    cAViewHelpToolbarCptn,    cAViewHelpToolbarHnt,    FDefHelpContext, DoToggleHelpToolbarVisible);
  FAViewMenuSidebarNm   := AddAction(cAViewMenuSidebarNm,    cAViewMenuSidebarCptn,    cAViewMenuSidebarHnt,    FDefHelpContext, DoToggleMenuSidebarVisible);
  FAViewProgressWindowNm := AddAction(cAViewProgressWindowNm, cAViewProgressWindowCptn, cAViewProgressWindowHnt, FDefHelpContext, DoToggleProgressWindowVisible);

  FActionWindowPreviousMenu := AddAction16(cAWindowPreviousMenuNm, cAWindowPreviousCptn, cAWindowPreviousHnt, cResTI_ArrowLeft,        DoWindowPrevious);
  FActionWindowPreviousMenu.ShortCut := ShortCut(VK_LEFT, [ssAlt]);
  FActionWindowPreviousToolbar := AddAction24(cAWindowPreviousToolbarNm, cAWindowPreviousCptn, cAWindowPreviousHnt, cResTI_ArrowLeft,        DoWindowPrevious);
  FActionWindowPreviousToolbar.ShortCut := ShortCut(VK_LEFT, [ssAlt]);

  FActionWindowNextMenu := AddAction16(cAWindowNextMenuNm,     cAWindowNextCptn,     cAWindowNextHnt,     cResTI_ArrowRight,       DoWindowNext);
  FActionWindowNextMenu.ShortCut := ShortCut(VK_Right, [ssAlt]);
  FActionWindowNextToolbar := AddAction24(cAWindowNextToolbarNm,     cAWindowNextCptn,     cAWindowNextHnt,     cResTI_ArrowRight,       DoWindowNext);
  FActionWindowNextToolbar.ShortCut := ShortCut(VK_Right, [ssAlt]);

  lAction := AddAction16(cAHelpActiveFormMenuNm,    cAHelpActiveFormCptn, cAHelpActiveFormHnt, cResTI_Help,          DoHelpActiveForm);
  lAction.ShortCut := ShortCut(VK_F1, []);
  lAction := AddAction24(cAHelpActiveFormToolbarNm, cAHelpActiveFormCptn, cAHelpActiveFormHnt, cResTI_Help,          DoHelpActiveForm);
  lAction.ShortCut := ShortCut(VK_F1, []);
  lAction := AddAction16(cAHelpWhatsThisMenuNm,     cAHelpWhatsThisCptn,  cAHelpWhatsThisHnt,  cResTI_HelpWhatsThis, DoHelpWhatsThis);
  lAction.ShortCut := ShortCut(VK_F1, [ssShift]);
  lAction := AddAction24(cAHelpWhatsThisToolbarNm,  cAHelpWhatsThisCptn,  cAHelpWhatsThisHnt,  cResTI_HelpWhatsThis, DoHelpWhatsThis);
  lAction.ShortCut := ShortCut(VK_F1, [ssShift]);

  if (FContactName <> '') and (FContactEmailAddress <> '') then
    AddAction16(cAHelpContactNm, Format(cAHelpContactCptn, [FContactName]), Format(cAHelpContactHnt, [FContactName]), cResTI_Email, DoHelpContact);
  AddAction16(cAHelpAboutNm,     cAHelpAboutCptn,     cAHelpAboutHnt,     cResTI_HelpAbout,          DoHelpAbout);

  AddAction16(cAHelpContentsNm,  cAHelpContentsCptn,  cAHelpContentsHnt,  cResTI_Help,               DoHelpContents);
end;

procedure TtiApplicationMenuSystem.DoToggleApplicationToolbarVisible(Sender: TObject);
var
  lVisible : boolean;
begin
  lVisible := not FtbxApplication.Visible;
  FtbxApplication.Visible := lVisible;
  FindAction(cAViewAppToolBarNm).Checked := lVisible;
end;

procedure TtiApplicationMenuSystem.DoToggleHelpToolbarVisible(Sender: TObject);
var
  lVisible : boolean;
begin
  lVisible := not FtbxHelp.Visible;
  FtbxHelp.Visible := lVisible;
  FindAction(cAViewHelpToolbarNm).Checked := lVisible;
end;

procedure TtiApplicationMenuSystem.DoToggleMenuSidebarVisible(Sender: TObject);
var
  lVisible : boolean;
begin
  lVisible := not FdpMenuSidebar.Visible;
  FdpMenuSidebar.Visible := lVisible;
  FindAction(cAViewMenuSidebarNm).Checked := lVisible;
end;

procedure TtiApplicationMenuSystem.DoToggleProgressWindowVisible(Sender: TObject);
var
  lVisible : boolean;
begin
  lVisible := not FdpMessageBar.Visible;
  FdpMessageBar.Visible := lVisible;
  FindAction(cAViewProgressWindowNm).Checked := lVisible;
end;

//procedure TtiApplicationMenuSystem.DoOnViewMenuPopup(Sender: TTBCustomItem; FromLink: Boolean);
//begin
//  FAViewAppToolBarNm.Checked    := FtbxApplication.Visible;
//  FAViewHelpToolbarNm.Checked   := FtbxHelp.Visible;
//  FAViewMenuSidebarNm.Checked   := FdpMenuSidebar.Visible;
//  FAViewProgressWindowNm.Checked := FdpMessageBar.Visible;
//end;

procedure TtiApplicationMenuSystem.DoWindowNext(Sender: TObject);
begin
  FormMgr.ShowNextForm;
end;

procedure TtiApplicationMenuSystem.DoWindowPrevious(Sender: TObject);
begin
  FormMgr.ShowPrevForm;
end;

procedure TtiApplicationMenuSystem.DoFileExit(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TtiApplicationMenuSystem.DoHelpAbout(Sender: TObject);
var
  lForm: TForm;
begin
  Assert(FAboutFormClass <> nil, 'FAboutFormClass not assigned');
  lForm := FAboutFormClass.Create(nil);
  try
    lForm.ShowModal;
  finally
    lForm.Free;
  end;
end;

procedure TtiApplicationMenuSystem.DoHelpContact(Sender: TObject);
begin
  Assert(FContactName <> '', 'Contact name must be assigned');
  Assert(FContactEmailAddress <> '', 'Contact email address must be assigned');
  tiShellExecute(Format('mailto: <%s> %s?Subject=Enquiry', [FContactName, FContactEmailAddress]));
end;

procedure TtiApplicationMenuSystem.DoHelpContents(Sender: TObject);
begin
  Application.HelpShowTableOfContents;
end;

procedure TtiApplicationMenuSystem.DoHelpWhatsThis(Sender: TObject);
begin
  FWhatsThis.ContextHelp;
end;

procedure TtiApplicationMenuSystem.CreateHelpSystem;
begin

  if not FileExists(FHelpFileName) then
  begin
    FHelpAvailable := False;
    FindAction(cAHelpActiveFormMenuNm).Visible := false;
    FindAction(cAHelpActiveFormToolbarNm).Visible := false;
    FindAction(cAHelpWhatsThisMenuNm).Visible := false;
    FindAction(cAHelpWhatsThisToolbarNm).Visible := false;
    FindAction(cAHelpContentsNm).Visible := false;
    FtbxHelp.Visible := false;
    FAViewHelpToolbarNm.Visible := false;
  end else
  begin
    FHelpAvailable := True;
    FWhatsThis := TWhatsThis.Create(MainForm);
    FWhatsThis.F1Action := goContext; //goContext, goDefault, goTOC
    FWhatsThis.Options := [wtMenuRightClick{, wtNoContextHelp, wtNoContextMenu, wtInheritFormContext}];
    // (wtDirectHelp, wtMenuRightClick, wtNoContextHelp,
    //                     wtNoContextMenu, wtPopupOnForm, wtInheritFormContext,
    //                     wtUseTag, wtUseAppKey)

    FWhatsThis.PopupCaption := 'What'#39's this?';
    FWhatsThis.PopupHelpContext := 0;
    FWhatsThis.OnGetContext := WhatsThisGetContext;
    FWhatsThis.OnGetActiveWindow := DoOnWhatsThisGetActiveWindow;
    FWhatsThis.Active := True;
    FWhatsThis.Loaded;
  end;

end;

procedure TtiApplicationMenuSystem.HelpJump(const pHelpTopic: string);
begin
  Application.HelpJump(pHelpTopic);
end;

procedure TtiApplicationMenuSystem.HelpJump(pHelpContext: integer);
begin
  Application.HelpContext(pHelpContext);
end;

procedure TtiApplicationMenuSystem.HelpJump;
begin
  if (FormMgr.ActiveForm <> nil) and
     (FormMgr.ActiveForm.HelpContext <> 0) then
    HelpJump(FormMgr.ActiveForm.HelpContext)
  else
    HelpJump(FDefHelpContext);
end;

function TtiApplicationMenuSystem.DoOnWhatsThisGetActiveWindow: TWinControl;
begin
  if FormMgr.ActiveForm <> nil then
    result := FormMgr.ActiveForm
  else
    result := nil;
end;

procedure TtiApplicationMenuSystem.DoHelpActiveForm(Sender: TObject);
begin
  HelpJump;
end;

function TtiApplicationMenuSystem.GetParentPnl: TPanel;
begin
  result := TPanel(FpnlParent);
end;

function TtiApplicationMenuSystem.GetStatusPannelMessage: string;
begin
  result:= FStatusPanelMessage.Caption;
end;

procedure TtiApplicationMenuSystem.SetStatusPannelMessage(const AValue: string);
begin
  FStatusPanelMessage.Caption:= AValue;
end;

procedure TtiApplicationMenuSystem.SetFormMessage(const AMessage: string; pMessageType: TtiUserFeedbackMessageType);
var
  LMessage: string;
begin
  // ToDo: Must sort this out so the two can co-exist
  if AMessage = '' then
  begin
    FhtmlMessage.LoadFromString('');
    FpnlMessageText.Visible := false;
  end
  else
  begin
    // Sould be able to control color of message from outside the AMS
    case pMessageType of
    tiufmtInfo: begin
                   FPnlMessageText.Color := clPaleBlue;
                   FhtmlMessage.OnClick := DoLblMessageClick;
                   FhtmlMessage.Hint := cMessageWindowHint;
                   FhtmlMessage.ShowHint := True;
                end;
    tiufmtError: begin
                   FPnlMessageText.Color := clYellow;
                   FhtmlMessage.OnClick := nil;
                   FhtmlMessage.Hint := '';
                   FhtmlMessage.ShowHint := False;
                 end;
    else
      raise EtiOPFProgrammerException.Create('Invalid pMessageType');
    end;

    FhtmlMessage.DefBackground := FPnlMessageText.Color;
    LMessage := tiStrTran(AMessage, CrLf, Cr);
    LMessage := tiStrTran(LMessage, Lf, Cr);
    LMessage := tiStrTran(LMessage, Cr, '<br>');
    FhtmlMessage.LoadFromString('<strong>' + LMessage + '</strong>');
    FpnlMessageText.Visible := True;
  end;

  ArrangeMessagePanels;
end;

procedure TtiApplicationMenuSystem.SetMenuSideBarWidth(const AValue: integer);
begin
  FdpMenuSidebar.DockedWidth:= AValue;
end;

procedure TtiApplicationMenuSystem.CreateContextMenuItems;
begin
  FtbxContextMenu   := AddMainMenuItem('Context',1);
  FtbxContextMenu.Visible := False;
  FdxWinXPBarContext := AddMenuSidebarGroup('Context', 'Context');
  FdxWinXPBarContext.Visible := False;
end;

procedure TtiApplicationMenuSystem.AddContextAction(const AAction: TAction);
begin
  AddMenuItem(AAction, FtbxContextMenu);
  AddMenuSidebarItem(AAction, FdxWinXPBarContext);
  if (AAction.ShortCut = Shortcut(Word(VK_ESCAPE), [])) then
  begin
    Assert(FEscapeContextAction = nil, 'EscapeContextAction <> nil');
    FEscapeContextAction:= AAction;
  end;
end;

procedure TtiApplicationMenuSystem.ClearContextActions;
begin
  FEscapeContextAction:= nil;
  FtbxContextMenu.Visible := False;
  FdxWinXPBarContext.Visible := False;
  FtbxContextMenu.Clear;
  FdxWinXPBarContext.Clear;

  if Assigned(FApplicationBusyToolbarImage) then
    FApplicationBusyToolbarImage.Resize;
end;

procedure TtiApplicationMenuSystem.AddContextActions(const AList: TList);
var
  i : Integer;
begin
  ClearContextActions;
  for i := 0 to AList.Count - 1 do
    AddContextAction((TObject(AList.Items[i]) as TAction) as TAction);
  if AList.Count > 0 then
  begin
    FtbxContextMenu.Visible := True;
    FdxWinXPBarContext.Visible := True;
  end;

  if Assigned(FApplicationBusyToolbarImage) then
    FApplicationBusyToolbarImage.Resize;
end;

procedure TtiApplicationMenuSystem.SetNavigationControlsEnabled(const AValue: Boolean);
var
  i : Integer;
  LVisible: Boolean;
begin
  FNavigationControlsEnabled := AValue;
  DoBeginUpdate(nil);
  try
    for i := 0 to Pred(FdxContainer.ControlCount) do
      if FdxContainer.Controls[i] <> FdxWinXPBarContext then
      begin
        if AValue and not FdxContainer.Controls[i].Visible then
          FdxContainer.Controls[i].Top := FdxContainer.Height-2;  // Make sure the group goes to the bottom of those aligned to alTop
        FdxContainer.Controls[i].Visible := AValue;
      end;

    for i := 0 to FMainMenuBar.Items.Count - 1 do
      if (FMainMenuBar.Items[i] <> FtbxFileMenu) and
//         (FMainMenuBar.Items[i] <> FtbxViewMenu) and
         (FMainMenuBar.Items[i] <> FtbxWindowMenu) and
         (FMainMenuBar.Items[i] <> FtbxHelpMenu) and
         (FMainMenuBar.Items[i] <> FtbxContextMenu) and
         ((not Assigned(FApplicationBusyToolbarImage)) or
           (not FApplicationBusyToolbarImage.IsBusyItem(FMainMenuBar.Items[i]))) then
      begin
        LVisible := AValue;
        if Assigned(FOnGetShowModalMenu) and (FMainMenuBar.Items[i] is TTBXSubmenuItem) then
          FOnGetShowModalMenu(FMainMenuBar.Items[i] as TTBXSubmenuItem, LVisible);
        FMainMenuBar.Items[i].Visible := LVisible;
      end;

    FActionWindowNextMenu.Enabled        := (FormMgr.FormCount > 1)   and (AValue);
    FActionWindowNextToolbar.Enabled     := (FormMgr.FormCount > 1)   and (AValue);
    FActionWindowPreviousMenu.Enabled    := (FormMgr.FormCount > 1)   and (AValue);
    FActionWindowPreviousToolbar.Enabled := (FormMgr.FormCount > 1)   and (AValue);

    if Assigned(FApplicationBusyToolbarImage) then
      FApplicationBusyToolbarImage.Resize;
  finally
    DoEndUpdate(nil);
  end;
end;

procedure TtiApplicationMenuSystem.MouseToContextMenuSideBar;
var
  lPt: TPoint;
begin
  if ContextMenuSideBar = nil then
    Exit; //==>
  if not ContextMenuSideBar.Visible  then
    ContextMenuSideBar.Visible := true;
  if ContextMenuSideBar.Collapsed then
    ContextMenuSideBar.Collapsed := false;
  lPt := Point(30, 50);
  lPt := ContextMenuSideBar.ClientToScreen(lPt);
  SetCursorPos(lPt.x, lPt.y);
end;

function TtiApplicationMenuSystem.GetFormCaption: TCaption;
begin
  Result := FlblCaption.Caption;
end;

procedure TtiApplicationMenuSystem.SetEscapeKeyEnabled(const AValue: boolean);
begin
  if FEscapeContextAction = nil then
    Exit; //==>

  if AValue then
    FEscapeContextAction.ShortCut := Shortcut(Word(VK_ESCAPE), [])
  else
    FEscapeContextAction.ShortCut:= 0;

  FormMgr.ActiveForm.EscapeKeyEnabled := AValue;
end;

procedure TtiApplicationMenuSystem.SetFormCaption(const AValue: TCaption);
begin
  FlblCaption.Caption := AValue;
end;

destructor TtiApplicationMenuSystem.Destroy;
begin
  FreeAndNil(FApplicationBusyToolbarImage);
  FreeAndNil(FBruteForceNoFlicker);
  FFormMgr.Free;
  FDefaultDisplaySettings.Free;
  inherited;
end;

procedure TtiApplicationMenuSystem.DoBeginUpdate(Sender: TObject);
begin
  if FUpdateCount = 0 then
  begin
    FBruteForceNoFlicker := TtiBruteForceNoFlicker.Create(FdxContainer);
    FdxContainer.BeginUpdate;
  end;
  Inc(FUpdateCount);
end;

procedure TtiApplicationMenuSystem.DoEndUpdate(Sender: TObject);
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    FdxContainer.EndUpdate;
    FreeAndNil(FBruteForceNoFlicker);
  end;
end;

procedure TtiApplicationMenuSystem.SetFormErrorMessage(const AMessage: string);
begin
  SetFormMessage(AMessage, tiufmtError);
end;

procedure TtiApplicationMenuSystem.SetFormInfoMessage(const AMessage: string);
begin
  SetFormMessage(AMessage, tiufmtInfo);
end;

procedure TtiApplicationMenuSystem.DoLBLMessageClick(Sender: TObject);
begin
  SetFormMessage('', tiufmtInfo);
end;

procedure TtiApplicationMenuSystem.DoLblMessageOnHotSpotClick(ASender: TObject;
  const ASRC: string; var AHandled: boolean);
var
  LSrc: string;
begin
  // 'file://...'
  if Pos(CTIProtocolFile + ':', ASrc) = 1 then
  begin
    LSrc := HTMLToDos(ASrc);
    try
      tiOpenFile(LSrc);
    except
      on E: Exception do
        FormErrorMessage := E.Message + CrLf + LSrc;
    end;
    AHandled := true;
  end
  // 'self://...'
  else if Pos(CTIProtocolSelf + ':', ASrc) = 1 then
  begin
    if Assigned(FhtmlMessageOnHotSpotClickEvent) then
      FhtmlMessageOnHotSpotClickEvent(ASrc);
    AHandled := true;
  end
  // 'http://...' or 'mailto://...'
  else if (Pos(CTIProtocolHTTP + ':', ASrc) = 1) or
          (Pos(CTIProtocolMailTo + ':', ASrc) = 1) then
  begin
    tiShellExecute(ASrc);
    AHandled := true;
  end;
end;

procedure TtiApplicationMenuSystem.OnMainMenuBarResize(Sender: TObject);
begin
  if Assigned(FApplicationBusyToolbarImage) then
    FApplicationBusyToolbarImage.Resize;
end;

// Thread progress and message text panels need to co-exist.
// If only one visible then use all available space, else share equally.
procedure TtiApplicationMenuSystem.ArrangeMessagePanels;
begin
  Assert(FPnlProgress.Parent <> nil, 'Progress panel parent not assigned');
  if gFormThreadProgress.Visible and (not FPnlMessageText.Visible) then
    FPnlProgress.Align := alClient
  else if FPnlMessageText.Visible and (not gFormThreadProgress.Visible) then
    FPnlMessageText.Align := alClient
  else if gFormThreadProgress.Visible and FPnlMessageText.Visible then
  begin
    FPnlProgress.Align := alLeft;
    FPnlProgress.Width := FPnlProgress.Parent.ClientWidth div 2;
  end;

  FPnlProgress.Visible := gFormThreadProgress.Visible;
  FPnlEmptyMessage.Visible := (not gFormThreadProgress.Visible) and
      (not FPnlMessageText.Visible);

  if gFormThreadProgress.Visible and FPnlMessageText.Visible then
    FPnlMessageText.Align := alClient;
end;

procedure TtiApplicationMenuSystem.DoProgressVisibleChange(const AVisible: Boolean);
begin
  ArrangeMessagePanels;
end;

initialization
finalization
  uAMS.Free;
  uAMS := nil;

end.

