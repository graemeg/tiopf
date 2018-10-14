unit tiVTTreeView;

{$I tiDefines.inc}

interface

{$IFDEF VIRTUAL_TREEVIEW}

uses
{$IFNDEF FPC}
  Windows
  ,Messages
//  ,tiVirtualTrees
{$ELSE}
  LMessages
  ,LCLIntf
  ,LCLProc
  ,VirtualStringTree
{$ENDIF}
  ,VirtualTrees
  ,tiVirtualTreesNEW
  ,Classes
  ,Menus
  ,Graphics
  ,ExtCtrls
  ,Controls
  ,ImgList
  ,Forms
  ,Contnrs
  ,tiFocusPanel
  ,tiCtrlButtonPanel
  ,tiObject
  ,tiVTAbstract
  ,tiVTSearch  
 ;

type


  TtiVTTreeView = class;

  TtiVTTVNodeEvent = procedure(ptiVTTreeView: TtiVTTreeView;
    pNode: PVirtualNode;   // What ever data type you suggest here. Want to have access to the node to delete, paint, etc
    AData: TtiObject) of object;

  TtiVTTVNodeConfirmEvent = procedure(ptiVTTreeView: TtiVTTreeView;
    pNode: PVirtualNode; // What ever data type you suggest here. Want to have access to the node to delete, paint, etc
    AData: TtiObject;
    var pConfirm: boolean) of object;

  TtiVTTVOnFilterDataEvent = procedure(AData: TtiObject; var pInclude: boolean) of object;

  TtiVTTVDeriveNodeText = procedure(
    AtiVTTreeView: TtiVTTreeView;
    ANode: PVirtualNode;
    AData: TtiObject;
    var ANodeText: UnicodeString) of object;

  TtiVTTVOnPaintText = procedure(
    AtiVTTreeView: TtiVTTreeView;
    ANode: PVirtualNode;
    AData: TtiObject;
    ACanvas : TCanvas) of object;

  TtiVTTVNodeCheckboxClickEvent = procedure(
    AtiVTTreeView: TtiVTTreeView;
    ANode: PVirtualNode;   // What ever data type you suggest here. Want to have access to the node to delete, paint, etc
    AData: TtiObject;
    ASetChecked: Boolean) of object;

  // Does not have to be a collection. Perhaps better if it's not because one of the
  // main properties is a class reference
  TtiVTTVDataMapping = class(TCollectionItem)
  private
    FCanDelete: boolean;
    FCanEdit: boolean;
    FCanInsert: boolean;
    FCanView: Boolean;
    FUseCheckbox: Boolean;
    FImageIndex: integer;
    FDisplayPropName: string;
    FDataClass: TtiClass;
    FOnDeriveNodeText: TtiVTTVDeriveNodeText;
    FOnCanEdit: TtiVTTVNodeConfirmEvent;
    FOnCanInsert: TtiVTTVNodeConfirmEvent;
    FOnCanDelete: TtiVTTVNodeConfirmEvent;
    FOnCanView: TtiVTTVNodeConfirmEvent;
    FOnInsert: TtiVTTVNodeEvent;
    FOnEdit: TtiVTTVNodeEvent;
    FOnDelete: TtiVTTVNodeEvent;
  protected
  published
    // Properties
    property CanDelete: boolean read FCanDelete write FCanDelete default false;
    property CanInsert: boolean read FCanInsert write FCanInsert default false;
    property CanEdit:   boolean read FCanEdit   write FCanEdit default false;
    property CanView:   boolean read FCanView   write FCanView default false;

    property UseCheckBox: boolean read FUseCheckbox write FUseCheckbox default false;

    property DataClass: TtiClass   read FDataClass write FDataClass;
    property DisplayPropName: string      read FDisplayPropName write FDisplayPropName;
    property ImageIndex: integer          read FImageIndex write FImageIndex default -1;

    // Events
    property OnDeriveNodeText: TtiVTTVDeriveNodeText read FOnDeriveNodeText write FOnDeriveNodeText;

    property OnInsert: TtiVTTVNodeEvent read FOnInsert write FOnInsert;
    property OnDelete: TtiVTTVNodeEvent read FOnDelete write FOnDelete;
    property OnEdit: TtiVTTVNodeEvent read FOnEdit write FOnEdit;

    property OnCanView:   TtiVTTVNodeConfirmEvent read FOnCanView write FOnCanView;
    property OnCanInsert: TtiVTTVNodeConfirmEvent read FOnCanInsert write FOnCanInsert;
    property OnCanDelete: TtiVTTVNodeConfirmEvent read FOnCanDelete write FOnCanDelete;
    property OnCanEdit: TtiVTTVNodeConfirmEvent read FOnCanEdit write FOnCanEdit;

  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  end;


  // Don't mind if this is not a collection
  TtiVTTVDataMappings = class(TCollection)
  private
    FTreeView: TtiVTTreeView;
    FiItemNo: integer;
    function GetItem(Index: Integer): TtiVTTVDataMapping;
    procedure SetItem(Index: Integer; const AValue: TtiVTTVDataMapping);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(pTreeView: TtiVTTreeView);
    procedure Assign(Source: TPersistent); override;
    function Add: TtiVTTVDataMapping; overload;
    function Add(const AClass: TtiClass; const pDisplayPropName: string; pImageIndex: Integer): TtiVTTVDataMapping; overload;
    property Items[Index: Integer]: TtiVTTVDataMapping read GetItem write SetItem;
  end;


  TTVPopupMenu = class(TPopupMenu)
  private
    FmiInsert: TMenuItem;
    FmiDelete: TMenuItem;
    FmiEdit: TMenuItem;
    FmiExpand: TMenuItem;
    FmiExpandAll: TMenuItem;
    FVT: TtiVTTreeView;
    procedure DoInsert(sender: TObject);
    procedure DoDelete(sender: TObject);
    procedure DoEdit(sender: TObject);
    procedure DoExpand(sender: TObject);
    procedure DoExpandAll(sender: TObject);
    procedure DoOnPopup(sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VT: TtiVTTreeView read FVT write FVT;
  end;

//  This class simplifies the presentation of a TtiOject/TtiOjectList-based hierarchy,
//  the Container, as a treeview.
//  Requirements:
//  1) The Container must inherit from TtiObject, and:
//     i) be or inherit from TtiObjectList, or
//     ii) have published properties that could qualify as Containers (recursive definition)
//  2) Each node-level on the treeview must have an associated TtiVTTVDataMapping
//  3) For a node to have child-nodes, the associated data type in the TtiVTTVDataMappings 
//     must qualify as a Container
//  Usage:     
//  1) Call TtiVTTreeView.DataMappings.Add(...) for each type in Container you wish to appear
//     as a node in the treeview
//  2) The root Container is assigned to TtiVTTreeView.Data

  TtiVTTreeView = class(TtiVTAbstract)
  private
    FData: TtiObject;
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs;
    FVTDefaultDataMapping: TtiVTTVDataMapping;
    FVTDataMappings: TtiVTTVDataMappings;
    FPopupMenu: TTVPopupMenu;
    FOnSelectNode: TtiVTTVNodeEvent;
    FOnDblClick: TtiVTTVNodeEvent;
    FReadOnly: Boolean;
    FbSettingData: Boolean;
    FsSelectedAddress: string;
    FOwnsData: Boolean;
    FApplyFilter: Boolean;
    FOnFilter: TtiVTTVOnFilterDataEvent;
    FOnNodeCheckboxClick: TtiVTTVNodeCheckboxClickEvent;
    FOnGetImageIndex: TtiVTGetImageIndexEvent;
    FShowEmptyRootNode: boolean;
///
    FpmiShowFind: TMenuItem;
    FSearching: boolean;
    procedure SetSearching(const AValue: boolean);
///
    procedure DoOnChange(sender: TBaseVirtualTree; node: PVirtualNode);
    procedure SetTVDataMappings(const AValue: TtiVTTVDataMappings);
    function  GetSelectedAddress: string;
    procedure SetSelectedAddress(const AValue: string);

    function  GetImages: TCustomImageList;
    procedure SetImages(const AValue: TCustomImageList);
    procedure DoOnEnter(Sender: TObject);
    procedure DoOnExit(Sender: TObject);

    procedure DoOnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoOnInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure DoOnFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoOnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString);
    procedure DoOnGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DoOnCheckChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoOnChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
        var Allowed: Boolean);

    function  GetSelectedData: TtiObject;
    procedure SetSelectedData(const AValue: TtiObject);
    function  GetOnKeyDown: TKeyEvent;
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    function  GetButtonStyle: TLVButtonStyle;
    function  GetVisibleButtons: TtiLVVisibleButtons;
    procedure SetButtonStyle(const AValue: TLVButtonStyle);
    procedure SetVisibleButtons(const AValue: TtiLVVisibleButtons);
    function  GetTreeOptions: TStringTreeOptions;
    procedure SetTreeOptions(const AValue: TStringTreeOptions);
    procedure SetApplyFilter(const AValue: Boolean);
    function  TestObjectAgainstFilter(AValue: TtiObject): Boolean;

    procedure SelectObjectOrOwner(AValue: TtiObject);
    function  GetDefaultText: WideString;
    procedure SetDefaultText(const AValue: WideString);
    procedure SetShowEmptyRootNode(const AValue: boolean);

  protected
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetChildControlNames; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DoReSize(Sender: TObject); virtual;
    function  IsUpdating: Boolean;
    procedure SetData(const AValue: TtiObject); virtual;
    procedure SetReadOnly(const AValue: boolean); virtual;

    function  CalcIfObjectHasChildren(AObject: TtiObject): Boolean;
    function  CalcNodeChildren(AObject: TtiObject): Integer; overload;
    function  CalcNodeChildren(Node: PVirtualNode): Integer; overload;
    function  CalcNodeChildren(AObject: TtiObject; AChildList: TtiObjectList): Integer; overload;
    function  IsMappingForObject(AtiVTTVDataMapping: TtiVTTVDataMapping; AObj: TtiObject): Boolean;
    function  CalcMappingForObject(pObj: TtiObject): TtiVTTVDataMapping;
///
    procedure VTSearchInsideNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      const SearchText: string; out Result: boolean); override;
    procedure DoShowFind(Sender: TObject); virtual;
    property Searching: boolean read FSearching write SetSearching default true;
///
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetFocus; override;

    procedure   Refresh; reintroduce;

    procedure   DoInsert(Sender: TObject);
    procedure   DoDelete(Sender: TObject);
    procedure   DoEdit(Sender: TObject);
    procedure   DoDblClick(Sender: TObject);

    procedure   FullExpand(pLevel: Integer = -1);
    procedure   FullCollapse;

    function    GetObjectFromNode(Node: PVirtualNode): TtiObject; override;
    function    GetNodeFromObject(const AData: TtiObject): PVirtualNode;

    function    GetListForNode(Node: PVirtualNode): TtiObjectList;
    procedure   SetObjectForNode(pNode: PVirtualNode;  AObject: TtiObject);
    function    GetMappingForNode(Node: PVirtualNode): TtiVTTVDataMapping;

    function    CanDelete: boolean;
    function    CanInsert: boolean;
    function    CanEdit: boolean;
    function    CanView: boolean;

    procedure   SetDataNodeCheckState(const AData: TtiObject; const ACheckState: TCheckState);
    function    GetDataNodeCheckState(const AData: TtiObject): TCheckState;
    procedure   EnableAllNodes;

    procedure   RefreshCurrentNode;

    property    Data: TtiObject read FData write SetData;
    property    SelectedData: TtiObject read GetSelectedData write SetSelectedData;
    property    OwnsData: Boolean read FOwnsData write FOwnsData;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Visible;
    property ShowFocusRect;

    property DataMappings: TtiVTTVDataMappings read FVTDataMappings write SetTVDataMappings;

    property ApplyFilter: Boolean read FApplyFilter write SetApplyFilter default False;
    property DefaultText: WideString read GetDefaultText write SetDefaultText;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property ButtonStyle : TLVButtonStyle read GetButtonStyle write SetButtonStyle default lvbsMicroButtons;
    property VisibleButtons : TtiLVVisibleButtons read GetVisibleButtons write SetVisibleButtons default [];
    property Images: TCustomImageList read GetImages write SetImages;
    property SelectedAddress: string read GetSelectedAddress write SetSelectedAddress;
    property TreeOptions: TStringTreeOptions read GetTreeOptions write SetTreeOptions;
    property ShowEmptyRootNode: boolean read FShowEmptyRootNode write SetShowEmptyRootNode default true;

    property OnDblClick: TtiVTTVNodeEvent read FOnDblClick Write FOnDblClick;
    property OnFilter: TtiVTTVOnFilterDataEvent read FOnFilter write FOnFilter;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnSelectNode: TtiVTTVNodeEvent read FOnSelectNode write FOnSelectNode;
    property OnGetImageIndex: TtiVTGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;

    property OnGetNodeHint;
    property OnPaintText;
    property OnAdvancedPaintText;
    property OnScroll;
    property OnCollapsed;
    property OnExpanded;

    property OnNodeCheckboxClick: TtiVTTVNodeCheckboxClickEvent read FOnNodeCheckboxClick write FOnNodeCheckboxClick;
  end;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

uses
   tiConstants
  ,tiImageMgr
  ,tiResources
  ,tiUtils
  ,tiGUIConstants
  ,tiRTTI
  ,tiGUIUtils
  ,SysUtils
  ,TypInfo
 ;

type
  TNodeDataRec = packed record
    NodeData: TtiObject;
    NodeMapping: TtiVTTVDataMapping;
    NodeChildren: TtiObjectList;
  end;
  PNodeDataRec = ^TNodeDataRec;

type
  TPropInfoArray = array of PPropInfo;

procedure GetObjectPropInfos(pObj: TtiObject; out pInfos: TPropInfoArray);
begin
  SetLength(pInfos, GetPropList(pObj.ClassInfo, [tkClass], nil));
  GetPropList(pObj.ClassInfo, [tkClass], PPropList(pInfos));
end;

// Take an object, and a string class name and retrun true if the object
// is, or descends from the string class name.
function IsClassOfType(AData: TObject; AClassName: string): boolean;
var
  lsClassName: string;
  lClass: TClass;
begin
  lsClassName := upperCase(AClassName);
  lClass := AData.ClassType;
  result := false;
  while (not result) and
    (lClass <> nil) do
  begin
    if UpperCase(lClass.ClassName) = lsClassName then
    begin
      result := true;
      break; //==>
    end;
    lClass := lClass.ClassParent;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiVTTreeView
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiVTTreeView.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];
  Height := 120;
  Width := 120;
  OnResize := DoResize;

  SetVT(TtiVirtualStringTree.Create(self));
  VT.Parent := Self;
  VT.Align := alClient;

  VT.NodeDataSize := SizeOf(TNodeDataRec);
  FVTDataMappings := TtiVTTVDataMappings.Create(self);
  VT.OnChange := DoOnChange;
  VT.OnDblClick := DoDblClick;
  VT.OnChecking := DoOnChecking;
  VT.OnChecked:= DoOnCheckChange;

  FPopupMenu := TTVPopupMenu.Create(nil);
///
  FSearching := true;

  FpmiShowFind         := TMenuItem.Create(self);
  FpmiShowFind.Caption := '&Find...';
  FpmiShowFind.OnClick := DoShowFind;
  FpmiShowFind.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Find);
  FpmiShowFind.Visible := true;
  FpmiShowFind.ShortCut := ShortCut(Word('F'), [ssCtrl]);
  FPopupMenu.Items.Add(FpmiShowFind);
///
  FPopupMenu.VT := self;
  VT.PopupMenu := FPopupMenu;

  {$IFDEF WINDOWS}
  VT.DragMode := dmManual;
  {$ENDIF}


  VT.OnEnter    := DoOnEnter;
  VT.OnExit     := DoOnExit;

  VT.OnInitNode := DoOnInitNode;
  VT.OnInitChildren := DoOnInitChildren;
  VT.OnFreeNode := DoOnFreeNode;

  VT.OnGetText := DoOnGetText;
  VT.OnGetImageIndex := DoOnGetImageIndex;

  VT.TreeOptions.AnimationOptions := VT.TreeOptions.AnimationOptions + [toAnimatedToggle];
  VT.CheckImageKind := ckSystemDefault;

  FVTDefaultDataMapping := TtiVTTVDataMapping.Create(nil);
  FVTDefaultDataMapping.DisplayPropName := 'Caption';
  FVTDefaultDataMapping.ImageIndex := 0;

  FReadOnly := false;
  FShowEmptyRootNode := true;
  SetButtonStyle(lvbsNormalButtons);
end;

destructor TtiVTTreeView.destroy;
begin
  FVTDefaultDataMapping.Free;
  FVTDataMappings.Free;
  FPopupMenu.Free;
  if FOwnsData then
    FreeAndNil(FData);
  inherited;
end;

procedure TtiVTTreeView.SetData(const AValue: TtiObject);
begin
  if FOwnsData then
    FreeAndNil(FData);

  FData := AValue;

  // We always have a single root node under which all other nodes appear. It
  // doesn't need to be like this as the virtual treeview has it's own internal
  // root node but can display many top level nodes without a single visible
  // parent. Allow this some day...
  if Assigned(FData) and (DataMappings.Count > 0) and
     (FShowEmptyRootNode or CalcIfObjectHasChildren(FData)) then
    VT.RootNodeCount := 1 //FData --- probably should have a "ShowRootNode" property like the TTreeView.  ipk: See TODO below.
  else
    VT.RootNodeCount := 0;

  VT.ClearSelection;
  
{ TODO : We have this: (toShowRoot in TreeOptions.PaintOptions)
         which is currently ignored.  I tried including it in above test
         but a RootNodeCount := 0 results in nothing showing.
         Do we need to set RootNodeCount to actual (first level) count? }
end;

procedure TtiVTTreeView.SetDataNodeCheckState(const AData: TtiObject;
  const ACheckState: TCheckState);
var
  LNode: PVirtualNode;
begin
  LNode := GetNodeFromObject(AData);

  if LNode = nil then
    exit;

  VT.CheckState[LNode] := ACheckState;
end;

procedure TtiVTTreeView.EnableAllNodes;
var
  LNode: PVirtualNode;
begin
  LNode := VT.RootNode.FirstChild;
  while Assigned(LNode) do
  begin
    Exclude(LNode.States, vsDisabled);
    LNode := VT.GetNext(LNode);
  end;
end;

procedure TtiVTTreeView.SetDefaultText(const AValue: WideString);
begin
  VT.DefaultText := AValue;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TTVPopupMenu
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TTVPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Images := gTIImageListMgr.ILNormal16;

  FmiExpandAll := TMenuItem.Create(nil);
  FmiExpandAll.Caption := 'Expand &all';
  FmiExpandAll.OnClick := DoExpandAll;
  Items.Add(FmiExpandAll);

  FmiExpand := TMenuItem.Create(nil);
  FmiExpand.Caption := 'E&xpand';
  FmiExpand.OnClick := DoExpand;
  FmiExpand.Shortcut := TextToShortcut('Enter');
  Items.Add(FmiExpand);

  FmiInsert := TMenuItem.Create(nil);
  FmiInsert.Caption := cCaptionNew;
  FmiInsert.OnClick := DoInsert;
  FmiInsert.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Insert);
  Items.Add(FmiInsert);

  FmiEdit := TMenuItem.Create(nil);
  FmiEdit.Caption := cCaptionEdit;
  FmiEdit.OnClick := DoEdit;
  FmiEdit.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Edit);
  Items.Add(FmiEdit);

  FmiDelete := TMenuItem.Create(nil);
  FmiDelete.Caption := cCaptionDelete;
  FmiDelete.OnClick := DoDelete;
  FmiDelete.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Delete);
  Items.Add(FmiDelete);

  OnPopup := DoOnPopup;

end;

destructor TTVPopupMenu.Destroy;
begin
  FmiInsert.Free;
  FmiEdit.Free;
  FmiDelete.Free;
  inherited;
end;

procedure TTVPopupMenu.DoDelete(sender: TObject);
begin
  VT.DoDelete(nil);
end;

procedure TTVPopupMenu.DoEdit(sender: TObject);
begin
  VT.DoEdit(nil);
end;

procedure TTVPopupMenu.DoExpand(sender: TObject);
begin
  VT.VT.Expanded[VT.VT.GetFirstSelected]:= not VT.VT.Expanded[VT.VT.GetFirstSelected];
end;

procedure TTVPopupMenu.DoExpandAll(sender: TObject);
begin
  VT.FullExpand;
end;

procedure TTVPopupMenu.DoInsert(sender: TObject);
begin
  VT.DoInsert(Sender);
end;

procedure TTVPopupMenu.DoOnPopup(sender: TObject);
begin
  FmiInsert.Visible := true;
  FmiEdit.Visible  := true;
  FmiDelete.Visible := true;

  FmiExpand.Enabled := VT.VT.HasChildren[VT.VT.GetFirstSelected];
  FmiInsert.Enabled := VT.CanInsert;
  FmiEdit.Enabled  := VT.CanEdit;
  FmiDelete.Enabled := VT.CanDelete;

  if VT.VT.Expanded[VT.VT.GetFirstSelected] then
    FmiExpand.Caption := 'C&ollapse'
  else
    FmiExpand.Caption := 'E&xpand';

  if FmiInsert.Visible and FmiInsert.Enabled then
    FmiInsert.Shortcut := TextToShortcut('Ins')
  else
    FmiInsert.Shortcut := TextToShortcut('');

  if FmiEdit.Visible and FmiEdit.Enabled then
    FmiEdit.Shortcut := TextToShortcut('Ctrl+Enter')
  else
    FmiEdit.Shortcut := TextToShortcut('');

  if FmiDelete.Visible and FmiDelete.Enabled then
    FmiDelete.Shortcut := TextToShortcut('Del')
  else
    FmiDelete.Shortcut := TextToShortcut('');
end;

procedure TtiVTTreeView.DoDelete(Sender : TObject);
var
  lNode: PVirtualNode;
  lData: TtiObject;
  lMapping: TtiVTTVDataMapping;
  lbCanDelete: Boolean;
begin
  if not CanDelete then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lData := GetObjectFromNode(lNode);
  lMapping := GetMappingForNode(lNode);

  if not Assigned(lMapping) then
    Exit; //==>

  if not Assigned(lMapping.OnDelete) then
    Exit; //==>

  lbCanDelete := lMapping.CanDelete;

  if Assigned(lMapping.OnCanDelete) then
    lMapping.OnCanDelete(Self, lNode, lData, lbCanDelete);

  if lbCanDelete then
    lMapping.OnDelete(self, lNode, lData);
end;

procedure TtiVTTreeView.DoInsert(Sender : TObject);
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
  lbCanInsert: boolean;
begin
  if not CanInsert then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if not Assigned(lMapping) then
    Exit; //==>

  // If there was a mapping with OnInsert assigned, then it gets priority
  if Assigned(lMapping.OnInsert) then
  begin
    lbCanInsert := lMapping.CanInsert;

    if Assigned(lMapping.OnCanInsert) then
      lMapping.OnCanInsert(self, lNode, GetObjectFromNode(lNode), lbCanInsert);

    if not lbCanInsert then
      Exit; //==>

    lMapping.OnInsert(self, lNode, GetObjectFromNode(lNode));
  end
  else
  begin
    // No OnInsert method for this mapping, so execute the default one.
  end;

  // An OnInsert method was called, so re-read the child nodes
  if lbCanInsert then
  begin
    VT.ReinitNode(lNode, True);

    // This is a bit of a work-around. We are assuming that any new nodes will
    // have been added to the end of the list (this is not such a bad assumption,
    // except when the node has been added by drag-and-drop). A better approach
    // would be to add a pointer for all the children to a TList, then to scan
    // the TList looking for a newly added data object. When this newly added
    // data object is found, use SetSelectedData to set the selected tree node.

    lNode := VT.GetLastChild(lNode);
    VT.Selected[lNode]:= True;
    VT.FocusedNode := lNode;
    VT.ScrollIntoView(lNode, True);
  end;
end;

procedure TtiVTTreeView.DoEdit(Sender : TObject);
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
  lbCanEdit: boolean;
begin
  if not CanEdit then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if not Assigned(lMapping) then
    Exit; //==>

  if Assigned(lMapping.OnEdit) then
  begin
    lbCanEdit := lMapping.CanEdit;

    if Assigned(lMapping.OnCanEdit) then
      lMapping.OnCanEdit(Self, lNode, GetObjectFromNode(lNode), lbCanEdit);

    if not lbCanEdit then
      Exit; //==>

    lMapping.OnEdit(Self, lNode, GetObjectFromNode(lNode));
  end;

  VT.RepaintNode(lNode);
end;

procedure TtiVTTreeView.DoOnChange(sender: TBaseVirtualTree; node: PVirtualNode);
begin
  if Assigned(Node) and Assigned(FOnSelectNode) then
    FOnSelectNode(Self, Node, GetObjectFromNode(Node));
  FCtrlBtnPnl.EnableButtons;
end;

procedure TtiVTTreeView.DoOnCheckChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LData: TtiObject;
  LCheckState: Boolean;
begin
  if Assigned(FOnNodeCheckboxClick) then
  begin
    LData:= GetObjectFromNode(Node);
    LCheckState:= Node.CheckState in [csCheckedNormal, csCheckedPressed];
    FOnNodeCheckboxClick(Self, Node, LData, LCheckState);
  end;

//    csUncheckedNormal,  // unchecked and not pressed
//    csUncheckedPressed, // unchecked and pressed
//    csCheckedNormal,    // checked and not pressed
//    csCheckedPressed,   // checked and pressed
//    csMixedNormal,      // 3-state check box and not pressed
//    csMixedPressed      // 3-state check box and pressed
end;

procedure TtiVTTreeView.DoOnChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
    var Allowed: Boolean);
begin
  if not assigned(FOnNodeCheckboxClick) then
    exit;

  if (Allowed) and (NewState in [csCheckedNormal, csCheckedPressed]) then
    VT.Expanded[Node] := true;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiVTTVDataMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

procedure TtiVTTVDataMapping.Assign(Source: TPersistent);
begin
  if Source is TtiVTTVDataMapping then
  begin
    with TtiVTTVDataMapping(Source) do
    begin
      Self.FCanDelete := FCanDelete;
      Self.FCanEdit := FCanEdit;
      Self.FCanInsert := FCanInsert;
      Self.FCanView := FCanView;
      Self.FImageIndex := FImageIndex;
      Self.FDisplayPropName := FDisplayPropName;
      Self.FDataClass := FDataClass;
      Self.FOnDeriveNodeText := FOnDeriveNodeText;
      Self.FOnCanEdit := FOnCanEdit;
      Self.FOnCanInsert := FOnCanInsert;
      Self.FOnCanDelete := FOnCanDelete;
      Self.FOnInsert := FOnInsert;
      Self.FOnEdit := FOnEdit;
      Self.FOnDelete := FOnDelete;
    end;
  end
  else
    inherited;
end;

constructor TtiVTTVDataMapping.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TtiVTTVDataMapping.Destroy;
begin
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiVTTVDataMappings
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiVTTVDataMappings.Create(pTreeView: TtiVTTreeView);
begin
  inherited Create(TtiVTTVDataMapping);
  FTreeView := pTreeView;
  FiItemNo := 0;
end;

function TtiVTTVDataMappings.Add: TtiVTTVDataMapping;
begin
  result := TtiVTTVDataMapping(inherited Add);
end;

function TtiVTTVDataMappings.GetItem(Index: Integer): TtiVTTVDataMapping;
begin
  Result := TtiVTTVDataMapping(inherited GetItem(Index));
end;



function TtiVTTVDataMappings.GetOwner: TPersistent;
begin
  result := FTreeView;
end;



procedure TtiVTTVDataMappings.SetItem(Index: Integer; const AValue: TtiVTTVDataMapping);
begin
  inherited SetItem(Index, AValue);
end;

procedure TtiVTTVDataMappings.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

procedure TtiVTTreeView.SetTVDataMappings(const AValue: TtiVTTVDataMappings);
begin
  FVTDataMappings.Assign(AValue);
end;

function TtiVTTreeView.GetSelectedAddress: string;
var
  lNode: PVirtualNode;
begin
  result := '';
  lNode := VT.GetFirstSelected;
  if not Assigned(lNode) then
    Exit; //==>

  Result := IntToStr(lNode{$IFDEF FPC}^{$ENDIF}.Index);
  while Assigned(lNode{$IFDEF FPC}^{$ENDIF}.Parent) and (lNode <> VT.RootNode) do // VT.RootNode is the invisible root node
  begin
    lNode := lNode{$IFDEF FPC}^{$ENDIF}.Parent;
    result := IntToStr(lNode{$IFDEF FPC}^{$ENDIF}.Index) + '.' + result;
  end;
end;


procedure TtiVTTreeView.SetSearching(const AValue: boolean);
begin
  FSearching := AValue;
  FpmiShowFind.Visible := AValue;
  FpmiShowFind.Enabled := AValue;
//  FpmiSortGroup.Visible := FpmiClearSort.Visible or FpmiShowFind.Visible;
end;

procedure TtiVTTreeView.SetSelectedAddress(const AValue: string);
var
  i: integer;
  liAddress: integer;
  lsAddress: string;
  lNode: PVirtualNode;
begin
  // If we are setting the data, then delay setting the selected address until the
  // setting of the data is finished.
  if FbSettingData then
  begin
    FsSelectedAddress := AValue;
    Exit; //==>
  end;

  lsAddress := AValue;
  lNode := nil;
  while lsAddress <> '' do
  begin
    i := pos('.', lsAddress);
    // Get the next part of the address, sep by '.'
    // then strip the extracted part from the address for next time
    if i = 0 then
    begin
      liAddress := StrToInt(lsAddress);
      lsAddress := '';
    end
    else
    begin
      liAddress := StrToInt(Copy(lsAddress, 1, i - 1));
      lsAddress := Copy(lsAddress, i + 1, Length(lsAddress) - i);
    end;

    // Get a pointer to the node at address index liAddress
    if lNode = nil then
    begin
      lNode := VT.GetFirst;
      while Assigned(lNode) and (liAddress > 0) do
      begin
        lNode := VT.GetNextSibling(lNode);
        Dec(liAddress);
      end;
    end
    else
    begin
      lNode := VT.GetFirstChild(lNode);
      while Assigned(lNode) and (liAddress > 0) do
      begin
        lNode := VT.GetNextSibling(lNode);
        Dec(liAddress)
      end;
    end;

    // Select, then expand this node
    if lNode <> nil then
    begin
      // Expand the node if we still have to drill down further
      if lsAddress <> '' then
        VT.Expanded[lNode]:= True;

      VT.Selected[lNode]:= True;
      VT.FocusedNode := lNode;
      VT.ScrollIntoView(lNode, True);
    end
    else
      Break; //==>
  end;
end;

function TtiVTTreeView.CanDelete: boolean;
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
begin
  Result := false;
  if (FData = nil) or FbSettingData or FReadOnly or IsUpdating or
     (not VT.Focused) or
     (not (tiLVBtnVisDelete in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons)
  then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if not Assigned(lMapping) then
    Exit; //==>

  Result := lMapping.CanDelete;
  if Assigned(lMapping.OnCanDelete) then
    lMapping.OnCanDelete(Self, lNode, GetObjectFromNode(lNode), Result);
end;

function TtiVTTreeView.CanInsert: boolean;
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
begin
  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or
     IsUpdating or (not VT.Focused) or
     (not (tiLVBtnVisNew in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons) then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if not Assigned(lMapping) then
    Exit; //==>

  Result := lMapping.CanInsert;
  if Assigned(lMapping.OnCanInsert) then
    lMapping.OnCanInsert(Self, lNode, GetObjectFromNode(lNode), Result);
end;

function TtiVTTreeView.CanEdit: boolean;
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
begin
  Result := False;
  if (FData = nil) or FbSettingData or FReadOnly or IsUpdating or
     (not VT.Focused) or
     (not (tiLVBtnVisDelete in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons)
  then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if lMapping = nil then
    Exit; //==>

  Result := lMapping.CanEdit;
  if Assigned(lMapping.OnCanEdit) then
    lMapping.OnCanEdit(Self, lNode, GetObjectFromNode(lNode), Result);
end;

function TtiVTTreeView.GetImages: TCustomImageList;
begin
  result := VT.Images;
end;

procedure TtiVTTreeView.SetImages(const AValue: TCustomImageList);
begin
  VT.Images := AValue;
end;

procedure TtiVTTreeView.FullExpand(pLevel: Integer = -1);
var
  lSelectedAddress: string;
  lNode: PVirtualNode;
  lNodeLevel: Integer;
begin
  if pLevel < 0 then
    VT.FullExpand                 
  else
  begin
    lSelectedAddress := SelectedAddress;

    VT.BeginUpdate;
    try
      lNode := VT.GetFirst;
      while Assigned(lNode) do
      begin
        lNodeLevel := Integer(VT.GetNodeLevel(lNode));
        if lNodeLevel < pLevel then
          VT.Expanded[lNode]:= True;
        // A good optimization would be to skip all children once we reach pLevel
        lNode := VT.GetNext(lNode);
      end;

      SelectedAddress := lSelectedAddress;
    finally
      VT.EndUpdate;
    end;
  end;
end;


procedure TtiVTTreeView.FullCollapse;
begin
  VT.FullCollapse;
end;

function TtiVTTreeView.GetSelectedData: TtiObject;
begin
  result := nil;
  if VT.SelectedCount = 0 then
    Exit; //==>

  Result := GetObjectFromNode(VT.GetFirstSelected);
end;

{Currently does a brain-dead linear search. I'm thinking we should use a hash map to do these reverse lookups.
 I might use my Ptr-to-Ptr hash table from  the Shell Control Pack ActiveX.

 The JCL has a hash-map for string->pointer, but the string->ptr conversion would suck performance - defeating the purpose
 of the map. }
procedure TtiVTTreeView.SetSelectedData(const AValue: TtiObject);
var
  lNode: PVirtualNode;
begin
  if AValue = nil then
  begin
    if VT.RootNodeCount > 0 then
    begin
      VT.Selected[VT.GetFirst]:= True;
      VT.FocusedNode := VT.GetFirst;
    end;
    Exit; //==>
  end;

  lNode := VT.GetFirst;
  while Assigned(lNode) do
  begin
    if GetObjectFromNode(lNode) = AValue then
    begin
      VT.Selected[lNode]:= True;
      VT.FocusedNode := lNode;
      VT.ScrollIntoView(lNode, True);
      Break; //-->
    end;
    lNode := VT.GetNext(lNode);
  end;
end;

procedure TtiVTTreeView.SetShowEmptyRootNode(const AValue: boolean);
begin
  if AValue <> FShowEmptyRootNode then
  begin
    FShowEmptyRootNode := AValue;
    Refresh;
  end;
end;

function TtiVTTreeView.GetOnKeyDown: TKeyEvent;
begin
  result := VT.OnKeyDown;
end;

procedure TtiVTTreeView.SetOnKeyDown(const AValue: TKeyEvent);
begin
  VT.OnKeyDown := AValue;
end;

procedure TtiVTTreeView.SetReadOnly(const AValue: boolean);
begin
  FReadOnly := AValue;
  if FReadOnly then
    VT.Color := clBtnFace
  else
    VT.Color := clWhite;
end;

procedure TtiVTTreeView.BeginUpdate;
begin
end;

procedure TtiVTTreeView.EndUpdate;
begin
end;

procedure TtiVTTreeView.Refresh;
begin
  inherited;
  VT.Refresh;
end;

procedure TtiVTTreeView.RefreshCurrentNode;
begin
  VT.RepaintNode(VT.GetFirstSelected);
end;

procedure TtiVTTreeView.SetFocus;
begin
  if not Enabled then
    Exit; //==>
  inherited SetFocus;
  VT.SetFocus;
end;

function TtiVTTVDataMappings.Add(const AClass: TtiClass;
  const pDisplayPropName: string; pImageIndex: Integer): TtiVTTVDataMapping;
begin
  Result := Add;
  Result.DataClass := AClass;
  Result.DisplayPropName := pDisplayPropName;
  Result.ImageIndex := pImageIndex;
end;

function TtiVTTreeView.GetButtonStyle: TLVButtonStyle;
begin
  Result := FCtrlBtnPnl.ButtonStyle;
end;

function TtiVTTreeView.GetDataNodeCheckState(
  const AData: TtiObject): TCheckState;
var
  LNode: PVirtualNode;
begin
  LNode := GetNodeFromObject(AData);

  if LNode = nil then
    raise Exception.Create(Format('No treeview node assigned for object %s', [AData.Caption]));

  Result := LNode.CheckState;
end;

function TtiVTTreeView.GetDefaultText: WideString;
begin
  Result := VT.DefaultText;
end;

function TtiVTTreeView.GetVisibleButtons: TtiLVVisibleButtons;
begin
  result := FCtrlBtnPnl.VisibleButtons;
end;

procedure TtiVTTreeView.SetButtonStyle(const AValue: TLVButtonStyle);
begin
  tiCtrlButtonPanel.CreateCtrlBtnPnl(FCtrlBtnPnl, AValue, Self,
                                      CanView, CanInsert,
                                       CanEdit,CanDelete);
  FCtrlBtnPnl.Align := alTop;

  FCtrlBtnPnl.OnNew      := DoInsert;
  FCtrlBtnPnl.OnEdit     := DoEdit;
  FCtrlBtnPnl.OnDelete   := DoDelete;
  FCtrlBtnPnl.RefreshButtons;
  DoResize(nil)
end;

procedure TtiVTTreeView.SetChildControlNames;
begin
  inherited;
  if VT <> nil then
    VT.Name := tiGetUniqueComponentNameFromParent(Self, 'TV');
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.Name := tiGetUniqueComponentNameFromParent(Self, 'CtrlBtnPnl');
  if Assigned(FPopupMenu) then
    FPopupMenu.Name := tiGetUniqueComponentNameFromParent(Self, 'PopupMenu');
  if Assigned(FpmiShowFind) then
    FpmiShowFind.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiShowFind');
end;

procedure TtiVTTreeView.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FCtrlBtnPnl.VisibleButtons := AValue;
  DoReSize(Nil);
end;

procedure TtiVTTreeView.DoReSize(Sender: TObject);
begin
  // Do nothing
end;

procedure TtiVTTreeView.DoShowFind(Sender: TObject);
begin
  SP.Showing := not SP.Showing;
end;

procedure TtiVTTreeView.DoDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self, VT.GetFirstSelected, GetObjectFromNode(VT.GetFirstSelected))
  else if CanEdit then
    DoEdit(nil);
end;

function TtiVTTreeView.CanView: Boolean;
var
  lNode: PVirtualNode;
  lMapping: TtiVTTVDataMapping;
begin
  Result := false;
  if (FData = nil) or FbSettingData or IsUpdating or
     (not VT.Focused) or
     (not (tiLVBtnVisView in VisibleButtons)) or
     (ButtonStyle = lvbsNoButtons)
  then
    Exit; //==>

  lNode := VT.GetFirstSelected;
  lMapping := GetMappingForNode(lNode);

  if lMapping = nil then
    Exit; //==>

  Result := lMapping.CanView;
  if Assigned(lMapping.OnCanView) then
    lMapping.OnCanView(Self, lNode, GetObjectFromNode(lNode), Result);
end;


procedure TtiVTTreeView.DoOnEnter(Sender: TObject);
begin
  FCtrlBtnPnl.EnableButtons;
end;

procedure TtiVTTreeView.DoOnExit(Sender: TObject);
begin
  FCtrlBtnPnl.EnableButtons;
end;

procedure TtiVTTreeView.DoOnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LData: TtiObject;
  LMapping: TtiVTTVDataMapping;
  LNodeHasChildren: Boolean;
begin
  if (VT.GetNodeLevel(Node)) = 0 then
  begin
    LData := FData;
    Include(InitialStates, ivsExpanded);
  end
  else
    LData := GetListForNode(ParentNode).Items[Node{$IFDEF FPC}^{$ENDIF}.Index];

  SetObjectForNode(Node, LData);

  LNodeHasChildren := CalcIfObjectHasChildren(LData);

  if LNodeHasChildren then
    Include(InitialStates, ivsHasChildren);

  LMapping:= GetMappingForNode(Node);

  if not Assigned(LMapping) then
    Exit; //==>

  if LMapping.UseCheckBox then
    if LNodeHasChildren then
      Node.CheckType := ctTriStateCheckBox
    else
      Node.CheckType := ctCheckBox;
end;

procedure TtiVTTreeView.DoOnInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := CalcNodeChildren(Node);
end;

function TtiVTTreeView.CalcNodeChildren(AObject: TtiObject): Integer;
var
  LChildList: TtiObjectList;
begin
  LChildList := TtiObjectList.Create;
  try
    LChildList.OwnsObjects := False;
    LChildList.AutoSetItemOwner := False;
    Result := CalcNodeChildren(AObject, LChildList);
  finally
    LChildList.Free;
  end;
end;

function TtiVTTreeView.CalcNodeChildren(Node: PVirtualNode): Integer;
var
  LNodeRec: PNodeDataRec;
  LObj: TtiObject;
  LChildList: TtiObjectList;
begin
{
  A node can have the following children;
    If it is a list, its Items are children
    If it has published TtiObject properties, they are children
    If it has published TtiObjectList properties, those list items are children
}
  LNodeRec := VT.GetNodeData(Node);
  if not Assigned(LNodeRec.NodeChildren) then
  begin
    LNodeRec.NodeChildren := TtiObjectList.Create;
    LNodeRec.NodeChildren.OwnsObjects := False;
    LNodeRec.NodeChildren.AutoSetItemOwner := False;
  end;
  LChildList := LNodeRec.NodeChildren;
  LChildList.Clear;

  LObj := LNodeRec.NodeData;
  Result := CalcNodeChildren(LObj, LChildList);
end;

// Quickly determine if the given item in the tree has child items
function TtiVTTreeView.CalcIfObjectHasChildren(AObject: TtiObject): Boolean;
var
  LPropList: TPropInfoArray;
  LPropClass: TClass;
  LPropIndex: Integer;

  LCandidateList: TtiObjectList;
  LCandidateIndex: Integer;
  LCandidate: TtiObject;
begin
  Result := false;

  if AObject is TtiObjectList then
  begin
    for LCandidateIndex := 0 to Pred(TtiObjectList(AObject).Count) do
    begin
      LCandidate := TtiObjectList(AObject).Items[LCandidateIndex];
      if Assigned(CalcMappingForObject(LCandidate)) and
         TestObjectAgainstFilter(LCandidate) then
        Exit(true); //==>
    end;
  end;

  GetObjectPropInfos(AObject, LPropList);
  for LPropIndex := 0 to High(LPropList) do
  begin
    LPropClass := GetObjectPropClass(LPropList[LPropIndex]);
    if LPropClass.InheritsFrom(TtiObjectList) then
    begin
      LCandidateList := TtiObjectList(GetObjectProp(AObject, LPropList[LPropIndex]));
      for LCandidateIndex := 0 to Pred(LCandidateList.Count) do
      begin
        LCandidate:= LCandidateList[LCandidateIndex];
        if Assigned(CalcMappingForObject(LCandidate)) and
           TestObjectAgainstFilter(LCandidate) then
          Exit(true); //==>
      end;
    end
    else if LPropClass.InheritsFrom(TtiObject) then
    begin
      LCandidate := TtiObject(GetObjectProp(AObject, LPropList[LPropIndex]));
      if Assigned(CalcMappingForObject(LCandidate)) and
         TestObjectAgainstFilter(LCandidate) then
        Exit(true); //==>
    end;
  end;
end;

// Count the number of child items for the given item in the tree
function TtiVTTreeView.CalcNodeChildren(AObject: TtiObject;
  AChildList: TtiObjectList): Integer;
var
  LPropList: TPropInfoArray;
  LPropClass: TClass;
  LPropIndex: Integer;

  LCandidateList: TtiObjectList;
  LCandidateIndex: Integer;
  LCandidate: TtiObject;
begin
  if AObject is TtiObjectList then
  begin
    for LCandidateIndex := 0 to Pred(TtiObjectList(AObject).Count) do
    begin
      LCandidate := TtiObjectList(AObject).Items[LCandidateIndex];
      if Assigned(CalcMappingForObject(LCandidate)) and
         TestObjectAgainstFilter(LCandidate) then
        AChildList.Add(LCandidate);
    end;
  end;

  GetObjectPropInfos(AObject, LPropList);
  for LPropIndex := 0 to High(LPropList) do
  begin
    LPropClass := GetObjectPropClass(LPropList[LPropIndex]);
    if LPropClass.InheritsFrom(TtiObjectList) then
    begin
      LCandidateList := TtiObjectList(GetObjectProp(AObject, LPropList[LPropIndex]));
      for LCandidateIndex := 0 to Pred(LCandidateList.Count) do
      begin
        LCandidate:= LCandidateList[LCandidateIndex];
        if Assigned(CalcMappingForObject(LCandidate)) and
           TestObjectAgainstFilter(LCandidate) then
          AChildList.Add(LCandidate);
      end;
    end
    else if LPropClass.InheritsFrom(TtiObject) then
    begin
      LCandidate := TtiObject(GetObjectProp(AObject, LPropList[LPropIndex]));
      if Assigned(CalcMappingForObject(LCandidate)) and
         TestObjectAgainstFilter(LCandidate) then
        AChildList.Add(LCandidate);
    end;
  end;

  Result := AChildList.Count;
end;

procedure TtiVTTreeView.DoOnFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FreeAndNil(PNodeDataRec(VT.GetNodeData(Node)){$IFDEF FPC}^{$ENDIF}.NodeChildren);
end;

procedure TtiVTTreeView.DoOnGetText(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode;
  Column: TColumnIndex;
  TextType: TVSTTextType;
  var CellText: UnicodeString);
var
  LMapping: TtiVTTVDataMapping;
  LData: TtiObject;
  LCellText: UnicodeString;
begin
  LMapping:= GetMappingForNode(Node);

  if not Assigned(LMapping) then
    Exit; //==>

  //Set cell in all columns after the first to blank.
  if Column > 0 then
  begin
    CellText := '';
    Exit; //==>
  end;
      
  LData:= GetObjectFromNode(Node);

  if not Assigned(LMapping.OnDeriveNodeText) then
    //TODO: Handle multiple columns
    CellText := LData.PropValue[LMapping.DisplayPropName]
  else
  begin
    LMapping.OnDeriveNodeText(Self, Node, LData, LCellText);
    CellText:= LCellText;
  end;
end;

procedure TtiVTTreeView.DoOnGetImageIndex(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode;
  Kind: TVTImageKind;
  Column: TColumnIndex;
  var Ghosted: Boolean;
  var ImageIndex: Integer);
begin
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Node, GetObjectFromNode(Node), Kind, Column,
        Ghosted, ImageIndex)
  else if Assigned(GetMappingForNode(Node)) then
    ImageIndex := GetMappingForNode(Node).ImageIndex;
end;

function TtiVTTreeView.IsUpdating: Boolean;
begin
  Result := False; // TODO: Do we need this anymorew? Result := FUpdateCount > 0;
end;

function TtiVTTreeView.GetTreeOptions: TStringTreeOptions;
begin
  Result := VT.TreeOptions;
end;

procedure TtiVTTreeView.SetTreeOptions(const AValue: TStringTreeOptions);
begin
  VT.TreeOptions.Assign(AValue);
end;

function TtiVTTreeView.GetMappingForNode(Node: PVirtualNode): TtiVTTVDataMapping;
var
  NodeRec: PNodeDataRec;
begin
  if not Assigned(Node) then
  begin
    Result := nil;
    Exit; //==>
  end;

  NodeRec := PNodeDataRec(VT.GetNodeData(Node));
  if not Assigned(NodeRec{$IFDEF FPC}^{$ENDIF}.NodeMapping) then
    NodeRec{$IFDEF FPC}^{$ENDIF}.NodeMapping := CalcMappingForObject(NodeRec{$IFDEF FPC}^{$ENDIF}.NodeData);

  Result := NodeRec{$IFDEF FPC}^{$ENDIF}.NodeMapping;
end;

function TtiVTTreeView.GetNodeFromObject(
  const AData: TtiObject): PVirtualNode;
var
  LNode: PVirtualNode;
  LNodeData: TtiObject;
begin
  result := nil;

  LNode := VT.GetFirst;
  while Assigned(LNode) do
  begin
    LNodeData := GetObjectFromNode(LNode);
    if LNodeData = AData then
    begin
      result := LNode;
      break;
    end else
      LNode := VT.GetNext(LNode);
  end;
end;

function TtiVTTreeView.GetObjectFromNode(Node: PVirtualNode): TtiObject;
begin
  if Assigned(Node) then
    Result:= PNodeDataRec(VT.GetNodeData(Node)){$IFDEF FPC}^{$ENDIF}.NodeData
  else
    Result:= nil;
end;

procedure TtiVTTreeView.SetObjectForNode(pNode: PVirtualNode; AObject: TtiObject);
begin
  PNodeDataRec(VT.GetNodeData(pNode)){$IFDEF FPC}^{$ENDIF}.NodeData := AObject;
end;

function TtiVTTreeView.GetListForNode(Node: PVirtualNode): TtiObjectList;
begin
  Result := PNodeDataRec(VT.GetNOdeData(NOde)){$IFDEF FPC}^{$ENDIF}.NodeChildren;
end;

procedure TtiVTTreeView.SelectObjectOrOwner(AValue: TtiObject);
begin
  if TestObjectAgainstFilter(AValue) then
    SelectedData:= AValue
  else if Assigned(AValue.Owner) then
    SelectObjectOrOwner(AValue.Owner);
end;

procedure TtiVTTreeView.SetApplyFilter(const AValue: Boolean);
var
  LData: TtiObject;
  LSelected: TtiObject;
begin
  FApplyFilter := AValue;
  if Assigned(FData) then
  begin
    LData:= FData;
    LSelected:= SelectedData;
    Data:= nil;
    Data:= LData;
    if Assigned(LSelected) then
      SelectObjectOrOwner(LSelected);
  end;
end;

function TtiVTTreeView.IsMappingForObject(AtiVTTVDataMapping: TtiVTTVDataMapping; AObj: TtiObject): Boolean;
begin
  Result := Assigned(AtiVTTVDataMapping) and Assigned(AObj);
  if Result then
    Result := IsClassOfType  (AObj, AtiVTTVDataMapping.DataClass.ClassName)
          and tiIsPublishedProp(AObj, AtiVTTVDataMapping.DisplayPropName);
end;

function TtiVTTreeView.CalcMappingForObject(pObj: TtiObject): TtiVTTVDataMapping;
var
  i: Integer;
begin
  for i := 0 to Pred(DataMappings.Count) do
  begin
    if IsClassOfType(pObj, DataMappings.Items[i].DataClass.ClassName) then
    begin
      Result := DataMappings.Items[i];
      if (tiIsPublishedProp(pObj, Result.DisplayPropName))
      or (assigned(Result.OnDeriveNodeText)) then
        Exit; //==>
    end;
  end;
  Result := nil;
end;

procedure TtiVTTreeView.SetEnabled(AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    inherited;
    FCtrlBtnPnl.EnableButtons;
  end;
end;

function TtiVTTreeView.TestObjectAgainstFilter(AValue: TtiObject): Boolean;
begin
  Assert(AValue.TestValid, CTIErrorInvalidObject);
  Result:= True;
  if ApplyFilter and Assigned(OnFilter) then
    OnFilter(AValue, Result);
end;

procedure TtiVTTreeView.VTSearchInsideNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; out Result: boolean);
 var
   LNodeText, LSearchText: string;
begin
   LSearchText := AnsiUpperCase(SearchText);
   LNodeText := AnsiUpperCase(VT.Text[Node, 0]);
   // look for partial match on SearchText as well as complete match
   Result := (AnsiStrPos(PChar(LNodeText), PChar(LSearchText)) <> nil);
end;

procedure TtiVTTVDataMappings.Assign(Source: TPersistent);
begin
end;

{$ENDIF}

end.

