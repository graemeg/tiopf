
unit tiTreeView;

{$I tiDefines.inc}

interface
uses
{$IFNDEF FPC}
   Messages
  , Windows
  , CommCtrl
{$ELSE}
  lMessages
  ,LClIntf
  ,LCLProc
{$ENDIF}
  , ComCtrls
  , Classes
  , Menus
  , Graphics
  , ExtCtrls
  , Controls
  , ImgList
  , Forms

  , Contnrs
  , tiFocusPanel
  , tiCtrlButtonPanel
  , tiObject
 ;

const
  cuiSplitterPos = 175;

type


  TtiTreeView = class;

  TtiTVNodeEvent = procedure(ptiTreeView: TtiTreeView;
    pNode: TTreeNode;
    AData: TObject;
    pParentNode: TTreeNode;
    pParentData: TObject) of object;

  TtiTVNodeConfirmEvent = procedure(ptiTreeView: TtiTreeView;
    pNode: TTreeNode;
    AData: TObject;
    pParentNode: TTreeNode;
    pParentData: TObject;
    var pbConfirm: boolean) of object;

  TtiTVDragDropEvent = procedure(ptiTVSource: TtiTreeView;
    pDataSource: TObject;
    pDataParentSource: TObject;
    ptiTVTarget: TtiTreeView;
    pDataTarget: TObject;
    pDataParentTarget: TObject;
    var pSelectedData: TObject) of object;

  TtiTVDragDropConfirmEvent = procedure(ptiTVSource: TtiTreeView;
    pDataSource: TObject;
    pDataParentSource: TObject;
    ptiTVTarget: TtiTreeView;
    pDataTarget: TObject;
    pDataParentTarget: TObject;
    var pbConfirm: boolean) of object;

  TTVGetDataPageEvent = procedure(AData: TObject;
    pNode: TTreeNode) of object;
  TtiTVGetFontEvent = procedure(pNode: TTreeNode;
    AData: TObject;
    pFont: TFont;
    pCanvas: TCanvas) of object;
  TTVUpdateNodeText = procedure(const AValue: string) of object;
  TtiTVGetImageIndexEvent = procedure(AData: TObject;
    var piImageIndex: integer) of object;

  TtiTVOnFilterDataEvent = procedure(AData: TObject;
    var pbInclude: boolean) of object;

  TtiTVDataClassRef = class of TPersistent;
  TtiTVFormClassRef = class of TForm;
  TTVOnGetChildFormEvent = procedure(pChildForm: TForm;
    AData: TObject;
    pNode: TTreeNode) of object;

  
  TtiTVDataMapping = class(TCollectionItem)
  private
    FsDisplayPropName: string;
    FsDataClassName: string;
    FiImageIndex: integer;
    FOnGetDataPage: TTVGetDataPageEvent;
    FbHasChildren: boolean;
    FOnCanView:   TtiTVNodeConfirmEvent;
    FOnCanDelete: TtiTVNodeConfirmEvent;
    FOnCanInsert: TtiTVNodeConfirmEvent;
    FOnDelete: TtiTVNodeEvent;
    FOnInsert: TtiTVNodeEvent;
    FbCanInsert: boolean;
    FbCanDelete: boolean;
    FName: TComponentName;
    FbCanEdit: boolean;
    FOnCanEdit: TtiTVNodeConfirmEvent;
    FOnEdit: TtiTVNodeEvent;
    FbCanDragCopy: boolean;
    FbCanDragMove: boolean;
    FOnCanAcceptDrop: TtiTVDragDropConfirmEvent;
    FOnDrop: TtiTVDragDropEvent;
    FbCanView: boolean;
    procedure SetDataClassName(const AValue: string);
    function ClassNameToCollectionItemName(const AValue: string): string;
  protected
    function GetDisplayName: string; override;
    property OnGetDataPage: TTVGetDataPageEvent read FOnGetDataPage write FOnGetDataPage;
  published
    // Properties
    property CanDelete: boolean read FbCanDelete write FbCanDelete default false;
    property CanInsert: boolean read FbCanInsert write FbCanInsert default false;
    property CanEdit:   boolean read FbCanEdit write FbCanEdit default false;
    property CanView:   boolean read FbCanView write FbCanView default false;
    property CanDragCopy: boolean read FbCanDragCopy write FbCanDragCopy default false;
    property CanDragMove: boolean read FbCanDragMove write FbCanDragMove default false;

    property DataClass: string read FsDataClassName write SetDataClassName;
    property DisplayPropName: string read FsDisplayPropName write FsDisplayPropName;
    property ImageIndex: integer read FiImageIndex write FiImageIndex default -1;
    property HasChildren: boolean read FbHasChildren write FbHasChildren default true;
    property Name: TComponentName read FName write FName;

    // Events
    property OnInsert: TtiTVNodeEvent read FOnInsert write FOnInsert;
    property OnDelete: TtiTVNodeEvent read FOnDelete write FOnDelete;
    property OnEdit: TtiTVNodeEvent read FOnEdit write FOnEdit;
    property OnDrop: TtiTVDragDropEvent read FOnDrop write FOnDrop;

    property OnCanView:   TtiTVNodeConfirmEvent read FOnCanView write FOnCanView;
    property OnCanInsert: TtiTVNodeConfirmEvent read FOnCanInsert write FOnCanInsert;
    property OnCanDelete: TtiTVNodeConfirmEvent read FOnCanDelete write FOnCanDelete;
    property OnCanEdit: TtiTVNodeConfirmEvent read FOnCanEdit write FOnCanEdit;
    property OnCanAcceptDrop: TtiTVDragDropConfirmEvent read FOnCanAcceptDrop write FOnCanAcceptDrop;

  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TtiTVDataMapping); reintroduce;
  end;

  
  TtiTVDataMappings = class(TCollection)
  private
    FTreeView: TtiTreeView;
    FiItemNo: integer;
    function GetItem(Index: Integer): TtiTVDataMapping;
    procedure SetItem(Index: Integer; const AValue: TtiTVDataMapping);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(pTreeView: TtiTreeView);
    function Add: TtiTVDataMapping; overload;
    function Add(const AClass: TClass; const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping; overload;
    property Items[Index: Integer]: TtiTVDataMapping read GetItem write SetItem;
  end;

  
  TTVPopupMenu = class(TPopupMenu)
  private
    FmiInsert: TMenuItem;
    FmiDelete: TMenuItem;
    FmiEdit: TMenuItem;
    FmiExpand: TMenuItem;
    FmiExpandAll: TMenuItem;
    FTV: TtiTreeView;
    procedure DoInsert(sender: TObject);
    procedure DoDelete(sender: TObject);
    procedure DoEdit(sender: TObject);
    procedure DoExpand(sender: TObject);
    procedure DoExpandAll(sender: TObject);
    procedure DoOnPopup(sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TV: TtiTreeView read FTV write FTV;
  end;


  TtiTVDragObject = class(TDragObject)
  private
    FData: TObject;
    FParentData: TObject;
    FtiTreeView: TtiTreeView;
    FNode: TTreeNode;
    FParentNode: TTreeNode;
  public
    property tiTreeView: TtiTreeView read FtiTreeView write FtiTreeView;
    property Data: TObject read FData write FData;
    property ParentData: TObject read FParentData write FParentData;
    property Node: TTreeNode read FNode write FNode;
    property ParentNode: TTreeNode read FParentNode write FParentNode;
    //    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  end;


  TtiTreeView = class(TtiFocusPanel)
  private
    FTV: TTreeView;
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs;
    FTVDataMappings: TtiTVDataMappings;
    FNodesLoaded: TList;

    FPopupMenu: TTVPopupMenu;
    FOnSelectNode: TtiTVNodeEvent;

    FOnGetImageIndex: TtiTVGetImageIndexEvent;
    FOnGetFont: TtiTVGetFontEvent;
    FOnFilterData: TtiTVOnFilterDataEvent;

    FtmrSetData: TTimer;
    FbSettingData: boolean;
    FsSelectedAddress: string;
    FbFullExpand: boolean;
    FFullExpandLevel : integer;
    FbApplyFilter: boolean;
    FReadOnly: boolean;
    FUpdateCount: Integer;
    FSaveCursor: TCursor;
    FOnDblClick: TtiTVNodeEvent;

    procedure DoOnExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    function HasNodeChildren(AValue: TObject): boolean;
    procedure AddNodeChildren(pNode: TTreeNode;
      AData: TtiObject);
    procedure GetObjectPropNames(pPersistent: TObject; pSL: TStringList);
    function CountObjectProps(pPersistent: TObject): integer;
    procedure DoOnChange(sender: TObject; node: TTreeNode);
    function CanShowObjectProp(AValue: TObject): boolean;
    procedure DoCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode;
      State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure SetTVDataMappings(const AValue: TtiTVDataMappings);
    function FindMapping(AData: TtiObject): TtiTVDataMapping;
    function GetSelectedAddress: string;
    procedure SetSelectedAddress(const AValue: string);
    procedure GetNodeInfo(var pNode: TTreeNode;
      var AData: TtiObject;
      var pParentNode: TTreeNode;
      var pParentData: TtiObject;
      var pMapping: TtiTVDataMapping);

    function GetImages: TCustomImageList;
    procedure SetImages(const AValue: TCustomImageList);
    function GetOnChanging: TTVChangingEvent;
    procedure SetOnChanging(const AValue: TTVChangingEvent);
    function GetItems: TTreeNodes;
    procedure SetItems(const AValue: TTreeNodes);
    function GetSelected: TTreeNode;
    procedure SetSelected(const AValue: TTreeNode);

    procedure RefreshTreeNode(ptiTreeView: TtiTreeView; pTreeNode: TTreeNode);

    // Drag & Drop Sender side events
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoStartDrag(Sender: TObject; var DragObject: TDragObject); reintroduce;
    procedure DoOnEnter(Sender: TObject);
    procedure DoOnExit(Sender: TObject);
    //    procedure DoEndDrag(  Sender, Target: TObject; X, Y: Integer); Reintroduce;

        // Drag & Drop Receiver side events
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    function GetSelectedData: TObject;
    procedure SetSelectedData(const AValue: TObject);
    function GetTreeSortType: TSortType;
    procedure SetTreeSortType(const AValue: TSortType);

    procedure SetApplyFilter(const AValue: boolean);
    procedure GetNodeImageIndex(pNode: TTreeNode; AData: TtiObject;
      pDataMapping: TtiTVDataMapping);
    function GetOnKeyDown: TKeyEvent;
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetEnableTree(const AValue: boolean);
    function GetEnableTree: boolean;
    function  GetButtonStyle: TLVButtonStyle;
    function  GetVisibleButtons: TtiLVVisibleButtons;
    procedure SetButtonStyle(const AValue: TLVButtonStyle);
    procedure SetVisibleButtons(const AValue: TtiLVVisibleButtons);

  protected
    FData: TtiObject;
    procedure DoSetData(Sender: TObject); virtual;
    procedure SetData(const AValue: TtiObject); virtual;
    procedure SetName(const AValue: TComponentName); override;
    property  NodesLoaded: TList read FNodesLoaded;
    procedure SetReadOnly(const AValue: boolean); virtual;
    procedure DoReSize(Sender: TObject); virtual;
    function  IsUpdating: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetFocus; override;

    property TV: TTreeView read FTV;
    property Items: TTreeNodes read GetItems write SetItems;
    property Data: TtiObject read FData write SetData;
    property Selected: TTreeNode read GetSelected write SetSelected;

    procedure UpdateNodeText(const AValue: string);
    procedure RefreshCurrentNode;
    procedure RefreshCurrentNodesParent;
    procedure DoInsert(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoEdit(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure FullExpand(pLevel : integer = -1);
    procedure FullCollapse;
    function  FindNodeByData(AData: TObject): TTreeNode;
    property  SelectedData: TObject read GetSelectedData write SetSelectedData;
    function  CanView: boolean;
    function  CanDelete: boolean;
    function  CanInsert: boolean;
    function  CanEdit: boolean;
    procedure BeginUpdate;
    procedure EndUpdate;

  published
    property Align;
    property Anchors;
    property BorderWidth;
    property BevelInner;
    property BevelOuter;
    property Constraints;
    property Color;
    property ParentColor;
    property Visible;

    property ButtonStyle : TLVButtonStyle read GetButtonStyle write SetButtonStyle default lvbsMicroButtons;
    property VisibleButtons : TtiLVVisibleButtons read GetVisibleButtons write SetVisibleButtons default [];
    property Images: TCustomImageList read GetImages write SetImages;

    property OnChanging: TTVChangingEvent read GetOnChanging write SetOnChanging;
    property OnSelectNode: TtiTVNodeEvent read FOnSelectNode write FOnSelectNode;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnDblClick: TtiTVNodeEvent read FOnDblClick Write FOnDblClick;

    // We can't publish OnChange as is is used internally
    // property OnChange;
    property OnGetImageIndex: TtiTVGetImageIndexEvent
      read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetFont: TtiTVGetFontEvent
      read FOnGetFont write FOnGetFont;
    property OnFilterData: TtiTVOnFilterDataEvent
      read FOnFilterData write FOnFilterData;
    property DataMappings: TtiTVDataMappings read FTVDataMappings write SetTVDataMappings;
    property SelectedAddress: string read GetSelectedAddress write SetSelectedAddress;

    property TreeSortType: TSortType read GetTreeSortType write SetTreeSortType;

    property ApplyFilter: boolean
      read FbApplyFilter
      write SetApplyFilter default false;
    property ReadOnly: boolean
      read FReadOnly
      write SetReadOnly default false;
    property EnableTree : boolean read GetEnableTree write SetEnableTree default true;
  end;

function IsClassOfType(AData: TObject; AClassName: string): boolean;

implementation
uses
  SysUtils
  , TypInfo
  //  ,tiLog  // If this unit is not commented, then it is my error as I have been
  //            // doing some debugging and forgot to clean up my mess.
  ,tiImageMgr
  ,tiResources
 ;

const
  cusMethodPrefix = 'tiTVMapping';

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
// * TtiTreeView
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTreeView.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
  Height := 120;
  Width := 120;
  OnResize := DoResize;

  FTV := TTreeView.Create(self);
  FTV.Parent := self;
  FTV.Name := Name + '_TV';
  FTV.Top := 1;
  FTV.Left := 2;
  FTV.Height := Height - 2;
  FTV.Width := Width - 2;

  FTVDataMappings := TtiTVDataMappings.Create(self);
  FTV.OnExpanding := DoOnExpanding;

  FTV.OnChange := DoOnChange;
  FTV.OnCustomDrawItem := DoCustomDrawItem;
  FTV.ReadOnly := true;
  FTV.OnDblClick := DoDblClick;

  FPopupMenu := TTVPopupMenu.Create(nil);
  FPopupMenu.TV := self;
  FTV.PopupMenu := FPopupMenu;

  FTV.DragMode := dmManual;

  FTV.OnMouseDown := DoMouseDown;
  FTV.OnStartDrag := DoStartDrag;
  FTV.OnDragOver := DoDragOver;
  FTV.OnDragDrop := DoDragDrop;
  FTV.OnEnter    := DoOnEnter;
  FTV.OnExit     := DoOnExit;

  FbApplyFilter := false;

  FNodesLoaded := TList.Create;

  // If the TreeView's data is set in a forms onCreate event, under some
  // conditions the tree will not be painted correctly. This hack with a
  // TTimer will work around the problem - but there must be a better
  // solution.
  FtmrSetData := TTimer.Create(nil);
  FtmrSetData.Enabled := false;
  FtmrSetData.OnTimer := DoSetData;
  FtmrSetData.Interval := 15;
  FsSelectedAddress := EmptyStr;
  FbFullExpand := false;
  FReadOnly := false;
  SetButtonStyle(lvbsNormalButtons);
  FUpdateCount := 0;
end;

destructor TtiTreeView.Destroy;
begin
  FTVDataMappings.Free;
  FPopupMenu.Free;
  FNodesLoaded.Free;
  FtmrSetData.Free;
  inherited;
end;

procedure TtiTreeView.DoOnExpanding(Sender: TObject;
  Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  lDataMapping: TtiTVDataMapping;
  lCursor: TCursor;
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    BeginUpdate;
    try
      // We allow Expansion ONLY when child form is VALID...
      AllowExpansion := true;
      // Call OnGetDataPage
      if Selected <> Node then
      begin
        Selected := node;

        if Selected <> node then
          AllowExpansion := false
        else
          DoOnChange(Self, Selected);
      end;

      if AllowExpansion then
      begin
      // Check that this node has not been added already. Do this by adding
      // the node's data to a list of pointers.
        if (NodesLoaded.IndexOf(Node.Data) = -1) then
        begin
          NodesLoaded.Add(Node.Data);
          AddNodeChildren(Node, TtiObject(Node.Data));
        end;

      { TODO : Assign parentNode and ParentData }
        if Assigned(OnSelectNode) then
          OnSelectNode(Self, Node, TObject(Node.Data), nil, nil); // !!.01 bug fix IK

  //      GetDataPage(Node.Data, Node);

      // Execute the collection based OnGetDataPage, if one exists
        lDataMapping := FindMapping(TObject(Node.Data) as TtiObject);
        if (lDataMapping <> nil) and
          (assigned(lDataMapping.OnGetDataPage)) then
          lDataMapping.OnGetDataPage(TObject(Node.Data), Node);
      end;
    finally
      EndUpdate;
    end;
  finally
    Screen.Cursor := lCursor;
  end;

end;

// Given a data object, which is a TPersistent, scan the DataMappings
// collection to find a mapping.
function TtiTreeView.FindMapping(AData: TtiObject): TtiTVDataMapping;
var
  i: integer;
  lDataMapping: TtiTVDataMapping;
begin
  result := nil;
  for i := 0 to DataMappings.Count - 1 do
  begin
    if IsClassOfType(AData, DataMappings.Items[i].DataClass) then
    begin
      lDataMapping := DataMappings.Items[i];
      if IsPublishedProp(AData, lDataMapping.DisplayPropName) then
      begin
        result := lDataMapping;
        Break; //==>
      end;
    end;
  end;
end;

procedure TtiTreeView.GetNodeImageIndex(pNode: TTreeNode;
  AData: TtiObject;
  pDataMapping: TtiTVDataMapping);
var
  liImageIndex: integer;
  lDataMapping: TtiTVDataMapping;
begin
  if pDataMapping = nil then
    lDataMapping := FindMapping(AData)
  else
    lDataMapping := pDataMapping;

  if lDataMapping = nil then
    Exit; //==>

  // If the OnGetImageIndex event is assinged, then execute it
  if Assigned(FOnGetImageIndex) {and
  Assigned(Images) }then
  begin
    liImageIndex := lDataMapping.ImageIndex;
    FOnGetImageIndex(AData, liImageIndex);
    pNode.ImageIndex := liImageIndex;
    pNode.SelectedIndex := liImageIndex;
    // No OnGetImageIndex event, then use the image index fro lDataMapping
  end
  else
  begin
    pNode.ImageIndex := lDataMapping.ImageIndex;
    pNode.SelectedIndex := lDataMapping.ImageIndex;
  end;

end;

procedure TtiTreeView.AddNodeChildren({ptiTreeView : TtiTreeView;}
  pNode: TTreeNode;
  AData: TtiObject);

  procedure AddChildNode({ptiTreeView : TtiTreeView     ;}
    AData: TtiObject;
    pDataMapping: TtiTVDataMapping;
    pParentNode: TTreeNode);
  var
    lsCaption: string;
    lNode: TTreeNode;
    i: integer;
    lbInclude: boolean;
  begin

    // Check to see if this node is already added, and if it is, then we are
    // probably doing a refresh after the user has inserted a new node.
    for i := 0 to pParentNode.Count - 1 do
    {$IFNDEF FPC}
      if pParentNode.Item[i].Data = AData then
    {$ELSE}
      {$Note Probably fixed in Lazarus SVN (?)}
      if TtiObject(pParentNode.Items[i].Data) = AData then
    {$ENDIF}
        Exit; //==>

    if FbApplyFilter and Assigned(FOnFilterData) then
    begin
      lbInclude := true;
      FOnFilterData(AData, lbInclude);
      if not lbInclude then
        Exit; //==>
    end;

    lsCaption := GetPropValue(AData, pDataMapping.DisplayPropName, True);
    lNode := {ptiTreeView.} Items.AddChildObject(pParentNode, lsCaption, AData);
    lNode.HasChildren := HasNodeChildren(AData);
    GetNodeImageIndex(lNode, AData, pDataMapping);

  end;

  procedure AddChildPersistent(pParentNode: TTreeNode;
    AData: TtiObject);
  var
    lDataMapping: TtiTVDataMapping;
  begin
    lDataMapping := FindMapping(AData);
    if lDataMapping = nil then
      exit; //==>
    AddChildNode(AData, lDataMapping, pParentNode);
  end;

  // The object is a TList of TPersistent(s), so add the each item
  // in the list as a child node.
  procedure AddChildList(pParentNode: TTreeNode;
    AData: TList);
  var
    lData: TtiObject;
    i: integer;
    lDataMapping: TtiTVDataMapping;
  begin
    for i := 0 to AData.Count - 1 do
    begin
      if (TObject(AData.Items[i]) is TtiObject) then
      begin
        lData := TtiObject(AData.Items[i]);
        lDataMapping := FindMapping(lData);
        if lDataMapping <> nil then
          AddChildNode(lData, lDataMapping, pParentNode);
      end;
    end;
    pNode.HasChildren := pNode.HasChildren or (AData.Count > 0);
  end;

var
  lChild: TObject;
  i: integer;
  lslObjProps: TStringList;
begin

  lslObjProps := TStringList.Create;
  try
    // Get a list of object type properties
    GetObjectPropNames(AData, lslObjProps);
    // Scan the list of object type properties
    for i := 0 to lslObjProps.Count - 1 do
    begin
      // Get a pointer to an object type property
      lChild := (GetObjectProp(AData, lslObjProps.Strings[i]) as TObject);
      // If it's a TtiObject, then add it
      if (lChild is TtiObject) then
        AddChildPersistent(pNode, TtiObject(lChild))
          // If it's a TList, then add each list element.
      else if (lChild is TList) then
        AddChildList(pNode, TList(lChild));
    end;

  finally
    lslObjProps.Free;
  end;

end;

function TtiTreeView.HasNodeChildren(AValue: TObject): boolean;
var
  lMapping: TtiTVDataMapping;
begin
  // The data value is a TtiObject
  if (AValue is TtiObject) then
  begin
    lMapping := FindMapping(TtiObject(AValue));
    if ((lMapping <> nil) and
      (lMapping.HasChildren)) then
      result :=
        (CountObjectProps(AValue) > 0)

    else if ((lMapping <> nil) and
      (not lMapping.HasChildren)) then
      result := false
    else
      result :=
        (CountObjectProps(AValue) > 0)
       ;

    // The data value is a TList
  end
  else if (AValue is TList) then
  begin
    {
      begin
        if TList(AValue).Count > 0 then
        begin
          lList := TList(AValue);
          for i := 0 to lList.Count - 1 do
          begin
            result := FindMapping(TtiObject(lList.Items[i])) <> nil;
            if result then
              Break; //==>
          end;
        end
        else
    }
    result := true;

    // The data value is not a TtiObject or TList
  end
  else
    result := false;

end;

procedure TtiTreeView.SetData(const AValue: TtiObject);
begin

  FtmrSetData.Enabled := false;

  Items.Clear;
  FNodesLoaded.Clear;

  FData := AValue;
  if AValue = nil then
  begin
    Items.Clear;
    exit; //==>
  end;

  FbSettingData := true;
  FtmrSetData.Enabled := true;

end;

// If the TreeView's data is set in a forms onCreate event, under some
// conditions the tree will not be painted correctly. This hack with a
// TTimer will work around the problem - but there must be a better
// solution.

procedure TtiTreeView.DoSetData(Sender: TObject);
var
  lsCaption: string;
  lNode: TTreeNode;
  lbDummy: boolean;
begin

  try
    Assert(FData <> nil, 'Data property is nil');

    FtmrSetData.Enabled := false;

    if IsPublishedProp(FData, 'Caption') then
      lsCaption := GetPropValue(FData, 'Caption', True)
    else
      lsCaption := 'Top';

    lNode := Items.AddObject(nil, lsCaption, FData);
    lNode.HasChildren := HasNodeChildren(FData);
    GetNodeImageIndex(lNode, FData, nil);

    if lNode.HasChildren then
      DoOnExpanding(FTV, lNode, lbDummy);

    lNode.Expand(false);

    { TODO : Assign ParentNode and ParentData }
    if Assigned(FOnSelectNode) then
      FOnSelectNode(Self, lNode, TObject(lNode.Data), nil, nil);

    // Is this necessary here /
    // GetDataPage(AData: TObject; pNode: TTreeNode);

  finally
    FbSettingData := false;
  end;

  if FsSelectedAddress <> EmptyStr then
    SetSelectedAddress(FsSelectedAddress);

  if FbFullExpand then
    FullExpand;

  FCtrlBtnPnl.EnableButtons;

end;

procedure TtiTreeView.UpdateNodeText(const AValue: string);
begin
  if Selected <> nil then
    Selected.Text := AValue;
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
  //FmiExpand.Shortcut := TextToShortcut('Enter');
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
  TV.DoDelete(nil);
end;

procedure TTVPopupMenu.DoEdit(sender: TObject);
begin
  TV.DoEdit(nil);
end;

procedure TTVPopupMenu.DoExpand(sender: TObject);
begin
  if FTV.Selected.Expanded then
    FTV.Selected.Collapse(false)
  else
    TV.Selected.Expand(false);
end;

procedure TTVPopupMenu.DoExpandAll(sender: TObject);
begin
  FTV.Selected.Expand(true);
end;

procedure TTVPopupMenu.DoInsert(sender: TObject);
begin
  TV.DoInsert(Sender);
end;

procedure TTVPopupMenu.DoOnPopup(sender: TObject);
begin
  FmiInsert.Visible := true;
  FmiEdit.Visible  := true;
  FmiDelete.Visible := true;

  FmiExpand.Enabled := FTV.Selected.HasChildren;
  FmiInsert.Enabled := FTV.CanInsert;
  FmiEdit.Enabled  := FTV.CanEdit;
  FmiDelete.Enabled := FTV.CanDelete;

  if FTV.Selected.Expanded then
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

procedure TtiTreeView.GetNodeInfo(var pNode: TTreeNode;
  var AData: TtiObject;
  var pParentNode: TTreeNode;
  var pParentData: TtiObject;
  var pMapping: TtiTVDataMapping);
begin
  if pNode = nil then
    pNode := Selected;

  if Selected = nil then
    Exit; //==>

  AData := nil;
  pParentNode := nil;
  pParentData := nil;

  if pNode <> nil then
  begin
    AData := TtiObject(pNode.Data);
    pParentNode := pNode.Parent;
    if pParentNode <> nil then
      pParentData := TtiObject(pParentNode.Data);
  end;

  pMapping := FindMapping(TtiObject(AData));

end;

procedure TtiTreeView.RefreshTreeNode(ptiTreeView: TtiTreeView; pTreeNode: TTreeNode);
var
  i: integer;
  lbDummy: boolean;
  lDataMapping: TtiTVDataMapping;
begin
  // This code will delete, then reread all the child nodes
  // It would be better to check if any notes have been deleted,
  // but this scanning of the parent's data object would be difficult.
  // Delete all the parent's children
  for i := pTreeNode.Count - 1 downto 0 do
  begin
    {$IFNDEF FPC}
    ptiTreeView.NodesLoaded.Remove(pTreeNode.Item[i].Data);
    ptiTreeView.Items.Delete(pTreeNode.Item[i]);
    {$ELSE}
    {$Note Probably fixed in Lazarus SVN}
    ptiTreeView.NodesLoaded.Remove(pTreeNode.Items[i].Data);
    ptiTreeView.Items.Delete(pTreeNode.Items[i]);
    {$ENDIF}
  end;

  ptiTreeView.NodesLoaded.Remove(pTreeNode.Data);

  // Update the node text and image.
  lDataMapping := FindMapping(TtiObject(pTreeNode.Data));
  if lDataMapping <> nil then begin
    pTreeNode.Text := GetPropValue(TObject(pTreeNode.Data), lDataMapping.DisplayPropName, True);
    GetNodeImageIndex(pTreeNode, TtiObject(pTreeNode.Data), lDataMapping);
  end;

  // Collapse the parent node
  pTreeNode.Collapse(true);
  // Expand and reread the parent node's children
  DoOnExpanding(ptiTreeView.TV, pTreeNode, lbDummy);

end;

procedure TtiTreeView.DoDelete(Sender: TObject);
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  liNodeIndex: integer;
  lbCanDelete: boolean;
  lMapping: TtiTVDataMapping;
begin

  if not CanDelete then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  if not (Assigned(lMapping.OnDelete)) then
    Exit; //==>

  lbCanDelete := (lMapping.CanDelete);

  if Assigned(lMapping.OnCanDelete) then
    lMapping.OnCanDelete(self, lNode, lData, lParentNode, lParentData, lbCanDelete);

  liNodeIndex := -1;

  if lbCanDelete then
  begin
    liNodeIndex := lNode.Index;
    lMapping.OnDelete(self, lNode, lData, lParentNode, lParentData);
  end;

  if lbCanDelete then
  begin

//    FTV.OnChanging := nil;
//    try
      RefreshTreeNode(Self, lParentNode);
//    finally
//      FTV.OnChanging := DoOnChanging;
//    end;

    // Reposition the selected node as close as possible to the position
    // of the deleted node.
   {$IFNDEF FPC}
    if (liNodeIndex > 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Item[liNodeIndex - 1]
    else if (liNodeIndex = 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Item[0]
    else
      Selected := lParentNode;
    {$ELSE}
    {$Note Probably fixed in Lazarus SVN}
    if (liNodeIndex > 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Items[liNodeIndex - 1]
    else if (liNodeIndex = 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Items[0]
    else
      Selected := lParentNode;

    {$ENDIF}

  end;

end;

procedure TtiTreeView.DoInsert(Sender: TObject);
  procedure _ExpandAndCountChildren(pNode: TTreeNode; var piCount: integer);
  begin
    piCount := 0;
    if pNode = nil then
      Exit; //==>
    if (not pNode.Expanded) then
      pNode.Expand(false);
    piCount := pNode.Count;
  end;

var
  lNode: TTreeNode;
  lParentNode: TTreeNode;
  lData: TtiObject;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
  lbCanInsert: boolean;
  liParentNodeCount: integer;
  liNodeCount: integer;
  liCount: integer;
begin

  if not CanInsert then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  // If there was a mapping with OnInsert assigned, then it gets priority
  if (Assigned(lMapping.OnInsert)) then
  begin

    lbCanInsert := (lMapping.CanInsert);

    if Assigned(lMapping.OnCanInsert) then
      lMapping.OnCanInsert(self, lNode, lData, lParentNode, lParentData, lbCanInsert);

    if not lbCanInsert then
      Exit; //==>

    // The mapping has an OnInsert method, and CanInsert evaluated to true,
    // so execute the mappings OnInsert method.
    _ExpandAndCountChildren(lParentNode, liParentNodeCount);
    _ExpandAndCountChildren(lNode, liNodeCount);

    lMapping.OnInsert(self, lNode, lData, lParentNode, lParentData);

  end
  else

    // No OnInsert method for this mapping, so execute the default one.
  begin

    _ExpandAndCountChildren(lParentNode, liParentNodeCount);
    _ExpandAndCountChildren(lNode, liNodeCount);

  end;

  // An OnInsert method was called, so re-read the child nodes
  if lbCanInsert then
  begin

    // OnInsert() may have added data to either lData or lParentData, we
    // don't know. To make things more complicated, lData and lParentData are both
    // TObject(s), so it is difficult to find exactly where the data was added.
    // (lData/lParentData may be a TtiObject owning any number of TLists, or
    // may be a TList - we don't know)
    // Add any newly created data items as nodes to the tree
    if (lParentNode <> nil) and
      (lParentData <> nil) then
      AddNodeChildren(lParentNode, lParentData);
    AddNodeChildren(lNode, lData);

    // This is a bit of a work-around. We are assuming that any new nodes will
    // have been added to the end of the list (this is not such a bad assumption,
    // except when the node has been added by drag-and-drop). A better approach
    // would be to add a pointer for all the children to a TList, then to scan
    // the TList looking for a newly added data object. When this newly added
    // data object is found, use SetSelectedData to set the selected tree node.

    // Check to see if we have added any new children to lNode
    if lNode <> nil then
    begin
      _ExpandAndCountChildren(lNode, liCount);
      if liNodeCount <> liCount then
       {$IFNDEF FPC}
        Selected := lNode.Item[lNode.Count - 1];
        {$ELSE}
        Selected := lNode.Items[lNode.Count - 1];
        {$ENDIF}
      //CurrentChildForm.SetFocus;
    end;

    // Check to see if we have added any new children to lParentNode
    if lParentNode <> nil then
    begin
      _ExpandAndCountChildren(lParentNode, liCount);
      if liParentNodeCount <> liCount then
      {$IFNDEF FPC}
        Selected := lParentNode.Item[lParentNode.Count - 1];
        {$ELSE}
        Selected := lParentNode.Items[lParentNode.Count - 1];
        {$ENDIF}
      //CurrentChildForm.SetFocus;
    end;

  end;

end;

procedure TtiTreeView.DoEdit(Sender: TObject);
var
  lNode: TTreeNode;
  lParentNode: TTreeNode;
  lData: TtiObject;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
  lbCanEdit: boolean;
begin

  if not CanEdit then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  // If there was a mapping with OnInsert assigned, then it gets priority
  if (Assigned(lMapping.OnEdit)) then
  begin

    lbCanEdit := (lMapping.CanEdit);

    if Assigned(lMapping.OnCanEdit) then
      lMapping.OnCanEdit(self, lNode, lData, lParentNode, lParentData, lbCanEdit);

    if not lbCanEdit then
      Exit; //==>

    lMapping.OnEdit(self, lNode, lData, lParentNode, lParentData);

  end;

  lNode.Text := GetPropValue(lData, lMapping.DisplayPropName, True);

end;

procedure TtiTreeView.GetObjectPropNames(pPersistent: TObject;
  pSL: TStringList);
var
  lCount: integer;
  lSize: integer;
  lList: PPropList;
  i: integer;
begin
  pSL.Clear;
  lCount := GetPropList(pPersistent.ClassInfo, [tkClass], nil);
  lSize := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
    GetPropList(pPersistent.ClassInfo, [tkClass], lList);
    for i := 0 to lcount - 1 do
     {$IFNDEF FPC}
      psl.add(lList[i].Name);
      {$ELSE}
      psl.add(lList^[i]^.Name);
      {$ENDIF}
  finally
    FreeMem(lList, lSize);
  end;
end;

function TtiTreeView.CountObjectProps(pPersistent: TObject): integer;
var
  lsl: TStringList;
  i: integer;
begin
  result := 0;
  lsl := TStringList.Create;
  try
    GetObjectPropNames(pPersistent, lsl);
    for i := 0 to lsl.Count - 1 do
    begin
      if CanShowObjectProp(GetObjectProp(pPersistent, lsl.Strings[i])) then
        inc(result);
    end;
  finally
    lsl.Free;
  end;
end;

function TtiTreeView.CanShowObjectProp(AValue: TObject): boolean;
var
  lMapping: TtiTVDataMapping;
  i: integer;
begin
  result := false;

  // A TtiObject
  if (AValue is TtiObject) then
  begin
    lMapping := FindMapping(TtiObject(AValue));
    result := ((lMapping <> nil) and
      (IsPublishedProp(TtiObject(AValue), lMapping.DisplayPropName)));
    Exit; //==>
  end;

  // A TList
  if (AValue is TList) then
  begin
    for i := 0 to TList(AValue).Count - 1 do
    begin
      if TObject(TList(AValue).Items[i]) is TtiObject then
      begin
        result := CanShowObjectProp(TObject(TList(AValue).Items[i]));
        if result then
          Exit; //==>
      end;
    end;
  end;

end;

procedure TtiTreeView.DoOnChange(sender: TObject; node: TTreeNode);
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  if Node = nil then
    Exit; //==>

  if Assigned(FOnSelectNode) then
  begin

    lNode := nil;
    lData := nil;
    lParentNode := nil;
    lParentData := nil;
    lMapping := nil;

    // Get the current node's information
    GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

    FOnSelectNode(Self, lNode, lData, lParentNode, lParentData);

  end;
  FCtrlBtnPnl.EnableButtons;

end;

procedure TtiTreeView.DoCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  function _GetCanvasColor(pColor: TColor): TColor;
  begin
    if FReadOnly then
      result := clBtnFace // clSilver
    else
      result := pColor;
  end;
begin
  if (Sender.Focused) and
    (Sender.Selected = Node) then
  begin
    FTV.Canvas.Brush.Color := clNavy;
    FTV.Canvas.Font.Color := clWhite;
  end
  else if (Sender.Focused) and
    (Sender.Selected <> Node) then
  begin
    FTV.Canvas.Brush.Color := _GetCanvasColor(clWhite);
    if Assigned(FOnGetFont) then
      FOnGetFont(Node, TObject(Node.Data), TV.Canvas.Font, TV.Canvas)
    else
      FTV.Canvas.Font.Color := clBlack;
  end
  else if (not Sender.Focused) and
    (Sender.Selected = Node) then
  begin
    FTV.Canvas.Brush.Color := _GetCanvasColor(clBtnFace{clSilver});
    if Assigned(FOnGetFont) then
      FOnGetFont(Node, TObject(Node.Data), TV.Canvas.Font, TV.Canvas)
    else
      FTV.Canvas.Font.Color := clBlack;
  end
  else if (not Sender.Focused) and
    (Sender.Selected <> Node) then
  begin
    FTV.Canvas.Brush.Color := _GetCanvasColor(clWhite);
    if Assigned(FOnGetFont) then
      FOnGetFont(Node, TObject(Node.Data), TV.Canvas.Font, TV.Canvas)
    else
      FTV.Canvas.Font.Color := clBlack;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTVDataMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTVDataMapping.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FsDisplayPropName := 'Caption';
  FsDataClassName := 'TtiObject';
  FName := ClassNameToCollectionItemName(FsDataClassName);
  FiImageIndex := -1;
  FbHasChildren := true;
  FbCanDelete := false;
  FbCanInsert := false;
  FbCanEdit := false;
  FbCanDragCopy := false;
  FbCanDragMove := false;
end;



destructor TtiTVDataMapping.Destroy;
begin
  inherited;
end;



procedure TtiTVDataMapping.Assign(Source: TtiTVDataMapping);
begin
  DataClass := Source.DataClass;
  DisplayPropName := Source.DisplayPropName;
  ImageIndex := Source.ImageIndex;
  OnGetDataPage := Source.OnGetDataPage;
  HasChildren := Source.HasChildren;
  { TODO : What about asigning the events? }
end;



function TtiTVDataMapping.GetDisplayName: string;
var
  lsCreate: string;
  lsDelete: string;
  lsUpdate: string;
begin

  { TODO : Assigned(FOnCanInsert) is not returning the correct result }
  if Assigned(FOnCanInsert) then
    lsCreate := '?'
  else if FbCanInsert then
    lsCreate := 'Y'
  else
    lsCreate := 'N';
  lsCreate := 'Create-' + lsCreate;

  if Assigned(FOnCanEdit) then
    lsUpdate := '?'
  else if FbCanEdit then
    lsUpdate := 'Y'
  else
    lsUpdate := 'N';
  lsUpdate := 'Update-' + lsUpdate;

  if Assigned(FOnCanDelete) then
    lsDelete := '?'
  else if FbCanDelete then
    lsDelete := 'Y'
  else
    lsDelete := 'N';
  lsDelete := 'Delete-' + lsDelete;

  result := lsCreate + ', ' +
    lsUpdate + ', ' +
    lsDelete + ': ' + DataClass;

  if Result = '' then
    Result := inherited GetDisplayName;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTVDataMappings
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTVDataMappings.Create(pTreeView: TtiTreeView);
begin
  inherited Create(TtiTVDataMapping);
  FTreeView := pTreeView;
  FiItemNo := 0;
  //  Name     := ClassName + IntToStr(uiMappingCount);
end;



function TtiTVDataMappings.Add: TtiTVDataMapping;
begin
  result := TtiTVDataMapping(inherited Add);
  //  SetItemName(result);
end;



function TtiTVDataMappings.GetItem(Index: Integer): TtiTVDataMapping;
begin
  Result := TtiTVDataMapping(inherited GetItem(Index));
end;



function TtiTVDataMappings.GetOwner: TPersistent;
begin
  result := FTreeView;
end;



procedure TtiTVDataMappings.SetItem(Index: Integer; const AValue: TtiTVDataMapping);
begin
  inherited SetItem(Index, AValue);
end;


{
procedure TtiTVDataMappings.SetItemName(Item: TCollectionItem);
begin
  (Item as TtiTVDataMapping).Name := 'TVDataMapping' + IntToStr(FiItemNo);
  Inc(FiItemNo);
end;
}



procedure TtiTVDataMappings.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;



procedure TtiTreeView.SetTVDataMappings(const AValue: TtiTVDataMappings);
begin
  FTVDataMappings.Assign(AValue);
end;



function TtiTreeView.GetSelectedAddress: string;
var
  lNode: TTreeNode;
begin
  result := '';
  lNode := Selected;
  if lNode = nil then
    Exit; //==>
  result := IntToStr(lNode.Index);
  while lNode.Parent <> nil do
  begin
    lNode := lNode.Parent;
    result := IntToStr(lNode.Index) + '.' + result;
  end;
end;



procedure TtiTreeView.SetSelectedAddress(const AValue: string);
var
  i: integer;
  liAddress: integer;
  lsAddress: string;
  lNode: TTreeNode;
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
      if liAddress <= Items.Count - 1 then
        lNode := Items[liAddress];
    end
    else
    begin
      if liAddress <= lNode.Count - 1 then
      {$IFNDEF FPC}
        lNode := lNode.Item[liAddress]
       {$ELSE}
        lNode := lNode.Items[liAddress]
       {$ENDIF}
      else
        lNode := nil;
    end;

    // Select, then expand this node
    if lNode <> nil then
    begin
      // Expand the node if we still have to drill down further
      if lsAddress <> '' then
        lNode.Expand(false);
      Selected := lNode;
    end
    else
      Break; //==>
  end;
end;

function TtiTreeView.CanDelete: boolean;
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or IsUpdating or
     (not FTV.Focused) or
     (not (tiLVBtnVisDelete in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons) then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  result := lMapping.CanDelete;
  if Assigned(lMapping.OnCanDelete) then
    lMapping.OnCanDelete(self, lNode, lData, lParentNode, lParentData, result);

end;

function TtiTreeView.CanInsert: boolean;
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or
     IsUpdating or (not FTV.Focused) or
     (not (tiLVBtnVisNew in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons) then
    Exit; //==>
  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  result := lMapping.CanInsert;
  if Assigned(lMapping.OnCanInsert) then
    lMapping.OnCanInsert(self, lNode, lData, lParentNode, lParentData, result);

end;

function TtiTreeView.CanEdit: boolean;
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or IsUpdating or
     (not FTV.Focused) or
     (not (tiLVBtnVisDelete in VisibleButtons))  or
     (ButtonStyle = lvbsNoButtons) then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  result := lMapping.CanEdit;
  if Assigned(lMapping.OnCanEdit) then
    lMapping.OnCanEdit(self, lNode, lData, lParentNode, lParentData, result);

end;

function TtiTreeView.GetImages: TCustomImageList;
begin
  result := TV.Images;
end;

procedure TtiTreeView.SetImages(const AValue: TCustomImageList);
begin
  TV.Images := AValue;
end;

function TtiTreeView.GetOnChanging: TTVChangingEvent;
begin
  result := TV.OnChanging;
end;

procedure TtiTreeView.SetOnChanging(const AValue: TTVChangingEvent);
begin
  TV.OnChanging := AValue;
end;



function TtiTreeView.GetItems: TTreeNodes;
begin
  result := TV.Items;
end;



procedure TtiTreeView.SetItems(const AValue: TTreeNodes);
begin
  TV.Items := AValue;
end;



function TtiTreeView.GetSelected: TTreeNode;
begin
  result := FTV.Selected;
end;



procedure TtiTreeView.SetSelected(const AValue: TTreeNode);
begin
  FTV.Selected := AValue;
end;



procedure TtiTreeView.FullExpand(pLevel : integer = -1);
var
  lSelectedAddress: string;
begin

  if FData = nil then
    Exit; //==>

  if FbSettingData then
  begin
    FbFullExpand := true;
    FFullExpandLevel := pLevel;
    Exit; //==>
  end;

  lSelectedAddress := SelectedAddress;
  FbFullExpand := false;
  FTV.FullExpand;
  SelectedAddress := lSelectedAddress;

end;



procedure TtiTreeView.FullCollapse;
begin
  FTV.FullCollapse;
  FTV.Selected := FTV.Items[0];
end;



procedure TtiTreeView.SetName(const AValue: TComponentName);
begin
  inherited SetName(AValue);
  TV.Name := Name + '_TV';
end;

function TtiTVDataMapping.ClassNameToCollectionItemName(const AValue: string): string;
begin
  result := cusMethodPrefix + Copy(AValue, 2, Length(AValue) - 1);
end;



procedure TtiTVDataMapping.SetDataClassName(const AValue: string);
begin
  if FName = ClassNameToCollectionItemName(FsDataClassName) then
    FName := ClassNameToCollectionItemName(AValue);
  FsDataClassName := AValue;
end;

function TtiTreeView.FindNodeByData(AData: TObject): TTreeNode;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Items.Count - 1 do
    if TObject(Items[i].Data) = AData then
    begin
      result := Items[i];
      Break; //==>
    end;
end;



procedure TtiTreeView.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure ExpandNodeByData(ptiTreeView: TtiTreeView;
    AList: TList);
  var
    i: integer;
    lNode: TTreeNode;
  begin
    for i := 0 to AList.Count - 1 do
    begin
      lNode := ptiTreeView.FindNodeByData(TObject(AList.Items[i]));
      if lNode <> nil then
        lNode.Expand(false);
    end;
  end;

  function GetExpanded(ptiTreeView: TtiTreeView): TList;
  var
    i: integer;
  begin
    result := TList.Create;
    for i := 0 to ptiTreeView.Items.Count - 1 do
      if ptiTreeView.Items[i].Expanded then
        result.Add(ptiTreeView.Items[i].Data);
  end;

var
  lNode: TTreeNode;
  lTargetData: TtiObject;
  lParentNode: TTreeNode;
  lTargetParentData: TtiObject;
  lMapping: TtiTVDataMapping;
  lDragObject: TtiTVDragObject;
  lExpSource: TList;
  lExpTarget: TList;
  lSelectedData: TObject;
begin

  lNode := FTV.GetNodeAt(X, Y);

  if lNode = nil then
    Exit; //==>

  lTargetData := nil;
  lParentNode := nil;
  lTargetParentData := nil;
  lMapping := nil;
  lSelectedData := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lTargetData, lParentNode, lTargetParentData, lMapping);

  if (lMapping = nil) or
    (not Assigned(lMapping.OnDrop)) then
    Exit; //==>

  lDragObject := Source as TtiTVDragObject;

  // If we have left+clicked, and moved the mouse a little, a drag event may
  // have been triggered, this is probably not what we are wanting, so throw
  // this event away.
  if lDragObject.Data = lTargetData then
    Exit; //==>

  lExpSource := GetExpanded(lDragObject.tiTreeView);
  lExpTarget := GetExpanded(Self);
  try
    lMapping.OnDrop(lDragObject.tiTreeView, // Source TV
      lDragObject.Data, // Source data
      lDragObject.ParentData, // Source parent data
      Self, // Target TV
      lTargetData, // Target data
      lTargetParentData, // Target parent data
      lSelectedData); // Data to select after drop

    lDragObject.tiTreeView.RefreshTreeNode(lDragObject.tiTreeView, lDragObject.ParentNode);
    RefreshTreeNode(Self, lParentNode);
    ExpandNodeByData(Self, lExpTarget);
    ExpandNodeByData(lDragObject.tiTreeView, lExpSource);

  finally
    lExpSource.Free;
    lExpTarget.Free;
  end;

  //tiShowMessage(['Parent node', FindNodeByData(lTargetData).Text]);

  if FindNodeByData(lTargetData) <> nil then
    RefreshTreeNode(Self, FindNodeByData(lTargetData));
  //    FindNodeByData(lParentData).Expand(false);
  //  end;

  if lSelectedData <> nil then
    SelectedData := lSelectedData;

end;



procedure TtiTreeView.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
  lDragObject: TtiTVDragObject;
begin

  Accept := false;

  lNode := FTV.GetNodeAt(X, Y);
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if (lMapping = nil) or
    (not Assigned(lMapping.OnCanAcceptDrop)) then
    Exit; //==>

  lDragObject := Source as TtiTVDragObject;

  lMapping.OnCanAcceptDrop(lDragObject.tiTreeView,
    lDragObject.Data,
    lDragObject.ParentData,
    Self,
    lData,
    lParentData,
    Accept);

end;



procedure TtiTreeView.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  // Only allow drag if mouse button is mbLeft or mbRight
  if (Button <> mbLeft) then
    Exit; //==>

  if ReadOnly then
    Exit; //==>

  lNode := FTV.GetNodeAt(X, Y);
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if (lNode = nil) or
    (lData = nil) or
    (lMapping = nil) then
    Exit; //==>

  if (lMapping.CanDragMove and not (ssCtrl in Shift)) or
    (lMapping.CanDragCopy and (ssCtrl in Shift)) then
    FTV.BeginDrag(false);

end;



procedure TtiTreeView.DoStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
  lDragObject: TtiTVDragObject;
begin

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);
  lDragObject := TtiTVDragObject.Create{$IFDEF FPC}(nil){$ENDIF};
  lDragObject.Data := lData;
  lDragObject.ParentData := lParentData;
  lDragObject.tiTreeView := Self;
  lDragObject.Node := lNode;
  lDragObject.ParentNode := lParentNode;

  DragObject := lDragObject;

end;



function TtiTreeView.GetSelectedData: TObject;
begin
  result := nil;
  if Selected = nil then
    Exit; //==>
  if Selected.Data = nil then
    Exit; //==>
  result := TObject(Selected.Data);
end;



procedure TtiTreeView.SetSelectedData(const AValue: TObject);
var
  i: integer;
begin
  if AValue = nil then
  begin
    if FTV.Items.Count > 0 then
      FTV.Selected := FTV.Items[0];
    Exit; //==>
  end;

  // ToDo: Add code so the FullExpand is not required
  // and if Node.Data can not be found to match AValue, then scan for child nodes
  // that have not been read yet.
  FullExpand;
  for i := 0 to FTV.Items.Count - 1 do
    if TObject(FTV.Items[i].Data) = AValue then
    begin
      FTV.Selected := FTV.Items[i];
      Break; //==>
    end;

end;

function TtiTreeView.GetTreeSortType: TSortType;
begin
  Result := TV.SortType;
end;

procedure TtiTreeView.SetTreeSortType(const AValue: TSortType);
begin
  TV.SortType := AValue;
end;

procedure TtiTreeView.SetApplyFilter(const AValue: boolean);
begin
  FbApplyFilter := AValue;
  { TODO : TtiTreeView.SetApplyFilter should re-read tree nodes }
end;

function TtiTreeView.GetOnKeyDown: TKeyEvent;
begin
  result := FTV.OnKeyDown;
end;

procedure TtiTreeView.SetOnKeyDown(const AValue: TKeyEvent);
begin
  FTV.OnKeyDown := AValue;
end;

procedure TtiTreeView.SetReadOnly(const AValue: boolean);
begin
  FReadOnly := AValue;
  if FReadOnly then
    FTV.Color := clBtnFace
    // FTV.Color := clSilver
  else
    FTV.Color := clWhite;
end;

procedure TtiTreeView.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FSaveCursor := Cursor;
    Cursor := crHourGlass;
    FTV.Items.BeginUpdate;
  end;
  Inc(FUpdateCount);
end;

procedure TtiTreeView.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    FTV.Items.EndUpdate;
    Cursor := FSaveCursor;
  end;
end;

procedure TtiTreeView.SetEnableTree(const AValue: boolean);
begin
  if FTV.Enabled <> AValue then
    FTV.Enabled:=AValue;
end;

function TtiTreeView.GetEnableTree: boolean;
begin
  result:=FTV.Enabled;
end;

procedure TtiTreeView.RefreshCurrentNode;
var
  lCurrentNode : TTreeNode;
begin
  lCurrentNode := TV.Selected;
  if lCurrentNode = nil then
    Exit; //==>
  RefreshTreeNode(Self,lCurrentNode);
end;

procedure TtiTreeView.SetFocus;
begin
  if not Enabled then
    Exit; //==> 
  inherited SetFocus;
  TV.SetFocus;
end;

function TtiTVDataMappings.Add(const AClass: TClass;
  const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping;
begin
  Result := Add;
  Result.DataClass := AClass.ClassName;
  Result.DisplayPropName := pDisplayPropName;
  Result.ImageIndex := pImageIndex;
end;

function TtiTreeView.GetButtonStyle: TLVButtonStyle;
begin
  Result := FCtrlBtnPnl.ButtonStyle;
end;

function TtiTreeView.GetVisibleButtons: TtiLVVisibleButtons;
begin
  result := FCtrlBtnPnl.VisibleButtons;
end;

procedure TtiTreeView.SetButtonStyle(const AValue: TLVButtonStyle);
begin
  tiCtrlButtonPanel.CreateCtrlBtnPnl(FCtrlBtnPnl, AValue, Self,
                                      CanView, CanInsert, CanEdit, CanDelete);
  FCtrlBtnPnl.OnNew      := DoInsert;
  FCtrlBtnPnl.OnEdit     := DoEdit;
  FCtrlBtnPnl.OnDelete   := DoDelete;
  FCtrlBtnPnl.RefreshButtons;
  DoReSize(nil);
end;

procedure TtiTreeView.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FCtrlBtnPnl.VisibleButtons := AValue;
  DoReSize(Nil); 
end;

procedure TtiTreeView.DoReSize(Sender: TObject);
var
  lTop : integer;
begin

  if FCtrlBtnPnl.VisibleButtons = [] then
    lTop := 1
  else
    lTop := FCtrlBtnPnl.Height + 2;

  FTV.SetBounds(
    1,
    lTop,
    Width - 2,
    Height - lTop - 1);
    
end;

procedure TtiTreeView.DoDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self, Selected, SelectedData, nil, nil)
  else if CanEdit then
    DoEdit(nil);
end;

function TtiTreeView.CanView: boolean;
var
  lNode: TTreeNode;
  lData: TtiObject;
  lParentNode: TTreeNode;
  lParentData: TtiObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or IsUpdating or
     (not FTV.Focused) or
     (not (tiLVBtnVisView in VisibleButtons)) or
     (ButtonStyle = lvbsNoButtons) then
    Exit; //==>

  lNode := nil;
  lData := nil;
  lParentNode := nil;
  lParentData := nil;
  lMapping := nil;

  // Get the current node's information
  GetNodeInfo(lNode, lData, lParentNode, lParentData, lMapping);

  if lMapping = nil then
    Exit; //==>

  result := lMapping.CanView;
  if Assigned(lMapping.OnCanView) then
    lMapping.OnCanView(self, lNode, lData, lParentNode, lParentData, result);
end;


procedure TtiTreeView.DoOnEnter(Sender: TObject);
begin
  FCtrlBtnPnl.EnableButtons;
end;

procedure TtiTreeView.DoOnExit(Sender: TObject);
begin
  FCtrlBtnPnl.EnableButtons;
end;

procedure TtiTreeView.RefreshCurrentNodesParent;
var
  lSelectedData: TtiObject;
begin
  if (Selected = nil) or (Selected.Parent = nil) then
    Exit; //==>
  BeginUpdate;
  try
    lSelectedData := TtiObject(Selected.Data);
    Selected := Selected.Parent;
    RefreshCurrentNode;
    SelectedData := lSelectedData;
  finally
    EndUpdate;
  end;
end;

function TtiTreeView.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

end.

