{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/       
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au
                                   
  Revision history:
    May 2000, Peter Hinrichsen, Made open source
    August 2002 Ian Krigsman, fixed node selection bug. Change made by Andrew Denton

  Purpose: A TTreeView for browsing a nested list of
           TPersistent(s) and TList(s)

  ToDo:
    1. Create a component editor so a double click on the TtiTreeView loads
       a TListView of node mappings.
    2. Create a method like FullExpand, but which takes a parameter giving the
       number of levels to expand to.
    3. Had to introduce a TTimer in SetData :( to solve some problems when
       the data prop is set in a form's constructor. There must be a better
       solution than this, but what is it?

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiTreeView;

interface
uses
  ComCtrls
  , CommCtrl
  , Classes
  , Menus
  , Graphics
  , ExtCtrls
  , Controls
  , ImgList
  , Forms

  , Contnrs
  , Messages
  , Windows
  ;

const
  cuiSplitterPos = 175;

type

  {
    // This stub of code defines the interface a child form
    // must have if it is to be displayed as in the right
    // hand pane of the TtiTreeView
    TMyChildForm = class(TForm)
    private
      FData: TPersistent; // Can be any TPersistent descendant
      FTreeNode: TTreeNode;
      function  GetValid: boolean;
      procedure SetData(const Value: TPersistent);
    published
      property Data : TPersistent read FData write SetData ;
      property Valid : boolean    read GetValid ;
      property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    public
    end;
  }

  TtiTreeView = class;

  TtiTVNodeEvent = procedure(ptiTreeView: TtiTreeView;
    pNode: TTreeNode;
    pData: TObject;
    pParentNode: TTreeNode;
    pParentData: TObject) of object;

  TtiTVNodeConfirmEvent = procedure(ptiTreeView: TtiTreeView;
    pNode: TTreeNode;
    pData: TObject;
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

  TTVGetDataPageEvent = procedure(pData: TObject;
    pNode: TTreeNode) of object;
  TtiTVGetFontEvent = procedure(pNode: TTreeNode;
    pData: TObject;
    pFont: TFont;
    pCanvas: TCanvas) of object;
  TTVUpdateNodeText = procedure(const psValue: string) of object;
  TtiTVGetImageIndexEvent = procedure(pData: TObject;
    var piImageIndex: integer) of object;

  TtiTVOnFilterDataEvent = procedure(pData: TObject;
    var pbInclude: boolean) of object;

  TtiTVDataClassRef = class of TPersistent;
  TtiTVFormClassRef = class of TForm;
  TTVOnGetChildFormEvent = procedure(pChildForm: TForm;
    pData: TObject;
    pNode: TTreeNode) of object;

  //----------------------------------------------------------------------------
  TtiTVDataMapping = class(TCollectionItem)
  private
    FsDisplayPropName: string;
    FsDataClassName: string;
    FiImageIndex: integer;
    FOnGetDataPage: TTVGetDataPageEvent;
    FbHasChildren: boolean;
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
    procedure SetDataClassName(const Value: string);
    function ClassNameToCollectionItemName(const pValue: string): string;
  protected
    function GetDisplayName: string; override;
    property OnGetDataPage: TTVGetDataPageEvent read FOnGetDataPage write FOnGetDataPage;
  published
    // Properties
    property CanDelete: boolean read FbCanDelete write FbCanDelete default false;
    property CanInsert: boolean read FbCanInsert write FbCanInsert default false;
    property CanEdit: boolean read FbCanEdit write FbCanEdit default false;
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

    property OnCanInsert: TtiTVNodeConfirmEvent read FOnCanInsert write FOnCanInsert;
    property OnCanDelete: TtiTVNodeConfirmEvent read FOnCanDelete write FOnCanDelete;
    property OnCanEdit: TtiTVNodeConfirmEvent read FOnCanEdit write FOnCanEdit;
    property OnCanAcceptDrop: TtiTVDragDropConfirmEvent read FOnCanAcceptDrop write FOnCanAcceptDrop;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TtiTVDataMapping); reintroduce;
  end;

  //----------------------------------------------------------------------------
  TtiTVDataMappings = class(TCollection)
  private
    FTreeView: TtiTreeView;
    FiItemNo: integer;
    function GetItem(Index: Integer): TtiTVDataMapping;
    procedure SetItem(Index: Integer; const Value: TtiTVDataMapping);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(pTreeView: TtiTreeView);
    function Add: TtiTVDataMapping;
    property Items[Index: Integer]: TtiTVDataMapping read GetItem write SetItem;
  end;

  //----------------------------------------------------------------------------
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
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property TV: TtiTreeView read FTV write FTV;
  end;

  //----------------------------------------------------------------------------
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

  //----------------------------------------------------------------------------
  TtiTVDataFormMapping = class(TObject)
  private
    FDataClass: TClass;
    FForm: TForm;
  public
    property DataClass: TClass read FDataClass write FDataClass;
    property FormInstance: TForm read FForm write FForm;
  end;

  //----------------------------------------------------------------------------
  TtiTVDataFormMappings = class(TObjectList)
  public
    function FindByDataClass(pDataClass: TClass; pExactMatch: boolean = false): TtiTVDataFormMapping;
  end;

  //----------------------------------------------------------------------------
  TtiTVFormInstances = class(TObjectList)
  public
    function FindByFormClass(pFormClass: TFormClass;
      pbMultiInstance: boolean): TForm;
  end;

  TtiTVSplitter = class(TSplitter)
  private
    FbMouseOver: boolean;
  protected
    procedure Paint; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawGrabBar(pRect: TRect);
  public
    constructor Create(Owner: TComponent); override;
  end;

  //----------------------------------------------------------------------------
  TtiTreeView = class(TCustomPanel)
  private
    FTV: TTreeView;
    FData: TPersistent;
    FTVDataMappings: TtiTVDataMappings;
    FNodesLoaded: TList;

    FPopupMenu: TTVPopupMenu;
    FOnDelete: TtiTVNodeEvent;
    FOnInsert: TtiTVNodeEvent;
    FOnSelectNode: TtiTVNodeEvent;

    FOnGetImageIndex: TtiTVGetImageIndexEvent;
    FOnGetFont: TtiTVGetFontEvent;
    FOnFilterData: TtiTVOnFilterDataEvent;

    FSplitter: TtiTVSplitter;
    FDataFormMappings: TtiTVDataFormMappings;
    FFormInstances: TtiTVFormInstances;
    FCurrentChildForm: TForm;
    FOnAfterGetChildForm: TTVOnGetChildFormEvent;
    FbHasChildForms: boolean;
    FOnEdit: TtiTVNodeEvent;
    FtmrSetData: TTimer;
    FbSettingData: boolean;
    FsSelectedAddress: string;
    FbFullExpand: boolean;
    FFullExpandLevel : integer;
    FbApplyFilter: boolean;
    FReadOnly: boolean;
    FUpdating: boolean;
    FSaveCursor: TCursor;

    procedure DoOnExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    function HasNodeChildren(pValue: TObject): boolean;
    procedure AddNodeChildren(pNode: TTreeNode;
      pData: TObject);
    procedure GetObjectPropNames(pPersistent: TObject; pSL: TStringList);
    function CountObjectProps(pPersistent: TObject): integer;
    procedure DoOnChange(sender: TObject; node: TTreeNode);
    function CanShowObjectProp(pValue: TObject): boolean;
    procedure DoCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode;
      State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure SetTVDataMappings(const Value: TtiTVDataMappings);
    function FindMapping(pData: TPersistent): TtiTVDataMapping;
    function GetSelectedAddress: string;
    procedure SetSelectedAddress(const Value: string);
    procedure GetNodeInfo(var pNode: TTreeNode;
      var pData: TObject;
      var pParentNode: TTreeNode;
      var pParentData: TObject;
      var pMapping: TtiTVDataMapping);
    function GetCanDeleteSelected: boolean;
    function GetCanInsertSelected: boolean;
    function GetCanEditSelected: boolean;

    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    function GetOnChanging: TTVChangingEvent;
    procedure SetOnChanging(const Value: TTVChangingEvent);
    function GetItems: TTreeNodes;
    procedure SetItems(const Value: TTreeNodes);
    function GetSelected: TTreeNode;
    procedure SetSelected(const Value: TTreeNode);

    procedure SetSplitterPos(const Value: integer);
    function  GetSplitterPos: integer;
    procedure SetHasChildForms(const Value: boolean);
    procedure ClearCurrentChildForm;
    procedure DoOnChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure GetDataPage(pData: TObject; pNode: TTreeNode);
    function GetOnDblClick: TNotifyEvent;
    procedure SetOnDblClick(const Value: TNotifyEvent);
    procedure RefreshTreeNode(ptiTreeView: TtiTreeView; pTreeNode: TTreeNode);

    // Drag & Drop Sender side events
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoStartDrag(Sender: TObject; var DragObject: TDragObject); reintroduce;
    //    procedure DoEndDrag(   Sender, Target: TObject; X, Y: Integer); Reintroduce ;

        // Drag & Drop Receiver side events
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    function GetSelectedData: TObject;
    procedure SetSelectedData(const Value: TObject);
    function GetTreeSortType: TSortType;
    procedure SetTreeSortType(const Value: TSortType);
    procedure DoSetData(Sender: TObject);
    function GetSplitterVisible: boolean;
    procedure SetSplitterVisible(const Value: boolean);
    procedure SetApplyFilter(const Value: boolean);
    procedure GetNodeImageIndex(pNode: TTreeNode; pData: TPersistent;
      pDataMapping: TtiTVDataMapping);
    function GetOnKeyDown: TKeyEvent;
    procedure SetOnKeyDown(const Value: TKeyEvent);
    procedure SetEnableTree(const Value: boolean);
    function GetEnableTree: boolean;

  protected
    procedure SetData(const Value: TPersistent); virtual;
    procedure SetName(const Value: TComponentName); override;
    property NodesLoaded: TList read FNodesLoaded;
    procedure SetReadOnly(const Value: boolean); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;

  public
    constructor Create(owner: TComponent); override;
    destructor  destroy; override;
    procedure   SetFocus ; override ;

    property TV: TTreeView read FTV;
    property Items: TTreeNodes read GetItems write SetItems;
    property Data: TPersistent read FData write SetData;
    property CurrentChildForm: TForm read FCurrentChildForm;
    property Selected: TTreeNode read GetSelected write SetSelected;

    procedure UpdateNodeText(const psValue: string);
    procedure RefreshCurrentNode ;
    procedure DoInsert;
    procedure DoDelete;
    procedure DoEdit;
    procedure FullExpand( pLevel : integer = -1 ) ;
    procedure FullCollapse;
    procedure RegisterChildForm(pDataClassRef: TtiTVDataClassRef;
      pFormClassRef: TtiTVFormClassRef;
      pbMultiInstance: boolean = false);
    function IsCurrentChildFormValid: boolean;
    function FindNodeByData(pData: TObject): TTreeNode;
    property SelectedData: TObject read GetSelectedData write SetSelectedData;
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

    property Images: TCustomImageList read GetImages write SetImages;
    property CanDeleteSelected: boolean read GetCanDeleteSelected;
    property CanInsertSelected: boolean read GetCanInsertSelected;
    property CanEditSelected: boolean read GetCanEditSelected;

    property OnChanging: TTVChangingEvent read GetOnChanging write SetOnChanging;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;

    property OnInsert: TtiTVNodeEvent read FOnInsert write FOnInsert;
    property OnDelete: TtiTVNodeEvent read FOnDelete write FOnDelete;
    property OnEdit: TtiTVNodeEvent read FOnEdit write FOnEdit;
    property OnSelectNode: TtiTVNodeEvent read FOnSelectNode write FOnSelectNode;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;

    // We can't publish OnChange as is is used internally
    // property OnChange ;
    property OnGetImageIndex: TtiTVGetImageIndexEvent
      read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetFont: TtiTVGetFontEvent
      read FOnGetFont write FOnGetFont;
    property OnFilterData: TtiTVOnFilterDataEvent
      read FOnFilterData write FOnFilterData;
    property DataMappings: TtiTVDataMappings read FTVDataMappings write SetTVDataMappings;
    property SelectedAddress: string read GetSelectedAddress write SetSelectedAddress;

    property SplitterPos: integer read GetSplitterPos write SetSplitterPos default cuiSplitterPos;
    property HasChildForms: boolean read FbHasChildForms write SetHasChildForms default false;
    property OnAfterGetChildForm: TTVOnGetChildFormEvent
      read FOnAfterGetChildForm
      write FOnAfterGetChildForm;

    property TreeSortType: TSortType read GetTreeSortType write SetTreeSortType;

    property SplitterVisible: boolean
      read GetSplitterVisible
      write SetSplitterVisible default true;
    property ApplyFilter: boolean
      read FbApplyFilter
      write SetApplyFilter default false;
    property ReadOnly: boolean
      read FReadOnly
      write SetReadOnly default false;
    property EnableTree : boolean read GetEnableTree write SetEnableTree default true;
  end;

function IsClassOfType(pData: TObject; pClassName: string): boolean;

implementation
uses
  SysUtils
  , TypInfo
  //  ,tiLog  // If this unit is not commented, then it is my error as I have been
  //            // doing some debugging and forgot to clean up my mess.
  ;

{$R tiTreeView.dcr}

const
  cusMethodPrefix = 'tiTVMapping';
  cuColorSplitterGrabBar = $00FE9E83; // Pale navy blue
  cuiSplitterWidth = 8;

  // Take an object, and a string class name and retrun true if the object
  // is, or descends from the string class name.
  //------------------------------------------------------------------------------

function IsClassOfType(pData: TObject; pClassName: string): boolean;
var
  lsClassName: string;
  lClass: TClass;
begin
  lsClassName := upperCase(pClassName);
  lClass := pData.ClassType;
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

constructor TtiTreeView.create(owner: TComponent);
begin
  inherited create(owner);

  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;

  FTV := TTreeView.Create(self);
  FTV.Parent := self;
  FTV.Name := Name + '_TV';
  FTV.Align := alClient;

  FTVDataMappings := TtiTVDataMappings.Create(self);
  FTV.OnExpanding := DoOnExpanding;
  FTV.OnChange := DoOnChange;
  FTV.OnCustomDrawItem := DoCustomDrawItem;
  FTV.ReadOnly := true;
  FTV.ChangeDelay := 500;

  FPopupMenu := TTVPopupMenu.Create(nil);
  FPopupMenu.TV := self;
  FTV.PopupMenu := FPopupMenu;
  FTV.OnChanging := DoOnChanging;

  FTV.DragMode := dmManual;

  FTV.OnMouseDown := DoMouseDown;
  FTV.OnStartDrag := DoStartDrag;
  FTV.OnDragOver := DoDragOver;
  FTV.OnDragDrop := DoDragDrop;

  FbApplyFilter := false;

  FNodesLoaded := TList.Create;

  FSplitter          := TtiTVSplitter.Create(self);
  FSplitter.Parent   := self;
  FSplitter.Left     := cuiSplitterPos;
  FSplitter.Align    := alLeft;
  FSplitter.Beveled  := false;
  FSplitter.Width    := 0;
  FSplitter.Visible  := false;
  FSplitter.AutoSnap := false ;
  FSplitter.MinSize  := 10 ;

  FDataFormMappings := TtiTVDataFormMappings.Create;
  FFormInstances := TtiTVFormInstances.Create;
  FbHasChildForms := false;

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
end;

//------------------------------------------------------------------------------

destructor TtiTreeView.destroy;
begin
  FSplitter.Free;
  FDataFormMappings.Free;
  FFormInstances.Free;
  FTVDataMappings.Free;
  FPopupMenu.Free;
  FNodesLoaded.Free;
  FtmrSetData.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TtiTreeView.DoOnExpanding(Sender: TObject;
  Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  lDataMapping: TtiTVDataMapping;
begin

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
        AddNodeChildren(Node, Node.Data);
      end;

    { TODO : Assign parentNode and ParentData }
      if Assigned(OnSelectNode) then
        OnSelectNode(Self, Node, Node.Data, nil, nil); // !!.01 bug fix IK

      GetDataPage(Node.Data, Node);

    // Execute the collection based OnGetDataPage, if one exists
      lDataMapping := FindMapping(TObject(Node.Data) as TPersistent);
      if (lDataMapping <> nil) and
        (assigned(lDataMapping.OnGetDataPage)) then
        lDataMapping.OnGetDataPage(Node.Data, Node);
    end;
  finally
    EndUpdate;
  end;

end;

// Given a data object, which is a TPersistent, scan the DataMappings
// collection to find a mapping.
//------------------------------------------------------------------------------

function TtiTreeView.FindMapping(pData: TPersistent): TtiTVDataMapping;
var
  i: integer;
  lDataMapping: TtiTVDataMapping;
begin
  result := nil;
  for i := 0 to DataMappings.Count - 1 do
  begin
    if IsClassOfType(pData, DataMappings.Items[i].DataClass) then
    begin
      lDataMapping := DataMappings.Items[i];
      if IsPublishedProp(pData, lDataMapping.DisplayPropName) then
      begin
        result := lDataMapping;
        Break; //==>
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TtiTreeView.GetNodeImageIndex(pNode: TTreeNode;
  pData: TPersistent;
  pDataMapping: TtiTVDataMapping);
var
  liImageIndex: integer;
  lDataMapping: TtiTVDataMapping;
begin
  if pDataMapping = nil then
    lDataMapping := FindMapping(pData)
  else
    lDataMapping := pDataMapping;

  if lDataMapping = nil then
    Exit; //==>

  // If the OnGetImageIndex event is assinged, then execute it
  if Assigned(FOnGetImageIndex) {and
  Assigned( Images ) }then
  begin
    liImageIndex := lDataMapping.ImageIndex;
    FOnGetImageIndex(pData, liImageIndex);
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

//------------------------------------------------------------------------------

procedure TtiTreeView.AddNodeChildren({ptiTreeView : TtiTreeView ;}
  pNode: TTreeNode;
  pData: TObject);

  procedure AddChildNode({ptiTreeView  : TtiTreeView      ;}
    pData: TPersistent;
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
      if pParentNode.Item[i].Data = pData then
        Exit; //==>

    if FbApplyFilter and Assigned(FOnFilterData) then
    begin
      lbInclude := true;
      FOnFilterData(pData, lbInclude);
      if not lbInclude then
        Exit; //==>
    end;

    lsCaption := GetPropValue(pData, pDataMapping.DisplayPropName);
    lNode := {ptiTreeView.} Items.AddChildObject(pParentNode, lsCaption, pData);
    lNode.HasChildren := HasNodeChildren(pData);
    GetNodeImageIndex(lNode, pData, pDataMapping);

  end;

  procedure AddChildPersistent(pParentNode: TTreeNode;
    pData: TPersistent);
  var
    lDataMapping: TtiTVDataMapping;
  begin
    lDataMapping := FindMapping(pData);
    if lDataMapping = nil then
      exit; //==>
    AddChildNode(pData, lDataMapping, pParentNode);
  end;

  // The object is a TList of TPersistent(s), so add the each item
  // in the list as a child node.
  procedure AddChildList(pParentNode: TTreeNode;
    pData: TList);
  var
    lData: TPersistent;
    i: integer;
    lDataMapping: TtiTVDataMapping;
  begin
    for i := 0 to pData.Count - 1 do
    begin
      if (TObject(pData.Items[i]) is TPersistent) then
      begin
        lData := TPersistent(pData.Items[i]);
        lDataMapping := FindMapping(lData);
        if lDataMapping <> nil then
          AddChildNode(lData, lDataMapping, pParentNode);
      end;
    end;
    pNode.HasChildren := pNode.HasChildren or (pData.Count > 0);
  end;

var
  lChild: TObject;
  i: integer;
  lslObjProps: TStringList;
begin

  lslObjProps := TStringList.Create;
  try
    // Get a list of object type properties
    GetObjectPropNames(pData, lslObjProps);
    // Scan the list of object type properties
    for i := 0 to lslObjProps.Count - 1 do
    begin
      // Get a pointer to an object type property
      lChild := (GetObjectProp(pData, lslObjProps.Strings[i]) as TObject);
      // If it's a TPersistent, then add it
      if (lChild is TPersistent) then
        AddChildPersistent(pNode, TPersistent(lChild))
          // If it's a TList, then add each list element.
      else if (lChild is TList) then
        AddChildList(pNode, TList(lChild));
    end;

  finally
    lslObjProps.Free;
  end;

end;

//------------------------------------------------------------------------------

function TtiTreeView.HasNodeChildren(pValue: TObject): boolean;
var
  lMapping: TtiTVDataMapping;
begin
  // The data value is a TPersistent
  if (pValue is TPersistent) then
  begin
    lMapping := FindMapping(TPersistent(pValue));
    if ((lMapping <> nil) and
      (lMapping.HasChildren)) then
      result :=
        (CountObjectProps(pValue) > 0)

    else if ((lMapping <> nil) and
      (not lMapping.HasChildren)) then
      result := false
    else
      result :=
        (CountObjectProps(pValue) > 0)
        ;

    // The data value is a TList
  end
  else if (pValue is TList) then
  begin
    {
      begin
        if TList( pValue ).Count > 0 then
        begin
          lList := TList( pValue ) ;
          for i := 0 to lList.Count - 1 do
          begin
            result := FindMapping( TPersistent( lList.Items[i] )) <> nil ;
            if result then
              Break ; //==>
          end ;
        end
        else
    }
    result := true;

    // The data value is not a TPersistent or TList
  end
  else
    result := false;

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.SetData(const Value: TPersistent);
begin

  FtmrSetData.Enabled := false;

  Items.Clear;
  FNodesLoaded.Clear;

  FData := Value;
  if Value = nil then
  begin
    ClearCurrentChildForm;
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
      lsCaption := GetPropValue(FData, 'Caption')
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
      FOnSelectNode(Self, lNode, lNode.Data, nil, nil);

    // Is this necessary here /
    // GetDataPage(pData: TObject; pNode: TTreeNode) ;

    if (FData = nil) and
      (HasChildForms) then
      if CurrentChildForm <> nil then
        ClearCurrentChildForm;

  finally
    FbSettingData := false;
  end;

  if FsSelectedAddress <> EmptyStr then
    SetSelectedAddress(FsSelectedAddress);

  if FbFullExpand then
    FullExpand;

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.UpdateNodeText(const psValue: string);
begin
  if Selected <> nil then
    Selected.Text := psValue;
end;

//------------------------------------------------------------------------------

function TtiTreeView.GetSplitterVisible: boolean;
begin
  result := FSplitter.Visible;
end;

//------------------------------------------------------------------------------

procedure TtiTreeView.SetSplitterVisible(const Value: boolean);
begin
  FSplitter.Visible := Value;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TTVPopupMenu
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TTVPopupMenu.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FmiExpandAll := TMenuItem.Create(nil);
  FmiExpandAll.Caption := 'Expand &all';
  FmiExpandAll.OnClick := DoExpandAll;
  //FmiExpand.Shortcut := TextToShortcut( 'Enter' );
  Items.Add(FmiExpandAll);

  FmiExpand := TMenuItem.Create(nil);
  FmiExpand.Caption := 'E&xpand';
  FmiExpand.OnClick := DoExpand;
  FmiExpand.Shortcut := TextToShortcut('Enter');
  Items.Add(FmiExpand);

  FmiInsert := TMenuItem.Create(nil);
  FmiInsert.Caption := '&Insert';
  FmiInsert.OnClick := DoInsert;
  Items.Add(FmiInsert);

  FmiEdit := TMenuItem.Create(nil);
  FmiEdit.Caption := '&Edit';
  FmiEdit.OnClick := DoEdit;
  Items.Add(FmiEdit);

  FmiDelete := TMenuItem.Create(nil);
  FmiDelete.Caption := '&Delete';
  FmiDelete.OnClick := DoDelete;
  Items.Add(FmiDelete);

  OnPopup := DoOnPopup;

end;

//------------------------------------------------------------------------------

destructor TTVPopupMenu.Destroy;
begin
  FmiInsert.Free;
  FmiEdit.Free;
  FmiDelete.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TTVPopupMenu.DoDelete(sender: TObject);
begin
  TV.DoDelete
end;

//------------------------------------------------------------------------------

procedure TTVPopupMenu.DoEdit(sender: TObject);
begin
  TV.DoEdit;
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
  TV.DoInsert;
end;

//------------------------------------------------------------------------------

procedure TTVPopupMenu.DoOnPopup(sender: TObject);
begin
  FmiInsert.Visible := true;
  FmiEdit.Visible   := true;
  FmiDelete.Visible := true;

  FmiExpand.Enabled := FTV.Selected.HasChildren;
  FmiInsert.Enabled := FTV.CanInsertSelected;
  FmiEdit.Enabled   := FTV.CanEditSelected;
  FmiDelete.Enabled := FTV.CanDeleteSelected;

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
  var pData: TObject;
  var pParentNode: TTreeNode;
  var pParentData: TObject;
  var pMapping: TtiTVDataMapping);
begin
  if pNode = nil then
    pNode := Selected;

  if Selected = nil then
    Exit; //==>

  pData := nil;
  pParentNode := nil;
  pParentData := nil;

  if pNode <> nil then
  begin
    pData := pNode.Data;
    pParentNode := pNode.Parent;
    if pParentNode <> nil then
      pParentData := pParentNode.Data;
  end;

  pMapping := FindMapping(TPersistent(pData));

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.RefreshTreeNode(ptiTreeView: TtiTreeView; pTreeNode: TTreeNode);
var
  i: integer;
  lbDummy: boolean;
begin
  // This code will delete, then reread all the child nodes
  // It would be better to check if any notes have been deleted,
  // but this scanning of the parent's data object would be difficult.
  // Delete all the parent's children
  for i := pTreeNode.Count - 1 downto 0 do
  begin
    ptiTreeView.NodesLoaded.Remove(pTreeNode.Item[i].Data);
    ptiTreeView.Items.Delete(pTreeNode.Item[i]);
  end;

  ptiTreeView.NodesLoaded.Remove(pTreeNode.Data);

  // Collapse the parent node
  pTreeNode.Collapse(true);
  // Expand and reread the parent node's children
  DoOnExpanding(ptiTreeView.TV, pTreeNode, lbDummy);

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.DoDelete;
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
  liNodeIndex: integer;
  lbCanDelete: boolean;
  lMapping: TtiTVDataMapping;
begin

  if not CanDeleteSelected then
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
  end
  else
  begin
    lbCanDelete := Assigned(OnDelete);
    if lbCanDelete then
      OnDelete(self, lNode, lData, lParentNode, lParentData);
  end;

  if lbCanDelete then
  begin

    FTV.OnChanging := nil ;
    try
      RefreshTreeNode(Self, lParentNode);
    finally
      FTV.OnChanging := DoOnChanging;
    end ;
    // Reposition the selected node as close as possible to the position
    // of the deleted node.
    if (liNodeIndex > 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Item[liNodeIndex - 1]
    else if (liNodeIndex = 0) and (lParentNode.Count > 0) then
      Selected := lParentNode.Item[0]
    else
      Selected := lParentNode;

  end;

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.DoInsert;
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
  lData: TObject;
  lParentData: TObject;
  lMapping: TtiTVDataMapping;
  lbCanInsert: boolean;
  liParentNodeCount: integer;
  liNodeCount: integer;
  liCount: integer;
begin

  if not CanInsertSelected then
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
    lbCanInsert := Assigned(OnInsert);

    if not lbCanInsert then
      Exit; //==>

    _ExpandAndCountChildren(lParentNode, liParentNodeCount);
    _ExpandAndCountChildren(lNode, liNodeCount);

    // if lbCanInsert then
    OnInsert(self, lNode, lData, lParentNode, lParentData);

  end;

  // An OnInsert method was called, so re-read the child nodes
  if lbCanInsert then
  begin

    // OnInsert( ) may have added data to either lData or lParentData, we
    // don't know. To make things more complicated, lData and lParentData are both
    // TObject(s), so it is difficult to find exactly where the data was added.
    // ( lData/lParentData may be a TPersistent owning any number of TLists, or
    // may be a TList - we don't know )
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
        Selected := lNode.Item[lNode.Count - 1];
      //CurrentChildForm.SetFocus ;
    end;

    // Check to see if we have added any new children to lParentNode
    if lParentNode <> nil then
    begin
      _ExpandAndCountChildren(lParentNode, liCount);
      if liParentNodeCount <> liCount then
        Selected := lParentNode.Item[lParentNode.Count - 1];
      //CurrentChildForm.SetFocus ;
    end;

  end;

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.DoEdit;
var
  lNode: TTreeNode;
  lParentNode: TTreeNode;
  lData: TObject;
  lParentData: TObject;
  lMapping: TtiTVDataMapping;
  lbCanEdit: boolean;
begin

  if not CanEditSelected then
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

  end
  else
  begin
    lbCanEdit := Assigned(OnEdit);
    if lbCanEdit then
      OnEdit(self, lNode, lData, lParentNode, lParentData);

  end;

  lNode.Text := GetPropValue(lData, lMapping.DisplayPropName);

end;

//------------------------------------------------------------------------------

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
      psl.add(lList[i].Name);
  finally
    FreeMem(lList, lSize);
  end;
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function TtiTreeView.CanShowObjectProp(pValue: TObject): boolean;
var
  lMapping: TtiTVDataMapping;
  i: integer;
begin
  result := false;

  // A TPersistent
  if (pValue is TPersistent) then
  begin
    lMapping := FindMapping(TPersistent(pValue));
    result := ((lMapping <> nil) and
      (IsPublishedProp(TPersistent(pValue), lMapping.DisplayPropName)));
    Exit; //==>
  end;

  // A TList
  if (pValue is TList) then
  begin
    for i := 0 to TList(pValue).Count - 1 do
    begin
      if TObject(TList(pValue).Items[i]) is TPersistent then
      begin
        result := CanShowObjectProp(TObject(TList(pValue).Items[i]));
        if result then
          Exit; //==>
      end;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TtiTreeView.DoOnChange(sender: TObject; node: TTreeNode);
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
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

  GetDataPage(Node.Data, Node);

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
    if Assigned( FOnGetFont ) then
      FOnGetFont( Node, Node.Data, TV.Canvas.Font, TV.Canvas )
    else
      FTV.Canvas.Font.Color := clBlack;
  end
  else if (not Sender.Focused) and
    (Sender.Selected = Node) then
  begin
    FTV.Canvas.Brush.Color := _GetCanvasColor(clBtnFace{clSilver});
    if Assigned( FOnGetFont ) then
      FOnGetFont( Node, Node.Data, TV.Canvas.Font, TV.Canvas )
    else
      FTV.Canvas.Font.Color := clBlack;
  end
  else if (not Sender.Focused) and
    (Sender.Selected <> Node) then
  begin
    FTV.Canvas.Brush.Color := _GetCanvasColor(clWhite);
    if Assigned( FOnGetFont ) then
      FOnGetFont( Node, Node.Data, TV.Canvas.Font, TV.Canvas )
    else
      FTV.Canvas.Font.Color := clBlack;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTVDataMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTVDataMapping.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FsDisplayPropName := 'Caption';
  FsDataClassName := 'TPersistent';
  FName := ClassNameToCollectionItemName(FsDataClassName);
  FiImageIndex := -1;
  FbHasChildren := true;
  FbCanDelete := false;
  FbCanInsert := false;
  FbCanEdit := false;
  FbCanDragCopy := false;
  FbCanDragMove := false;
end;

// -----------------------------------------------------------------------------

destructor TtiTVDataMapping.Destroy;
begin
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TtiTVDataMapping.Assign(Source: TtiTVDataMapping);
begin
  DataClass := Source.DataClass;
  DisplayPropName := Source.DisplayPropName;
  ImageIndex := Source.ImageIndex;
  OnGetDataPage := Source.OnGetDataPage;
  HasChildren := Source.HasChildren;
  { TODO : What about asigning the events? }
end;

// -----------------------------------------------------------------------------

function TtiTVDataMapping.GetDisplayName: string;
var
  lsCreate: string;
  lsDelete: string;
  lsUpdate: string;
begin

  { TODO : Assigned( FOnCanInsert ) is not returning the correct result }
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
  //  Name      := ClassName + IntToStr( uiMappingCount ) ;
end;

// -----------------------------------------------------------------------------

function TtiTVDataMappings.Add: TtiTVDataMapping;
begin
  result := TtiTVDataMapping(inherited Add);
  //  SetItemName( result ) ;
end;

// -----------------------------------------------------------------------------

function TtiTVDataMappings.GetItem(Index: Integer): TtiTVDataMapping;
begin
  Result := TtiTVDataMapping(inherited GetItem(Index));
end;

// -----------------------------------------------------------------------------

function TtiTVDataMappings.GetOwner: TPersistent;
begin
  result := FTreeView;
end;

// -----------------------------------------------------------------------------

procedure TtiTVDataMappings.SetItem(Index: Integer; const Value: TtiTVDataMapping);
begin
  inherited SetItem(Index, Value);
end;

// -----------------------------------------------------------------------------
{
procedure TtiTVDataMappings.SetItemName(Item: TCollectionItem);
begin
  ( Item as TtiTVDataMapping ).Name := 'TVDataMapping' + IntToStr( FiItemNo ) ;
  Inc( FiItemNo ) ;
end;
}

// -----------------------------------------------------------------------------

procedure TtiTVDataMappings.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetTVDataMappings(const Value: TtiTVDataMappings);
begin
  FTVDataMappings.Assign(Value);
end;

// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetSelectedAddress(const Value: string);
var
  i: integer;
  liAddress: integer;
  lsAddress: string;
  lNode: TTreeNode;
  lbHasChildForms: boolean;
begin

  // If we are setting the data, then delay setting the selected address until the
  // setting of the data is finished.
  if FbSettingData then
  begin
    FsSelectedAddress := Value;
    Exit; //==>
  end;

  lsAddress := Value;
  lNode := nil;
  lbHasChildForms := FbHasChildForms;
  FbHasChildForms := false;
  try
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
          lNode := lNode.Item[liAddress]
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
  finally
    FbHasChildForms := lbHasChildForms;
    FsSelectedAddress := EmptyStr;
  end;
end;

function TtiTreeView.GetCanDeleteSelected: boolean;
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or FUpdating then
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

function TtiTreeView.GetCanInsertSelected: boolean;
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or FUpdating then
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

function TtiTreeView.GetCanEditSelected: boolean;
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
  lMapping: TtiTVDataMapping;
begin

  result := false;
  if (FData = nil) or FbSettingData or FReadOnly or FUpdating then
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

procedure TtiTreeView.SetImages(const Value: TCustomImageList);
begin
  TV.Images := Value;
end;

function TtiTreeView.GetOnChanging: TTVChangingEvent;
begin
  result := TV.OnChanging;
end;

procedure TtiTreeView.SetOnChanging(const Value: TTVChangingEvent);
begin
  TV.OnChanging := Value;
end;

// -----------------------------------------------------------------------------

function TtiTreeView.GetItems: TTreeNodes;
begin
  result := TV.Items;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetItems(const Value: TTreeNodes);
begin
  TV.Items := Value;
end;

// -----------------------------------------------------------------------------

function TtiTreeView.GetSelected: TTreeNode;
begin
  result := FTV.Selected;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetSelected(const Value: TTreeNode);
begin
  FTV.Selected := Value;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.FullExpand( pLevel : integer = -1 ) ;
begin

  if FData = nil then
    Exit; //==>

  if FbSettingData then
  begin
    FbFullExpand := true;
    FFullExpandLevel := pLevel ;
    Exit; //==>
  end;

  FbFullExpand := false;
  FTV.FullExpand;
  FTV.Selected := FTV.Items[0];

end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.FullCollapse;
begin
  FTV.FullCollapse;
  FTV.Selected := FTV.Items[0];
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  TV.Name := Name + '_TV';
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetSplitterPos(const Value: integer);
var
  lValue : integer ;
begin
//  if Value < FSplitter.MinSize then
//  /  lValue := FSplitter.MinSize
//  else
    lValue := Value ;
  TV.Width := lValue;
  FSplitter.Left := lValue;
end;

// -----------------------------------------------------------------------------

function TtiTreeView.GetSplitterPos: integer;
begin
  result := TV.Width;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetHasChildForms(const Value: boolean);
begin
  if FbHasChildForms = Value then
    Exit; //==>

  FbHasChildForms := Value;

  if FbHasChildForms then
  begin
    FSplitter.Width := cuiSplitterWidth;
    FSplitter.Visible := true;
    TV.Align := alLeft;
    if csDesigning in ComponentState then
      SplitterPos := ClientWidth div 2;
  end
  else
  begin
    FDataFormMappings.Clear;
    FFormInstances.Clear;
    FSplitter.Visible := false;
    FSplitter.Width := 0;
    TV.Align := alClient;
  end;

end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.RegisterChildForm(pDataClassRef: TtiTVDataClassRef;
  pFormClassRef: TtiTVFormClassRef;
  pbMultiInstance: boolean = false);
var
  lForm: TForm;
  lDataFormMapping: TtiTVDataFormMapping;
begin

  lDataFormMapping :=
    FDataFormMappings.FindByDataClass(pDataClassRef, true);

  lForm := FFormInstances.FindByFormClass(pFormClassRef, pbMultiInstance);
  lDataFormMapping.FormInstance := lForm;

  lForm.Visible := false;
  lForm.Parent := self;
  lForm.BorderStyle := bsNone;
  lForm.Align := alClient;

  HasChildForms := true;

end;

// -----------------------------------------------------------------------------

function TtiTreeView.IsCurrentChildFormValid: boolean;
begin
  result := true;
  if (FCurrentChildForm <> nil) and
    //     ( IsPublishedProp( FCurrentChildForm, 'Data' )      ) and
//     ( GetObjectProp( FCurrentChildForm, 'Data' ) <> nil ) and
  (IsPublishedProp(FCurrentChildForm, 'Valid')) then
    result := GetPropValue(FCurrentChildForm, 'Valid', false);
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.ClearCurrentChildForm;
begin
  if FCurrentChildForm = nil then
    Exit; //==>
  SetObjectProp(FCurrentChildForm, 'Data', nil);
  SetObjectProp(FCurrentChildForm, 'TreeNode', nil);
  FCurrentChildForm.Visible := false;
  FCurrentChildForm := nil;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.DoOnChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := (not HasChildForms) or IsCurrentChildFormValid;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.GetDataPage(pData: TObject; pNode: TTreeNode);
begin

  if not HasChildForms then
    Exit; //==>

  // If CurrentChildForm was assigned, then do some cleaning up.
  if FCurrentChildForm <> nil then
    ClearCurrentChildForm;

  // Find a childForm for editing the Data class attached to the selected node.
  FCurrentChildForm :=
    FDataFormMappings.FindByDataClass(pData.ClassType).FormInstance;

  // There was a form for this node, so setup the form
  if FCurrentChildForm <> nil then
  begin

    // To here...
    if Assigned(FOnAfterGetChildForm) then
      FOnAfterGetChildForm(FCurrentChildForm,
        pData,
        pNode);

    SetObjectProp(FCurrentChildForm, 'TreeNode', pNode);
    SetObjectProp(FCurrentChildForm, 'Data', pData);
    FCurrentChildForm.Visible := true;

  end;

end;

// -----------------------------------------------------------------------------

function TtiTVDataMapping.ClassNameToCollectionItemName(const pValue: string): string;
begin
  result := cusMethodPrefix + Copy(pValue, 2, Length(pValue) - 1);
end;

// -----------------------------------------------------------------------------

procedure TtiTVDataMapping.SetDataClassName(const Value: string);
begin
  if FName = ClassNameToCollectionItemName(FsDataClassName) then
    FName := ClassNameToCollectionItemName(Value);
  FsDataClassName := Value;
end;

// -----------------------------------------------------------------------------

function TtiTreeView.GetOnDblClick: TNotifyEvent;
begin
  result := FTV.OnDblClick;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetOnDblClick(const Value: TNotifyEvent);
begin
  FTV.OnDblClick := Value;
end;

// -----------------------------------------------------------------------------

function TtiTreeView.FindNodeByData(pData: TObject): TTreeNode;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].Data = pData then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure ExpandNodeByData(ptiTreeView: TtiTreeView;
    pList: TList);
  var
    i: integer;
    lNode: TTreeNode;
  begin
    for i := 0 to pList.Count - 1 do
    begin
      lNode := ptiTreeView.FindNodeByData(pList.Items[i]);
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
  lTargetData: TObject;
  lParentNode: TTreeNode;
  lTargetParentData: TObject;
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

  //tiShowMessage(['Parent node', FindNodeByData( lTargetData ).Text]);

  if FindNodeByData(lTargetData) <> nil then
    RefreshTreeNode(Self, FindNodeByData(lTargetData));
  //    FindNodeByData( lParentData ).Expand( false ) ;
  //  end ;

  if lSelectedData <> nil then
    SelectedData := lSelectedData;

end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
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

// -----------------------------------------------------------------------------

procedure TtiTreeView.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
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

// -----------------------------------------------------------------------------

procedure TtiTreeView.DoStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  lNode: TTreeNode;
  lData: TObject;
  lParentNode: TTreeNode;
  lParentData: TObject;
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
  lDragObject := TtiTVDragObject.Create;
  lDragObject.Data := lData;
  lDragObject.ParentData := lParentData;
  lDragObject.tiTreeView := Self;
  lDragObject.Node := lNode;
  lDragObject.ParentNode := lParentNode;

  DragObject := lDragObject;

end;

// -----------------------------------------------------------------------------

function TtiTreeView.GetSelectedData: TObject;
begin
  result := nil;
  if Selected = nil then
    Exit; //==>
  if Selected.Data = nil then
    Exit; //==>
  result := TObject(Selected.Data);
end;

// -----------------------------------------------------------------------------

procedure TtiTreeView.SetSelectedData(const Value: TObject);
var
  i: integer;
begin
  if Value = nil then
  begin
    if FTV.Items.Count > 0 then
      FTV.Selected := FTV.Items[0];
    Exit; //==>
  end;

  // ToDo: Add code so the FullExpand is not required
  // and if Node.Data can not be found to match Value, then scan for child nodes
  // that have not been read yet.
  FullExpand;
  for i := 0 to FTV.Items.Count - 1 do
    if TObject(FTV.Items[i].Data) = Value then
    begin
      FTV.Selected := FTV.Items[i];
      Break; //==>
    end;

end;

function TtiTreeView.GetTreeSortType: TSortType;
begin
  Result := TV.SortType;
end;

procedure TtiTreeView.SetTreeSortType(const Value: TSortType);
begin
  TV.SortType := Value;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVDataFormMappings
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function TtiTVDataFormMappings.FindByDataClass(pDataClass: TClass; pExactMatch: boolean = false): TtiTVDataFormMapping;
var
  i: integer;
begin
  result := nil;
  if pExactMatch then
  begin
    i := 0;
    while (i < Count) and (result = nil) do
    begin
      if pDataClass = TtiTVDataFormMapping(Items[i]).DataClass then
        result := TtiTVDataFormMapping(Items[i]);
      inc(i);
    end;
  end
  else
    for i := 0 to Count - 1 do
      if pDataClass.InheritsFrom(TtiTVDataFormMapping(Items[i]).DataClass) then
      begin
        if result = nil then
          result := TtiTVDataFormMapping(Items[i])
        else if TtiTVDataFormMapping(Items[i]).DataClass.InheritsFrom(result.DataClass) then
          result := TtiTVDataFormMapping(Items[i]);
      end;

  if result = nil then
  begin
    // A mapping for pDataClass was not found, so create one
    result := TtiTVDataFormMapping.Create;
    result.DataClass := pDataClass;
    result.FormInstance := nil;
    Add(result);
  end;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVFormInstances
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function TtiTVFormInstances.FindByFormClass(pFormClass: TFormClass;
  pbMultiInstance: boolean): TForm;
var
  i: integer;
  lsMessage: string;
  lForm: TForm;
begin
  result := nil;
  if not pbMultiInstance then
    for i := 0 to Count - 1 do
      if TForm(Items[i]).ClassType = pFormClass then
      begin
        result := TForm(Items[i]);
      end;

  if result = nil then
  begin
    // The form was not found, so create it.
    lForm := pFormClass.Create(nil);
    lsMessage := '';

    // Confirm there is a Data property on lForm
    if not IsPublishedProp(lForm, 'Data') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''Data'' property not published on form ' +
        lForm.ClassName;
    end;

    // Confirm there is a Valid property on lForm
    if not IsPublishedProp(lForm, 'Valid') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''Valid'' property not published on form ' +
        lForm.ClassName;
    end;

    // Confirm there is a TreeNode property on lForm
    if not IsPublishedProp(lForm, 'TreeNode') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''TreeNode'' property not published on form' +
        lForm.ClassName;
    end;

    // Set the data property to nil
    try
      SetObjectProp(lForm, 'Data', nil);
    except
      on e: exception do
      begin
        if lsMessage <> '' then
          lsMessage := lsMessage + #13;
        lsMessage := lsMessage +
          'Error setting ' +
          lForm.ClassName +
          '.Data := nil. Message: ' +
          e.Message;
      end;
    end;

    if lsMessage <> '' then
    begin
      lForm.Free;
      raise exception.Create(lsMessage + #13 + #13 +
        'Called in TtiTreeView.RegisterChildForm');
    end;

    Add(lForm);
    result := lForm;
  end;

end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVSplitter
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTVSplitter.Create(Owner: TComponent);
begin
  inherited;
  Width := cuiSplitterWidth;
  FbMouseOver := false;
  ResizeStyle := rsUpdate;
end;

procedure TtiTVSplitter.CMMouseEnter(var Message: TMessage);
begin
  FbMouseOver := true;
  Paint;
end;

procedure TtiTVSplitter.CMMouseLeave(var Message: TMessage);
begin
  FbMouseOver := false;
  Paint;
end;

procedure TtiTVSplitter.DrawGrabBar(pRect: TRect);
var
  lFillRect: TRect;
  lSaveColor: TColor;
begin
  lSaveColor := Canvas.Brush.Color;

  // Draw the outline of the rectangle
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(pRect);

  // If the mouse is over the splitter bar, then fill the grab bar part
  // with colour.
  if FbMouseOver then
  begin
    lFillRect.Top := pRect.Top + 1;
    lFillRect.Left := pRect.Left + 1;
    lFillRect.Bottom := pRect.Bottom - 2;
    lFillRect.Right := pRect.Right - 2;
    Canvas.Brush.Color := cuColorSplitterGrabBar;
    Canvas.FillRect(lFillRect);
  end;

  // Draw a shadow around the inside of the grab bar
  Canvas.Pen.Color := clWhite;
  Canvas.MoveTo(pRect.Left + 1, pRect.Top + 1);
  Canvas.LineTo(pRect.Right - 1, pRect.Top + 1);
  Canvas.MoveTo(pRect.Left + 1, pRect.Top + 1);
  Canvas.LineTo(pRect.Left + 1, pRect.Bottom - 1);

  // Draw some texture inside the grab bar
  Canvas.Pen.Style := psDot;
  Canvas.MoveTo(pRect.Left + 3, pRect.Top + 15);
  Canvas.LineTo(pRect.Left + 3, pRect.Bottom - 15);
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(pRect.Left + 4, pRect.Top + 16);
  Canvas.LineTo(pRect.Left + 4, pRect.Bottom - 16);

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clBlack;

  // Draw the top triangle
  Canvas.Polygon([Point(pRect.Left + 2, pRect.Top + 5),
    Point(pRect.Left + 2, pRect.Top + 10),
      Point(pRect.Left + 4, pRect.Top + 7)]);

  // Draw the bottom triangle
  Canvas.Polygon([Point(pRect.Left + 2, pRect.Bottom - 5),
    Point(pRect.Left + 2, pRect.Bottom - 10),
      Point(pRect.Left + 4, pRect.Bottom - 7)]);
  Canvas.Brush.Color := lSaveColor;

end;

procedure TtiTVSplitter.Paint;
var
  lRect: TRect;
begin
  inherited;
  lRect.Top := Height div 4;
  lRect.Bottom := Height div 4 * 3;
  lRect.Left := 1;
  lRect.Right := 7;
  DrawGrabBar(lRect);
end;

procedure TtiTreeView.SetApplyFilter(const Value: boolean);
begin
  FbApplyFilter := Value;
  { TODO : TtiTreeView.SetApplyFilter should re-read tree nodes }
end;

function TtiTreeView.GetOnKeyDown: TKeyEvent;
begin
  result := FTV.OnKeyDown;
end;

procedure TtiTreeView.SetOnKeyDown(const Value: TKeyEvent);
begin
  FTV.OnKeyDown := Value;
end;

procedure TtiTreeView.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if FReadOnly then
    FTV.Color := clBtnFace
    // FTV.Color := clSilver
  else
    FTV.Color := clWhite;
end;

procedure TtiTreeView.BeginUpdate;
begin
  FUpdating := True;
  FSaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FTV.Items.BeginUpdate;
end;

procedure TtiTreeView.EndUpdate;
begin
  FUpdating := False;
  FTV.Items.EndUpdate;
  Screen.Cursor := FSaveCursor;
end;

procedure TtiTreeView.SetEnableTree(const Value: boolean);
begin
  if FTV.Enabled <> Value then
    FTV.Enabled:=Value;
end;

function TtiTreeView.GetEnableTree: boolean;
begin
  result:=FTV.Enabled;
end;

procedure TtiTreeView.RefreshCurrentNode;
var
  lCurrentNode : TTreeNode ;
begin
  lCurrentNode := TV.Selected ;
  if lCurrentNode = nil then
    Exit ; //==>
  RefreshTreeNode(Self,lCurrentNode);
end;

procedure TtiTreeView.SetFocus;
begin
  if not Enabled then
    Exit ; //==> 
  inherited SetFocus ;
  TV.SetFocus ;
end;

end.

