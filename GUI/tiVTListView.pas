
unit tiVTListView;

interface

{$I tiDefines.inc}

{$IFDEF VIRTUAL_TREEVIEW}

uses
{.$IFDEF _PROFILE}
   Classes
  ,SysUtils
  ,Graphics
  ,Contnrs
  ,TypInfo
  ,Types
  ,ImgList
  ,Controls
  ,Menus
  ,tiImageMgr
  ,tiResources
  ,tiObject
  ,tiFocusPanel
  ,tiSpeedButton
  ,tiCtrlButtonPanel
  ,tiUtils
  ,tiGUIConstants
  ,tiVisitor
  ,tiOID
  ,Variants
  ,VirtualTrees
  ,tiVirtualTreesNEW
  ,StdCtrls     // TLabel, TCustomEdit
  ,ExtCtrls     // TCustomPanel
  ,Buttons      // TSpeedButton
  ,tiStreams
  ,tiMulticastEvent
{$IFNDEF FPC}
  ,tiINI
{$ELSE}
  ,tiINI
  ,LCLIntf
  ,LCLProc
  ,VirtualStringTree
{$ENDIF}
  ,tiVTAbstract
  ,tiVTSearch
  ;

{
  TVirtualTreeOptions = class(TCustomVirtualTreeOptions)
    property SelectionOptions; toMultiSelect
}


const
  crsDefaultColFieldName    = 'Caption';
  cDefaultAlternateRowColor = clPaleBlue; // Pale blue
  cDefaultAlternateRowCount = 2; // Show every second row in pale blue
  cInvalidValueColor = TColor($00c0ff); // Orange

Const
  FilterConjs : Array[0..2] Of String = ('',
    'And',
    'Or');

  FilterOps : Array[0..8] Of String = ('',
    'Equals',
    'Does Not Equal',
    'Is Greater Than',
    'Is Greater Than Or Equal To',
    'Is Less Than',
    'Is Less Than Or Equal To',
    'Contains',
    'Does Not Contain'
    );

  ctkString = [tkChar, tkString, tkWChar, tkLString, tkWString {$IFDEF UNICODE} , tkUString {$ENDIF}];
  ctkInt = [tkInteger, tkInt64];
  ctkFloat = [tkFloat];
  ctkSimple = ctkString + ctkInt + ctkFloat;

type
  TtiVTSortOrder = class;
  TtiVTSortOrders = class;
  TtiVTColumn = class;
  TtiVTColumns = class;
  TtiVTHeader = class;
  TtiInternalVirtualTree = class;
  TtiCustomVirtualTree = class;
  TtiVTListView = class;

  TvtTypeKind = (vttkString, vttkInt, vttkFloat, vttkDate, vttkDateTime, vttkTime, vttkCurrency);

  TFilterConj = (fcNone, fcAnd, fcOr);

  TFilterOp = (foNone, foEqual, foNotEqual,
    foGreater, foGreaterEqual,
    foLess, foLessEqual,
    foContains, foNotContains
    );

  TLVFilter = Class
  Private
    FJoin : TFilterConj;
    FFilterOperator : TFilterOp;
    FFieldName : String;
    FValue : String;
    FListView : TtiVTListView;
    Function GetSimplePropType(pPersistent : TtiObject; pPropName : String) : TvtTypeKind;
    Function WildCardMatch(pInputStr, pWilds : String; pIgnoreCase : Boolean) : Boolean;
    Function GetStringExpression : String;
    Function StringPropPasses(pValue : TtiObject) : Boolean;
    Function IntPropPasses(pValue : TtiObject) : Boolean;
    Function FloatPropPasses(pValue : TtiObject) : Boolean;
  Public
    Constructor Create;
    Function PassesFilter(pData : TtiObject) : Boolean;
    Property ListView : TtiVTListView Read FListView;
    Property Join : TFilterConj Read FJoin Write FJoin;
    Property PropName : String Read FFieldName Write FFieldName;
    Property FilterOperator : TFilterOp Read FFilterOperator Write FFilterOperator;
    Property Value : String Read FValue Write FValue;
    Property StringExpression : String Read GetStringExpression;
  End;


  TtiVTClearSortEvent     = procedure(pVT : TtiCustomVirtualTree) of object;
  TtiVTOnFilterDataEvent  = procedure(AData  : TtiObject; var pInclude : boolean) of object;

  //TtiVTEvent              = procedure(pVT : TtiCustomVirtualTree) of object;
  TtiVTItemEvent          = procedure(pVT : TtiCustomVirtualTree; AData : TtiObject; AItem : PVirtualNode) of object;
  TtiVTItemEditEvent      = procedure(pVT : TtiCustomVirtualTree; AData : TtiObject; AItem : PVirtualNode ) of object;
  TtiVTOnPaintText        = procedure(const pVT : TtiCustomVirtualTree;
                                       const pCanvas : TCanvas;
                                       const AData  : TtiObject;
                                             pColumn : Integer;
                                             pNode  : PVirtualNode
                                      ) of object;

  TtiVTOnBeforeCellPaintEvent = procedure(const pVT : TtiCustomVirtualTree;
                                       const pCanvas : TCanvas;
                                       const AData  : TtiObject;
                                             pColumn : Integer;
                                             pNode  : PVirtualNode;
                                             ACellRect : TRect
                                      ) of object;
  TtiVTOnAfterCellPaintEvent = procedure(const pVT : TtiCustomVirtualTree;
                                       const pCanvas : TCanvas;
                                       const AData  : TtiObject;
                                             pColumn : Integer;
                                             pNode  : PVirtualNode;
                                             ACellRect : TRect
                                      ) of object;


  TtiVTBeforeItemEraseEvent = procedure(const pVT : TtiCustomVirtualTree;
                                        const pCanvas : TCanvas;
                                        const AData  : TtiObject;
                                        pNode: PVirtualNode;
                                        pItemRect: TRect;
                                    var pItemColor: TColor;
                                    var pEraseAction: TItemEraseAction) of object;



  TtiVTAfterItemEraseEvent = procedure(const pVT : TtiCustomVirtualTree;
                                       const pCanvas : TCanvas;
                                       const AData  : TtiObject;
                                       pNode: PVirtualNode;
                                       pItemRect: TRect) of object;

  //TtiVTOnClick            = procedure(pVT : TtiCustomVirtualTree;
  //                                     AItem  : PVirtualNode  ;
  //                                     AData  : TtiObject;
  //                                     pColumn : TtiVTColumn) of object;
  TtiVTCanPerformAction = procedure(pVT: TtiCustomVirtualTree;
                                    AData: TtiObject;
                                    AItem: PVirtualNode;
                                    var pCanPerformAction: Boolean) of object;

  TtiDeriveListColumnValue = procedure(const pVT : TtiCustomVirtualTree;
                                        const AData : TtiObject;
                                        const ptiListColumn : TtiVTColumn;
                                        var   pResult : string) of object;
  TtiAdvancedDeriveListColumnValue = procedure(const AVT: TtiCustomVirtualTree;
                                                const AData : TtiObject;
                                                const ANode : PVirtualNode;
                                                const AtiListColumn : TtiVTColumn;
                                                var   AResult : string) of object;

  //TtiVTInfoTipEvent = procedure (const pVT: TtiCustomVirtualTree;
  //                               const AData: TtiObject;
  //                               const AItem: PVirtualNode;
  //                               var pInfoTip: string) of object;
  //
  //TtiVTInfoTypeType = (itNone, itDefault, itCustom);

  TtiVTOnNodeHint = procedure(const AVT: TtiCustomVirtualTree;
                              const AData: TtiObject;
                                    ANode: PVirtualNode;
                                    AColumn: TColumnIndex;
                              var   AHintText: UnicodeString) of object;

  TtiVTFocusChangeEvent = procedure(pSender: TtiCustomVirtualTree;
                                    AData: TtiObject;
                                    pNode: PVirtualNode;
                                    pColumn: TColumnIndex) of object;

  TtiVTHeaderClickEvent = procedure(const pSender: TtiCustomVirtualTree;
                                     const ptiListColumn : TtiVTColumn) of object;

  TtiVTOnIsValidColumnValue = procedure(AData: TtiObject; AColumn: TtiVTColumn;
                                        var AValue: UnicodeString;
                                        var AIsValid: Boolean) of object;

  TvtSortDirection = (vtsdAscending, vtsdDescending);

  TtiVTSortOrder = class(TCollectionItem)
  private
    FsFieldName: string;
    FSortDirection: TvtSortDirection;
  protected
    function GetDisplayName : string; override;
  published
    property FieldName : string read FsFieldName write FsFieldName;
    property  SortDirection : TvtSortDirection read FSortDirection write FSortDirection;
  public
    constructor Create(Owner : TCollection); override;
    procedure   Assign(source : TPersistent); override;
  end;


  TtiVTSortOrders = class(TCollection)
  private
    FOwner : TComponent;
    FGroupColumnCount: Integer;
    function  GetItem(Index : integer): TtiVTSortOrder;
    procedure SetItem(Index : integer; const AValue : TtiVTSortOrder);
  published
  protected
    function  GetOwner : TPersistent; override;
  public
    constructor Create(owner : TComponent);
    property    Items[Index: integer ]: TtiVTSortOrder read GetItem write SetItem; default;
    function    Add(AFieldName: string; ADirection: TvtSortDirection = vtsdAscending): TtiVTSortOrder; overload;
    function    Add : TtiVTSortOrder; overload;
    function    Remove(const ASortOrder: TtiVTSortOrder): Integer;
    function    IndexOf(const ASortOrder: TtiVTSortOrder): Integer;
    function    ItemByFieldName(const AFieldName: string;
      out ASortOrder: TtiVTSortOrder): boolean;

  published
    property GroupColumnCount: Integer read FGroupColumnCount write FGroupColumnCount;
  end;


  TtiVTColumn = class(TtiVirtualTreeColumn)
  private
    FsFieldName: string;
    FsDisplayMask: string;
    FDataType: TvtTypeKind;
    FAllowInlineEdit: boolean;
    FDerived: boolean;
    FOnDeriveColumn: TtiDeriveListColumnValue;
    FOnAdvancedDeriveColumn: TtiAdvancedDeriveListColumnValue;
    FOnIsValidValue: TtiVTOnIsValidColumnValue;
    FStoredWidth: Integer;
    procedure   SetFieldName(const AValue: string);
    procedure   SetDataType(const AValue: TvtTypeKind);
    procedure   SetDerived(const AValue: boolean);
    function    GetHasDerivedHandler: boolean;
  public
    constructor Create(Collection : TCollection); override;
    destructor  Destroy; override;
    function    Clone : TtiVTColumn;
    function    IsValidValue(AData: TtiObject; var AValue: UnicodeString): Boolean;
    Procedure StoreWidth;
    Property StoredWidth : Integer Read FStoredWidth Write FStoredWidth;
  published
    property    FieldName   : string read FsFieldName    write SetFieldName;
    property    DisplayMask : string read FsDisplayMask  write FsDisplayMask;
    property    DataType    : TvtTypeKind read FDataType write SetDataType;
    property    AllowInlineEdit : boolean read FAllowInlineEdit write FAllowInlineEdit ;
    property    OnIsValidValue: TtiVTOnIsValidColumnValue read FOnIsValidValue write FOnIsValidValue;
    property    Derived     : boolean read FDerived      write SetDerived;
    property    HasDerivedHandler: boolean read GetHasDerivedHandler;
    property    OnDeriveColumn : TtiDeriveListColumnValue read FOnDeriveColumn write FOnDeriveColumn;
    property    OnAdvancedDeriveColumn : TtiAdvancedDeriveListColumnValue read FOnAdvancedDeriveColumn write FOnAdvancedDeriveColumn;
  end;

  TtiVTColumns = class(TVirtualTreeColumns)
  private
    //FOwner : TComponent;
    function  GetItem(Index : TColumnIndex): TtiVTColumn;
    procedure SetItem(Index : TColumnIndex; const AValue : TtiVTColumn);
  protected
  public
// VT 5.4 now has Create() virtual, so need override and with TVTHeader class
// Since this method is empty, just comment out until required
//    constructor Create(AOwner : TtiVTHeader); override;
    destructor  Destroy; override;
    Procedure StoreWidths;
    Procedure RevertToStoredWidths;
    property  Items[Index: TColumnIndex ]: TtiVTColumn read GetItem write SetItem; default;
    procedure DisplayLabelsToStringList(pSL : TStringList);
    function  FindByDisplayLabel(const AValue : string): TtiVTColumn;
    function  FindByFieldName(const AValue : String) : TtiVTColumn;
  end;

  TtiVTHeader = class(TtiVSTHeader)
  private
    function GetColumns: TtiVTColumns;
    procedure SetColumns(const AValue: TtiVTColumns);
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
  public
    constructor Create(AOwner: TBaseVirtualTree); override;
  published
    property Columns: TtiVTColumns read GetColumns write SetColumns stored False; // VT stores manually
  end;

  TtiInternalVirtualTree = class(TtiVirtualStringTree)
  protected
    FtiOwner: TtiCustomVirtualTree;

    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
      var EraseAction: TItemEraseAction); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;

    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetHeaderClass: TVTHeaderClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TtiVTExportFile = class;

  TtiVTExportFileNameEvent = procedure (const AVTExportFile: TtiVTExportFile;
    var AExportFileName: string; var AAskUser: boolean) of object;
  TtiVTExportedToFileEvent = procedure (const AVTExportFile: TtiVTExportFile;
    var AAskUserToView: boolean) of object;

  TtiVTExport = class;

  TtiVTExportedEvent = procedure (const AVTExport: TtiVTExport) of object;

  // Multicast event for filtering
  TtiVTFilterDataEventArgs = class(TtiMulticastEventArgs)
  private
    FData: TtiObject;
    FInclude: Boolean;
  public
    constructor Create(const AData: TtiObject);
    property Data: TtiObject read FData;
    property Include: Boolean read FInclude write FInclude;
  end;

  TtiVTOnFilterDataMulticastEventDispatcher = class(TtiMulticastEventDispatcherAbs)
  protected
    function GetEventArgsClass: TtiMulticastEventArgsClass; override;
    procedure InvokeHandler(AMethod: TMethod; ASender: TObject; AArgs: TtiMulticastEventArgs); override;
    function GetDefaultEventHandler: TtiVTOnFilterDataEvent;
    procedure SetDefaultEventHandler(const AValue: TtiVTOnFilterDataEvent);
  public
    // Register and unregister methods to support the pre-existing event
    // handler signature.
    procedure RegisterEventHandler(
        const AHandler: TtiVTOnFilterDataEvent;
        const AIsDefaultHandler: Boolean = False);
    procedure UnregisterEventHandler(const AHandler: TtiVTOnFilterDataEvent);
    property DefaultEventHandler: TtiVTOnFilterDataEvent read GetDefaultEventHandler write SetDefaultEventHandler;
  end;

  TtiStringEditLink = class(TStringEditLink)
  private
    FtiTree: TtiCustomVirtualTree;
  public
    constructor CreateWith(AtiTree: TtiCustomVirtualTree);
    property tiTree: TtiCustomVirtualTree read FtiTree;
  end;

  TtiVTEdit = class(TVTEdit)
  protected
    procedure DoOnChange(Sender: TObject);
    function IsValidValue: Boolean; // override;
    function GetLink: TtiStringEditLink;
    property Link: TtiStringEditLink read GetLink;
  public
    constructor Create(Link: TStringEditLink);
  end;

  TtiCustomVirtualTree = class(TtiVTAbstract)
  private
    FData: TtiObjectList;
    FFilteredData: TObjectList;
    FGroupedData: TList; // List of indexes into FFilteredData
    FGroupingApplied: Boolean;
    FFilterEventDispatcher: TtiVTOnFilterDataMulticastEventDispatcher;

    FShowAlternateRowColor: Boolean;
    FSortOrders: TtiVTSortOrders;
    FAlternateRowColor: TColor;

    FFilters : TList;
    FFiltered : boolean;

    FSorted: Boolean;
    FReadOnly: Boolean;

    FOnGetImageIndex: TtiVTGetImageIndexEvent;
    FOnCanEdit: TtiVTCanPerformAction;
    FOnCanInsert: TtiVTCanPerformAction;
    FOnCanView: TtiVTCanPerformAction;
    FOnCanDelete: TtiVTCanPerformAction;
    FOnFocusChanged: TtiVTFocusChangeEvent;
    FOnHeaderClick: TtiVTHeaderClickEvent;
    FOnItemArrive: TtiVTItemEvent;
    FOnItemLeave: TtiVTItemEvent;
    FOnFilterData : TtiVTOnFilterDataEvent;
    FOnPaintText : TtiVTOnPaintText;
    FOnBeforeCellPaint: TtiVTOnBeforeCellPaintEvent;
    FOnAfterCellPaint: TtiVTOnAfterCellPaintEvent;
    FOnBeforeItemErase: TtiVTBeforeItemEraseEvent;
    FOnAfterItemErase: TtiVTAfterItemEraseEvent;
    FOnDblClick: TtiVTItemEvent;
    FOnEdited: TtiVTItemEvent;
    FAlternateRowCount: Byte;
    FDisabledColor: TColor;
    FRowSelect: boolean;
    FShowNodeHint: boolean;
    FOnGetNodeHint: TtiVTOnNodeHint;
    FHeaderClickSorting: boolean;
    FMultiSelect: boolean;
    FDisplayParentNodeData: boolean;

    procedure DrawSortGlyph(const ACanvas: TCanvas;
      const ASortDirection: TSortDirection; const APosition: TPoint);
    function  GetHeader: TtiVTHeader;
    procedure SetData(const AValue: TtiObjectList);
    procedure SetHeader(const AValue: TtiVTHeader);
    procedure SetAlternateRowColor(const AValue: TColor);
    procedure SetShowAlternateRowColor(const AValue: Boolean);
    function  GetImages: TCustomImageList;
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetSortOrders(const AValue: TtiVTSortOrders);
    function  GetOnKeyDown: TKeyEvent;
    function  GetOnKeyPress: TKeyPressEvent;
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetOnKeyPress(const AValue: TKeyPressEvent);
    procedure SetReadOnly(const AValue: Boolean);
    function  GetSelectedData: TtiObject;
    procedure SetSelectedData(const AValue: TtiObject);
    procedure SetOnPaintText(const AValue: TtiVTOnPaintText);
//    procedure SetOnBeforeCellPaint(const AValue: TtiVTOnBeforeCellPaintEvent);
    procedure SetOnAfterCellPaint(const AValue: TtiVTOnAfterCellPaintEvent);
    procedure SetRootNodeCount;
    function  GetSelectedIndex: integer;
    procedure SetSelectedIndex(const AValue: integer);
    procedure DoDblClick(Sender: TObject); virtual;
    procedure FocusNode(ANode: PVirtualNode);
    procedure SetRowSelect(const AValue: boolean);
    procedure SetShowNodeHint(const AValue: boolean);
    procedure SetOnGetNodeHint(const AValue: TtiVTOnNodeHint);
    procedure VTHeaderAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VTHeaderClick(Sender: TVTHeader; AHitInfo: TVTHeaderHitInfo);
    procedure VTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);

    function ItemPassesFilter(pObject : TtiObject) : Boolean;

    function GetFilterAsString: String;
    procedure VTCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var Allowed: Boolean);
    procedure VTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex);
    procedure VTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: UnicodeString);
    procedure SetMultiSelect(const AValue: boolean);
    function GetSelectedCount: integer;
    function GetNearest: TtiObject;
    function DoSortElement(AOrder: TtiVTSortOrder; AData1, AData2: TtiObject): Integer;
    function SortProc(AData1, AData2: Pointer): Integer;
    procedure SetOnAfterItemErase(const AValue: TtiVTAfterItemEraseEvent);
    procedure SetOnBeforeItemErase(const AValue: TtiVTBeforeItemEraseEvent);

    //
    //FOnDblClick  : TtiLVItemEditEvent;
    //FbApplyFilter: boolean;
    //FbApplySort: boolean;
    //FSortOrders: TtiVTSortOrders;
    //FOnGetFont: TtiLVOnGetFont;
    //FOnLVClick: TtiLVOnClick;
    //FReadOnly : boolean;
    //FOnTipInfo: TtiLVInfoTipEvent;
    //FInfoTipType: TtiLVInfoTypeType;
  protected
    procedure   SetChildControlNames; override;
    procedure VTSearchInsideNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      const SearchText: string; out Result: boolean); override;
    procedure   ApplyGrouping; virtual;
    procedure   ClearGrouping; virtual;
    procedure   DoEnter; override;

    property    OnFocusChanged : TtiVTFocusChangeEvent read FOnFocusChanged write FOnFocusChanged;
    property    OnKeyDown      : TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property    OnKeyPress     : TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    //property    OnClick        : TtiLVOnClick read FOnLVClick write FOnLVClick;
    property    OnDblClick     : TtiVTItemEvent read FOnDblClick write FOnDblClick;
    property    OnFilterData   : TtiVTOnFilterDataEvent read FOnFilterData write FOnFilterData;
    //property    OnGetFont      : TtiLVOnGetFont read  FOnGetFont write FOnGetFont;
    //property    OnGetImageIndex : TtiLVGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property    OnHeaderClick : TtiVTHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;

    property    OnItemArrive    : TtiVTItemEvent read FOnItemArrive write FOnItemArrive;
    property    OnItemLeave     : TtiVTItemEvent read FOnItemLeave write FOnItemLeave;
    // ToDo: Replace with OnPaintText in TtiVTAbstract
    property    OnPaintText     : TtiVTOnPaintText read FOnPaintText Write SetOnPaintText;
//    property    OnBeforeCellPaint: TtiVTOnBeforeCellPaintEvent read FOnBeforeCellPaint Write SetOnBeforeCellPaint;
    property    OnAfterCellPaint: TtiVTOnAfterCellPaintEvent read FOnAfterCellPaint Write SetOnAfterCellPaint;
    property    OnBeforeItemErase: TtiVTBeforeItemEraseEvent read FOnBeforeItemErase write SetOnBeforeItemErase;
    property    OnAfterItemErase: TtiVTAfterItemEraseEvent   read FOnAfterItemErase  write SetOnAfterItemErase;
    property    OnGetNodeHint   : TtiVTOnNodeHint read FOnGetNodeHint write SetOnGetNodeHint;
    property    OnEdited        : TtiVTItemEvent read FOnEdited write FOnEdited;
    //property    OnInfoTip      : TtiLVInfoTipEvent read FOnTipInfo   write SetOnInfoTip;
    //
    //
    //property    Align        ;
    //property    Anchors      ;
    //property    Constraints  ;
    //property    Visible      ;
    //Property    ShowFocusRect;
    //property    SmallImages    : TCustomImageList read GetSmallImages write SetSmallImages;
    property    ReadOnly       : Boolean read FReadOnly write SetReadOnly default false;
    //property    InfoTypeType   : TtiLVInfoTypeType read FInfoTipType Write SetInfoTipType;

    function  CalcRootNodeChildCount(RootNode: PVirtualNode): Integer;

    procedure Loaded; override;
    procedure ReadData;
    procedure DisconnectFromData;
    procedure ConnectToData;
    procedure SetHeaderClickSorting(const Value: boolean); virtual;
    procedure SetEnabled(AValue: Boolean); override;
    function  GetFilteredItemCount: Integer;
    function  GetFilteredItems(AIndex: Integer): TtiObject;
    function  GetChildNodeFromObject(const AParentNode: PVirtualNode; const AData: TtiObject): PVirtualNode;
    function  CanEditRow: Boolean;

    procedure VTDoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction); virtual;
    procedure VTDoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString); virtual;
    procedure VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual;
    procedure VTGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); virtual;
    procedure VTInitChildren(Node: PVirtualNode;  var ChildCount: Cardinal); virtual;
    procedure DoOnPaintText(pSender: TBaseVirtualTree; const pTargetCanvas: TCanvas; pNode: PVirtualNode;  pColumn: TColumnIndex; pTextType: TVSTTextType);
//    procedure DoOnBeforeCellPaint(ASender: TBaseVirtualTree; ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex; ACellPaintMode: TVTCellPaintMode; ACellRect: TRect; var AContentRect: TRect);
    procedure DoOnAfterCellPaint(ASender: TBaseVirtualTree; ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex; ACellRect: TRect);
    procedure DoOnBeforeItemErase (ASender: TBaseVirtualTree; ATargetCanvas: TCanvas; ANode: PVirtualNode; AItemRect: TRect; var AItemColor: TColor; var AEraseAction: TItemEraseAction);
    procedure DoOnAfterItemErase (ASender: TBaseVirtualTree; ATargetCanvas: TCanvas; ANode: PVirtualNode; AItemRect: TRect);
    procedure DoOnGetHint(Sender: TBaseVirtualTree; ANode: PVirtualNode; Column: TColumnIndex; var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: UnicodeString);

    property AlternateRowColor: TColor read FAlternateRowColor write SetAlternateRowColor default cDefaultAlternateRowColor;
    property AlternateRowCount: Byte Read FAlternateRowCount Write FAlternateRowCount default cDefaultAlternateRowCount;
    property DisabledColor: TColor Read FDisabledColor Write FDisabledColor default clBtnFace;
    property HeaderClickSorting: boolean read FHeaderClickSorting write SetHeaderClickSorting default True;
    property Images: TCustomImageList read GetImages write SetImages;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default False;
    property RowSelect: boolean read FRowSelect write SetRowSelect default True;
    property ShowAlternateRowColor: Boolean read FShowAlternateRowColor write SetShowAlternateRowColor default True;
    property ShowNodeHint: boolean read FShowNodeHint write SetShowNodeHint;
    property SortOrders: TtiVTSortOrders read FSortOrders write SetSortOrders;
    property DisplayParentNodeData: boolean read FDisplayParentNodeData write FDisplayParentNodeData;

    property OnGetImageIndex: TtiVTGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnCanView: TtiVTCanPerformAction read FOnCanView write FOnCanView;
    property OnCanDelete: TtiVTCanPerformAction read FOnCanDelete write FOnCanDelete;
    property OnCanInsert: TtiVTCanPerformAction read FOnCanInsert write FOnCanInsert;
    property OnCanEdit: TtiVTCanPerformAction read FOnCanEdit write FOnCanEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   ApplySort(const AApplyGrouping: boolean = true); virtual;
    procedure   ApplyFilter; virtual;

    function    CanView: Boolean; virtual;
    function    CanInsert: Boolean; virtual;
    function    CanEdit: Boolean; virtual;
    function    CanDelete: Boolean; virtual;
    procedure   ClearSort; virtual;

    // Property value filtering
    procedure   ClearFilters; virtual;
    procedure   AddFilter(pFilter : TLVFilter);
    procedure   InsertFilter(pPos : Integer; pFilter : TLVFilter);
    procedure   DeleteFilter(pPos : Integer);
    // Multicast event handler filtering
    property    FilterEventDispatcher: TtiVTOnFilterDataMulticastEventDispatcher read FFilterEventDispatcher;

    procedure   DeleteNode(Node: PVirtualNode); virtual;

    function    IsNodeDataItem(Node: PVirtualNode): Boolean;

    function    GetObjectFromNode(pNode: PVirtualNode): TtiObject; override;
    function    GetNodeFromObject(const AData: TtiObject): PVirtualNode;
    function    GetTextFromObject(AObj: TtiObject; AColumnIndex: TColumnIndex; ANode: PVirtualNode = nil): string;
    procedure   SetTextInObject(AObj: TtiObject; AColumnIndex: TColumnIndex; AText: string);

    procedure   Refresh(const ASelectedData: TtiObject = nil); reintroduce; overload; virtual;
    procedure   Refresh(const ASelectedOID: string); reintroduce; overload; virtual;
    procedure   Refresh(const ASelectedList: TtiObjectList; AFocused: TtiObject); reintroduce; overload; virtual;

    //procedure   PositionCursor(AIndex : integer    ); overload;
    //procedure   PositionCursor(AData  : TtiObject); overload;

    //procedure   Last;
    //procedure   First;
    //property    ApplyFilter : boolean read FbApplyFilter write SetApplyFilter;
    property    FilterAsString : String read GetFilterAsString;

    function    Focused: boolean; override;

    property    SelectedData : TtiObject read GetSelectedData write SetSelectedData;
    property    Nearest: TtiObject read GetNearest; // For repositioning Selected after a delete
    property    SelectedIndex : integer read GetSelectedIndex write SetSelectedIndex;
    procedure   AssignSelectedDataList(const AList: TtiObjectList);
    property    SelectedCount: integer read GetSelectedCount;

    procedure   SetFocus; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    function    AddColumn(const AFieldName : string;
                           const pDataType : TvtTypeKind;
                           const pDisplayLabel : string = '';
                           pColWidth : Integer = -1;
                           const pAllowInlineEdit : boolean = False;
                           const pOnIsValidValue: TtiVTOnIsValidColumnValue = nil): TtiVTColumn; overload;
    function    AddColumn(const pDeriveColumnMethod : TtiDeriveListColumnValue;
                           const pDisplayLabel : string = '';
                           pColWidth : Integer = -1): TtiVTColumn; overload;
    function    AddColumn(const AAdvancedDeriveColumnMethod: TtiAdvancedDeriveListColumnValue;
                           const ADisplayLabel: string = '';
                           AColWidth: Integer = -1): TtiVTColumn; overload;
    function    AddColumn(const AFieldName : string;
                           const ADataType : TvtTypeKind;
                           const ADeriveColumnMethod: TtiDeriveListColumnValue;
                           const ADisplayLabel : string = '';
                           const AColWidth : Integer = -1): TtiVTColumn; overload;
    function    AddColumn(const AFieldName : string;
                           const ADataType : TvtTypeKind;
                           const AAdvancedDeriveColumnMethod: TtiAdvancedDeriveListColumnValue;
                           const ADisplayLabel : string = '';
                           const AColWidth : Integer = -1): TtiVTColumn; overload;

    //procedure   ClearColumns; virtual;
    //property    SortOrders     : TtiVTSortOrders read FSortOrders;
    //property    GroupCols      : TtiLVGroupCols read FGroupCols;
    property    Data : TtiObjectList read FData write SetData;
    property    Header: TtiVTHeader read GetHeader write SetHeader;
    property    Sorted: Boolean read FSorted;
    function    SelectedNodeScreenOrigin: TPoint;
    property    FilteredItemCount: Integer read GetFilteredItemCount;
    property    FilteredItems[AIndex: Integer]: TtiObject read GetFilteredItems;
    function    IsInFilteredData(AObject: TObject): boolean;
    procedure   RefreshObject(AObject: TtiObject);
    function    CanInlineEditFocusedCell: boolean;
    procedure   ScrollSelectedIntoView;
  published
  end;


  TtiCustomVirtualEditTree = class(TtiCustomVirtualTree)
    // Adds View,Edit,New,Delete buttons, context menu items and events
  private
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs;
    FButtonStyle: TLVButtonStyle;
    FVisibleButtons: TtiLVVisibleButtons;
    FAllowedButtons: TtiLVVisibleButtons;
    FEditInlineOnly: boolean;
    FPopupMenu: TPopupMenu;
    FpmiView  : TMenuItem;
    FpmiEdit  : TMenuItem;
    FpmiNew   : TMenuItem;
    FpmiDelete : TMenuItem;
    FpmiFilter : TMenuItem;
    FpmiSortGroup: TMenuItem;
    FpmiClearSort: TMenuItem;
    FpmiShowFind: TMenuItem;
    FStdPopupItemCount: Integer;

    FOnItemDelete: TtiVTItemEditEvent;
    FOnItemEdit: TtiVTItemEditEvent;
    FOnItemInsert: TtiVTItemEditEvent;
    FOnItemView: TtiVTItemEditEvent;
    FOnClearSort: TtiVTClearSortEvent;
    FFiltering : Boolean;
    FExporting: boolean;
    FSearching: boolean;
    FOnExportFileName: TtiVTExportFileNameEvent;
    FOnExported: TtiVTExportedEvent;
    FOnExportedToFile: TtiVTExportedToFileEvent;

    procedure SetButtonStyle(const AValue: TLVButtonStyle);
    procedure SetVisibleButtons(const AValue: TtiLVVisibleButtons);
    procedure SetOnClearSort(const AValue: TtiVTClearSortEvent);
    procedure SetOnItemView(const AValue: TtiVTItemEditEvent);
    procedure SetOnItemDelete(const AValue: TtiVTItemEditEvent);
    procedure SetOnItemEdit(const AValue: TtiVTItemEditEvent);
    procedure SetOnItemInsert(const AValue: TtiVTItemEditEvent);
    procedure AddGroupingPopupItems;
    procedure DoCollapseAll(Sender: TObject);
    procedure DoExpandAll(Sender: TObject);
    procedure DoRefreshButtons;
    procedure SetExporting(const AValue: boolean);
    procedure SetSearching(const AValue: boolean);
    procedure SetFiltering(const AValue: boolean);
    procedure SetOnExportFileName(const AValue: TtiVTExportFileNameEvent);
    procedure SetOnExported(const AValue: TtiVTExportedEvent);
    procedure SetOnExportedToFile(const AValue: TtiVTExportedToFileEvent);
    function  GetButtonPanel: TtiCtrlBtnPnlAbs;
  protected
    procedure CreateButtonPanel;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetHeaderClickSorting(const AValue: boolean); override;
    procedure SetChildControlNames; override;

    procedure DoMenuPopup(Sender: TObject); virtual;
    procedure DoClearSort(Sender: TObject); virtual;
    procedure DoViewOrEdit(Sender: TObject); virtual;
    procedure DoView(Sender: TObject); virtual;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoDelete(Sender: TObject); virtual;
    procedure DoExportTree(Sender: TObject); virtual;
    procedure DoShowFind(Sender: TObject); virtual;
    procedure DoFilter(Sender : TObject); virtual;
    procedure DoNew(Sender: TObject); virtual;
    procedure DoDblClick(Sender: TObject); override;

    procedure   Loaded; override;
    procedure   DoEnter; override;
    procedure   DoExit ; override;

    procedure VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); override;

    property ButtonStyle: TLVButtonStyle read FButtonStyle write SetButtonStyle default lvbsButtonsAndLabels;
    property Exporting: boolean read FExporting write SetExporting default true;
    property Searching: boolean read FSearching write SetSearching default true;
    property Filtering: boolean read FFiltering write SetFiltering default true;
    property VisibleButtons : TtiLVVisibleButtons read FVisibleButtons write SetVisibleButtons default [];
    property AllowedButtons : TtiLVVisibleButtons read FAllowedButtons;
    property EditInlineOnly: boolean read FEditInlineOnly write FEditInlineOnly;

    property OnClearSort: TtiVTClearSortEvent read FOnClearSort write SetOnClearSort;
    property OnItemDelete: TtiVTItemEditEvent read FOnItemDelete write SetOnItemDelete;
    property OnItemEdit: TtiVTItemEditEvent read FOnItemEdit write SetOnItemEdit;
    property OnItemInsert: TtiVTItemEditEvent read FOnItemInsert write SetOnItemInsert;
    property OnItemView: TtiVTItemEditEvent read FOnItemView write SetOnItemView;

    procedure EnablePopupMenu;
    procedure DisablePopupMenu;
    procedure SPEnterFindEdit(Sender: TObject); override;
    procedure SPExitFindEdit(Sender: TObject); override;
    procedure SPEnterKey(Sender: TObject); override;
    procedure SPShowing(const ASender: TtiVTSearchPanel; const AIsShowing: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    function CanDelete: Boolean; override;
    function CanEdit: Boolean; override;
    function CanInsert: Boolean; override;
    function CanView: Boolean; override;

    property OnExportFileName: TtiVTExportFileNameEvent read FOnExportFileName write SetOnExportFileName;
    property OnExportedToFile: TtiVTExportedToFileEvent read FOnExportedToFile write SetOnExportedToFile;
    property OnExported: TtiVTExportedEvent read FOnExported write SetOnExported;

    procedure Refresh(const ASelectedData: TtiObject = nil); override;
    procedure Refresh(const ASelectedOID: string); override;
    procedure SetAllowedButtons(const AAllowedButtons: TtiLVVisibleButtons);

    property ButtonPanel: TtiCtrlBtnPnlAbs read GetButtonPanel;
  end;

  TtiVTListView = class(TtiCustomVirtualEditTree)
  public
  published
    property Align;
    property Anchors;
    property AlternateRowColor;
    property AlternateRowCount;
    property DisabledColor;
    property ButtonStyle;
    property Color;
    property Constraints;
    property DisplayParentNodeData;
    property Exporting;
    property Filtering;
    property Header;
    property HeaderClickSorting;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property EditInlineOnly;
    property MultiSelect;
    property RowSelect;
    property Searching;
    property ShowAlternateRowColor;
    property ShowNodeHint;
    property Sorted;
    property SortOrders;
    property VisibleButtons;
    property VT;
    property Visible;

    property OnCanDelete;
    property OnCanEdit;
    property OnCanInsert;
    property OnCanView;
    property OnClearSort;
    property OnDblClick;
    property OnEdited;
    property OnFilterData;
    property OnFocusChanged;
    property OnGetImageIndex;
    property OnExportFileName;
    property OnExportedToFile;
    property OnExported;
    property OnHeaderClick;
    property OnItemArrive;
    property OnItemDelete;
    property OnItemEdit;
    property OnItemInsert;
    property OnItemLeave;
    property OnItemView;
    property OnKeyDown;
    property OnKeyPress;
    property OnPaintText;
//    property OnBeforeCellPaint;
    property OnAfterCellPaint;
    property OnGetNodeHint;
    property OnBeforeItemErase;
    property OnAfterItemErase;
    // ...
  end;

  TtiVTExport = class(TtiObject)
  protected
    FOutputStr: string;
    FSourceTree: TtiCustomVirtualEditTree;
    FStream: TtiPreSizedStream;
    procedure WriteDetailEx(const AFieldSeparator, AFieldDelimiter: char);
    procedure WriteHeaderEx(const AFieldSeparator, AFieldDelimiter: char);
    procedure WriteToOutput(const AText: string); virtual;
    function CreateOutput: boolean; virtual;
    procedure WriteHeader; virtual;
    procedure WriteTitles; virtual;
    procedure WriteDetail; virtual;
    procedure WriteFooter; virtual;
    procedure CloseOutput; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function FormatName: string; virtual; abstract;
    class function ImageName: string; virtual; abstract;
    procedure Execute(const ATree: TtiCustomVirtualEditTree);
  end;

  TtiVTExportFile = class(TtiVTExport)
  protected
    FFileName: string;
    FDefaultExt: string;
    function GetFileName: Boolean; virtual;
    function CreateOutput: boolean; override;
    procedure CloseOutput; override;
  public
    property FileName: string read FFileName;
    property DefaultExt: string read FDefaultExt;
  end;

  TTiVTExportClipboard = class(TtiVTExport)
  protected
    procedure CloseOutput; override;
    procedure WriteDetail; override;
    procedure WriteHeader; override;
  public
    class function FormatName: string; override;
    class function ImageName: string; override;
  end;

  TtiVTExportClass = class of TtiVTExport;

  TtiVTExportFileNameStore = class(TtiObject)
  public
    function GetFileName(const AVT: TComponent; const AExportFormatName: string): string; virtual; abstract;
    procedure SetFileName(const AVT: TComponent; const AExportFormatName: string; const AFileName: string); virtual; abstract;
  end;

  TtiVTIniExportFileNameStore = class (TtiVTExportFileNameStore)
  private
    FIniFile: TtiINIFile;
    procedure GetID(const AVT: TComponent; const AExportFormatName: string;
      out ASection, AIdent: string);
  public
    constructor CreateExt(const AIniFile: TtiINIFile);
    function GetFileName(const AVT: TComponent;
      const AExportFormatName: string): string; override;
    procedure SetFileName(const AVT: TComponent;
      const AExportFormatName: string; const AFileName: string); override;
  end;

  TtiVTExportRegistry = class(TTiObject)
  private
    FExportMappings: TStrings;
    FExportFileNameStore: TtiVTExportFileNameStore;
    function GetExportCount: integer;
    procedure SetExportFileNameStore(const AValue: TtiVTExportFileNameStore);
    function GetExportFileNameStore: TtiVTExportFileNameStore;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterExport(const AVTExportClass: TtiVTExportClass);
    function GetExport(const AFormatName: string): TtiVTExportClass; overload;
    function GetExport(const AIndex: integer): TtiVTExportClass; overload;
    property ExportCount: integer read GetExportCount;
    property ExportFileNameStore: TtiVTExportFileNameStore
      read GetExportFileNameStore write SetExportFileNameStore;
  end;

function tiVTExportRegistry: TtiVTExportRegistry;

function tiVTDisplayMaskFromDataType(const AValue : TvtTypeKind): string;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

uses
   Math
  ,Dialogs
  ,tiConstants
  ,tiExcept
  ,tiGUIUtils
  ,tiDialogs // Debuggin
  ,Windows  // VK_XXX
  ,Forms // TBorderStyle
  ,tiVTExportCSV
  ,tiVTExportHTML
  ,Clipbrd           // global Clipboard object
  ,tiVTFilter
  ,tiRTTI
  ;

type
  TMyRecord = packed record
    Ref: TtiObject;
  end;
  PMyRecord = ^TMyRecord;

  TtiVTExportMenuItem = class(TMenuItem)
  private
    FExportName: string;
    procedure SetExportName(const AValue: string);
  public
    property ExportName: string read FExportName write SetExportName;
  end;



const
  AsTSortDirection: array[TvtSortDirection] of TSortDirection
   = (sdAscending, sdDescending);

function tiVTDisplayMaskFromDataType(const AValue : TvtTypeKind): string;
begin
  // ToDo: Should use OS constants
  case AValue of
    vttkString   : result := '';
    vttkInt      : result := '#,##0';
    vttkFloat    : result := '#,##0.000';
    vttkDate     : result := 'dd/mm/yyyy';
    vttkDateTime : result := 'dd/mm/yyyy hh:nn:ss';
    vttkTime     : result := 'hh:nn:ss';
    vttkCurrency : Result := '#,##0.00';
  else
    Assert(false, 'Invalid DataType');
  end;
end;



{ TtiVTSortOrders }

function TtiVTSortOrders.Add(AFieldName: string; ADirection: TvtSortDirection = vtsdAscending): TtiVTSortOrder;
begin
  Result := Add;
  Result.FieldName := AFieldName;
  Result.SortDirection := ADirection;
end;

function TtiVTSortOrders.Add: TtiVTSortOrder;
begin
  Result := TtiVTSortOrder(inherited Add);
end;

constructor TtiVTSortOrders.Create(owner: TComponent);
begin
  Assert(Owner <> nil, 'Owner not assigned');
  Assert(Owner is TtiCustomVirtualTree, 'Owner not a TtiCustomVirtualTree');
  inherited Create(TtiVTSortOrder);
  FOwner := Owner as TComponent;
end;

function TtiVTSortOrders.GetItem(Index: integer): TtiVTSortOrder;
begin
  Result := inherited Items[Index] as TtiVTSortOrder;
end;

function TtiVTSortOrders.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TtiVTSortOrders.SetItem(Index: integer; const AValue: TtiVTSortOrder);
begin
  inherited Items[Index]:= AValue;
end;

function TtiVTSortOrders.IndexOf(const ASortOrder: TtiVTSortOrder): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = ASortOrder then
      Exit;
  Result := -1;
end;

function TtiVTSortOrders.ItemByFieldName(const AFieldName: string;
  out ASortOrder: TtiVTSortOrder): boolean;
var
  i: integer;
begin
  i := 0;

  while (i < Count) and not AnsiSameText(Items[i].FieldName, AFieldName) do
    Inc(i);

  Result := i < Count;

  if Result then
    ASortOrder := Items[i];

end;

function TtiVTSortOrders.Remove(const ASortOrder: TtiVTSortOrder): Integer;
begin
  Result := IndexOf(ASortOrder);
  if Result >= 0 then
    Delete(Result);
end;

{ TtiVTSortOrder }

procedure TtiVTSortOrder.Assign(source: TPersistent);
begin
  inherited Assign(TtiVTSortOrder(source));
end;

constructor TtiVTSortOrder.Create(Owner: TCollection);
begin
  inherited;
  FieldName    := '';
  SortDirection := vtsdAscending;
end;

function TtiVTSortOrder.GetDisplayName: string;
begin
  result := FieldName;
end;


{ TtiVTColumn }

function TtiVTColumn.Clone: TtiVTColumn;
begin
  Result               := TtiVTColumn.Create(nil);
  try
    Result.DisplayName   := Self.DisplayName ;
    Result.FieldName     := Self.FieldName   ;
    Result.DisplayMask   := Self.DisplayMask ;
    Result.DataType      := Self.DataType    ;
    Result.Derived       := Self.Derived     ;
    Result.OnDeriveColumn := Self.OnDeriveColumn;
    Result.OnAdvancedDeriveColumn := Self.OnAdvancedDeriveColumn;
    Result.Width         := Self.Width;
    Result.Alignment     := Self.Alignment;
  except
    Result.Free;
    raise;
  end;
end;

constructor TtiVTColumn.Create(Collection: TCollection);
begin
  inherited;

  FsFieldName   := crsDefaultColFieldName;
  FsDisplayMask := tiVTDisplayMaskFromDataType(vttkString);
  FDataType     := vttkString;
  FDerived      := false;
end;

destructor TtiVTColumn.Destroy;
begin
  inherited;
end;

function TtiVTColumn.IsValidValue(AData: TtiObject; var AValue: UnicodeString): Boolean;
var
  LIsValid: Boolean;
  LDummyInt: Integer;
  LDummyFloat: Extended;
  LDummyDateTime: TDateTime;
begin
  LIsValid := True;
  if Derived then
    LIsValid := False
  else
  begin
    case DataType of
      vttkString:
        LIsValid := True;
      vttkInt:
        LIsValid := TryStrToInt(AValue, LDummyInt);
      vttkFloat,
      vttkCurrency:
        LIsValid := TryStrToFloat(AValue, LDummyFloat);
      vttkDate,
      vttkDateTime:
        LIsValid := TryStrToDate(AValue, LDummyDateTime);
      else
        Assert(False);
    end;
  end;

  if LIsValid and Assigned(FOnIsValidValue) then
    FOnIsValidValue(AData, Self, AValue, LIsValid);

  Result := LIsValid;
end;

procedure TtiVTColumn.SetDataType(const AValue: TvtTypeKind);
begin
  if DisplayMask = tiVTDisplayMaskFromDataType(FDataType) then
    DisplayMask := tiVTDisplayMaskFromDataType(AValue);
  FDataType := AValue;

  case AValue of
    vttkString, vttkDate, vttkDateTime, vttkTime:
      Alignment := taLeftJustify;

    vttkInt, vttkFloat, vttkCurrency:
      Alignment := taRightJustify;

    else
      raise EtiOPFProgrammerException.Create('Invalid TvtTypeKind');
  end;
end;

procedure TtiVTColumn.SetDerived(const AValue: boolean);
begin
  FDerived := AValue;
end;

procedure TtiVTColumn.SetFieldName(const AValue: string);
begin
  if not FDerived then
    Assert(AValue <> '', 'Can not assign empty field name.');

  if Text = FsFieldName then
    Text := AValue;

  FsFieldName := AValue;
end;

procedure TtiVTColumn.StoreWidth;
begin
  StoredWidth := Width;
end;

function TtiVTColumn.GetHasDerivedHandler: boolean;
begin
  result := Assigned(FOnDeriveColumn) or Assigned(FOnAdvancedDeriveColumn);
end;

{ TtiVTColumns }

//constructor TtiVTColumns.Create(AOwner: TtiVTHeader);
//begin
//  inherited Create(AOwner);
//end;

destructor TtiVTColumns.Destroy;
begin
  inherited;
end;

procedure TtiVTColumns.DisplayLabelsToStringList(pSL: TStringList);
var
  i : integer;
begin
  pSL.Clear;
  for i := 0 to count - 1 do
    pSL.Add(Items[i].Text);
end;

function TtiVTColumns.FindByDisplayLabel(const AValue: string): TtiVTColumn;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Text = AValue then begin
      result := Items[i];
      break; //==>
    end;
end;

function TtiVTColumns.FindByFieldName(const AValue: String): TtiVTColumn;
var
  i : Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FieldName = AValue then
    begin
      result := Items[i];
      break; //==>
    end;
end;

function TtiVTColumns.GetItem(Index: TColumnIndex): TtiVTColumn;
begin
  result := TtiVTColumn(inherited GetItem(Index));
end;

procedure TtiVTColumns.RevertToStoredWidths;
Var
  I : Integer;
begin
  For I := 0 To Pred(Count) Do
    Items[I].Width := Items[I].StoredWidth;
end;

procedure TtiVTColumns.SetItem(Index: TColumnIndex; const AValue: TtiVTColumn);
begin
  inherited SetItem(Index, AValue);
end;


procedure TtiVTColumns.StoreWidths;
Var
  I : Integer;
begin
  For I := 0 To Pred(Count) Do
    Items[I].StoreWidth;
end;

{ TtiVTHeader }

constructor TtiVTHeader.Create(AOwner: TBaseVirtualTree);
begin
  inherited;
  Options := Options + [hoVisible];
end;

function TtiVTHeader.GetColumns: TtiVTColumns;
begin
  Result := inherited Columns as TtiVTColumns;
end;

function TtiVTHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  Result := TtiVTColumns;
end;

procedure TtiVTHeader.SetColumns(const AValue: TtiVTColumns);
begin
  inherited Columns := AValue;
end;


{ TtiVTFilterDataEventArgs }

constructor TtiVTFilterDataEventArgs.Create(const AData: TtiObject);
begin
  FData := AData;
end;

{ TtiVTOnFilterDataMulticastEventDispatcher }

function TtiVTOnFilterDataMulticastEventDispatcher.GetEventArgsClass: TtiMulticastEventArgsClass;
begin
  Result := TtiVTFilterDataEventArgs;
end;

procedure TtiVTOnFilterDataMulticastEventDispatcher.InvokeHandler(
  AMethod: TMethod; ASender: TObject; AArgs: TtiMulticastEventArgs);
var
  LInclude: Boolean;
begin
  // Call the event handler using the pre-existing event handler signature.
  LInclude := (AArgs as TtiVTFilterDataEventArgs).Include;
  TtiVTOnFilterDataEvent(AMethod)((AArgs as TtiVTFilterDataEventArgs).Data,
    LInclude);
  (AArgs as TtiVTFilterDataEventArgs).Include := LInclude;
end;

procedure TtiVTOnFilterDataMulticastEventDispatcher.RegisterEventHandler(
  const AHandler: TtiVTOnFilterDataEvent;
  const AIsDefaultHandler: Boolean);
var
  LHandler: TMethod;
begin
  if Assigned(AHandler) then
    RegisterEventHandlerMethod(TMethod(AHandler), AIsDefaultHandler)
  else
  begin
    // Allow for clearing of the default handler
    LHandler.Data := nil;
    LHandler.Code := nil;
    RegisterEventHandlerMethod(TMethod(LHandler), AIsDefaultHandler);
  end;
end;

procedure TtiVTOnFilterDataMulticastEventDispatcher.UnregisterEventHandler(
  const AHandler: TtiVTOnFilterDataEvent);
begin
  if Assigned(AHandler) then
    UnregisterEventHandlerMethod(TMethod(AHandler));
end;

function TtiVTOnFilterDataMulticastEventDispatcher.GetDefaultEventHandler: TtiVTOnFilterDataEvent;
begin
  Result := TtiVTOnFilterDataEvent(DefaultEventHandlerMethod);
end;

procedure TtiVTOnFilterDataMulticastEventDispatcher.SetDefaultEventHandler(
  const AValue: TtiVTOnFilterDataEvent);
begin
  RegisterEventHandler(AValue, {AIsDefaultHandler} True);
end;

{ TtiVTEdit }

constructor TtiVTEdit.Create(Link: TStringEditLink);
begin
  inherited Create(Link);
  Self.Visible := False;
  Self.BorderStyle := bsSingle;
  Self.AutoSize := False;
  Self.OnChange := DoOnChange;
end;

function TtiVTEdit.GetLink: TtiStringEditLink;
begin
  Result := (FLink as TtiStringEditLink);
end;

procedure TtiVTEdit.DoOnChange(Sender: TObject);
begin
  if IsValidValue then
    Self.Color := clWindow
  else
    Self.Color := cInvalidValueColor;
end;

function TtiVTEdit.IsValidValue: Boolean;
var
  LColumn: TtiVTColumn;
  LData: TtiObject;
  LText: UnicodeString;
begin
  Result := True;
  if Assigned(Link) and Assigned(Link.tiTree) then
  begin
    LColumn := Link.tiTree.Header.Columns[Link.Column] as TtiVTColumn;
    Assert(Assigned(LColumn), 'Column assigned');
    LData := Link.tiTree.GetObjectFromNode(Link.Node);

    LText := Self.Text;
    Result := LColumn.IsValidValue(LData, LText);
    // We ignore any change in value as part of the validation.
  end;
end;

{ TtiStringEditLink }

constructor TtiStringEditLink.CreateWith(AtiTree: TtiCustomVirtualTree);
begin
  Create;
  FtiTree := AtiTree;
end;

{ TtiCustomVirtualTree }

function TtiCustomVirtualTree.AddColumn(const AFieldName: string;
  const pDataType: TvtTypeKind; const pDisplayLabel: string;
  pColWidth: Integer; const pAllowInlineEdit: boolean;
  const pOnIsValidValue: TtiVTOnIsValidColumnValue): TtiVTColumn;
var
  lVTC: TtiVTColumn;
begin

  lVTC := VT.Header.Columns.Add as TtiVTColumn;
  try
    lVTC.Derived := AFieldName = '';
    lVTC.FieldName := AFieldName;
    lVTC.DataType := pDataType;
    lVTC.AllowInlineEdit := pAllowInlineEdit;
    lVTC.OnIsValidValue := pOnIsValidValue;
    if pDisplayLabel = '' then
      lVTC.Text := AFieldName
    else
      lVTC.Text := pDisplayLabel;

    lVTC.Text := lVTC.DisplayName;
    if pColWidth <> -1 then
      lVTC.Width := pColWidth
    else
      lVTC.Width := VT.Canvas.TextWidth(lVTC.Text);

    if HeaderClickSorting then
      lVTC.Style := vsOwnerDraw;

    Result := lVTC;
  except
    FreeAndNil(lVTC);
    raise;
  end;
end;

function TtiCustomVirtualTree.AddColumn(
  const pDeriveColumnMethod : TtiDeriveListColumnValue;
  const pDisplayLabel : string = '';
  pColWidth : Integer = -1): TtiVTColumn;
begin
  Result := AddColumn('', vttkString, pDisplayLabel, pColWidth);
  Result.OnDeriveColumn := pDeriveColumnMethod;
end;

function TtiCustomVirtualTree.AddColumn(
  const AAdvancedDeriveColumnMethod: TtiAdvancedDeriveListColumnValue;
  const ADisplayLabel: string = '';
  AColWidth: Integer = -1): TtiVTColumn;
begin
  Result := AddColumn('', vttkString, ADisplayLabel, AColWidth);
  Result.OnAdvancedDeriveColumn := AAdvancedDeriveColumnMethod;
end;

function TtiCustomVirtualTree.DoSortElement(AOrder: TtiVTSortOrder;
  AData1, AData2: TtiObject): Integer;

  procedure _DoRaiseException(AFieldName : string; AClassName : string);
  begin
    raise exception.Create('Unable to read field <' + AFieldName + '> from <' +
        AClassName + '> in DoSortElement()');
  end;

var
  LVal1: Variant;
  LVal2: Variant;
  LColumn: TtiVTColumn;
  LDerived: Boolean;

  function _DerivedTextToVariant(const AVal: string; const ADataType: TvtTypeKind): Variant;
  begin
    // It is better to compare using the actual derived column data type
    // so that numbers etc are sorted correctly. This won't work if the
    // derived text is formatted in such a way as to make it incompatible
    // with data type conversion
    if AVal = '' then
      result := varNull;
    case ADataType of
      vttkString:   result := AVal;
      vttkInt:      if AVal <> '' then result := StrToInt64Def(AVal, 0);
      vttkFloat:    if AVal <> '' then result := StrToFloatDef(AVal, 0.0);
      vttkDate:     if AVal <> '' then result := StrToDateDef(AVal, 0.0);
      vttkDateTime: if AVal <> '' then result := StrToDateTimeDef(AVal, 0.0);
      vttkTime:     if AVal <> '' then result := StrToTimeDef(AVal, 0.0);
      vttkCurrency: if AVal <> '' then result := StrToCurrDef(AVal, 0.0);
    else
      raise EtiOPFProgrammerException.CreateFmt('Invalid data type=%d', [Ord(ADataType)]);
    end
  end;

begin
  Assert(Assigned(AOrder), 'AOrder must be assigned');
  Assert(AData1.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(AData2.TestValid(TtiObject), CTIErrorInvalidObject);

  LColumn := Header.Columns.FindByFieldName(AOrder.FieldName);

//++IPK 2012-07-09 LDerived assignment corrected.
// Reason: We only want to GetTextFromObject() if the fieldname is not supplied.
// A date field (for example) might be derived for the display, but the PropValue
// is required for sorting - which is the reason the OnDeriveColumn event was
// introduced in tiOPF2 in the first place.
// Will this mod introduce side effects? Possibly, if the user was expecting to
// provide the string value for sorting in the OnDeriveColumn for a column flagged as derived.

  LDerived := Assigned(LColumn) and (LColumn.FieldName = ''); //++IPK 2012-07-09 was: LColumn.Derived;

  if LDerived then
  begin
    // Derived column - call event hander to get values
    LVal1 := _DerivedTextToVariant(GetTextFromObject(AData1, LColumn.Index), LColumn.DataType);
    LVal2 := _DerivedTextToVariant(GetTextFromObject(AData2, LColumn.Index), LColumn.DataType);
  end
  else
  begin
    // Regular property - read values from objects
    LVal1 := AData1.PropValue[AOrder.FieldName];
    LVal2 := AData2.PropValue[AOrder.FieldName];
  end;

  if VarIsNull(LVal1) then
    _DoRaiseException(AOrder.FieldName, AData1.ClassName);

  if VarIsNull(LVal2) then
    _DoRaiseException(AOrder.FieldName, AData2.ClassName);

  if (Assigned(LColumn) and (LColumn.DataType = vttkString)) or
     ((not LDerived) and (AData1.PropType(AOrder.FieldName) = tiTKString)) then
     Result := AnsiCompareText(LVal1, LVal2)
  else begin
    if LVal1 < LVal2 then
      Result := -1
    else if LVal1 > LVal2 then
      Result := 1
    else
      Result := 0;
  end;

  if AOrder.SortDirection = vtsdDescending then
    Result := -Result;
end;

function TtiCustomVirtualTree.SortProc(AData1, AData2: Pointer): Integer;
var
  LData1: TtiObject absolute AData1;
  LData2: TtiObject absolute AData2;
  i: Integer;
begin
  Result := 0;
  for i := 0 to Pred(SortOrders.Count) do
  begin
    Result := DoSortElement(SortOrders[i], LData1, LData2);
    if Result <> 0 then
      Break;
  end;
end;

function TtiCustomVirtualTree.Focused: boolean;
begin
  result:= VT.Focused;
end;

procedure TtiCustomVirtualTree.ApplyGrouping;
var
  CurrentGroup: string;
  ItemGroup: string;
  FilteredIndex: Integer;
  ColumnIndex: Integer;
begin
  ClearGrouping;

  if SortOrders.GroupColumnCount <= 0 then
    Exit; //==>

  if not Sorted then
    Exit; //==>

  CurrentGroup := '';
  FilteredIndex := 0;
  while FilteredIndex < FFilteredData.Count do
  begin
    ItemGroup := '';
    for ColumnIndex := 0 to Pred(SortOrders.GroupColumnCount) do
      ItemGroup := ItemGroup + GetTextFromObject(TtiObject(FFilteredData[FilteredIndex]), ColumnIndex);

    if ItemGroup <> CurrentGroup then
    begin
      CurrentGroup := ItemGroup;
      FGroupedData.Add(Pointer(FilteredIndex));
    end;
    Inc(FilteredIndex);
  end;
  FGroupingApplied := True;
end;

procedure TtiCustomVirtualTree.ApplySort(const AApplyGrouping: boolean);
{$IFDEF _PROFILE}
var
  StartTick: Integer;
{$ENDIF}
begin
  if not Assigned(Data) then
    Exit;

{$IFDEF _PROFILE}
  StartTick := GetTickCount;
{$ENDIF}
  if SortOrders.Count > 0 then
    FFilteredData.SortList(SortProc);
  FSorted := true;

  FGroupingApplied := AApplyGrouping;
  if AApplyGrouping then
    ApplyGrouping
  else
    ClearGrouping;

  SetRootNodeCount;
{$IFDEF _PROFILE}
  ShowMessage(Format('Sort time %d ms for %d items', [GetTickCount-StartTick, FFilteredData.Count]));
{$ENDIF}
end;

procedure TtiCustomVirtualTree.AssignSelectedDataList(const AList: TtiObjectList);
var
 LNode: PVirtualNode;
 LData: TtiObject;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(AList.OwnsObjects = false, 'AList.OwnsObjects = false');
  Assert(AList.AutoSetItemOwner = false, 'AList.AutoSetItemOwner = false');
  AList.Clear;
  LNode:= VT.RootNode.FirstChild;
  while Assigned(LNode) do
  begin
    if vsSelected in LNode.States then
    begin
      LData:= GetObjectFromNode(LNode);
      AList.Add(LData);
    end;
    LNode := VT.GetNext(LNode);
  end;
end;

procedure TtiCustomVirtualTree.ClearGrouping;
begin
  FGroupedData.Clear;
  FGroupingApplied := False;
end;

procedure TtiCustomVirtualTree.ClearSort;
begin
  SortOrders.Clear;
  ReadData;
  FSorted := false;
  ApplyGrouping;
  SetRootNodeCount;
end;

procedure TtiCustomVirtualTree.BeginUpdate;
begin
  VT.BeginUpdate;
end;

function TtiCustomVirtualTree.CalcRootNodeChildCount(RootNode: PVirtualNode): Integer;
var
  RootFilteredIndex: Integer;
begin
  Assert(VT.GetNodeLevel(RootNode) = 0);

  if FGroupingApplied then
  begin
    RootFilteredIndex := Integer(FGroupedData[RootNode.Index]);
    if Integer(RootNode.Index) >= Pred(FGroupedData.Count) then
      Result := FFilteredData.Count - RootFilteredIndex
    else
      Result := Integer(FGroupedData[RootNode.Index + 1]) - RootFilteredIndex;
  end
  else
    Result := 0;
end;

function TtiCustomVirtualTree.CanEditRow: Boolean;
begin
  Result := Enabled and VT.Focused and not FReadOnly and (VT.SelectedCount = 1) and (SelectedData <> nil);

  if Result and Assigned(FOnCanEdit) then
    FOnCanEdit(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode, Result);
end;

function TtiCustomVirtualTree.CanDelete: Boolean;
begin
  // Was incorrectly returning false
  //Result := Enabled and VT.Focused and not ReadOnly and (VT.SelectedCount > 0) and IsNodeDataItem(VT.FocusedNode);
  Result := Enabled and VT.Focused and not ReadOnly and (VT.SelectedCount > 0) and (SelectedData <> nil);
  if Result and Assigned(FOnCanDelete) then
    FOnCanDelete(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode, Result);
end;

function TtiCustomVirtualTree.CanEdit: Boolean;
begin
  Result := CanEditRow;
end;

function TtiCustomVirtualTree.CanInsert: Boolean;
begin
  Result := Enabled and not ReadOnly;
  if Result and Assigned(FOnCanInsert) then
    FOnCanInsert(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode, Result);
end;

function TtiCustomVirtualTree.CanView: Boolean;
var
  lEnabled: Boolean;
  lFocused: Boolean;
  l1Selected: Boolean;
  lIsDataItem: Boolean;
begin
  lEnabled := Enabled;
  lFocused := VT.Focused;
  l1Selected := (VT.SelectedCount = 1);
  // This is returning false, even when an item is selected
  //lIsDataItem := IsNodeDataItem(VT.FocusedNode);

  // Try this:
  lIsDataItem := SelectedData <> nil;
  Result := lEnabled and lFocused and l1Selected and lIsDataItem;

  if Result and Assigned(FOnCanView) then
    FOnCanView(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode, Result);
end;

procedure TtiCustomVirtualTree.ConnectToData;
begin
  BeginUpdate;
  ReadData;

  if Sorted then
    ApplySort(FGroupingApplied);

  SetRootNodeCount;
  EndUpdate;
end;

constructor TtiCustomVirtualTree.Create(AOwner: TComponent);
begin
  inherited;
  BorderWidth := 0;
  FFilteredData := TObjectList.Create(False);
  FGroupedData := TList.Create;
  FSortOrders := TtiVTSortOrders.Create(Self);
  FFilterEventDispatcher := TtiVTOnFilterDataMulticastEventDispatcher.Create;

  SetVT(TtiInternalVirtualTree.Create(Self));
  VT.SetSubComponent(True);
  VT.Parent := Self;
  VT.NodeDataSize := SizeOf(Pointer);
  VT.Align := alClient;
  VT.OnHeaderClick := VTHeaderClick;
  VT.OnHeaderDrawQueryElements := VTHeaderDrawQueryElements;
  VT.OnAdvancedHeaderDraw := VTHeaderAdvancedHeaderDraw;
  VT.OnFocusChanged := VTFocusChanged;

  FFilters := TList.Create;

  // Property defaults
  FAlternateRowCount := 2;
  FShowAlternateRowColor := True;
  FAlternateRowColor := cDefaultAlternateRowColor;
  FDisabledColor := clBtnFace;
  FHeaderClickSorting:= True;
  VT.Header.Options := VT.Header.Options + [hoShowSortGlyphs, hoOwnerDraw];
  FDisplayParentNodeData := false;

  VT.OnDblClick := DoDblClick;
  VT.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, {toShowTreeLines,} toShowVertGridLines, toThemeAware, toUseBlendedImages];
  FRowSelect:= True;
  VT.TreeOptions.SelectionOptions := [toFullRowSelect, toExtendedFocus];
  FMultiSelect:= False;
  VT.Header.Style := hsFlatButtons;

  // Editing
  VT.OnCreateEditor := VTCreateEditor;
  VT.TreeOptions.MiscOptions := VT.TreeOptions.MiscOptions + [toEditable];
  VT.OnEditing := VTEditing;
  VT.OnEdited := VTEdited;
  VT.OnNewText := VTNewText;
end;

procedure TtiCustomVirtualTree.DeleteNode(Node: PVirtualNode);
var
  FilteredIndex: Integer;
  GroupIndex: Integer;
begin
  FilteredIndex := FFilteredData.Remove(GetObjectFromNode(Node));

  if FGroupingApplied then
  begin
    GroupIndex := 0;
    while GroupIndex < FGroupedData.Count do
    begin
      if Integer(FGroupedData[GroupIndex]) >= FilteredIndex then
        Break; //-->
      Inc(GroupIndex);
    end;

    while GroupIndex < FGroupedData.Count do
    begin
      FGroupedData[GroupIndex]:= Pointer(Integer(FGroupedData[GroupIndex]) - 1);
      Inc(GroupIndex);
    end;
  end;

  VT.DeleteNode(Node);
end;

destructor TtiCustomVirtualTree.Destroy;
begin
  // Unregister event handlers.
  FFilterEventDispatcher.DefaultEventHandler := nil;
  // FVT.Free;
  FFilters.Free;
  FreeAndNil(FFilterEventDispatcher);
  FreeAndNil(FGroupedData);
  FreeAndNil(FFilteredData);
  FreeAndNil(FSortOrders);
  inherited;
end;

procedure TtiCustomVirtualTree.DisconnectFromData;
begin
  VT.RootNodeCount := 0;
  FGroupedData.Clear;
  FGroupingApplied := False;
  FFilteredData.Clear;
end;

procedure TtiCustomVirtualTree.EndUpdate;
begin
  VT.EndUpdate;
end;

function TtiCustomVirtualTree.GetHeader: TtiVTHeader;
begin
  Result := VT.Header as TtiVTHeader;
end;

function TtiCustomVirtualTree.GetImages: TCustomImageList;
begin
  Result := VT.Images;
end;

function TtiCustomVirtualTree.GetObjectFromNode(pNode: PVirtualNode): TtiObject;
var
  LData: PMyRecord;
begin
  LData := PMyRecord(VT.GetNodeData(pNode));
  if Assigned(LData) then
    Result := LData.Ref
  else
    Result := nil;
end;

function TtiCustomVirtualTree.GetOnKeyDown: TKeyEvent;
begin
  Result := VT.OnKeyDown;
end;

function TtiCustomVirtualTree.GetOnKeyPress: TKeyPressEvent;
begin
  Result := VT.OnKeyPress;
end;

function TtiCustomVirtualTree.GetSelectedCount: integer;
var
  LList: TtiObjectList;
begin
  LList:= TtiObjectList.Create;
  try
    LList.AutoSetItemOwner:= False;
    LList.OwnsObjects:= False;
    AssignSelectedDataList(LList);
    result:= LList.Count;
  finally
    LList.Free;
  end;
end;

function TtiCustomVirtualTree.GetSelectedData: TtiObject;
begin
  Result := GetObjectFromNode(VT.FocusedNode);
end;

function TtiCustomVirtualTree.GetTextFromObject(AObj: TtiObject; 
  AColumnIndex: TColumnIndex; ANode: PVirtualNode): string;
var
  LColumn: TtiVTColumn;
  LField: string;
  LMask: string;
  LCurrency: Currency;
begin
  Assert(Assigned(AObj));
  if AColumnIndex = NoColumn then
  begin
    Result := '';
    Exit; //==>
  end;

  LColumn := Header.Columns[AColumnIndex];
  LField := LColumn.FieldName;
  if (not LColumn.Derived) and (LField <> '') then
  begin
    LMask := LColumn.DisplayMask;
    case LColumn.DataType of
      vttkString:
        Result := AObj.PropValue[LField];

      vttkInt:
        Result := FormatFloat(LMask, AObj.PropValue[LField]);

      vttkFloat:
        Result := FormatFloat(LMask, AObj.PropValue[LField]);

      vttkDate:
        Result := FormatDateTime(LMask, AObj.PropValue[LField]);

      vttkDateTime:
        Result := FormatDateTime(LMask, AObj.PropValue[LField]);

      vttkTime:
        Result := FormatDateTime(LMask, AObj.PropValue[LField]);

      vttkCurrency:
        begin
          LCurrency:= AObj.PropValue[LField];
          Result := Format('%m', [LCurrency]);
        end

      else
        Assert(False);
    end;
  end
  else
  begin
    Result := '';
    if LColumn.Derived and Assigned(LColumn.OnAdvancedDeriveColumn) then
      LColumn.OnAdvancedDeriveColumn(Self, AObj, ANode, LColumn, Result)
    else if LColumn.Derived and Assigned(LColumn.OnDeriveColumn) then
      LColumn.OnDeriveColumn(Self, AObj, LColumn, Result)
    else
      Result := '<' + LColumn.FieldName + '> not correctly defined';
  end;
end;

procedure TtiCustomVirtualTree.SetTextInObject(AObj: TtiObject; 
  AColumnIndex: TColumnIndex; AText: string);
var
  Column: TtiVTColumn;
  Field: string;
begin
  Assert(Assigned(AObj));
  Column := Header.Columns[AColumnIndex];
  Field := Column.FieldName;
  Assert(not Column.Derived);
  Assert(Field <> '');
  case Column.DataType of
    vttkString:
      SetPropValue(AObj, Field, AText);

    vttkInt:
      SetPropValue(AObj, Field, StrToInt(AText));

    vttkFloat:
      SetPropValue(AObj, Field, StrToFloat(AText));

    vttkDate:
      SetPropValue(AObj, Field, StrToDate(AText));

    vttkDateTime:
      SetPropValue(AObj, Field, StrToDateTime(AText));

    vttkCurrency:
      SetPropValue(AObj, Field, StrToFloat(AText));

    else
      Assert(False);
  end;
end;

function TtiCustomVirtualTree.IsInFilteredData(AObject: TObject): boolean;
var
  Lindex: Integer;
begin
  result := false;

  LIndex := 0;
  while LIndex <= FFilteredData.Count - 1 do
    if FFilteredData.Items[LIndex] = AObject then
    begin
      result := true;
      break;
    end else
      Inc(LIndex);
end;

function TtiCustomVirtualTree.IsNodeDataItem(Node: PVirtualNode): Boolean;
var
  Obj: TtiObject;
begin
  Assert(Assigned(Node));

  Obj := GetObjectFromNode(Node);
  if Assigned(Obj) then
    if VT.GetNodeLevel(Node) = 0 then
      Result := CalcRootNodeChildCount(Node) = 1
    else
      Result := True
  else
    Result := False;
end;

procedure TtiCustomVirtualTree.Loaded;
begin
  inherited;

  if HeaderClickSorting then
    VT.Header.Options := Vt.Header.Options + [hoShowSortGlyphs, hoOwnerDraw];
    
end;

//procedure TtiCustomVirtualTree.PositionCursor(AIndex: integer);
//begin
//  if VT.RootNodeCount > 0 then
//    VT.SelectFirst;
//   TODO: Implement
//end;

//procedure TtiCustomVirtualTree.PositionCursor(AData: TtiObject);
//begin
//  // TODO: Implement
////  VT.SelectFirst;
//end;

procedure TtiCustomVirtualTree.ReadData;
var
  DataIndex: Integer;
  ShouldInclude: Boolean;
begin
  if FData = nil then
    Exit; //==>

  FFilteredData.Clear;

  if FFiltered Then
  begin
    DataIndex := 0;
    while DataIndex < FData.Count do
    begin
      ShouldInclude := ItemPassesFilter(FData[DataIndex]);
      if ShouldInclude then
        FFilteredData.Add(FData[DataIndex]);
      Inc(DataIndex);
    end;
  end
  else
  begin
    DataIndex := 0;
    while DataIndex < FData.Count do
    begin
      FFilteredData.Add(FData[DataIndex]);
      Inc(DataIndex);
    end;
  end;
end;

procedure TtiCustomVirtualTree.Refresh(const ASelectedData: TtiObject = nil);
begin
  inherited Refresh;
  ConnectToData;
  if ASelectedData <> nil then
    SelectedData := ASelectedData;
end;

procedure TtiCustomVirtualTree.SetAlternateRowColor(const AValue: TColor);
begin
  if FAlternateRowColor <> AValue then
  begin
    FAlternateRowColor := AValue;
    Invalidate;
  end;
end;

procedure TtiCustomVirtualTree.SetChildControlNames;
begin
  inherited;
  if VT <> nil then
    VT.Name := tiGetUniqueComponentNameFromParent(Self, 'VT', '');
end;

procedure TtiCustomVirtualTree.SetData(const AValue: TtiObjectList);
begin
  if FData = AValue then
    Exit; //==>

  DisconnectFromData;

  // NilData is causing problems with design time cols
  //  if AValue = nil then
  //    FData := FNilData
  //  else
  FData := AValue;

  if (FData = nil) or
     (FData.Count<1) then
  begin
    // if not FDestroying then  // TJK: what's wrong with (csDestroying in ComponentState)?
    if not (csDestroying in ComponentState) then
    begin
      FFilteredData.Clear; // SetupCols refers to this so must reset now if no data
      {Refresh(False); { This has been included in an attempt to force a repaint.  If readonly was *reset* (off)
                          and no items needed display, the readonly color would remain.  Also tried FLV.Invalidate
                          in SetColor to no avail.  Alas, the problem alludes me... ipk 2003-10-14}
      VT.Invalidate;
    end;
    Exit; //==>
  end;

  Refresh;
  if Assigned(FData) then
    ConnectToData;

  // Turned this off 31/08/2000. I remember fixing the problem in the TtiTreeView -
  // it was somthing to do with using the wrong canvas. Don't remember fixing it in the
  // TtiCustomListView, but it appears to have gone away, for the time begin at least...

  // Turned it back on 02/01/2001
  //FTimerSetData.Enabled := true;
//  if FbSelectFirstRow then
//    PositionCursor(0);
end;

procedure TtiCustomVirtualTree.SetFocus;
begin
  if not Enabled then
    Exit; //==>
  inherited SetFocus;
  VT.SetFocus;
end;

procedure TtiCustomVirtualTree.SetHeader(const AValue: TtiVTHeader);
begin
  VT.Header := AValue;
end;

procedure TtiCustomVirtualTree.SetHeaderClickSorting(const Value: boolean);
var
  I: integer;
begin
  FHeaderClickSorting := Value;

  if FHeaderClickSorting then
  begin
    VT.Header.Options := VT.Header.Options + [hoOwnerDraw, hoShowSortGlyphs];

    for I := 0 to VT.Header.Columns.Count - 1 do
      VT.Header.Columns[I].Style := vsOwnerDraw;

  end
  else
  begin
  end;

end;

procedure TtiCustomVirtualTree.SetImages(const AValue: TCustomImageList);
begin
  VT.Images := AValue;
end;

procedure TtiCustomVirtualTree.SetMultiSelect(const AValue: boolean);
begin
  if AValue = FMultiSelect then
    Exit; //==>
  FMultiSelect := AValue;
  if FMultiSelect then
    VT.TreeOptions.SelectionOptions:= VT.TreeOptions.SelectionOptions + [toMultiSelect]
  else
    VT.TreeOptions.SelectionOptions:= VT.TreeOptions.SelectionOptions - [toMultiSelect];
end;

procedure TtiCustomVirtualTree.SetOnKeyDown(const AValue: TKeyEvent);
begin
  VT.OnKeyDown := AValue;
end;

procedure TtiCustomVirtualTree.SetOnKeyPress(const AValue: TKeyPressEvent);
begin
  VT.OnKeyPress := AValue;
end;

procedure TtiCustomVirtualTree.SetOnGetNodeHint(const AValue: TtiVTOnNodeHint);
begin
  FOnGetNodeHint := AValue;
end;

procedure TtiCustomVirtualTree.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
end;

function GetNodeFromIndex(VT: TtiCustomVirtualTree; Parent: PVirtualNode; Index: Integer): PVirtualNode;
begin
  if Assigned(Parent) then
    Result := Parent.FirstChild
  else
    Result := VT.VT.GetFirst;

  while Assigned(Result) and (Index >= 0) do
  begin
    Dec(Index);
    Result := VT.VT.GetNextSibling(Result);
  end;
end;

procedure TtiCustomVirtualTree.SetRootNodeCount;
begin
  // ToDo: This is the only way I can get the thing to refresh
  VT.RootNodeCount := 0;
  if FGroupingApplied then
    VT.RootNodeCount := FGroupedData.Count
  else
    VT.RootNodeCount := FFilteredData.Count;
end;

procedure TtiCustomVirtualTree.SetRowSelect(const AValue: boolean);
begin
  if AValue = FRowSelect then
    Exit; //==>
  FRowSelect := AValue;
  if FRowSelect then
  begin
    VT.TreeOptions.SelectionOptions:= VT.TreeOptions.SelectionOptions + [toFullRowSelect] - [toExtendedFocus];
    VT.TreeOptions.MiscOptions:= VT.TreeOptions.MiscOptions - [toGridExtensions];
    VT.TreeOptions.PaintOptions:= VT.TreeOptions.PaintOptions + [toHideFocusRect];
  end else
  begin
    VT.TreeOptions.SelectionOptions:= VT.TreeOptions.SelectionOptions - [toFullRowSelect] + [toExtendedFocus];
    VT.TreeOptions.MiscOptions:= VT.TreeOptions.MiscOptions + [toGridExtensions];
    VT.TreeOptions.PaintOptions:= VT.TreeOptions.PaintOptions - [toHideFocusRect];
  end;
end;

procedure TtiCustomVirtualTree.SetSelectedData(const AValue: TtiObject);
var
  LNode: PVirtualNode;
begin
  LNode := GetNodeFromObject(AValue);
  if Assigned(LNode) then
    FocusNode(lNode);
end;

function TtiCustomVirtualTree.GetNearest: TtiObject;
var
  LNode: PVirtualNode;
begin
  if VT.FocusedNode <> nil then
  begin
    LNode:= VT.FocusedNode.NextSibling;
    if LNode = nil then
      LNode:= VT.FocusedNode.PrevSibling;
    Result := GetObjectFromNode(LNode);
  end else
    result:= nil;
end;

function TtiCustomVirtualTree.GetNodeFromObject(const AData: TtiObject): PVirtualNode;
var
  LNode: PVirtualNode;
  LData: TtiObject;
begin
  Result := nil;

  if AData = nil then
    Exit; //==>

  // This is the nastiest, slowest algorithm. It causes all child nodes to be initialised. But it works.
  LNode := VT.GetFirst;
  while Assigned(LNode) and not Assigned(Result) do
  begin
    LData := GetObjectFromNode(LNode);

    if vsHasChildren in LNode.States then
      Result := GetChildNodeFromObject(LNode, AData)
    else if LData = AData then
      Result := LNode;

    if not Assigned(Result) then
      LNode := VT.GetNextSibling(LNode);
  end;
end;

function TtiCustomVirtualTree.GetChildNodeFromObject(
  const AParentNode: PVirtualNode; const AData: TtiObject): PVirtualNode;
var
  LNode: PVirtualNode;
begin
  Assert(Assigned(AParentNode));
  Result := nil;

  if (vsHasChildren in AParentNode.States) and (AParentNode.ChildCount = 0) then
    TtiInternalVirtualTree(VT).InitChildren(AParentNode);

  LNode := VT.GetFirstChild(AParentNode);
  while Assigned(LNode) and not Assigned(Result) do
  begin
    if GetObjectFromNode(LNode) = AData then
      Result := LNode
    else
      LNode := VT.GetNextSibling(LNode);
  end;
end;

procedure TtiCustomVirtualTree.Refresh(const ASelectedList: TtiObjectList;
  AFocused: TtiObject);
var
  i: integer;
  LNode: PVirtualNode;
begin
  inherited Refresh;
  ConnectToData;
  for i := 0 to ASelectedList.Count-1 do
  begin
    LNode := GetNodeFromObject(ASelectedList.Items[i]);
    VT.Selected[LNode]:= True;
  end;
  LNode := GetNodeFromObject(AFocused);
  VT.FocusedNode := LNode;
end;

procedure TtiCustomVirtualTree.Refresh(const ASelectedOID: string);
var
  LSelected: TtiObject;
begin
  if ASelectedOID <> '' then
    LSelected:= Data.Find(ASelectedOID)
  else
    LSelected:= nil;
  Refresh(LSelected);
end;

procedure TtiCustomVirtualTree.RefreshObject(AObject: TtiObject);
var
  LNode: PVirtualNode;
begin
  LNode := GetNodeFromObject(AObject);
  if Assigned(LNode) then
    VT.InvalidateNode(LNode);
end;

function TtiCustomVirtualTree.CanInlineEditFocusedCell: boolean;
var
  LColumn: TtiVTColumn;
begin
  if VT.FocusedColumn >= 0 then
  begin
    LColumn := Header.Columns[VT.FocusedColumn] as TtiVTColumn;
    Result := Assigned(LColumn) and LColumn.AllowInlineEdit and CanEditRow;
  end else
    Result := False;
end;

procedure TtiCustomVirtualTree.SetShowAlternateRowColor(const AValue: Boolean);
begin
  if FShowAlternateRowColor <> AValue then
  begin
    FShowAlternateRowColor := AValue;
    Invalidate;
  end;
end;

procedure TtiCustomVirtualTree.SetShowNodeHint(const AValue: boolean);
begin
  if AValue = FShowNodeHint then
    Exit; //==>
  FShowNodeHint := AValue;
  if FShowNodeHint then
  begin
    VT.OnGetHint:= DoOnGetHint;
    VT.HintMode:= hmHint;
    VT.ShowHint:= true;
  end else
  begin
    VT.OnGetHint:= nil;
    VT.HintMode:= hmDefault;
    VT.ShowHint:= false;
  end;
end;

//procedure TtiCustomVirtualTree.SetOnBeforeCellPaint(const AValue: TtiVTOnBeforeCellPaintEvent);
//begin
//  FOnBeforeCellPaint := AValue;
//  if Assigned(FOnBeforeCellPaint) then
//    VT.OnBeforeCellPaint := DoOnBeforeCellPaint
//  else
//    VT.OnBeforeCellPaint := nil;
//end;

procedure TtiCustomVirtualTree.SetOnAfterCellPaint(const AValue: TtiVTOnAfterCellPaintEvent);
begin
  FOnAfterCellPaint := AValue;
  if Assigned(FOnAfterCellPaint) then
    VT.OnAfterCellPaint := DoOnAfterCellPaint
  else
    VT.OnAfterCellPaint := nil;
end;

procedure TtiCustomVirtualTree.SetOnBeforeItemErase(const AValue: TtiVTBeforeItemEraseEvent);
begin
  FOnBeforeItemErase := AValue;
  if Assigned(FOnBeforeItemErase) then
    VT.OnBeforeItemErase := DoOnBeforeItemErase
  else
    VT.OnBeforeItemErase := nil;
end;

procedure TtiCustomVirtualTree.SetOnAfterItemErase(const AValue: TtiVTAfterItemEraseEvent);
begin
  FOnAfterItemErase := AValue;
  if Assigned(FOnAfterItemErase) then
    VT.OnAfterItemErase := DoOnAfterItemErase
  else
    VT.OnAfterItemErase := nil;
end;

procedure TtiCustomVirtualTree.SetOnPaintText(
  const AValue: TtiVTOnPaintText);
begin
  FOnPaintText := AValue;
  if Assigned(FOnPaintText) then
    VT.OnPaintText := DoOnPaintText
  else
    VT.OnPaintText := nil;
end;

procedure TtiCustomVirtualTree.FocusNode(ANode: PVirtualNode);
begin
  if not VT.IsVisible[ANode] then
    VT.IsVisible[ANode]:= True;
  if not VT.Selected[ANode] then
    VT.Selected[ANode]:= True;
  if VT.FocusedNode <> ANode then
    VT.FocusedNode := ANode;
  VT.ScrollIntoView(ANode, False, False);
end;

procedure TtiCustomVirtualTree.SetSortOrders(const AValue: TtiVTSortOrders);
begin
  FSortOrders.Assign(AValue);
end;

procedure TtiCustomVirtualTree.VTDoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
var
  RootNode: PVirtualNode;
begin
  if not Enabled then
  begin
    ItemColor := VT.Colors.DisabledColor;
    EraseAction := eaColor;
    Exit; //==>
  end;

  if (VT.GetNodeLevel(Node) < 2) and
     (Not Assigned(FOnBeforeCellPaint)) then
  begin
    RootNode := VT.NodeParent[Node];
    if not Assigned(RootNode) then
      RootNode := Node;

    if FShowAlternateRowColor then
    begin
      if (RootNode.Index mod FAlternateRowCount = 0) then
        ItemColor := AlternateRowColor
      else
        ItemColor := VT.Color;
      EraseAction := eaColor;
    end
    else
      EraseAction := eaDefault;
  end;
end;

procedure TtiCustomVirtualTree.VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex);
begin
  if (FLastNode <> nil) and Assigned(FOnItemLeave) then
  begin
    FOnItemLeave(Self, GetObjectFromNode(FLastNode), FLastNode);
    FLastNode := nil;
  end;
  if VT.Selected[Node] and Assigned(FOnItemArrive) then
  begin
    FLastNode:= Node;
    FOnItemArrive(Self, GetObjectFromNode(Node), Node);
  end;
  if Assigned(OnFocusChanged) then
    OnFocusChanged(Self, GetObjectFromNode(Node), Node, Column);
end;

procedure TtiCustomVirtualTree.VTDoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
var
  Obj: TtiObject;
begin
  Obj := GetObjectFromNode(Node);

  if VT.GetNodeLevel(Node) = 0 then
  begin
    // Parent nodes normally only show data for the group columns.
    if (not FDisplayParentNodeData) and (Column >= SortOrders.GroupColumnCount) and
       (CalcRootNodeChildCount(Node) > 1) then
      Text := ''
    else
      Text := GetTextFromObject(Obj, Column, Node);
  end
  else
  begin
    // must be level 1
    if (Column < SortOrders.GroupColumnCount) then
      Text := ''
    else
      Text := GetTextFromObject(Obj, Column, Node);
  end;
end;

procedure TtiCustomVirtualTree.VTGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var ImageIndex: Integer);
begin
  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self, Node, GetObjectFromNode(Node), Kind, Column, Ghosted, ImageIndex);
end;

procedure TtiCustomVirtualTree.VTHeaderAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  LSortOrder: TtiVTSortOrder;

begin

  if hpeSortGlyph in Elements then
  begin
    if SortOrders.ItemByFieldName(
      (PaintInfo.Column as TtiVTColumn).FieldName, LSortOrder) then
      DrawSortGlyph(PaintInfo.TargetCanvas,
        AsTSortDirection[LSortOrder.SortDirection], PaintInfo.SortGlyphPos);

  end;

end;

procedure TtiCustomVirtualTree.VTHeaderClick(Sender: TVTHeader;
  AHitInfo: TVTHeaderHitInfo);
const
  // TvtSortDirection = (vtsdAscending, vtsdDescending);
  InvertSortDirection: array[TvtSortDirection] of TvtSortDirection
    = (vtsdDescending, vtsdAscending);
  noGrouping = false;

var

  LColumn: TtiVTColumn;
  LSortOrder: TtiVTSortOrder;
  LNewSortDirection: TvtSortDirection;
  LExistingSortOrder: boolean;

begin
  if AHitInfo.Column = NoColumn then
    Exit; //==>

  LColumn := Header.Columns[AHitInfo.Column];

  if HeaderClickSorting and (AHitInfo.Button = mbLeft)then
  begin
    ClearSearchState;
    LColumn.Style := vsOwnerDraw;
    LExistingSortOrder := SortOrders.ItemByFieldName(LColumn.FieldName, LSortOrder);

    if LExistingSortOrder then
      LNewSortDirection := InvertSortDirection[LSortOrder.SortDirection]
    else
      LNewSortDirection := vtsdAscending;

    if not (ssCtrl in AHitInfo.Shift) then
    begin
      SortOrders.Clear;
      SortOrders.Add(LColumn.FieldName, LNewSortDirection);
    end
    else
    begin

      if not LExistingSortOrder then
        LSortOrder := SortOrders.Add(LColumn.FieldName, LNewSortDirection);

      LSortOrder.SortDirection := LNewSortDirection;
    end;

    ApplySort(noGrouping);

  end;

    if Assigned(OnHeaderClick) then
      OnHeaderClick(self, LColumn);

end;

procedure TtiCustomVirtualTree.VTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
var
  LSortOrder: TtiVTSortOrder;
begin
  // is this column sorted?
  if (PaintInfo.Column <> nil) and SortOrders.ItemByFieldName(
    (PaintInfo.Column as TtiVTColumn).FieldName, LSortOrder) then
  begin
    PaintInfo.ShowSortGlyph := true;
    // we'll paint sort glyph
    Include(Elements, hpeSortGlyph);
  end;

end;

procedure TtiCustomVirtualTree.VTInitChildren(Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if VT.GetNodeLevel(Node) = 0 then
  begin
    ChildCount := CalcRootNodeChildCount(Node);
  end;
end;

procedure TtiCustomVirtualTree.VTInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);
var
  NodeData: PMyRecord;
begin
  NodeData := VT.GetNodeData(Node);
  if FGroupingApplied then
  begin
    if VT.GetNodeLevel(Node) = 0 then
    begin
      NodeData.Ref := FFilteredData[Integer(FGroupedData[Node.Index])] as TtiObject;
      if CalcRootNodeChildCount(Node) > 1 then
        Include(InitStates, ivsHasChildren);
    end
    else // must be level 1
    begin
      NodeData.Ref := FFilteredData[Integer(FGroupedData[Parent.Index]) + Integer(Node.Index)] as TtiObject;
    end
  end
  else
  begin
    // Node levels other than 0 are not possible when grouping is not applied
    // if VT.GetNodeLevel(Node) = 0 then
    NodeData.Ref := FFilteredData[Node.Index] as TtiObject;
  end;
end;

procedure TtiCustomVirtualTree.DoEnter;
begin
  inherited;
  VT.SetFocus;
end;

//procedure TtiCustomVirtualTree.DoOnBeforeCellPaint(ASender: TBaseVirtualTree;
//  ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
//   ACellPaintMode: TVTCellPaintMode; ACellRect: TRect; var AContentRect: TRect);
//var
//  LData: TtiObject;
//begin
//  if Assigned(FOnBeforeCellPaint) then
//  begin
//    LData := GetObjectFromNode(ANode);
//    FOnBeforeCellPaint(Self, ATargetCanvas, LData, AColumn, ANode, ACellRect);
//    ATargetCanvas.FillRect(ACellRect);
//  end;
//end;

procedure TtiCustomVirtualTree.DoOnAfterCellPaint(ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
  ACellRect: TRect);
var
  LData: TtiObject;
begin
  if Assigned(FOnAfterCellPaint) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnAfterCellPaint(Self, ATargetCanvas, LData, AColumn, ANode, ACellRect);
  end;
end;

procedure TtiCustomVirtualTree.DoOnBeforeItemErase(ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; AItemRect: TRect;
  var AItemColor: TColor; var AEraseAction: TItemEraseAction);
var
  LData: TtiObject;
begin
  if Assigned(FOnBeforeItemErase) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnBeforeItemErase(Self, ATargetCanvas, LData, ANode, AItemRect, AItemColor, AEraseAction);
  end;
end;

procedure TtiCustomVirtualTree.DoOnAfterItemErase(ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; AItemRect: TRect);
var
  LData: TtiObject;
begin
  if Assigned(FOnAfterItemErase) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnAfterItemErase(Self, ATargetCanvas, LData, ANode, AItemRect);
  end;
end;

procedure TtiCustomVirtualTree.DoOnGetHint(Sender: TBaseVirtualTree;
  ANode: PVirtualNode; Column: TColumnIndex;
  var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: UnicodeString);
var
  LData: TtiObject;
begin
  if FShowNodeHint and Assigned(FOnGetNodeHint) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnGetNodeHint(Self, LData, ANode, Column, AHintText);
  end;
end;

procedure TtiCustomVirtualTree.DoOnPaintText(pSender: TBaseVirtualTree; const pTargetCanvas: TCanvas;
  pNode: PVirtualNode; pColumn: TColumnIndex; pTextType: TVSTTextType);
var
  lData: TtiObject;
begin
  if Assigned(FOnPaintText) then
  begin
    lData := GetObjectFromNode(pNode);
    FOnPaintText(Self, pTargetCanvas, lData, pColumn, pNode);
  end;
end;



procedure TtiCustomVirtualTree.DrawSortGlyph(const ACanvas: TCanvas;
  const ASortDirection: TSortDirection; const APosition: TPoint);
var
  LGlyphIndex: integer;

const
  SortGlyphs: array[TSortDirection, Boolean] of Integer = (// ascending/descending, normal/XP style
    (3, 5) {ascending}, (2, 4) {descending}
 );
begin
  LGlyphIndex := SortGlyphs[ASortDirection, tsUseThemes in VT.Header.Treeview.TreeStates];
  GetUtilityImages.Draw(ACanvas, APosition.X, APosition.Y, LGlyphIndex);

end;


function TtiCustomVirtualTree.GetSelectedIndex: integer;
var
  lSelected: TtiObject;
begin
  lSelected := SelectedData;
  if lSelected <> nil then
  begin
    if FGroupingApplied then
      Result := FGroupedData.IndexOf(lSelected)
    else
      Result := FFilteredData.IndexOf(lSelected)
  end
  else
    Result := -1;
end;

procedure TtiCustomVirtualTree.SetSelectedIndex(const AValue: integer);
var
  lSelected: TObject;
begin
  // Not sure what to do here
  if AValue = -1 then
    Exit; //==>
  if FGroupingApplied then
  begin
    if (AValue < FGroupedData.Count) then
      lSelected := FGroupedData.Items[AValue]
    else if FGroupedData.Count > 0 then
      lSelected := FGroupedData.Last
    else
      lSelected := nil;
  end else
  begin
    if (AValue < FFilteredData.Count) then
      lSelected := FFilteredData.Items[AValue]
    else if FFilteredData.Count > 0 then
      lSelected := FFilteredData.Last
    else
      lSelected := nil;
  end;
  SelectedData := lSelected as TtiObject;
end;

procedure TtiCustomVirtualTree.DoDblClick(Sender: TObject);
var
  lNode: PVirtualNode;
begin
  if Assigned(FOnDblClick) then
  begin
    lNode := VT.FocusedNode;
    FOnDblClick(Self, GetObjectFromNode(lNode), lNode);
  end;
end;

procedure TtiCustomVirtualTree.ScrollSelectedIntoView;
var
  LData: TtiObject;
begin
  LData:= SelectedData;
  SelectedData:= LData;
end;

function TtiCustomVirtualTree.SelectedNodeScreenOrigin: TPoint;
var
  lRect : TRect;
begin
  if Assigned(VT.FocusedNode) then
  begin
    lRect := VT.GetDisplayRect(VT.FocusedNode, 0, True);
    Result := VT.ClientToScreen(lRect.TopLeft);
  end else
  // ToDo: Will have to do better than this...
  begin
    Result := VT.ClientToScreen(Point(0, 0));
  end;
end;

procedure TtiCustomVirtualTree.SetEnabled(AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    inherited;
    VT.Enabled := AValue;
    // ToDo: Make these properties on TtiCustomVirtualTree;
    if AValue then
      VT.Color := clWindow
    else
      VT.Color := FDisabledColor;
  end;
end;

function TtiCustomVirtualTree.GetFilteredItemCount: Integer;
begin
  Result := FFilteredData.Count;
end;

function TtiCustomVirtualTree.GetFilteredItems(AIndex: Integer): TtiObject;
begin
  Result := FFilteredData.Items[AIndex] as TtiObject;
end;

procedure TtiCustomVirtualTree.ClearFilters;
begin
  FFilters.Clear;
  FFiltered := False;  
end;

function TtiCustomVirtualTree.ItemPassesFilter(pObject: TtiObject): Boolean;
Var
  I : Integer;
  lFilter : TLVFilter;
  lPrevConj : TFilterConj;
  LFilterDataEventArgs: TtiVTFilterDataEventArgs;
Begin
  Result := True;
  If FFiltered Then
  Begin
    // Default filter event handler
    if Assigned(FOnFilterData) then
      FOnFilterData(pObject, Result);

    // Multicast (additional) filter event handlers
    if Result and (FilterEventDispatcher.EventHandlerCount > 0) then
    begin
      LFilterDataEventArgs := TtiVTFilterDataEventArgs.Create(pObject);
      try
        LFilterDataEventArgs.Include := Result;
        FilterEventDispatcher.DispatchEvent(Self, LFilterDataEventArgs);
        Result := Result and LFilterDataEventArgs.Include;
      finally
        FreeAndNil(LFilterDataEventArgs);
      end;
    end;

    If Result Then // No point checking if it doesn't pass the first stage.
    Begin
      lPrevConj := fcAnd; // We need to AND it with any Filter event result.
      For I := 0 To FFilters.Count - 1 Do
      Begin
        lFilter := TLVFilter(FFilters.Items[I]);
        Case lPrevConj Of
          fcOr : Result := Result Or lFilter.PassesFilter(pObject);
          fcAnd : Result := Result And lFilter.PassesFilter(pObject);
          fcNone : Break;
        End; { Case }
        lPrevConj := lFilter.Join;
      End; { Loop }
    End;
  End;
end;

function TtiCustomVirtualTree.AddColumn(
    const AFieldName: string;
    const ADataType: TvtTypeKind;
    const ADeriveColumnMethod: TtiDeriveListColumnValue;
    const ADisplayLabel: string;
    const AColWidth: Integer): TtiVTColumn;
begin
  Result := AddColumn(AFieldName, ADataType, ADisplayLabel, AColWidth);
  Result.OnDeriveColumn := ADeriveColumnMethod;
end;

function TtiCustomVirtualTree.AddColumn(
    const AFieldName: string;
    const ADataType: TvtTypeKind;
    const AAdvancedDeriveColumnMethod: TtiAdvancedDeriveListColumnValue;
    const ADisplayLabel: string;
    const AColWidth: Integer): TtiVTColumn;
begin
  Result := AddColumn(AFieldName, ADataType, ADisplayLabel, AColWidth);
  Result.OnAdvancedDeriveColumn := AAdvancedDeriveColumnMethod;
end;

procedure TtiCustomVirtualTree.AddFilter(pFilter: TLVFilter);
begin
  FFilters.Add(pFilter);
end;

function TtiCustomVirtualTree.GetFilterAsString: String;
Var
  I : Integer;
begin
  For I := 0 To FFilters.Count - 1 Do
    Result := Result + ' ' + TLVFilter(FFilters.Items[I]).StringExpression;
end;

procedure TtiCustomVirtualTree.ApplyFilter;
begin
  FFiltered := True;
end;

procedure TtiCustomVirtualTree.InsertFilter(pPos: Integer; pFilter: TLVFilter);
begin
  FFilters.Insert(pPos, pFilter);
end;

procedure TtiCustomVirtualTree.DeleteFilter(pPos: Integer);
begin
  If (pPos <= FFilters.Count -1) Then
    FFilters.Delete(pPos);
end;


procedure TtiCustomVirtualTree.VTCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  LEditLink: TtiStringEditLink;
begin
  LEditLink := TtiStringEditLink.CreateWith(Self);
  EditLink := LEditLink;
  LEditLink.Edit := TtiVTEdit.Create(LEditLink);
end;

procedure TtiCustomVirtualTree.VTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Header.Columns[Column].AllowInlineEdit and CanEditRow;
end;

procedure TtiCustomVirtualTree.VTEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LObj: TtiObject;
  LColumn: TtiVTColumn;
begin
  LObj := GetObjectFromNode(Node);
  LColumn := Header.Columns[Column];
  if Assigned(LObj) and LColumn.AllowInlineEdit then
    LObj.Dirty := True;
  if Assigned(FOnEdited) then
    FOnEdited(Self, LObj, Node);
end;

procedure TtiCustomVirtualTree.VTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: UnicodeString);
var
  LObj: TtiObject;
  LColumn: TtiVTColumn;
begin
  LObj := GetObjectFromNode(Node);

  // Check if value is valid.
  LColumn := Header.Columns[Column];
  if not LColumn.IsValidValue(LObj, NewText) then
  begin
    SysUtils.Beep;
    Exit; //==>
  end;

  if VT.GetNodeLevel(Node) = 0 then
  begin
    if (Column < SortOrders.GroupColumnCount) or (CalcRootNodeChildCount(Node) <= 1) then
      SetTextInObject(LObj, Column, NewText);
  end
  else
  begin
    // must be level 1
    if (Column >= SortOrders.GroupColumnCount) then
      SetTextInObject(LObj, Column, NewText);
  end;
end;

procedure TtiCustomVirtualTree.VTSearchInsideNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; out Result: boolean);
 var
   I: integer;
   LSearchText, LColumnText: string;


 begin
   I := 0;
   LSearchText := AnsiUpperCase(SearchText);
   Result := false;

   while (I < VT.Header.Columns.Count) and ( not Result) do
   begin
     LColumnText := AnsiUpperCase(VT.Text[Node, I]);
     // look for partial match on SearchText as well as complete match
     Result := (AnsiStrPos(PChar(LColumnText), PChar(LSearchText)) <> nil);
     Inc(I);
   end;

 end;

{ TtiInternalVirtualTree }

constructor TtiInternalVirtualTree.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDesignInteractive];
  FtiOwner := Owner as TtiCustomVirtualTree;
end;

procedure TtiInternalVirtualTree.DoBeforeItemErase(Canvas: TCanvas;
  Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
  var EraseAction: TItemEraseAction);
begin
  FtiOwner.VTDoBeforeItemErase(Canvas, Node, ItemREct, Color, EraseAction);
  inherited;
end;

procedure TtiInternalVirtualTree.DoFocusChange(Node: PVirtualNode;
  Column: TColumnIndex);
begin
  FtiOwner.VTDoFocusChanged(Node, Column);
  inherited;
end;

function TtiInternalVirtualTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  FtiOwner.VTGetImageIndex(Node, Kind, Column, Ghosted, Index);
  Result := nil;
end;

procedure TtiInternalVirtualTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
begin
  FtiOwner.VTDoGetText(Node, Column, TextType, Text);
end;

function TtiInternalVirtualTree.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean;
begin
  FtiOwner.VTInitChildren(Node, ChildCount);
  Result := inherited DoInitChildren(Node, ChildCount); //++IPK VT proc changed to func in 5.4. Not sure if this is assignment is sufficient
end;

procedure TtiInternalVirtualTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  FtiOwner.VTInitNode(Parent, Node, InitStates);
  inherited;
end;

function TtiInternalVirtualTree.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TtiVTColumn;
end;

function TtiInternalVirtualTree.GetHeaderClass: TVTHeaderClass;
begin
  Result := TtiVTHeader;
end;


{ TtiCustomVirtualEditTree }

function TtiCustomVirtualEditTree.CanDelete: Boolean;
begin
  Result := (tiLVBtnVisDelete in VisibleButtons) and
    (tiLVBtnVisDelete in AllowedButtons) and
    inherited CanDelete;
end;

function TtiCustomVirtualEditTree.CanEdit: Boolean;
var
  LColumn: TtiVTColumn;
  LCanEditColumn: boolean;
begin
  if FEditInlineOnly then
  begin
    if VT.FocusedColumn >= 0 then
    begin
      LColumn := Header.Columns[VT.FocusedColumn] as TtiVTColumn;
      LCanEditColumn := Assigned(LColumn) and LColumn.AllowInlineEdit;
    end else
      LCanEditColumn := False;
  end else
    LCanEditColumn := True;

  Result := (tiLVBtnVisEdit in VisibleButtons) and
    (tiLVBtnVisEdit in AllowedButtons) and
    LCanEditColumn and
    inherited CanEdit;
end;

function TtiCustomVirtualEditTree.CanInsert: Boolean;
begin
  Result := (tiLVBtnVisNew in VisibleButtons) and
    (tiLVBtnVisNew in AllowedButtons) and
    inherited CanInsert;
end;

function TtiCustomVirtualEditTree.CanView: Boolean;
begin
  Result := (tiLVBtnVisView in VisibleButtons) and
    (tiLVBtnVisView in AllowedButtons) and
    inherited CanView;
end;

constructor TtiCustomVirtualEditTree.Create(AOwner: TComponent);
var
  LExportMenuItem: TtiVTExportMenuItem;
  LExportSubMenu: TtiVTExportMenuItem;
  I: integer;

begin
  inherited;

  // Create the popup menu
  FPopupMenu := TPopupMenu.Create(self);
  FPopupMenu.Images := gTIImageListMgr.ILNormal16;
  FPopupMenu.OnPopup := DoMenuPopup;

  // Create select columns menu item
  FpmiView         := TMenuItem.Create(self);
  FpmiView.Caption := '&View';
  FpmiView.OnClick := DoViewOrEdit;
  FpmiView.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_View);
  FPopupMenu.Items.Add(FpmiView);

  FpmiEdit         := TMenuItem.Create(self);
  FpmiEdit.Caption := '&Edit';
  FpmiEdit.OnClick := DoEdit;
  FpmiEdit.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Edit);
  FPopupMenu.Items.Add(FpmiEdit);

  FpmiNew         := TMenuItem.Create(self);
  FpmiNew.Caption := '&New';
  FpmiNew.OnClick := DoNew;
  FpmiNew.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Insert);
  FPopupMenu.Items.Add(FpmiNew);

  FpmiDelete       := TMenuItem.Create(self);
  FpmiDelete.Caption := '&Delete';
  FpmiDelete.OnClick := DoDelete;
  FpmiDelete.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Delete);
  FPopupMenu.Items.Add(FpmiDelete);

  FpmiSortGroup    := TMenuItem.Create(self);
  FpmiSortGroup.Caption := '-';
  FPopupMenu.Items.Add(FpmiSortGroup);

  FpmiClearSort         := TMenuItem.Create(self);
  FpmiClearSort.Caption := '&Clear Sort';
  FpmiClearSort.OnClick := DoClearSort;
  FpmiClearSort.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Sort);
  FpmiClearSort.Visible := true;
  FPopupMenu.Items.Add(FpmiClearSort);

  FSearching := true;

  FpmiShowFind         := TMenuItem.Create(self);
  FpmiShowFind.Caption := '&Find...';
  FpmiShowFind.OnClick := DoShowFind;
  FpmiShowFind.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Find);
  FpmiShowFind.Visible := true;
  FpmiShowFind.ShortCut := ShortCut(Word('F'), [ssCtrl]);
  FPopupMenu.Items.Add(FpmiShowFind);
  EnablePopupMenu;

  FFiltering := true;
  FFiltered  := true;

  FpmiFilter := TMenuItem.Create(Self);
  FpmiFilter.Caption := 'Fil&ter';
  FpmiFilter.OnClick := DoFilter;
  FpmiFilter.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Query);
  FpmiFilter.Visible := true;
  FpmiFilter.ShortCut := ShortCut(Word('T'), [ssCtrl]);
  FPopupMenu.Items.Add(FpmiFilter);

  FExporting := true;

  LExportMenuItem      := TtiVTExportMenuItem.Create(self);
  LExportMenuItem.Caption := '-';
  LExportMenuItem.Visible := true;
  FPopupMenu.Items.Add(LExportMenuItem);

  LExportSubMenu := TtiVTExportMenuItem.Create(self);
  LExportSubMenu.Caption := 'Export';
  LExportSubMenu.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Export);
  LExportSubMenu.Visible := true;
  FPopupMenu.Items.Add(LExportSubMenu);

  for I := 0 to tiVTExportRegistry.ExportCount - 1 do
  begin
    LExportMenuItem := TtiVTExportMenuItem.Create(self);
    LExportMenuItem.Caption := Format( 'Export to %s',
      [tiVTExportRegistry.GetExport(I).FormatName] );
    LExportMenuItem.ExportName
      := tiVTExportRegistry.GetExport(I).FormatName;
    LExportMenuItem.ImageIndex := gTIImageListMgr.ImageIndex16(
      tiVTExportRegistry.GetExport(I).ImageName);
    LExportMenuItem.OnClick := DoExportTree;
    LExportMenuItem.Visible := true;
    LExportSubMenu.Add(LExportMenuItem);
  end;

  FStdPopupItemCount := FPopupMenu.Items.Count;
  FButtonStyle := lvbsButtonsAndLabels;
  FAllowedButtons := [tiLVBtnVisView, tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]; // All allowed
  FEditInlineOnly := False;
end;

procedure TtiCustomVirtualEditTree.CreateButtonPanel;
begin
  tiCtrlButtonPanel.CreateCtrlBtnPnl(FCtrlBtnPnl, ButtonStyle, Self,
                                      CanView, CanInsert, CanEdit, CanDelete);
  FCtrlBtnPnl.Align := alTop;
  FCtrlBtnPnl.VisibleButtons := FVisibleButtons;
  FCtrlBtnPnl.AllowedButtons := FAllowedButtons;

  if Assigned(FOnItemDelete) then
    FCtrlBtnPnl.OnDelete := DoDelete;
  if FEditInlineOnly or Assigned(FOnItemEdit) then
    FCtrlBtnPnl.OnEdit := DoViewOrEdit;
  if Assigned(FOnItemInsert) then
    FCtrlBtnPnl.OnNew := DoNew;
  if Assigned(FOnItemView) then
    FCtrlBtnPnl.OnView := DoViewOrEdit;

  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiCustomVirtualEditTree.CreateWnd;
begin
  inherited;
  if not Assigned(FCtrlBtnPnl) then
    CreateButtonPanel;
end;

procedure TtiCustomVirtualEditTree.DestroyWnd;
begin
  FreeAndNil(FCtrlBtnPnl);
  inherited;
end;

procedure TtiCustomVirtualEditTree.DisablePopupMenu;
begin
  PopupMenu:= nil;
end;

procedure TtiCustomVirtualEditTree.DoClearSort(Sender: TObject);
begin
  ClearSort;

  if Assigned(OnClearSort) then
    FOnClearSort(self);

end;

procedure TtiCustomVirtualEditTree.DoDblClick(Sender: TObject);
begin
  if (VT.FocusedNode = nil) or
     (vsHasChildren in VT.FocusedNode.States) then
    Exit; //==> Node will be expanded, so don't do anything else

  // If the OnDblClick property is assigned, this gets precedence over Edit/View/New
  if Assigned(FOnDblClick) then
    inherited DoDblClick(Sender)
  else if CanEdit or CanView then
    DoViewOrEdit(Self)
  else if CanInsert then
    DoNew(Self);
end;

procedure TtiCustomVirtualEditTree.DoDelete(Sender: TObject);
begin
  if Assigned(FOnItemDelete) then
    FOnItemDelete(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
end;

procedure TtiCustomVirtualEditTree.DoEdit(Sender: TObject);
begin
  if FEditInlineOnly then
  begin
    VT.SetFocus;
    if (VT.FocusedNode <> nil) and (VT.FocusedColumn >= 0) then
      VT.EditNode(VT.FocusedNode, VT.FocusedColumn);
  end else if Assigned(FOnItemEdit) then
  begin
    VT.SetFocus;
    FOnItemEdit(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.DoNew(Sender: TObject);
begin
  if Assigned(FOnItemInsert) then
  begin
    VT.SetFocus;
    FOnItemInsert(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.DoView(Sender: TObject);
begin
  if Assigned(VT.FocusedNode) and Assigned(FOnItemView) then
  begin
    VT.SetFocus;
    FOnItemView(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.DoViewOrEdit(Sender: TObject);
begin
  if CanEdit then 
    DoEdit(Sender)
  else if CanView then
    DoView(Sender);
end;

procedure TtiCustomVirtualEditTree.EnablePopupMenu;
begin
  PopupMenu:= FPopupMenu;
end;

procedure TtiCustomVirtualEditTree.Loaded;
begin
  inherited;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.RefreshButtons;
    
  { If it has at least a streamed item then show an ordered TtiVTListView }
  if FSortOrders.Count > 0 then
    FSorted := True;
end;

procedure TtiCustomVirtualEditTree.Refresh(const ASelectedOID: string);
begin
  inherited;
  DoRefreshButtons;
end;

procedure TtiCustomVirtualEditTree.DoMenuPopup(Sender: TObject);
var
  i : Integer;
begin
  FpmiView.Visible  := (tiLVBtnVisView in VisibleButtons) and Assigned(FOnItemView);
  FpmiEdit.Visible  := (tiLVBtnVisEdit in VisibleButtons) and Assigned(FOnItemEdit);
  FpmiNew.Visible   := (tiLVBtnVisNew in VisibleButtons) and Assigned(FOnItemInsert);
  FpmiDelete.Visible := (tiLVBtnVisDelete in VisibleButtons) and Assigned(FOnItemDelete);

  FpmiView.Shortcut := TextToShortcut('');
  FpmiEdit.Shortcut := TextToShortcut('');
  FpmiNew.Shortcut := TextToShortcut('');
  FpmiDelete.Shortcut := TextToShortcut('');

  if FpmiView.Visible then
    FpmiView.Shortcut := TextToShortcut('Enter');

  if FpmiEdit.Visible then
    FpmiEdit.Shortcut := TextToShortcut('Enter');

  if FpmiNew.Visible then
    FpmiNew.Shortcut := TextToShortcut('Ins');

  if FpmiDelete.Visible then
    FpmiDelete.Shortcut := TextToShortcut('Del');

  FpmiView.Enabled  := CanView;
  FpmiEdit.Enabled  := CanEdit;
  FpmiDelete.Enabled := CanDelete;
  FpmiNew.Enabled   := CanInsert;

  for i := FPopupMenu.Items.Count - 1 downto FStdPopupItemCount do
    FPopupMenu.Items.Delete(i);

  if FGroupingApplied then
    AddGroupingPopupItems;

end;

procedure TtiCustomVirtualEditTree.AddGroupingPopupItems;
var
  lMI: TMenuItem;
begin
  lMI         := TMenuItem.Create(self);
  lMI.Caption := '-';
  FPopupMenu.Items.Add(lMI);

  lMI         := TMenuItem.Create(self);
  lMI.Caption := 'Expand all';
  lMI.OnClick := DoExpandAll;
  FPopupMenu.Items.Add(lMI);

  lMI         := TMenuItem.Create(self);
  lMI.Caption := 'Collapse all';
  lMI.OnClick := DoCollapseAll;
  FPopupMenu.Items.Add(lMI);

end;

procedure TtiCustomVirtualEditTree.DoExpandAll(Sender: TObject);
begin
  VT.FullExpand(nil);
end;

procedure TtiCustomVirtualEditTree.DoExportTree(Sender: TObject);
var
  LExporter: TtiVTExport;
  LExportName: string;

begin
  LExportName := (Sender as TtiVtExportMenuItem).ExportName;
  LExporter := tiVTExportRegistry.GetExport(LExportName).Create;
  try
    LExporter.Execute(Self);
  finally
    LExporter.Free;
  end;

end;

procedure TtiCustomVirtualEditTree.DoShowFind(Sender: TObject);
begin
  SP.Showing := not SP.Showing;
end;

procedure TtiCustomVirtualEditTree.DoCollapseAll(Sender: TObject);
begin
  VT.FullCollapse(nil);
end;

procedure TtiCustomVirtualEditTree.SetButtonStyle(const AValue: TLVButtonStyle);
begin
  if FButtonStyle <> AValue then
  begin
    FButtonStyle := AValue;
    CreateButtonPanel;
  end;
end;

procedure TtiCustomVirtualEditTree.SetChildControlNames;
begin
  inherited;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.Name := tiGetUniqueComponentNameFromParent(Self, 'CtrlBtnPnl');
  if Assigned(FPopupMenu) then
    FPopupMenu.Name := tiGetUniqueComponentNameFromParent(Self, 'PopupMenu');
  if Assigned(FpmiView) then
    FpmiView.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiView');
  if Assigned(FpmiEdit) then
    FpmiEdit.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiEdit');
  if Assigned(FpmiNew) then
    FpmiNew.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiNew');
  if Assigned(FpmiDelete) then
    FpmiDelete.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiDelete');
  if Assigned(FpmiFilter) then
    FpmiFilter.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiFilter');
  if Assigned(FpmiSortGroup) then
    FpmiSortGroup.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiSortGroup');
  if Assigned(FpmiClearSort) then
    FpmiClearSort.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiClearSort');
  if Assigned(FpmiShowFind) then
    FpmiShowFind.Name := tiGetUniqueComponentNameFromParent(Self, 'pmiShowFind');
end;

procedure TtiCustomVirtualEditTree.SetOnClearSort(const AValue: TtiVTClearSortEvent);
begin
  FOnClearSort := AValue;
end;

procedure TtiCustomVirtualEditTree.SetOnExported(
  const AValue: TtiVTExportedEvent);
begin
  FOnExported := AValue;
end;

procedure TtiCustomVirtualEditTree.SetOnExportedToFile(
  const AValue: TtiVTExportedToFileEvent);
begin
  FOnExportedToFile := AValue;
end;

procedure TtiCustomVirtualEditTree.SetOnExportFileName(
  const AValue: TtiVTExportFileNameEvent);
begin
  FOnExportFileName := AValue;
end;

procedure TtiCustomVirtualEditTree.SetOnItemDelete(const AValue: TtiVTItemEditEvent);
begin
  FOnItemDelete := AValue;
  if Assigned(FCtrlBtnPnl) then
    if Assigned(FOnItemDelete) then
      FCtrlBtnPnl.OnDelete := DoDelete
    else
      FCtrlBtnPnl.OnDelete := nil;
end;

procedure TtiCustomVirtualEditTree.SetOnItemEdit(const AValue: TtiVTItemEditEvent);
begin
  FOnItemEdit := AValue;
  if Assigned(FCtrlBtnPnl) then
    if Assigned(FOnItemEdit) then
      FCtrlBtnPnl.OnEdit := DoEdit
    else
      FCtrlBtnPnl.OnEdit := nil;
end;

procedure TtiCustomVirtualEditTree.SetOnItemInsert(const AValue: TtiVTItemEditEvent);
begin
  FOnItemInsert := AValue;
  if Assigned(FCtrlBtnPnl) then
    if Assigned(FOnItemInsert) then
      FCtrlBtnPnl.OnNew := DoNew
    else
      FCtrlBtnPnl.OnNew := nil;
end;

procedure TtiCustomVirtualEditTree.SetOnItemView(const AValue: TtiVTItemEditEvent);
begin
  FOnItemView := AValue;
  if Assigned(FCtrlBtnPnl) then
    if Assigned(FOnItemView) then
      FCtrlBtnPnl.OnView := DoViewOrEdit
    else
      FCtrlBtnPnl.OnView := nil;
end;

procedure TtiCustomVirtualEditTree.SetSearching(const AValue: boolean);
begin
  FSearching := AValue;
  FpmiShowFind.Visible := AValue;
  FpmiShowFind.Enabled := AValue;
  FpmiSortGroup.Visible := FpmiClearSort.Visible or FpmiShowFind.Visible;
end;

procedure TtiCustomVirtualEditTree.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FVisibleButtons := AValue;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.VisibleButtons := AValue;
end;

procedure TtiCustomVirtualEditTree.SPEnterFindEdit(Sender: TObject);
begin
  DisablePopupMenu;
end;

procedure TtiCustomVirtualEditTree.SPEnterKey(Sender: TObject);
begin
  DoViewOrEdit(Self);
end;

procedure TtiCustomVirtualEditTree.SPExitFindEdit(Sender: TObject);
begin
  EnablePopupMenu;
end;

procedure TtiCustomVirtualEditTree.SPShowing(const ASender: TtiVTSearchPanel;
  const AIsShowing: Boolean);
begin
  inherited;
  if not AIsShowing then
    EnablePopupMenu;
end;

procedure TtiCustomVirtualEditTree.VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex);
begin
  inherited;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.EnableButtons;
end;

procedure TtiCustomVirtualEditTree.Refresh(const ASelectedData: TtiObject);
begin
  inherited;
  DoRefreshButtons;
end;

procedure TtiCustomVirtualEditTree.SetAllowedButtons(
  const AAllowedButtons: TtiLVVisibleButtons);
begin
  FAllowedButtons := AAllowedButtons;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.AllowedButtons := FAllowedButtons;
end;

function TtiCustomVirtualEditTree.GetButtonPanel: TtiCtrlBtnPnlAbs;
begin
  if not Assigned(FCtrlBtnPnl) then
    CreateButtonPanel;
  Result := FCtrlBtnPnl;
end;

procedure TtiCustomVirtualEditTree.DoEnter;
begin
  inherited;
  DoRefreshButtons;
end;

procedure TtiCustomVirtualEditTree.DoExit;
begin
  inherited;
  DoRefreshButtons;
end;

procedure TtiCustomVirtualEditTree.DoRefreshButtons;
begin
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.EnableButtons;
end;

procedure TtiCustomVirtualEditTree.SetEnabled(AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    inherited;
    if Assigned(FCtrlBtnPnl) then
      FCtrlBtnPnl.EnableButtons;
  end;
end;

procedure TtiCustomVirtualEditTree.SetExporting(const AValue: boolean);
var
  I: integer;

begin
  FExporting := AValue;

  for I := 0 to FPopupMenu.Items.Count - 1 do
      if FPopupMenu.Items[I] is TtiVTExportMenuItem then
      begin
        FPopupMenu.Items[I].Visible := FExporting;
        FPopupMenu.Items[I].Enabled := FExporting;
      end;
end;

procedure TtiCustomVirtualEditTree.SetHeaderClickSorting(const AValue: boolean);
begin
  inherited SetHeaderClickSorting(AValue);
  // hide ClearSort popup menu item when sorting is disabled
  FpmiClearSort.Visible := AValue;
  FpmiSortGroup.Visible := FpmiClearSort.Visible or FpmiShowFind.Visible;
end;

procedure TtiCustomVirtualEditTree.SetFiltering(const AValue: boolean);
begin
  FFiltered := AValue;
  FFiltering := AValue;
  FpmiFilter.Visible := AValue;
  FpmiFilter.Enabled := AValue;
end;

procedure TtiCustomVirtualEditTree.DoFilter(Sender: TObject);
Var
  lObject : TtiObject;
begin
  lObject := FData.First;
  If TfrmLVFilter.EditFilter(lObject, FFilters) Then
  Begin
    FFiltered := True;
    Refresh;
  End;
end;


{ TtiVTExportMenuItem }

procedure TtiVTExportMenuItem.SetExportName(const AValue: string);
begin
  FExportName := AValue;
end;

var
  utiVTExportRegistry: TtiVTExportRegistry;

function tiVTExportRegistry: TtiVTExportRegistry;
begin

  if (utiVTExportRegistry = nil) then
    utiVTExportRegistry := TtiVTExportRegistry.Create;

  Result := utiVTExportRegistry;
end;

  { TtiVTExport }

procedure TtiVTExport.CloseOutput;
begin

    if Assigned(FSourceTree.OnExported) then
      FSourceTree.OnExported(self);

end;

constructor TtiVTExport.Create;
begin
  inherited;
  FStream := TtiPreSizedStream.Create(0, cStreamGrowBy);
end;

function TtiVTExport.CreateOutput: boolean;
begin
  Result := true;
end;

destructor TtiVTExport.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TtiVTExport.Execute(const ATree: TtiCustomVirtualEditTree);
begin
  FSourceTree := ATree;
  Assert(Assigned(FSourceTree),
    'Internal error: TtiVTExport.Execute - unassignsed ATree');

  if CreateOutput then
  begin
    WriteHeader;
    WriteTitles;
    WriteDetail;
    WriteFooter;
    CloseOutput;
  end;
end;

procedure TtiVTExport.WriteDetail;
begin

end;

procedure TtiVTExport.WriteDetailEx(
  const AFieldSeparator, AFieldDelimiter: char);
var
  lNode: PVirtualNode;
  I: integer;
  LColumn: TtiVTColumn;
  LLastColumnIndex: integer;
  LObject: TtiObject;
  LValueText: string;

begin
  lNode := FSourceTree.VT.GetFirst;

  while (lNode <> nil) do
  begin
    SetLength(FOutputStr, 0);
    LObject := FSourceTree.GetObjectFromNode(lNode);
    LLastColumnIndex := FSourceTree.VT.Header.Columns.Count - 1;

    for I := 0 to LLastColumnIndex do
    begin
      LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

      if (not LColumn.Derived) or LColumn.HasDerivedHandler then
      begin
        if LColumn.DataType = vttkString then
          FmtStr(FOutputStr, '%s%s%s%s%s', [FOutputStr, AFieldSeparator,
            AFieldDelimiter, FSourceTree.GetTextFromObject(LObject, I, lNode),
              AFieldDelimiter])
        else if (LColumn.DataType in [vttkInt, vttkFloat]) then
        begin
          LValueText := FSourceTree.GetTextFromObject(LObject, I, lNode);
          // strip out all instances of field separator within formatted numbers
          LValueText := tiStrTran(LValueText, AFieldSeparator, '');
          FmtStr(FOutputStr, '%s%s%s', [FOutputStr, AFieldSeparator, LValueText]);
        end
        else
          FmtStr(FOutputStr, '%s%s%s', [FOutputStr, AFieldSeparator,
            FSourceTree.GetTextFromObject(LObject, I, lNode)]);
      end;

    end;

    if LLastColumnIndex >= 0 then
      Delete(FOutputStr, 1, Length(AFieldSeparator));

    WriteToOutput(FOutputStr);
    lNode := FSourceTree.VT.GetNext(lNode);
  end;
end;

procedure TtiVTExport.WriteFooter;
begin

end;

procedure TtiVTExport.WriteHeader;
begin

end;

procedure TtiVTExport.WriteHeaderEx(const AFieldSeparator, AFieldDelimiter: char);
var
  I, LHeaderRow: integer;
  LColumn: TtiVTColumn;
  LLastColumnIndex: integer;

begin
  LLastColumnIndex := FSourceTree.VT.Header.Columns.Count - 1;

  for LHeaderRow := 0 to (FSourceTree.VT.Header as TtiVTHeader).RowCount - 1 do
  begin
    SetLength(FOutputStr, 0);

    for I := 0 to LLastColumnIndex do
    begin
      LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

      if (not LColumn.Derived) or LColumn.HasDerivedHandler then
          FmtStr(FOutputStr, '%s%s%s%s%s', [FOutputStr, AFieldSeparator,
            AFieldDelimiter, LColumn.DisplayNames[LHeaderRow], AFieldDelimiter])

    end;

    if LLastColumnIndex >= 0 then
      Delete(FOutputStr, 1, Length(AFieldSeparator));

    WriteToOutput(FOutputStr);
  end;
end;

procedure TtiVTExport.WriteTitles;
begin

end;

procedure TtiVTExport.WriteToOutput(const AText: string);
begin
  FStream.WriteLn(AText);
end;

{ TtiVTExportFile }

procedure TtiVTExportFile.CloseOutput;
var
  LAskToViewFile: boolean;

begin

  try
    FStream.SaveToFile(FFileName);
    LAskToViewFile := true;

    if Assigned(FSourceTree.OnExportedToFile) then
      FSourceTree.OnExportedToFile(self, LAskToViewFile);

    if LAskToViewFile and tiAppConfirmation('Do you want to view "%s" ?', [FFileName]) then
      tiEditFile(FFileName);

  except
    on E: Exception do
    begin
      tiAppError(Format('Error creating output file %s (%s).', [FFileName,
        E.Message]));
    end;
  end;

  inherited CloseOutput;
end;

function TtiVTExportFile.CreateOutput: boolean;
begin
  Result := inherited CreateOutput and GetFileName;
end;

function TtiVTExportFile.GetFileName: Boolean;
const
  fmtFilter = '%s Files|*.%s|All Files|*.*';
var
  LSaveDialog: TSaveDialog;
  LAskUser: boolean;

begin
  Result := false;
  FFileName := tiVTExportRegistry.ExportFileNameStore.GetFileName(
    FSourceTree, FormatName);
  LAskUser := true;

  if Assigned(FSourceTree.OnExportFileName) then
  begin
    FSourceTree.OnExportFileName(self, FFileName, LAskUser);
    Result := true;
  end;

  if LAskUser then
  begin
    LSaveDialog := TSaveDialog.Create(nil);

    try
      LSaveDialog.DefaultExt := FDefaultExt;
      LSaveDialog.FileName := FFileName;
      LSaveDialog.Filter := Format(fmtFilter, [FormatName, FDefaultExt]);

      if LSaveDialog.Execute then
      begin
        FFileName := LSaveDialog.FileName;
        tiVTExportRegistry.ExportFileNameStore.SetFileName(FSourceTree,
          FormatName, FFileName);
        Result := True;
      end else
        Result := False;

    finally
      LSaveDialog.Free;
    end;

  end;

  if Result then
    tiVTExportRegistry.ExportFileNameStore.SetFileName(FSourceTree, FormatName, FFileName);

end;

{ TtiVTExportRegistry }

constructor TtiVTExportRegistry.Create;
begin
  inherited Create;
  FExportMappings := TStringList.Create;
  (FExportMappings as TStringList).Sorted := true;
  (FExportMappings as TStringList).Duplicates := dupError;
end;

destructor TtiVTExportRegistry.Destroy;
begin
  FExportMappings.Free;
  SetExportFileNameStore(nil);
  inherited;
end;

function TtiVTExportRegistry.GetExport(const AFormatName: string): TtiVTExportClass;
var
  I: Integer;
begin
  I := FExportMappings.IndexOf(UpperCase(AFormatName));
  if (I = -1) then
    ShowMessage('Request for invalid export class "' + AFormatName + '"');
  Result := TtiVTExportClass(FExportMappings.Objects[I]);
end;

function TtiVTExportRegistry.GetExport(const AIndex: integer): TtiVTExportClass;
begin
  Assert( (AIndex >= 0) and (AIndex < FExportMappings.Count),
    'Internal error: bad export index');
  Result := TtiVTExportClass(FExportMappings.Objects[AIndex]);
end;

function TtiVTExportRegistry.GetExportCount: integer;
begin
  Result := FExportMappings.Count;
end;

function TtiVTExportRegistry.GetExportFileNameStore: TtiVTExportFileNameStore;
begin
  // Do this assignment as late as possible (ie, here!) so user has a chance to
  // register a custom filename store after creating registry without clobbering
  // default store.
  if not Assigned(FExportFileNameStore) then
    SetExportFileNameStore(TtiVTIniExportFileNameStore.CreateExt(gINI));

  Result := FExportFileNameStore;
end;

procedure TtiVTExportRegistry.RegisterExport(const AVTExportClass: TtiVTExportClass);
var
  LFormatName: string;

begin
  LFormatName := UpperCase(AVTExportClass.FormatName);
  try
    FExportMappings.AddObject(LFormatName, TObject(AVTExportClass));
  except
    on EStringListError do
    begin
      ShowMessage('Attempt to register duplicate export "'
        + AVTExportClass.FormatName + '"');
      Exit;
    end;
  end;

end;

procedure TtiVTExportRegistry.SetExportFileNameStore(
  const AValue: TtiVTExportFileNameStore);
begin
  if Assigned(FExportFileNameStore) then
    FExportFileNameStore.Free;

  FExportFileNameStore := AValue;
end;

{ TtiVTIniExportFileNameStore }

constructor TtiVTIniExportFileNameStore.CreateExt(const AIniFile: TtiINIFile);
begin
  inherited Create;
  FIniFile := AIniFile;
end;

function TtiVTIniExportFileNameStore.GetFileName(const AVT: TComponent;
  const AExportFormatName: string): string;
var
  LSection, LIdent: string;

begin
  GetID(AVT, AExportFormatName, LSection, LIdent);
  Result :=  FIniFile.ReadString(LSection, LIdent, '');
end;

procedure TtiVTIniExportFileNameStore.GetID(const AVT: TComponent;
  const AExportFormatName: string; out ASection, AIdent: string);
begin
  Assert(Assigned(AVT) and (AVT.Name <> ''),
    'Internal error: TExportFileNamePersister.GetID() - AVT or AVT.Name unassigned');

  if Assigned(AVT.Owner) and (AVT.Owner.Name <> '') then
    ASection := AVT.Owner.Name
  else
    ASection := 'nil';

  FmtStr(AIdent, '%s.%s', [AVT.Name, AExportFormatName]);
end;

procedure TtiVTIniExportFileNameStore.SetFileName(const AVT: TComponent;
   const AExportFormatName: string; const AFileName: string);
var
  LSection, LIdent: string;

begin
  GetID(AVT, AExportFormatName, LSection, LIdent);
  FIniFile.WriteString(LSection, LIdent, AFileName);
end;

{ TTiVTExportClipboard }

resourcestring
  ClipboardDesc = 'Clipboard';

const
  ClipboardFieldSeparator = #9;
  ClipboardFieldDelimiter = '"';

procedure TTiVTExportClipboard.CloseOutput;
begin
  Clipboard.AsText := FStream.AsString;
  inherited CloseOutput;
end;

class function TTiVTExportClipboard.FormatName: string;
begin
  Result := ClipboardDesc;
end;

class function TTiVTExportClipboard.ImageName: string;
begin
  Result := cResTI_CopyToClipboard;
end;

procedure TTiVTExportClipboard.WriteDetail;
begin
  WriteDetailEx(ClipboardFieldSeparator, ClipboardFieldDelimiter);
end;

procedure TTiVTExportClipboard.WriteHeader;
begin
  WriteHeaderEx(ClipboardFieldSeparator, ClipboardFieldDelimiter);
end;

{ TLVFilter }

Constructor TLVFilter.Create;
Begin
  Inherited;
  Join := fcNone;
  FilterOperator := foNone;
  Value := '';
  PropName := '';
End;

Function TLVFilter.FloatPropPasses(pValue : TtiObject) : Boolean;
Var
  lsProp : String;
  lrProp : Extended;
  lrVal : Extended;
  lRetCode : Integer;
  lValidNum : Boolean;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from FloatPropPasses', [FFieldName, E.Message]);
  End;

  lrProp := StrToFloat(lsProp);

  Val(Value, lrVal, lRetCode);
  lValidNum := (lRetCode = 0);
  If Not lValidNum Then
  Begin
    Try
      lrVal := StrToDateTime(Value);
      lValidNum := True;
    Except
      lValidNum := False;
    End;
  End;
  If lValidNum Then
  Begin
    Case FilterOperator Of
      foEqual : Result := (lrProp = lrVal);
      foNotEqual : Result := (lrProp <> lrVal);
      foGreater : Result := (lrProp > lrVal);
      foGreaterEqual : Result := (lrProp >= lrVal);
      foLess : Result := (lrProp < lrVal);
      foLessEqual : Result := (lrProp <= lrVal);
    Else
      Raise Exception.Create('Invalid operator passed to FloatPropPasses.');
    End;
  End
  Else
    Result := False;
End;

Function TLVFilter.GetSimplePropType(pPersistent : TtiObject; pPropName : String) : TvtTypeKind;
Var
  lPropType : TTypeKind;
Begin
  Try
    lPropType := PropType(pPersistent, pPropName);
  Except
    On E : Exception Do
      Raise Exception.Create('Error in tiGetSimpleTypeKind ' + 'Message: ' + E.Message);
  End;
  Case lPropType Of
    tkInteger,
      tkInt64,
      tkEnumeration : Result := vttkInt;
    tkFloat : Result := vttkFloat;
    tkString,
      tkChar,
      tkWChar,
      tkLString,
      tkWString : Result := vttkString;
  Else
    Raise Exception.Create('Invalid property type passed to ' +
      'tiGetSimpleTypeKind');
  End;
End;

Function TLVFilter.GetStringExpression : String;
Begin
  Result := Format('%s %s "%s" %s', [FFieldName, FilterOps[Ord(FFilterOperator)], FValue, FilterConjs[Ord(FJoin)]]);
End;

Function TLVFilter.IntPropPasses(pValue : TtiObject) : Boolean;
Var
  lsProp : String;
  liProp : Integer;
  liVal : Integer;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from IntPropPasses', [FFieldName, E.Message]);
  End;
  Try
    liProp := StrToInt(lsProp);
  Except
    liProp := 0
  End;
  Try
    liVal := StrToInt(Value);
  Except
    liVal := 0
  End;
  Case FilterOperator Of
    foEqual : Result := (liProp = liVal);
    foNotEqual : Result := (liProp <> liVal);
    foGreater : Result := (liProp > liVal);
    foGreaterEqual : Result := (liProp >= liVal);
    foLess : Result := (liProp < liVal);
    foLessEqual : Result := (liProp <= liVal);
  Else
    Raise Exception.Create('Invalid operator passed to IntPropPasses');
  End;
End;

Function TLVFilter.PassesFilter(pData : TtiObject) : Boolean;
Var
  lPropType : TvtTypeKind;
Begin
  lPropType := GetSimplePropType(pData, FFieldName);
  Case lPropType Of
    vttkInt : Result := IntPropPasses(pData);
    vttkFloat : Result := FloatPropPasses(pData);
    vttkString : Result := StringPropPasses(pData);
  Else
    Raise Exception.Create('Invalid property type passed to TVFilter.PassesFilter');
  End;
End;

Function TLVFilter.StringPropPasses(pValue : TtiObject) : Boolean;
Var
  lsProp : String;
  lsVal : String;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from StringPropPasses', [FFieldName, E.Message]);
  End;
  lsVal := UpperCase(Value);
  Case FilterOperator Of
    foEqual : Result := (lsProp = lsVal);
    foNotEqual : Result := (lsProp <> lsVal);
    foGreater : Result := (lsProp > lsVal);
    foGreaterEqual : Result := (lsProp >= lsVal);
    foLess : Result := (lsProp < lsVal);
    foLessEqual : Result := (lsProp <= lsVal);
    foContains : Result := WildcardMatch(lsProp, '*' + lsVal + '*', True);
    foNotContains : Result := (Not WildcardMatch(lsProp, '*' + lsVal + '*', True));
  Else
    Raise Exception.Create('Invalid operator passed to TestStringFilter');
  End;
End;

Function FindPart(Const pHelpWilds, pInputStr : String) : Integer;
Var
  I, J : Integer;
  Diff : Integer;
Begin
  I := Pos('?', pHelpWilds);
  If I = 0 Then
  Begin
    { if no '?' in pHelpWilds }
    Result := Pos(pHelpWilds, pInputStr);
    Exit;
  End;
  { '?' in pHelpWilds }
  Diff := Length(pInputStr) - Length(pHelpWilds);
  If Diff < 0 Then
  Begin
    Result := 0;
    Exit;
  End;
  { now move pHelpWilds over pInputStr }
  For I := 0 To Diff Do
  Begin
    For J := 1 To Length(pHelpWilds) Do
    Begin
      If (pInputStr[I + J] = pHelpWilds[J]) Or
        (pHelpWilds[J] = '?') Then
      Begin
        If J = Length(pHelpWilds) Then
        Begin
          Result := I + 1;
          Exit;
        End;
      End
      Else
        Break;
    End;
  End;
  Result := 0;
End;

Function TLVFilter.WildCardMatch(pInputStr, pWilds : String; pIgnoreCase : Boolean) : Boolean;

  Function SearchNext(Var pWilds : String) : Integer;
 { looking for next *, returns position and string until position }
  Begin
    Result := Pos('*', pWilds);
    If Result > 0 Then
      pWilds := Copy(pWilds, 1, Result - 1);
  End;

Var
  CWild, CInputWord : Integer; { counter for positions }
  I, LenHelpWilds : Integer;
  MaxInputWord, MaxWilds : Integer; { Length of pInputStr and pWilds }
  HelpWilds : String;
Begin
  If pWilds = pInputStr Then
  Begin
    Result := True;
    Exit;
  End;
  Repeat { delete '**', because '**' = '*' }
    I := Pos('**', pWilds);
    If I > 0 Then
      pWilds := Copy(pWilds, 1, I - 1) + '*' + Copy(pWilds, I + 2, MaxInt);
  Until I = 0;
  If pWilds = '*' Then
  Begin { for fast end, if pWilds only '*' }
    Result := True;
    Exit;
  End;
  MaxInputWord := Length(pInputStr);
  MaxWilds := Length(pWilds);
  If pIgnoreCase Then
  Begin { upcase all letters }
    pInputStr := AnsiUpperCase(pInputStr);
    pWilds := AnsiUpperCase(pWilds);
  End;
  If (MaxWilds = 0) Or (MaxInputWord = 0) Then
  Begin
    Result := False;
    Exit;
  End;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  Repeat
    If pInputStr[CInputWord] = pWilds[CWild] Then
    Begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    End;
    If pWilds[CWild] = '?' Then
    Begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    End;
    If pWilds[CWild] = '*' Then
    Begin { handling of '*' }
      HelpWilds := Copy(pWilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      If I = 0 Then
      Begin
        { no '*' in the rest, compare the ends }
        If HelpWilds = '' Then
          Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        For I := 0 To LenHelpWilds - 1 Do
        Begin
          If (HelpWilds[LenHelpWilds - I] <> pInputStr[MaxInputWord - I]) And
            (HelpWilds[LenHelpWilds - I] <> '?') Then
          Begin
            Result := False;
            Exit;
          End;
        End; { Loop }
        Exit;
      End;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(pInputStr, CInputWord, MaxInt));
      If I = 0 Then
      Begin
        Result := False;
        Exit;
      End;
      CInputWord := I + LenHelpWilds;
      Continue;
    End;
    Result := False;
    Exit;
  Until (CInputWord > MaxInputWord) Or (CWild > MaxWilds);
  { no completed evaluation }
  If CInputWord <= MaxInputWord Then
    Result := False;
  If (CWild <= MaxWilds) And (pWilds[MaxWilds] <> '*') Then
    Result := False;
End;


initialization

  tiVTExportRegistry.RegisterExport(TTiVTExportClipboard);

finalization

  utiVTExportRegistry.Free;

{$ENDIF}

end.

