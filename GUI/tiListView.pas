{$I tiDefines.inc}

unit tiListView;

interface
uses
{$IFNDEF FPC}
   Windows
  ,Messages
{$ELSE}
  LMessages
  ,LCLIntf
  ,LCLProc
{$ENDIF}
  ,tiFocusPanel
  ,tiSpeedButton
  ,tiCtrlButtonPanel
  ,tiObject
  ,ComCtrls
  ,Classes
  ,TypInfo
  ,Controls
  ,Menus
  ,Contnrs
  ,ExtCtrls
  ,Graphics
  ,ImgList
  ,Buttons
 ;

const

  crsDefaultColDisplayLabel = 'Caption';
  crsDefaultColFieldName    = 'Caption';

  // Type kinds for use with tiGetProperty
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString ];
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 ];
  // Float type properties
  ctkFloat  = [ tkFloat ];
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat;

type

  TlvTypeKind = (lvtkString, lvtkInt, lvtkFloat, lvtkDateTime, lvtkCurrency);

  TtiCustomListView = class;
  TtiListColumn = class;

  TtiLVOnFilterDataEvent  = procedure(AData  : TtiObject; var pbInclude : boolean) of object;
  TtiLVGetImageIndexEvent = procedure(AData  : TtiObject; var pImageIndex : integer) of object;
  TtiLVEvent              = procedure(pLV : TtiCustomListView) of object;
  TtiLVItemEvent          = procedure(pLV : TtiCustomListView; AData : TtiObject; AItem : TListItem) of object;
  TtiLVItemEditEvent      = procedure(pLV : TtiCustomListView; AData : TtiObject; AItem : TListItem ) of object;
  TtiLVOnGetFont          = procedure(pLV : TtiCustomListView;
                                       pCanvas : TCanvas;
                                       AItem  : TListItem;
                                       AData  : TtiObject) of object;
  TtiLVOnClick            = procedure(pLV : TtiCustomListView;
                                       AItem  : TListItem  ;
                                       AData  : TtiObject;
                                       pColumn : TListColumn) of object;
  TtiLVOnMoveItem         = procedure(AList      : TList;
                                       pDataMove  : TtiObject;
                                       pDataBefore : TtiObject;
                                       pDataAfter : TtiObject) of object;
  TtiLVCanPerformAction   = procedure(pLV : TtiCustomListView; AData : TtiObject; AItem : TListItem; var pbCanPerformAction : boolean) of object;

  TtiLVDragDropEvent    = procedure(ptiLVSource      : TtiCustomListView;
                                     pDataSource      : TtiObject      ;
                                     ptiLVTarget      : TtiCustomListView;
                                     pDataTarget      : TtiObject) of object;

  TtiLVDragDropConfirmEvent = procedure(ptiTLSource      : TtiCustomListView  ;
                                         pDataSource      : TtiObject        ;
                                         ptiTLTarget      : TtiCustomListView  ;
                                         pDataTarget      : TtiObject        ;
                                     var pbConfirm : boolean) of object;

  TtiDeriveListColumnValue = procedure(const pLV : TtiCustomListView;
                                        const AData : TtiObject;
                                        const ptiListColumn : TtiListColumn;
                                        var   pResult : string) of object;

  TtiOnGetCaptionEvent = procedure(const pLV : TtiCustomListView;
                                        const AData : TtiObject;
                                        const ptiListColumn : TtiListColumn;
                                        var   pResult : string) of object;

  TtiRuntimeGenColEvent = procedure (const AColName : string;
                                      var pAdd : boolean) of object;

  TtiLVInfoTipEvent = procedure (const pLV: TtiCustomListView;
                                 const AData: TtiObject;
                                 const AItem: TListItem;
                                 var pInfoTip: string) of object;

  TtiLVInfoTypeType = (itNone, itDefault, itCustom);

  TtiLVDragObject = class(TDragObject)
  private
    FItem: TListItem;
    FData: TtiObject;
    FtiListView: TtiCustomListView;
  public
    property  tiListView : TtiCustomListView read FtiListView write FtiListView;
    property  Data      : TtiObject       read FData       write FData      ;
    property  Item      : TListItem         read FItem       write FItem      ;
  end;

  // ToDo:
  // There is some cross validation between properties done in the Set methods
  // The F??? values are set, but this means the property inspector is not
  // updated with the new value at design time. Notification of the changes
  // should be send to the property inspector.
  TtiListColumn = class(TCollectionItem)
  private
    FsFieldName: string;
    FsDisplayLabel: string;
    FsDisplayMask: string;
    FDataType: TlvTypeKind;
    FDerived: boolean;
    FOnDeriveColumn: TtiDeriveListColumnValue;
    FOnGetCaption : TtiOnGetCaptionEvent;
    FWidth: integer;
    FAlignment: TAlignment;
    procedure   SetFieldName(const AValue: string);
    procedure   SetDataType(const AValue: TlvTypeKind);
    procedure   SetDerived(const AValue: boolean);
    procedure   SetOnDeriveColumn(const AValue: TtiDeriveListColumnValue);
    procedure   SetWidth(const AValue: integer);
  protected
    function    GetDisplayName : string; override;
  published
    property    DisplayLabel : string read FsDisplayLabel write FsDisplayLabel;
    property    FieldName   : string read FsFieldName    write SetFieldName;
    property    DisplayMask : string read FsDisplayMask  write FsDisplayMask;
    property    DataType    : TlvTypeKind read FDataType write SetDataType;
    property    Derived     : boolean read FDerived      write SetDerived;
    property    OnDeriveColumn : TtiDeriveListColumnValue read FOnDeriveColumn write SetOnDeriveColumn;
    Property    OnGetCaption : TtiOnGetCaptionEvent Read FOnGetCaption Write FOnGetCaption;
    property    Width : integer read FWidth write SetWidth default -1;
    property    Alignment : TAlignment read FAlignment write FAlignment default taRightJustify;
  public
    constructor Create(ACollection : TCollection); override;
    destructor  Destroy; override;
    function    Clone : TtiListColumn;
  end;

  TtiListColumns = class(TCollection)
  private
    FOwner : TComponent;
    function  GetItem(Index : integer): TtiListColumn;
    procedure SetItem(Index : integer; const AValue : TtiListColumn);
  published
  protected
    function  GetOwner : TPersistent; override;
  public
    constructor Create(AOwner : TComponent);
    destructor  Destroy; override;
    property  Items[Index: integer ]: TtiListColumn
                read GetItem write SetItem;
    function  Add : TtiListColumn;
    procedure DisplayLabelsToStringList(pSL : TStringList);
    function  FindByDisplayLabel(const AValue : string): TtiListColumn;
    procedure Clear; reintroduce;
  end;

  TlvSortDirection = (lvsdAscending, lvsdDescending);

  TlvSortOrder = class(TCollectionItem)
  private
    FsFieldName: string;
    FSortDirection: TlvSortDirection;
  protected
    function GetDisplayName : string; override;
  published
    property FieldName : string read FsFieldName write FsFieldName;
    property SortDirection : TlvSortDirection read FSortDirection write FSortDirection;
  public
    constructor Create(ACollection : TCollection); override;
    procedure   Assign(source : TPersistent); override;
  end;

  TlvSortOrders = class(TCollection)
  private
    FOwner : TComponent;
    function  GetItem(Index : integer): TlvSortOrder;
    procedure SetItem(Index : integer; const AValue : TlvSortOrder);
  published
  protected
    function  GetOwner : TPersistent; override;
  public
    constructor Create(AOwner : TComponent);
    property    Items[Index: integer ]: TlvSortOrder
                  read GetItem write SetItem;
    function    Add : TlvSortOrder;
  end;

  TtiLVNullDataRow = class(TtiObject)
  private
    FsNoData: string;
  published
    property NoData : string read FsNoData;
  public
    constructor Create; override;
  end;

  TtiLVNullData = Class(TObjectList)
  public
    constructor Create;
  end;

  TtiListViewInternal = class(TListView)
  private
    FOnVScroll: TNotifyEvent;
    FVScrolled : boolean;
    {$IFNDEF FPC}
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    {$ELSE}
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    {$ENDIF}
  public
    constructor Create(AOwner : TComponent); override;
    property    OnVScroll : TNotifyEvent read FOnVScroll write FOnVScroll;
    property    VScrolled : boolean read FVScrolled write FVScrolled;
  end;

  TtiCustomListView = class(TtiFocusPanel)
  private
    FLV          : TtiListViewInternal;
    FData        : TList;
    FDataInternal : TList;
    FSelectedData : TList;
    FNullData    : TtiLVNullData;

    FCols : TStringList;

    FOnDblClick  : TtiLVItemEditEvent;

    FOnFilterData : TtiLVOnFilterDataEvent;
    FOnGetImageIndex: TtiLVGetImageIndexEvent;
    FbLoading : boolean;

    FtiListColumns : TtiListColumns;
    FbRunTimeGenCols : boolean;
    FbApplyFilter: boolean;
    FbApplySort: boolean;
    FSortOrders: TlvSortOrders;
    FsLastSortField : string;
    FsSortField : string;
    FLastSortDirection : TlvSortDirection;
    FbSortOnHeadingClick: boolean;
    FListColsBeingDisplayed : TObjectList;
    FbRunTimeSelectedCols: boolean;
    FAfterRefreshData: TtiLVEvent;
    FTimerSort : TTimer;
    FOnGetFont: TtiLVOnGetFont;
    FOnItemArive: TtiLVItemEvent;
    FOnItemLeave: TtiLVItemEvent;
    FbSelectFirstRow: boolean;
    FOnLVClick: TtiLVOnClick;
    FbProcessingOnClick : boolean;
    FLastItem : TListItem;
    FOnMoveItem: TtiLVOnMoveItem;
    FOnCanInsert : TtiLVCanPerformAction;
    FOnCanEdit  : TtiLVCanPerformAction;
    FOnCanDelete : TtiLVCanPerformAction;
    FOnCanAcceptDrop: TtiLVDragDropConfirmEvent;
    FOnDrop: TtiLVDragDropEvent;
    FCanStartDrag: boolean;
    FReadOnly : boolean;
    FOnRuntimeGenCol: TtiRuntimeGenColEvent;
    FDestroying : boolean;
    FOnTipInfo: TtiLVInfoTipEvent;
    FInfoTipType: TtiLVInfoTypeType;

    FOnCanView: TtiLVCanPerformAction;

    procedure GetPropertyNames(pPersistent : TObject;
                                pSL : TStringList;
                                APropFilter : TTypeKinds = ctkSimple);

    function  GetSimplePropType(pPersistent: TtiObject;
                                 APropName: string): TlvTypeKind;

    procedure OnGetRowData(Sender: TObject; Item: TListItem);

    procedure SetApplySort(const AValue: boolean);
    procedure ConnectToData;
    procedure DisConnectFromData;
    procedure DoOnTimerSort(Sender : TObject);
    procedure DoColumnClick(Sender: TObject; Column: TListColumn);
    procedure DoSortColumn;
    procedure DoCustomDrawItem(Sender : TCustomListView;
                                Item  : TListItem;
                                State : TCustomDrawState;
                                var DefaultDraw: Boolean);
    function  GetSelectedDataList : TList;
    function  GetMultiSelect: boolean;
    procedure SetMultiSelect(const AValue: boolean);
    function  GetOnChange: TLVChangeEvent;
    procedure SetOnChange(const AValue: TLVChangeEvent);
    {$IFNDEF FPC}
    function  GetOnChanging: TLVChangingEvent;
    procedure SetOnChanging(const AValue: TLVChangingEvent);
    {$ENDIF}
    function  GetOnKeyDown: TKeyEvent;
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    function  GetOnKeyPress: TKeyPressEvent;
    procedure SetOnKeyPress(const AValue: TKeyPressEvent);
    function  GetSmallImages: TCustomImageList;
    procedure SetSmallImages(const AValue: TCustomImageList);
    function  GetViewStyle: TViewStyle;
    procedure SetViewStyle(const AValue: TViewStyle);
    function  GetRowSelect: Boolean;
    procedure SetRowSelect(const AValue: Boolean);
    function  GetItems: TListItems;
    function  GetColumns: TListColumns;
    function  GetSelected: TListItem;
    function  GetListView: TListView;
    procedure SetRunTimeGenCols(const AValue: boolean);
    function  GetSelectedData: TtiObject;
    function  GetSelectedIndex: integer;
    procedure SetSelectedIndex(const AValue: integer);

    // Drag & Drop Sender side events
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    procedure DoStartDrag(Sender: TObject; var DragObject: TDragObject); Reintroduce;

    // Drag & Drop Receiver side events
    procedure DoDragOver( Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
    procedure DoDragDrop( Sender, Source: TObject; X, Y: Integer);
    procedure SetSelectedData(const AValue: TtiObject);
    function  GetOnVScroll: TNotifyEvent;
    procedure SetOnVScroll(const AValue: TNotifyEvent);
    function  AreColsDifferent: boolean;
    function  GetColWidth(pListColumn: TtiListColumn; pLVColumn: TListColumn): Integer;
    procedure DoOnPlnClick(Sender: TObject);
    procedure SetOnInfoTip(const AValue: TtiLVInfoTipEvent);
    procedure SetInfoTipType(const AValue: TtiLVInfoTypeType);
    procedure DoTipInfo(Sender: TObject; Item: TListItem; var InfoTip: string);
  protected

    function    GetDefaultInfoTip(AIndex: Integer): string; virtual;
    procedure   SetData(const AValue: TList); virtual;
    procedure   SetupCols; virtual;
    procedure   DoRunTimeGenCols;
    procedure   Loaded; override;
    procedure   DoApplyFilter; virtual;
    procedure   DoApplySort  ; virtual;
    procedure   DoReSize(Sender : TObject); virtual;
    procedure   DoOnClick(Sender: TObject); override;

    procedure   GetColWidths;
    procedure   DoAddColumn(pListColumn: TtiListColumn);
    procedure   AddImageColumn;
    procedure   SetApplyFilter(const AValue: boolean); virtual;
    function    GetReadOnly: boolean; virtual;
    procedure   SetReadOnly(const AValue: boolean); virtual;
    procedure   DoOnSelectItem(Sender : TObject; Item : TListItem; Selected : boolean); virtual;
    function    ValueFromProperty(ptiListColumn : TtiListColumn; AData : TtiObject): string;
    function    ValueFromDerivedProperty(ptiListColumn : TtiListColumn; AData : TtiObject): string;
    function    GetCellText(const piData, piCol: integer): string;
    procedure   SetApplySortNoRefresh(const AValue, pRefresh: boolean);
    procedure   SetLVColor;

    property    DataInternal : TList read FDataInternal;
    property    RunTimeGenCols : boolean
                  read  FbRunTimeGenCols
                  write SetRunTimeGenCols default true;

    property    Align        ;
    property    Anchors      ;
    property    Constraints  ;
    property    Visible      ;

    property    MultiSelect  : boolean read GetMultiSelect write SetMultiSelect;
    property    OnDblClick   : TtiLVItemEditEvent read FOnDblClick write FOnDblClick;
    property    OnChange     : TLVChangeEvent read GetOnChange write SetOnChange;
    {$IFNDEF FPC}
    property    OnChanging   : TLVChangingEvent read GetOnChanging write SetOnChanging;
    {$ENDIF}
    property    OnKeyDown    : TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property    OnKeyPress   : TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property    SmallImages  : TCustomImageList read GetSmallImages write SetSmallImages;
    property    ViewStyle    : TViewStyle read GetViewStyle write SetViewStyle;
    property    RowSelect    : Boolean read GetRowSelect write SetRowSelect;

    // These three properties are needed for drag-and-drop
    //property    OnDragOver   : TDragOverEvent read GetOnDragOver  write SetOnDragOver;
    //property    OnDragDrop   : TDragDropEvent read GetOnDragDrop  write SetOnDragDrop;
    //property    OnMouseDown  : TMouseEvent    read GetOnMouseDown write SetOnMouseDown;

    property    OnLVClick : TtiLVOnClick
                  read FOnLVClick
                  write FOnLVClick;

    property    OnFilterData : TtiLVOnFilterDataEvent
                  read  FOnFilterData
                  write FOnFilterData;
    property    OnGetFont : TtiLVOnGetFont
                  read  FOnGetFont
                  write FOnGetFont;

    property    OnGetImageIndex : TtiLVGetImageIndexEvent
                  read FOnGetImageIndex
                  write FOnGetImageIndex;

    property     SortOrders : TlvSortOrders
                  read FSortOrders
                  write FSortOrders;
    property    SortOnHeadingClick : boolean
                  read FbSortOnHeadingClick
                  write FbSortOnHeadingClick default true;
    property    AfterRefreshData : TtiLVEvent
                  read  FAfterRefreshData
                  write FAfterRefreshData;
    property    OnItemArive : TtiLVItemEvent
                  read FOnItemArive
                  write FOnItemArive;
    property    OnItemLeave : TtiLVItemEvent
                  read FOnItemLeave
                  write FOnItemLeave;
    property    SelectFirstRow : boolean read FbSelectFirstRow write FbSelectFirstRow default true;
    property    OnMoveItem : TtiLVOnMoveItem
                  read  FOnMoveItem
                  write FOnMoveItem;

    procedure   SetEnabled(AValue : Boolean); override;
    procedure   DoOnDblClick(Sender : TObject); virtual;

    property    OnCanView  : TtiLVCanPerformAction read FOnCanView   write FOnCanView;
    property    OnCanDelete : TtiLVCanPerformAction read FOnCanDelete write FOnCanDelete;
    property    OnCanInsert : TtiLVCanPerformAction read FOnCanInsert write FOnCanInsert;
    property    OnCanEdit  : TtiLVCanPerformAction read FOnCanEdit   write FOnCanEdit;

    property    CanStartDrag : boolean read FCanStartDrag write FCanStartDrag;
    property    OnDrop          : TtiLVDragDropEvent        read FOnDrop        write FOnDrop       ;
    property    OnCanAcceptDrop : TtiLVDragDropConfirmEvent read FOnCanAcceptDrop write FOnCanAcceptDrop;

    property    ReadOnly : boolean read GetReadOnly write SetReadOnly default false;
    property    OnVScroll : TNotifyEvent read GetOnVScroll write SetOnVScroll;
    property    OnRuntimeGenCol : TtiRuntimeGenColEvent read FOnRuntimeGenCol write FOnRuntimeGenCol;
    property    OnInfoTip : TtiLVInfoTipEvent read FOnTipInfo   write SetOnInfoTip;
    property    InfoTypeType: TtiLVInfoTypeType read FInfoTipType Write SetInfoTipType;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Refresh(pReadData : boolean = true); reintroduce;

    procedure   PositionCursor(AIndex : integer    ); overload;
    procedure   PositionCursor(AData  : TtiObject); overload;
    procedure   Last;
    procedure   First;
    procedure   MoveSelection(piMoveBy : integer);

    function    CanView    : boolean; virtual;
    function    CanInsert  : boolean; virtual;
    function    CanEdit    : boolean; virtual;
    function    CanDelete  : boolean; virtual;
    function    CanMoveItem(AItem : TListItem;
                             piMoveBy : integer): boolean;
    procedure   MoveItem(AItem   : TListItem;
                          piMoveBy : integer);

    property    ApplyFilter : boolean read FbApplyFilter write SetApplyFilter;
    property    ApplySort  : boolean read FbApplySort   write SetApplySort;

    property    Data : TList read FData write SetData;
    property    Items : TListItems read GetItems;

    // A ref to the underlying listview.columns property
    property    Columns    : TListColumns   read GetColumns;
    // The available col mappings
    property    ListColumns : TtiListColumns read FtiListColumns write FtiListColumns;
    // The col mappings currently being viewed, contains 0..n of TtiListColumn
    // Note: Might contain nil to allow for first col to be an image
    property    ListColsBeingDisplayed : TObjectList    read FListColsBeingDisplayed;

    property    RunTimeSelectedCols : boolean
                  read FbRunTimeSelectedCols
                  write FbRunTimeSelectedCols;

    property    SelectedDataList : TList read GetSelectedDataList;
    property    SelectedData : TtiObject read GetSelectedData write SetSelectedData;
    property    Selected : TListItem read GetSelected;
    property    SelectedIndex : integer read GetSelectedIndex write SetSelectedIndex;

    // Delete a list item, along with the data it points to
    procedure   DeleteData(pListItem : TListItem);

    // This is necessary for drag and drop, and should be replaced by a
    // 'Simple' drop method.
    {$IFNDEF FPC}
    function    GetItemAt(X, Y : Integer): TListItem;
    {$ENDIF}
    procedure   SetFocus; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   SynchroniseWith(pListView : TtiCustomListView);
    function    AddColumn(const AFieldName : string;
                           const pDataType : TlvTypeKind;
                           const pDisplayLabel : string = '';
                           pColWidth : integer = -1): TtiListColumn;
    procedure   ClearColumns; virtual;
  published

    Property ShowFocusRect; // AD Jul 2003.

    property    LV : TListView read GetListView;

  end;

  TtiListView = class(TtiCustomListView)
  private
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs;
    FOnItemEdit  : TtiLVItemEditEvent;
    FOnItemInsert : TtiLVItemEditEvent;
    FOnItemDelete : TtiLVItemEditEvent;
    FOnItemView  : TtiLVItemEditEvent;
    FPopupMenu : TPopupMenu;
    FpmiView  : TMenuItem;
    FpmiEdit  : TMenuItem;
    FpmiNew   : TMenuItem;
    FpmiDelete : TMenuItem;
    procedure SetOnItemDelete(const AValue: TtiLVItemEditEvent);
    procedure SetOnItemEdit(const AValue: TtiLVItemEditEvent);
    procedure SetOnItemInsert(const AValue: TtiLVItemEditEvent);
    procedure SetOnItemView(const AValue: TtiLVItemEditEvent);
    function  GetButtonStyle: TLVButtonStyle;
    procedure SetButtonStyle(const AValue: TLVButtonStyle);
    function  GetVisibleButtons: TtiLVVisibleButtons;
    procedure SetVisibleButtons(const AValue: TtiLVVisibleButtons);
  protected
    procedure   DoEnter; override;
    procedure   DoExit; override;
    procedure   DoReSize(Sender : TObject); override;
    procedure   MenuOnPopup(sender : TObject);
    procedure   pmiViewOnClick(sender : TObject);
    procedure   pmiEditOnClick(sender : TObject);
    procedure   pmiDeleteOnClick(sender : TObject);
    procedure   pmiNewOnClick(sender : TObject);
    procedure   DoOnSelectItem(Sender: TObject; Item: TListItem; ItmSelected: boolean); override;
    procedure   SetEnabled(AValue: Boolean); override;
    procedure   SetReadOnly(const AValue: boolean); override;
    procedure   DoOnDblClick(Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    property    OnVScroll;
    procedure   Loaded; override;
    procedure   SetData(const AValue: TList); override;
    procedure   DoDelete(Sender: TObject);
    procedure   DoEdit(Sender: TObject);
    procedure   DoNew(Sender: TObject);
    procedure   DoView(Sender: TObject);
    function    CanView    : boolean; override;
    function    CanInsert  : boolean; override;
    function    CanEdit    : boolean; override;
    function    CanDelete  : boolean; override;

  published

    property    ButtonStyle : TLVButtonStyle read GetButtonStyle write SetButtonStyle default lvbsMicroButtons;
    property    VisibleButtons : TtiLVVisibleButtons read GetVisibleButtons write SetVisibleButtons default [];

    property    OnItemView  : TtiLVItemEditEvent read FOnItemView   write SetOnItemView  ;
    property    OnItemEdit  : TtiLVItemEditEvent read FOnItemEdit   write SetOnItemEdit  ;
    property    OnItemInsert : TtiLVItemEditEvent read FOnItemInsert Write SetOnItemInsert;
    property    OnItemDelete : TtiLVItemEditEvent read FOnItemDelete Write SetOnItemDelete;

    property    Align        ;
    property    Anchors      ;
    property    MultiSelect  ;
    property    OnDblClick   ;
    property    OnChange     ;
   {$IFNDEF FPC} property    OnChanging   ;{$ENDIF}
    property    OnKeyDown    ;
    property    OnKeyPress   ;
    property    SmallImages  ;
    property    ViewStyle    ;
    property    RowSelect    ;
    property    Constraints  ;
    property    Visible      ;
    property    OnEnter      ;
    property    OnExit       ;

    // These three properties are needed for drag-and-drop
    //property    OnDragOver ;
    //property    OnDragDrop ;
    //property    OnMouseDown;

    property    OnLVClick;

    property    OnFilterData;
    property    OnGetFont;

    property    ApplyFilter;
    property    ApplySort  ;

    property    OnGetImageIndex;

    property    ListColumns;
    property    SortOrders;
    property    SortOnHeadingClick;
    property    AfterRefreshData;
    property    OnItemArive;
    property    OnItemLeave;
    property    SelectFirstRow;
    property    OnMoveItem;
    property    RuntimeGenCols;

    property    OnCanView;
    property    OnCanDelete;
    property    OnCanInsert;
    property    OnCanEdit  ;

    property    CanStartDrag    ;
    property    OnDrop          ;
    property    OnCanAcceptDrop ;
    property    ReadOnly;
    property    OnRuntimeGenCol ;
    property    OnInfoTip;
    property    InfoTypeType;

  end;

function tiListViewDisplayMaskFromDataType(const AValue : TlvTypeKind): string;

implementation
uses
   SysUtils
   ,Math
   ,Forms
   ,Dialogs
   ,tiLog
   ,tiUtils
   ,tiImageMgr
   ,tiResources
   ,tiExcept
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}

//   ,tiLog // Debugging
  ;

var
  // Why do I need a unit wide uSortOrders?
  // Because there is no other way that I can think of to get
  // some data to control the sorting into TList.Sort.
  uSortOrders : TlvSortOrders;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiCustomListView
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCustomListView.Create(AOwner: TComponent);
const
  cHeight = 97 ;
  cWidth  = 121;
begin
  inherited Create(AOwner);
  FDestroying := false;

  // The listView must be able to AcceptControls to work correctly.
  ControlStyle := ControlStyle + [csAcceptsControls];
  self.BevelInner := bvNone;
  self.BevelOuter := bvNone;
  Height  := cHeight ;
  Width   := cWidth;
  OnClick := DoOnPlnClick;
  OnResize := DoResize;

  FLV            := TtiListViewInternal.Create(self);
  FLV.Parent     := self;
  FLV.ParentFont := true;
  FLV.TabStop    := True;
  //FLV.Anchors    := [akLeft,akTop,akRight,akBottom];

  FLV.OnMouseDown := DoMouseDown;
  {$IFNDEF FPC}
   FLV.OnStartDrag := DoStartDrag;
  {$ELSE}
   {$WARNING TODO: Fix it when LCL will have better drag&drop support}
  {$ENDIF}
  FLV.OnDragOver := DoDragOver ;
  FLV.OnDragDrop := DoDragDrop ;
  FLV.OnDblClick := DoOnDblClick;
  FLV.SetBounds(
    1,
    1,
    Width - 2,
    Height - 2);

  FCanStartDrag  := false;

  FDataInternal := TList.Create;
  FListColsBeingDisplayed  := TObjectList.Create;
  FListColsBeingDisplayed.OwnsObjects := true;

  FbLoading := true;
  FbRunTimeGenCols := true;
  FbRunTimeSelectedCols := false;

  FCols    := TStringList.Create;

  // Configure the list view
  FLV.ReadOnly        := true;
  FLV.HideSelection   := false;
  FLV.ViewStyle       := vsReport;
  FLV.RowSelect       := true;
  FLV.ParentFont      := false;

  FtiListColumns := TtiListColumns.Create(self);

  FsLastSortField := '';
  FLastSortDirection := lvsdAscending;

  FSortOrders := TlvSortOrders.Create(self);
  FLV.OnColumnClick := DoColumnClick;
  FbSortOnHeadingClick := true;

  FLV.OnSelectItem := DoOnSelectItem;

  // It is somtimes possible that the ListView common control will cause
  // an access violation within the windows dll while re-painting it self.
  // This has been worked around by using a TTimer to pass a message to the
  // control that it must sort the data as the result of a column click, then
  // repaint it self. This functionality would probably have been better
  // achieved by defining a custom message type and sending that to trigger
  // the repaint process.
  FTimerSort := TTimer.Create(nil);
  FTimerSort.Enabled := false;
  FTimerSort.Interval := 10;
  FTimerSort.OnTimer := DoOnTimerSort;

  // Likewise, when the OnGetFont property is set, somtimes, the font of the
  // first row of data in the listView is set to bold. I can't work out why, or
  // or how to reproduce this but a timer which triggers a move of the cursor
  // 15ms after the data property has been set works around the problem.
//  FTimerSetData := TTimer.Create(nil);
//  FTimerSetData.Enabled := false;
//  FTimerSetData.Interval := 15;
//  FTimerSetData.OnTimer := DoOnTimerSetData;

  // A TList to hold pointer to the data objects associated with selected nodes
  FSelectedData := TList.Create;

  FNullData := TtiLVNullData.Create;
  //Data     := FNullData;

  FbSelectFirstRow := true;

  FLV.OnClick := DoOnClick;

  FLastItem := nil;

  //FbProcessingOnClick := false;
  //FLV.OnMouseDown := DoOnMouseDown;
//  FLV.OnMouseDown  := DoOnMouseUp;
  //FLV.OnClick := DoMouseClick;
end;

destructor TtiCustomListView.Destroy;
begin
  FDestroying := true;
  FData := nil;
  FLV.Free;
  FCols.Free;
  FtiListColumns.Free;
  FDataInternal.Free;
  FSortOrders.Free;
  FListColsBeingDisplayed.Free;
  FTimerSort.Free;
  FSelectedData.Free;
  FNullData.Free;
  inherited;
end;

procedure TtiCustomListView.Loaded;
begin
  inherited;
  if FbLoading then begin
    // NilData is causing problem with design time cols
    // Data := FNilData;
    //FbRunTimeGenCols := FtiListColumns.Count = 0;
    FbLoading := false;
  end;
  DoReSize(nil);
end;

procedure TtiCustomListView.OnGetRowData(Sender: TObject; Item: TListItem);
var
  i           : integer;
  lData       : TtiObject;
  liImageIndex : integer;
begin

  try
    if FData = nil then
      exit; //==>

    Assert(Item <> nil,  'Nil list item');
    Assert(FData <> nil, 'Data property not set');
    // Added this to fix a problem with PositionCursor(0) when there where
    // 3 items in the list. The root cause of the proble is unknown, and this
    // is a workaroudn.
    if FDataInternal.Count-1 < Item.Index then
      Exit; //==>
    Assert(FDataInternal.Count-1 >= Item.Index , 'Item.Index > FDataInternal.Count');
    lData := TtiObject(FDataInternal.Items[ Item.Index ]);

    for i := 0 to FtiListColumns.Count - 1 do
      Item.SubItems.Add('');

    if lData = nil then
      Exit; //==>

    Item.Data := lData;
    for i := 1 to Columns.Count - 1 do
      Item.SubItems[i-1]:=
        GetCellText(Item.Index, i-1);

    if Assigned(FOnGetImageIndex) and
       Assigned(SmallImages) then begin
      liImageIndex := -1;
      FOnGetImageIndex(lData, liImageIndex);
      Item.ImageIndex   := liImageIndex;
    end;
  except
    on e:exception do
      raise exception.Create('Error in ' + Name + ': ' + ClassName +
                              '.OnGetRowData: ' + e.message);
  end;
end;

procedure TtiCustomListView.SetData(const AValue: TList);
begin

  if FData = AValue then
    Exit; //==>

  DisConnectFromData;

  // NilData is causing problems with design time cols
  //  if AValue = nil then
  //    FData := FNilData
  //  else
  FData := AValue;

  if (FData = nil) or
     (FData.Count<1) then
  begin
    if not FDestroying then
    begin
      FDataInternal.Clear; // SetupCols refers to this so must reset now if no data
      Refresh(False); { This has been included in an attempt to force a repaint.  If readonly was *reset* (off)
                          and no items needed display, the readonly color would remain.  Also tried FLV.Invalidate
                          in SetColor to no avail.  Alas, the problem alludes me... ipk 2003-10-14}
      SetupCols;
    end;
    Exit; //==>
  end;

  Refresh;

  // Turned this off 31/08/2000. I remember fixing the problem in the TtiTreeView -
  // it was somthing to do with using the wrong canvas. Don't remember fixing it in the
  // TtiCustomListView, but it appears to have gone away, for the time begin at least...

  // Turned it back on 02/01/2001
  //FTimerSetData.Enabled := true;
  if FbSelectFirstRow then
    PositionCursor(0);

end;

procedure TtiCustomListView.refresh(pReadData : boolean = true);
var
  lData: TtiObject;
begin
  // Do not re-read the data, only refresh the current row.
  if not pReadData then begin
    FLV.Refresh;
    Exit; //==>
  end;

  lData := SelectedData;

  if (Selected <> nil) and
     Assigned(FOnItemLeave) then
    FOnItemLeave(Self, lData, Selected);

  {$IFNDEF FPC}FLV.OnData   := nil;{$ENDIF}
  BeginUpdate;
  try
    DisConnectFromData;
    DoApplyFilter;
    DoApplySort;
    ConnectToData;
    RePaint;
    if Assigned(FAfterRefreshData) then
      FAfterRefreshData(self);
    if lData <> nil then
      SelectedData := lData
    else if (Items.Count > 0) and SelectFirstRow then
      PositionCursor(0);
  finally
    EndUpdate;
  end;

end;

procedure TtiCustomListView.DisConnectFromData;
begin
  if {$IFNDEF FPC}(not Assigned(FLV.OnData)) and {$ENDIF}
     (not FLV.OwnerData) and
     (FLV.Items.Count = 0) then
    exit; //==>

  if FbRunTimeGenCols then
    FtiListColumns.Clear;

 {$IFNDEF FPC}
  FLV.OnCustomDrawItem := nil;
  FLV.OnData   := nil;
 {$ENDIF}
  FLV.OwnerData := false;
  //FLV.Columns.Clear;
  //ColMappings.Clear;
  FLV.Items.Clear;

end;

procedure TtiCustomListView.ConnectToData;
begin
//  if FDataInternal.Count > 0 then begin

    // The order of these three assignments is important.
    // Do not change the order.
    // Must set OwnerData, AutoGenCols, Items.Count, OnData order.

    FLV.OwnerData  := true;

    if FbRunTimeGenCols then
      DoRunTimeGenCols;

    SetupCols;
    {$IFNDEF FPC}
    Items.Count := FDataInternal.Count;
    FLV.OnData     := OnGetRowData;
    if Assigned(FOnGetFont) then
      FLV.OnCustomDrawItem :=  DoCustomDrawItem;
    {$ENDIF}
//  end;
end;

// There where no columns specified at design time, so setup the
// columns at runtime.
procedure TtiCustomListView.DoRunTimeGenCols;
var
  i : integer;
  lsColName : string;
  lCol : TtiListColumn;
  lData : TObject;
  lAdd : boolean;
begin

  ClearColumns;
  //FColMappings.Clear;
  //FtiListColumns.Clear;

  // Read all the available cols into the col string list
  if (Data = nil) or
     (Data.Count = 0) then
    Exit; //==>

  // Find the first, non-nil data item
  lData := nil;
  for i := 0 to Data.Count - 1 do
    if Data.Items[i] <> nil then
    begin
      lData := TObject(Data.Items[i]);
      Break; //==>
    end;

  Assert(lData is TtiObject, 'Not a list of TtiObject');

  GetPropertyNames(TtiObject(lData), FCols);

  // Read the column headings
  for i := 0 to FCols.count - 1 do
  begin
    lsColName := FCols.Strings[i];
    if Assigned(FOnRuntimeGenCol) then
    begin
       lAdd := true;
       FOnRuntimeGenCol(lsColName, lAdd);
       if not lAdd then
         Continue; //==>
    end;

    if (lsColName <> '') then
    begin
      lCol := FtiListColumns.Add;
      lCol.DisplayLabel := lsColName;
      lCol.FieldName   := lsColName;
      lCol.DataType    := GetSimplePropType(TtiObject(lData), lsColName);
    end;
  end;

end;

procedure TtiCustomListView.SetupCols;
var
  i : integer;
begin
  if not AreColsDifferent then begin
    GetColWidths;
    Exit; //==>
    end;
  FListColsBeingDisplayed.Clear;
  FLV.Columns.Clear;
  AddImageColumn;
  // Read the column headings from FtiListColumns
  for i := 0 to FtiListColumns.Count - 1 do
    DoAddColumn(FtiListColumns.Items[i]);

  GetColWidths;

end;

function TtiCustomListView.AreColsDifferent : boolean;
var
  i : integer;
begin
  result :=
    (FData = nil) or
    (FData.Count < 1) or
    (FtiListColumns.Count <> FLV.Columns.Count - 1) or
    (FListColsBeingDisplayed.Count <> FtiListColumns.Count) or
    (FtiListColumns.Count <> FLV.Columns.Count - 1);
  if result then
    Exit; //==>
  for i := 0 to FtiListColumns.Count - 1 do
  begin
    if (FLV.Columns[i+1].Alignment <> FtiListColumns.Items[i].Alignment) or
       (FLV.Columns[i+1].Caption   <> FtiListColumns.Items[i].DisplayLabel) then
    begin
      result := true;
      Exit; //==>
    end;
  end;
end;

procedure TtiCustomListView.AddImageColumn;
var
  lCol : TListColumn;
begin

  // Note: Col[0] will always be left justified, so we add a dummy col
  //       at [0] to hold the image only (if there is one)
  lCol := Columns.Add;
  lCol.Caption  := '';
  lCol.AutoSize := false;

  if Assigned(SmallImages) then
    Columns[Columns.Count-1].Width := SmallImages.Width + 2
  else
    Columns[Columns.Count-1].Width := 1;

  FListColsBeingDisplayed.Add(nil);

end;

procedure TtiCustomListView.DoAddColumn(pListColumn : TtiListColumn);
var
  lCol : TListColumn;
begin
  lCol          := Columns.Add;
  lCol.AutoSize := false;
  lCol.Alignment := pListColumn.Alignment;
  lCol.Caption  := pListColumn.DisplayLabel;
  lCol.Width    := GetColWidth(pListColumn, lCol);
  FListColsBeingDisplayed.Add(pListColumn.Clone);
end;

function TtiCustomListView.GetColWidth(pListColumn : TtiListColumn; pLVColumn : TListColumn):Integer;
begin
  if pListColumn.Width = -1 then
    result := Canvas.TextWidth(pLVColumn.Caption) + 12
  else
    result := pListColumn.Width;
end;

procedure TtiCustomListView.DoOnPlnClick(Sender: TObject);
begin
  if Enabled and (Not Focused) then
    SetFocus;
end;

procedure TtiCustomListView.GetColWidths;
var
  i : integer;
  j : integer;
  laWidths : Array of integer;
  liTextWidth : integer;
begin

  SetLength(laWidths, Columns.Count);

  // Read the heading widths (remember, the first col is a dummy for the image)
  for j := 1 to Columns.Count - 1 do begin
    if {not RunTimeGenCols and} FtiListColumns.Items[j-1].Width = -1 then
    begin
      liTextWidth := Canvas.TextWidth(Columns[j].Caption);
      laWidths[j]:= Max(laWidths[j], liTextWidth);
    end;
  end;

  // Read the data widths (remember, the first col is a dummy for the image)
  for i := 0 to FDataInternal.Count - 1 do
    for j := 1 to Columns.Count - 1 do
      if FtiListColumns.Items[j-1].Width = -1 then
        laWidths[j]:=
          Max(Canvas.TextWidth(GetCellText(i, j-1)), laWidths[j]);

  // Now set the column widths to match the calculated values.
  for j := 1 to Columns.Count - 1 do
    if {not RunTimeGenCols and} FtiListColumns.Items[j-1].Width = -1 then
      Columns[j].Width := laWidths[j] + 12
    else
      Columns[j].Width := FtiListColumns.Items[j-1].Width;

end;

function TtiCustomListView.ValueFromProperty(ptiListColumn : TtiListColumn; AData : TtiObject): string;
var
  lsFieldName  : string;
  lCurrency    : Currency;
  lFloat       : double;
  lInt         : integer;
  lDate        : TDateTime;
  lsDisplayMask : string;
begin
  if Assigned(pTiListColumn.FOnGetCaption) then
  begin
    try
      ptiListColumn.FOnGetCaption(Self, AData, ptiListColumn, Result);
    except
      On E : Exception Do
        Result := 'Error in OnGetCaption. Message : ' + E.Message;
    end;
  end
  else
  begin
    lsFieldName  := ptiListColumn.FieldName;
    lsDisplayMask := ptiListColumn.DisplayMask;
    try
      case ptiListColumn.DataType of
      lvtkString  : result := GetPropValue(AData, lsFieldName);
      lvtkCurrency : begin
                       lCurrency := GetPropValue(AData, lsFieldName);
                       Result := Format('%m',[lCurrency]);
                     end;
      lvtkFloat   : begin
                       lFloat := GetPropValue(AData, lsFieldName);
                       result := FormatFloat(lsDisplayMask, lFloat);
                     end;
      lvtkDateTime : begin
                       lDate := GetPropValue(AData, lsFieldName);
                       result := FormatDateTime(lsDisplayMask, lDate);
                     end;
      lvtkInt     : begin
                       lInt  := GetPropValue(AData, lsFieldName);
                       result := FormatFloat(lsDisplayMask, lInt);
                     end;
      end;
    except
      on e: exception do
        result := 'Error reading <' + lsFieldName +
                  '> in _ValueFromProperty. Message:' +
                  e.message;
    end;
  end;  { if }
end;


function TtiCustomListView.ValueFromDerivedProperty(ptiListColumn : TtiListColumn; AData : TtiObject): string;
begin
  try
    if Assigned(ptiListColumn.OnDeriveColumn) then
    begin
      result := '';
      ptiListColumn.OnDeriveColumn(
        Self,
        AData,
        ptiListColumn,
        result);
    end
    else
      result := 'Unknown';
  except
    on e:exception do
      result := 'Error reading column <' + ptiListColumn.DisplayLabel +
                '> in _ValueFromDerivedProperty. Message:' +
                e.message;
  end;
end;



function TtiCustomListView.GetCellText(const piData, piCol : integer): string;
var
  ltiListColumn : TtiListColumn;
  lData        : TtiObject;
begin

  result := '';
  Assert(piCol+2 <= FListColsBeingDisplayed.Count,
         'FListColsBeingDisplayed has ' + IntToStr(FListColsBeingDisplayed.Count) +
         ' items but trying to access index ' + IntToStr(piCol+1));
  ltiListColumn := TtiListColumn(FListColsBeingDisplayed.Items[piCol+1]);
  Assert(ltiListColumn <> nil, 'Can not find tiListColumn for index ' + IntToStr(piCol+1));
  lData        := TtiObject(FDataInternal.Items[piData]);
  if lData = nil then
    result := ''
  else if not ltiListColumn.Derived then
    result := ValueFromProperty(ltiListColumn, lData)
  else
    result := ValueFromDerivedProperty(ltiListColumn, lData);

end;

procedure TtiCustomListView.GetPropertyNames(pPersistent: TObject;pSL: TStringList;
                                        APropFilter: TTypeKinds);
var
  lCount : integer;
  lSize : integer;
  lList : PPropList;
  i : integer;
  lPropFilter : TTypeKinds;
begin
  Assert(pPersistent <> nil, 'pPersistent not assigned.');
  Assert(pSL <> nil, 'pSL not assigned.');
  lPropFilter := APropFilter;
  pSL.Clear;
  lCount := GetPropList(pPersistent.ClassInfo, lPropFilter, nil);
  lSize := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
    GetPropList(pPersistent.ClassInfo, lPropFilter, lList);
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

function TtiCustomListView.GetSimplePropType(pPersistent : TtiObject;
                                        APropName : string): TlvTypeKind;
var
  lPropType : TTypeKind;
begin

  try
    lPropType := PropType(pPersistent, APropName);
  except
    on e:exception do
      raise exception.Create('Error in tiGetSimpleTypeKind ' +
                              'Message: ' + e.message);
  end;

  case lPropType of
  tkInteger,
  tkInt64,
  tkEnumeration : result := lvtkInt;

  tkFloat      : result := lvtkFloat;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  tkWString    : result := lvtkString;

  else
    raise exception.Create('Invalid property type passed to ' +
                            'tiGetSimpleTypeKind');
  end;

end;


procedure TtiCustomListView.PositionCursor(AIndex : integer);
//var
//  liItemHeight : integer;
begin

  if (AIndex > Items.Count-1) or
     (AIndex < 0) then
    Exit; //==>

{
  liItemHeight := Canvas.TextHeight('M') + 1;
  FLV.Scroll(0, liItemHeight * FLV.TopItem.Index * -1);

  if AIndex > FLV.VisibleRowCount then
    FLV.Scroll(0, liItemHeight * AIndex);

  FLV.Selected := Items[AIndex];
  FLV.Selected.MakeVisible(false);
}

  FLV.Selected := nil;
  FLV.Selected := Items[AIndex];
  FLV.Selected.MakeVisible(false);
  DoOnSelectItem(Self, Selected, True);

end;

procedure TtiCustomListView.SetApplyFilter(const AValue: boolean);
begin
  FbApplyFilter := AValue;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiListColumn
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiListColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FsDisplayLabel := crsDefaultColDisplayLabel;
  FsFieldName   := crsDefaultColFieldName;
  FsDisplayMask := tiListViewDisplayMaskFromDataType(lvtkString);
  FDataType     := lvtkString;
  FDerived      := false;
  FWidth        := -1;
  FAlignment    := taRightJustify;
end;

destructor TtiListColumn.Destroy;
begin
  inherited;
end;

function TtiListColumn.Clone: TtiListColumn;
begin
  result               := TtiListColumn.Create(nil);
  result.DisplayName   := self.DisplayName ;
  result.DisplayLabel  := self.DisplayLabel;
  result.FieldName     := self.FieldName   ;
  result.DisplayMask   := self.DisplayMask ;
  result.DataType      := self.DataType    ;
  result.Derived       := self.Derived     ;
  result.FOnGetCaption := self.FOnGetCaption;
  result.OnDeriveColumn := self.OnDeriveColumn;
  result.Width         := self.Width;
  result.Alignment     := self.Alignment;
end;

{
procedure TtiListColumn.Assign(Source: TtiObject);
var
  lData : TtiListColumn;
begin
  Assert(Source is TtiListColumn, 'Source not TtiListColumn');
  lData := TtiListColumn(Source)  ;
  inherited Assign(TtiObject(lData));
  DisplayName := lData.DisplayName ;
  DisplayLabel := lData.DisplayLabel;
  FieldName   := lData.FieldName   ;
  DisplayMask := lData.DisplayMask ;
  DataType    := lData.DataType    ;
end;
}
function TtiListColumn.GetDisplayName: string;
begin
  result := DisplayLabel;
end;

procedure TtiListColumn.SetDataType(const AValue: TlvTypeKind);
begin
  if DisplayMask = tiListViewDisplayMaskFromDataType(FDataType) then
    DisplayMask := tiListViewDisplayMaskFromDataType(AValue);
  FDataType := AValue;

  case AValue of
  lvtkString, lvtkDateTime : Alignment := taLeftJustify;
  lvtkInt, lvtkFloat, lvtkCurrency : Alignment := taRightJustify;
  else
    raise exception.Create('Invalid TlvTypeKind passed to ' + ClassName + '.SetDataType');
  end;

end;

// The mask for the currency type is purely for reference. The format function
// in GetCellText will automatically choose the correct currency format based on
// the Locale set in Windows.
function tiListViewDisplayMaskFromDataType(const AValue : TlvTypeKind): string;
begin
  case AValue of
  lvtkString  : result := '';
  lvtkInt     : result := '#,##0';
  lvtkFloat   : result := '#,##0.000';
  lvtkDateTime : result := 'dd/mm/yyyy';
  lvtkCurrency : Result := '#,##0.00';
  else
    Assert(false, 'Invalid DataType');
  end;
end;

procedure TtiListColumn.SetFieldName(const AValue: string);
begin
  if not FDerived then
  begin
    Assert(AValue <> '', 'Can not assign empty field name.');
  end;
  if FsDisplayLabel = FsFieldName then
    FsDisplayLabel := AValue;
  FsFieldName := AValue;
//  end
//  else
//  begin
//    FsFieldName := '';
//    FsDisplayLabel := '';
//  end;
end;

procedure TtiListColumn.SetDerived(const AValue: boolean);
begin
  FDerived := AValue;
{
  if FDerived then
  begin
    FsFieldName     := '';
    FsDisplayMask   := '';
    FDataType      := lvtkString;
//    Derived       :=
//    OnDeriveColumn := nil;
  end
  else
  begin
    if FsFieldName = '' then
      FsFieldName     := 'Caption';
    if FsDisplayMask = '' then
      FsDisplayMask   := 'Caption';
    FDataType      := lvtkString;
    FOnDeriveColumn := nil;
  end;
}
end;

procedure TtiListColumn.SetOnDeriveColumn(const AValue: TtiDeriveListColumnValue);
begin
  FOnDeriveColumn := AValue;
{
  if Assigned(FOnDeriveColumn) then
  begin
    FDerived         := true;
    FsFieldName     := '';
    FsDisplayMask   := '';
    FDataType       := lvtkString;
//    OnDeriveColumn :=
  end
  else
  begin
//    FieldName     := '';
//    DisplayMask   := '';
//    DataType      := lvtkString;
    FDerived       := false;
//    FOnDeriveColumn := nil;
  end;
}  
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiListColumns
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiListColumns.Add: TtiListColumn;
begin
  result := TtiListColumn(inherited add);
  result.Collection := Self;
  Assert(FOwner is TtiCustomListView, 'Owner not a TtiCustomListView');
//  if csDesigning in TtiCustomListView(Owner).ComponentState then
//    TtiCustomListView(Owner).SetupCols;
//  TtiCustomListView(Owner).RunTimeGenCols := false;
end;

procedure TtiListColumns.Clear;
begin
  inherited;
end;

//constructor TtiListColumns.Create(pListView: TtiCustomListView);
constructor TtiListColumns.Create(AOwner : TComponent);
begin
  inherited Create(TtiListColumn);
  FOwner := AOwner;
end;

destructor TtiListColumns.Destroy;
begin
  inherited;
end;

function TtiListColumns.FindByDisplayLabel(const AValue : string): TtiListColumn;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].DisplayLabel = AValue then begin
      result := Items[i];
      break; //==>
    end;
end;

function TtiListColumns.GetItem(Index: integer): TtiListColumn;
begin
  result := TtiListColumn(inherited GetItem(Index));
end;

function TtiListColumns.GetOwner: TPersistent;
begin
  result := FOwner;
end;

procedure TtiListColumns.DisplayLabelsToStringList(pSL: TStringList);
var
  i : integer;
begin
  pSL.Clear;
  for i := 0 to count - 1 do
    pSL.Add(Items[i].DisplayLabel);
end;

procedure TtiListColumns.SetItem(Index: integer;const AValue: TtiListColumn);
begin
  inherited SetItem(Index, AValue);
end;

procedure TtiCustomListView.DoApplyFilter;
var
  i        : integer;
  lData    : TtiObject;
  lbInclude : boolean;
begin

  FDataInternal.Clear;
  if FData = nil then
    exit; //==>

  for i := 0 to FData.Count - 1 do begin
    if (Assigned(FOnFilterData)) and
       (FbApplyFilter) then begin
      Assert(TObject(FData.Items[i]) is TtiObject, 'Data not TtiObject');
      lData := TtiObject(FData.Items[i]);
      FOnFilterData(lData, lbInclude);
      if lbInclude then
        FDataInternal.Add(lData);
    end else
      FDataInternal.Add(FData.Items[i]);
  end;


end;

function DoSortData(pData1, pData2: Pointer): integer;
  procedure _DoRaiseException(AFieldName : string; AClassName : string);
  begin
    raise exception.Create('Unable to read field <' +
                            AFieldName + '> from <' +
                            AClassName + '> in _DoSortData()');
  end;

  function _DoSortData(pData1, pData2 : TtiObject;
                        pSortOrder : TlvSortOrder): integer;
  var
    lVal1 : variant;
    lVal2 : variant;
  begin
    try
      lVal1 := GetPropValue(pData1, pSortOrder.FieldName, false);
      if VarIsNull(lVal1) then
        _DoRaiseException(pSortOrder.FieldName, pData1.ClassName);
    except
      on e:exception do
        _DoRaiseException(pSortOrder.FieldName, pData1.ClassName);
    end;

    try
      lVal2 := GetPropValue(pData2, pSortOrder.FieldName, false);
      if VarIsNull(lVal2) then
        _DoRaiseException(pSortOrder.FieldName, pData2.ClassName);
    except
      on e:exception do
        _DoRaiseException(pSortOrder.FieldName, pData2.ClassName);
    end;

    if tiIsVariantOfType(lVal1, varOLEStr) then
      lVal1 := UpperCase(lVal1);
    if tiIsVariantOfType(lVal2, varOLEStr) then
      lVal2 := UpperCase(lVal2);

    if lVal1 < lVal2 then
      result := -1
    else if lVal1 > lVal2 then
      result := 1
    else
      result := 0;

    case pSortOrder.SortDirection of
    lvsdAscending : result := result * 1;  // Do nothing
    lvsdDescending : result := result * -1; // Descending sort order
    else
      Assert(false, 'Invalid sort direction.');
    end;

  end;
var
  lData1 : TtiObject;
  lData2 : TtiObject;
  i     : integer;
begin
  Assert(TObject(pData1) is TtiObject, 'pData1 not TtiObject');
  Assert(TObject(pData2) is TtiObject, 'pData2 not TtiObject');

  lData1 := TtiObject(pData1);
  lData2 := TtiObject(pData2);

  result := 0;
  for i := 0 to uSortOrders.Count - 1 do begin
    result := _DoSortData(lData1, lData2, uSortOrders.Items[i]);
    if result <> 0 then
      break; //==>
  end;

end;

procedure TtiCustomListView.DoApplySort;
var
  i : integer;
  lSortOrder : TlvSortOrder;
begin

  if not ApplySort then
    exit; // ==>

  if FData = nil then
    exit; //==>

  if FData.Count = 0 then
    exit; //==>

  if FSortOrders.Count = 0 then begin
    FbApplySort := False;
    raise exception.Create('No sort orders defined');
  end;

  // uSortOrders.Assign not working, so use this hack
  // uSortOrders.Assign(FSortOrders);
  uSortOrders.Clear;
  for i := 0 to FSortOrders.Count - 1 do begin
    lSortOrder              := uSortOrders.Add;
    lSortOrder.FieldName    := FSortOrders.Items[i].FieldName;
    lSortOrder.SortDirection := FSortOrders.Items[i].SortDirection;
  end;

  FDataInternal.Sort(DoSortData);

end;

procedure TtiCustomListView.SetApplySort(const AValue: boolean);
begin
  SetApplySortNoRefresh(AValue, true);
end;

procedure TtiCustomListView.DoColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = 0 then
    exit; //==>

  if not FbSortOnHeadingClick then
    Exit; //==>
    
  // Can not currently sort on a derived column, sorting by column click
  // available on simple field types only.
  if TtiListColumn(FListColsBeingDisplayed.Items[Column.Index]).Derived then
  begin
    MessageDlg('Sorry, you can not sort on this column.',
                mtError,
                [mbOK], 0);
    Exit; //==>
  end;

{ We used to save the Column ref itself, but that was not guarranteed
  to be valid by the time the DoSortColumn event got fired. }
  FsSortField :=
    TtiListColumn(FListColsBeingDisplayed.Items[Column.Index]).FieldName;
  FTimerSort.Enabled := true;

end;

procedure TtiCustomListView.DoSortColumn;
var
  lSortDirection : TlvSortDirection;
  lSortOrder : TlvSortOrder;
begin

  if not FbSortOnHeadingClick then
    exit; //==>

  DisconnectFromData;

  if FsLastSortField = FsSortField  then begin
    lSortDirection := lvsdAscending;
    case FLastSortDirection of
    lvsdAscending : lSortDirection := lvsdDescending;
    lvsdDescending : lSortDirection := lvsdAscending;
    else
      Assert(false, 'Invalid sort direction.');
    end;
  end else
    lSortDirection := lvsdAscending;

  FsLastSortField   := FsSortField;
  FLastSortDirection := lSortDirection;

  SortOrders.Clear;
  lSortOrder := SortOrders.Add;
  lSortOrder.FieldName := FsSortField;
  lSortOrder.SortDirection := lSortDirection;

  if not FbApplySort then
    FbApplySort := true;

  Refresh;

end;


procedure TtiCustomListView.DoOnTimerSort(sender: TObject);
begin
  FTimerSort.Enabled := false;
  DoSortColumn;
end;

procedure TtiCustomListView.DoCustomDrawItem(Sender: TCustomListView;
                                          Item: TListItem;
                                          State: TCustomDrawState;
                                          var DefaultDraw: Boolean);
begin
  if Assigned(FOnGetFont) then begin
    FOnGetFont(Self,
                FLV.Canvas,
                Item,
                TtiObject(FDataInternal.Items[Item.Index]));
  end;
end;

{
procedure TtiCustomListView.DoOnTimerSetData(Sender: TObject);
begin
  if not Showing then
    Exit; //==>

  TTimer(Sender).Enabled := false;
  if Items.Count < 2 then
    Exit; //==>

  if FbSelectFirstRow then
  begin
    PositionCursor(1);
    PositionCursor(0);
  end;

end;
}

// Scan the ListView and add the data object for all items marked as selected
// to FSelectedData, then return FSelectedData as the result of the function.
function TtiCustomListView.GetSelectedDataList : TList;
var
  lListItem : TListItem;
begin

  // Clear the result list
  FSelectedData.Clear;

  // Find the first selected item
  lListItem := FLV.Selected;

  {$IFNDEF FPC}
  // Scan the rest of the items
  while lListItem <> nil do begin
    FSelectedData.Add(TObject(FDataInternal.Items[lListItem.Index]));
    lListItem := FLV.GetNextItem(lListItem, sdAll, [isSelected]);
  end;
  {$ELSE}
   {$WARNING TODO: How to fix that ?}
  {$ENDIF}

  result := FSelectedData;

end;

// Delete a list item, along with the data it points to.
procedure TtiCustomListView.DeleteData(pListItem: TListItem);
var
  i    : integer;
  lData : TObject;
begin

  if pListItem = nil then
    Exit; //==>

  lData := TObject(pListItem.Data);

  i := FSelectedData.IndexOf(lData);
  if i <> -1 then
    FSelectedData.Delete(i);

  i := FData.IndexOf(lData);
  if i <> -1 then
    FData.Delete(i);

  i := FDataInternal.IndexOf(lData);
  if i <> -1 then
    FDataInternal.Delete(i);

  Refresh;

end;

procedure TtiCustomListView.First;
begin
  PositionCursor(0);
end;

procedure TtiCustomListView.MoveSelection(piMoveBy : integer);
begin
  PositionCursor(SelectedIndex + piMoveBy);
end;

procedure TtiCustomListView.Last;
begin
  PositionCursor(FDataInternal.Count - 1);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TlvSortOrders
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TlvSortOrders.Add: TlvSortOrder;
begin
  result := TlvSortOrder(inherited add);
end;

constructor TlvSortOrders.Create(AOwner : TComponent);
begin
  inherited Create(TlvSortOrder);
  FOwner := AOwner;
end;

function TlvSortOrders.GetItem(Index: integer): TlvSortOrder;
begin
  result := TlvSortOrder(inherited GetItem(Index));
end;

function TlvSortOrders.GetOwner: TPersistent;
begin
  result := FOwner;
end;

procedure TlvSortOrders.SetItem(Index: integer; const AValue: TlvSortOrder);
begin
  inherited SetItem(Index, AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TlvSortOrder
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TlvSortOrder.Assign(source: TPersistent);
begin
  inherited Assign(TlvSortOrder(source));
end;

constructor TlvSortOrder.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FieldName    := 'Enter value';
  SortDirection := lvsdAscending;
end;

function TlvSortOrder.GetDisplayName: string;
begin
  result := FieldName;
end;

{ TtiLVNullDataRow }

constructor TtiLVNullDataRow.Create;
begin
  inherited;
  FsNoData := 'No data';
end;

{ TtiLVNullData }

constructor TtiLVNullData.Create;
begin
  inherited;
  Add(TtiLVNullDataRow.Create);
end;

function TtiCustomListView.CanDelete: boolean;
begin
  result := Enabled and
            FLV.Focused and 
            (not FReadOnly) and
            (FLV.Selected <> nil);
  if result and Assigned(FOnCanDelete) then
    FOnCanDelete(self, SelectedData, Selected, result);
end;

function TtiCustomListView.CanEdit: boolean;
begin
  result := Enabled and
            FLV.Focused and 
            (Not FReadOnly) and
            (FLV.Selected <> nil);
  if result and Assigned(FOnCanEdit) then
    FOnCanEdit(self, SelectedData, Selected, result);
end;

function TtiCustomListView.CanInsert: boolean;
begin
  result := Enabled and (not FReadOnly);
  if result and Assigned(FOnCanInsert) then
    FOnCanInsert(self, SelectedData, Selected, result);
end;

procedure TtiCustomListView.PositionCursor(AData: TtiObject);
var
  i : integer;
begin
  i := FDataInternal.IndexOf(AData);
  if i <> -1 then
    PositionCursor(i);
end;

procedure TtiCustomListView.DoOnSelectItem(Sender: TObject; Item: TListItem;Selected: boolean);
var
  lSelectedData : TtiObject;
begin
  if FbProcessingOnClick then
    Exit; //==>

  if Selected and Assigned(FOnItemArive) then
  begin
    FLastItem := Item;
    lSelectedData := TtiObject(FDataInternal.Items[FLastItem.Index]);
    FOnItemArive(self, lSelectedData, FLastItem);
  end;

  if (Not Selected) and
     (Assigned(FOnItemLeave)) and
     (FLastItem <> nil) then
  begin
    lSelectedData := TtiObject(FDataInternal.Items[FLastItem.Index]);
    FOnItemLeave(self, lSelectedData, FLastItem);
    FLastItem := nil;
  end;
end;

procedure TtiCustomListView.DoOnClick(Sender: TObject);
var
  lItem  : TListItem;
  AData  : TtiObject;
  pColumn : TListColumn;
  lPoint : TPoint;
  i      : integer;
  liLeft : integer;
begin
  if Assigned(FOnLVClick) then
  begin
   {$IFNDEF FPC}
    lPoint.X := Mouse.CursorPos.X;
    lPoint.Y := Mouse.CursorPos.Y;
    lPoint  := Self.ScreenToClient(lPoint);

    lItem := FLV.GetItemAt(lPoint.X, lPoint.Y);
    {$ELSE}
    lItem := FLV.ItemFocused;
    {$ENDIF}
    if lItem = nil then
      Exit; //==>

    if lItem.Data <> nil then
      AData := TtiObject(lItem.Data)
    else
      AData := nil;

    pColumn := nil;
    liLeft := 0;
    for i := 0 to Columns.Count - 1 do
    begin
      liLeft := liLeft + Columns[i].Width;
      if lPoint.X <= liLeft then
      begin
        pColumn := Columns[i];
        Break; //==>
      end;
    end;
    FOnLVClick(Self, lItem, AData, pColumn);

  end;
end;

function TtiCustomListView.GetMultiSelect: boolean;
begin
  result := FLV.MultiSelect;
end;

procedure TtiCustomListView.SetMultiSelect(const AValue: boolean);
begin
  FLV.MultiSelect := AValue;
end;

function TtiCustomListView.GetOnChange: TLVChangeEvent;
begin
  result := FLV.OnChange;
end;

procedure TtiCustomListView.SetOnChange(const AValue: TLVChangeEvent);
begin
  FLV.OnChange := AValue;
end;

{$IFNDEF FPC}
function TtiCustomListView.GetOnChanging: TLVChangingEvent;
begin
  result := FLV.OnChanging;
end;

procedure TtiCustomListView.SetOnChanging(const AValue: TLVChangingEvent);
begin
  FLV.OnChanging := AValue;
end;
{$ENDIF}

function TtiCustomListView.GetOnKeyDown: TKeyEvent;
begin
  result := FLV.OnKeyDown;
end;

procedure TtiCustomListView.SetOnKeyDown(const AValue: TKeyEvent);
begin
  FLV.OnKeyDown := AValue;
end;

function TtiCustomListView.GetOnKeyPress: TKeyPressEvent;
begin
  result := FLV.OnKeyPress;
end;

procedure TtiCustomListView.SetOnKeyPress(const AValue: TKeyPressEvent);
begin
  FLV.OnKeyPress := AValue;
end;

function TtiCustomListView.GetSmallImages: TCustomImageList;
begin
  result := FLV.SmallImages;
end;

procedure TtiCustomListView.SetSmallImages(const AValue: TCustomImageList);
begin
  FLV.SmallImages := AValue;
end;

function TtiCustomListView.GetViewStyle: TViewStyle;
begin
  result := FLV.ViewStyle;
end;

procedure TtiCustomListView.SetViewStyle(const AValue: TViewStyle);
begin
  FLV.ViewStyle := AValue;
end;

function TtiCustomListView.GetRowSelect: Boolean;
begin
  result := FLV.RowSelect;
end;

procedure TtiCustomListView.SetRowSelect(const AValue: Boolean);
begin
  FLV.RowSelect := AValue;
end;

function TtiCustomListView.GetItems: TListItems;
begin
  result := FLV.Items;
end;

function TtiCustomListView.GetColumns: TListColumns;
begin
  result := FLV.Columns;
end;

function TtiCustomListView.GetSelected: TListItem;
begin
  result := FLV.Selected;
end;

function TtiCustomListView.GetListView: TListView;
begin
  result := FLV;
end;


{$IFNDEF FPC}
function TtiCustomListView.GetItemAt(X, Y: Integer): TListItem;
begin
 result := FLV.GetItemAt(X, Y);
end;
{$ENDIF}


function TtiCustomListView.CanMoveItem(AItem : TListItem; piMoveBy : integer): boolean;
begin
  if piMoveBy = 0 then
  begin
    result := true;
    Exit; //==>
  end;

  // Move down
  if piMoveBy < 0 then
    result := (AItem <> nil) and
              (AItem.Index > 0)
  else
    result := (AItem <> nil) and
              (AItem.Index < Items.Count - 1)
 ;

end;

procedure TtiCustomListView.MoveItem(AItem: TListItem; piMoveBy: integer);
var
  liMoveFrom : integer;
  liBefore   : integer;
  liAfter    : integer;
  lDataMove  : TtiObject;
  lDataBefore : TtiObject;
  lDataAfter : TtiObject;
begin
  if Assigned(FOnMoveItem) then
  begin
    if piMoveBy = 0 then
      Exit; //==>

    liMoveFrom := AItem.Index;
    if piMoveBy > 0 then
    begin
      liBefore  := liMoveFrom + piMoveBy;
      liAfter   := liMoveFrom + piMoveBy + 1
    end
    else
    begin
      liBefore  := liMoveFrom + piMoveBy - 1;
      liAfter   := liMoveFrom + piMoveBy ;
    end;

    if liBefore < 0 then
      lDataBefore := nil
    else
      lDataBefore := TtiObject(FDataInternal.Items[liBefore]);

    if liAfter > FDataInternal.Count-1 then
      lDataAfter := nil
    else
      lDataAfter := TtiObject(FDataInternal.Items[liAfter]);

    lDataMove := TtiObject(FDataInternal.Items[ liMoveFrom ]);
    FOnMoveItem(FDataInternal, lDataMove, lDataBefore, lDataAfter);
    Refresh;
    Application.ProcessMessages;
    PositionCursor(lDataMove);
    SetFocus;
  end;

end;

procedure TtiCustomListView.SetRunTimeGenCols(const AValue: boolean);
begin
  FbRunTimeGenCols := AValue;
  //if not AValue then
  //  FtiListColumns.Clear;
end;

procedure TtiCustomListView.SetFocus;
begin
  if not Enabled then
    Exit; //==> 
  inherited SetFocus;
  LV.SetFocus;
end;

function TtiCustomListView.GetSelectedData: TtiObject;
begin
  if Selected = nil then
    result := nil
  else
    result := TtiObject(Selected.Data);
end;

procedure TtiCustomListView.SetLVColor;
begin
  if (not Enabled) or FReadOnly then
    FLV.Color := clBtnFace 
  else
    FLV.Color := clWindow;
end;

procedure TtiCustomListView.SetEnabled(AValue: Boolean);
begin
  inherited SetEnabled(AValue);
  SetLVColor;
  { TODO : TtiCustomListView.SetEnabled sets the ListView's color property, but
           the font color should also be set in a way that does not conflict
           with the OnGetFont event. The select record's focus should also be
           turned off. }
end;

function TtiCustomListView.GetSelectedIndex: integer;
begin
  if Selected = nil then
    result := -1
  else
    result := Selected.Index;
end;

procedure TtiCustomListView.SetSelectedIndex(const AValue: integer);
begin
  if AValue <= FLV.Items.Count - 1 then
    FLV.Selected := FLV.Items[AValue];
end;

procedure TtiCustomListView.BeginUpdate;
begin
//  FLV.Columns.BeginUpdate;
{$IFNDEF FPC}
 FLV.Items.BeginUpdate;
 {$ELSE}
 FLV.BeginUpdate;
{$ENDIF}
end;

procedure TtiCustomListView.EndUpdate;
begin
//  FLV.Columns.EndUpdate;
{$IFNDEF FPC}
FLV.Items.EndUpdate;
{$ELSE}
FLV.EndUpdate;
{$ENDIF}
end;

procedure TtiCustomListView.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lTargetItem      : TListItem  ;
  lTargetData      : TtiObject;
  lDragObject      : TtiLVDragObject;
begin
 {$IFNDEF FPC}
  lTargetItem := FLV.GetItemAt(X, Y);
  lTargetData := nil;

  if lTargetItem <> nil then
    lTargetData := TtiObject(FDataInternal.Items[lTargetItem.Index]);

  lDragObject := Source as TtiLVDragObject;

  // If we have left+clicked, and moved the mouse a little, a drag event may
  // have been triggered, this is probably not what we are wanting, so throw
  // this event away.
  if lDragObject.Data = lTargetData then
    Exit; //==>

  FOnDrop(lDragObject.tiListView,
           lDragObject.Data,
           Self,
           lTargetData);

  Refresh;
  {$ELSE}
     {$WARNING TODO: Fix it when LCL will have better drag&drop support}
  {$ENDIF}
end;

procedure TtiCustomListView.DoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  lItem      : TListItem;
  lData      : TtiObject;
  lDragObject : TtiLVDragObject;
begin
 {$IFNDEF FPC}
  Accept := false;

  if Not Assigned(FOnCanAcceptDrop) then
    Exit; //==>

  lItem := FLV.GetItemAt(X, Y);
  lData := nil;

  if lItem <> nil then
    lData := TtiObject(FDataInternal.Items[lItem.Index]);

  lDragObject := Source as TtiLVDragObject;

  OnCanAcceptDrop(lDragObject.tiListView,
                   lDragObject.Data,
                   Self,
                   lData,
                   Accept);
 {$ELSE}
    {$WARNING TODO: Fix it when LCL will have better drag&drop support}
 {$ENDIF}
end;

procedure TtiCustomListView.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lItem : TListItem;
  lData : TtiObject  ;
begin
{$IFNDEF FPC}
  // Only allow drag if mouse button is mbLeft or mbRight
  if (Button <> mbLeft) then
    Exit; //==>

//  if not Assigned(FOnCanAcceptDrop) or
//     not Assigned(FOnDrop) then
//    Exit; //==>
  if not FCanStartDrag then
    Exit; //==>

  lItem := FLV.GetItemAt(X, Y);

  if lItem = nil then
    Exit; //==>

  lData      := TtiObject(FDataInternal.Items[lItem.Index]);

  if lData = nil then
    Exit; //==>

  FLV.BeginDrag(false);
 {$ELSE}
    {$WARNING TODO: Fix it when LCL will have better drag&drop support}
 {$ENDIF}
end;

procedure TtiCustomListView.DoStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  lDragObject : TtiLVDragObject;
begin
  lDragObject           := TtiLVDragObject.Create;
  lDragObject.tiListView := Self;
  lDragObject.Data      := SelectedData;
  lDragObject.Item      := Selected;
  DragObject            := lDragObject;
end;

function TtiCustomListView.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;

procedure TtiCustomListView.SetReadOnly(const AValue: boolean);
begin
  FReadOnly := AValue;
  SetLVColor;
end;

procedure TtiCustomListView.SetSelectedData(const AValue: TtiObject);
begin
  PositionCursor(AValue);
end;

{ TtiListViewInternal }

constructor TtiListViewInternal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVScrolled := false;
end;

{$IFNDEF FPC}
procedure TtiListViewInternal.WMVScroll(var Message: TWMVScroll);
{$ELSE}
procedure TtiListViewInternal.WMVScroll(var Msg: TLMScroll);
{$ENDIF}
begin
  inherited;
  FVScrolled := true;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

procedure TtiCustomListView.SynchroniseWith(pListView: TtiCustomListView);
var
  liIndex : integer;
  lScrollBy : integer;
  lBottomItemIndex : integer;
  lOffset : integer;
begin

try
  if pListView = nil then
    Exit; //==>

  if pListView.Selected = nil then
    Exit; //==>

  if (FLV.Items.Count = 0) then
    Exit; //==>

  if pListView.LV.Selected.Index < pListView.LV.TopItem.Index then
    pListView.LV.Selected := pListView.LV.TopItem;

  if TtiListViewInternal(pListView.LV).VScrolled then
  begin
    TtiListViewInternal(pListView.LV).VScrolled := false;
    lOffset := 0;
  end
  else
    lOffset := 0;

  lBottomItemIndex :=
    pListView.LV.TopItem.Index +
    pListView.LV.VisibleRowCount - lOffset;

  if pListView.Selected.Index >
     (lBottomItemIndex) then
    pListView.LV.Selected := pListView.LV.Items[lBottomItemIndex];

  liIndex := pListView.LV.TopItem.Index;
  {$IFNDEF FPC}
  if (liIndex <= (FLV.Items.Count-1)) then
  begin
    lScrollBy :=
      FLV.Items[liIndex].Top - 19;
      // Why the 19, Don't really know. Think it's the height of the
      // column headings. Must find a better way of deriving the 19.
    FLV.Scroll(0, lScrollBy);
  end;
  {$ENDIF}
  
  liIndex := pListView.LV.TopItem.Index;
  if (liIndex <= (FLV.Items.Count-1)) then
    FLV.Items[liIndex].MakeVisible(false);

  liIndex := pListView.Selected.Index;
  FLV.Selected := FLV.Items[liIndex];
  if (liIndex <= (FLV.Items.Count-1)) then
    FLV.Items[liIndex].MakeVisible(false);
except
  on e:exception do
    raise exception.Create(e.message + #13 + ' in TtiCustomListView.SynchroniseWith');
end;
end;

function TtiCustomListView.GetOnVScroll: TNotifyEvent;
begin
  result := FLV.OnVScroll;
end;

procedure TtiCustomListView.SetOnVScroll(const AValue: TNotifyEvent);
begin
  FLV.OnVScroll := AValue;
end;

function TtiCustomListView.AddColumn(const AFieldName: string;
                                      const pDataType: TlvTypeKind;
                                      const pDisplayLabel: string = '';
                                      pColWidth : integer = -1): TtiListColumn;
begin

  if RunTimeGenCols then
  begin
    RunTimeGenCols := false;
    ClearColumns;
    AddImageColumn;
  end;

  result := FtiListColumns.Add;
  result.FieldName   := AFieldName;
  result.DataType    := pDataType;
  if pDisplayLabel = '' then
    result.DisplayLabel := AFieldName
  else
    result.DisplayLabel := pDisplayLabel;
  result.Width := pColWidth;

  DoAddColumn(result);

end;

procedure TtiCustomListView.ClearColumns;
begin
  BeginUpdate;
  try
    FListColsBeingDisplayed.Clear;
    FtiListColumns.Clear;
    FSortOrders.Clear;
    FLV.Columns.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TtiListColumn.SetWidth(const AValue: integer);
begin
  FWidth := AValue;
//  Assert(Collection <> nil, 'Collection = nil');
//  Assert(Collection is TtiListColumns, 'Collection not a TtiListColumns');
//  Assert(Collection.Owner <> nil, 'Collection.Owner = nil');
//  Assert(Collection.Owner is TtiCustomListView, 'Collection.Owner not a TtiCustomListView');
end;

procedure TtiCustomListView.DoReSize(Sender: TObject);
begin
  FLV.SetBounds(
    1,
    1,
    Width - 2,
    Height - 2);
end;

procedure TtiCustomListView.SetApplySortNoRefresh(const AValue, pRefresh: boolean);
begin
  if FbApplySort = AValue then
    exit; //==>
  FbApplySort := AValue;
  if pRefresh then
    Refresh;
end;

procedure TtiCustomListView.SetOnInfoTip(const AValue: TtiLVInfoTipEvent);
begin
  FOnTipInfo := AValue;
end;

function TtiCustomListView.GetDefaultInfoTip(AIndex: Integer): string;
var
  i : Integer;
begin
  Result := '';
  for i := 1 to Columns.Count - 1 do
  begin
    if i > 1 then
      Result := Result + Cr;
    Result := Result + Columns.Items[i].DisplayName + ':  ';
    Result := Result + GetCellText(AIndex, i-1);
  end;
end;

procedure TtiCustomListView.DoTipInfo(Sender: TObject;
                                       Item: TListItem;
                                       var InfoTip: string);
var
  lInfoTip: string;
begin
  Assert(Item <> nil, 'Item not assigned');
  case FInfoTipType of
  itNone   :;// Do nothing
  itDefault : InfoTip := GetDefaultInfoTip(Item.Index);
  itCustom : begin
                if Assigned(FOnTipInfo) then
                begin
                  FOnTipInfo(Self,
                             SelectedData,
                             FLV.Selected,
                             lInfoTip);
                  InfoTip:= lInfoTip;
                end;
              end;
  else
    raise ETIOPFProgrammerException.Create('Invalid TtiLVInfoTypType');
  end;

end;

{ TtiListView }

constructor TtiListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Create the popup menu
  FPopupMenu := TPopupMenu.Create(self);
  FPopupMenu.Images := gTIImageListMgr.ILNormal16;
  FPopupMenu.OnPopup := MenuOnPopup;
  PopupMenu := FPopupMenu;

  // Create select columns menu item
  FpmiView         := TMenuItem.Create(self);
  FpmiView.Caption := '&View';
  FpmiView.OnClick := pmiViewOnClick;
  FpmiView.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_View);
  FPopupMenu.Items.Add(FpmiView);

  FpmiEdit         := TMenuItem.Create(self);
  FpmiEdit.Caption := '&Edit';
  FpmiEdit.OnClick := pmiEditOnClick;
  FpmiEdit.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Edit);
  FPopupMenu.Items.Add(FpmiEdit);

  FpmiNew         := TMenuItem.Create(self);
  FpmiNew.Caption := '&New';
  FpmiNew.OnClick := pmiNewOnClick;
  FpmiNew.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Insert);
  FPopupMenu.Items.Add(FpmiNew);

  FpmiDelete       := TMenuItem.Create(self);
  FpmiDelete.Caption := '&Delete';
  FpmiDelete.OnClick := pmiDeleteOnClick;
  FpmiDelete.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Delete);
  FPopupMenu.Items.Add(FpmiDelete);

  SetButtonStyle(lvbsNormalButtons);

end;

destructor TtiListView.Destroy;
begin

  inherited;
end;

function TtiListView.GetVisibleButtons: TtiLVVisibleButtons;
begin
  result := FCtrlBtnPnl.VisibleButtons;
end;

procedure TtiListView.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FCtrlBtnPnl.VisibleButtons := AValue;
  DoReSize(Nil);
end;

procedure TtiListView.DoReSize(Sender: TObject);
var
  lTop : integer;
begin

  if FCtrlBtnPnl.VisibleButtons = [] then
    lTop := 1
  else begin
    lTop := FCtrlBtnPnl.Height + 2;
    FCtrlBtnPnl.SetBounds(1, 1, Width-2, FCtrlBtnPnl.Height);
  end;

  FLV.SetBounds(
    1,
    lTop,
    Width - 2,
    Height - lTop - 1);

end;

procedure TtiListView.SetButtonStyle(const AValue: TLVButtonStyle);
begin
  tiCtrlButtonPanel.CreateCtrlBtnPnl(FCtrlBtnPnl, AValue, Self,
                                      CanView, CanInsert, CanEdit, CanDelete);
  if Assigned(FOnItemView) then
    FCtrlBtnPnl.OnView      := DoView;
  if Assigned(FOnItemInsert) then
    FCtrlBtnPnl.OnNew      := DoNew;
  if Assigned(FOnItemEdit) then
    FCtrlBtnPnl.OnEdit     := DoEdit;
  if Assigned(FOnItemDelete) then
    FCtrlBtnPnl.OnDelete   := DoDelete;
  FCtrlBtnPnl.RefreshButtons;
  DoReSize(nil);
end;

procedure TtiListView.SetOnItemDelete(const AValue: TtiLVItemEditEvent);
begin
  FOnItemDelete := AValue;
  if Assigned(FOnItemDelete) then
    FCtrlBtnPnl.OnDelete := DoDelete
  else
    FCtrlBtnPnl.OnDelete := nil;
end;

procedure TtiListView.SetOnItemEdit(const AValue: TtiLVItemEditEvent);
begin
  FOnItemEdit := AValue;
  if Assigned(FOnItemEdit) then
    FCtrlBtnPnl.OnEdit := DoEdit
  else
    FCtrlBtnPnl.OnEdit := nil;
end;

procedure TtiListView.SetOnItemInsert(const AValue: TtiLVItemEditEvent);
begin
  FOnItemInsert := AValue;
  if Assigned(FOnItemInsert) then
    FCtrlBtnPnl.OnNew := DoNew
  else
    FCtrlBtnPnl.OnNew := nil;
end;

procedure TtiListView.Loaded;
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
  MenuOnPopup(nil);
end;

procedure TtiListView.SetData(const AValue: TList);
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.DoOnSelectItem(Sender: TObject; Item: TListItem;ItmSelected: boolean);
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.SetEnabled(AValue: Boolean);
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.SetReadOnly(const AValue: boolean);
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.pmiDeleteOnClick(sender: TObject);
begin
  DoDelete(nil);
end;

procedure TtiListView.DoDelete(Sender: TObject);
var
  liSelected : integer;
  lSelectedData : TtiObject;
begin

  if Selected = nil then
    Exit; //==>
  if SelectedData = nil then
    Exit; //==>
  if not Assigned(FOnItemDelete) then
    Exit; //==>

  liSelected := FLV.Selected.Index;
  lSelectedData := SelectedData;
  FOnItemDelete(Self, lSelectedData, Selected);
  // After deleting an object, we must refresh the entire list view, then
  // position the cursor as close as possible to the object we just deleted.
  Refresh;
  SetFocus;
  // PositionCursor is still a little buggy
  PositionCursor(liSelected);

end;

procedure TtiListView.pmiEditOnClick(sender: TObject);
begin
  DoEdit(nil);
end;

procedure TtiListView.DoEdit(Sender: TObject);
var
  lSelectedData : TtiObject;
begin
  if Selected = nil then
    Exit; //==>
  if SelectedData = nil then
    Exit; //==>
  if not Assigned(FOnItemEdit) then
    Exit; //==>

  lSelectedData := SelectedData;

  BeginUpdate;
  try
    FOnItemEdit(Self, lSelectedData, FLV.Selected);
    // After an edit, we want to leave the cursor on the same object, but
    // refresh the row in the list view.
    Refresh(false);
  finally
    EndUpdate;
  end;
  if CanFocus then
    SetFocus;
  FCtrlBtnPnl.RefreshButtons;

end;

procedure TtiListView.pmiNewOnClick(sender: TObject);
begin
  DoNew(nil);
end;

procedure TtiListView.DoNew(Sender: TObject);
var
  lData : TtiObject;
begin
  if not Assigned(FOnItemInsert) then
    Exit; //==>
  lData := nil;
  BeginUpdate;
  try
    FOnItemInsert(Self, lData, Nil);
    Refresh(true);
  finally
    EndUpdate;
  end;

  if CanFocus then
    SetFocus;
//  // PositionCursor is still a little buggy
//  //PositionCursor(lData);
//  Last;
  FCtrlBtnPnl.RefreshButtons;

end;

procedure TtiListView.MenuOnPopup(sender: TObject);
begin
  FpmiView.Visible  := (tiLVBtnVisView in VisibleButtons) and Assigned(FOnItemView);
  FpmiEdit.Visible  := (tiLVBtnVisEdit in VisibleButtons) and Assigned(FOnItemEdit);
  FpmiNew.Visible   := (tiLVBtnVisNew in VisibleButtons) and Assigned(FOnItemInsert);
  FpmiDelete.Visible := (tiLVBtnVisDelete in VisibleButtons) and Assigned(FOnItemDelete);

  if FpmiView.Visible then
    FpmiView.Shortcut := TextToShortcut('Enter')
  else
    FpmiView.Shortcut := TextToShortcut('');

  if FpmiEdit.Visible then
    FpmiEdit.Shortcut := TextToShortcut('Enter')
  else
    FpmiEdit.Shortcut := TextToShortcut('');

  if FpmiNew.Visible then
    FpmiNew.Shortcut := TextToShortcut('Ins')
  else
    FpmiNew.Shortcut := TextToShortcut('');

  if FpmiDelete.Visible then
    FpmiDelete.Shortcut := TextToShortcut('Del')
  else
    FpmiDelete.Shortcut := TextToShortcut('');

  FpmiView.Enabled  := CanView;
  FpmiEdit.Enabled  := CanEdit;
  FpmiDelete.Enabled := CanDelete;
  FpmiNew.Enabled   := CanInsert;

end;

procedure TtiListView.DoOnDblClick(Sender: TObject);
var
  lSelectedData : TtiObject;
begin

  if Assigned(FOnDblClick) then
  begin
    lSelectedData := SelectedData;
    FOnDblClick(Self, lSelectedData, Selected);
    Exit; //==>
  end;

  if CanView then
    DoView(nil)
  else if CanEdit then
    DoEdit(nil)
  else if CanInsert then
    DoNew(nil);

end;

function TtiListView.GetButtonStyle: TLVButtonStyle;
begin
  Result := FCtrlBtnPnl.ButtonStyle;
end;

procedure TtiListView.DoView(Sender: TObject);
var
  lSelectedData : TtiObject;
begin
  if Selected = nil then
    Exit; //==>
  if SelectedData = nil then
    Exit; //==>
  if not Assigned(FOnItemView) then
    Exit; //==>
  lSelectedData := SelectedData;
  FOnItemView(Self, lSelectedData, FLV.Selected);
  if CanFocus then
    SetFocus;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.SetOnItemView(const AValue: TtiLVItemEditEvent);
begin
  FOnItemView := AValue;
  if Assigned(FOnItemView) then
    FCtrlBtnPnl.OnView := DoView
  else
    FCtrlBtnPnl.OnView := nil;
end;

function TtiCustomListView.CanView: boolean;
begin
  result := Enabled and
            FLV.Focused and 
           (FLV.Selected <> nil);
  if result and Assigned(FOnCanView) then
    FOnCanView(self, SelectedData, Selected, result);
end;

procedure TtiListView.pmiViewOnClick(sender: TObject);
begin
  DoView(nil);
end;

function TtiListView.CanDelete: boolean;
begin
  Result := (tiLVBtnVisDelete in VisibleButtons) and
            (inherited CanDelete);
end;

function TtiListView.CanEdit: boolean;
begin
  Result := (tiLVBtnVisEdit in VisibleButtons) and
            (inherited CanEdit);
end;

function TtiListView.CanInsert: boolean;
begin
  Result := (tiLVBtnVisNew in VisibleButtons) and
            (inherited CanInsert);
end;

function TtiListView.CanView: boolean;
begin
  Result := (tiLVBtnVisView in VisibleButtons) and
            (inherited CanView);
end;

procedure TtiListView.DoEnter;
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiListView.DoExit;
begin
  inherited;
  FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiCustomListView.SetInfoTipType(const AValue: TtiLVInfoTypeType);
begin
  FInfoTipType := AValue;
  case FInfoTipType of
  itNone   : begin
                FLV.ShowHint := false;
                {$IFNDEF FPC}FLV.OnInfoTip := nil;{$ENDIF}
              end;
  itDefault : begin
                FLV.ShowHint := True;
                {$IFNDEF FPC}FLV.OnInfoTip := DoTipInfo;{$ENDIF}
              end;
  itCustom : begin
                FLV.ShowHint := True;
                {$IFNDEF FPC}FLV.OnInfoTip := DoTipInfo;{$ENDIF}
              end;
  else
    raise ETIOPFProgrammerException.Create('Invalid TtiLVInfoTypType');
  end;
end;

procedure TtiCustomListView.DoOnDblClick(Sender: TObject);
var
  lSelectedData : TtiObject;
begin
  if Assigned(FOnDblClick) then
  begin
    lSelectedData := SelectedData;
    FOnDblClick(Self, lSelectedData, Selected);
  end;
end;

initialization
  uSortOrders := TlvSortOrders.Create(nil);

finalization
  uSortOrders.Free;

end.


