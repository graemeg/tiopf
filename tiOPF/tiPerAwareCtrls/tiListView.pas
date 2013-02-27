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
    June 1998, Peter Hinrichsen, Created
    Sept 1999, Peter Hinrichsen, Added functionality
    May 2000,  Peter Hinrichsen, Made open source
    Feb 2002, Andrew Denton, Added lvtkCurrency
    Jun 2003, Andrew Denton, promoted RuntimeSelectedCols to public for easier
                             configuration of tiListViewPlus.
    Jun 2003, Andrew Denton, Promoted internal helper functions _ValueFromProperty
                             and _ValueFromDerivedProperty to class methods to
                             allow them to be used by the export routines in
                             TtiListViewPlus.
    Jul 2003, Andrew Denton, Added OnGetCaption event to TtiListColumn.

  Purpose:
    A TListView descendant to simplify the browsing of a TList of TPersistent.
    Adds the following features:
    * Ability to sort and filter the elements in the list
    * Runtime control over font and canvas colour.
    * Automatic setup of columns which map into the properties of a TPersistent.
    * Design time control over column headings and display masks
    * A popup menu is hard-wired into the ListView so a right click will allow
      the user to edit, insert or delete a row.

  ToDo:
    1. Remove the FTimerSetData hack.
    2. Implement the Null Object Pattern so the list view shows somthing when
       when there is no data to view
    3. Display a small glyph in the col heading to show the col and order
       of the sort.
    4. Fix the uSortOrder hack
    5. Give the ability to chain another popup menu onto the hard coded one.
    6. Move the TtiListView class into a sep unit
    7. Move the buttons from the TtiCustomListView to the TtiListView
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


{$I tiDefines.inc}

unit tiListView ;

interface
uses
  ComCtrls
  ,Classes
  ,TypInfo
  ,Controls
  ,Menus
  ,Contnrs
  ,ExtCtrls
  ,Graphics
  ,Windows
  ,ImgList
  ,Buttons
  ,Messages
  ,tiFocusPanel
  ,tiSpeedButton
  ;

const

  // 8 x 8 images
  cGlyphBtnNew08     = 'tiLVBtnNew08' ;
  cGlyphBtnNew08H    = 'tiLVBtnNew08H' ;
  cGlyphBtnNew08D    = 'tiLVBtnNew08D' ;

  cGlyphBtnEdit08    = 'tiLVBtnEdit08' ;
  cGlyphBtnEdit08H   = 'tiLVBtnEdit08H' ;
  cGlyphBtnEdit08D   = 'tiLVBtnEdit08D' ;

  cGlyphBtnDelete08  = 'tiLVBtnDelete08' ;
  cGlyphBtnDelete08H = 'tiLVBtnDelete08H' ;
  cGlyphBtnDelete08D = 'tiLVBtnDelete08D' ;

  // 16 x 16 images
  cGlyphBtnNew16     = 'tiLVBtnNew16' ;
  cGlyphBtnNew16H    = 'tiLVBtnNew16H' ;
  cGlyphBtnNew16D    = 'tiLVBtnNew16D' ;

  cGlyphBtnEdit16    = 'tiLVBtnEdit16' ;
  cGlyphBtnEdit16H   = 'tiLVBtnEdit16H' ;
  cGlyphBtnEdit16D   = 'tiLVBtnEdit16D' ;

  cGlyphBtnDelete16  = 'tiLVBtnDelete16' ;
  cGlyphBtnDelete16H = 'tiLVBtnDelete16H' ;
  cGlyphBtnDelete16D = 'tiLVBtnDelete16D' ;

  // 24 x 24 images
  cGlyphBtnNew24     = 'tiLVBtnNew24' ;
  cGlyphBtnNew24H    = 'tiLVBtnNew24H' ;
  cGlyphBtnNew24D    = 'tiLVBtnNew24D' ;

  cGlyphBtnEdit24    = 'tiLVBtnEdit24' ;
  cGlyphBtnEdit24H   = 'tiLVBtnEdit24H' ;
  cGlyphBtnEdit24D   = 'tiLVBtnEdit24D' ;

  cGlyphBtnDelete24  = 'tiLVBtnDelete24' ;
  cGlyphBtnDelete24H = 'tiLVBtnDelete24H' ;
  cGlyphBtnDelete24D = 'tiLVBtnDelete24D' ;


const

  crsDefaultColDisplayLabel = 'Caption' ;
  crsDefaultColFieldName    = 'Caption' ;

  // Type kinds for use with tiGetProperty
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString ] ;
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 ] ;
  // Float type properties
  ctkFloat  = [ tkFloat ] ;
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat ;

type

  TlvTypeKind = ( lvtkString, lvtkInt, lvtkFloat, lvtkDateTime, lvtkCurrency ) ;

  TtiCustomListView = class ;
  TtiListColumn = class ;

  TtiLVOnFilterDataEvent  = procedure( pData   : TPersistent ; var pbInclude : boolean ) of object ;
  TtiLVGetImageIndexEvent = procedure( pData   : TPersistent ; var pImageIndex : integer ) of object ;
  TtiLVEvent              = procedure( pLV : TtiCustomListView ) of object ;
  TtiLVItemEvent          = procedure( pLV : TtiCustomListView ; pData : TPersistent ; pItem : TListItem ) of object ;
  TtiLVItemEditEvent      = procedure( pLV : TtiCustomListView ; pData : TPersistent ; pItem : TListItem  ) of object ;
  TtiLVOnGetFont          = procedure( pLV : TtiCustomListView ;
                                       pCanvas : TCanvas ;
                                       pItem   : TListItem ;
                                       pData   : TPersistent ) of object ;
  TtiLVOnClick            = procedure( pLV : TtiCustomListView ;
                                       pItem   : TListItem   ;
                                       pData   : TPersistent ;
                                       pColumn : TListColumn ) of object ;
  TtiLVOnMoveItem         = procedure( pList       : TList ;
                                       pDataMove   : TPersistent ;
                                       pDataBefore : TPersistent ;
                                       pDataAfter  : TPersistent ) of object ;
  TtiLVCanPerformAction   = procedure( pLV  : TtiCustomListView ; pData : TPersistent ; pItem : TListItem ; var pbCanPerformAction : boolean ) of object ;

  TtiLVDragDropEvent    = procedure( ptiLVSource       : TtiCustomListView ;
                                     pDataSource       : TPersistent       ;
                                     ptiLVTarget       : TtiCustomListView ;
                                     pDataTarget       : TPersistent ) of object ;

  TtiLVDragDropConfirmEvent = procedure( ptiTLSource       : TtiCustomListView   ;
                                         pDataSource       : TPersistent         ;
                                         ptiTLTarget       : TtiCustomListView   ;
                                         pDataTarget       : TPersistent         ;
                                     var pbConfirm : boolean ) of object ;

  TtiDeriveListColumnValue = procedure( const pLV : TtiCustomListView ;
                                        const pData : TPersistent ;
                                        const ptiListColumn : TtiListColumn ;
                                        var   pResult : string ) of object ;

  TtiOnGetCaptionEvent = procedure( const pLV : TtiCustomListView ;
                                        const pData : TPersistent ;
                                        const ptiListColumn : TtiListColumn ;
                                        var   pResult : string ) of object ;

  TtiRuntimeGenColEvent = procedure ( const pColName : string ;
                                      var pAdd : boolean ) of object ;

  TtiLVInfoTipEvent = procedure (const pLV: TtiCustomListView;
                                 const pData: TPersistent;
                                 const pItem: TListItem;
                                 var pInfoTip: string) of object ;

  //----------------------------------------------------------------------------
  TtiLVDragObject = class( TDragObject )
  private
    FItem: TListItem;
    FData: TPersistent;
    FtiListView: TtiCustomListView;
  public
    property  tiListView : TtiCustomListView read FtiListView write FtiListView ;
    property  Data       : TPersistent       read FData       write FData       ;
    property  Item       : TListItem         read FItem       write FItem       ;
  end ;

  // ToDo:
  // There is some cross validation between properties done in the Set methods
  // The F??? values are set, but this means the property inspector is not
  // updated with the new value at design time. Notification of the changes
  // should be send to the property inspector.
  //----------------------------------------------------------------------------
  TtiListColumn = class( TCollectionItem )
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
    procedure   SetFieldName(const Value: string);
    procedure   SetDataType(const Value: TlvTypeKind);
    procedure   SetDerived(const Value: boolean);
    procedure   SetOnDeriveColumn(const Value: TtiDeriveListColumnValue);
    procedure   SetWidth(const Value: integer);
  protected
    function    GetDisplayName : string ; override ;
  published
    property    DisplayLabel : string read FsDisplayLabel write FsDisplayLabel ;
    property    FieldName    : string read FsFieldName    write SetFieldName ;
    property    DisplayMask  : string read FsDisplayMask  write FsDisplayMask ;
    property    DataType     : TlvTypeKind read FDataType write SetDataType ;
    property    Derived      : boolean read FDerived      write SetDerived ;
    property    OnDeriveColumn : TtiDeriveListColumnValue read FOnDeriveColumn write SetOnDeriveColumn ;
    Property    OnGetCaption : TtiOnGetCaptionEvent Read FOnGetCaption Write FOnGetCaption;
    property    Width : integer read FWidth write SetWidth default -1 ;
    property    Alignment : TAlignment read FAlignment write FAlignment default taRightJustify ;
  public
    constructor Create( Collection : TCollection ) ; override ;
    destructor  Destroy ; override ;
    function    Clone : TtiListColumn ;
  end ;

  //----------------------------------------------------------------------------
  TtiListColumns = class( TCollection )
  private
    FOwner : TComponent ;
    function  GetItem( Index : integer ) : TtiListColumn ;
    procedure SetItem( Index : integer ; const Value : TtiListColumn ) ;
  published
  protected
    function  GetOwner : TPersistent ; override ;
  public
    constructor Create( Owner : TComponent ) ;
    destructor  Destroy ; override ;
    property  Items[Index: integer ] : TtiListColumn
                read GetItem write SetItem ;
    function  Add : TtiListColumn ;
    procedure DisplayLabelsToStringList( pSL : TStringList ) ;
    function  FindByDisplayLabel( const pValue : string ) : TtiListColumn ;
    procedure Clear ; reintroduce ;
  end ;

  TlvSortDirection = ( lvsdAscending, lvsdDescending ) ;

  //----------------------------------------------------------------------------
  TlvSortOrder = class( TCollectionItem )
  private
    FsFieldName: string;
    FSortDirection: TlvSortDirection;
  protected
    function GetDisplayName : string ; override ;
  published
    property FieldName : string read FsFieldName write FsFieldName ;
    property SortDirection : TlvSortDirection read FSortDirection write FSortDirection ;
  public
    constructor Create( Collection : TCollection ) ; override ;
    procedure   Assign( source : TPersistent ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TlvSortOrders = class( TCollection )
  private
    FOwner : TComponent ;
    function  GetItem( Index : integer ) : TlvSortOrder ;
    procedure SetItem( Index : integer ; const Value : TlvSortOrder ) ;
  published
  protected
    function  GetOwner : TPersistent ; override ;
  public
    constructor Create( owner : TComponent ) ;
    property    Items[Index: integer ] : TlvSortOrder
                  read GetItem write SetItem ;
    function    Add : TlvSortOrder ;
  end ;

  TtiLVNullDataRow = class( TPersistent )
  private
    FsNoData: string;
  published
    property NoData : string read FsNoData ;
  public
    constructor Create ;
  end ;

  TtiLVNullData = Class( TObjectList )
  public
    constructor Create ;
  end ;

  TtiLVBtnVis = ( tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete ) ;
  TtiLVVisibleButtons = set of TtiLVBtnVis ;

  TtiListViewInternal = class( TListView )
  private
    FOnVScroll: TNotifyEvent;
    FVScrolled : boolean ;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create( Owner : TComponent ) ; override ;
    property    OnVScroll : TNotifyEvent read FOnVScroll write FOnVScroll ;
    property    VScrolled : boolean read FVScrolled write FVScrolled ;
  end ;

  //----------------------------------------------------------------------------
  TtiCtrlBtnPnlAbs = class( TCustomPanel )
  private
    FLVVisibleButtons: TtiLVVisibleButtons;
  protected
    procedure SetVisibleButtons(const Value: TtiLVVisibleButtons); virtual ;
    procedure OnNewClick(Sender : TObject ) ; virtual ;
    procedure OnEditClick(Sender : TObject ) ; virtual ;
    procedure OnDeleteClick( Sender : TObject ) ; virtual ;
    procedure DrawButtons ; virtual ; abstract ;
    procedure EnableButtons ; virtual ; abstract ;
    function  TILV : TtiCustomListView ;
  public
    constructor Create( Owner : TComponent ) ; override ;
    property    VisibleButtons : TtiLVVisibleButtons read FLVVisibleButtons write SetVisibleButtons ;
  published
  end ;

  //----------------------------------------------------------------------------
  TtiCtrlBtnPnlButton = class( TtiCtrlBtnPnlAbs )
  private
    FBtnSize   : integer ;
    FbtnNew    : TtiSpeedButton ;
    FBtnEdit   : TtiSpeedButton ;
    FBtnDelete : TtiSpeedButton ;
  protected
    procedure SetupSpeedButton(const pBtn : TSpeedButton);virtual;
    procedure DrawButtons ; override ;
    procedure EnableButtons ; override ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  TtiCtrlBtnPnlMicroButton = class( TtiCtrlBtnPnlButton )
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  TtiCtrlBtnPnlNormalButton = class( TtiCtrlBtnPnlButton )
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  TtiCtrlBtnPnlLargeButton = class( TtiCtrlBtnPnlButton )
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  TLVButtonStyle = ( lvbsMicroButtons, lvbsNormalButtons, lvbsLargeButtons ) ;

  //----------------------------------------------------------------------------
  TtiCustomListView = class( TtiFocusPanel )
  private
    FLV           : TtiListViewInternal ;
    FData         : TList ;
    FDataInternal : TList ;
    FSelectedData : TList ;
    FNullData     : TtiLVNullData ;

    FCols : TStringList ;
    FPopupMenu : TPopupMenu ;
    FpmiEdit   : TMenuItem ;
    FpmiNew    : TMenuItem ;
    FpmiDelete : TMenuItem ;
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs ;

    FOnItemEdit   : TtiLVItemEditEvent ;
    FOnItemInsert : TtiLVItemEditEvent ;
    FOnItemDelete : TtiLVItemEditEvent ;
    FOnDblClick   : TtiLVItemEditEvent;

    FOnFilterData : TtiLVOnFilterDataEvent ;
    FOnGetImageIndex: TtiLVGetImageIndexEvent;
    FbLoading : boolean ;

    FtiListColumns : TtiListColumns ;
    FbRunTimeGenCols : boolean ;
    FbApplyFilter: boolean;
    FbApplySort: boolean;
    FSortOrders: TlvSortOrders;
    FsLastSortField : string ;
    FsSortField : string ;
    FLastSortDirection : TlvSortDirection ;
    FbSortOnHeadingClick: boolean;
    FListColsBeingDisplayed : TObjectList ;
    FbRunTimeSelectedCols: boolean;
    FAfterRefreshData: TtiLVEvent;
    FTimerSort : TTimer ;
    FOnGetFont: TtiLVOnGetFont;
    FOnItemArive: TtiLVItemEvent;
    FOnItemLeave: TtiLVItemEvent;
    FbSelectFirstRow: boolean;
    FOnLVClick: TtiLVOnClick;
    FbProcessingOnClick : boolean ;
    FLastItem : TListItem ;
    FOnMoveItem: TtiLVOnMoveItem;
    FOnCanInsert : TtiLVCanPerformAction;
    FOnCanEdit   : TtiLVCanPerformAction;
    FOnCanDelete : TtiLVCanPerformAction;
    FOnCanAcceptDrop: TtiLVDragDropConfirmEvent;
    FOnDrop: TtiLVDragDropEvent;
    FCanStartDrag: boolean;
    FReadOnly : boolean ;
    FOnRuntimeGenCol: TtiRuntimeGenColEvent;
    FDestroying : boolean ;
    FButtonStyle: TLVButtonStyle;
    FOnTipInfo: TtiLVInfoTipEvent;

    procedure MenuOnPopup( sender : TObject ) ;

    procedure GetPropertyNames( pPersistent : TObject ;
                                pSL : TStringList ;
                                pPropFilter : TTypeKinds = ctkSimple ) ;

    function  GetSimplePropType( pPersistent: TPersistent;
                                 psPropName: string): TlvTypeKind;

    procedure OnGetRowData(Sender: TObject; Item: TListItem);

    procedure pmiEditOnClick( sender : TObject ) ;
    procedure pmiDeleteOnClick( sender : TObject ) ;
    procedure pmiNewOnClick( sender : TObject ) ;
    procedure SetApplySort(const Value: boolean);
    procedure ConnectToData;
    procedure DisConnectFromData;
    procedure DoOnTimerSort( Sender : TObject ) ;
    procedure DoColumnClick(Sender: TObject; Column: TListColumn);
    procedure DoSortColumn;
    procedure DoCustomDrawItem( Sender : TCustomListView;
                                Item   : TListItem;
                                State  : TCustomDrawState;
                                var DefaultDraw: Boolean);
    function  GetSelectedDataList : TList;
    procedure DoOnClick( Sender: TObject) ;
    procedure DoOnDblClick( Sender : TObject ) ;
    function  GetMultiSelect: boolean;
    procedure SetMultiSelect(const Value: boolean);
    function  GetOnChange: TLVChangeEvent;
    procedure SetOnChange(const Value: TLVChangeEvent);
    function  GetOnChanging: TLVChangingEvent;
    procedure SetOnChanging(const Value: TLVChangingEvent);
    function  GetOnKeyDown: TKeyEvent;
    procedure SetOnKeyDown(const Value: TKeyEvent);
    function  GetOnKeyPress: TKeyPressEvent;
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    function  GetSmallImages: TCustomImageList;
    procedure SetSmallImages(const Value: TCustomImageList);
    function  GetViewStyle: TViewStyle;
    procedure SetViewStyle(const Value: TViewStyle);
    function  GetRowSelect: Boolean;
    procedure SetRowSelect(const Value: Boolean);
    function  GetItems: TListItems;
    function  GetColumns: TListColumns;
    function  GetSelected: TListItem;
    function  GetListView: TListView;
    procedure SetRunTimeGenCols(const Value: boolean);
    function  GetSelectedData: TPersistent;
    function  GetVisibleButtons: TtiLVVisibleButtons;
    procedure SetVisibleButtons(const Value: TtiLVVisibleButtons);
    procedure EnableButtons ;
    function  GetSelectedIndex: integer;
    procedure SetSelectedIndex(const Value: integer);

    // Drag & Drop Sender side events
    procedure DoMouseDown( Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    procedure DoStartDrag( Sender: TObject; var DragObject: TDragObject); Reintroduce ;

    // Drag & Drop Receiver side events
    procedure DoDragOver(  Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(  Sender, Source: TObject; X, Y: Integer);
    procedure SetSelectedData(const Value: TPersistent);
    function  GetOnVScroll: TNotifyEvent;
    procedure SetOnVScroll(const Value: TNotifyEvent);
    procedure DoReSize(Sender : TObject);
    function  AreColsDifferent: boolean;
    function  GetColWidth(pListColumn: TtiListColumn; pLVColumn: TListColumn): Integer;
    procedure DoOnPlnClick(Sender: TObject);
    procedure DrawButtons;
    procedure SetButtonStyle(const Value: TLVButtonStyle);
    function  GetOnInfoTip: TtiLVInfoTipEvent;
    procedure SetOnInfoTip(const Value: TtiLVInfoTipEvent);
    procedure DoTipInfo(Sender: TObject; Item: TListItem;
      var InfoTip: string);
  protected

    procedure   SetData(const Value: TList); virtual ;
    procedure   SetupCols ; virtual ;
    procedure   DoRunTimeGenCols ;
    procedure   Loaded ; override ;
    procedure   DoApplyFilter ; virtual ;
    procedure   DoApplySort   ; virtual ;
    procedure   GetColWidths ;
    procedure   DoAddColumn(pListColumn: TtiListColumn);
    procedure   AddImageColumn;
    procedure   SetApplyFilter(const Value: boolean); virtual ;
    function    GetReadOnly: boolean; virtual ;
    procedure   SetReadOnly(const Value: boolean); virtual ;
    procedure   DoOnSelectItem( Sender : TObject ; Item : TListItem ; Selected : boolean ) ;
    function    ValueFromProperty( ptiListColumn : TtiListColumn ; pData : TPersistent ) : string ;
    function    ValueFromDerivedProperty( ptiListColumn : TtiListColumn ; pData : TPersistent ) : string ;
    function    GetCellText(const piData, piCol: integer): string;
    procedure   SetApplySortNoRefresh(const Value, pRefresh: boolean);
    procedure   SetLVColor;

    property    DataInternal : TList read FDataInternal ;
    property    RunTimeGenCols : boolean
                  read  FbRunTimeGenCols
                  write SetRunTimeGenCols default true ;

    property    Align         ;
    property    Anchors       ;
    property    Constraints   ;
    property    Visible       ;

    property    MultiSelect   : boolean read GetMultiSelect write SetMultiSelect ;
    property    OnDblClick    : TtiLVItemEditEvent read FOnDblClick write FOnDblClick ;
    property    OnChange      : TLVChangeEvent read GetOnChange write SetOnChange ;
    property    OnChanging    : TLVChangingEvent read GetOnChanging write SetOnChanging ;
    property    OnKeyDown     : TKeyEvent read GetOnKeyDown write SetOnKeyDown ;
    property    OnKeyPress    : TKeyPressEvent read GetOnKeyPress write SetOnKeyPress ;
    property    SmallImages   : TCustomImageList read GetSmallImages write SetSmallImages ;
    property    ViewStyle     : TViewStyle read GetViewStyle write SetViewStyle ;
    property    RowSelect     : Boolean read GetRowSelect write SetRowSelect ;

    // These three properties are needed for drag-and-drop
    //property    OnDragOver    : TDragOverEvent read GetOnDragOver  write SetOnDragOver ;
    //property    OnDragDrop    : TDragDropEvent read GetOnDragDrop  write SetOnDragDrop ;
    //property    OnMouseDown   : TMouseEvent    read GetOnMouseDown write SetOnMouseDown ;

    property    OnLVClick : TtiLVOnClick
                  read FOnLVClick
                  write FOnLVClick ;

    property    OnItemEdit   : TtiLVItemEditEvent read FOnItemEdit   write FOnItemEdit   ;
    property    OnItemInsert : TtiLVItemEditEvent read FOnItemInsert write FOnItemInsert ;
    property    OnItemDelete : TtiLVItemEditEvent read FOnItemDelete write FOnItemDelete ;

    property    OnFilterData : TtiLVOnFilterDataEvent
                  read  FOnFilterData
                  write FOnFilterData ;
    property    OnGetFont : TtiLVOnGetFont
                  read  FOnGetFont
                  write FOnGetFont ;

    property    OnGetImageIndex : TtiLVGetImageIndexEvent
                  read FOnGetImageIndex
                  write FOnGetImageIndex ;

    property     SortOrders : TlvSortOrders
                  read FSortOrders
                  write FSortOrders ;
    property    SortOnHeadingClick : boolean
                  read FbSortOnHeadingClick
                  write FbSortOnHeadingClick default true ;
    property    AfterRefreshData : TtiLVEvent
                  read  FAfterRefreshData
                  write FAfterRefreshData ;
    property    OnItemArive : TtiLVItemEvent
                  read FOnItemArive
                  write FOnItemArive ;
    property    OnItemLeave : TtiLVItemEvent
                  read FOnItemLeave
                  write FOnItemLeave ;
    property    SelectFirstRow : boolean read FbSelectFirstRow write FbSelectFirstRow default true ;
    property    OnMoveItem : TtiLVOnMoveItem
                  read  FOnMoveItem
                  write FOnMoveItem ;

    property    VisibleButtons : TtiLVVisibleButtons
                  read GetVisibleButtons
                  write SetVisibleButtons default [] ;
    property   ButtonStyle : TLVButtonStyle read FButtonStyle write SetButtonStyle default lvbsMicroButtons ;

    procedure   SetEnabled( Value : Boolean ) ; override ;

    property    OnCanDelete : TtiLVCanPerformAction read FOnCanDelete write FOnCanDelete ;
    property    OnCanInsert : TtiLVCanPerformAction read FOnCanInsert write FOnCanInsert ;
    property    OnCanEdit   : TtiLVCanPerformAction read FOnCanEdit   write FOnCanEdit   ;

    property    CanStartDrag : boolean read FCanStartDrag write FCanStartDrag ;
    property    OnDrop           : TtiLVDragDropEvent        read FOnDrop        write FOnDrop        ;
    property    OnCanAcceptDrop  : TtiLVDragDropConfirmEvent read FOnCanAcceptDrop write FOnCanAcceptDrop ;

    property    ReadOnly : boolean read GetReadOnly write SetReadOnly default false ;
    property    OnVScroll : TNotifyEvent read GetOnVScroll write SetOnVScroll ;
    property    OnRuntimeGenCol : TtiRuntimeGenColEvent read FOnRuntimeGenCol write FOnRuntimeGenCol ;
    property    OnInfoTip : TtiLVInfoTipEvent read GetOnInfoTip write SetOnInfoTip;
  public
    constructor Create( owner : TComponent ) ; override ;
    destructor  Destroy ; override ;
    procedure   Refresh( pReadData : boolean = true ) ; reintroduce ;

    procedure   DoDelete ;
    procedure   DoEdit ;
    procedure   DoNew ;

    procedure   PositionCursor( piIndex : integer     ) ; overload ;
    procedure   PositionCursor( pData   : TPersistent ) ; overload ;
    procedure   Last ;
    procedure   First ;
    procedure   MoveSelection( piMoveBy : integer ) ;

    function    CanInsert   : boolean ;
    function    CanEdit     : boolean ; reintroduce ;
    function    CanDelete   : boolean ;
    function    CanMoveItem( pItem : TListItem ;
                             piMoveBy : integer ) : boolean ;
    procedure   MoveItem( pItem    : TListItem ;
                          piMoveBy : integer ) ;

    property    ApplyFilter : boolean read FbApplyFilter write SetApplyFilter ;
    property    ApplySort   : boolean read FbApplySort   write SetApplySort ;

    property    Data : TList read FData write SetData ;
    property    Items : TListItems read GetItems ;

    // A ref to the underlying listview.columns property
    property    Columns     : TListColumns   read GetColumns ;
    // The available col mappings
    property    ListColumns : TtiListColumns read FtiListColumns write FtiListColumns ;
    // The col mappings currently being viewed, contains 0..n of TtiListColumn
    // Note: Might contain nil to allow for first col to be an image
    property    ListColsBeingDisplayed : TObjectList    read FListColsBeingDisplayed ;

    property    RunTimeSelectedCols : boolean
                  read FbRunTimeSelectedCols
                  write FbRunTimeSelectedCols ;

    property    SelectedDataList : TList read GetSelectedDataList ;
    property    SelectedData : TPersistent read GetSelectedData write SetSelectedData ;
    property    Selected : TListItem read GetSelected ;
    property    SelectedIndex : integer read GetSelectedIndex write SetSelectedIndex ;

    // Delete a list item, along with the data it points to
    procedure   DeleteData( pListItem : TListItem ) ;

    // This is necessary for drag and drop, and should be replaced by a
    // 'Simple' drop method.
    function    GetItemAt( X, Y : Integer ) : TListItem ;
    procedure   SetFocus ; override ;
    procedure   BeginUpdate ;
    procedure   EndUpdate ;
    procedure   SynchroniseWith( pListView : TtiCustomListView ) ;
    function    AddColumn( const pFieldName : string ;
                           const pDataType  : TlvTypeKind ;
                           const pDisplayLabel : string = '' ;
                           pColWidth : integer = -1 ) : TtiListColumn ;
    procedure   ClearColumns; virtual ;
  published

    Property ShowFocusRect; // AD Jul 2003.

    property    LV : TListView read GetListView ;

  end ;

  TtiListView = class( TtiCustomListView )
  public
    property    OnVScroll ;

  published

//    property    LV  ;

    property    Align         ;
    property    Anchors       ;
    property    MultiSelect   ;
    property    OnDblClick    ;
    property    OnChange      ;
    property    OnChanging    ;
    property    OnKeyDown     ;
    property    OnKeyPress    ;
    property    SmallImages   ;
    property    ViewStyle     ;
    property    RowSelect     ;
    property    Constraints   ;
    property    Visible       ;
    property    OnEnter       ;
    property    OnExit        ;

    // These three properties are needed for drag-and-drop
    //property    OnDragOver  ;
    //property    OnDragDrop  ;
    //property    OnMouseDown ;

    property    OnLVClick ;

    property    OnItemEdit   ;
    property    OnItemInsert ;
    property    OnItemDelete ;
    property    OnFilterData ;
    property    OnGetFont ;

    property    ApplyFilter ;
    property    ApplySort   ;

    property    OnGetImageIndex ;

    property    ListColumns ;
    property    SortOrders ;
    property    SortOnHeadingClick ;
    property    AfterRefreshData ;
    property    OnItemArive ;
    property    OnItemLeave ;
    property    SelectFirstRow ;
    property    OnMoveItem ;
    property    RuntimeGenCols ;
    property    VisibleButtons ;
    property    ButtonStyle ;

    property    OnCanDelete ;
    property    OnCanInsert ;
    property    OnCanEdit   ;

    property    CanStartDrag     ;
    property    OnDrop           ;
    property    OnCanAcceptDrop  ;
    property    ReadOnly ;
    property    OnRuntimeGenCol  ;
    property    OnInfoTip;

  end ;

function tiListViewDisplayMaskFromDataType( const Value : TlvTypeKind ) : string ;

implementation
uses
   SysUtils
   ,Math
   ,Forms
   ,Dialogs
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}

//   ,tiLog // Debugging
   ;

{$R tiListView.res}

var
  // Why do I need a unit wide uSortOrders?
  // Because there is no other way that I can think of to get
  // some data to control the sorting into TList.Sort.
  uSortOrders : TlvSortOrders ;

const
  cMicroButtonHeight  = 12 ;
  cNormalButtonHeight = 24 ;
  cLargeButtonHeight  = 32 ;
  cBtnSpace           =  4 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiCustomListView
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCustomListView.create(owner: TComponent);
const
  cHeight = 97  ;
  cWidth  = 121 ;
begin
  inherited create( owner ) ;
  FDestroying := false ;

  // The listView must be able to AcceptControls to work correctly.
  ControlStyle := ControlStyle + [csAcceptsControls] ;
  self.BevelInner := bvNone ;
  self.BevelOuter := bvNone ;
  Height := cHeight  ;
  Width  := cWidth ;
  OnClick := DoOnPlnClick ;

  FLV             := TtiListViewInternal.Create( self ) ;
//  FLV             := TListView.Create( self ) ;
  FLV.Parent      := self ;
  FLV.ParentFont  := true ;
  FLV.Anchors     := [akLeft,akTop,akRight,akBottom] ;
  FLV.OnDblClick  := DoOnDblClick ;

  FLV.OnMouseDown := DoMouseDown ;
  FLV.OnStartDrag := DoStartDrag ;
  FLV.OnDragOver  := DoDragOver  ;
  FLV.OnDragDrop  := DoDragDrop  ;
  FLV.OnResize    := DoResize ;
  FLV.SetBounds(
    1,
    1,
    cWidth - 2,
    cHeight - 2 );

  FCanStartDrag   := false ;
  FCtrlBtnPnl := TtiCtrlBtnPnlMicroButton.Create(Self);
  FCtrlBtnPnl.Parent := Self ;
  FCtrlBtnPnl.Top := 1 ;
  FCtrlBtnPnl.Left := 1 ;

  FDataInternal := TList.Create ;
  FListColsBeingDisplayed   := TObjectList.Create ;
  FListColsBeingDisplayed.OwnsObjects := true ;

  FbLoading := true ;
  FbRunTimeGenCols := true ;
  FbRunTimeSelectedCols := false ;

  FCols     := TStringList.Create ;

  // Configure the list view
  FLV.ReadOnly         := true ;
  FLV.HideSelection    := false ;
  FLV.ViewStyle        := vsReport ;
  FLV.RowSelect        := true ;
  FLV.ParentFont       := false ;

  // Create the popup menu
  FPopupMenu := TPopupMenu.Create( self ) ;
  FPopupMenu.OnPopup := MenuOnPopup ;
  PopupMenu  := FPopupMenu ;

  // Create select columns menu item
  FpmiEdit          := TMenuItem.Create( self ) ;
  FpmiEdit.Caption  := '&Edit' ;
  FpmiEdit.OnClick  := pmiEditOnClick ;
  FPopupMenu.Items.Add( FpmiEdit ) ;

  FpmiNew          := TMenuItem.Create( self ) ;
  FpmiNew.Caption  := '&New' ;
  FpmiNew.OnClick  := pmiNewOnClick ;
  FPopupMenu.Items.Add( FpmiNew ) ;

  FpmiDelete        := TMenuItem.Create( self ) ;
  FpmiDelete.Caption  := '&Delete' ;
  FpmiDelete.OnClick  := pmiDeleteOnClick ;
  FPopupMenu.Items.Add( FpmiDelete ) ;

  FtiListColumns := TtiListColumns.Create( self ) ;

  FsLastSortField := '' ;
  FLastSortDirection := lvsdAscending ;

  FSortOrders := TlvSortOrders.Create( self ) ;
  FLV.OnColumnClick := DoColumnClick ;
  FbSortOnHeadingClick := true ;

  FLV.OnSelectItem := DoOnSelectItem ;

  // It is somtimes possible that the ListView common control will cause
  // an access violation within the windows dll while re-painting it self.
  // This has been worked around by using a TTimer to pass a message to the
  // control that it must sort the data as the result of a column click, then
  // repaint it self. This functionality would probably have been better
  // achieved by defining a custom message type and sending that to trigger
  // the repaint process.
  FTimerSort := TTimer.Create( nil ) ;
  FTimerSort.Enabled := false ;
  FTimerSort.Interval := 10 ;
  FTimerSort.OnTimer := DoOnTimerSort ;

  // Likewise, when the OnGetFont property is set, somtimes, the font of the
  // first row of data in the listView is set to bold. I can't work out why, or
  // or how to reproduce this but a timer which triggers a move of the cursor
  // 15ms after the data property has been set works around the problem.
//  FTimerSetData := TTimer.Create( nil ) ;
//  FTimerSetData.Enabled := false ;
//  FTimerSetData.Interval := 15 ;
//  FTimerSetData.OnTimer := DoOnTimerSetData ;

  // A TList to hold pointer to the data objects associated with selected nodes
  FSelectedData := TList.Create ;

  FNullData := TtiLVNullData.Create ;
  //Data      := FNullData ;

  FbSelectFirstRow := true ;

  FLV.OnClick := DoOnClick ;

  FLastItem := nil ;

  //FbProcessingOnClick := false ;
  //FLV.OnMouseDown := DoOnMouseDown ;
//  FLV.OnMouseDown   := DoOnMouseUp ;
  //FLV.OnClick := DoMouseClick ;
end;

//------------------------------------------------------------------------------
destructor TtiCustomListView.Destroy ;
begin
  FDestroying := true ;
  FData := nil ;
  FLV.Free ;
  FCols.Free ;
  FtiListColumns.Free ;
  FDataInternal.Free ;
  FSortOrders.Free ;
  FListColsBeingDisplayed.Free ;
  FTimerSort.Free ;
  FSelectedData.Free ;
  FNullData.Free ;
  inherited ;
end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.Loaded;
begin
  inherited;
  if FbLoading then begin
    // NilData is causing problem with design time cols
    // Data := FNilData ;
    //FbRunTimeGenCols := FtiListColumns.Count = 0 ;
    FbLoading := false ;
  end ;
  DrawButtons ;
  EnableButtons ;
  MenuOnPopup( nil ) ;
end;

procedure TtiCustomListView.DrawButtons;
begin
  Assert(FCtrlBtnPnl <> nil, 'FCtrlBtnPnl not assigned' ) ;
  FCtrlBtnPnl.DrawButtons ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.OnGetRowData(Sender: TObject; Item: TListItem);
var
  i            : integer ;
  lData        : TPersistent ;
  liImageIndex : integer ;
begin

  try
    if FData = nil then
      exit ; //==>

    Assert( Item <> nil,  'Nil list item' ) ;
    Assert( FData <> nil, 'Data property not set' ) ;
    // Added this to fix a problem with PositionCursor( 0 ) when there where
    // 3 items in the list. The root cause of the proble is unknown, and this
    // is a workaroudn.
    if FDataInternal.Count-1 < Item.Index then
      Exit ; //==>
    Assert( FDataInternal.Count-1 >= Item.Index , 'Item.Index > FDataInternal.Count' ) ;
//    Assert( FDataInternal.Items[Item.Index] <> nil, 'Nil data item in FDataInternal' ) ;

//    Assert( TObject( FDataInternal.Items[Item.Index]) is TPersistent,
//            'Data object is not TPersistent, it''s a ' +
//            TObject( FDataInternal.Items[Item.Index] ).ClassName ) ;

//    lData := TObject( FDataInternal.Items[ Item.Index ]) as TPersistent ;
    lData := TPersistent( FDataInternal.Items[ Item.Index ]) ;

    for i := 0 to FtiListColumns.Count - 1 do
      Item.SubItems.Add( '' ) ;

    if lData = nil then
      Exit ; //==>

    Item.Data := lData ;

{
    Assert( lData <> nil,
            'Nil data object' ) ;
    Item.Data := lData ;

    for i := 0 to FtiListColumns.Count - 1 do
      Item.SubItems.Add( '' ) ;
}
    for i := 1 to Columns.Count - 1 do
      Item.SubItems[i-1] :=
        GetCellText( Item.Index, i-1 ) ;

    if Assigned( FOnGetImageIndex ) and
       Assigned( SmallImages ) then begin
      liImageIndex := -1 ;
      FOnGetImageIndex( lData, liImageIndex ) ;
      Item.ImageIndex    := liImageIndex ;
    end ;
  except
        on e:exception do
          raise exception.create( 'Error in ' + Name + ': ' + ClassName +
                                  '.OnGetRowData: ' + e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.SetData(const Value: TList ) ;
begin

  if FData = Value then
    Exit ; //==>

  DisConnectFromData ;

  // NilData is causing problems with design time cols
  //  if Value = nil then
  //    FData := FNilData
  //  else
  FData := Value ;

  if (FData = nil) or
     (FData.Count<1) then
  begin
    if not FDestroying then
    begin
      EnableButtons ;
      FDataInternal.Clear ; // SetupCols refers to this so must reset now if no data
      Refresh( False ); { This has been included in an attempt to force a repaint.  If readonly was *reset* (off)
                          and no items needed display, the readonly color would remain.  Also tried FLV.Invalidate
                          in SetColor to no avail.  Alas, the problem alludes me... ipk 2003-10-14}
      SetupCols ;
    end;
    Exit ; //==>
  end ;

  Refresh ;

  // Turned this off 31/08/2000. I remember fixing the problem in the TtiTreeView -
  // it was somthing to do with using the wrong canvas. Don't remember fixing it in the
  // TtiCustomListView, but it appears to have gone away, for the time begin at least...

  // Turned it back on 02/01/2001
  //FTimerSetData.Enabled := true ;
  if FbSelectFirstRow then
    PositionCursor( 0 ) ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.refresh( pReadData : boolean = true ) ;
var
  lCursor : TCursor ;
begin
  // Do not re-read the data, only refresh the current row.
  if not pReadData then begin
    FLV.Refresh ;
    Exit ; //==>
  end ;

  // Re-read all the data, then repaint the listView
  lCursor := Screen.Cursor ;
  Screen.Cursor := crHourGlass ;
  FLV.OnData    := nil ;
  Visible := false ;
  Application.ProcessMessages ;
  try
    DisConnectFromData ;
    DoApplyFilter ;
    DoApplySort ;
    ConnectToData ;
    RePaint ;
    if Assigned( FAfterRefreshData ) then
      FAfterRefreshData( self ) ;

  finally
    Visible := true ;
    Screen.Cursor := lCursor ;
  end ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DisConnectFromData ;
begin
  if ( not Assigned( FLV.OnData )) and
     ( not FLV.OwnerData ) and
     ( FLV.Items.Count = 0 ) then
    exit ; //==>

  if FbRunTimeGenCols then
    FtiListColumns.Clear ;

  FLV.OnCustomDrawItem := nil ;
  FLV.OnData    := nil ;
  FLV.OwnerData := false ;
  //FLV.Columns.Clear ;
  //ColMappings.Clear ;
  FLV.Items.Clear ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.ConnectToData ;
begin
//  if FDataInternal.Count > 0 then begin

    // The order of these three assignments is important.
    // Do not change the order.
    // Must set OwnerData, AutoGenCols, Items.Count, OnData order.

    FLV.OwnerData   := true ;

    if FbRunTimeGenCols then
      DoRunTimeGenCols ;

    SetupCols ;

    FLV.OnData      := OnGetRowData ;
    Items.Count := FDataInternal.Count ;
    if Assigned( FOnGetFont ) then
      FLV.OnCustomDrawItem :=  DoCustomDrawItem ;

//  end ;
end ;

// There where no columns specified at design time, so setup the
// columns at runtime.
//------------------------------------------------------------------------------
procedure TtiCustomListView.DoRunTimeGenCols ;
var
  i : integer;
  lsColName : string ;
  lCol : TtiListColumn ;
  lData : TObject ;
  lAdd : boolean ;
begin

  ClearColumns;
  //FColMappings.Clear ;
  //FtiListColumns.Clear ;

  // Read all the available cols into the col string list
  if ( Data = nil ) or
     ( Data.Count = 0 ) then
    Exit ; //==>

  // Find the first, non-nil data item
  lData := nil ;
  for i := 0 to Data.Count - 1 do
    if Data.Items[i] <> nil then
    begin
      lData := Data.Items[i] ;
      Break ; //==>
    end ;

  Assert( lData is TPersistent, 'Not a list of TPersistent' ) ;

  GetPropertyNames( TPersistent( lData ), FCols ) ;

  // Read the column headings
  for i := 0 to FCols.count - 1 do
  begin
    lsColName := FCols.Strings[i] ;
    if Assigned( FOnRuntimeGenCol ) then
    begin
       lAdd := true ;
       FOnRuntimeGenCol( lsColName, lAdd ) ;
       if not lAdd then
         Continue ; //==>
    end ;

    if ( lsColName <> '' ) then
    begin
      lCol := FtiListColumns.Add ;
      lCol.DisplayLabel := lsColName ;
      lCol.FieldName    := lsColName ;
      lCol.DataType     := GetSimplePropType( TPersistent( lData ), lsColName ) ;
    end ;
  end ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.SetupCols ;
var
  i : integer;
begin
  if not AreColsDifferent then begin
    GetColWidths ;
    Exit ; //==>
    end;
  FListColsBeingDisplayed.Clear ;
  FLV.Columns.Clear ;
  AddImageColumn ;
  // Read the column headings from FtiListColumns
  for i := 0 to FtiListColumns.Count - 1 do
    DoAddColumn( FtiListColumns.Items[i] ) ;

  GetColWidths ;

end;

function TtiCustomListView.AreColsDifferent : boolean ;
var
  i : integer ;
begin
  result :=
    ( FData = nil ) or
    ( FData.Count < 1 ) or
    ( FtiListColumns.Count <> FLV.Columns.Count - 1 ) or
    ( FListColsBeingDisplayed.Count <> FtiListColumns.Count ) or
    ( FtiListColumns.Count <> FLV.Columns.Count - 1 ) ;
  if result then
    Exit ; //==>
  for i := 0 to FtiListColumns.Count - 1 do
  begin
    if ( FLV.Columns[i+1].Alignment <> FtiListColumns.Items[i].Alignment ) or
       ( FLV.Columns[i+1].Caption   <> FtiListColumns.Items[i].DisplayLabel ) then
    begin
      result := true ;
      Exit ; //==>
    end;
  end ;
end;

procedure TtiCustomListView.AddImageColumn ;
var
  lCol : TListColumn ;
begin

  // Note: Col[0] will always be left justified, so we add a dummy col
  //       at [0] to hold the image only (if there is one)
  lCol := Columns.Add ;
  lCol.Caption   := '' ;
  lCol.AutoSize  := false ;

  if Assigned( SmallImages ) then
    Columns[Columns.Count-1].Width := SmallImages.Width + 2
  else
    Columns[Columns.Count-1].Width := 1 ;

  FListColsBeingDisplayed.Add( nil ) ;

end ;

procedure TtiCustomListView.DoAddColumn( pListColumn : TtiListColumn ) ;
var
  lCol : TListColumn ;
begin
  lCol           := Columns.Add ;
  lCol.AutoSize  := false ;
  lCol.Alignment := pListColumn.Alignment ;
  lCol.Caption   := pListColumn.DisplayLabel ;
  lCol.Width     := GetColWidth( pListColumn, lCol ) ;
  FListColsBeingDisplayed.Add( pListColumn.Clone ) ;
end;

function TtiCustomListView.GetColWidth( pListColumn : TtiListColumn ; pLVColumn : TListColumn ):Integer ;
begin
  if pListColumn.Width = -1 then
    result := Canvas.TextWidth( pLVColumn.Caption ) + 12
  else
    result := pListColumn.Width ;
end ;

procedure TtiCustomListView.DoOnPlnClick(Sender: TObject);
begin
  if Enabled and ( Not Focused ) then
    SetFocus ;
end ;

procedure TtiCustomListView.GetColWidths ;
var
  i : integer ;
  j : integer ;
  laWidths : Array of integer ;
  liTextWidth : integer ;
begin

  SetLength( laWidths, Columns.Count ) ;

  // Read the heading widths (remember, the first col is a dummy for the image)
  for j := 1 to Columns.Count - 1 do begin
    if {not RunTimeGenCols and} FtiListColumns.Items[j-1].Width = -1 then
    begin
      liTextWidth := Canvas.TextWidth(Columns[j].Caption ) ;
      laWidths[j] := Max( laWidths[j], liTextWidth ) ;
    end ;
  end ;

  // Read the data widths (remember, the first col is a dummy for the image)
  for i := 0 to FDataInternal.Count - 1 do
    for j := 1 to Columns.Count - 1 do
      if FtiListColumns.Items[j-1].Width = -1 then
        laWidths[j] :=
          Max( Canvas.TextWidth( GetCellText( i, j-1 )), laWidths[j] ) ;

  // Now set the column widths to match the calculated values.
  for j := 1 to Columns.Count - 1 do
    if {not RunTimeGenCols and} FtiListColumns.Items[j-1].Width = -1 then
      Columns[j].Width := laWidths[j] + 12
    else
      Columns[j].Width := FtiListColumns.Items[j-1].Width ;

end ;

function TtiCustomListView.ValueFromProperty( ptiListColumn : TtiListColumn ; pData : TPersistent ) : string ;
var
  lsFieldName   : string ;
  lCurrency     : Currency;
  lFloat        : real ;
  lInt          : integer ;
  lDate         : TDateTime ;
  lsDisplayMask : string ;
begin
  If Assigned(pTiListColumn.FOnGetCaption) Then
  Begin
    Try
      ptiListColumn.FOnGetCaption(Self, pData, ptiListColumn, Result);
    Except
      On E : Exception Do
        Result := 'Error in OnGetCaption. Message : ' + E.Message;
    End;
  End
  Else
  Begin
  lsFieldName   := ptiListColumn.FieldName ;
  lsDisplayMask := ptiListColumn.DisplayMask ;
  try
    case ptiListColumn.DataType of
    lvtkString   : result := GetPropValue( pData, lsFieldName ) ;
    lvtkCurrency : Begin
                     lCurrency := GetPropValue( pData, lsFieldName ) ;
                     Result := Format('%m',[lCurrency]);
                   End;
    lvtkFloat    : begin
                     lFloat := GetPropValue( pData, lsFieldName ) ;
                     result := FormatFloat( lsDisplayMask, lFloat ) ;
                   end ;
    lvtkDateTime : begin
                     lDate  := GetPropValue( pData, lsFieldName ) ;
                     result := FormatDateTime( lsDisplayMask, lDate ) ;
                   end ;
    lvtkInt      : begin
                     lInt   := GetPropValue( pData, lsFieldName ) ;
                     result := FormatFloat( lsDisplayMask, lInt ) ;
                   end ;
    end ;
  except
    on e:exception do
      result := 'Error reading <' + lsFieldName +
                '> in _ValueFromProperty. Message:' +
                e.message ;
  end ;
end ;
end ;

function TtiCustomListView.ValueFromDerivedProperty( ptiListColumn : TtiListColumn ; pData : TPersistent ) : string ;
begin
  try
    if Assigned( ptiListColumn.OnDeriveColumn ) then
    begin
      result := '' ;
      ptiListColumn.OnDeriveColumn(
        Self,
        pData,
        ptiListColumn,
        result ) ;
    end
    else
      result := 'Unknown' ;
  except
    on e:exception do
      result := 'Error reading column <' + ptiListColumn.DisplayLabel +
                '> in _ValueFromDerivedProperty. Message:' +
                e.message ;
  end ;
end ;



//------------------------------------------------------------------------------
function TtiCustomListView.GetCellText( const piData, piCol : integer ) : string ;
var
  ltiListColumn : TtiListColumn ;
  lData         : TPersistent ;
begin

  result := '' ;
  Assert(piCol+2 <= FListColsBeingDisplayed.Count,
         'FListColsBeingDisplayed has ' + IntToStr(FListColsBeingDisplayed.Count) +
         ' items but trying to access index ' + IntToStr(piCol+1));
  ltiListColumn := TtiListColumn( FListColsBeingDisplayed.Items[piCol+1] ) ;
  Assert( ltiListColumn <> nil, 'Can not find tiListColumn for index ' + IntToStr( piCol+1));
  lData         := TPersistent( FDataInternal.Items[piData] ) ;
  if lData = nil then
    result := ''
  else if not ltiListColumn.Derived then
    result := ValueFromProperty( ltiListColumn, lData )
  else
    result := ValueFromDerivedProperty( ltiListColumn, lData ) ;

end ;

// Derive the column alignment from the col's data type.
// Note: Col[0] will always be left justified.
//------------------------------------------------------------------------------
{
function TtiCustomListView.GetColAlignment( psColName : string ) : TAlignment ;
var
  lPropType : TTypeKind ;
  lData : TPersistent ;
begin
  result := taLeftJustify ;
  if ( Data = nil ) or
     ( Data.Count = 0 ) or
     ( Data.Items[0] = nil ) then
    exit ; //==>

  Assert( TObject(Data.Items[0]) is TPersistent,
            'Object in list passed as data to TtiCustomListView not TPersistent' ) ;

  lData := ( TObject(Data.Items[0]) as TPersistent ) ;

  try
    lPropType := PropType( lData, psColName ) ;
  except
    on e:exception do
      raise exception.create( 'Error getting property type for <' +
                              psColName + '> from <' +
                              lData.ClassName + '> Message: ' + e.message ) ;
  end ;

  if lPropType in [ tkInteger, tkInt64, tkFloat ] then
    result := taRightJustify ;

end ;
}

//------------------------------------------------------------------------------
procedure TtiCustomListView.GetPropertyNames( pPersistent: TObject;pSL: TStringList;
                                        pPropFilter: TTypeKinds);
var
  lCount : integer ;
  lSize  : integer ;
  lList  : PPropList ;
  i : integer ;
  lPropFilter : TTypeKinds ;
begin
  Assert( pPersistent <> nil, 'pPersistent not assigned.' ) ;
  Assert( pSL <> nil, 'pSL not assigned.' ) ;
  lPropFilter := pPropFilter ;
  pSL.Clear ;
  lCount := GetPropList(pPersistent.ClassInfo, lPropFilter, nil);
  lSize := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
    GetPropList(pPersistent.ClassInfo, lPropFilter, lList);
    for i := 0 to lcount - 1 do
      psl.add( lList[i].Name ) ;
  finally
    FreeMem( lList, lSize ) ;
  end ;
end;

//------------------------------------------------------------------------------
function TtiCustomListView.GetSimplePropType( pPersistent : TPersistent ;
                                        psPropName : string ) : TlvTypeKind ;
var
  lPropType : TTypeKind ;
begin

  try
    lPropType := PropType( pPersistent, psPropName ) ;
  except
    on e:exception do
      raise exception.create( 'Error in tiGetSimpleTypeKind ' +
                              'Message: ' + e.message ) ;
  end ;

  case lPropType of
  tkInteger,
  tkInt64,
  tkEnumeration : result := lvtkInt ;

  tkFloat       : result := lvtkFloat ;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  tkWString     : result := lvtkString ;

  else
    raise exception.create( 'Invalid property type passed to ' +
                            'tiGetSimpleTypeKind' ) ;
  end ;

end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.pmiDeleteOnClick(sender: TObject);
begin
  DoDelete ;
end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoDelete ;
var
  liSelected : integer ;
  lSelectedData : TPersistent ;
begin
  if Selected = nil then
    Exit ; //==>
  if SelectedData = nil then
    Exit ; //==>
  if not Assigned( FOnItemDelete ) then
    Exit ; //==>

  try
    liSelected := FLV.Selected.Index ;
    lSelectedData := SelectedData ;
    FOnItemDelete( Self, lSelectedData, Selected ) ;
    // After deleting an object, we must refresh the entire list view, then
    // position the cursor as close as possible to the object we just deleted.
    Refresh ;
    SetFocus ;
    // PositionCursor is still a little buggy
    PositionCursor( liSelected ) ;
  except
    on e:exception do
      raise exception.create( 'Error in ' + ClassName + 'DoDelete. Message: ' +
                              e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.pmiEditOnClick(sender: TObject);
begin
  DoEdit ;
end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoEdit ;
var
  lSelectedData : TPersistent ;
begin
  if Selected = nil then
    Exit ; //==>
  if SelectedData = nil then
    Exit ; //==>
  if not Assigned( FOnItemEdit ) then
    Exit ; //==>

  try
    lSelectedData := SelectedData ;

    BeginUpdate ;
    try
      FOnItemEdit( Self, lSelectedData, FLV.Selected ) ;
      // After an edit, we want to leave the cursor on the same object, but
      // refresh the row in the list view.
      Refresh( false ) ;
    finally
      EndUpdate ;
    end ;
    if CanFocus then
      SetFocus ;
  except
    on e:exception do
      raise exception.create( 'Error in ' + ClassName + 'DoEdit. Message: ' +
                              e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.pmiNewOnClick(sender: TObject);
begin
  DoNew ;
end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoNew ;
var
  lData : TPersistent ;
begin
  if not Assigned( FOnItemInsert ) then
    Exit ; //==>
  try
    lData := nil ;
    BeginUpdate ;
    try
      FOnItemInsert( Self, lData, Nil ) ;
      Refresh( true ) ;
    finally
      EndUpdate ;
    end ;
    // After inserting a new record, we must refresh the entire list view,
    // then position the cursor on the newly inserted object.
    SetFocus ;
    // PositionCursor is still a little buggy
    //PositionCursor( lData ) ;
    Last ;
  except
    on e:exception do
      raise exception.create( 'Error in ' + ClassName + 'DoNew. Message: ' +
                              e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.MenuOnPopup(sender: TObject);
begin
  FpmiEdit.Visible   := Assigned( FOnItemEdit ) ;
  FpmiNew.Visible    := Assigned( FOnItemInsert ) ;
  FpmiDelete.Visible := Assigned( FOnItemDelete ) ;

  if FpmiEdit.Visible then
    FpmiEdit.Shortcut := TextToShortcut( 'Enter' )
  else
    FpmiEdit.Shortcut := TextToShortcut( '' ) ;

  if FpmiNew.Visible then
    FpmiNew.Shortcut := TextToShortcut( 'Ins' )
  else
    FpmiNew.Shortcut := TextToShortcut( '' ) ;

  if FpmiDelete.Visible then
    FpmiDelete.Shortcut := TextToShortcut( 'Del' ) 
  else
    FpmiDelete.Shortcut := TextToShortcut( '' ) ;

  FpmiEdit.Enabled   := CanEdit ;
  FpmiDelete.Enabled := CanDelete ;
  FpmiNew.Enabled    := CanInsert ;

end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.PositionCursor( piIndex : integer ) ;
//var
//  liItemHeight : integer ;
begin

  if ( piIndex > Items.Count-1 ) or
     ( piIndex < 0 ) then
    Exit ; //==>

{
  liItemHeight := Canvas.TextHeight( 'M' ) + 1 ;
  FLV.Scroll( 0, liItemHeight * FLV.TopItem.Index * -1 ) ;

  if piIndex > FLV.VisibleRowCount then
    FLV.Scroll( 0, liItemHeight * piIndex ) ;

  FLV.Selected := Items[piIndex] ;
  FLV.Selected.MakeVisible( false ) ;
}

  FLV.Selected := nil ;
  FLV.Selected := Items[piIndex] ;
  FLV.Selected.MakeVisible( false ) ;

end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.SetApplyFilter(const Value: boolean);
begin
  FbApplyFilter := Value;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiListColumn
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//------------------------------------------------------------------------------
constructor TtiListColumn.Create(Collection: TCollection);
begin
  inherited Create( Collection ) ;
  FsDisplayLabel := crsDefaultColDisplayLabel ;
  FsFieldName    := crsDefaultColFieldName ;
  FsDisplayMask  := tiListViewDisplayMaskFromDataType( lvtkString ) ;
  FDataType      := lvtkString ;
  FDerived       := false ;
  FWidth         := -1 ;
  FAlignment     := taRightJustify ;
end;

//------------------------------------------------------------------------------
destructor TtiListColumn.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
function TtiListColumn.Clone: TtiListColumn;
begin
  result                := TtiListColumn.Create( nil ) ;
  result.DisplayName    := self.DisplayName  ;
  result.DisplayLabel   := self.DisplayLabel ;
  result.FieldName      := self.FieldName    ;
  result.DisplayMask    := self.DisplayMask  ;
  result.DataType       := self.DataType     ;
  result.Derived        := self.Derived      ;
  result.FOnGetCaption  := self.FOnGetCaption;
  result.OnDeriveColumn := self.OnDeriveColumn ;
  result.Width          := self.Width ;
  result.Alignment      := self.Alignment ;
end;

//------------------------------------------------------------------------------
{
procedure TtiListColumn.Assign(Source: TPersistent);
var
  lData : TtiListColumn ;
begin
  Assert( Source is TtiListColumn, 'Source not TtiListColumn' ) ;
  lData := TtiListColumn( Source )   ;
  inherited Assign( TPersistent( lData )) ;
  DisplayName  := lData.DisplayName  ;
  DisplayLabel := lData.DisplayLabel ;
  FieldName    := lData.FieldName    ;
  DisplayMask  := lData.DisplayMask  ;
  DataType     := lData.DataType     ;
end;
}
//------------------------------------------------------------------------------
function TtiListColumn.GetDisplayName: string;
begin
  result := DisplayLabel ;
end;

//------------------------------------------------------------------------------
procedure TtiListColumn.SetDataType(const Value: TlvTypeKind);
begin
  if DisplayMask = tiListViewDisplayMaskFromDataType( FDataType ) then
    DisplayMask := tiListViewDisplayMaskFromDataType( Value ) ;
  FDataType := Value;

  case Value of
  lvtkString, lvtkDateTime : Alignment := taLeftJustify ;
  lvtkInt, lvtkFloat, lvtkCurrency : Alignment := taRightJustify ;
  else
    raise exception.Create( 'Invalid TlvTypeKind passed to ' + ClassName + '.SetDataType' );
  end ;

end;

//------------------------------------------------------------------------------
// The mask for the currency type is purely for reference. The format function
// in GetCellText will automatically choose the correct currency format based on
// the Locale set in Windows.
function tiListViewDisplayMaskFromDataType( const Value : TlvTypeKind ) : string ;
begin
  case Value of
  lvtkString   : result := '' ;
  lvtkInt      : result := '#,##0' ;
  lvtkFloat    : result := '#,##0.000' ;
  lvtkDateTime : result := 'dd/mm/yyyy' ;
  lvtkCurrency : Result := '#,##0.00';
  else
    Assert( false, 'Invalid DataType' ) ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure TtiListColumn.SetFieldName(const Value: string);
begin
  if not FDerived then
  begin
    Assert( Value <> '', 'Can not assign empty field name.' ) ;
  end ;
  if FsDisplayLabel = FsFieldName then
    FsDisplayLabel := Value ;
  FsFieldName := Value;
//  end
//  else
//  begin
//    FsFieldName := '' ;
//    FsDisplayLabel := '' ;
//  end;
end;

//------------------------------------------------------------------------------
procedure TtiListColumn.SetDerived(const Value: boolean);
begin
  FDerived := Value;
{
  if FDerived then
  begin
    FsFieldName      := '' ;
    FsDisplayMask    := '' ;
    FDataType       := lvtkString ;
//    Derived        :=
//    OnDeriveColumn := nil ;
  end
  else
  begin
    if FsFieldName = '' then
      FsFieldName      := 'Caption' ;
    if FsDisplayMask = '' then
      FsDisplayMask    := 'Caption' ;
    FDataType       := lvtkString ;
    FOnDeriveColumn := nil ;
  end ;
}
end;

//------------------------------------------------------------------------------
procedure TtiListColumn.SetOnDeriveColumn(const Value: TtiDeriveListColumnValue);
begin
  FOnDeriveColumn := Value;
{
  if Assigned( FOnDeriveColumn ) then
  begin
    FDerived          := true ;
    FsFieldName      := '' ;
    FsDisplayMask    := '' ;
    FDataType        := lvtkString ;
//    OnDeriveColumn :=
  end
  else
  begin
//    FieldName      := '' ;
//    DisplayMask    := '' ;
//    DataType       := lvtkString ;
    FDerived        := false ;
//    FOnDeriveColumn := nil ;
  end ;
}  
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiListColumns
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiListColumns.Add: TtiListColumn;
begin
  result := TtiListColumn( inherited add ) ;
  result.Collection := Self ;
  Assert( FOwner is TtiCustomListView, 'Owner not a TtiCustomListView');
//  if csDesigning in TtiCustomListView(Owner).ComponentState then
//    TtiCustomListView(Owner).SetupCols ;
//  TtiCustomListView(Owner).RunTimeGenCols := false ;
end;

procedure TtiListColumns.Clear;
begin
  inherited;
end;

//constructor TtiListColumns.Create(pListView: TtiCustomListView);
constructor TtiListColumns.Create( Owner : TComponent ) ;
begin
  inherited Create( TtiListColumn ) ;
  FOwner := Owner ;
end;

//------------------------------------------------------------------------------
destructor TtiListColumns.Destroy;
begin
  inherited;
end;

function TtiListColumns.FindByDisplayLabel( const pValue : string): TtiListColumn;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].DisplayLabel = pValue then begin
      result := Items[i] ;
      break ; //==>
    end ;
end;

//------------------------------------------------------------------------------
function TtiListColumns.GetItem(Index: integer): TtiListColumn;
begin
  result := TtiListColumn( inherited GetItem( Index )) ;
end;

//------------------------------------------------------------------------------
function TtiListColumns.GetOwner: TPersistent;
begin
  result := FOwner ;
end;

//------------------------------------------------------------------------------
procedure TtiListColumns.DisplayLabelsToStringList(pSL: TStringList);
var
  i : integer ;
begin
  pSL.Clear ;
  for i := 0 to count - 1 do
    pSL.Add( Items[i].DisplayLabel ) ;
end;

//------------------------------------------------------------------------------
procedure TtiListColumns.SetItem(Index: integer;const Value: TtiListColumn);
begin
  inherited SetItem( Index, Value ) ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoApplyFilter ;
var
  i         : integer ;
  lData     : TPersistent ;
  lbInclude : boolean ;
begin

  FDataInternal.Clear ;
  if FData = nil then
    exit ; //==>

  for i := 0 to FData.Count - 1 do begin
    if ( Assigned( FOnFilterData )) and
       ( FbApplyFilter ) then begin
      Assert( TObject( FData.Items[i] ) is TPersistent, 'Data not TPersistent' ) ;
      lData := TPersistent( FData.Items[i] ) ;
      FOnFilterData( lData, lbInclude ) ;
      if lbInclude then
        FDataInternal.Add( lData ) ;
    end else
      FDataInternal.Add( FData.Items[i] ) ;
  end ;


end ;

//------------------------------------------------------------------------------
function DoSortData(pData1, pData2: Pointer): integer;
  procedure _DoRaiseException( pFieldName : string ; pClassName : string ) ;
  begin
    raise exception.Create( 'Unable to read field <' +
                            pFieldName + '> from <' +
                            pClassName + '> in _DoSortData()' ) ;
  end ;

  function _DoSortData( pData1, pData2 : TPersistent ;
                        pSortOrder : TlvSortOrder ) : integer ;
  var
    lVal1  : variant ;
    lVal2  : variant ;
  begin
    try
      lVal1 := GetPropValue( pData1, pSortOrder.FieldName, false ) ;
      if VarIsNull( lVal1 ) then
        _DoRaiseException( pSortOrder.FieldName, pData1.ClassName ) ;
    except
      on e:exception do
        _DoRaiseException( pSortOrder.FieldName, pData1.ClassName ) ;
    end ;

    try
      lVal2 := GetPropValue( pData2, pSortOrder.FieldName, false ) ;
      if VarIsNull( lVal2 ) then
        _DoRaiseException( pSortOrder.FieldName, pData2.ClassName ) ;
    except
      on e:exception do
        _DoRaiseException( pSortOrder.FieldName, pData2.ClassName ) ;
    end ;

    if lVal1 < lVal2 then
      result := -1
    else if lVal1 > lVal2 then
      result := 1
    else
      result := 0 ;

    case pSortOrder.SortDirection of
    lvsdAscending  : result := result * 1 ;  // Do nothing
    lvsdDescending : result := result * -1 ; // Descending sort order
    else
      Assert( false, 'Invalid sort direction.' ) ;
    end ;

  end ;
var
  lData1 : TPersistent ;
  lData2 : TPersistent ;
  i      : integer ;
begin
  Assert( TObject( pData1 ) is TPersistent, 'pData1 not TPersistent' ) ;
  Assert( TObject( pData2 ) is TPersistent, 'pData2 not TPersistent' ) ;

  lData1 := TPersistent( pData1 ) ;
  lData2 := TPersistent( pData2 ) ;

  result := 0 ;
  for i := 0 to uSortOrders.Count - 1 do begin
    result := _DoSortData( lData1, lData2, uSortOrders.Items[i] ) ;
    if result <> 0 then
      break ; //==>
  end ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoApplySort ;
var
  i : integer ;
  lSortOrder : TlvSortOrder ;
begin

  if not ApplySort then
    exit ; // ==>

  if FData = nil then
    exit ; //==>

  if FData.Count = 0 then
    exit ; //==>

  if FSortOrders.Count = 0 then begin
    FbApplySort := False ;
    raise exception.Create( 'No sort orders defined' ) ;
  end ;

  // uSortOrders.Assign not working, so use this hack
  // uSortOrders.Assign( FSortOrders ) ;
  uSortOrders.Clear ;
  for i := 0 to FSortOrders.Count - 1 do begin
    lSortOrder               := uSortOrders.Add ;
    lSortOrder.FieldName     := FSortOrders.Items[i].FieldName ;
    lSortOrder.SortDirection := FSortOrders.Items[i].SortDirection ;
  end ;

  FDataInternal.Sort( DoSortData ) ;

end ;

//------------------------------------------------------------------------------
procedure TtiCustomListView.SetApplySort(const Value: boolean);
begin
  SetApplySortNoRefresh(Value, true);
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = 0 then
    exit ; //==>

  if not FbSortOnHeadingClick then
    Exit ; //==>
    
  // Can not currently sort on a derived column, sorting by column click
  // available on simple field types only.
  if TtiListColumn( FListColsBeingDisplayed.Items[Column.Index] ).Derived then
  begin
    MessageDlg( 'Sorry, you can not sort on this column.',
                mtError,
                [mbOK], 0 ) ;
    Exit ; //==>
  end ;

{ We used to save the Column ref itself, but that was not guarranteed
  to be valid by the time the DoSortColumn event got fired. }
  FsSortField :=
    TtiListColumn( FListColsBeingDisplayed.Items[Column.Index] ).FieldName ;
  FTimerSort.Enabled := true ;

end ;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.DoSortColumn ;
var
  lSortDirection : TlvSortDirection ;
  lSortOrder : TlvSortOrder ;
begin

  if not FbSortOnHeadingClick then
    exit ; //==>

  DisconnectFromData ;

  if FsLastSortField = FsSortField  then begin
    lSortDirection := lvsdAscending ;
    case FLastSortDirection of
    lvsdAscending  : lSortDirection := lvsdDescending ;
    lvsdDescending : lSortDirection := lvsdAscending ;
    else
      Assert( false, 'Invalid sort direction.' ) ;
    end ;
  end else
    lSortDirection := lvsdAscending ;

  FsLastSortField    := FsSortField ;
  FLastSortDirection := lSortDirection ;

  SortOrders.Clear ;
  lSortOrder := SortOrders.Add ;
  lSortOrder.FieldName := FsSortField ;
  lSortOrder.SortDirection := lSortDirection ;

  if not FbApplySort then
    FbApplySort := true ;

  Refresh ;

end ;


//------------------------------------------------------------------------------
procedure TtiCustomListView.DoOnTimerSort(sender: TObject);
begin
  FTimerSort.Enabled := false ;
  DoSortColumn ;
end;

//------------------------------------------------------------------------------
procedure TtiCustomListView.DoCustomDrawItem( Sender: TCustomListView;
                                          Item: TListItem;
                                          State: TCustomDrawState;
                                          var DefaultDraw: Boolean);
begin
  if Assigned( FOnGetFont ) then begin
    FOnGetFont( Self,
                FLV.Canvas,
                Item,
                TPersistent( FDataInternal.Items[Item.Index] )) ;
  end ;
end;

//------------------------------------------------------------------------------
{
procedure TtiCustomListView.DoOnTimerSetData(Sender: TObject);
begin
  if not Showing then
    Exit ; //==>

  TTimer( Sender ).Enabled := false ;
  if Items.Count < 2 then
    Exit ; //==>

  if FbSelectFirstRow then
  begin
    PositionCursor( 1 ) ;
    PositionCursor( 0 ) ;
  end ;

end;
}

// Scan the ListView and add the data object for all items marked as selected
// to FSelectedData, then return FSelectedData as the result of the function.
// -----------------------------------------------------------------------------
function TtiCustomListView.GetSelectedDataList : TList;
var
  lListItem : TListItem ;
begin

  // Clear the result list
  FSelectedData.Clear ;

  // Find the first selected item
  lListItem := FLV.Selected ;

  // Scan the rest of the items
  while lListItem <> nil do begin
    FSelectedData.Add( TObject( FDataInternal.Items[lListItem.Index] )) ;
    lListItem := FLV.GetNextItem( lListItem, sdAll, [isSelected] ) ;
  end ;

  result := FSelectedData ;

end;

// Delete a list item, along with the data it points to.
// -----------------------------------------------------------------------------
procedure TtiCustomListView.DeleteData(pListItem: TListItem);
var
  i     : integer ;
  lData : TObject ;
begin

  if pListItem = nil then
    Exit ; //==>

  lData := pListItem.Data ;

  i := FSelectedData.IndexOf( lData ) ;
  if i <> -1 then
    FSelectedData.Delete( i ) ;

  i := FData.IndexOf( lData ) ;
  if i <> -1 then
    FData.Delete( i ) ;

  i := FDataInternal.IndexOf( lData ) ;
  if i <> -1 then
    FDataInternal.Delete( i ) ;

  Refresh ;

end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.First;
begin
  PositionCursor( 0 ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.MoveSelection( piMoveBy : integer );
begin
  PositionCursor( SelectedIndex + piMoveBy ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.Last;
begin
  PositionCursor( FDataInternal.Count - 1 ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TlvSortOrders
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TlvSortOrders.Add: TlvSortOrder ;
begin
  result := TlvSortOrder( inherited add ) ;
end;

//------------------------------------------------------------------------------
constructor TlvSortOrders.Create( Owner : TComponent ) ;
begin
  inherited Create( TlvSortOrder ) ;
  FOwner := Owner ;
end;

//------------------------------------------------------------------------------
function TlvSortOrders.GetItem(Index: integer): TlvSortOrder ;
begin
  result := TlvSortOrder( inherited GetItem( Index )) ;
end;

//------------------------------------------------------------------------------
function TlvSortOrders.GetOwner: TPersistent;
begin
  result := FOwner ;
end;

//------------------------------------------------------------------------------
procedure TlvSortOrders.SetItem(Index: integer; const Value: TlvSortOrder );
begin
  inherited SetItem( Index, Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TlvSortOrder
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TlvSortOrder.Assign(source: TPersistent);
begin
  inherited Assign( TlvSortOrder( source )) ;
end;

//------------------------------------------------------------------------------
constructor TlvSortOrder.Create(Collection: TCollection);
begin
  inherited Create( Collection ) ;
  FieldName     := 'Enter value' ;
  SortDirection := lvsdAscending ;
end;

//------------------------------------------------------------------------------
function TlvSortOrder.GetDisplayName: string;
begin
  result := FieldName ;
end;

{ TtiLVNullDataRow }

constructor TtiLVNullDataRow.Create;
begin
  inherited ;
  FsNoData := 'No data' ;
end;

{ TtiLVNullData }

constructor TtiLVNullData.Create;
begin
  inherited ;
  Add( TtiLVNullDataRow.Create ) ;
end;

function TtiCustomListView.CanDelete: boolean;
begin
  result := Enabled and ( not FReadOnly ) and ( FLV.Selected <> nil ) ;
  if result and Assigned( FOnCanDelete ) then
    FOnCanDelete( self, SelectedData, Selected, result ) ;
end;

function TtiCustomListView.CanEdit: boolean;
begin
  result := Enabled and ( Not FReadOnly ) and ( FLV.Selected <> nil ) ;
  if result and Assigned( FOnCanEdit ) then
    FOnCanEdit( self, SelectedData, Selected, result ) ;
end;

function TtiCustomListView.CanInsert: boolean;
begin
  result := Enabled and ( not FReadOnly ) ;
  if result and Assigned( FOnCanInsert ) then
    FOnCanInsert( self, SelectedData, Selected, result ) ;
end;

procedure TtiCustomListView.PositionCursor(pData: TPersistent);
var
  i : integer ;
begin
  i := FDataInternal.IndexOf( pData ) ;
  if i <> -1 then
    PositionCursor( i ) ;
end;

procedure TtiCustomListView.DoOnSelectItem(Sender: TObject; Item: TListItem;Selected: boolean);
var
  lSelectedData : TPersistent ;
begin
  if FbProcessingOnClick then
    Exit ; //==>

  if Selected and Assigned( FOnItemArive ) then
  begin
    FLastItem := Item ;
    lSelectedData := TPersistent(FDataInternal.Items[FLastItem.Index]) ;
    FOnItemArive( self, lSelectedData, FLastItem ) ;
  end ;

  if ( Not Selected ) and
     ( Assigned( FOnItemLeave )) and
     ( FLastItem <> nil ) then
  begin
    lSelectedData := TPersistent(FDataInternal.Items[FLastItem.Index]) ;
    FOnItemLeave( self, lSelectedData, FLastItem ) ;
    FLastItem := nil ;
  end ;
  EnableButtons ;
end;

procedure TtiCustomListView.DoOnClick(Sender: TObject);
var
  lItem   : TListItem ;
  pData   : TPersistent ;
  pColumn : TListColumn ;
  lPoint  : TPoint;
  i       : integer ;
  liLeft  : integer ;
begin
  if Assigned( FOnLVClick ) then
  begin

    lPoint.X := Mouse.CursorPos.X ;
    lPoint.Y := Mouse.CursorPos.Y ;
    lPoint   := Self.ScreenToClient( lPoint ) ;

    lItem := FLV.GetItemAt( lPoint.X, lPoint.Y ) ;
    if lItem = nil then
      Exit ; //==>

    if lItem.Data <> nil then
      pData := TPersistent( lItem.Data )
    else
      pData := nil ;

    pColumn := nil ;
    liLeft := 0 ;
    for i := 0 to Columns.Count - 1 do
    begin
      liLeft := liLeft + Columns[i].Width ;
      if lPoint.X <= liLeft then
      begin
        pColumn := Columns[i] ;
        Break ; //==>
      end;
    end ;
    FOnLVClick( Self, lItem, pData, pColumn ) ;

  end ;
end;

// -----------------------------------------------------------------------------
{
procedure TtiCustomListView.DrawMicroButtons;
var
  liBtnCount : integer ;
begin

  liBtnCount := 0 ;

  FBtnNew.Visible    := false ;
  FBtnEdit.Visible   := false ;
  FBtnDelete.Visible := false ;

  if (( csDesigning in ComponentState ) or Assigned( FOnItemInsert )) and
     ( tiLVBtnVisNew in FVisibleButtons )then
  begin
    FBtnNew.Visible := true ;
    FBtnNew.Left := 1 + ( liBtnCount * ( FBtnNew.Width + 4 ));
    inc( liBtnCount ) ;
  end ;

  if (( csDesigning in ComponentState ) or Assigned( FOnItemEdit )) and
     ( tiLVBtnVisEdit in FVisibleButtons ) then
  begin
    FBtnEdit.Visible := true ;
    FBtnEdit.Left := 1 + ( liBtnCount * ( FBtnEdit.Width + 4 ));
    inc( liBtnCount ) ;
  end ;

  if (( csDesigning in ComponentState ) or Assigned( FOnItemDelete )) and
     ( tiLVBtnVisDelete in FVisibleButtons ) then
  begin
    FBtnDelete.Visible := true ;
    FBtnDelete.Left := 1 + ( liBtnCount * ( FBtnDelete.Width + 4 ));
//    inc( liBtnCount ) ;
  end ;
  DoReSize(nil);
end;
}

// -----------------------------------------------------------------------------
function TtiCustomListView.GetVisibleButtons: TtiLVVisibleButtons;
begin
  result := FCtrlBtnPnl.VisibleButtons ;
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.SetVisibleButtons( const Value: TtiLVVisibleButtons);
begin
  FCtrlBtnPnl.VisibleButtons := Value ;
  DoReSize(Nil);
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.EnableButtons;
begin
  Assert(FCtrlBtnPnl <> nil, 'FCtrlBtnPnl not assigned' ) ;
  FCtrlBtnPnl.EnableButtons ;
end;

// -----------------------------------------------------------------------------
function TtiCustomListView.GetMultiSelect: boolean;
begin
  result := FLV.MultiSelect ;
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.SetMultiSelect(const Value: boolean);
begin
  FLV.MultiSelect := Value ;
end;

{
// -----------------------------------------------------------------------------
function TtiCustomListView.GetOnDblClick: TNotifyEvent;
begin
  result := FLV.OnDblClick ;
end;

// -----------------------------------------------------------------------------
procedure TtiCustomListView.SetOnDblClick(const Value: TNotifyEvent);
begin
  FLV.OnDblClick := Value ;
end;
}

function TtiCustomListView.GetOnChange: TLVChangeEvent;
begin
  result := FLV.OnChange ;
end;

procedure TtiCustomListView.SetOnChange(const Value: TLVChangeEvent);
begin
  FLV.OnChange := Value ;
end;

function TtiCustomListView.GetOnChanging: TLVChangingEvent;
begin
  result := FLV.OnChanging ;
end;

procedure TtiCustomListView.SetOnChanging(const Value: TLVChangingEvent);
begin
  FLV.OnChanging := Value ;
end;

function TtiCustomListView.GetOnKeyDown: TKeyEvent;
begin
  result := FLV.OnKeyDown ;
end;

procedure TtiCustomListView.SetOnKeyDown(const Value: TKeyEvent);
begin
  FLV.OnKeyDown := Value ;
end;

function TtiCustomListView.GetOnKeyPress: TKeyPressEvent;
begin
  result := FLV.OnKeyPress ;
end;

procedure TtiCustomListView.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  FLV.OnKeyPress := Value ;
end;

function TtiCustomListView.GetSmallImages: TCustomImageList;
begin
  result := FLV.SmallImages ;
end;

procedure TtiCustomListView.SetSmallImages(const Value: TCustomImageList);
begin
  FLV.SmallImages := Value ;
end;

function TtiCustomListView.GetViewStyle: TViewStyle;
begin
  result := FLV.ViewStyle ;
end;

procedure TtiCustomListView.SetViewStyle(const Value: TViewStyle);
begin
  FLV.ViewStyle := Value ;
end;

function TtiCustomListView.GetRowSelect: Boolean;
begin
  result := FLV.RowSelect ;
end;

procedure TtiCustomListView.SetRowSelect(const Value: Boolean);
begin
  FLV.RowSelect := Value ;
end;

function TtiCustomListView.GetItems: TListItems;
begin
  result := FLV.Items ;
end;

function TtiCustomListView.GetColumns: TListColumns;
begin
  result := FLV.Columns ;
end;

function TtiCustomListView.GetSelected: TListItem;
begin
  result := FLV.Selected ;
end;

function TtiCustomListView.GetListView: TListView;
begin
  result := FLV ;
end;

function TtiCustomListView.GetItemAt(X, Y: Integer): TListItem;
begin
  result := FLV.GetItemAt( X, Y ) ;
end;

function TtiCustomListView.CanMoveItem( pItem : TListItem ; piMoveBy : integer ) : boolean;
begin
  if piMoveBy = 0 then
  begin
    result := true ;
    Exit ; //==>
  end ;

  // Move down
  if piMoveBy < 0 then
    result := ( pItem <> nil ) and
              ( pItem.Index > 0 )
  else
    result := ( pItem <> nil ) and
              ( pItem.Index < Items.Count - 1 )
  ;

end;

procedure TtiCustomListView.MoveItem(pItem: TListItem; piMoveBy: integer);
var
  liMoveFrom  : integer ;
  liBefore    : integer ;
  liAfter     : integer ;
  lDataMove   : TPersistent ;
  lDataBefore : TPersistent ;
  lDataAfter  : TPersistent ;
begin
  if Assigned( FOnMoveItem ) then
  begin
    if piMoveBy = 0 then
      Exit ; //==>

    liMoveFrom := pItem.Index ;
    if piMoveBy > 0 then
    begin
      liBefore   := liMoveFrom + piMoveBy ;
      liAfter    := liMoveFrom + piMoveBy + 1
    end
    else
    begin
      liBefore   := liMoveFrom + piMoveBy - 1 ;
      liAfter    := liMoveFrom + piMoveBy  ;
    end ;

    if liBefore < 0 then
      lDataBefore := nil
    else
      lDataBefore := TPersistent(FDataInternal.Items[liBefore]) ;

    if liAfter > FDataInternal.Count-1 then
      lDataAfter := nil
    else
      lDataAfter := TPersistent( FDataInternal.Items[liAfter]);

    lDataMove  := TPersistent( FDataInternal.Items[ liMoveFrom ]) ;
    FOnMoveItem( FDataInternal, lDataMove, lDataBefore, lDataAfter ) ;
    Refresh ;
    Application.ProcessMessages ;
    PositionCursor( lDataMove ) ;
    SetFocus ;
  end ;

end;

procedure TtiCustomListView.SetRunTimeGenCols(const Value: boolean);
begin
  FbRunTimeGenCols := Value;
  //if not Value then
  //  FtiListColumns.Clear ;
end;

procedure TtiCustomListView.SetFocus;
begin
  if not Enabled then
    Exit ; //==> 
  inherited SetFocus ;
  LV.SetFocus ;
end;

function TtiCustomListView.GetSelectedData: TPersistent;
begin
  if Selected = nil then
    result := nil
  else
    result := TPersistent( Selected.Data ) ;
end;

procedure TtiCustomListView.SetLVColor;
begin
  if (not Enabled) or FReadOnly then
    FLV.Color := clBtnFace 
  else
    FLV.Color := clWindow;
end;

procedure TtiCustomListView.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled( Value ) ;
  SetLVColor;
  { TODO : TtiCustomListView.SetEnabled sets the ListView's color property, but
           the font color should also be set in a way that does not conflict
           with the OnGetFont event. The select record's focus should also be
           turned off. }
  EnableButtons ;
end;

procedure TtiCustomListView.DoOnDblClick(Sender: TObject);
var
  lSelectedData : TPersistent ;
begin

  if Assigned( FOnDblClick ) then
  begin
    lSelectedData := SelectedData ;
    FOnDblClick( Self, lSelectedData, Selected ) ;
    Exit ; //==>
  end ;

  if CanEdit then
    DoEdit
  else if CanInsert then
    DoNew ;

end;

function TtiCustomListView.GetSelectedIndex: integer;
begin
  if Selected = nil then
    result := -1
  else
    result := Selected.Index ;
end;

procedure TtiCustomListView.SetSelectedIndex(const Value: integer);
begin
  if Value <= FLV.Items.Count - 1 then
    FLV.Selected := FLV.Items[Value] ;
end;

procedure TtiCustomListView.BeginUpdate;
begin
  FLV.Items.BeginUpdate ;
end;

procedure TtiCustomListView.EndUpdate;
begin
  FLV.Items.EndUpdate ;
end;

procedure TtiCustomListView.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lTargetItem       : TListItem   ;
  lTargetData       : TPersistent ;
  lDragObject       : TtiLVDragObject ;
begin

  lTargetItem := FLV.GetItemAt( X, Y ) ;
  lTargetData := nil ;

  if lTargetItem <> nil then
    lTargetData := TPersistent( FDataInternal.Items[lTargetItem.Index] ) ;

  lDragObject := Source as TtiLVDragObject ;

  // If we have left+clicked, and moved the mouse a little, a drag event may
  // have been triggered, this is probably not what we are wanting, so throw
  // this event away.
  if lDragObject.Data = lTargetData then
    Exit ; //==>

  FOnDrop( lDragObject.tiListView,
           lDragObject.Data,
           Self,
           lTargetData ) ;

  Refresh ;

end;

procedure TtiCustomListView.DoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  lItem       : TListItem ;
  lData       : TPersistent ;
  lDragObject : TtiLVDragObject ;
begin

  Accept := false ;

  if Not Assigned( FOnCanAcceptDrop ) then
    Exit ; //==>

  lItem := FLV.GetItemAt( X, Y ) ;
  lData := nil ;

  if lItem <> nil then
    lData := TPersistent( FDataInternal.Items[lItem.Index] ) ;

  lDragObject := Source as TtiLVDragObject ;

  OnCanAcceptDrop( lDragObject.tiListView,
                   lDragObject.Data,
                   Self,
                   lData,
                   Accept ) ;

end;

procedure TtiCustomListView.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lItem : TListItem ;
  lData : TPersistent   ;
begin

  // Only allow drag if mouse button is mbLeft or mbRight
  if ( Button <> mbLeft ) then
    Exit ; //==>

//  if not Assigned( FOnCanAcceptDrop ) or
//     not Assigned( FOnDrop ) then
//    Exit ; //==>
  if not FCanStartDrag then
    Exit ; //==>

  lItem := FLV.GetItemAt( X, Y ) ;

  if lItem = nil then
    Exit ; //==>

  lData       := TPersistent( FDataInternal.Items[lItem.Index] ) ;

  if lData = nil then
    Exit ; //==>

  FLV.BeginDrag( false ) ;

end;

procedure TtiCustomListView.DoStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  lDragObject : TtiLVDragObject ;
begin
  lDragObject            := TtiLVDragObject.Create ;
  lDragObject.tiListView := Self ;
  lDragObject.Data       := SelectedData ;
  lDragObject.Item       := Selected ;
  DragObject             := lDragObject ;
end;

function TtiCustomListView.GetReadOnly: boolean;
begin
  Result := FReadOnly ;
end;

procedure TtiCustomListView.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value ;
  SetLVColor;
  EnableButtons ;
end;

procedure TtiCustomListView.SetSelectedData(const Value: TPersistent);
begin
  PositionCursor( Value ) ;
end;

{ TtiListViewInternal }

constructor TtiListViewInternal.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  FVScrolled := false ;
end;

procedure TtiListViewInternal.WMVScroll(var Message: TWMVScroll);
begin
  inherited ;
  FVScrolled := true ;
  if Assigned( FOnVScroll ) then
    FOnVScroll( Self ) ;
end;

procedure TtiCustomListView.SynchroniseWith(pListView: TtiCustomListView);
var
  liIndex : integer ;
  lScrollBy : integer ;
  lBottomItemIndex : integer ;
  lOffset : integer ;
begin

try
  if pListView = nil then
    Exit ; //==>

  if pListView.Selected = nil then
    Exit ; //==>

  if (FLV.Items.Count = 0 ) then
    Exit ; //==>

  if pListView.LV.Selected.Index < pListView.LV.TopItem.Index then
    pListView.LV.Selected := pListView.LV.TopItem ;

  if TtiListViewInternal( pListView.LV ).VScrolled then
  begin
    TtiListViewInternal( pListView.LV ).VScrolled := false ;
    lOffset := 0 ;
  end
  else
    lOffset := 0 ;

  lBottomItemIndex :=
    pListView.LV.TopItem.Index +
    pListView.LV.VisibleRowCount - lOffset ;

  if pListView.Selected.Index >
     ( lBottomItemIndex ) then
    pListView.LV.Selected := pListView.LV.Items[lBottomItemIndex];

  liIndex := pListView.LV.TopItem.Index ;
  if (liIndex <= (FLV.Items.Count-1)) then
  begin
    lScrollBy :=
      FLV.Items[liIndex].Top - 19 ;
      // Why the 19, Don't really know. Think it's the height of the
      // column headings. Must find a better way of deriving the 19.
    FLV.Scroll( 0, lScrollBy ) ;
  end ;

  liIndex := pListView.LV.TopItem.Index ;
  if (liIndex <= (FLV.Items.Count-1)) then
    FLV.Items[liIndex].MakeVisible( false ) ;

  liIndex := pListView.Selected.Index ;
  FLV.Selected := FLV.Items[liIndex] ;
  if (liIndex <= (FLV.Items.Count-1)) then
    FLV.Items[liIndex].MakeVisible( false ) ;
except
  on e:exception do
    raise exception.create( e.message + #13 + ' in TtiCustomListView.SynchroniseWith' ) ;
end ;
end ;

function TtiCustomListView.GetOnVScroll: TNotifyEvent;
begin
  result := FLV.OnVScroll ;
end;

procedure TtiCustomListView.SetOnVScroll(const Value: TNotifyEvent);
begin
  FLV.OnVScroll := Value ;
end;

function TtiCustomListView.AddColumn( const pFieldName: string;
                                      const pDataType: TlvTypeKind;
                                      const pDisplayLabel: string = '' ;
                                      pColWidth : integer = -1 ) : TtiListColumn ;
begin

  if RunTimeGenCols then
  begin
    RunTimeGenCols := false ;
    ClearColumns ;
    AddImageColumn ;
  end ;

  result := FtiListColumns.Add ;
  result.FieldName    := pFieldName ;
  result.DataType     := pDataType ;
  if pDisplayLabel = '' then
    result.DisplayLabel := pFieldName
  else
    result.DisplayLabel := pDisplayLabel ;
  result.Width := pColWidth ;

  DoAddColumn(result);

end;

procedure TtiCustomListView.ClearColumns;
begin
  BeginUpdate;
  try
    FListColsBeingDisplayed.Clear ;
    FtiListColumns.Clear ;
    FSortOrders.Clear ;
    FLV.Columns.Clear;
  finally
    EndUpdate;
  end;
end ;

procedure TtiListColumn.SetWidth(const Value: integer);
begin
  FWidth := Value;
//  Assert(Collection <> nil, 'Collection = nil' ) ;
//  Assert(Collection is TtiListColumns, 'Collection not a TtiListColumns' ) ;
//  Assert(Collection.Owner <> nil, 'Collection.Owner = nil' ) ;
//  Assert(Collection.Owner is TtiCustomListView, 'Collection.Owner not a TtiCustomListView' );
end;

procedure TtiCustomListView.DoReSize(Sender: TObject);
var
  lTop : integer ;
begin
  if FCtrlBtnPnl.VisibleButtons = [] then
    lTop := 1
  else
    lTop := FCtrlBtnPnl.Height + 2;

  FLV.Height := ClientHeight - FLV.Top - 1;
  FLV.Width  := ClientWidth - FLV.Left - 1;

  FLV.SetBounds(
    1,
    lTop,
    ClientWidth - 2,
    ClientHeight - lTop - 1 );
end;

{ TtiCtrlBtnPnlAbs }

constructor TtiCtrlBtnPnlAbs.Create(Owner: TComponent);
begin
  inherited ;
  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
//  if ( csDesigning in ComponentState ) then
  ControlStyle := ControlStyle - [csAcceptsControls] ;
  ControlStyle := ControlStyle - [csSetCaption] ;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BevelOuter := bvNone ;
end;

procedure TtiCtrlBtnPnlAbs.OnDeleteClick(Sender: TObject);
begin
  TILV.DoDelete ;
end;

procedure TtiCtrlBtnPnlAbs.OnEditClick(Sender: TObject);
begin
  TILV.DoEdit ;
end;

procedure TtiCtrlBtnPnlAbs.OnNewClick(Sender: TObject);
begin
  TILV.DoNew ;
end;

procedure TtiCtrlBtnPnlAbs.SetVisibleButtons( const Value: TtiLVVisibleButtons);
begin
  FLVVisibleButtons := Value;
  DrawButtons ;
end;

function TtiCtrlBtnPnlAbs.TILV: TtiCustomListView;
begin
  Assert( Owner is TtiCustomListView, 'Owner not a TtiCustomListView' ) ;
  result := ( Owner as TtiCustomListView ) ;
end;

{ TtiCtrlBtnPnlButton }

constructor TtiCtrlBtnPnlButton.Create(Owner: TComponent);
begin
  inherited;
  Height := FBtnSize ;
  Width  := ( FBtnSize + cBtnSpace ) * 3 ;

  FbtnNew             := TtiSpeedButton.Create( self ) ;
  SetupSpeedButton(FbtnNew);
  FBtnNew.Hint        := 'New [Ins]' ;
  FBtnNew.OnClick := OnNewClick ;
  FBtnNew.ControlStyle := FBtnNew.ControlStyle + [csNoDesignVisible];

  FBtnEdit            := TtiSpeedButton.Create( self ) ;
  SetupSpeedButton(FBtnEdit);
  FBtnEdit.Hint       := 'Edit [Enter]' ;
  FBtnEdit.OnClick    := OnEditClick ;
  FBtnEdit.ControlStyle := FBtnEdit.ControlStyle + [csNoDesignVisible];

  FBtnDelete          := TtiSpeedButton.Create( self ) ;
  SetupSpeedButton(FBtnDelete);
  FBtnDelete.Hint     := 'Delete [Del]' ;
  FBtnDelete.OnClick  := OnDeleteClick ;
  FBtnDelete.ControlStyle := FBtnDelete.ControlStyle + [csNoDesignVisible];
  Visible := false ;
  
end;

procedure TtiCtrlBtnPnlButton.DrawButtons;
var
  liBtnCount : integer ;
begin

  liBtnCount := 0 ;

  FBtnNew.Visible    := false ;
  FBtnEdit.Visible   := false ;
  FBtnDelete.Visible := false ;

  FBtnNew.ControlStyle := FBtnNew.ControlStyle + [csNoDesignVisible];
  FBtnEdit.ControlStyle := FBtnEdit.ControlStyle + [csNoDesignVisible];
  FBtnDelete.ControlStyle := FBtnDelete.ControlStyle + [csNoDesignVisible];

  Visible := FLVVisibleButtons <> [] ;

  if (( csDesigning in ComponentState ) or Assigned( TILV.OnItemInsert )) and
     ( tiLVBtnVisNew in FLVVisibleButtons )then
  begin
    FBtnNew.Visible := true ;
    FBtnNew.Left := 1 + ( liBtnCount * ( FBtnNew.Width + cBtnSpace ));
    FBtnNew.ControlStyle := FBtnNew.ControlStyle - [csNoDesignVisible];
    inc( liBtnCount ) ;
  end ;

  if (( csDesigning in ComponentState ) or Assigned( TILV.OnItemEdit )) and
     ( tiLVBtnVisEdit in FLVVisibleButtons ) then
  begin
    FBtnEdit.Visible := true ;
    FBtnEdit.Left := 1 + ( liBtnCount * ( FBtnEdit.Width + cBtnSpace ));
    FBtnEdit.ControlStyle := FBtnEdit.ControlStyle - [csNoDesignVisible];
    inc( liBtnCount ) ;
  end ;

  if (( csDesigning in ComponentState ) or Assigned( TILV.OnItemDelete )) and
     ( tiLVBtnVisDelete in FLVVisibleButtons ) then
  begin
    FBtnDelete.Visible := true ;
    FBtnDelete.Left := 1 + ( liBtnCount * ( FBtnDelete.Width + cBtnSpace ));
    FBtnDelete.ControlStyle := FBtnDelete.ControlStyle - [csNoDesignVisible];
  end ;

  FBtnNew.Invalidate ;
  FBtnEdit.Invalidate ;
  FBtnDelete.Invalidate ;
  Invalidate ;
  TILV.Invalidate;
end;

procedure TtiCtrlBtnPnlButton.EnableButtons;
var
  lDesigning : boolean ;
begin
  lDesigning := ( csDesigning in ComponentState ) ;
  FBtnNew.Enabled    := FBtnNew.Visible    and (TILV.CanInsert or lDesigning);
  FBtnEdit.Enabled   := FBtnEdit.Visible   and (TILV.CanEdit or lDesigning);
  FBtnDelete.Enabled := FBtnDelete.Visible and (TILV.CanDelete or lDesigning);
end;

procedure TtiCtrlBtnPnlButton.SetupSpeedButton(const pBtn: TSpeedButton);
begin
  pBtn.Parent   := Self ;
  pBtn.Visible  := false ;
  pBtn.ShowHint := true ;
  pBtn.Flat     := true ;
  pBtn.Top      := 0 ;
  pBtn.Height   := FBtnSize ;
  pBtn.Width    := FBtnSize ;
end;

{ TtiCtrlBtnPnlMicroButton }

constructor TtiCtrlBtnPnlMicroButton.Create(Owner: TComponent);
begin
  FBtnSize := cMicroButtonHeight ;
  inherited;
  FBtnNew.Glyph.LoadFromResourceName(            HInstance, cGlyphBtnNew08     ) ;
  FBtnEdit.Glyph.LoadFromResourceName(           HInstance, cGlyphBtnEdit08    ) ;
  FBtnDelete.Glyph.LoadFromResourceName(         HInstance, cGlyphBtnDelete08  ) ;
  FBtnNew.GlyphHot.LoadFromResourceName(         HInstance, cGlyphBtnNew08H    ) ;
  FBtnEdit.GlyphHot.LoadFromResourceName(        HInstance, cGlyphBtnEdit08H   ) ;
  FBtnDelete.GlyphHot.LoadFromResourceName(      HInstance, cGlyphBtnDelete08H ) ;
  FBtnNew.GlyphDisabled.LoadFromResourceName(    HInstance, cGlyphBtnNew08D    ) ;
  FBtnEdit.GlyphDisabled.LoadFromResourceName(   HInstance, cGlyphBtnEdit08D   ) ;
  FBtnDelete.GlyphDisabled.LoadFromResourceName( HInstance, cGlyphBtnDelete08D ) ;
end;

procedure TtiCustomListView.SetButtonStyle(const Value: TLVButtonStyle);
var
  lVisibleButtons : TtiLVVisibleButtons ;
begin
  FButtonStyle := Value;
  lVisibleButtons := FCtrlBtnPnl.VisibleButtons ;
  case FButtonStyle of
  lvbsMicroButtons :begin
                      FCtrlBtnPnl.Free ;
                      FCtrlBtnPnl := TtiCtrlBtnPnlMicroButton.Create(Self);
                    end ;
  lvbsNormalButtons :begin
                      FCtrlBtnPnl.Free ;
                      FCtrlBtnPnl := TtiCtrlBtnPnlNormalButton.Create(Self);
                    end ;
  lvbsLargeButtons :begin
                      FCtrlBtnPnl.Free ;
                      FCtrlBtnPnl := TtiCtrlBtnPnlLargeButton.Create(Self);
                    end ;
  else
    raise exception.create( 'Invalid button style' ) ;
  end ;
  FCtrlBtnPnl.Parent := Self ;
  FCtrlBtnPnl.Top := 1 ;
  FCtrlBtnPnl.Left := 1 ;
  FCtrlBtnPnl.VisibleButtons := lVisibleButtons ;
  DrawButtons ;
  EnableButtons ;
  DoReSize(nil);
end;

{ TtiCtrlBtnPnlLargeButton }

constructor TtiCtrlBtnPnlLargeButton.Create(Owner: TComponent);
begin
  FBtnSize := cLargeButtonHeight ;
  inherited;
  FBtnNew.Glyph.LoadFromResourceName(            HInstance, cGlyphBtnNew24     ) ;
  FBtnEdit.Glyph.LoadFromResourceName(           HInstance, cGlyphBtnEdit24    ) ;
  FBtnDelete.Glyph.LoadFromResourceName(         HInstance, cGlyphBtnDelete24  ) ;
  FBtnNew.GlyphHot.LoadFromResourceName(         HInstance, cGlyphBtnNew24H    ) ;
  FBtnEdit.GlyphHot.LoadFromResourceName(        HInstance, cGlyphBtnEdit24H   ) ;
  FBtnDelete.GlyphHot.LoadFromResourceName(      HInstance, cGlyphBtnDelete24H ) ;
  FBtnNew.GlyphDisabled.LoadFromResourceName(    HInstance, cGlyphBtnNew24D    ) ;
  FBtnEdit.GlyphDisabled.LoadFromResourceName(   HInstance, cGlyphBtnEdit24D   ) ;
  FBtnDelete.GlyphDisabled.LoadFromResourceName( HInstance, cGlyphBtnDelete24D ) ;
end;

{ TtiCtrlBtnPnlNormalButton }

constructor TtiCtrlBtnPnlNormalButton.Create(Owner: TComponent);
begin
  FBtnSize := cNormalButtonHeight ;
  inherited;
  FBtnNew.Glyph.LoadFromResourceName(            HInstance, cGlyphBtnNew16     ) ;
  FBtnEdit.Glyph.LoadFromResourceName(           HInstance, cGlyphBtnEdit16    ) ;
  FBtnDelete.Glyph.LoadFromResourceName(         HInstance, cGlyphBtnDelete16  ) ;
  FBtnNew.GlyphHot.LoadFromResourceName(         HInstance, cGlyphBtnNew16H    ) ;
  FBtnEdit.GlyphHot.LoadFromResourceName(        HInstance, cGlyphBtnEdit16H   ) ;
  FBtnDelete.GlyphHot.LoadFromResourceName(      HInstance, cGlyphBtnDelete16H ) ;
  FBtnNew.GlyphDisabled.LoadFromResourceName(    HInstance, cGlyphBtnNew16D    ) ;
  FBtnEdit.GlyphDisabled.LoadFromResourceName(   HInstance, cGlyphBtnEdit16D   ) ;
  FBtnDelete.GlyphDisabled.LoadFromResourceName( HInstance, cGlyphBtnDelete16D ) ;
end;

procedure TtiCustomListView.SetApplySortNoRefresh(const Value, pRefresh: boolean);
begin
  if FbApplySort = value then
    exit ; //==>
  FbApplySort := Value;
  if pRefresh then
    Refresh ;
end;

function TtiCustomListView.GetOnInfoTip: TtiLVInfoTipEvent;
begin
  Result := FOnTipInfo;
end;

procedure TtiCustomListView.SetOnInfoTip(const Value: TtiLVInfoTipEvent);
begin
  FOnTipInfo := Value;
  if Assigned(FOnTipInfo) then
  begin
    FLV.ShowHint := True ;
    FLV.OnInfoTip := DoTipInfo;
  end else
    FLV.OnInfoTip := nil ;
end;

procedure TtiCustomListView.DoTipInfo( Sender: TObject;
                                       Item: TListItem;
                                       var InfoTip: string);
var
  lInfoTip: string;
begin
  if Assigned(FOnTipInfo) then
  begin
    FOnTipInfo(Self,
               SelectedData,
               FLV.Selected,
               lInfoTip);
    InfoTip:= lInfoTip;
  end;
end;

initialization
  uSortOrders := TlvSortOrders.Create( nil ) ;

finalization
  uSortOrders.Free ;

end.


