unit tiVTListView;

interface

{$I tiDefines.inc}

uses
{.$IFDEF _PROFILE}
   Dialogs
  ,Classes
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
  ,cTIPerAwareCtrls
  ,tiVisitor
  ,tiOID
  ,Variants
  ,tiVirtualTrees
{$IFNDEF FPC}
//  ,tiVirtualTrees
{$ELSE}
  ,LCLIntf
  ,LCLProc
  ,VirtualStringTree
{$ENDIF}
 ;


const
  crsDefaultColFieldName    = 'Caption';
  cDefaultAlternateRowColor = clPaleBlue; // Pale blue
  cDefaultAlternateRowCount = 2; // Show every second row in pale blue

type
  TtiVTSortOrder = class;
  TtiVTSortOrders = class;
  TtiVTColumn = class;
  TtiVTColumns = class;
  TtiVTHeader = class;
  TtiInternalVirtualTree = class;
  TtiCustomVirtualTree = class;
  TtiVTListView = class;


  TvtTypeKind = (vttkString, vttkInt, vttkFloat, vttkDate, vttkDateTime, vttkCurrency);

  TtiVTClearSortEvent     = procedure(pVT : TtiCustomVirtualTree) of object;
  TtiVTOnFilterDataEvent  = procedure(AData  : TtiObject; var pInclude : boolean) of object;


  TtiVTGetImageIndexEvent = procedure(pSender: TtiCustomVirtualTree;
                                      pNode: PVirtualNode;
                                      AData: TtiObject;
                                      pKind: TVTImageKind;
                                      pColumn: TColumnIndex;
                                      var pGhosted: Boolean;
                                      var pImageIndex: Integer) of object;

  //TtiVTEvent              = procedure(pVT : TtiCustomVirtualTree) of object;
  TtiVTItemEvent          = procedure(pVT : TtiCustomVirtualTree; AData : TtiObject; AItem : PVirtualNode) of object;
  TtiVTItemEditEvent      = procedure(pVT : TtiCustomVirtualTree; AData : TtiObject; AItem : PVirtualNode ) of object;
  TtiVTOnPaintText        = procedure(const pVT : TtiCustomVirtualTree;
                                       const pCanvas : TCanvas;
                                       const AData  : TtiObject;
                                             pColumn : Integer;
                                             pNode  : PVirtualNode
                                      ) of object;
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
                              var   AHintText: WideString) of object;

  TtiVTFocusChangeEvent = procedure(pSender: TtiCustomVirtualTree;
                                    AData: TtiObject;
                                    pNode: PVirtualNode;
                                    pColumn: TColumnIndex) of object;

  TtiVTHeaderClickEvent = procedure(const pSender: TtiCustomVirtualTree;
                                     const ptiListColumn : TtiVTColumn) of object;

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
    function    ItemByFieldName(const AFieldName: string;
      out ASortOrder: TtiVTSortOrder): boolean;

  published
    property GroupColumnCount: Integer read FGroupColumnCount write FGroupColumnCount;
  end;


  TtiVTColumn = class(TVirtualTreeColumn)
  private
    FsFieldName: string;
    FsDisplayMask: string;
    FDataType: TvtTypeKind;
    FDerived: boolean;
    FOnDeriveColumn: TtiDeriveListColumnValue;
    procedure   SetFieldName(const AValue: string);
    procedure   SetDataType(const AValue: TvtTypeKind);
    procedure   SetDerived(const AValue: boolean);
    procedure   SetOnDeriveColumn(const AValue: TtiDeriveListColumnValue);
  protected
  public
    constructor Create(Collection : TCollection); override;
    destructor  Destroy; override;
    function    Clone : TtiVTColumn;
  published
    property    FieldName   : string read FsFieldName    write SetFieldName;
    property    DisplayMask : string read FsDisplayMask  write FsDisplayMask;
    property    DataType    : TvtTypeKind read FDataType write SetDataType;
    property    Derived     : boolean read FDerived      write SetDerived;
    property    OnDeriveColumn : TtiDeriveListColumnValue read FOnDeriveColumn write SetOnDeriveColumn;
  end;

  TtiVTColumns = class(TVirtualTreeColumns)
  private
    //FOwner : TComponent;
    function  GetItem(Index : TColumnIndex): TtiVTColumn;
    procedure SetItem(Index : TColumnIndex; const AValue : TtiVTColumn);
  protected
  public
    constructor Create(AOwner : TtiVTHeader);
    destructor  Destroy; override;
    property  Items[Index: TColumnIndex ]: TtiVTColumn read GetItem write SetItem; default;
    procedure DisplayLabelsToStringList(pSL : TStringList);
    function  FindByDisplayLabel(const AValue : string): TtiVTColumn;
  end;

  TtiVTHeader = class(TVTHeader)
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

  TtiInternalVirtualTree = class(TVirtualStringTree)
  protected
    FtiOwner: TtiCustomVirtualTree;

    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
      var EraseAction: TItemEraseAction); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
       var Ghosted: Boolean; var Index: Integer); override;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;

    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetHeaderClass: TVTHeaderClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TtiCustomVirtualTree = class(TtiFocusPanel)
    // Features not ported:
    //   Runtime column generation - you must assign columns, or see nothing
    //   Drag and drop
  private
    FVT: TVirtualStringTree;

    FData: TtiObjectList;
    FFilteredData: TObjectList;
    FGroupedData: TList; // List of indexes into FFilteredData
    FGroupingApplied: Boolean;
    FLastNode: PVirtualNode;

    FShowAlternateRowColor: Boolean;
    FSortOrders: TtiVTSortOrders;
    FAlternateRowColor: TColor;

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
    FOnBeforeCellPaint: TtiVTOnPaintText;
    FOnDblClick: TtiVTItemEvent;
    FAlternateRowCount: Byte;
    FDisabledColor: TColor;
    FRowSelect: boolean;
    FShowNodeHint: boolean;
    FOnGetNodeHint: TtiVTOnNodeHint;
    FHeaderClickSorting: boolean;

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
    procedure SetOnBeforeCellPaint(const AValue: TtiVTOnPaintText);
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
    procedure VTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure SetHeaderClickSorting(const Value: boolean);

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
    procedure   ApplyGrouping; virtual;
    procedure   DoEnter; override;

    property    OnFocusChanged : TtiVTFocusChangeEvent read FOnFocusChanged write FOnFocusChanged;
    property    OnKeyDown      : TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property    OnKeyPress     : TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    //property    OnClick        : TtiLVOnClick read FOnLVClick write FOnLVClick;
    property    OnDblClick     : TtiVTItemEvent read FOnDblClick write FOnDblClick;
    property    OnFilterData   : TtiVTOnFilterDataEvent read  FOnFilterData write FOnFilterData;
    //property    OnGetFont      : TtiLVOnGetFont read  FOnGetFont write FOnGetFont;
    //property    OnGetImageIndex : TtiLVGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property    OnHeaderClick : TtiVTHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;

    property    OnItemArrive    : TtiVTItemEvent read FOnItemArrive write FOnItemArrive;
    property    OnItemLeave     : TtiVTItemEvent read FOnItemLeave write FOnItemLeave;
    property    OnPaintText     : TtiVTOnPaintText read FOnPaintText Write SetOnPaintText;
    property    OnBeforeCellPaint: TtiVTOnPaintText read FOnBeforeCellPaint Write SetOnBeforeCellPaint;
    property    OnGetNodeHint   : TtiVTOnNodeHint read FOnGetNodeHint write SetOnGetNodeHint;
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
    function  SetSelectedChildData(Parent: PVirtualNode;  Data: TtiObject): Boolean;
    procedure SetEnabled(AValue: Boolean); override;

    procedure VTDoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction); virtual;
    procedure VTDoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString); virtual;
    procedure VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual;
    procedure VTGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); virtual;
    procedure VTInitChildren(Node: PVirtualNode;  var ChildCount: Cardinal); virtual;
    procedure DoOnPaintText(pSender: TBaseVirtualTree; const pTargetCanvas: TCanvas; pNode: PVirtualNode;  pColumn: TColumnIndex; pTextType: TVSTTextType);
    procedure DoOnBeforeCellPaint(ASender: TBaseVirtualTree; ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex; ACellRect: TRect);
    procedure DoOnGetHint(Sender: TBaseVirtualTree; ANode: PVirtualNode; Column: TColumnIndex; var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: WideString);

    property AlternateRowColor: TColor read FAlternateRowColor write SetAlternateRowColor default cDefaultAlternateRowColor;
    property AlternateRowCount: Byte Read FAlternateRowCount Write FAlternateRowCount default cDefaultAlternateRowCount;
    property DisabledColor: TColor Read FDisabledColor Write FDisabledColor default clBtnFace;
    property HeaderClickSorting: boolean read FHeaderClickSorting write SetHeaderClickSorting default True;
    property Images: TCustomImageList read GetImages write SetImages;
    property RowSelect: boolean read FRowSelect write SetRowSelect default True;
    property ShowAlternateRowColor: Boolean read FShowAlternateRowColor write SetShowAlternateRowColor default True;
    property ShowNodeHint: boolean read FShowNodeHint write SetShowNodeHint;
    property SortOrders: TtiVTSortOrders read FSortOrders write SetSortOrders;


    property OnGetImageIndex: TtiVTGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnCanView: TtiVTCanPerformAction read FOnCanView write FOnCanView;
    property OnCanDelete: TtiVTCanPerformAction read FOnCanDelete write FOnCanDelete;
    property OnCanInsert: TtiVTCanPerformAction read FOnCanInsert write FOnCanInsert;
    property OnCanEdit: TtiVTCanPerformAction read FOnCanEdit write FOnCanEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   ApplySort; virtual;

    function    CanView: Boolean; virtual;
    function    CanInsert: Boolean; virtual;
    function    CanEdit: Boolean; virtual;
    function    CanDelete: Boolean; virtual;
    procedure   ClearSort; virtual;

    procedure   DeleteNode(Node: PVirtualNode); virtual;

    function    IsNodeDataItem(Node: PVirtualNode): Boolean;

    function    GetObjectFromNode(pNode: PVirtualNode): TtiObject;
    function    GetTextFromObject(Obj: TtiObject; ColumnIndex: TColumnIndex): string;

    procedure   Refresh(const pSelectedData: TtiObject = nil); reintroduce; virtual;

    //procedure   PositionCursor(AIndex : integer    ); overload;
    //procedure   PositionCursor(AData  : TtiObject); overload;

    //procedure   Last;
    //procedure   First;
    //property    ApplyFilter : boolean read FbApplyFilter write SetApplyFilter;

    property    SelectedData : TtiObject read GetSelectedData write SetSelectedData;
    property    SelectedIndex : integer read GetSelectedIndex write SetSelectedIndex;

    procedure   SetFocus; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    function    AddColumn(const AFieldName : string;
                           const pDataType : TvtTypeKind;
                           const pDisplayLabel : string = '';
                           pColWidth : Integer = -1): TtiVTColumn; overload;
    function    AddColumn(const pDeriveColumnMethod : TtiDeriveListColumnValue;
                           const pDisplayLabel : string = '';
                           pColWidth : Integer = -1): TtiVTColumn; overload;

    //procedure   ClearColumns; virtual;
    //property    SortOrders     : TtiVTSortOrders read FSortOrders;
    //property    GroupCols      : TtiLVGroupCols read FGroupCols;
    property    Data : TtiObjectList read FData write SetData;
    property    VT: TVirtualStringTree read FVT;
    property    Header: TtiVTHeader read GetHeader write SetHeader;
    property    Sorted: Boolean read FSorted;
    function    SelectedNodeScreenOrigin: TPoint;
  published
  end;


  TtiCustomVirtualEditTree = class(TtiCustomVirtualTree)
    // Adds View,Edit,New,Delete buttons, context menu items and events
  private
    FCtrlBtnPnl : TtiCtrlBtnPnlAbs;
    FButtonStyle: TLVButtonStyle;
    FVisibleButtons: TtiLVVisibleButtons;
    FPopupMenu: TPopupMenu;
    FpmiView  : TMenuItem;
    FpmiEdit  : TMenuItem;
    FpmiNew   : TMenuItem;
    FpmiDelete : TMenuItem;
    FpmiClearSort: TMenuItem;
    FStdPopupItemCount: Integer;

    FOnItemDelete: TtiVTItemEditEvent;
    FOnItemEdit: TtiVTItemEditEvent;
    FOnItemInsert: TtiVTItemEditEvent;
    FOnItemView: TtiVTItemEditEvent;
    FOnClearSort: TtiVTClearSortEvent;

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
  protected
    procedure CreateButtonPanel;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetEnabled(AValue: Boolean); override;

    procedure DoMenuPopup(Sender: TObject); virtual;
    procedure DoClearSort(Sender: TObject); virtual;
    procedure DoDelete(Sender: TObject); virtual;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoNew(Sender: TObject); virtual;
    procedure DoView(Sender: TObject); virtual;
    procedure DoDblClick(Sender: TObject); override;

    procedure   Loaded; override;
    procedure   DoEnter; override;
    procedure   DoExit ; override;

    procedure VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); override;

    property ButtonStyle: TLVButtonStyle read FButtonStyle write SetButtonStyle default lvbsButtonsAndLabels;
    property VisibleButtons : TtiLVVisibleButtons read FVisibleButtons write SetVisibleButtons default [];

    property OnClearSort: TtiVTClearSortEvent read FOnClearSort write SetOnClearSort;
    property OnItemDelete: TtiVTItemEditEvent read FOnItemDelete write SetOnItemDelete;
    property OnItemEdit: TtiVTItemEditEvent read FOnItemEdit write SetOnItemEdit;
    property OnItemInsert: TtiVTItemEditEvent read FOnItemInsert write SetOnItemInsert;
    property OnItemView: TtiVTItemEditEvent read FOnItemView write SetOnItemView;
  public
    constructor Create(AOwner: TComponent); override;

    function CanDelete: Boolean; override;
    function CanEdit: Boolean; override;
    function CanInsert: Boolean; override;
    function CanView: Boolean; override;

    procedure Refresh(const pSelectedData: TtiObject = nil); override;

    property ButtonPanel: TtiCtrlBtnPnlAbs read FCtrlBtnPnl;
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
    property Header;
    property HeaderClickSorting;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property RowSelect;
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
    property OnFilterData;
    property OnFocusChanged;
    property OnGetImageIndex;
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
    property OnBeforeCellPaint;
    property OnGetNodeHint;
    // ...
  end;



function tiVTDisplayMaskFromDataType(const AValue : TvtTypeKind): string;

//______________________________________________________________________________________________________________________________
implementation

uses
   ExtCtrls
  ,Math
  ,tiConstants
  ,tiExcept
  ,tiGUIUtils

  ,tiDialogs // Debuggin

 ;

type
  TMyRecord = packed record
    Ref: TtiObject;
  end;
  PMyRecord = ^TMyRecord;


const
  AsTSortDirection: array[TvtSortDirection] of TSortDirection
   = (sdAscending, sdDescending);

function tiVTDisplayMaskFromDataType(const AValue : TvtTypeKind): string;
begin
  // ToDo: Should use OS constants
  case AValue of
    vttkString  : result := '';
    vttkInt     : result := '#,##0';
    vttkFloat   : result := '#,##0.000';
    vttkDate    : result := 'dd/mm/yyyy';
    vttkDateTime : result := 'dd/mm/yyyy hh:nn:ss';
    vttkCurrency : Result := '#,##0.00';
  else
    Assert(false, 'Invalid DataType');
  end;
end;



//______________________________________________________________________________________________________________________________
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

//______________________________________________________________________________________________________________________________
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


//______________________________________________________________________________________________________________________________
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

procedure TtiVTColumn.SetDataType(const AValue: TvtTypeKind);
begin
  if DisplayMask = tiVTDisplayMaskFromDataType(FDataType) then
    DisplayMask := tiVTDisplayMaskFromDataType(AValue);
  FDataType := AValue;

  case AValue of
    vttkString, vttkDate, vttkDateTime:
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

procedure TtiVTColumn.SetOnDeriveColumn(const AValue: TtiDeriveListColumnValue);
begin
  FOnDeriveColumn := AValue;
end;


//______________________________________________________________________________________________________________________________
{ TtiVTColumns }

constructor TtiVTColumns.Create(AOwner: TtiVTHeader);
begin
  inherited Create(AOwner);
  //TtiVTColumn);
  //FOwner := Owner;
end;

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

function TtiVTColumns.GetItem(Index: TColumnIndex): TtiVTColumn;
begin
  result := TtiVTColumn(inherited GetItem(Index));
end;

procedure TtiVTColumns.SetItem(Index: TColumnIndex; const AValue: TtiVTColumn);
begin
  inherited SetItem(Index, AValue);
end;


//______________________________________________________________________________________________________________________________
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



//______________________________________________________________________________________________________________________________
{ TtiCustomVirtualTree }

function TtiCustomVirtualTree.AddColumn(const AFieldName: string;
  const pDataType: TvtTypeKind; const pDisplayLabel: string;
  pColWidth: Integer): TtiVTColumn;
var
  lVTC: TtiVTColumn;
begin

  lVTC := VT.Header.Columns.Add as TtiVTColumn;
  try
    lVTC.Derived := AFieldName = '';
    lVTC.FieldName := AFieldName;
    lVTC.DataType := pDataType;
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

var {threadvar - declare threadvar for 100% thread safety and a significant performance hit}
  _SortOrders: TtiVTSortOrders;

function _DoSortElement(pOrder: TtiVTSortOrder; pData1, pData2: TtiObject): Integer;
  procedure _DoRaiseException(AFieldName : string; AClassName : string);
  begin
    raise exception.Create('Unable to read field <' +
                            AFieldName + '> from <' +
                            AClassName + '> in _DoSortData()');
  end;
var
  lval1: Variant;
  lval2: Variant;
begin
  Assert(pData1.TestValid(TtiObject), cTIInvalidObjectError);
  Assert(pData2.TestValid(TtiObject), cTIInvalidObjectError);

  if pData1.PropType(pOrder.FieldName) = tiTKString then
    lVal1 := UpperCase(pData1.PropValue[pOrder.FieldName])
  else
    lVal1 := pData1.PropValue[pOrder.FieldName];

  if pData2.PropType(pOrder.FieldName) = tiTKString then
    lVal2 := UpperCase(pData2.PropValue[pOrder.FieldName])
  else
    lVal2 := pData2.PropValue[pOrder.FieldName];

  if VarIsNull(lval1) then
    _DoRaiseException(pOrder.FieldName, pData1.ClassName);

  if VarIsNull(lval2) then
    _DoRaiseException(pOrder.FieldName, pData2.ClassName);

  if lval1 < lval2 then
    Result := -1
  else if lval1 > lval2 then
    Result := 1
  else
    Result := 0;

  if pOrder.SortDirection = vtsdDescending then
    Result := -Result;
end;

function _SortProc(pdata1, pdata2: Pointer): Integer;
var
  ldata1: TtiObject absolute pdata1;
  ldata2: TtiObject absolute pdata2;

  i: Integer;
begin
  Result := 0;
  for i := 0 to Pred(_SortOrders.Count) do
  begin
    Result := _DoSortElement(_SortOrders[i], ldata1, ldata2);
    if Result <> 0 then
      Break;
  end;
end;


procedure TtiCustomVirtualTree.ApplyGrouping;
var
  CurrentGroup: string;
  ItemGroup: string;
  FilteredIndex: Integer;
  ColumnIndex: Integer;
begin
  FGroupedData.Clear;
  FGroupingApplied := False;

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

procedure TtiCustomVirtualTree.ApplySort;
{$IFDEF _PROFILE}
var
  StartTick: Integer;
{$ENDIF}
begin
  if not Assigned(Data) then
    Exit;

  if SortOrders.Count = 0 then
    raise Exception.Create('No sort orders defined');

{$IFDEF _PROFILE}
  StartTick := GetTickCount;
{$ENDIF}
  _SortOrders := SortOrders;
  FFilteredData.Sort(_SortProc);
  _SortOrders := nil;
  FSorted := true;
  ApplyGrouping;
  SetRootNodeCount;
{$IFDEF _PROFILE}
  ShowMessage(Format('Sort time %d ms for %d items', [GetTickCount-StartTick, FFilteredData.Count]));
{$ENDIF}
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
  // Was incorrectly returning false
  //Result := Enabled and VT.Focused and not FReadOnly and (VT.SelectedCount = 1) and IsNodeDataItem(VT.FocusedNode);
  Result := Enabled and VT.Focused and not FReadOnly and (VT.SelectedCount = 1) and (SelectedData <> nil);

  if Result and Assigned(FOnCanEdit) then
    FOnCanEdit(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode, Result);
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
  ReadData;

  if Sorted then
    ApplySort;

  ApplyGrouping;

  SetRootNodeCount;
end;

constructor TtiCustomVirtualTree.Create(AOwner: TComponent);
begin
  inherited;

  BorderWidth := 2;

  FFilteredData := TObjectList.Create(False);

  FGroupedData := TList.Create;

  FSortOrders := TtiVTSortOrders.Create(Self);

  FVT := TtiInternalVirtualTree.Create(Self);
  FVT.Name:= tiGetUniqueComponentName(Name + 'VT');

  FVT.SetSubComponent(True);

  FVT.Parent := Self;
  FVT.NodeDataSize := SizeOf(Pointer);
  FVT.Align := alClient;
  FVT.OnHeaderClick := VTHeaderClick;
  FVT.OnHeaderDrawQueryElements := VTHeaderDrawQueryElements;
  FVT.OnAdvancedHeaderDraw := VTHeaderAdvancedHeaderDraw;
  // Property defaults
  FAlternateRowCount := 2;
  FShowAlternateRowColor := True;
  FAlternateRowColor := cDefaultAlternateRowColor;
  FDisabledColor := clBtnFace;
  FHeaderClickSorting:= True;
  VT.Header.Options := VT.Header.Options + [hoShowSortGlyphs, hoOwnerDraw];

  VT.OnDblClick := DoDblClick;
  VT.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, {toShowTreeLines,} toShowVertGridLines, toThemeAware, toUseBlendedImages];
  FRowSelect:= True;
  VT.TreeOptions.SelectionOptions := [toFullRowSelect];
  VT.Header.Style := hsXPStyle;

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
  // FVT.Free;
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
  Result := FVT.Header as TtiVTHeader;
end;

function TtiCustomVirtualTree.GetImages: TCustomImageList;
begin
  Result := VT.Images;
end;

function TtiCustomVirtualTree.GetObjectFromNode(pNode: PVirtualNode): TtiObject;
Var
  lData : PMyRecord;
Begin
  // ToDo: Must understand this better.

  if FGroupingApplied then
  begin
    if Assigned(pNode) then
      Result := PMyRecord(VT.GetNodeData(pNode)).Ref
    else
      Result := nil;
  end else
  begin
    // And this was used by PH as part of SelectedData.
    // This might be the problem with SelectedData?
    lData := PMyRecord(FVT.GetNodeData(pNode));
    if Assigned(lData) then
      Result := lData.Ref
    else
      Result := nil;
  end;
end;

function TtiCustomVirtualTree.GetOnKeyDown: TKeyEvent;
begin
  Result := VT.OnKeyDown;
end;

function TtiCustomVirtualTree.GetOnKeyPress: TKeyPressEvent;
begin
  Result := VT.OnKeyPress;
end;

function TtiCustomVirtualTree.GetSelectedData: TtiObject;
begin
  Result := GetObjectFromNode(VT.FocusedNode);
end;

function TtiCustomVirtualTree.GetTextFromObject(Obj: TtiObject; ColumnIndex: TColumnIndex): string;
var
  Column: TtiVTColumn;
  Field: string;
  Mask: string;
begin
  Assert(Assigned(Obj));
  Column := Header.Columns[ColumnIndex];
  Field := Column.FieldName;
  if (not Column.Derived) and (Field <> '') then
  begin
    Mask := Column.DisplayMask;
    case Column.DataType of
      vttkString:
        Result := GetPropValue(Obj, Field, True);

      vttkInt:
        Result := FormatFloat(Mask, GetPropValue(Obj, Field, True));

      vttkFloat:
        Result := FormatFloat(Mask, GetPropValue(Obj, Field, True));

      vttkDate:
        Result := FormatDateTime(Mask, GetPropValue(Obj, Field, True));

      vttkDateTime:
        Result := FormatDateTime(Mask, GetPropValue(Obj, Field, True));

      vttkCurrency:
        Result := Format('%m', [GetPropValue(Obj, Field, True)]);

      else
        Assert(False);
    end;
  end
  else if (Column.Derived) and (Assigned(Column.OnDeriveColumn)) then
    Column.OnDeriveColumn(Self, Obj, Column, Result)
  else
    Result := '<' + Column.FieldName + '> not correctly defined';
end;

function TtiCustomVirtualTree.IsNodeDataItem(Node: PVirtualNode): Boolean;
var
  Obj: TtiObject;
begin
  // If the node has grouped children, you have to select one of the children to edit
  Obj := GetObjectFromNode(Node);
  if Assigned(Obj) then
    if VT.GetNodeLevel(Node) = 0 then
      Result := CalcRootNodeChildCount(VT.FocusedNode) = 1
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
  FFilteredData.Clear;

  if Assigned(OnFilterData) then
  begin
    DataIndex := 0;
    while DataIndex < FData.Count do
    begin
      ShouldInclude := True;
      OnFilterData(FData[DataIndex], ShouldInclude);
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

procedure TtiCustomVirtualTree.Refresh(const pSelectedData: TtiObject = nil);
begin
  inherited Refresh;
  // ToDo: Can't get the thing to re-draw.
  // VT.Invalidate
  // VT.InvalidateToBottom(VT.TopNode);

  ConnectToData;

  if pSelectedData <> nil then
    SelectedData := pSelectedData;


//  else
//    First;
end;

procedure TtiCustomVirtualTree.SetAlternateRowColor(const AValue: TColor);
begin
  if FAlternateRowColor <> AValue then
  begin
    FAlternateRowColor := AValue;
    Invalidate;
  end;
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
      FVT.Invalidate;
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
    FVT.TreeOptions.SelectionOptions:= FVT.TreeOptions.SelectionOptions + [toFullRowSelect] - [toExtendedFocus];
    FVT.TreeOptions.MiscOptions:= FVT.TreeOptions.MiscOptions - [toGridExtensions];
  end else
  begin
    FVT.TreeOptions.SelectionOptions:= FVT.TreeOptions.SelectionOptions - [toFullRowSelect] + [toExtendedFocus];
    FVT.TreeOptions.MiscOptions:= FVT.TreeOptions.MiscOptions + [toGridExtensions];
  end;
end;

procedure TtiCustomVirtualTree.SetSelectedData(const AValue: TtiObject);
var
  lNode: PVirtualNode;
  lData: TtiObject;
begin
  if AValue = nil then
    Exit; //==>
  // This is the nastiest, slowest algorithm. It causes all child nodes to be initialised. But it works.
  lNode := VT.GetFirst;
  while Assigned(lNode) do
  begin
    lData := GetObjectFromNode(lNode);
    if vsHasChildren in lNode.States then
    begin
      if SetSelectedChildData(lNode, AValue) then
        Break; //-->
    end
    else if lData = AValue then
    begin
      FocusNode(lNode);
      Break; //-->
    end;
    lNode := VT.GetNextSibling(lNode);
  end;
end;


//procedure TtiCustomVirtualTree.SetSelectedDataIterateProc(
//  pSender: TBaseVirtualTree;
//  pNode: PVirtualNode;
//  AData: Pointer;
//  var pAbort: Boolean);
//var
//  lData1: TtiObject;
//  lData2: TtiObject;
//begin
//  lData1 := ObjectFromNode(pNode);
//  lData2 := TtiObject(AData);
//  if lData1 = lData2 then
//  begin
//    if FGroupingApplied and
//       (pNode.Parent <> nil) then
//        FVT.FullExpand(pNode.Parent);
//    FVT.Selected[pNode]:= True;
//    FVT.FocusedNode := pNode;
//    pAbort := True;
//  end;
//end;

//procedure TtiCustomVirtualTree.SetSelectedData(const AValue: TtiObject);
//begin
//  Assert(AValue.TestValid(TtiObject), cTIInvalidObjectError);
//  FVT.IterateSubtree(nil, SetSelectedDataIterateProc, AValue);
//end;

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
    FVT.OnGetHint:= DoOnGetHint;
    FVT.HintMode:= hmHint;
    FVT.ShowHint:= true;
  end else
  begin
    FVT.OnGetHint:= nil;
    FVT.HintMode:= hmDefault;
    FVT.ShowHint:= false;
  end;
end;

procedure TtiCustomVirtualTree.SetOnBeforeCellPaint(const AValue: TtiVTOnPaintText);
begin
  FOnBeforeCellPaint := AValue;
  if Assigned(FOnBeforeCellPaint) then
    FVT.OnBeforeCellPaint := DoOnBeforeCellPaint
  else
    FVT.OnBeforeCellPaint := nil;
end;

procedure TtiCustomVirtualTree.SetOnPaintText(
  const AValue: TtiVTOnPaintText);
begin
  FOnPaintText := AValue;
  if Assigned(FOnPaintText) then
    FVT.OnPaintText := DoOnPaintText
  else
    FVT.OnPaintText := nil;
end;

procedure TtiCustomVirtualTree.FocusNode(ANode: PVirtualNode);
begin
  VT.IsVisible[ANode]:= True;
  VT.Selected[ANode]:= True;
  VT.ScrollIntoView(ANode, True, False);
  VT.FocusedNode := ANode;
end;

function TtiCustomVirtualTree.SetSelectedChildData(Parent: PVirtualNode; Data: TtiObject): Boolean;
var
  lNode: PVirtualNode;
begin
  Assert(Assigned(Parent));
  Result := False;

  if (vsHasChildren in Parent.States) and (Parent.ChildCount = 0) then
    TtiInternalVirtualTree(VT).InitChildren(Parent);

  lNode := VT.GetFirstChild(Parent);
  while Assigned(lNode) and not Result do
  begin
    if GetObjectFromNode(lNode) = Data then
    begin
      FocusNode(lNode);
      Result := True;
    end
    else
      lNode := VT.GetNextSibling(lNode);
  end;
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

  if VT.GetNodeLevel(Node) < 2 then
  begin
    RootNode := VT.NodeParent[Node];
    if not Assigned(RootNode) then
      RootNode := Node;

    if FShowAlternateRowColor and
       (RootNode.Index mod FAlternateRowCount = 0) then
      ItemColor := AlternateRowColor
    else
      ItemColor := VT.Color;

    EraseAction := eaColor;
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
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
var
  Obj: TtiObject;
begin
  Obj := GetObjectFromNode(Node);

  if VT.GetNodeLevel(Node) = 0 then
  begin
    if (Column >= SortOrders.GroupColumnCount) and (CalcRootNodeChildCount(Node) > 1) then
      Text := ''
    else
      Text := GetTextFromObject(Obj, Column);
  end
  else
  begin
    // must be level 1
    if (Column < SortOrders.GroupColumnCount) then
      Text := ''
    else
      Text := GetTextFromObject(Obj, Column);
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
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
const
  // TvtSortDirection = (vtsdAscending, vtsdDescending);
  InvertSortDirection: array[TvtSortDirection] of TvtSortDirection
    = (vtsdDescending, vtsdAscending);

var
  LColumn: TtiVTColumn;
  LSortOrder: TtiVTSortOrder;
  LNewSortDirection: TvtSortDirection;
  LExistingSortOrder: boolean;
begin
  if HeaderClickSorting then
  begin
    LColumn := Header.Columns[Column];
    LColumn.Style := vsOwnerDraw;
    LExistingSortOrder := SortOrders.ItemByFieldName(LColumn.FieldName, LSortOrder);

    if LExistingSortOrder then
      LNewSortDirection := InvertSortDirection[LSortOrder.SortDirection]
    else
      LNewSortDirection := vtsdAscending;

    if not (ssCtrl in Shift) then
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

    ApplySort;

    if Assigned(OnHeaderClick) then
      OnHeaderClick(self, Header.Columns[Column]);

  end;

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
  FVT.SetFocus;
end;

procedure TtiCustomVirtualTree.DoOnBeforeCellPaint(ASender: TBaseVirtualTree;
  ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
  ACellRect: TRect);
var
  LData: TtiObject;
begin
  if Assigned(FOnBeforeCellPaint) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnBeforeCellPaint(Self, ATargetCanvas, LData, AColumn, ANode);
  end;
end;

procedure TtiCustomVirtualTree.DoOnGetHint(Sender: TBaseVirtualTree;
  ANode: PVirtualNode; Column: TColumnIndex;
  var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: WideString);
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

//______________________________________________________________________________________________________________________________

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
    lNode := FVT.FocusedNode;
    FOnDblClick(Self, GetObjectFromNode(lNode), lNode);
  end;
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
    FVT.Enabled := AValue;
    // ToDo: Make these properties on TtiCustomVirtualTree;
    if AValue then
      FVT.Color := clWindow
    else
      FVT.Color := FDisabledColor;
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

procedure TtiInternalVirtualTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer);
begin
  FtiOwner.VTGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

procedure TtiInternalVirtualTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
begin
  FtiOwner.VTDoGetText(Node, Column, TextType, Text);
end;

procedure TtiInternalVirtualTree.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal);
begin
  FtiOwner.VTInitChildren(Node, ChildCount);
end;

procedure TtiInternalVirtualTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  FtiOwner.VTInitNode(Parent, Node, InitStates);
end;

function TtiInternalVirtualTree.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TtiVTColumn;
end;

function TtiInternalVirtualTree.GetHeaderClass: TVTHeaderClass;
begin
  Result := TtiVTHeader;
end;


//______________________________________________________________________________________________________________________________
{ TtiCustomVirtualEditTree }

function TtiCustomVirtualEditTree.CanDelete: Boolean;
begin
  Result := (tiLVBtnVisDelete in VisibleButtons) and inherited CanDelete;
end;

function TtiCustomVirtualEditTree.CanEdit: Boolean;
begin
  Result := (tiLVBtnVisEdit in VisibleButtons) and inherited CanEdit;
end;

function TtiCustomVirtualEditTree.CanInsert: Boolean;
begin
  Result := (tiLVBtnVisNew in VisibleButtons) and inherited CanInsert;
end;

function TtiCustomVirtualEditTree.CanView: Boolean;
begin
  Result := (tiLVBtnVisView in VisibleButtons) and inherited CanView;
end;

constructor TtiCustomVirtualEditTree.Create(AOwner: TComponent);
begin
  inherited;

  // Create the popup menu
  FPopupMenu := TPopupMenu.Create(self);
  FPopupMenu.Images := gTIImageListMgr.ILNormal16;
  FPopupMenu.OnPopup := DoMenuPopup;
  PopupMenu := FPopupMenu;

  // Create select columns menu item
  FpmiView         := TMenuItem.Create(self);
  FpmiView.Caption := '&View';
  FpmiView.OnClick := DoView;
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

  FpmiClearSort         := TMenuItem.Create(self);
  FpmiClearSort.Caption := '&Clear Sort';
  FpmiClearSort.OnClick := DoClearSort;
  FpmiClearSort.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Sort);
  FpmiClearSort.Visible := true;
  FPopupMenu.Items.Add(FpmiClearSort);

  FStdPopupItemCount := FPopupMenu.Items.Count;
  FButtonStyle := lvbsButtonsAndLabels;
end;

procedure TtiCustomVirtualEditTree.CreateButtonPanel;
begin
  tiCtrlButtonPanel.CreateCtrlBtnPnl(FCtrlBtnPnl, ButtonStyle, Self,
                                      CanView, CanInsert, CanEdit, CanDelete);
  FCtrlBtnPnl.Align := alTop;
  FCtrlBtnPnl.VisibleButtons := FVisibleButtons;

  if Assigned(FOnItemDelete) then
    FCtrlBtnPnl.OnDelete := DoDelete;
  if Assigned(FOnItemEdit) then
    FCtrlBtnPnl.OnEdit := DoEdit;
  if Assigned(FOnItemInsert) then
    FCtrlBtnPnl.OnNew := DoNew;
  if Assigned(FOnItemView) then
    FCtrlBtnPnl.OnView := DoView;

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
  else if CanEdit then
    DoEdit(Self)
  else if CanView then
    DoView(Self)
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
  if Assigned(FOnItemEdit) then
  begin
    FVT.SetFocus;
    FOnItemEdit(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.DoNew(Sender: TObject);
begin
  if Assigned(FOnItemInsert) then
  begin
    FVT.SetFocus;
    FOnItemInsert(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.DoView(Sender: TObject);
begin
  if Assigned(VT.FocusedNode) and Assigned(FOnItemView) then
  begin
    FVT.SetFocus;
    FOnItemView(Self, GetObjectFromNode(VT.FocusedNode), VT.FocusedNode);
  end;
end;

procedure TtiCustomVirtualEditTree.Loaded;
begin
  inherited;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.RefreshButtons;
end;

procedure TtiCustomVirtualEditTree.DoMenuPopup(Sender: TObject);
var
  i : Integer;
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
  FVT.FullExpand(nil);
end;

procedure TtiCustomVirtualEditTree.DoCollapseAll(Sender: TObject);
begin
  FVT.FullCollapse(nil);
end;

procedure TtiCustomVirtualEditTree.SetButtonStyle(const AValue: TLVButtonStyle);
begin
  if FButtonStyle <> AValue then
  begin
    FButtonStyle := AValue;
    CreateButtonPanel;
  end;
end;

procedure TtiCustomVirtualEditTree.SetOnClearSort(const AValue: TtiVTClearSortEvent);
begin
  FOnClearSort := AValue;
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
      FCtrlBtnPnl.OnView := DoView
    else
      FCtrlBtnPnl.OnView := nil;
end;

procedure TtiCustomVirtualEditTree.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FVisibleButtons := AValue;
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.VisibleButtons := AValue;
end;

procedure TtiCustomVirtualEditTree.VTDoFocusChanged(Node: PVirtualNode; Column: TColumnIndex);
begin
  if Assigned(FCtrlBtnPnl) then
    FCtrlBtnPnl.EnableButtons;
  inherited;
end;

procedure TtiCustomVirtualEditTree.Refresh(const pSelectedData: TtiObject);
begin
  inherited;
  DoRefreshButtons;
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

end.

