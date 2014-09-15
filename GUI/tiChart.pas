unit tiChart;

{$I tiDefines.inc}

interface

uses
  Classes, Controls, Contnrs, Forms, Graphics, Menus, VCLTee.Series, Types, StdCtrls,
  ExtCtrls, Grids,
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, Tabs, DockTabSet, ComCtrls,
  tiBaseObject, tiObject, tiSpeedButton, tiRoundedPanel, tiResources, tiPerAwareCtrls
  ,tiVTTreeView
  ,tiVirtualTrees
;

const
  cZoomPercent = 10;
  cChartLegendItemHeight = 28;
  cChartLegendItemCheckBoxLeft = 50;

type

  TtiChartLegendPanel = class;
  TtiChartLegendFormAbs = class;
  TtiChartLegendForm = class;

  TtiClearPanel = class(TCustomPanel)
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    property Canvas;
  end;

  TtiTimeSeriesChart = class;

  TtiChartTestDataItem = class(TtiObject)
  private
    FXValue: Real;
    FYValue1: Real;
    FYValue2: Real;
  public
    property XValue: Real read FXValue write FXValue;
    property YValue1: Real read FYValue1 write FYValue1;
    property YValue2: Real read FYValue2 write FYValue2;
  end;

  TtiChartTestData = class(TtiObjectList)
  public
    constructor Create; override;
    procedure AssignGraphData(const AData: TObject; const AChart: TtiTimeSeriesChart);
    procedure DataGap(const ADataBeforeGap: TObject; const ADataAfterGap: TObject; const AChart:
        TtiTimeSeriesChart);
  end;

  TtiChartDataMapping = class(TCollectionItem)
  private
    FDisplayLabel: string;
    FPropertyName: string;
    procedure SetPropertyName(const AValue: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;
    function Clone: TtiChartDataMapping;
  published
    property DisplayLabel: string read FDisplayLabel write FDisplayLabel;
    property PropertyName: string read FPropertyName write SetPropertyName;
  end;

  TtiChartDataMappings = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: integer): TtiChartDataMapping;
    procedure SetItem(Index: integer; const AValue: TtiChartDataMapping);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner : TComponent);
    destructor Destroy; override;
    function Add: TtiChartDataMapping;
    procedure Clear; reintroduce;
    function FindByFieldName(const AFieldName : string): TtiChartDataMapping;
    procedure NamesToStringList(const AStringList : TStringList);
    property Items[Index: integer]: TtiChartDataMapping read GetItem write
        SetItem;
  end;

  TtiChartSeries = class(TtiObject)
  private
    FSeriesSource: TtiObject;
    FChartSeries: TChartSeries; // Owns
    FDataAssigned: boolean;
  public
    destructor Destroy; override;
    property SeriesSource: TtiObject read FSeriesSource write FSeriesSource;
    property ChartSeries: TChartSeries read FChartSeries write FChartSeries;
    property DataAssigned: boolean read FDataAssigned write FDataAssigned;
  end;

  TtiChartSeriesList = class(TtiObjectList)
  private
    function GetItems(Index: integer): TtiChartSeries; reintroduce;
    procedure SetItems(Index: integer; const AValue: TtiChartSeries); reintroduce;
  public
    function FindByChartSeries(const AChartSeries: TChartSeries): TtiChartSeries;
    function SeriesNameToTitle(const ASeriesName: string): string;
    property Items[Index: integer]: TtiChartSeries read GetItems write SetItems;
  end;

  TAssignGraphDataEvent = procedure (const AData: TObject;
      const AChart: TtiTimeSeriesChart) of object;
  TAssignSeriesDataEvent = procedure (const AData: TObject;
      const AChart: TtiTimeSeriesChart; const AChartSeries: TtiChartSeries) of object;
  TChartDataGapEvent = procedure (const ADataBeforeGap: TObject;
      const ADataAfterGap: TObject; const AChart: TtiTimeSeriesChart) of object;
  TSeriesDataGapEvent = procedure (const ADataBeforeGap: TObject; const ADataAfterGap: TObject;
      const AChart: TtiTimeSeriesChart; const AChartSeries: TtiChartSeries) of object;
  TCrossHairEvent = procedure (const ASeries : TChartSeries; const AIndex : integer;
      const AData: TObject; const AList : TtiObjectList) of object;
  TRangeChangeEvent = procedure(
      const AChart: TtiTimeSeriesChart;
      const AZoomed: Boolean;
      const ABottomAxisMin: TDateTime;
      const ABottomAxisMax: TDateTime;
      const ALeftAxisMin: Double;
      const ALeftAxisMax: Double) of object;
  TGetDataByDateTimeEvent = procedure(const ADateTime: TDateTime;
      out AData: TObject) of object;
  TVisibleSeriesChangeEvent = procedure(const ASeriesName: string;
      const AVisible: boolean) of object;

  TtiChartInternal = class(TChart)
  protected
    procedure Paint; override;
  end;

  TtiChartPanel = class(TtiClearPanel)
  private
    FChart: TChart;
    FChartPanel: TtiClearPanel;
    FParenttiChart: TtiTimeSeriesChart;
    FscbHorizontal: TScrollBar;
    FscbVertical: TScrollBar;
    FScrollStyle: TScrollStyle;
    procedure SetScrollStyle(const AValue: TScrollStyle);
  public
    constructor Create(Owner : TComponent; AParenttiChart: TtiTimeSeriesChart); reintroduce; overload;
    destructor Destroy; override;
    property Chart: TChart read FChart write FChart;
    property scbHorizontal: TScrollBar read FscbHorizontal;
    property scbVertical: TScrollBar read FscbVertical;
    property ScrollBars: TScrollStyle read FScrollStyle write SetScrollStyle;
  end;

  TtiChartLegendPosition = (clpLeft, clpRight);

  // XE2 Hack: There is a problem exporting types from VCLTee.* units (no source provided)
  // in VCL.bpl for use in other projects  - the compiler complains that types are
  // undefined
  // Wrapping VCLTee.* types in a local type solves the linking problem.
  TtiChartSeries2 = TChartSeries;

  ItiChartLegendItemData = interface
    ['{C749B34A-331A-4863-A7EB-B414B81F607A}']
    function GetChartSeries(const AChart: TtiTimeSeriesChart): TtiChartSeries2;
  end;

  TtiChartLegendItem = class(TtiClearPanel, ItiChartLegendItemData)
  private
    FChartSeries: TtiChartSeries2;
    FCheckBox: TCheckBox;
    FLabel: TLabel;
    FtiChart   : TtiTimeSeriesChart;
    procedure DoOnCheckBoxClick(Sender: TObject);
    function GetChecked: Boolean;
    procedure SetChecked(const AValue: Boolean);
    function GetChartSeries(const AChart: TtiTimeSeriesChart): TtiChartSeries2;
  protected
    procedure Paint; override;
  public
    constructor Create(const AOwner: TtiChartLegendForm;
      const AChart: TtiTimeSeriesChart; const ASeries: TtiChartSeries2); reintroduce;
    destructor Destroy; override;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TOnSetCaptionEvent = procedure(const ACaption: string) of object;
  TOnSetUserPanelForm = procedure(const AForm: TForm) of object;

  TtiChartUserPanel = class(TtiBaseObject)
  private
    FCaption: string;
    FForm: TForm;
    FOnSetCaption: TOnSetCaptionEvent;
    FOnSetForm: TOnSetUserPanelForm;
    procedure SetCaption(const ACaption: string);
    procedure SetForm(const AForm: TForm);
  public
    property Form: TForm read FForm write SetForm;
    property Caption: string read FCaption write SetCaption;
    property OnSetCaption: TOnSetCaptionEvent read FOnSetCaption write FOnSetCaption;
    property OnSetForm: TOnSetUserPanelForm read FOnSetForm write FOnSetForm;
  end;

  TtiChartLegendKind = (clList, clTree);

  TtiChartLegendPanel = class(TtiClearPanel)
  private
//    FDockTabSet: TDockTabSet;
//    FButonPanel: TtiClearPanel;
//    FLegendFormPanel: TtiClearPanel;
//    FlegendButton: TtiSpeedButton;
    FPageControl: TPageControl;
    FChartLegendForm: TtiChartLegendFormAbs;
    FParenttiChart: TtiTimeSeriesChart;
    FLegendTabSheet: TTabSheet;
    FUserPanelTabSheet: TTabSheet;
    FUserPanelForm: TForm;
    FUserPanel: TtiChartUserPanel;
    procedure SetUserPanelCaption(const ACaption: string);
    procedure SetUserPanelForm(const AForm: TForm);
//    procedure DoUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
  public
    constructor CreateNew(Owner : TComponent; AParenttiChart: TtiTimeSeriesChart;
        const ALegendKind: TtiChartLegendKind;
        ALegendPosition: TtiChartLegendPosition; Dummy : integer = 0); reintroduce; overload;
    destructor Destroy; override;
    procedure SelectUserPanel;
    property ChartLegendForm: TtiChartLegendFormAbs read FChartLegendForm;
    property UserPanel: TtiChartUserPanel read FUserPanel;
  end;

  TtiChartLegendFormAbsEvent = procedure(const ASender: TtiChartLegendFormAbs) of object;

  // Form to display a legend (a small graphic of the seris - with it's title)
  TtiChartLegendFormAbs = class(TCustomForm)
  private
    FOnCreateSeries: TtiChartLegendFormAbsEvent;
    procedure SetOnCreateSeries(const AEventHandler: TtiChartLegendFormAbsEvent);
  protected
    FChart: TtiTimeSeriesChart;
    FPopupMenu : TPopupMenu;
    FpmiSelectAll: TMenuItem;
    FpmiSelectNone: TMenuItem;
    function GetSeriesVisible(const ASeriesName: string): boolean; virtual; abstract;
    procedure SetSeriesVisible(const ASeriesName: string; const AVisible: boolean);  virtual; abstract;

    procedure DoSelectAll(Sender: TObject);  virtual; abstract;
    procedure DoSelectNone(Sender: TObject);  virtual; abstract;
  public
    Constructor CreateNew(const AChart: TtiTimeSeriesChart); reintroduce; overload;
    destructor Destroy; override;
    procedure ClearSeries; virtual; abstract;
    // we can override this method or provide a delegate via OnCreateSeries, or both(!)
    procedure CreateSeries; virtual;
    property  SeriesVisible[const ASeriesName: string]: boolean read GetSeriesVisible write SetSeriesVisible;
    procedure SetSeriesVisibleByCaption(const ASeriesTitle: string; const AVisible: Boolean);  virtual; abstract;
    function IsSeriesVisibleByCaption(const ASeriesTitle: string): boolean; virtual; abstract;
    property Chart: TtiTimeSeriesChart read FChart;
  published
    property OnCreateSeries: TtiChartLegendFormAbsEvent read FOnCreateSeries write SetOnCreateSeries;
  end;

  TtiChartLegendForm = class(TtiChartLegendFormAbs)
  private
    FList: TObjectList;
    function FindBySeriesName(const ASeriesName: string): TtiChartLegendItem;
    function FindBySeriesTitle(const ASeriesTitle: string): TtiChartLegendItem;

  protected
    function GetSeriesVisible(const ASeriesName: string): boolean; override;
    procedure SetSeriesVisible(const ASeriesName: string; const AVisible: boolean); override;
    procedure DoSelectAll(Sender: TObject); override;
    procedure DoSelectNone(Sender: TObject); override;
  public
    Constructor CreateNew(const AChart: TtiTimeSeriesChart);
    destructor Destroy; override;
    procedure ClearSeries; override;
    procedure CreateSeries; override;
    procedure SetSeriesVisibleByCaption(const ASeriesTitle: string; const AVisible: Boolean); override;
    function IsSeriesVisibleByCaption(const ASeriesTitle: string): boolean; override;
  end;

  TtiChartSeriesMatchByText = function (
    const AChartLegendItemData: ItiChartLegendItemData; const APropertyValue: string): boolean of object;

  TtiChartLegendTreeViewNodeByTextFinder = function (
    const APropertyValue: string; out ANode: PVirtualNode): boolean of object;

  TtiChartLegendTreeViewForm = class(TtiChartLegendFormAbs)
  private
    FTreeView: TtiVTTreeView;

    procedure VTTVSelectNode(AtiVTTreeView: TtiVTTreeView; ANode: PVirtualNode;
      AData: TtiObject);
    procedure VTTVNodeCheckboxClick(AtiVTTreeView: TtiVTTreeView;
      ANode: PVirtualNode; AData: TtiObject; ASetChecked: Boolean);
    function GetSelectedElement: TtiObject;
    procedure SetSelectedElement(const AElement: TtiObject);
    function GetShowEmptyRootNode: boolean;
    procedure SetShowEmptyRootNode(const AShowNode: boolean);
    function GetOnSelectNode: TtiVTTVNodeEvent;
    procedure SetOnSelectNode(const ADelegate: TtiVTTVNodeEvent);
    function SameSeriesName(const AChartLegendItemData: ItiChartLegendItemData;
      const ASeriesName: string): boolean;
    function SameSeriesTitle(const AChartLegendItemData: ItiChartLegendItemData;
      const ASeriesTitle: string): boolean;
    function FindNodeBySeriesProperty(
      const ACompare: TtiChartSeriesMatchByText;
      const APropertyValue: string; out ANode: PVirtualNode): boolean;
    function FindNodeBySeriesName(const ASeriesName: string;
      out ANode: PVirtualNode): boolean;
    function FindNodeBySeriesTitle(const ASeriesTitle: string;
      out ANode: PVirtualNode): boolean;
    procedure SelectAll(const ASelected: boolean);
    procedure SetSeriesVisibleByText(
      const AFindNodeByText: TtiChartLegendTreeViewNodeByTextFinder;
      const AValue: string; const AVisible: boolean);
  protected
    function GetSeriesVisible(const ASeriesName: string): boolean; override;
    procedure SetSeriesVisible(const ASeriesName: string; const AVisible: boolean); override;
    procedure DoSelectAll(Sender: TObject); override;
    procedure DoSelectNone(Sender: TObject); override;
  public
    constructor CreateNew(const AChart: TtiTimeSeriesChart);
    procedure ClearSeries; override;
    procedure SetSeriesVisibleByCaption(const ASeriesTitle: string; const AVisible: Boolean); override;
    function IsSeriesVisibleByCaption(const ASeriesTitle: string): boolean; override;
    function AddDataMapping(const AClass: TtiClass): TtiVTTVDataMapping;
    property TreeView: TtiVTTreeView read FTreeView;

  published
    property SelectedElement: TtiObject read GetSelectedElement write SetSelectedElement;
    property ShowEmptyRootNode: boolean read GetShowEmptyRootNode write SetShowEmptyRootNode;
    property OnSelectNode: TtiVTTVNodeEvent read GetOnSelectNode write SetOnSelectNode;
  end;

  TtiChartWithLegendPanel = class(TtiClearPanel)
  private
    FChartLegendPanel: TtiChartLegendPanel;
    FLegendPosition: TtiChartLegendPosition;
    FParenttiChart: TtiTimeSeriesChart;
    FtiChartPanel: TtiChartPanel;
    function GetShowLegend: Boolean;
    procedure SetShowLegend(const AValue: Boolean);
  public
    constructor Create(Owner : TComponent; ALegendPosition:
        TtiChartLegendPosition; const ALegendKind: TtiChartLegendKind); reintroduce; overload;
    destructor Destroy; override;
    property ChartLegendPanel: TtiChartLegendPanel read FChartLegendPanel;
    property ShowLegend: Boolean read GetShowLegend write SetShowLegend;
  end;

  TtiChartManualZoomForm = class(TCustomForm)
  private
    FgbMinimum: TGroupBox;
    FdedtMinDate: TtiPerAwareDateEdit;
    FgbMaximum: TGroupBox;
    FdedtMaxDate: TtiPerAwareDateEdit;
    FtedtMaxTime: TtiPerAwareTimeEdit;
    FtedeMinTime: TtiPerAwareTimeEdit;
    FbtnOK: TtiSpeedButton;
    FbtnCancel: TtiSpeedButton;
    FfedtMinYAxis: TtiPerAwareFloatEdit;
    FfedtMaxYAxis: TtiPerAwareFloatEdit;

    FChart: TtiTimeSeriesChart;

    procedure DoCancel(Sender: TObject);
    procedure DoOK(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure Paint; override;

    property Chart: TtiTimeSeriesChart read FChart write FChart;
    procedure Execute(const AChart: TtiTimeSeriesChart; const AManualZoomButton: TtiSpeedButton);
  end;

  TtiChartButtonsPosition = (cbpLeft, cbpTop);

  TtiChartButtonDetailList = class;
  TtiChartButtonsPanel = class;

  TtiChartButtonDetail = class(TtiObject)
  private
    FHint: string;
    FImageResName: string;
    FDoButtonClick: TNotifyEvent;
  protected
    function    GetParent: TtiChartButtonDetailList; reintroduce;
  public
    constructor Create(
        const AHint: string;
        const AImageResName: string;
        const ADoButtonClick: TNotifyEvent); reintroduce;
    function    CreateButton(const AParent: TtiChartButtonsPanel; const APosLeft, APosTop: Integer): TtiSpeedButton;
    property    Parent: TtiChartButtonDetailList read GetParent;
  published
  end;

  TtiChartButtonDetailList = class(TtiObjectList)
  private
    FButtonComponentList: TObjectList;
  protected
    function    GetItems(i: integer): TtiChartButtonDetail; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiChartButtonDetail); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Add(const AObject: TtiChartButtonDetail); reintroduce;
    procedure   Insert(const AIndex: integer; const AObject: TtiChartButtonDetail); reintroduce;
    procedure   AddButton(
        const AButtonHint: string;
        const AImageResName: string;
        const ADoButtonClick: TNotifyEvent;
        const AButtonOrder: Integer = -1);
    procedure   CreateButtons(const AParent: TtiChartButtonsPanel; const AChartButtonsPosition: TtiChartButtonsPosition);
    property    Items[i:integer] : TtiChartButtonDetail read GetItems write SetItems;
  end;

  TtiChartButtonsPanel = class(TtiClearPanel)
  private
    FChartButtonsPosition: TtiChartButtonsPosition;
    FParenttiChart: TtiTimeSeriesChart;
    FChartButtonDetailList: TtiChartButtonDetailList;
 //   procedure SnapEditDialogToButton(pForm: TForm; pSender: TObject);
  public
    constructor Create(Owner : TComponent; AButtonsPosition:
        TtiChartButtonsPosition); reintroduce; overload;
    destructor Destroy; override;
    procedure   AddButton(
        const AButtonHint: string;
        const AImageResName: string;
        const ADoButtonClick: TNotifyEvent;
        const AButtonOrder: Integer);
  end;

  TtiDisplayGrid = class( TCustomPanel )
  private
    FGrid : TStringGrid ;
    FPnl: TtiRoundedPanel;
    function GetColCount: integer;
    function GetRowCount: integer;
    procedure SetColCount(const AValue: integer);
    procedure SetRowCount(const AValue: integer);
    function  GetCells(const ACol, ARow: integer): string;
    procedure SetCells(const ACol, ARow: integer; const AValue: string);
    procedure CheckCellCount(const ACol, ARow: integer);
    procedure HideSelected ;
    procedure SetRowHeight(const ARow : integer ; const AText : string ) ;
    procedure SetColWidth(const ACol : integer ; const AText : string ) ;
  published
    property Anchors     ;
    property Alignment   ;
    property BevelInner  ;
    property BevelOuter  ;
    property BorderStyle ;
    property ColCount : integer read GetColCount write SetColCount default 2 ;
    property RowCount : integer read GetRowCount write SetRowCount default 2 ;
  public
    Constructor Create( Owner : TComponent ) ; override ;
    Destructor Destroy; override;
    property    Cells[ const ACol, ARow : integer ] : string
                  read GetCells write SetCells ;
    procedure   Clear ;
    procedure   ClearCol(const ACol : integer ) ;
    procedure   ClearCell(const ACol, ARow : integer ) ;
    procedure   ClearRow(const ARow : integer ) ;
    function    IsEmpty: boolean;
    procedure   SetContent(const AContent: string);
  end ;

  TtiDataPointHintForm = class(TForm)
  private
    { Private declarations }
//    FDG: TtiDisplayGrid;
    FContent: TMemo;
    FFirstShow: boolean;
    function GetContent: string;
    procedure SetContent(const Value: string);
    function MaxStringWidth(const AStrings: TStrings;
      const ACanvas: TCanvas): integer;
//    FFadeInTime: Cardinal;
//    FinalAlphaBlendValue: Byte;
  public
    { Public declarations }
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure Show;
//    function GridIsEmpty: Boolean;
//    property DG : TtiDisplayGrid read FDG write FDG;
    property  Content: string read GetContent write SetContent;
  end;

  // ToDo: Refactor into TtiChart & TtiChartTimeSeries
  TtiChart = class(TtiClearPanel)
  end;

  TtiTimeSeriesChart = class(TtiChart)
  private
    FCrossHairsDrawn: Boolean;
    FDataCircleDrawn: Boolean;
    FDisplayTestData: Boolean;
    FDrawCrossHairs: Boolean;
    FDrawCrossHairsNow: Boolean;
    FDrawCrossHairsSaved: Boolean;
    FShowTestData: Boolean;
    FTimeSeriesChart: Boolean;
    FButtonsPanel: TtiChartButtonsPanel;
    FButtonsPosition: TtiChartButtonsPosition;
    FChart: TChart;
    FChartDataMappings: TtiChartDataMappings;
    FChartLegendForm: TtiChartLegendFormAbs;
    FChartPanel: TtiChartPanel;
    FChartWithLegendPanel: TtiChartWithLegendPanel;
    FConstrainViewToData: Boolean;
    FData: TtiObjectList;
    FDataUnderMouse: TObject;
    FDataPointHintForm: TtiDataPointHintForm;
    FOldCircX: Integer;
    FOldCircY: Integer;
    FOldX: Integer;
    FOldY: Integer;
    FSnapToDataRadius: Integer;
    FSnapToDataSize: Integer;
    FLegendPosition: TtiChartLegendPosition;
    FOnAssignGraphData: TAssignGraphDataEvent;
    FOnAssignSeriesData: TAssignSeriesDataEvent;
    FOnCrossHair: TCrossHairEvent;
    FOnChartDataGap: TChartDataGapEvent;
    FOnSeriesDataGap: TSeriesDataGapEvent;
    FOnRangeChange: TRangeChangeEvent;
    FOnGetDataByDateTime: TGetDataByDateTimeEvent;
    FOnVisibleSeriesChange: TVisibleSeriesChangeEvent;
    FBottomAxisMax: Real;
    FBottomAxisMin: Real;
    FLeftAxisMax: Real;
    FLeftAxisMin: Real;
    FXDataValueUnderMouse: Real;
    FXValueUnderMouse: Real;
    FYDataValueUnderMouse: Real;
    FYValueUnderMouse: Real;

    FScrollStyle: TScrollStyle;
    FTestData: TtiChartTestData;
    FShowDataPointHintTmr: TTimer;
    FSeriesList: TtiChartSeriesList;
    FNeedRefreshSeries: boolean;

    FManualZoomForm: TtiChartManualZoomForm;
    FIncludeLegendInClipboard: boolean;

    procedure AdjustScrollBarPositions;
    procedure ClearMousePositionVisualCues;
    procedure HideCrossHairs;
    procedure ShowCrossHairs(const AX, AY: Integer);
    procedure DoChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
        Integer);
    procedure DoDrawCrossHairs(const AX, AY: Integer; const ADoDraw : boolean);
    procedure DoHorizontalScroll(Sender : TObject);
    procedure DoMouseDown(Sender: TObject;Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoMouseUp(Sender: TObject;Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure DoOnAllowScroll(Sender: TChartAxis; var AMin: double; var AMax:
        double; var AllowScroll: boolean);
    procedure DoOnBeforeDrawAxes(sender : TObject);
    procedure DoOnScroll(Sender : TObject);
    procedure DoOnUndoZoom(Sender : TObject);
    procedure DoOnZoom(Sender : TObject);
    procedure DoOnAfterDraw(Sender : TObject);
    procedure DoSBCopyToClipBrdClick(sender : TObject);
    procedure DoSBDefaultZoomClick(sender : TObject);
//    procedure DoSBViewLegendClick(sender : TObject);
    procedure DoSBZoomInClick(sender : TObject);
    procedure DoSBZoomOutClick(sender : TObject);
    procedure DoSBDetailZoomClick(sender: TObject);
    procedure DoSBManualZoomClick(sender: TObject);
    procedure DoVerticalScroll(Sender : TObject);
    function FscbBottom: TScrollBar;
    function FscbLeft: TScrollBar;
    function GetAxisBottom: TChartAxis;
    function GetAxisLeft: TChartAxis;
    function GetChartColor: TColor;
    function GetChartPopupMenu: TPopupMenu;
    function GetMouseInCrossHairRegion: Boolean;
    function GetNextSeriesColor: TColor;
    function GetOnDblClickChart: TNotifyEvent;
    function GetOwnerForm(Control: TComponent): TForm;
    function GetView3D: Boolean;
    procedure HideDataPointHint;
    function IsFormFocused: Boolean;
    function LineSeriesPointerVisible: Boolean;
    procedure OnDrawKey(pSeries : TChartSeries; AIndex : integer; AData :
        TObject; AList : TtiObjectList);
    procedure ResetMousePositionVisualCues;
    procedure ResetZoom;
    function SeriesTitleToName(const ATitle: string): string;
    procedure SetChartColor(const AValue: TColor);
    procedure SetChartPopupMenu(const AValue: TPopupMenu);
    procedure SetConstrainViewToData(const Value: Boolean);
    procedure SetData(const AValue: TtiObjectList);
    procedure SetDrawCrossHairs(const AValue: Boolean);
    procedure SetDrawCrossHairsNow(const AValue: Boolean);
//    procedure SetLayeredAttribs;
    procedure SetOnDblClickChart(const AValue: TNotifyEvent);
    procedure SetScrollStyle(const AValue: TScrollStyle);
    procedure SetSeriesPointerVisible(const AValue: Boolean);
    procedure SetShowTestData(const AValue: Boolean);
    procedure SetSnapToDataSize(const AValue: Integer);
    procedure SetTimeSeriesChart(const AValue: Boolean);
    procedure SetView3D(const AValue: Boolean);
    procedure SetVisibleSeriesAsString(const AValue: string);
    function  GetVisibleSeriesAsString: string;
    function  GetVisibleSeriesCount: integer;
    procedure ShowDataPointHint;
//    procedure SnapEditDialogToButton(pForm: TForm; pSender: TObject);
    procedure Zoom(const AZoomPercent: double);
    procedure DoShowDataPointHintTmr(Sender: TObject);

    function GetShowButtons: Boolean;
    function GetShowLegend: Boolean;
    procedure SetShowButtons(const AValue: Boolean);
    procedure SetShowLegend(const AValue: Boolean);
    function GetZoomPen: TTeeZoomPen;
    procedure RangeChange;
    function GetDataPointHintText: string;
    procedure SetDataPointHintText(const AValue: string);
    function GetVisiblesSeriesMaxY: real;
    function GetVisiblesSeriesMinY: real;
    function GetVisiblesSeriesMaxX: real;
    function GetVisiblesSeriesMinX: real;
    procedure RepositionChart;
    function GetZoomed: Boolean;
    procedure DoDrawChart(const AZoomOut: Boolean);
    function GetUserPanel: TtiChartUserPanel;
    procedure SetIncludeLegendInClipboard(const Value: boolean);
    procedure GetSeriesData(const AChartSeries: TtiChartSeries);
    procedure ConstrainChartAxesView(const AXAxisMin, AXAxisMax, AYAxisMin, AYAxisMax: Double);
  protected
    procedure ClearSeries;
    procedure AddSeries(const ASeries: TChartSeries; const ASeriesSource: TtiObject);
    function AddBarSeries(const ATitle: string; const AVisible: boolean;
        const ASeriesSource: TtiObject): TBarSeries;
    function AddLineSeries(const ATitle: string; const AVisible: boolean;
        const ASeriesSource: TtiObject): TLineSeries;
    procedure ClearSeriesValues;
    procedure Loaded; override;
public
    constructor Create(Owner : TComponent; const AButtonsPosition:
        TtiChartButtonsPosition; const ALegendPosition: TtiChartLegendPosition;
        const ALegendKind: TtiChartLegendKind = clList);
        reintroduce; overload;
    destructor Destroy; override;
    function AddDateTimeBarSeries(const ATitle : string;
        const AVisible: boolean = true; const ASeriesSource: TtiObject = nil): TBarSeries;
    procedure AddDateTimeGap(const ASeries: TChartSeries; const AXBeforeGap:
        TDateTime; const AXAfterGap: TDateTime); overload;
    procedure AddDateTimeGap(const ASeriesName: string; const AXBeforeGap:
        TDateTime; const AXAfterGap: TDateTime); overload;
    procedure AddDateTimeGap(const AChartSeries: TtiChartSeries; const AXBeforeGap:
        TDateTime; const AXAfterGap: TDateTime); overload;
    function AddDateTimeLineSeries(const ATitle: string;
        const AVisible: boolean = true; const ASeriesSource: TtiObject = nil): TLineSeries;
    procedure AddDateTimeValues(const ASeries: TChartSeries;
      const AX: TDateTime; const AY: real; const ALabel: string;
      const AColor: TColor); overload;
    procedure AddDateTimeValues(const ASeriesTitle : string;
        const AX: TDateTime; const AY: real; const ALabel: string = '';
        const AColor: TColor = clDefault); overload;
    procedure AddDateTimeValues(const AChartSeries: TtiChartSeries;
        const AX: TDateTime; const AY: real; const ALabel: string = '';
        const AColor: TColor = clDefault); overload;
    procedure Clear;
    procedure RefreshSeries;
    procedure RedrawChart;
    procedure SetAxesRange(const AXAxisMin, AXAxisMax, AYAxisMin, AYAxisMax: Double);

    function SeriesByName(const ASeriesName : string): TChartSeries;
    function SeriesByTitle(const ASeriesTitle : string): TChartSeries;
    procedure ShowSeries(const ASeries: TChartSeries; const AVisible: Boolean);

    procedure AddButton(const AButtonHint: string; const AImageResName: string;
        const ADoButtonClick: TNotifyEvent; const AButtonOrder: Integer);

    property Chart: TChart read FChart write FChart;
    property ChartPanel: TtiChartPanel read FChartPanel write FChartPanel;
    property ChartWithLegendPanel: TtiChartWithLegendPanel read FChartWithLegendPanel;
    property CrossHairsDrawn: Boolean read FCrossHairsDrawn write
        FCrossHairsDrawn default true;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons;
    property ShowLegend: Boolean read GetShowLegend write SetShowLegend;
    property ZoomPen: TTeeZoomPen read GetZoomPen;
    property Data: TtiObjectList read FData write SetData;
    property DataPointHintForm: TtiDataPointHintForm read FDataPointHintForm write FDataPointHintForm;
    property DataPointHintText: string read GetDataPointHintText write SetDataPointHintText;
    property DataUnderMouse: TObject read FDataUnderMouse;
    property MouseInCrossHairRegion: Boolean read GetMouseInCrossHairRegion;
    property TimeSeriesChart: Boolean read FTimeSeriesChart write
        SetTimeSeriesChart;
    property VisibleSeriesAsString: string read GetVisibleSeriesAsString write
        SetVisibleSeriesAsString;
    property VisibleSeriesCount: integer read GetVisibleSeriesCount;
    property XDataValueUnderMouse: Real read FXDataValueUnderMouse write
        FXDataValueUnderMouse;
    property XValueUnderMouse: Real read FXValueUnderMouse write
        FXValueUnderMouse;
    property YDataValueUnderMouse: Real read FYDataValueUnderMouse write
        FYDataValueUnderMouse;
    property YValueUnderMouse: Real read FYValueUnderMouse write
        FYValueUnderMouse;
    property Zoomed: Boolean read GetZoomed;
    procedure SetSeriesVisibleByCaption(const ASeriesTitle: string; const AVisible: Boolean);
    function IsSeriesVisible(const ASeriesTitle: string): boolean;
    function SeriesNameToTitle(const ASeriesName: string): string;
    function PointsVisible: Integer;
    procedure SelectUserPanel;
    procedure DeselectAllSeries;
    procedure CopyToClipBoard;

    property VisiblesSeriesMinX: real read GetVisiblesSeriesMinX;
    property VisiblesSeriesMaxX: real read GetVisiblesSeriesMaxX;
    property VisiblesSeriesMinY: real read GetVisiblesSeriesMinY;
    property VisiblesSeriesMaxY: real read GetVisiblesSeriesMaxY;
    property UserPanel: TtiChartUserPanel read GetUserPanel;

  published
    property Align;
    property Anchors;
    property AxisBottom: TChartAxis read GetAxisBottom;
    property AxisLeft: TChartAxis read GetAxisLeft;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property ChartColor: TColor read GetChartColor write SetChartColor default
        clWhite;
    property ChartDataMappings: TtiChartDataMappings read FChartDataMappings;
    property ChartPopupMenu: TPopupMenu read GetChartPopupMenu write
        SetChartPopupMenu;
    property Color;
    property ConstrainViewToData: Boolean read FConstrainViewToData write
        SetConstrainViewToData;
    property DrawCrossHairs: Boolean read FDrawCrossHairs write
        SetDrawCrossHairs default true;
    property DrawCrossHairsNow: Boolean read FDrawCrossHairsNow write
        SetDrawCrossHairsNow;
    property ScrollBars: TScrollStyle read FScrollStyle write SetScrollStyle
        default ssBoth;
    property ShowTestData: Boolean read FShowTestData write SetShowTestData
        default false;
    property SnapToDataSize: Integer read FSnapToDataSize write
        SetSnapToDataSize default 15;
    property View3D: Boolean read GetView3D write SetView3D default false;
    property OnAssignGraphData: TAssignGraphDataEvent read FOnAssignGraphData
        write FOnAssignGraphData;
    property OnAssignSeriesData: TAssignSeriesDataEvent read FOnAssignSeriesData
        write FOnAssignSeriesData;
    property OnCrossHair: TCrossHairEvent read FOnCrossHair write FOnCrossHair;
    property OnChartDataGap: TChartDataGapEvent read FOnChartDataGap write FOnChartDataGap;
    property OnSeriesDataGap: TSeriesDataGapEvent read FOnSeriesDataGap write FOnSeriesDataGap;
    property OnDblClickChart: TNotifyEvent read GetOnDblClickChart write
        SetOnDblClickChart;
    property OnRangeChange: TRangeChangeEvent read FOnRangeChange
        write FOnRangeChange;
    property OnGetDataByDateTime: TGetDataByDateTimeEvent
        read FOnGetDataByDateTime write FOnGetDataByDateTime;
    property OnVisibleSeriesChange: TVisibleSeriesChangeEvent
        read FOnVisibleSeriesChange write FOnVisibleSeriesChange;
     property IncludeLegendInClipboard: boolean read FIncludeLegendInClipboard write SetIncludeLegendInClipboard;
  end;

implementation
uses
  SysUtils
  ,Math
  ,tiImageMgr
  ,tiUtils
  ,tiGUIUtils
  ,tiConstants
  ,DateUtils
  ,tiCtrlButtonPanel
;

type

  TtiAxis = ( axHorizontal, axVertical );

const
  // The TChart components assigns colours to the series in a strange order
  // This is the sequence of colour assignments.
  cuaSeriesColors : array[0..12] of TColor =
    (clNavy, clRed, clGreen, clMaroon,
      clOlive, clPurple, clTeal, clLime,
      clBlue, clFuchsia, clAqua, clYellow,
      clBlack);
  ciBorder   = 4;
  ciSCBThickness = 16; // Must be 16, will be changed by delphi at runtime if not :(
  cuiAxisLabelSize = 40;
  cuiAxisTitleSize = 20;

  cPointerVisibleLimit = 100;
  cScrollResolution = 400;

  CAxisEpsilon = 0.0000001;

const
  CSBLeft   =  4;
  CSBTop    = 10;
  CSBSize   = 20;
  CSBVerticalSpace = 8;

// Register with the component pallet
//procedure Register;
//begin
//  RegisterComponents('TechInsite',
//                      [  TtiChart
//                      ]);
//end;

{ TtiClearPanel }

//  liSBLeft : integer;

{
******************************** TtiClearPanel *********************************
}
constructor TtiClearPanel.Create(Owner : TComponent);
begin
  inherited Create(Owner);
  ParentBackground := false;
  Color := clWhite;
  BevelOuter := bvNone;
end;

destructor TtiClearPanel.Destroy;
begin
  inherited;
end;

{ TtiChartDataMapping }

{
***************************** TtiChartDataMapping ******************************
}
constructor TtiChartDataMapping.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  FDisplayLabel := 'Caption';
  FPropertyName := 'Caption';
end;

destructor TtiChartDataMapping.Destroy;
begin
  inherited;
end;

function TtiChartDataMapping.Clone: TtiChartDataMapping;
begin
  result := TtiChartDataMapping.Create(nil);
  result.DisplayLabel := DisplayLabel;
  result.PropertyName := PropertyName;
end;

function TtiChartDataMapping.GetDisplayName: string;
begin
  result := DisplayLabel;
end;

procedure TtiChartDataMapping.SetPropertyName(const AValue: string);
begin
  FPropertyName := AValue;
end;

{ TtiChartDataMappings }

{
***************************** TtiChartDataMappings *****************************
}
constructor TtiChartDataMappings.Create(Owner : TComponent);
begin
  inherited Create(TtiChartDataMapping);
  FOwner := Owner;
end;

destructor TtiChartDataMappings.Destroy;
begin
  inherited;
end;

function TtiChartDataMappings.Add: TtiChartDataMapping;
begin
  result := TtiChartDataMapping(inherited add);
end;

procedure TtiChartDataMappings.Clear;
begin
  inherited;
end;

function TtiChartDataMappings.FindByFieldName(const AFieldName : string):
    TtiChartDataMapping;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].PropertyName = AFieldName then begin
      result := Items[i];
      break; //==>
    end;
end;

function TtiChartDataMappings.GetItem(Index: integer): TtiChartDataMapping;
begin
  result := TtiChartDataMapping(inherited GetItem(Index));
end;

function TtiChartDataMappings.GetOwner: TPersistent;
begin
  result := FOwner;
end;

procedure TtiChartDataMappings.NamesToStringList(const AStringList : TStringList);
var
  i: Integer;
begin
  AStringList.Clear;
  for i := 0 to count - 1 do
    AStringList.Add(Items[i].PropertyName);
end;

procedure TtiChartDataMappings.SetItem(Index: integer; const AValue:
    TtiChartDataMapping);
begin
  inherited SetItem(Index, AValue);
end;

{ TtiChartInternal }

{
******************************* TtiChartInternal *******************************
}
procedure TtiChartInternal.Paint;
var
  LDrawCrossHairsSaved: Boolean;
begin
  LDrawCrossHairsSaved := TtiTimeSeriesChart(Owner).DrawCrossHairs;
  TtiTimeSeriesChart(Owner).DrawCrossHairs := false;
  try
    inherited;
  finally
    TtiTimeSeriesChart(Owner).DrawCrossHairs := LDrawCrossHairsSaved;
  end;
end;

{ TtiChartTestData }

{
******************************* TtiChartTestData *******************************
}
constructor TtiChartTestData.create;
var
  i: Integer;
  LData: TtiChartTestDataItem;
begin
  inherited;

  for i := 0 to 359 do begin
    LData := TtiChartTestDataItem.Create;
    LData.XValue := Date + i;
    LData.YValue1 := Sin(i/180*Pi);
    LData.YValue2 := Cos(i/180*Pi);
    Add(LData);
  end;
end;

procedure TtiChartTestData.AssignGraphData(const AData: TObject; const AChart:
    TtiTimeSeriesChart);
begin
  AChart.SeriesByName('Sin').AddXY(
    TtiChartTestDataItem(AData).XValue,
    TtiChartTestDataItem(AData).YValue1);
  AChart.SeriesByName('Cos').AddXY(
    TtiChartTestDataItem(AData).XValue,
    TtiChartTestDataItem(AData).YValue2);
end;

procedure TtiChartTestData.DataGap(const ADataBeforeGap: TObject; const ADataAfterGap:
    TObject; const AChart: TtiTimeSeriesChart);
begin
  if (TtiChartTestDataItem(ADataBeforeGap).YValue2 <= 0) and
     (TtiChartTestDataItem(ADataAfterGap).YValue2 >= 0) then
    AChart.AddDateTimeGap('Cos', TtiChartTestDataItem(ADataBeforeGap).XValue, TtiChartTestDataItem(ADataAfterGap).XValue);
end;

// Create a test gap as the Cos data goes above zero.
{ TtiChartPanel }

{
******************************** TtiChartPanel *********************************
}
constructor TtiChartPanel.Create(Owner : TComponent; AParenttiChart:
    TtiTimeSeriesChart);
begin
  inherited Create(Owner);
  FParenttiChart := AParenttiChart;
  Align := alClient;
  FScrollStyle := ssNone;

  FChartPanel := TtiClearPanel.Create(self);
  with FChartPanel do
  begin
    Parent := self;
    Name := tiGetUniqueComponentNameFromParent(self, 'ChartPanel');
    Caption := '';
    Anchors    := [akLeft,akTop,akRight,akBottom];
    Top := Parent.Top;
    Left := Parent.Left;
    Height := Parent.Height - ciSCBThickness;
    Width := Parent.Width - ciSCBThickness;
    ParentBackground := false;
    BevelOuter := bvNone;
  end;

  FChart := TChart.Create(self);
  with FChart do
  begin
    Parent  := FChartPanel;
    Name := tiGetUniqueComponentNameFromParent(self, 'Chart');
    Anchors    := [akLeft,akTop,akRight,akBottom];
    Top := Parent.Top;
    Left := Parent.Left;
    Height := Parent.Height;
    Width := Parent.Width;

    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderStyle := bsNone;
    Color      := clWhite;
    Legend.Visible := false;
    Title.Visible := false;
    View3D := false;

    // Set the BottomAxis properties
    BottomAxis.Title.Font.Color := clNavy;
    BottomAxis.Grid.SmallDots := true;
    BottomAxis.Grid.Color := clSilver;
    BottomAxis.LabelsSize := cuiAxisLabelSize;
    BottomAxis.TitleSize := cuiAxisTitleSize;

    // Set the LeftAxis properties
    LeftAxis.Title.Font.Color := clNavy;
    LeftAxis.Grid.SmallDots := true;
    LeftAxis.Grid.Color := clSilver;
    LeftAxis.LabelsSize := cuiAxisLabelSize;
    LeftAxis.TitleSize := cuiAxisTitleSize;

    ClipPoints := true;

    OnMouseMove           := FParenttiChart.DoChartMouseMove;
    OnMouseDown           := FParenttiChart.DoMouseDown;
    OnMouseUp             := FParenttiChart.DoMouseUp;
    OnMouseLeave          := FParenttiChart.DoMouseLeave;

    OnBeforeDrawAxes      := FParenttiChart.DoOnBeforeDrawAxes;
    OnZoom                := FParenttiChart.DoOnZoom;
    OnUndoZoom            := FParenttiChart.DoOnUndoZoom;
    OnScroll              := FParenttiChart.DoOnScroll;
    OnAllowScroll         := FParenttiChart.DoOnAllowScroll;
    OnAfterDraw           := FParenttiChart.DoOnAfterDraw;
  end;

  FscbHorizontal := TScrollBar.Create(self);
  with FscbHorizontal do
  begin
    Parent  := self;
    Name    := tiGetUniqueComponentNameFromParent(self, 'scbHorizontal');
    TabStop := false;
    Kind    := sbHorizontal;
    Top     := FChartPanel.Height;
    Left    := FChartPanel.Left;
    Height  := ciSCBThickness;
    Width   := FChartPanel.Width;
    Anchors := [akLeft,akRight,akBottom];
    Min     := 0;
    Max     := cScrollResolution;
    Position := 0;
    PageSize := Max - Min;
    OnChange := FParenttiChart.DoHorizontalScroll;
    Visible := false;
  end;

  FscbVertical := TScrollBar.Create(self);
  with FscbVertical do
  begin
    Parent  := self;
    Name    := tiGetUniqueComponentNameFromParent(self, 'scbVertical');
    TabStop := false;
    Kind    := sbVertical;
    Top     := FChartPanel.Top;
    Left    := FChartPanel.Width;
    Height  := FChartPanel.Height;
    Width   := ciSCBThickness;
    Anchors := [akRight,akTop,akBottom];
    Min     := 0;
    Max     := cScrollResolution;
    Position := 0;
    PageSize := Max - Min;
    OnChange := FParenttiChart.DoVerticalScroll;
    Visible := false;
  end;

  FParenttiChart.Chart := FChart;
  FParenttiChart.ChartPanel := self;
end;


destructor TtiChartPanel.Destroy;
begin
  FChart.Free;
  FChartPanel.Free;
  FscbHorizontal.Free;
  FscbVertical.Free;
  inherited;
end;

procedure TtiChartPanel.SetScrollStyle(const AValue: TScrollStyle);
begin
  FScrollStyle := AValue;
  case FScrollStyle of
  ssNone      : begin
                   FscbHorizontal.Visible := false;
                   FscbVertical.Visible   := false;
                   FChartPanel.Height     := self.Height;
                   FChartPanel.Width      := self.Width;
                 end;
  ssHorizontal : begin
                   FscbHorizontal.Visible := true;
                   FscbVertical.Visible   := false;
                   FChartPanel.Height     := self.Height - ciBorder - ciSCBThickness;
                   FChartPanel.Width      := self.Width;
                 end;
  ssVertical  : begin
                   FscbHorizontal.Visible := false;
                   FscbVertical.Visible   := true;
                   FChartPanel.Height     := self.Height;
                   FChartPanel.Width      := self.Width - ciBorder - ciSCBThickness;
                 end;
  ssBoth      : begin
                   FscbHorizontal.Visible := true;
                   FscbVertical.Visible   := true;
                   FChartPanel.Height     := self.Height - ciBorder - ciSCBThickness;
                   FChartPanel.Width      := self.Width - ciBorder - ciSCBThickness;
                 end;
  end;
end;

{ TtiChartUserPanel }

procedure TtiChartUserPanel.SetCaption(const ACaption: string);
begin
  if FCaption <> ACaption then
  begin
    FCaption := ACaption;
    if Assigned(FOnSetCaption) then
      FOnSetCaption(FCaption);
  end;
end;

procedure TtiChartUserPanel.SetForm(const AForm: TForm);
begin
  if FForm <> AForm then
  begin
    FForm := AForm;
    if Assigned(FOnSetForm) then
      FOnSetForm(FForm);
  end;
end;

{ TtiChartLegendPanel }

{
***************************** TtiChartLegendPanelForm ******************************
}
constructor TtiChartLegendPanel.CreateNew(Owner : TComponent; AParenttiChart:
    TtiTimeSeriesChart; const ALegendKind: TtiChartLegendKind;
    ALegendPosition: TtiChartLegendPosition; Dummy : integer = 0);
begin
  inherited Create(Owner);
  Parent := Owner as TWinControl;
  Align := alClient;
  FParenttiChart :=  AParenttiChart;
  BorderStyle := bsNone;
  //Anchors := [akLeft, akRight, akTop, akBottom];
  Height := Parent.Height;
  case ALegendPosition of
    clpLeft:  Align := alLeft;
    clpRight: Align := alRight;
  end;

  FUserPanel := TtiChartUserPanel.Create;
  FUserPanel.OnSetCaption := SetUserPanelCaption;
  FUserPanel.OnSetForm := SetUserPanelForm;

//  FDockTabSet := TDockTabSet.Create(Self);
//  with FDockTabSet do
//  begin
//    Name := 'FDockTabSet';
//    Parent := Self;
//    Name := tiGetUniqueComponentNameFromParent(Self, 'DockTabSet');
//    Left := 0;
//    Top := 0;
//    Width := 25;
//    Height := 292;
//    Align := alLeft;
//    Font.Color := clWindowText;
//    Font.Height := -11;
//    Font.Name := 'Tahoma';
//    Font.Style := [];
//    ShrinkToFit := true;
//    Style := tsModernPopout;
//    TabPosition := tpLeft;
//    DockSite := true;
//    DestinationDockSite := FDockTabSet;
//  end;

//  FButonPanel := TtiClearPanel.Create(self);
//  with FButonPanel do
//  begin
//    Parent := self;
//    Name := tiGetUniqueComponentNameFromParent(self, 'ButtonPanel');
//    Caption := '';
//    Width := 20;
//    Align := alLeft;
//  end;
//  FlegendButton := TtiSpeedButton.Create(self);
//  with FLegendButton do
//  begin
//    Parent := FButonPanel;
//    Name := tiGetUniqueComponentNameFromParent(FButtonPanel, 'LegendButton');
//    Height := 100;
//    Width := 20;
//    Flat   := true;
//    Color  := Self.Color;
//    ImageRes := tiRIGraphLine;
//    Caption := 'Legend';
////    Hint  := 'View legend';
////    ShowHint := true;
//  //  OnClick := FParenttiChart.DoSBViewLegendClick;
//  end;

//  FLegendFormPanel := TtiClearPanel.Create(self);
//  FLegendFormPanel.Parent := self;
//  FLegendFormPanel.Name := tiGetUniqueComponentNameFromParent(self, 'LegendFormPanel');
//  FLegendFormPanel.Caption := '';
//  FLegendFormPanel.Align := alClient;

  // Create a page control to hold the legend form and optional user form on
  // separate tab sheets. The tabs are hidden if the user form is not used.
  FPageControl := TPageControl.Create(nil);
  FPageControl.Align := alClient;
  FPageControl.Style := tsFlatButtons;
  FPageControl.Parent := Self;
  FPageControl.Name := tiGetUniqueComponentNameFromParent(Self, 'PageControl');
  FLegendTabSheet := TTabSheet.Create(FPageControl);
  FLegendTabSheet.Name := tiGetUniqueComponentNameFromParent(self, 'LegendTabSheet');
  FLegendTabSheet.Caption := 'Legend';
  FLegendTabSheet.PageIndex := 0;
  FLegendTabSheet.TabVisible := False;
  FLegendTabSheet.BorderWidth := 0;
  FLegendTabSheet.PageControl := FPageControl;
  FUserPanelTabSheet := TTabSheet.Create(FPageControl);
  FUserPanelTabSheet.Name := tiGetUniqueComponentNameFromParent(self, 'UserPanelTabSheet');
  FUserPanelTabSheet.Caption := 'User';
  FUserPanelTabSheet.PageIndex := 1;
  FUserPanelTabSheet.TabVisible := False;
  FUserPanelTabSheet.BorderWidth := 0;
  FUserPanelTabSheet.PageControl := FPageControl;
  FPageControl.ActivePageIndex := 0;
  case ALegendKind of
    clList:
      FChartLegendForm := TtiChartLegendForm.CreateNew(FParenttiChart);
    clTree:
      FChartLegendForm := TtiChartLegendTreeViewForm.CreateNew(FParenttiChart);
  end;
  FChartLegendForm.Name := tiGetUniqueComponentName('FChartLegendForm');
  FChartLegendForm.Caption := 'Legend';
  FChartLegendForm.Parent := FLegendTabSheet;
  FChartLegendForm.Align:= alClient;
//  FChartLegendForm.ManualDock(FDockTabSet);

  Width := FPageControl.Width;
end;

destructor TtiChartLegendPanel.Destroy;
begin
  FUserPanel.Free;
  FChartLegendForm.Free;
  FPageControl.Free;
  //FDockTabSet.Free;
  inherited;
end;

procedure TtiChartLegendPanel.SelectUserPanel;
begin
  if Assigned(FUserPanelForm) then
    FPageControl.ActivePage := FUserPanelTabSheet;
end;

procedure TtiChartLegendPanel.SetUserPanelCaption(const ACaption: string);
begin
  FUserPanelTabSheet.Caption := ACaption;
end;

procedure TtiChartLegendPanel.SetUserPanelForm(const AForm: TForm);
begin
  if FUserPanelForm <> AForm then
  begin
    if Assigned(FUserPanelForm) then
      FUserPanelForm.Parent := nil;
    FUserPanelForm := AForm;

    if Assigned(AForm) then
    begin
      if AForm.Width > Width then
        Width := AForm.Width + 8;
      AForm.Parent := FUserPanelTabSheet;
      FLegendTabSheet.TabVisible := True;
      FUserPanelTabSheet.TabVisible := True;
      FPageControl.ActivePage := FLegendTabSheet;
      AForm.Visible := True;
    end else begin
      FPageControl.ActivePage := FLegendTabSheet;
      FLegendTabSheet.TabVisible := False;
      FUserPanelTabSheet.TabVisible := False;
    end;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartLegendForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//function TtiChartLegendForm.CloseQuery: Boolean;
//begin
//  result := false;
//
//end;

constructor TtiChartLegendForm.CreateNew(const AChart: TtiTimeSeriesChart);
begin
  inherited CreateNew(AChart);
  FList:= TObjectList.Create(False);
end;

destructor TtiChartLegendForm.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TtiChartLegendForm.DoSelectAll(Sender: TObject);
var
  i: Integer;
  LChartItem: TtiChartLegendItem;
begin
  for i := 0 to FList.Count-1 do
  begin
    LChartItem := FList.Items[i] as TtiChartLegendItem;
    LChartItem.Checked := true;
  end;
end;

procedure TtiChartLegendForm.DoSelectNone(Sender: TObject);
var
  i: Integer;
  LChartItem: TtiChartLegendItem;
begin
  for i := 0 to FList.Count-1 do
  begin
    LChartItem := FList.Items[i] as TtiChartLegendItem;
    LChartItem.Checked := false;
  end;
end;

function TtiChartLegendForm.FindBySeriesName(
  const ASeriesName: string): TtiChartLegendItem;
var
  i: integer;
  LSeriesName: string;
begin
  for i := 0 to FList.Count-1 do
  begin
    LSeriesName := (FList.Items[i] as TtiChartLegendItem).FChartSeries.Name;
    if SameText(LSeriesName, ASeriesName) then
    begin
      result:= FList.Items[i] as TtiChartLegendItem;
      Exit; //==>
    end;
  end;
  result:= nil;
end;

function TtiChartLegendForm.FindBySeriesTitle(
  const ASeriesTitle: string): TtiChartLegendItem;
var
  i: integer;
  LSeriesDisplayName: string;
begin
  for i := 0 to FList.Count-1 do
  begin
    LSeriesDisplayName := (FList.Items[i] as TtiChartLegendItem).FChartSeries.Title;
    if SameText(LSeriesDisplayName, ASeriesTitle) then
    begin
      result:= FList.Items[i] as TtiChartLegendItem;
      Exit; //==>
    end;
  end;
  result:= nil;
end;

function TtiChartLegendForm.GetSeriesVisible(
  const ASeriesName: string): boolean;
var
  LChartLegendItem: TtiChartLegendItem;
begin
  LChartLegendItem:= FindBySeriesName(ASeriesName);
  Assert(Assigned(LChartLegendItem), 'SeriesName "' + ASeriesName + '" not found');
  Result:= LChartLegendItem.Checked;
end;

function TtiChartLegendForm.IsSeriesVisibleByCaption(
  const ASeriesTitle: string): boolean;
var
  LChartLegendItem: TtiChartLegendItem;
begin
  LChartLegendItem:= FindBySeriesTitle(ASeriesTitle);
  Result:= Assigned(LChartLegendItem) and LChartLegendItem.Checked;
end;

procedure TtiChartLegendForm.ClearSeries;
begin
  FList.Clear;
end;

procedure TtiChartLegendForm.CreateSeries;
var
  i : integer;
  LWidth : integer;
  LTop : integer;
  LSeriesEdit : TtiChartLegendItem;
begin
  inherited;
  Assert(FList.Count = 0, 'Attempt to call CreateSeries more than once');
  LWidth := 0;
  LTop := 20;
  for i := 0 to FChart.Chart.SeriesCount - 1 do
  begin
    LSeriesEdit := TtiChartLegendItem.Create(Self, FChart, FChart.Chart.Series[i] as TLineSeries);
    FList.Add(LSeriesEdit);
    if LSeriesEdit.Width > LWidth then
      LWidth := LSeriesEdit.Width;
    LSeriesEdit.Top:= LTop+1;
    LSeriesEdit.Left:= 1;
    Inc(LTop, LSeriesEdit.Height);
  end;

  if LTop > Parent.Height then
  begin
    LWidth := LWidth + 24; //scrollbar width
  end;

  if FChart.Chart.SeriesCount > 0 then
  begin
    Parent.Width:= LWidth + 4;
    Width:= LWidth;
  end;

end;

procedure TtiChartLegendForm.SetSeriesVisible(const ASeriesName: string;
  const AVisible: boolean);
var
  LChartLegendItem: TtiChartLegendItem;
begin
  LChartLegendItem:= FindBySeriesName(ASeriesName);
  Assert(Assigned(LChartLegendItem), 'SeriesName "' + ASeriesName + '" not found');
  LChartLegendItem.Checked:= AVisible;
end;

procedure TtiChartLegendForm.SetSeriesVisibleByCaption(
  const ASeriesTitle: string; const AVisible: Boolean);
var
  LChartLegendItem: TtiChartLegendItem;
begin
  LChartLegendItem:= FindBySeriesTitle(ASeriesTitle);
  Assert(Assigned(LChartLegendItem), 'SeriesTitle "' + ASeriesTitle + '" not found');
  LChartLegendItem.Checked:= AVisible;
end;

//procedure TtiChartLegendForm.BeginDrag;
//begin
////do nothing
//
//end;

{ TtiChartLegendItem }

{
****************************** TtiChartLegendItem ******************************
}
constructor TtiChartLegendItem.Create(const AOwner: TtiChartLegendForm;
      const AChart: TtiTimeSeriesChart; const ASeries: TtiChartSeries2);
begin
  inherited Create(AOwner);
  Parent:= AOwner;
  Name := tiGetUniqueComponentNameFromParent(Parent, 'ChartLegendItem');
  Caption := '';
  FtiChart:= AChart;
  FChartSeries:= ASeries;
  ControlStyle  := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
  Height := cChartLegendItemHeight;
  Color := clWhite;
  OnClick := DoOnCheckBoxClick;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Name := tiGetUniqueComponentNameFromParent(Self, 'Label');
  FLabel.Top := 4;
  FLabel.Left := cChartLegendItemCheckBoxLeft + 28;
  FLabel.Font.Color := ASeries.SeriesColor;
  FLabel.WordWrap := true;
  FLabel.AutoSize := false;
  FLabel.Caption := ASeries.Title;
  FLabel.Width := 80;
  FLabel.AutoSize := true;

  FCheckBox := TCheckBox.Create(Self);
  FCheckBox.Parent := Self;
  FCheckBox.Name := tiGetUniqueComponentNameFromParent(Self, 'CheckBox');
  FCheckbox.Top := 4;
  FCheckbox.Width := 16;
  FCheckBox.Height := FLabel.Height;
  FCheckBox.Left := cChartLegendItemCheckBoxLeft;
  // Set checked before assigning event handler to avoid Click event.
  FCheckBox.Checked := ASeries.Active;
  FCheckBox.OnClick := DoOnCheckBoxClick;

  Width:= FLabel.Left + FLabel.Width;
  Height := 4 + FLabel.Height + 4;
end;

destructor TtiChartLegendItem.Destroy;
begin
  FCheckBox.Free;
  FLabel.Free;
  inherited;
end;

procedure TtiChartLegendItem.DoOnCheckBoxClick(Sender: TObject);
begin
  Assert(FChartSeries<>nil, 'FChartSeries not assigned');
  Assert(FCheckBox<>nil, 'FCheckBox not assigned');
  Assert(FtiChart<>nil, 'FtiChart not assigned');
  if Sender is TtiChartLegendItem then
    FCheckBox.Checked := not FCheckBox.Checked
  else if Sender is TCheckBox then
    // Only process when the checkbox is clicked to avoid double-processing.
    // Setting the checkbox state above fires OnClick again.
    FtiChart.ShowSeries(TChartSeries(FChartSeries), FCheckBox.Checked);
end;

function TtiChartLegendItem.GetChartSeries(const AChart: TtiTimeSeriesChart): TtiChartSeries2;
begin
  Result := FChartSeries;
end;

function TtiChartLegendItem.GetChecked: Boolean;
begin
  result:= FCheckBox.Checked;
end;

procedure TtiChartLegendItem.Paint;
var
  LColor: TColor;
  LRow: Integer;
  LCol: Integer;
  LPenWidth: Integer;
begin
  inherited;
  if FChartSeries = nil then
    Exit; //==>
  // Setup the pen
  LColor := Canvas.Pen.Color;
  LPenWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FChartSeries.SeriesColor;
  // Draw horozontal line
  LRow := Height div 2;
  Canvas.PenPos := Point(4, LRow);
  Canvas.LineTo(cChartLegendItemCheckBoxLeft - 4, LRow);
  // Draw vertical line
  LCol := (cChartLegendItemCheckBoxLeft - 8) div 2;
  Canvas.PenPos := Point(LCol, Height div 2 - 8);
  Canvas.LineTo(LCol, Height - Height div 2 + 8);
  // Restore the pen properties
  Canvas.Pen.Color := LColor;
  Canvas.Pen.Width := LPenWidth;
end;

procedure TtiChartLegendItem.SetChecked(const AValue: Boolean);
begin
  FCheckBox.Checked:= AValue;
end;

{ TtiChartWithLegendPanel }

{
*************************** TtiChartWithLegendPanel ****************************
}
constructor TtiChartWithLegendPanel.Create(Owner : TComponent; ALegendPosition:
    TtiChartLegendPosition; const ALegendKind: TtiChartLegendKind);
begin
  inherited Create(Owner);
  Parent := Owner as TWinControl;
  Color := clWhite;
  FParenttiChart := TtiTimeSeriesChart(Owner);
  Align := alClient;
  FLegendPosition := ALegendPosition;

  FtiChartPanel := TtiChartPanel.Create(self, FParenttiChart);
  with FtiChartPanel do
  begin
    Parent := self;
  end;

  FChartLegendPanel := TtiChartLegendPanel.CreateNew(self, FParenttiChart,
    ALegendKind, FLegendPosition);

  FtiChartPanel.Align := alClient;
end;

destructor TtiChartWithLegendPanel.Destroy;
begin
  FChartLegendPanel.Free;
  FtiChartPanel.Free;
  inherited;
end;

function TtiChartWithLegendPanel.GetShowLegend: Boolean;
begin
  Result := FChartLegendPanel.Visible;
end;

procedure TtiChartWithLegendPanel.SetShowLegend(const AValue: Boolean);
begin
  FChartLegendPanel.Visible := AValue;
end;

{ TtiChartButtonsPanel }

{
***************************** TtiChartButtonsPanel *****************************
}
procedure TtiChartButtonsPanel.AddButton(const AButtonHint: string;
  const AImageResName: string; const ADoButtonClick: TNotifyEvent;
  const AButtonOrder: Integer);
begin
  FChartButtonDetailList.AddButton(AButtonHint, AImageResName, ADoButtonClick, AButtonOrder);
  FChartButtonDetailList.CreateButtons(self, FChartButtonsPosition);
end;

constructor TtiChartButtonsPanel.Create(Owner : TComponent; AButtonsPosition:
    TtiChartButtonsPosition);
begin
  inherited Create(Owner);
  FParenttiChart := TtiTimeSeriesChart(Owner);
  FChartButtonsPosition := AButtonsPosition;

  FChartButtonDetailList := TtiChartButtonDetailList.Create;
  FChartButtonDetailList.AddButton('Zoom in',   cResTI_ZoomIn,   FParenttiChart.DoSBZoomInClick);
  FChartButtonDetailList.AddButton('Zoom out',  cResTI_ZoomOut,  FParenttiChart.DoSBZoomOutClick);
  FChartButtonDetailList.AddButton('Detail zoom', cResTI_ZoomDetail, FParenttiChart.DoSBDetailZoomClick);
  FChartButtonDetailList.AddButton('Manual zoom', cResTI_ZoomManual, FParenttiChart.DoSBManualZoomClick);
  FChartButtonDetailList.AddButton('Undo zoom', cResTI_Maximize, FParenttiChart.DoSBDefaultZoomClick);
//  FChartButtonDetailList.AddButton('View legend', tiRIGraphLine, FParenttiChart.DoSBViewLegendClick);
  FChartButtonDetailList.AddButton('Copy to clipboard', cResTI_CopyToClipboard, FParenttiChart.DoSBCopyToClipBrdClick);
//  FChartButtonDetailList.AddButton('Configure', tiChart_Configure, DoSBConfigClick);

  FChartButtonDetailList.CreateButtons(self, FChartButtonsPosition);
end;

destructor TtiChartButtonsPanel.Destroy;
begin
  FChartButtonDetailList.Free;
  inherited;
end;

//procedure TtiChartButtonsPanel.SnapEditDialogToButton(pForm: TForm; pSender:
//    TObject);
//var
//  lSB: TControl;
//  lPoint: TPoint;
//begin
//  Assert(pSender is TControl, 'Sender not a TButton');
//  lSB := TControl(pSender);
//  lPoint.x := Chart.Left + Chart.LeftAxis.PosAxis - Chart.LeftAxis.MaxLabelsWidth - pForm.Width;
//  lPoint.y := lSB.Top;
//  lPoint := lSB.Parent.ClientToScreen(lPoint);
//  pForm.Top := lPoint.Y;
//  pForm.Left := lPoint.X;
//end;

const
  cuiDefColWidth = 30 ;
  cuiDefRowHeight = 14 ;
  cuiHMargin = 10 ;
  cuiVMargin = 5 ;

constructor TtiDisplayGrid.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  ControlStyle := ControlStyle - [csSetCaption] ;
  BevelInner  := bvNone ;
  BevelOuter  := bvNone ;
  BorderStyle := bsNone ;
//  Height      := 137 ;
//  Width       := 117 ;
//  ParentBackground := true;
  ParentColor := true;
  // ToDo: A TtiRoundedPanel here is probably not a good idea if we ever want to
  //       get transparent hint working
  FPnl:= TtiRoundedPanel.Create(Self);
  FPnl.Parent:= Self;
  FPnl.Name := tiGetUniqueComponentNameFromParent(Self, 'Pnl');
  FPnl.Caption := '';
  FPnl.Align:= alClient;
  FPnl.Color:= clInfoBk;
  FPnl.BorderColor:= clBlack;
  FPnl.BorderThickness:= 1;
  FPnl.CornerRadius:= 0;

  FGrid := TStringGrid.Create( self ) ;
  with FGrid do begin
    Parent := FPnl ;
    Name := tiGetUniqueComponentNameFromParent(Self, 'Grid');
    Align  := alClient ;
    BorderStyle := bsNone ;
    ColCount := 2 ;
    DefaultColWidth  := cuiDefColWidth ;
    DefaultRowHeight := cuiDefRowHeight ;
    Enabled := False ;
    FixedCols := 0 ;
    FixedRows := 0 ;
    Options := [] ;
    ScrollBars := ssNone ;
    ParentColor := true;
  end ;

  ColCount := 2 ;
  RowCount := 2 ;
  HideSelected ;
end;

destructor TtiDisplayGrid.Destroy;
begin
  FGrid.Free;
  FPnl.Free;
  inherited;
end;

function TtiDisplayGrid.GetCells(const ACol, ARow: integer): string;
begin
  result := FGrid.Cells[ACol,ARow] ;
end;

function TtiDisplayGrid.GetColCount: integer;
begin
  result := FGrid.ColCount ;
end;

function TtiDisplayGrid.GetRowCount: integer;
begin
  result := FGrid.RowCount ;
end;

procedure TtiDisplayGrid.SetCells( const ACol, ARow: integer;
                                   const AValue: string);
begin
  CheckCellCount( ACol, ARow ) ;
  FGrid.Cells[ACol,ARow] := AValue ;
  SetColWidth( ACol, AValue ) ;
  SetRowHeight( ARow, AValue ) ;
end;

procedure TtiDisplayGrid.CheckCellCount(const ACol, ARow: integer);
begin
  if FGrid.ColCount < ACol + 1 then begin
    FGrid.ColCount := ACol + 1 ;
    HideSelected ;
  end ;

  if FGrid.RowCount < ARow + 1 then begin
    FGrid.RowCount := ARow + 1 ;
    HideSelected ;
  end ;

end ;

procedure TtiDisplayGrid.SetColCount(const AValue: integer);
begin
  FGrid.ColCount := AValue ;
end;

procedure TtiDisplayGrid.SetRowCount(const AValue: integer);
begin
  FGrid.RowCount := AValue ;
end;

procedure TtiDisplayGrid.HideSelected;
var
  LEmptyRect: TGridRect;
begin
  FGrid.Selection := LEmptyRect ;
end;

function TtiDisplayGrid.IsEmpty: boolean;
var
  LCol, LRow: integer;
begin
  result := true;
  for LCol := 0 to FGrid.ColCount - 1 do
    for LRow := 0 to FGrid.RowCount - 1 do
      if self.Cells[LCol, LRow] <> '' then
        result := false;
end;

procedure TtiDisplayGrid.SetColWidth(const ACol: integer; const AText: string);
//var
//  i: Integer;
//  LGridWidth: Cardinal;
begin
  if Canvas.TextWidth( AText )+cuiHMargin > FGrid.ColWidths[ACol] then
    FGrid.ColWidths[ACol] := Canvas.TextWidth( AText )+cuiHMargin ;

//  LGridWidth := 0;
//  for i := 0 to FGrid.ColCount-1 do
//  begin
//    LGridWidth := LGridWidth + FGrid.ColWidths[i];
//  end;
//  FGrid.Width := LGridWidth;
//  Parent.Width := FGrid.Width;
end ;

procedure TtiDisplayGrid.SetContent(const AContent: string);
var
  LStrings: TStrings;
  i: integer;

begin
  FGrid.ColCount := 1;
//  FGrid.Cols[0].Text := AContent;


  LStrings := TStringList.Create;
  try
    LStrings.Text := AContent;
    FGrid.RowCount := LStrings.Count;

    // Remove any trailing CRLF in AContent
//    if (LStrings.Count > 0) and (Length(LStrings[LStrings.Count - 1]) = 0) then
//      LStrings.Delete(LStrings.Count - 1);

//    FGrid.ColCount := 1;
    FGrid.Cols[0].Assign(LStrings);
//    FGrid.RowCount := LStrings.Count;
//    FGrid.Cols[0].AddStrings(LStrings);
  finally
    LStrings.Free;
  end;

  FGrid.ColWidths[0] := 0;
  
  for i := 0 to FGrid.Cols[0].Count- 1 do
  begin
    SetColWidth(0, FGrid.Cols[0][i]);
    SetRowHeight(i, FGrid.Cols[0][i]);
  end;

  Parent.ClientWidth := FGrid.Width;
  Parent.ClientHeight := FGrid.Height;
  HideSelected;
end;

procedure TtiDisplayGrid.SetRowHeight(const ARow: integer; const AText: string);
//var
//  i: Integer;
//  LGridHeight: Cardinal;
begin
  if Canvas.TextHeight( AText )+cuiVMargin > FGrid.RowHeights[ARow] then
    FGrid.RowHeights[ARow] := Canvas.TextHeight( AText )+cuiVMargin ;

//  LGridHeight := 0;
//  for i := 0 to FGrid.RowCount-1 do
//  begin
//    LGridHeight := LGridHeight + FGrid.RowHeights[i];
//  end;
//  FGrid.Height := LGridHeight;
//  Parent.Height := FGrid.Height;
end;

procedure TtiDisplayGrid.Clear;
var
  i : integer ;
begin
  for i := 0 to FGrid.ColCount - 1 do
    ClearRow( i ) ;
end;

procedure TtiDisplayGrid.ClearCell(const ACol, ARow: integer);
begin
  FGrid.Cells[ACol, ARow] := '' ;
end;

procedure TtiDisplayGrid.ClearCol(const ACol: integer);
var
  i : integer ;
begin
  for i := 0 to FGrid.RowCount - 1 do
    ClearCell( ACol, i ) ;
end;

procedure TtiDisplayGrid.ClearRow(const ARow: integer);
var
  i : integer ;
begin
  for i := 0 to FGrid.ColCount - 1 do
    ClearCell( i, ARow ) ;
end;

constructor TtiDataPointHintForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner);
  FFirstShow:= True;
  Color := clInfoText;
  BorderWidth := 1;
  BorderStyle := bsNone;
//  Font.Color := clInfoText;
//  AlphaBlend := true;
//  AlphaBlendValue := 127;
//  TransparentColorValue := clSkyBlue;
//  TransparentColor := true;
//  FinalAlphaBlendValue := 225;

  FContent := TMemo.Create(self);
  FContent.Parent := self;
  FContent.Name := tiGetUniqueComponentNameFromParent(Self, 'Content');
  FContent.Align := alClient;
  FContent.WordWrap := false;
  FContent.ScrollBars := ssNone;
  FContent.Color := clInfoBk;
  FContent.BorderStyle := bsNone;
  FContent.ReadOnly := True;

//  FFadeInTime := 500; //ms

//  FFadeInTime := TTimer.Create(self);
//  FFadeInTime.Interval := 500;
//  FFadeInTime.Enabled := false;
//  FDG.Align   := alClient;
end;

destructor TtiDataPointHintForm.Destroy;
begin
//  FFadeInTime.Enabled := false;
//  FFadeInTime.Free;
  inherited;
end;

function TtiDataPointHintForm.GetContent: string;
begin
  Result := FContent.Text;
end;

//function TtiDataPointHintForm.GridIsEmpty: Boolean;
//begin
//  result := FDG.IsEmpty;
//end;

procedure TtiDataPointHintForm.SetContent(const Value: string);
begin
  FContent.Text := Value;
end;

function TtiDataPointHintForm.MaxStringWidth(const AStrings: TStrings;
  const ACanvas: TCanvas): integer;
var
  i: integer;
  LWidth: integer;

begin
  Result := 0;

  for i := AStrings.Count-1 downto 0 do
  begin
    LWidth := ACanvas.TextWidth(AStrings[i]);

    if LWidth > Result then
      Result := LWidth;
  end;


end;

procedure TtiDataPointHintForm.Show;
//var
//  LAlphaBlendDiff: Byte;
//  LAlphaBlendinc: Byte;
begin
   ClientHeight := FContent.Lines.Count * Canvas.TextHeight( 'M')
     + FContent.Margins.Top + FContent.Margins.Bottom;
   ClientWidth := MaxStringWidth(FContent.Lines, Canvas)
     + FContent.Margins.Left + FContent.Margins.Right;
  // ToDo: There is a problem with show being called multiple times while the
  //       form is still being positioned. This causes mouse litter. I can
  //       not identify the source of the problem, but this works around it.
  if FFirstShow then
    FFirstShow:= false
  else
    inherited;
//  AlphaBlendValue := 0;
//
//  LAlphaBlendDiff := FinalAlphaBlendValue - AlphaBlendValue;
//  LAlphaBlendInc := FFadeInTime div LAlphaBlendDiff;
//
//  while AlphaBlendValue < FinalAlphaBlendValue do
//  begin
////    sleep(1);
//    AlphaBlendValue := AlphaBlendValue + LAlphaBlendInc;
//  end;
end;

{
******************************** TtiChartSeries ********************************
}

destructor TtiChartSeries.Destroy;
begin
  FChartSeries.Free;
end;

{
****************************** TtiChartSeriesList ******************************
}
function TtiChartSeriesList.FindByChartSeries(
  const AChartSeries: TChartSeries): TtiChartSeries;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ChartSeries = AChartSeries then
    begin
      result := Items[i];
      break;
    end;
end;

function TtiChartSeriesList.GetItems(Index: integer): TtiChartSeries;
begin
  result := TtiChartSeries(inherited GetItems(Index));
end;

function TtiChartSeriesList.SeriesNameToTitle(
  const ASeriesName: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    if Items[i].ChartSeries.Name = ASeriesName then
      Exit(Items[i].ChartSeries.Title);
end;

procedure TtiChartSeriesList.SetItems(Index: integer; const AValue: TtiChartSeries);
begin
  inherited SetItems(Index, AValue);
end;

{
****************************** TtiTimeSeriesChart ******************************
}
constructor TtiTimeSeriesChart.Create(Owner : TComponent; const AButtonsPosition:
    TtiChartButtonsPosition; const ALegendPosition: TtiChartLegendPosition;
    const ALegendKind: TtiChartLegendKind);
begin
  inherited Create(Owner);
  Parent := Owner as TWinControl;
  FChartDataMappings := TtiChartDataMappings.Create(Self);

  FButtonsPosition := AButtonsPosition;
  FLegendPosition := ALegendPosition;
  Align := alClient;

  FButtonsPanel := TtiChartButtonsPanel.Create(self, FButtonsPosition);
  with FButtonsPanel do
  begin
    Parent := self;
    case FButtonsPosition of
      cbpLeft: Align := alLeft;
      cbpTop:  Align := alTop;
    end;
  end;

  FChartWithLegendPanel := TtiChartWithLegendPanel.Create(self, FLegendPosition, ALegendKind);
//  with FChartWithLegendPanel do
//  begin
//    Parent := self;
//    Align := alClient;
//  end;

  FChartLegendForm := FChartWithLegendPanel.ChartLegendPanel.ChartLegendForm;

  FDataPointHintForm := TtiDataPointHintForm.CreateNew(self);
  FDataPointHintForm.Parent := self;

  FSeriesList := TtiChartSeriesList.Create;
  FNeedRefreshSeries := false;

//  FDataPointHintForm.ParentBackground := false;
//
//  FDataPointHintForm.ParentColor := false;
//  FDataPointHintForm.AlphaBlend := true;
//  FDataPointHintForm.AlphaBlendValue := 200;

  FDrawCrossHairs    := true;
  FDrawCrossHairsNow := true;
  ResetMousePositionVisualCues;
  FCrossHairsDrawn := false;
  FDataCircleDrawn := False;
  SnapToDataSize   := 15;
  FTimeSeriesChart := false;
  FDisplayTestData := false;
  FConstrainViewToData := false;

  FBottomAxisMin := MaxInt;
  FBottomAxisMax := -MaxInt;
  FLeftAxisMin  := MaxInt;
  FLeftAxisMax  := -MaxInt;

  FScrollStyle   := ssBoth;
  FShowTestData := false;

  FShowDataPointHintTmr:= TTimer.Create(nil);
  FShowDataPointHintTmr.Enabled:= false;
  FShowDataPointHintTmr.Interval:= 250;
  FShowDataPointHintTmr.OnTimer:= DoShowDataPointHintTmr;

end;

procedure TtiTimeSeriesChart.DeselectAllSeries;
begin
  FChartLegendForm.DoSelectNone(self);
end;

destructor TtiTimeSeriesChart.Destroy;
begin
  ClearSeries;

  FTestData.Free;
  FButtonsPanel.Free;
  FChartWithLegendPanel.Free;
  FDataPointHintForm.Free;
  FChartDataMappings.Free;
  FShowDataPointHintTmr.Free;
  FSeriesList.Free;

  FreeAndNil(FManualZoomForm);

  inherited;
end;

procedure TtiTimeSeriesChart.ClearSeries;
begin
  FChart.SeriesList.Clear;
  FSeriesList.Clear;
  FChartLegendForm.ClearSeries;
  FNeedRefreshSeries := true;
end;

procedure TtiTimeSeriesChart.AddSeries(const ASeries: TChartSeries;
  const ASeriesSource: TtiObject);
var
  LChartSeries: TtiChartSeries;
begin
  LChartSeries := TtiChartSeries.Create;
  FSeriesList.Add(LChartSeries);
  LChartSeries.ChartSeries := ASeries;
  LChartSeries.SeriesSource := ASeriesSource;
  FChart.AddSeries(ASeries);
  FNeedRefreshSeries := true;
end;

function TtiTimeSeriesChart.AddBarSeries(const ATitle: string;
  const AVisible: boolean; const ASeriesSource: TtiObject): TBarSeries;
var
  LSeries: TBarSeries;
begin
  LSeries := TBarSeries.Create(nil);
  LSeries.Title := ATitle;
  LSeries.Marks.Visible := false;
  LSeries.BarWidthPercent := 100; // Perhaps this should be a param ?
  LSeries.Visible := AVisible;
  AddSeries(LSeries, ASeriesSource);
  result := LSeries;
end;

procedure TtiTimeSeriesChart.AddButton(const AButtonHint: string;
  const AImageResName: string; const ADoButtonClick: TNotifyEvent;
  const AButtonOrder: Integer);
begin
  Assert(Assigned(FButtonsPanel));
  FButtonsPanel.AddButton(AButtonHint, AImageResName, ADoButtonClick, AButtonOrder);
end;

function TtiTimeSeriesChart.AddDateTimeBarSeries(const ATitle : string;
  const AVisible: boolean; const ASeriesSource: TtiObject): TBarSeries;
begin
  TimeSeriesChart := true;
  result := AddBarSeries(ATitle, AVisible, ASeriesSource);
end;

procedure TtiTimeSeriesChart.AddDateTimeGap(const ASeries: TChartSeries;
  const AXBeforeGap, AXAfterGap: TDateTime);
begin
  Assert(Assigned(ASeries), 'Series not assigned');
  ASeries.AddNullXY((AXBeforeGap + AXAfterGap) / 2.0, (FLeftAxisMin + FLeftAxisMax) / 2.0, '');
end;

procedure TtiTimeSeriesChart.AddDateTimeGap(const ASeriesName: string; const
    AXBeforeGap: TDateTime; const AXAfterGap: TDateTime);
var
  LSeries: TChartSeries;
begin
  LSeries := SeriesByName(SeriesTitleToName(ASeriesName));
  Assert(LSeries <> nil, 'Cannot find series <' + ASeriesName + '>');
  AddDateTimeGap(LSeries, AXBeforeGap, AXAfterGap);
end;

procedure TtiTimeSeriesChart.AddDateTimeGap(const AChartSeries: TtiChartSeries;
  const AXBeforeGap, AXAfterGap: TDateTime);
begin
  Assert(AChartSeries.TestValid(TtiChartSeries), CTIErrorInvalidObject);
  AddDateTimeGap(AChartSeries.ChartSeries, AXBeforeGap, AXAfterGap);
end;

function TtiTimeSeriesChart.AddDateTimeLineSeries(const ATitle: string;
  const AVisible: boolean; const ASeriesSource: TtiObject): TLineSeries;
begin
  TimeSeriesChart := true;
  result := AddLineSeries(ATitle, AVisible, ASeriesSource);
end;

procedure TtiTimeSeriesChart.AddDateTimeValues(
  const ASeries: TChartSeries; const AX: TDateTime; const AY: real;
  const ALabel: string; const AColor: TColor);
var
  LColor: TColor;
begin
  Assert(Assigned(ASeries), 'Series not assigned');

  FBottomAxisMin := Min(FBottomAxisMin, AX);
  FBottomAxisMax := Max(FBottomAxisMax, AX);

  FLeftAxisMin := Min(FLeftAxisMin, AY);
  FLeftAxisMax := Max(FLeftAxisMax, AY);

  ASeries.ColorEachPoint := True;
  if AColor = clDefault then
    LColor := ASeries.SeriesColor
  else
    LColor := AColor;
  ASeries.AddXY(AX, AY, ALabel, LColor);
end;

procedure TtiTimeSeriesChart.AddDateTimeValues(const ASeriesTitle : string;
    const AX: TDateTime; const AY: real; const ALabel: string;
    const AColor: TColor);
var
  LSeries: TChartSeries;
begin
  LSeries := SeriesByName(SeriesTitleToName(ASeriesTitle));
  Assert(LSeries <> nil, 'Cannot find series <' + ASeriesTitle + '>');
  AddDateTimeValues(LSeries, AX, AY, ALabel, AColor);
end;

procedure TtiTimeSeriesChart.AddDateTimeValues(
  const AChartSeries: TtiChartSeries; const AX: TDateTime; const AY: real;
  const ALabel: string; const AColor: TColor);
begin
  Assert(AChartSeries.TestValid(TtiChartSeries), CTIErrorInvalidObject);
  AddDateTimeValues(AChartSeries.ChartSeries, AX, AY, ALabel, AColor);
end;

function TtiTimeSeriesChart.AddLineSeries(const ATitle: string;
  const AVisible: boolean; const ASeriesSource: TtiObject): TLineSeries;
var
  LSeries: TLineSeries;
begin
  LSeries := TLineSeries.Create(nil);
  LSeries.Name := SeriesTitleToName(ATitle);
  LSeries.Title := ATitle;
  LSeries.Pointer.Style := psCross;
  LSeries.Pointer.InflateMargins := false;
  LSeries.XValues.DateTime := true;
  LSeries.SeriesColor := GetNextSeriesColor;
  LSeries.Pointer.Pen.Color:= LSeries.SeriesColor;
  LSeries.Visible := AVisible;
  AddSeries(LSeries, ASeriesSource);
  result := LSeries;
end;

procedure TtiTimeSeriesChart.ClearSeriesValues;
begin
  FChart.SeriesList.ClearValues;
end;

procedure TtiTimeSeriesChart.AdjustScrollBarPositions;

  procedure _SetupScrollBar(pScrollBar : TScrollBar;
                               prMaxAxisRange : real;
                               prCurrentAxisRange : real;
                               prPosition : real;
                               Axis: TtiAxis;
                               out AScrollBarVisible: boolean);
    var
      LOnChange : TNotifyEvent;
      LScrollAxisRange: Double;
      LScrollBarRange: Integer;
    begin
      // We don't scroll the _entire_ axis range as that can put the data off
      // the graph at the maximum scroll.
      LScrollAxisRange := prMaxAxisRange - prCurrentAxisRange;
      AScrollBarVisible := not IsZero(LScrollAxisRange, 0.0000000001);
      if not AScrollBarVisible then
        Exit; //==>

      // Work with the scroll bar minimum and maximum to allow the scroll resolution to be changed.
      LScrollBarRange := pScrollBar.Max - pScrollBar.Min;

      LOnChange := pScrollBar.OnChange;
      pScrollBar.OnChange := nil;
      try
        if prMaxAxisRange <> 0 then
          pScrollBar.PageSize := Trunc(prCurrentAxisRange * LScrollBarRange / prMaxAxisRange)
        else
          pScrollBar.PageSize := LScrollBarRange;
        // Adjust scrollbar range to account for thumb tab.
        LScrollBarRange := pScrollBar.Max - (pScrollBar.PageSize - 1) - pScrollBar.Min;

        if prMaxAxisRange <> 0 then
        begin
          if Axis = axVertical then
            pScrollBar.Position := (pScrollBar.Max - (pScrollBar.PageSize - 1))- Trunc(prPosition * LScrollBarRange / LScrollAxisRange)
          else
            pScrollBar.Position := Trunc(prPosition * LScrollBarRange / LScrollAxisRange);
        end else
          pScrollBar.Position := 0;
      finally
        pScrollBar.OnChange := LOnChange;
      end;
    end;

  var
    LMaxAxisRange : real;
    LCurrentAxisRange : real;
    LPosition : real;
    LScrollStyle: TScrollStyle;
    LScrollBarVisible: boolean;

begin
  // No scroll bars to show
  if FScrollStyle = ssNone then
    Exit; //==>

  if not Zoomed then
  begin
    FChartPanel.ScrollBars := ssNone;
    Exit; //==>
  end;

  LScrollStyle := FScrollStyle;

  // Setup the bottom scrollbar
  if (LScrollStyle = ssBoth) or
     (LScrollStyle = ssHorizontal) then
  begin
    FscbBottom.Left := FChart.ChartRect.Left;
    FscbBottom.Width := FChart.ChartWidth;

    // Set the position of the slider in the scroll bar
    LMaxAxisRange    := VisiblesSeriesMaxX           - VisiblesSeriesMinX;
    LCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
    LPosition        := FChart.BottomAxis.Minimum - VisiblesSeriesMinX;
    _SetupScrollBar(FscbBottom, LMaxAxisRange, LCurrentAxisRange, LPosition,
        axHorizontal, LScrollBarVisible);
    // Remove scrollbar if necessary
    if not LScrollBarVisible then
    begin
      if LScrollStyle = ssBoth then
        LScrollStyle := ssVertical
      else
        LScrollStyle := ssNone;
    end;
  end;

  // Setup the left scrollbar
  if (LScrollStyle = ssBoth) or
     (LScrollStyle = ssVertical) then
  begin
    FscbLeft.Top   := FChart.ChartRect.Top;
    FscbLeft.Height := FChart.ChartHeight;

    // Set the position of the slider in the scroll bar
    LMaxAxisRange    := VisiblesSeriesMaxY           - VisiblesSeriesMinY;
    LCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum;
    LPosition        := FChart.LeftAxis.Minimum - VisiblesSeriesMinY;
    _SetupScrollBar(FscbLeft, LMaxAxisRange, LCurrentAxisRange, LPosition,
        axVertical, LScrollBarVisible);
    // Remove scrollbar if necessary
    if not LScrollBarVisible then
    begin
      if LScrollStyle = ssBoth then
        LScrollStyle := ssHorizontal
      else
        LScrollStyle := ssNone;
    end;
  end;

  // This will show/hide and position the scrollbars.
  FChartPanel.ScrollBars := LScrollStyle;
end;

procedure TtiTimeSeriesChart.Clear;
begin
  ClearSeries;
  DoSBDefaultZoomClick(nil);
  FChart.Refresh;
end;

procedure TtiTimeSeriesChart.ClearMousePositionVisualCues;
begin
  DoChartMouseMove(self,
                    [],
                    -1, -1);
end;

procedure TtiTimeSeriesChart.HideCrossHairs;
begin
  // Erase the old cross hairs
  if (FOldX <> -1) then begin
    DoDrawCrossHairs(FOldX, FOldY, False {pbDraw});
    FOldX := -1;
  end;
end;

procedure TtiTimeSeriesChart.ShowCrossHairs(const AX, AY: Integer);
begin
  // Draw crosshair at current position
  DoDrawCrossHairs(AX, AY, True {pbDraw});

  // Store old position
  FOldX := AX;
  FOldY := AY;
end;

procedure TtiTimeSeriesChart.DoChartMouseMove(Sender: TObject; Shift:
    TShiftState; X, Y: Integer);

  Procedure DrawCircle(AX, AY: Integer; ADraw: Boolean);
    begin
      if FDataCircleDrawn = ADraw then
        Exit; //==>

      FDataCircleDrawn := ADraw;

      With Chart,Canvas do
      begin
        // You have to enter the complimentary colour of what you want !
        Pen.Color := clGray;
        Pen.Style := psSolid;
        Pen.Mode := pmXor  ;
        Pen.Width := 1      ;
        Brush.Style := bsClear;

        Ellipse(aX-FSnapToDataRadius,
                 aY-FSnapToDataRadius,
                 aX+FSnapToDataRadius,
                 aY+FSnapToDataRadius);

        FOldCircX := aX;
        FOldCircY := aY;
      end;
    end;

var
  LSeries      : TChartSeries;
  LSeriesIndex  : integer;
  i : integer;
begin
  // Clear the values under the mouse cursor which are surfaced as
  // properties of TtiChart
  XValueUnderMouse    := 0;
  YValueUnderMouse    := 0;
  XDataValueUnderMouse := 0;
  YDataValueUnderMouse := 0;

  if FData = nil then
    Exit; //==>

  if Chart.SeriesCount = 0 then
    exit; //==>

  // Set the values under the mouse cursor, based on the first chart series
  XValueUnderMouse := Chart.Series[0].XScreenToValue(X);
  YValueUnderMouse := Chart.Series[0].YScreenToValue(Y);

  HideCrossHairs;

  // Erase old circle
  if (FOldCircX <> -1) then begin
    DrawCircle(FOldCircX, FOldCircY, False {ADraw});
    FOldCircX := -1;
  end;

  // Check if mouse is inside Chart rectangle
  if (not (GetMouseInCrossHairRegion and
             DrawCrossHairs and
             DrawCrossHairsNow)) or
     (ssLeft in Shift) then begin
    Screen.Cursor := crDefault;
    OnDrawKey(nil, -1, nil, nil);
    Exit;
  end;

  ShowCrossHairs(X, Y);

  if Screen.Cursor <> crNone then
    Screen.Cursor := crNone;

  // Scan all the series looking for the closest data point
  // If found, LSeries and LSeriesIndex will have been set.
  LSeriesIndex := -1;
  LSeries := nil;
  for i := 0 to FChart.SeriesCount - 1 do begin
    LSeries := FChart.Series[i];
    if LSeries.Active then
    begin
      LSeriesIndex := LSeries.GetCursorValueIndex;
      // Don't proceed for null points (gap markers)
      if LSeries.IsNull(LSeriesIndex) then
         LSeriesIndex := -1;
    end;

    if LSeriesIndex <> -1 then
      Break; //==>
  end;

  FDataUnderMouse := nil;

  // A non-null data point was found close to the mouse cursor, so set some values
  if (LSeriesIndex <> -1) and
     (LSeries <> nil) then
  begin
    XDataValueUnderMouse := LSeries.XValues[LSeriesIndex];
    YDataValueUnderMouse := LSeries.YValues[LSeriesIndex];

    if  Assigned(FOnChartDataGap) then
    begin
      Assert(Assigned(FOnGetDataByDateTime));
      FOnGetDataByDateTime(XDataValueUnderMouse, FDataUnderMouse);
    end;
    if (not Assigned(FDataUnderMouse)) and (LSeriesIndex < FData.Count) then
      FDataUnderMouse := TObject(FData.Items[LSeriesIndex]);

    DrawCircle(LSeries.CalcXPosValue(XDataValueUnderMouse),
               LSeries.CalcYPosValue(YDataValueUnderMouse),
               True {ADraw});

    OnDrawKey(LSeries,
               LSeriesIndex,
               DataUnderMouse,
               FData);

    if FDataPointHintForm.Visible then
      ShowDataPointHint
    else
    begin
      FShowDataPointHintTmr.Enabled:= false;
      FShowDataPointHintTmr.Enabled:= true;
    end;
  end else
  begin
    FShowDataPointHintTmr.Enabled:= false;
    OnDrawKey(nil, -1, nil, nil);
    HideDataPointHint;
  end;

  Application.ProcessMessages;
end;

procedure TtiTimeSeriesChart.DoDrawCrossHairs(const AX, AY: Integer; const ADoDraw :
    boolean);
begin
  if FCrossHairsDrawn = ADoDraw then
    Exit; //==>

  FCrossHairsDrawn := ADoDraw;
  FChart.Canvas.Pen.Color := clGray;
  FChart.Canvas.Pen.Mode := pmXor  ;
  FChart.Canvas.Pen.Style := psSolid;
  FChart.Canvas.Pen.Width := 1      ;

  // Draw the vertical line
  FChart.Canvas.MoveTo(AX,FChart.ChartRect.Top-FChart.Height3D);
  FChart.Canvas.LineTo(AX,FChart.ChartRect.Bottom-FChart.Height3D);

  // Draw the horizontal line
  FChart.Canvas.MoveTo(FChart.ChartRect.Left+FChart.Width3D,AY);
  FChart.Canvas.LineTo(FChart.ChartRect.Right+FChart.Width3D,AY);
end;

procedure TtiTimeSeriesChart.DoHorizontalScroll(Sender : TObject);
var
  LMin: Double;
  LMax: Double;
  LMaxAxisRange: Double;
  LCurrentAxisRange: Double;
  LScrollAxisRange: Double;
  LScrollBarRange: Integer;
  LScrollAxisAmount: Double;
begin
  LMaxAxisRange     := VisiblesSeriesMaxX - VisiblesSeriesMinX;
  LCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
  // We don't scroll the _entire_ axis range as that can put the data off
  // the graph at the maximum scroll.
  LScrollAxisRange := LMaxAxisRange - LCurrentAxisRange;

  // Work with the scroll bar minimum and maximum to allow the scroll resolution to be changed.
  LScrollBarRange := FscbBottom.Max - (FscbBottom.PageSize - 1) - FscbBottom.Min;

  // Scroll amount
  LScrollAxisAmount := ((FscbBottom.Min + LScrollBarRange - FscbBottom.Position) / LScrollBarRange) * LScrollAxisRange;
  LMin := (VisiblesSeriesMaxX - LCurrentAxisRange) - LScrollAxisAmount;
  LMax := LMin + LCurrentAxisRange;

  FChart.BottomAxis.SetMinMax(LMin, LMax);
  RangeChange;
end;

procedure TtiTimeSeriesChart.DoMouseDown(Sender: TObject;Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
  begin
    FDrawCrossHairsSaved := DrawCrossHairs;
    DrawCrossHairs := false;
  end;
end;

procedure TtiTimeSeriesChart.DoMouseLeave(Sender: TObject);
begin
  HideDataPointHint;
  HideCrossHairs;
  FChart.Repaint;
  Screen.Cursor := crDefault;
end;

procedure TtiTimeSeriesChart.DoMouseUp(Sender: TObject;Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
  begin
    DrawCrossHairs := FDrawCrossHairsSaved;
  end;
end;

procedure TtiTimeSeriesChart.DoOnAllowScroll(Sender: TChartAxis; var AMin:
    double; var AMax: double; var AllowScroll: boolean);
begin
  AllowScroll := (FChart.Zoomed) or (not FConstrainViewToData);
end;

procedure TtiTimeSeriesChart.DoOnBeforeDrawAxes(sender : TObject);

  const
    CDTMilliSecond = 1/24/60/60/1000;
    CDTSecond  = 1/24/60/60;
    CDTMinute  = 1/24/60  ;
    CDTHour    = 1/24     ;
    CDTDay     = 1.0      ;
    CDTMonth   = 365/12;
    CDTYear    = 365;
    CDT20Years = CDTYear*20;
  var
    LDTPeriod : TDateTime;

begin
  if not FTimeSeriesChart then
      Exit; //==>

    if FData = nil then
      Exit; //==>

    LDTPeriod := FChart.BottomAxis.Maximum -
                 FChart.BottomAxis.Minimum;

  {
      TDateTimeStep = (dtOneSecond, dtFiveSeconds, dtTenSeconds, dtFifteenSeconds,
                        dtThirtySeconds, dtOneMinute, dtFiveMinutes, dtTenMinutes,
                        dtFifteenMinutes, dtThirtyMinutes, dtOneHour, dtTwoHours,
                        dtSixHours, dtTwelveHours, dtOneDay, dtTwoDays, dtThreeDays,
                        dtOneWeek, dtHalfMonth, dtOneMonth, dtTwoMonths, dtSixMonths,
                        dtOneYear);
  }

    // > 20 years
    if LDTPeriod > CDT20Years then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneYear];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'yyyy';
    // > 1 and       <= 20 years
    end else if (LDTPeriod > CDTYear) and (LDTPeriod <= CDT20Years) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneYear];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'yyyy';
    // > 3 months  and <= 1 year
    end else if (LDTPeriod > CDTMonth*3) and (LDTPeriod <= CDTYear) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneMonth];
      FChart.BottomAxis.LabelsMultiline := true;
      FChart.BottomAxis.DateTimeFormat := 'mmm yyyy';
    // > 2 months <= 3 months
    end else if (LDTPeriod > CDTMonth*2) and (LDTPeriod <= CDTMonth*3) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneWeek];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    // > 1 months <= 2 months
    end else if (LDTPeriod > CDTMonth) and (LDTPeriod <= CDTMonth*2) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtThreeDays];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    // > 10 day    and <= 1 month
    end else if (LDTPeriod > CDTDay*10) and (LDTPeriod <= CDTMonth) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtTwoDays];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    // > 3 days    and <= 10 days
    end else if (LDTPeriod > CDTDay*3) and (LDTPeriod <= CDTDay*10) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneDay];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    // > 1 day    and <= 3 days
    end else if (LDTPeriod > CDTDay) and (LDTPeriod <= CDTDay*3) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtSixHours];
      FChart.BottomAxis.LabelsMultiline := true;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm dd-mmm';
    // > 4 hours  and <= 1 day
    end else if (LDTPeriod > CDTHour * 4) and (LDTPeriod <= CDTDay) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneHour];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm';
    // > 29 minutes and <= 4 hours
    end else if (LDTPeriod > CDTMinute * 29) and (LDTPeriod <= CDTHour * 4) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtFifteenMinutes];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm';
    // > 10 minutes and <= 29 minutes
    end else if (LDTPeriod > CDTMinute * 10) and (LDTPeriod <= CDTMinute * 29) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneMinute];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm';
    // > 2 minutes and <= 10 minutes
    end else if (LDTPeriod > CDTMinute * 2) and (LDTPeriod <= CDTMinute * 10) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtThirtySeconds];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss';
    // > 20 seconds and <= 2 minutes
    end else if (LDTPeriod > CDTSecond * 20) and (LDTPeriod <= CDTMinute * 2) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtTenSeconds];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss';
    // > 5 seconds and <= 20 seconds
    end else if (LDTPeriod > CDTSecond * 5) and (LDTPeriod <= CDTSecond * 20) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneSecond];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss';
    // > 250 milliseconds and <= 5 seconds
    end else if (LDTPeriod > CDTSecond / 4) and (LDTPeriod <= CDTSecond * 5) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneSecond];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss.zzz';
    // >= 1 millisecond and <= 250 milliseconds
    end else if (LDTPeriod >= CDTMilliSecond) and (LDTPeriod <= CDTMilliSecond * 250) then begin
      FChart.BottomAxis.Increment      := DateTimeStep[dtOneMilliSecond];
      FChart.BottomAxis.LabelsMultiline := false;
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss.zzz';
    end else
      // Do Nothing;
  ;
      //raise exception.create('Invalid axis range passed to TtiChart.DoOnBeforeDrawAxes');

  //    BottomAxis.RoundFirstLabel := true;
  //    BottomAxis.ExactDateTime  := true;
  //    BottomAxis.LabelsOnAxis := true;
  //    BottomAxis.TickOnLabelsOnly := true;
  //    BottomAxis.MinorTickCount := 0;
end;

procedure TtiTimeSeriesChart.DoOnScroll(Sender : TObject);
begin
  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.DoOnUndoZoom(Sender : TObject);
begin
  ResetZoom;
end;

procedure TtiTimeSeriesChart.DoOnZoom(Sender : TObject);
begin
  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.DoOnAfterDraw(Sender : TObject);
var
  LPoint: TPoint;
begin
  // The repaint overwrites any custom drawing.
  FCrossHairsDrawn := False;
  FDataCircleDrawn := False;

  if MouseInCrossHairRegion and DrawCrossHairs and DrawCrossHairsNow then
  begin
    try
      LPoint := Chart.ScreenToClient(Mouse.CursorPos);
    except
      // Ignore unable to get cursor pos (such as when screensaver is active
      // or workstation is locked)
      on EOSError do
        Exit; //==>
    end;

    ShowCrossHairs(LPoint.X, LPoint.Y);
  end;
end;

procedure TtiTimeSeriesChart.DoSBCopyToClipBrdClick(sender : TObject);
begin
  CopyToClipBoard;
end;

procedure TtiTimeSeriesChart.CopyToClipBoard;
var
  LScratchChart: TtiTimeSeriesChart;
  LSeries: TtiChartSeries;
  i: integer;
begin
  if IncludeLegendInClipboard and not FChart.Legend.Visible then
  begin
    LScratchChart := TtiTimeSeriesChart.Create(self, cbpTop, FLegendPosition);
    try
      LScratchChart.Visible := false;
      LScratchChart.BevelOuter := bvNone;
      LScratchChart.Color := Color;
      LScratchChart.DrawCrossHairs := False;
      LScratchChart.ShowButtons := False;
      LScratchChart.ShowLegend := false;
      LScratchChart.ConstrainViewToData := True;
      LScratchChart.AxisBottom.Title.Visible := False;
      LScratchChart.AxisLeft.Title.Visible := False;
      LScratchChart.OnAssignGraphData := FOnAssignGraphData;
      LScratchChart.OnAssignSeriesData := FOnAssignSeriesData;
      LScratchChart.OnChartDataGap := FOnChartDataGap;
      LScratchChart.OnSeriesDataGap := FOnSeriesDataGap;
      // Customizations for exporting as image.
      LScratchChart.Chart.MarginLeft := 1;
      LScratchChart.Chart.MarginTop := 2;
      LScratchChart.Chart.MarginRight := 1;
      LScratchChart.Chart.MarginBottom := 1;
      LScratchChart.Chart.Legend.Visible := True;
      LScratchChart.Chart.Legend.ColorWidth := 10;
      LScratchChart.Chart.Legend.FontSeriesColor := True;
      LScratchChart.Chart.Legend.LegendStyle := lsSeries;
      LScratchChart.Chart.Legend.Shadow.HorizSize := 0;
      LScratchChart.Chart.Legend.Shadow.VertSize := 0;
      LScratchChart.Chart.Legend.Symbol.Width := 10;
      LScratchChart.Chart.Legend.Transparent := True;
      LScratchChart.Chart.Legend.Title.Caption := 'Legend';
      LScratchChart.Width := Width;
      LScratchChart.Height := Height;

      for i := 0 to FSeriesList.Count - 1 do
      begin
        LSeries := FSeriesList.Items[i];
        LScratchChart.AddDateTimeLineSeries(LSeries.ChartSeries.Title,
            LSeries.ChartSeries.Visible, LSeries.SeriesSource);
      end;

      LScratchChart.Data := Data;
      LScratchChart.ConstrainChartAxesView(Chart.BottomAxis.Minimum,
          Chart.BottomAxis.Maximum, Chart.LeftAxis.Minimum, Chart.LeftAxis.Maximum);

      LScratchChart.Chart.CopyToClipboardMetafile(true);
    finally
      LScratchChart.Free;
    end;
  end
  else
    FChart.CopyToClipboardMetafile(True);  { <--- Enhanced Metafile = True }
end;

procedure TtiTimeSeriesChart.DoSBDefaultZoomClick(sender : TObject);
begin
  FDrawCrossHairsSaved := DrawCrossHairs;
  DrawCrossHairs := false;
  try
    ResetZoom;
  finally
    DrawCrossHairs := FDrawCrossHairsSaved;
  end;
end;

procedure TtiTimeSeriesChart.DoSBDetailZoomClick(sender: TObject);

  function _FindIndexInVisibleSeriesValues(const AValue: Double;
    out ASeries: TChartSeries; out AValueIndex: integer): boolean;
  var
    i: Integer;
    LSeries: TChartSeries;
    LMaxValueCount: integer;
  begin
    ASeries := nil;
    AValueIndex := -1;
    result := false;

    // Find the visible series with the most data points. This will give
    // a more accurate result as some series may have gaps.
    LMaxValueCount := 0;
    for i := 0 to Chart.SeriesList.Count - 1 do
    begin
      LSeries := Chart.SeriesList.Items[i];
      if LSeries.Visible and (LSeries.Count > LMaxValueCount) then
      begin
        ASeries := LSeries;
        LMaxValueCount := LSeries.Count;
      end;
    end;

    // Find the index of the first data point on or after the given X value.
    if Assigned(ASeries) then
      for i := 0 to ASeries.XValues.Count - 1 do
        if ASeries.XValues.Items[i] >= AValue then
        begin
          AValueIndex := i;
          result := true;
          break;
        end;
  end;

var
  LSeries: TChartSeries;
  LChartWidth: Integer;
  LXAxisMidPointIndex: Integer;
  LXAxisMinPointIndex: Integer;
  LXAxisMaxPointIndex: Integer;
  LXMid: Double;
  LXMin: Double;
  LXMax: Double;
begin
  //TODO: This might not work correctly if there are gaps in all visible series.

  // Current centre point date-time
  LXMid := (Chart.BottomAxis.Minimum + Chart.BottomAxis.Maximum) / 2;

  if _FindIndexInVisibleSeriesValues(LXMid, LSeries, LXAxisMidPointIndex) then
  begin
    Assert(Assigned(LSeries), 'Found index but series not assigned');
    Assert(LXAxisMidPointIndex <> -1, 'Found index but index not assigned');

    LChartWidth := Chart.ChartWidth;

    // Calculate min index for detail range, constrain to graph range
    if LXAxisMidPointIndex - (LChartWidth div 2) >= 0 then
      LXAxisMinPointIndex := LXAxisMidPointIndex - (LChartWidth div 2)
    else
      LXAxisMinPointIndex := 0;

    // Calculate max index for detail range, constrain to graph range
    if LXAxisMinPointIndex + LChartWidth - 1 <= (LSeries.XValues.Count - 1) then
      LXAxisMaxPointIndex := LXAxisMinPointIndex + LChartWidth - 1
    else
      LXAxisMaxPointIndex := LSeries.XValues.Count - 1;

    LXMin := LSeries.XValue[LXAxisMinPointIndex];
    LXMax := LSeries.XValue[LXAxisMaxPointIndex];

    ConstrainChartAxesView(LXMin, LXMax,
        Chart.LeftAxis.Minimum, Chart.LeftAxis.Maximum);

    RepositionChart;
    RangeChange;
  end;
end;

procedure TtiTimeSeriesChart.DoSBManualZoomClick(sender: TObject);
var
  LManualZoomButton: TtiSpeedButton;
begin
  Assert(sender is TtiSpeedButton);

  LManualZoomButton := sender as TtiSpeedButton;

  if not Assigned(FManualZoomForm) then
  begin
    FManualZoomForm := TtiChartManualZoomForm.CreateNew(self);
    FManualZoomForm.Parent := nil;
  end;

  FManualZoomForm.Execute(self, LManualZoomButton);
end;


//procedure TtiTimeSeriesChart.DoSBViewLegendClick(sender : TObject);
//begin
  //  if FChartLegendForm = nil then
  //    FChartLegendForm := TtiChartLegendForm.CreateNew(nil);
  //  FChartLegendForm.TIChart := FtiChart;
  //  SnapEditDialogToButton(FChartLegendForm, Sender);
  //  FChartLegendForm.Show;
//end;

procedure TtiTimeSeriesChart.DoSBZoomInClick(sender : TObject);
begin
  Zoom(cZoomPercent);
end;

procedure TtiTimeSeriesChart.DoSBZoomOutClick(sender : TObject);
begin
  Zoom(-cZoomPercent);
end;

procedure TtiTimeSeriesChart.DoShowDataPointHintTmr(Sender: TObject);
begin
  FShowDataPointHintTmr.Enabled:= false;
  ShowDataPointHint;
end;

procedure TtiTimeSeriesChart.DoVerticalScroll(Sender : TObject);
var
  LMin: Double;
  LMax: Double;
  LMaxAxisRange: Double;
  LCurrentAxisRange: Double;
  LScrollAxisRange: Double;
  LScrollBarRange: Integer;
  LScrollAxisAmount: Double;
begin
  LMaxAxisRange     := VisiblesSeriesMaxY - VisiblesSeriesMinY;
  LCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum;
  // We don't scroll the _entire_ axis range as that can put the data off
  // the graph at the maximum scroll.
  LScrollAxisRange := LMaxAxisRange - LCurrentAxisRange;

  // Work with the scroll bar minimum and maximum to allow the scroll resolution to be changed.
  LScrollBarRange := FscbLeft.Max - (FscbLeft.PageSize - 1) - FscbLeft.Min;

  // Scroll amount
  LScrollAxisAmount := (FscbLeft.Position / LScrollBarRange) * LScrollAxisRange;
  LMin := (VisiblesSeriesMaxY - LCurrentAxisRange) - LScrollAxisAmount;
  LMax := LMin + LCurrentAxisRange;

  FChart.LeftAxis.SetMinMax(LMin, LMax);
  RangeChange;
end;

function TtiTimeSeriesChart.FscbBottom: TScrollBar;
begin
  result := FChartPanel.scbHorizontal;
end;

function TtiTimeSeriesChart.FscbLeft: TScrollBar;
begin
  result := FChartPanel.scbVertical;
end;

function TtiTimeSeriesChart.GetAxisBottom: TChartAxis;
begin
  result := FChart.BottomAxis;
end;

function TtiTimeSeriesChart.GetAxisLeft: TChartAxis;
begin
  result := FChart.LeftAxis;
end;

function TtiTimeSeriesChart.GetChartColor: TColor;
begin
  result := FChart.Color;
end;

function TtiTimeSeriesChart.GetChartPopupMenu: TPopupMenu;
begin
  result := FChart.PopupMenu;
end;

function TtiTimeSeriesChart.GetDataPointHintText: string;
begin
  Result := DataPointHintForm.GetContent;
end;

function TtiTimeSeriesChart.GetMouseInCrossHairRegion: Boolean;
var
  LPoint: TPoint;
begin
  try
    LPoint.X := Mouse.CursorPos.X;
    LPoint.Y := Mouse.CursorPos.Y;
  except
    // Ignore unable to get cursor pos (such as when screensaver is active
    // or workstation is locked)
    on EOSError do
    begin
      Result := False;
      Exit; //==>
    end;
  end;

  try
    LPoint  := FChart.ScreenToClient(LPoint);
    result  := IsFormFocused and
                  PtInRect(Chart.ChartRect,
                           Point(LPoint.X-Chart.Width3D,
                           LPoint.Y+Chart.Height3D));
  except
    on e: EInvalidOperation do
      result := False;
  end;
end;

function TtiTimeSeriesChart.GetNextSeriesColor: TColor;
var
  LColorIndex: Integer;
begin
  LColorIndex := FChart.SeriesCount;
  while LColorIndex > High(cuaSeriesColors) do
    LColorIndex := LColorIndex - High(cuaSeriesColors);
  result := cuaSeriesColors[LColorIndex];
end;

function TtiTimeSeriesChart.GetOnDblClickChart: TNotifyEvent;
begin
  result := FChart.OnDblClick;
end;

function TtiTimeSeriesChart.GetOwnerForm(Control: TComponent): TForm;

  function _GetOwner(Control : TComponent): TComponent;
  begin
    try
      result := Control.Owner;
      if (result <> nil) and
         (not (result is TForm)) then
        result := _GetOwner(result);
    except
      on e: EInvalidOperation do
        result := nil;
    end;
  end;

begin
  Result := TForm(_GetOwner(Control));
end;

function TtiTimeSeriesChart.GetView3D: Boolean;
begin
  Result := FChart.View3D;
end;

function TtiTimeSeriesChart.GetVisibleSeriesAsString: string;
var
  i: Integer;
  LStringList: TStringList;
begin
  LStringList:= TStringList.Create;
  try
    for i:= 0 to FChart.SeriesCount - 1 do
      if FChartLegendForm.SeriesVisible[FChart.Series[i].Name] then
        LStringList.Values[FChart.Series[i].Name]:= '1'
      else
        LStringList.Values[FChart.Series[i].Name]:= '0';
    Result:= LStringList.CommaText;
  finally
    LStringList.Free;
  end;
end;

function TtiTimeSeriesChart.GetVisibleSeriesCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i:= 0 to FChart.SeriesCount - 1 do
    if FChart.Series[i].Visible then
      Inc(result);
end;

function TtiTimeSeriesChart.GetVisiblesSeriesMaxX: real;
var
  i: Integer;
  LValue: real;
begin
  Result := -MaxInt;
  for i := 0 to FChart.SeriesCount-1 do
    if FChart.Series[i].Visible then
    begin
      LValue := FChart.Series[i].MaxXValue;
      // Ignore empty series.
      if LValue > 0.0 then
        Result := Max(Result, LValue);
    end;
  if Result = -MaxInt then
    Result := 0;
end;

function TtiTimeSeriesChart.GetVisiblesSeriesMaxY: real;
var
  i: Integer;
  LValue: real;
begin
  Result := -MaxInt;
  for i := 0 to FChart.SeriesCount-1 do
    if FChart.Series[i].Visible then
    begin
      LValue := FChart.Series[i].MaxYValue;
      Result := Max(Result, LValue);
    end;
  if Result = -MaxInt then
    Result := 0;
end;

function TtiTimeSeriesChart.GetVisiblesSeriesMinX: real;
var
  i: Integer;
  LValue: real;
begin
  Result := MaxInt;
  for i := 0 to FChart.SeriesCount-1 do
    if FChart.Series[i].Visible then
    begin
      LValue := FChart.Series[i].MinXValue;
      // Ignore empty series.
      if LValue > 0.0 then
        Result := Min(Result, LValue);
    end;
  if Result = MaxInt then
    Result := 0;
end;

function TtiTimeSeriesChart.GetVisiblesSeriesMinY: real;
var
  i: Integer;
  LValue: real;
begin
  Result := MaxInt;
  for i := 0 to FChart.SeriesCount-1 do
    if FChart.Series[i].Visible then
    begin
      LValue := FChart.Series[i].MinYValue;
      Result := Min(Result, LValue);
    end;
  if Result = MaxInt then
    Result := 0;
end;

function TtiTimeSeriesChart.GetZoomed: Boolean;
begin
  Result :=
      (CompareValue(FChart.LeftAxis.Minimum, VisiblesSeriesMinY, CAxisEpsilon) > 0) or
      (CompareValue(FChart.LeftAxis.Maximum, VisiblesSeriesMaxY, CAxisEpsilon) < 0) or
      (CompareValue(FChart.BottomAxis.Minimum, VisiblesSeriesMinX, CAxisEpsilon) > 0) or
      (CompareValue(FChart.BottomAxis.Maximum, VisiblesSeriesMaxX, CAxisEpsilon) < 0);
end;

function TtiTimeSeriesChart.GetZoomPen: TTeeZoomPen;
begin
  Result := FChart.Zoom.Pen;
end;

procedure TtiTimeSeriesChart.HideDataPointHint;
begin
  if FDataPointHintForm.Showing then
  begin
    FDataPointHintForm.Hide;
    // Remove crosshair litter that remains behind hint form.
    FChart.Repaint;
  end;
end;

function TtiTimeSeriesChart.IsFormFocused: Boolean;
var
  LForm: TForm;
begin
  result := true;

  // This may AV when used in an ActiveX
  LForm := TForm(GetOwnerForm(self));

  // Added (and assert removed) for ActiveX deployment
  if LForm = nil then
    Exit; //==>
  //Assert(LForm <> nil, 'Owner form not found');

  // This will return the correct answere if
  // a) An MDIChildForm is active, and the application is deactiveate
  // b) An MDIChildForm is active, and another MIDChild form is made active
  // but will not return the correct answer if
  // c) An MIDChildForm is active, and a ModalDialog is activated over it.
  // Non MDIChildForms, non modal dialogs have not been tested.
  case (LForm.FormStyle) of
    fsMDIChild :
    begin
      try
        result := (Application.Active) and (Application.MainForm.ActiveMDIChild = LForm);
      except
        on e: EInvalidOperation do
        begin
          result := False;
          Exit; //==>
        end;
      end;
    end;

    // Form.Active will not work for an MDIForm. Must use Application.Active,
    // but this will return true if one of the child forms is active, but the
    // main form is not.
    //  fsMDIForm  : result := (LForm.Active) and (Application.Active);

    // This is not fool proof yet. The commented out code below will work for a
    // normal, mdi app. But will fail for an "Outlook" style ap where the client
    // forms are contained inside a main form.
    // fsNormal   : result := (LForm.Active);
    // The line below hacks around that problem.
    fsNormal   : result := true;

    fsStayOnTop : result := (LForm.Active);
  else
    raise exception.create('Invalid FormStyle passed to TtiChart.IsFormFocused');
  end;
end;

function TtiTimeSeriesChart.LineSeriesPointerVisible: Boolean;
var
  LPointsVisible: Integer;
begin
  Result := (FData <> nil) and (FChart.SeriesCount > 0);
  if not Result then
    Exit; //==>

  // Ensure visible count is correct if zoom level changed.
  FChart.Update;

  LPointsVisible := PointsVisible;
  Result := (LPointsVisible <> 0) and (LPointsVisible <= cPointerVisibleLimit);
end;

function TtiTimeSeriesChart.PointsVisible: Integer;
var
  i: Integer;
  j: Integer;
begin
  Result := 0;
  for i := 0 to FChart.SeriesCount - 1 do
    // Original solution was to iterate between FChart.Series[i].FirstValueIndex
    // and FChart.Series[i].LastValueIndex if they were not -1 but they aren't
    // updated until late in the chart update process.
    if FChart.Series[i].Visible then
    begin
      // For each point within the charts current X axis range
      for j := 0 to FChart.Series[i].Count - 1 do
      begin
        if (FChart.Series[i].XValue[j] >= FChart.BottomAxis.Minimum) and
           (FChart.Series[i].XValue[j] <= FChart.BottomAxis.Maximum) and
           (FChart.Series[i].YValue[j] >= FChart.LeftAxis.Minimum) and
           (FChart.Series[i].YValue[j] <= FChart.LeftAxis.Maximum) then
          Inc(Result);
      end;
    end;
end;

procedure TtiTimeSeriesChart.Loaded;
begin
  inherited;
  if FShowTestData then
    SetShowTestData(FShowTestData);
  SetScrollStyle(FScrollStyle);
end;

procedure TtiTimeSeriesChart.OnDrawKey(pSeries : TChartSeries; AIndex : integer;
    AData : TObject; AList : TtiObjectList);
begin
  if Assigned(FOnCrossHair) then
    FOnCrossHair(pSeries, AIndex, AData, AList);
end;

procedure TtiTimeSeriesChart.RefreshSeries;
var
  i: Integer;
begin
  for i := 0 to FChart.SeriesCount - 1 do
    FChart.Series[i].RefreshSeries;
  if ShowLegend then
    FChartLegendForm.CreateSeries;
  FNeedRefreshSeries := false;
end;

procedure TtiTimeSeriesChart.RepositionChart;
var
  LXMin, LXMax: Double;
  LYMin, LYMax: Double;
begin
  LXMin := Chart.BottomAxis.Minimum;
  LXMax := Chart.BottomAxis.Maximum;

  LYMin := Chart.LeftAxis.Minimum;
  LYMax := Chart.LeftAxis.Maximum;

  ConstrainChartAxesView(LXMin, LXMax, LYMin, LYMax);

  SetSeriesPointerVisible(LineSeriesPointerVisible);
  AdjustScrollBarPositions;
end;

procedure TtiTimeSeriesChart.ResetMousePositionVisualCues;
begin
  FOldX := -1;
  FOldY := -1;
  FOldCircX := -1;
  FOldCircY := -1;
end;

procedure TtiTimeSeriesChart.ResetZoom;
begin
  // Hide pointers to avoid thousands of them being visible briefly.
  SetSeriesPointerVisible(False);
  // Force display of all series points.
  FChart.BottomAxis.SetMinMax(VisiblesSeriesMinX, VisiblesSeriesMaxX);
  FChart.LeftAxis.SetMinMax(VisiblesSeriesMinY, VisiblesSeriesMaxY);
  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.SelectUserPanel;
begin
  FChartWithLegendPanel.ChartLegendPanel.SelectUserPanel;
end;

function TtiTimeSeriesChart.SeriesByName(const ASeriesName : string):
    TChartSeries;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FChart.SeriesCount - 1 do begin
    if SameText(FChart.Series[i].Name, ASeriesName) then begin
      result := FChart.Series[i];
      Break; //==>
    end;
  end;
end;

function TtiTimeSeriesChart.SeriesByTitle(const ASeriesTitle : string):
    TChartSeries;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FChart.SeriesCount - 1 do begin
    if SameText(FChart.Series[i].Title, ASeriesTitle) then begin
      result := FChart.Series[i];
      Break; //==>
    end;
  end;
end;

function TtiTimeSeriesChart.SeriesNameToTitle(
  const ASeriesName: string): string;
begin
  result := FSeriesList.SeriesNameToTitle(ASeriesName);
end;

function TtiTimeSeriesChart.SeriesTitleToName(const ATitle: string): string;
var
  I: Integer;
begin
  Result := '';

  // Must only contain alphanumeric characters.
  for I := 1 to Length(ATitle) do
    if CharInSet(ATitle[I], ['a'..'z','A'..'Z','0'..'9']) then
      Result := Result + ATitle[I];

  // Must start with non-digit.
  Result := 'cs' + Result;
//  result := 'cs' + StringReplace(ATitle, ' ', '', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TtiTimeSeriesChart.SetChartColor(const AValue: TColor);
begin
  FChart.Color := AValue;
  Color       := AValue;
end;

procedure TtiTimeSeriesChart.SetChartPopupMenu(const AValue: TPopupMenu);
begin
  FChart.PopupMenu := AValue;
end;

procedure TtiTimeSeriesChart.SetConstrainViewToData(const Value: Boolean);
var
  LCurrentAxisRange: Double;
  LMaxAxisRange: Double;
begin
  if FConstrainViewToData <> Value then
  begin
    FConstrainViewToData := Value;
    if FConstrainViewToData then
    begin
      // NOTE: The order of the following statements is important.
      // Keep horizontal view within data.
      LCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum;
      LMaxAxisRange := VisiblesSeriesMaxY - VisiblesSeriesMinY;
      if LMaxAxisRange > 0 then
      begin
        if LCurrentAxisRange > LMaxAxisRange then
          LCurrentAxisRange := LMaxAxisRange;
        if FChart.LeftAxis.Minimum < VisiblesSeriesMinY then
        begin
          FChart.LeftAxis.Minimum := VisiblesSeriesMinY;
          FChart.LeftAxis.Maximum := FChart.LeftAxis.Minimum + LCurrentAxisRange;
        end;
        if FChart.LeftAxis.Maximum > VisiblesSeriesMaxY then
        begin
          FChart.LeftAxis.Maximum := VisiblesSeriesMaxY;
          FChart.LeftAxis.Minimum := FChart.LeftAxis.Maximum - LCurrentAxisRange;
        end;
      end;

      // Keep horizontal view within data.
      LCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
      LMaxAxisRange := VisiblesSeriesMaxX - VisiblesSeriesMinX;
      if LMaxAxisRange > 0 then
      begin
        if LCurrentAxisRange > LMaxAxisRange then
          LCurrentAxisRange := LMaxAxisRange;
        if FChart.BottomAxis.Minimum < VisiblesSeriesMinX then
        begin
          FChart.BottomAxis.Minimum := VisiblesSeriesMinX;
          FChart.BottomAxis.Maximum := FChart.BottomAxis.Minimum + LCurrentAxisRange;
        end;
        if FChart.BottomAxis.Maximum > VisiblesSeriesMaxX then
        begin
          FChart.BottomAxis.Maximum := VisiblesSeriesMaxX;
          FChart.BottomAxis.Minimum := FChart.BottomAxis.Maximum - LCurrentAxisRange;
        end;
      end;
    end;
  end;
end;

procedure TtiTimeSeriesChart.SetData(const AValue: TtiObjectList);
begin
  if not Assigned(AValue) then
    ClearSeries;

  FData := AValue;
//  FChartLegendForm.SetData(FData);
  DoDrawChart(True {AZoomOut});

end;

procedure TtiTimeSeriesChart.DoDrawChart(const AZoomOut: Boolean);
var
  i: Integer;
  LChartSeries: TtiChartSeries;
begin
  if not Assigned(FData) then
    Exit; //==>

  ClearSeriesValues;

  DrawCrossHairsNow := false;
  try
    try
      if Assigned(FOnAssignGraphData) then
        for i := 0 to FData.Count - 1 do
        begin
            // Insert a gap if necessary so that the line is broken.
          if Assigned(FOnChartDataGap) and (i > 0) then
            FOnChartDataGap(TObject(FData.Items[i - 1]), TObject(FData.Items[i]), Self);

          FOnAssignGraphData(TObject(FData.Items[i]),
                              Self);
        end;
      if Assigned(FOnAssignSeriesData) or Assigned(FOnSeriesDataGap) then
        for i := 0 to FSeriesList.Count - 1 do
        begin
          LChartSeries := FSeriesList.Items[i];
          if LChartSeries.ChartSeries.Visible and (not LChartSeries.DataAssigned) then
            GetSeriesData(LChartSeries);
        end;
    except
      on e:exception do
        raise exception.create('Error in TtiChart.DoDrawChart ' +
                                'Message: ' + e.message);
    end;

    // Call RefreshSeries for all series
    // This is necessary for moving average series
    if FNeedRefreshSeries then
      RefreshSeries;

    // Zoom right out when showing new series.
    if AZoomOut then
      ResetZoom
    else begin
      // Hide pointers to avoid too many being visible briefly.
      SetSeriesPointerVisible(False);
      RepositionChart;
    end;
  finally
    DrawCrossHairsNow := true;
  end;
end;

procedure TtiTimeSeriesChart.RedrawChart;
begin
  DoDrawChart(not Zoomed);
end;

procedure TtiTimeSeriesChart.GetSeriesData(const AChartSeries: TtiChartSeries);
var
  i: integer;
begin
  Assert(AChartSeries.TestValid(TtiChartSeries), CTIErrorInvalidObject);
  Assert(not AChartSeries.DataAssigned, Format('Data for series %s already assigned',
      [AChartSeries.ChartSeries.Name]));
  for i := 0 to FData.Count - 1 do
  begin
    if Assigned(FOnSeriesDataGap) and (i > 0) then
      FOnSeriesDataGap(FData.Items[i - 1], FData.Items[i], Self, AChartSeries);
    if Assigned(FOnAssignSeriesData) then
      FOnAssignSeriesData(FData.Items[i], Self, AChartSeries);
  end;
  AChartSeries.DataAssigned := true;
end;

procedure TtiTimeSeriesChart.SetDataPointHintText(const AValue: string);
begin
  DataPointHintForm.Content := AValue;
end;

procedure TtiTimeSeriesChart.SetDrawCrossHairs(const AValue: Boolean);
begin
  FDrawCrossHairs := AValue;
  Screen.Cursor := crDefault;
  DrawCrossHairsNow := AValue;
end;

procedure TtiTimeSeriesChart.SetDrawCrossHairsNow(const AValue: Boolean);
begin
  if FDrawCrossHairsNow = AValue then
    Exit; //==>

  FDrawCrossHairsNow := AValue;

  if FDrawCrossHairsNow then begin
    OnMouseMove := DoChartMouseMove;
    // These two lines will cause the XHairs to be drawn when Alt+Tab back onto
    // the app, but will cause XHair litter when mousing over the chart region
    // for the first time. Requires more work...
    //MouseToChartCoOrds(liX, liY);
    //DoDrawCrossHairs( liX, liY, true);
  end else begin
    OnMouseMove := nil;
    ClearMousePositionVisualCues;
  end;
end;

procedure TtiTimeSeriesChart.SetIncludeLegendInClipboard(const Value: boolean);
begin
  FIncludeLegendInClipboard := Value;
end;

//procedure TtiTimeSeriesChart.SetLayeredAttribs;
////const
////  cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
////  cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
//var
//  AStyle: Integer;
//begin
////    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
////    SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
////  if not (csDesigning in ComponentState) and
////    (Assigned(SetLayeredWindowAttributes)) and HandleAllocated then
////  begin
////    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
////    if FAlphaBlend or FTransparentColor then
////    begin
////      if (AStyle and WS_EX_LAYERED) = 0 then
////        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
////      SetLayeredWindowAttributes(Handle, FTransparentColorValue, FAlphaBlendValue,
////        cUseAlpha[FAlphaBlend] or cUseColorKey[FTransparentColor]);
////    end
////    else
////    begin
////      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
////      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
////    end;
////  end;
//end;

procedure TtiTimeSeriesChart.SetOnDblClickChart(const AValue: TNotifyEvent);
begin
  FChart.OnDblClick := AValue;
end;

procedure TtiTimeSeriesChart.SetScrollStyle(const AValue: TScrollStyle);
begin
  FScrollStyle := AValue;
  if not Zoomed then
    FChartPanel.ScrollBars := ssNone
  else
    FChartPanel.ScrollBars := FScrollStyle;
end;

procedure TtiTimeSeriesChart.SetSeriesPointerVisible(const AValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to FChart.SeriesCount - 1 do
    if (FChart.Series[i] is TLineSeries) then
      TLineSeries(FChart.Series[i]).Pointer.Visible :=
        AValue and TLineSeries(FChart.Series[i]).Active;
end;

procedure TtiTimeSeriesChart.SetSeriesVisibleByCaption(
  const ASeriesTitle: string; const AVisible: Boolean);
begin
  FChartLegendForm.SetSeriesVisibleByCaption(ASeriesTitle, AVisible);
end;

function TtiTimeSeriesChart.IsSeriesVisible(const ASeriesTitle: string): boolean;
begin
  Result := FChartLegendForm.IsSeriesVisibleByCaption(ASeriesTitle);
end;

function TtiTimeSeriesChart.GetShowButtons: Boolean;
begin
  Result := FButtonsPanel.Visible;
end;

procedure TtiTimeSeriesChart.SetShowButtons(const AValue: Boolean);
begin
  FButtonsPanel.Visible := AValue;
end;

function TtiTimeSeriesChart.GetShowLegend: Boolean;
begin
  Result := FChartWithLegendPanel.ShowLegend;
end;

function TtiTimeSeriesChart.GetUserPanel: TtiChartUserPanel;
begin
  Result := FChartWithLegendPanel.ChartLegendPanel.UserPanel;
end;

procedure TtiTimeSeriesChart.SetShowLegend(const AValue: Boolean);
begin
  FChartWithLegendPanel.ShowLegend := AValue;
end;

procedure TtiTimeSeriesChart.SetShowTestData(const AValue: Boolean);
begin
  FShowTestData := AValue;
  if FShowTestData then
  begin
    if FTestData = nil then
      FTestData := TtiChartTestData.Create;
    OnAssignGraphData := FTestData.AssignGraphData;
    OnChartDataGap := FTestData.DataGap;
    AddLineSeries('Sin', true {AVisible}, nil {ASeriesSource});
    AddLineSeries('Cos', true {AVisible}, nil {ASeriesSource});
    Data := FTestData;
  end
  else
  begin
    OnAssignGraphData := nil;
    FData := nil;
    Clear;
  end;
end;

procedure TtiTimeSeriesChart.SetSnapToDataSize(const AValue: Integer);
begin
  FSnapToDataSize := AValue;
  FSnapToDataRadius := FSnapToDataSize div 2;
end;

procedure TtiTimeSeriesChart.SetTimeSeriesChart(const AValue: Boolean);
begin
  if (not FTimeSeriesChart) and
     (AValue) then begin
    FTimeSeriesChart := true;
    Exit; //==>
  end;

  FTimeSeriesChart := AValue;
  //  FChart.BottomAxis.RoundFirstLabel := true;
  //  FChart.BottomAxis.ExactDateTime  := true;
  // FChart.BottomAxis.LabelsOnAxis := true;
  // FChart.BottomAxis.TickOnLabelsOnly := true;
end;

procedure TtiTimeSeriesChart.SetView3D(const AValue: Boolean);
begin
  FChart.View3D := AValue;
end;

procedure TtiTimeSeriesChart.SetVisibleSeriesAsString(const AValue: string);
var
  i: Integer;
  LStringList: TStringList;
  LSeries: TLineSeries;
begin
  LStringList:= TStringList.Create;
  try
    LStringList.CommaText:= AValue;
    for i:= 0 to FChart.SeriesCount - 1 do
    begin
      LSeries:= FChart.Series[i] as TLineSeries;
      FChartLegendForm.SeriesVisible[LSeries.Name] := LStringList.Values[LSeries.Name] = '1';
    end;
  finally
    LStringList.Free;
  end;
end;

procedure TtiTimeSeriesChart.ShowDataPointHint;
const
  CHintSpacing = 10;
var
  LPoint: TPoint;
begin

  try
    LPoint := ScreenToClient(Mouse.CursorPos);
  except
    // Ignore unable to get cursor pos (such as when screensaver is active
    // or workstation is locked)
    on EOSError do
      Exit; //==>
  end;

  if (FDataPointHintForm.Left <> 0) and
    (LPoint.X + FDataPointHintForm.Width > Left + Width) then
    FDataPointHintForm.Left := LPoint.X - FDataPointHintForm.Width - CHintSpacing
  else
    FDataPointHintForm.Left := LPoint.X + CHintSpacing;

  if (FDataPointHintForm.Top <> 0) and
    (LPoint.Y + FDataPointHintForm.Height > Top + Height) then
    FDataPointHintForm.Top := LPoint.Y - FDataPointHintForm.Height - CHintSpacing
  else
    FDataPointHintForm.Top := LPoint.Y + CHintSpacing;

  FDataPointHintForm.Show;
end;

procedure TtiTimeSeriesChart.ShowSeries(const ASeries: TChartSeries;
  const AVisible: Boolean);
var
  LChartSeries: TtiChartSeries;
  LZoomed: Boolean;
begin
  // Get series data the first time that it is shown.
  if AVisible then
  begin
    LChartSeries := FSeriesList.FindByChartSeries(ASeries);
    Assert(LChartSeries.TestValid(TtiChartSeries), CTIErrorInvalidObject);
    if not LChartSeries.DataAssigned then
      GetSeriesData(LChartSeries);
  end;

  // Save zoom state. Showing a series can change this.
  LZoomed := Zoomed;
  ASeries.Active := AVisible;

  // Note: Must do this before ResetZoom to allow the form to respond to
  // OnVisibleSeriesChange before OnRangeChange
  if Assigned(FOnVisibleSeriesChange) then
    FOnVisibleSeriesChange(ASeries.Name, AVisible);

  if (not LZoomed) and AVisible then
    ResetZoom
  else
    RepositionChart;
end;

//procedure TtiTimeSeriesChart.SnapEditDialogToButton(pForm: TForm; pSender:
//    TObject);
//var
//  lSB: TControl;
//  lPoint: TPoint;
//begin
//  Assert(pSender is TControl, 'Sender not a TButton');
//  lSB := TControl(pSender);
//  lPoint.x := FChart.Left + FChart.LeftAxis.PosAxis - FChart.LeftAxis.MaxLabelsWidth - pForm.Width;
//  lPoint.y := lSB.Top;
//  lPoint := lSB.Parent.ClientToScreen(lPoint);
//  pForm.Top := lPoint.Y;
//  pForm.Left := lPoint.X;
//end;

procedure TtiTimeSeriesChart.Zoom(const AZoomPercent: double);
var
  LZoomFactor: Double;
  LCurrentAxisRange: Double;
  LXAxisChange: Double;
  LYAxisChange: Double;
  LXMin, LXMax: Double;
  LYMin, LYMax: Double;
begin
  if AZoomPercent <= -100.0 then
    raise exception.create('Invalid zoom percent passed to TtiChart.Zoom');

  if AZoomPercent >= 0.0 then
    LZoomFactor := ((100.0 + AZoomPercent) / 100.0) - 1.0
  else
    LZoomFactor := 1.0 - (100.0 / (100.0 + AZoomPercent));

  LCurrentAxisRange := Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum;
  LXAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;
  LCurrentAxisRange := Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum;
  LYAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;

  LXMin := Chart.BottomAxis.Minimum + LXAxisChange;
  LXMax := Chart.BottomAxis.Maximum - LXAxisChange;

  LYMin := Chart.LeftAxis.Minimum + LYAxisChange;
  LYMax := Chart.LeftAxis.Maximum - LYAxisChange;

  ConstrainChartAxesView(LXMin, LXMax, LYMin, LYMax);

  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.ConstrainChartAxesView(const AXAxisMin, AXAxisMax, AYAxisMin, AYAxisMax: Double);
var
  LMin: Double;
  LMax: Double;
begin
  LMin := AXAxisMin;
  LMax := AXAxisMax;
  if LMin = LMax then
  begin
    LMin := VisiblesSeriesMinX;
    LMax := VisiblesSeriesMaxX;
  end else begin
    if FConstrainViewToData and (CompareValue(LMin, VisiblesSeriesMinX, CAxisEpsilon) < 0) then
      LMin := VisiblesSeriesMinX;
    if FConstrainViewToData and (CompareValue(LMax, VisiblesSeriesMaxX, CAxisEpsilon) > 0) then
      LMax := VisiblesSeriesMaxX;
  end;

  if (not SameValue(LMin, Chart.BottomAxis.Minimum, CAxisEpsilon)) or
     (not SameValue(LMax, Chart.BottomAxis.Maximum, CAxisEpsilon)) then
    Chart.BottomAxis.SetMinMax(LMin, LMax);

  LMin := AYAxisMin;
  LMax := AYAxisMax;
  if LMin = LMax then
  begin
    LMin := VisiblesSeriesMinY;
    LMax := VisiblesSeriesMaxY;
  end else begin
    if FConstrainViewToData and (CompareValue(LMin, VisiblesSeriesMinY, CAxisEpsilon) < 0) then
      LMin := VisiblesSeriesMinY;
    if FConstrainViewToData and (CompareValue(LMax, VisiblesSeriesMaxY, CAxisEpsilon) > 0) then
      LMax := VisiblesSeriesMaxY;
  end;
  if (not SameValue(LMin, Chart.LeftAxis.Minimum, CAxisEpsilon)) or
     (not SameValue(LMax, Chart.LeftAxis.Maximum, CAxisEpsilon)) then
    Chart.LeftAxis.SetMinMax(LMin, LMax);
end;

procedure TtiTimeSeriesChart.SetAxesRange(const AXAxisMin, AXAxisMax,
  AYAxisMin, AYAxisMax: Double);
begin
  ConstrainChartAxesView(AXAxisMin, AXAxisMax, AYAxisMin, AYAxisMax);
  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.RangeChange;
var
  LBottomAxisMin: TDateTime;
  LBottomAxisMax: TDateTime;
  LLeftAxisMin: Double;
  LLeftAxisMax: Double;
begin
  if Assigned(FOnRangeChange) then
  begin
    LBottomAxisMin := FChart.BottomAxis.Minimum;
    LBottomAxisMax := FChart.BottomAxis.Maximum;
    LLeftAxisMin := FChart.LeftAxis.Minimum;
    LLeftAxisMax := FChart.LeftAxis.Maximum;
    FOnRangeChange(Self, Zoomed, LBottomAxisMin, LBottomAxisMax, LLeftAxisMin,
        LLeftAxisMax);
  end;
end;

{ TtiChartButtonsList }

procedure TtiChartButtonDetailList.Add(const AObject: TtiChartButtonDetail);
begin
  inherited Add(AObject);
end;

procedure TtiChartButtonDetailList.AddButton(const AButtonHint: string;
    const AImageResName: string; const ADoButtonClick: TNotifyEvent;
    const AButtonOrder: Integer);
var
  LButton: TtiChartButtonDetail;
begin
  LButton := TtiChartButtonDetail.Create(AButtonHint, AImageResName, ADoButtonClick);
  if AButtonOrder = -1 then
    Add(LButton)
  else
    Insert(AButtonOrder, LButton);
end;

constructor TtiChartButtonDetailList.Create;
begin
  inherited;
  FButtonComponentList := TObjectList.Create;
  FButtonComponentList.OwnsObjects := true;
end;

procedure TtiChartButtonDetailList.CreateButtons(const AParent: TtiChartButtonsPanel; const AChartButtonsPosition: TtiChartButtonsPosition);
var
  LButton: TtiSpeedButton;
  i: Integer;
  LSBLeft: Integer;
  LSBTop: Integer;
begin
  LSBTop  := CSBTop;
  LSBLeft := CSBLeft;

  FButtonComponentList.Clear;

  for i := 0 to Count - 1 do
  begin
    if i > 0 then //position first button in top corner and subsequent buttons either vertically or horizontally
      case AChartButtonsPosition of
        cbpLeft: LSBTop  := LSBTop  + CSBLeft + CSBSize;
        cbpTop:  LSBLeft := LSBLeft + CSBLeft + CSBSize;
      end;

    LButton := Items[i].CreateButton(AParent, LSBLeft, LSBTop);
    FButtonComponentList.Add(LButton);
  end;
end;

destructor TtiChartButtonDetailList.Destroy;
begin
  FButtonComponentList.Clear;
  FButtonComponentList.Free;
  inherited;
end;

function TtiChartButtonDetailList.GetItems(i: integer): TtiChartButtonDetail;
begin
  result := inherited GetItems(i) as TtiChartButtonDetail;
end;

procedure TtiChartButtonDetailList.Insert(const AIndex: integer;
  const AObject: TtiChartButtonDetail);
begin
  inherited Insert(AIndex, AObject);
end;

procedure TtiChartButtonDetailList.SetItems(i: integer;
  const AValue: TtiChartButtonDetail);
begin
  inherited SetItems(i, AValue);
end;

{ TtiChartButtonDetail }

constructor TtiChartButtonDetail.Create(const AHint: string;
    const AImageResName: string; const ADoButtonClick: TNotifyEvent);
begin
  inherited Create;
  FHint := AHint;
  FImageResName := AImageResName;
  FDoButtonClick := ADoButtonClick;
end;

function TtiChartButtonDetail.CreateButton(const AParent: TtiChartButtonsPanel;
  const APosLeft, APosTop: Integer): TtiSpeedButton;
var
  LButton: TtiSpeedButton;
begin
  LButton := TtiSpeedButton.Create(AParent);
  LButton.Parent   := AParent;
  LButton.Name     := tiGetUniqueComponentNameFromParent(AParent,
      'btn' + tiToComponentName(FHint));
  LButton.Top      := APosTop;
  LButton.Left     := APosLeft;
  LButton.Height   := CSBSize;
  LButton.Width    := CSBSize;
  LButton.Flat     := true;
  gTIImageListMgr.LoadBMPToTISPeedButton16(FImageResName, LButton);
  LButton.Hint     := FHint;
  LButton.ShowHint := true;
  LButton.OnClick  := FDoButtonClick;
  Result := LButton;
end;

function TtiChartButtonDetail.GetParent: TtiChartButtonDetailList;
begin
  Assert(inherited GetParent.TestValid(TtiChartButtonDetailList), CTIErrorInvalidObject);
  result := inherited GetParent as TtiChartButtonDetailList;
end;

{ TtiChartManualZoomForm }

constructor TtiChartManualZoomForm.Create(AOwner: TComponent);
begin
  Assert(false, 'Should create instance with CreateNew constructor when creating a form with no associated dfm file');
  inherited;
end;

constructor TtiChartManualZoomForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
  Height := 156;
  Width := 322;
  BorderIcons := [];
  BorderStyle := bsNone;
  Position    := poDesigned;
  OnDeActivate := DoCancel;

  FgbMinimum := TGroupBox.Create(self);
  FgbMinimum.Parent := self;
  FgbMinimum.Name := tiGetUniqueComponentNameFromParent(self, 'gbMinimum');
  FgbMinimum.Left    := 8;
  FgbMinimum.Top     := 8;
  FgbMinimum.Width   := 150;
  FgbMinimum.Height  := 110;
  FgbMinimum.Caption  := 'Minimum';
  FgbMinimum.TabOrder := 0;

  FdedtMinDate := TtiPerAwareDateEdit.Create(self);
  FdedtMinDate.Parent := FgbMinimum;
  FdedtMinDate.Name := tiGetUniqueComponentNameFromParent(FgbMinimum, 'dedtMinDate');
  FdedtMinDate.Left   := 8;
  FdedtMinDate.Top    := 16;
  FdedtMinDate.Width  := 130;
  FdedtMinDate.Height := 24;
  FdedtMinDate.TabOrder   := 0;
  FdedtMinDate.Caption    := 'Date';
  FdedtMinDate.LabelWidth := 30;

  FtedeMinTime := TtiPerAwareTimeEdit.Create(self);
  FtedeMinTime.Parent := FgbMinimum;
  FtedeMinTime.Name := tiGetUniqueComponentNameFromParent(FgbMinimum, 'tedeMinTime');
  FtedeMinTime.Left   := 8;
  FtedeMinTime.Top    := 42;
  FtedeMinTime.Width  := 130;
  FtedeMinTime.Height := 24;
  FtedeMinTime.TabOrder     := 1;
  FtedeMinTime.Caption      := 'Time';
  FtedeMinTime.LabelWidth   := 30;
  FtedeMinTime.FormatString := 'hh:mm:ss.zzz';

  FfedtMinYAxis := TtiPerAwareFloatEdit.Create(self);
  FfedtMinYAxis.Parent := FgbMinimum;
  FfedtMinYAxis.Name := tiGetUniqueComponentNameFromParent(FgbMinimum, 'fedtMinYAxis');
  FfedtMinYAxis.Left   := 8;
  FfedtMinYAxis.Top    := 76;
  FfedtMinYAxis.Width  := 130;
  FfedtMinYAxis.Height := 24;
  FfedtMinYAxis.TabOrder      := 2;
  FfedtMinYAxis.Caption       := 'Y Axis';
  FfedtMinYAxis.LabelWidth    := 40;
  FfedtMinYAxis.ValueAsString := '0';
  FfedtMinYAxis.Precision     := 5;
  FfedtMinYAxis.UnknownValue  := -1.000000000000000000;
  FfedtMinYAxis.IsKnown       := True;
  FfedtMinYAxis.Style         := fesUser;

  FgbMaximum := TGroupBox.Create(self);
  FgbMaximum.Parent := self;
  FgbMaximum.Name := tiGetUniqueComponentNameFromParent(self, 'gbMaximum');
  FgbMaximum.Left   := 164;
  FgbMaximum.Top    := 8;
  FgbMaximum.Width  := 150;
  FgbMaximum.Height := 110;
  FgbMaximum.Caption  := 'Maximum';
  FgbMaximum.TabOrder := 1;

  FdedtMaxDate := TtiPerAwareDateEdit.Create(self);
  FdedtMaxDate.Parent := FgbMaximum;
  FdedtMaxDate.Name := tiGetUniqueComponentNameFromParent(FgbMaximum, 'dedtMaxDate');
  FdedtMaxDate.Left   := 8;
  FdedtMaxDate.Top    := 16;
  FdedtMaxDate.Width  := 130;
  FdedtMaxDate.Height := 24;
  FdedtMaxDate.TabOrder   := 0;
  FdedtMaxDate.Caption    := 'Date';
  FdedtMaxDate.LabelWidth := 30;

  FtedtMaxTime := TtiPerAwareTimeEdit.Create(self);
  FtedtMaxTime.Parent := FgbMaximum;
  FtedtMaxTime.Name := tiGetUniqueComponentNameFromParent(FgbMaximum, 'tedtMaxTime');
  FtedtMaxTime.Left   := 8;
  FtedtMaxTime.Top    := 42;
  FtedtMaxTime.Width  := 130;
  FtedtMaxTime.Height := 24;
  FtedtMaxTime.TabOrder     := 1;
  FtedtMaxTime.Caption      := 'Time';
  FtedtMaxTime.LabelWidth   := 30;
  FtedtMaxTime.FormatString := 'hh:mm:ss.zzz';

  FfedtMaxYAxis := TtiPerAwareFloatEdit.Create(self);
  FfedtMaxYAxis.Parent := FgbMaximum;
  FfedtMaxYAxis.Name := tiGetUniqueComponentNameFromParent(FgbMaximum, 'fedtMaxYAxis');
  FfedtMaxYAxis.Left   := 8;
  FfedtMaxYAxis.Top    := 76;
  FfedtMaxYAxis.Width  := 130;
  FfedtMaxYAxis.Height := 24;
  FfedtMaxYAxis.TabOrder      := 2;
  FfedtMaxYAxis.Caption       := 'Y Axis';
  FfedtMaxYAxis.LabelWidth    := 40;
  FfedtMaxYAxis.ValueAsString := '0';
  FfedtMaxYAxis.Precision     := 5;
  FfedtMaxYAxis.UnknownValue  := -1.000000000000000000;
  FfedtMaxYAxis.IsKnown       := True;
  FfedtMaxYAxis.Style         := fesUser;

  FbtnOK := TtiSpeedButton.Create(self);
  FbtnOK.Parent := self;
  FbtnOK.Name := tiGetUniqueComponentNameFromParent(self, 'btnOK');
  FbtnOK.Left   := 164;
  FbtnOK.Top    := 124;
  FbtnOK.Width  := 58;
  FbtnOK.Height := 24;
  FbtnOK.Cursor   := crHandPoint;
  FbtnOK.Caption  := 'OK';
  FbtnOK.Flat     := True;
  FbtnOK.ImageRes := tiRINone;
  FbtnOK.OnClick := DoOK;

  FbtnCancel := TtiSpeedButton.Create(self);
  FbtnCancel.Parent := self;
  FbtnCancel.Name := tiGetUniqueComponentNameFromParent(self, 'btnCancel');
  FbtnCancel.Left   := 232;
  FbtnCancel.Top    := 124;
  FbtnCancel.Width  := 82;
  FbtnCancel.Height := 24;
  FbtnCancel.Cursor   := crHandPoint;
  FbtnCancel.Caption  := 'Cancel';
  FbtnCancel.Flat     := True;
  FbtnCancel.ImageRes := tiRINone;
  FbtnCancel.OnClick := DoCancel;
end;

destructor TtiChartManualZoomForm.Destroy;
begin
  FreeAndNil(FdedtMinDate);
  FreeAndNil(FtedeMinTime);
  FreeAndNil(FfedtMinYAxis);
  FreeAndNil(FgbMinimum);

  FreeAndNil(FdedtMaxDate);
  FreeAndNil(FtedtMaxTime);
  FreeAndNil(FfedtMaxYAxis);
  FreeAndNil(FgbMaximum);

  FreeAndNil(FbtnOK);
  FreeAndNil(FbtnCancel);
  inherited;
end;

procedure TtiChartManualZoomForm.DoCancel(Sender: TObject);
begin
  Close;
end;

procedure TtiChartManualZoomForm.DoOK(Sender: TObject);
var
  LXMin, LXMax: Double;
  LYMin, LYMax: Double;
begin
  LXMin := DateOf(StrToDate(FdedtMinDate.Value)) + TimeOf(FtedeMinTime.ValueAsTime);
  LXMax := DateOf(StrToDate(FdedtMaxDate.Value)) + TimeOf(FtedtMaxTime.ValueAsTime);
  LYMin := FfedtMinYAxis.Value;
  LYMax := FfedtMaxYAxis.Value;

  FChart.ConstrainChartAxesView(LXMin, LXMax, LYMin, LYMax);

  FChart.RepositionChart;
  FChart.RangeChange;
  Close;
end;

procedure TtiChartManualZoomForm.Execute(const AChart: TtiTimeSeriesChart; const AManualZoomButton: TtiSpeedButton);
var
  LPoint: TPoint;
begin
  Assert(Assigned(AChart));
  Assert(Assigned(AManualZoomButton));

  LPoint.Y := AManualZoomButton.Top + AManualZoomButton.Height + 4;
  LPoint.X := AManualZoomButton.Left;
  LPoint := AManualZoomButton.Parent.ClientToScreen(LPoint);

  Top := LPoint.Y;
  Left := LPoint.X;

  Chart := AChart;

  FdedtMinDate.ValueAsDate := DateOf(FChart.Chart.BottomAxis.Minimum);
  FtedeMinTime.ValueAsTime := TimeOf(FChart.Chart.BottomAxis.Minimum);
  FdedtMaxDate.ValueAsDate := DateOf(FChart.Chart.BottomAxis.Maximum);
  FtedtMaxTime.ValueAsTime := TimeOf(FChart.Chart.BottomAxis.Maximum);

  FfedtMinYAxis.Value := FChart.Chart.LeftAxis.Minimum;
  FfedtMaxYAxis.Value := FChart.Chart.LeftAxis.Maximum;

  Show;

end;

procedure TtiChartManualZoomForm.Paint;
var
  lColor: TColor;
begin
  inherited;
  lColor := Canvas.Pen.Color;
  try
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  finally
    Canvas.Pen.Color := lColor;
  end;
end;

{ TtiChartLegendFormAbs }

constructor TtiChartLegendFormAbs.CreateNew(const AChart: TtiTimeSeriesChart);
begin
  inherited CreateNew(Owner);
//  Parent := Owner as TWinControl;
  BorderStyle := bsNone;
  DragKind  :=	dkDrag;
  DragMode := dmManual;
  FChart := AChart;
  Color := clWhite;
  Width := 0;
  Visible:= True;
  self.AutoScroll := true;
  self.VertScrollBar.Tracking := true;

  FpmiSelectAll := TMenuItem.Create(self);
  FpmiSelectAll.Caption := 'Select all';
  FpmiSelectAll.OnClick := DoSelectAll;
  FpmiSelectNone := TMenuItem.Create(self);
  FpmiSelectNone.Caption := 'Select none';
  FpmiSelectNone.OnClick := DoSelectNone;

  FPopupMenu := TPopupMenu.Create(self);
  with FPopupMenu do
  begin
    FPopupMenu.Items.Add(FpmiSelectAll);
    FPopupMenu.Items.Add(FpmiSelectNone);
  end;
  PopupMenu := FPopupMenu;
end;

procedure TtiChartLegendFormAbs.CreateSeries;
begin
  if Assigned(FOnCreateSeries) then
    FOnCreateSeries(self);
end;

destructor TtiChartLegendFormAbs.Destroy;
begin
  FpmiSelectNone.Free;
  FpmiSelectAll.Free;
  FPopupMenu.Free;
  inherited;
end;

procedure TtiChartLegendFormAbs.SetOnCreateSeries(
  const AEventHandler: TtiChartLegendFormAbsEvent);
begin
  FOnCreateSeries := AEventHandler;
end;

{ TtiChartLegendTreeViewForm }

function TtiChartLegendTreeViewForm.AddDataMapping(
  const AClass: TtiClass): TtiVTTVDataMapping;
begin
  Result := FTreeView.DataMappings.Add;
  Result.DataClass := AClass;
  // Override these defaults in returned mapping as needed...
  Result.ImageIndex := -1; // No image
  Result.UseCheckBox := true; // Show check box
  // We can use dot notation to acces subelement properties
  // - all properties ref'd in DisplayPropName string must be published
  Result.DisplayPropName := 'Caption' ;
end;

procedure TtiChartLegendTreeViewForm.ClearSeries;
begin
  FTreeView.Data := nil;
end;

constructor TtiChartLegendTreeViewForm.CreateNew(
  const AChart: TtiTimeSeriesChart);
begin
  inherited CreateNew(AChart);
  FTreeView := TtiVTTreeView.Create(self);
  FTreeView.Parent      := self as TWinControl;
  FTreeView.Name := tiGetUniqueComponentNameFromParent(FTreeView.Parent, 'TreeView');
  FTreeView.Left        := 0;
  FTreeView.Top         := 0;
  FTreeView.Align       := alClient;
  FTreeView.DefaultText := '';
  FTreeView.ButtonStyle := lvbsNoButtons;
  FTreeView.OnSelectNode := VTTVSelectNode;
  FTreeView.OnNodeCheckboxClick := VTTVNodeCheckboxClick;
end;


procedure TtiChartLegendTreeViewForm.VTTVNodeCheckboxClick(AtiVTTreeView: TtiVTTreeView;
  ANode: PVirtualNode; AData: TtiObject; ASetChecked: Boolean);
var
  LNodeData: TtiObject;
  LChartLegendItemData: ItiChartLegendItemData;
  LSavedCursor: TCursor;
begin
  // Respond to click on the legend - set related chartseries
  Assert(FChart <> nil, 'FChart not assigned');
  Assert(ANode <> nil, 'ANode not assigned');
  LNodeData := FTreeView.GetObjectFromNode(ANode);
  Assert(LNodeData.TestValid(TtiObject), CTIErrorInvalidObject);
  if Supports(LNodeData, ItiChartLegendItemData, LChartLegendItemData) then
  begin
    LSavedCursor := FChart.Cursor;
    try
      FChart.Cursor := crHourGlass;
      FChart.ShowSeries(LChartLegendItemData.GetChartSeries(FChart), ASetChecked);
    finally
      FChart.Cursor := LSavedCursor;
    end;
  end;
end;

procedure TtiChartLegendTreeViewForm.SelectAll(const ASelected: boolean);
var
  LNode: PVirtualNode;
  LNodeData: TtiObject;
  LChartLegendItemData: ItiChartLegendItemData;
begin
  LNode := FTreeView.VT.GetFirst;
  while Assigned(LNode) do
  begin
    LNodeData := FTreeView.GetObjectFromNode(LNode);
    Assert(LNodeData.TestValid(TtiObject), CTIErrorInvalidObject);
    // only select series nodes in treeview
    if Supports(LNodeData, ItiChartLegendItemData, LChartLegendItemData) then
      if ASelected then
        FTreeView.VT.CheckState[LNode] := csCheckedNormal
      else
        FTreeView.VT.CheckState[LNode] := csUncheckedNormal;
    LNode := FTreeView.VT.GetNext(LNode);
  end;
end;

procedure TtiChartLegendTreeViewForm.DoSelectAll(Sender: TObject);
begin
  SelectAll(true);
end;

procedure TtiChartLegendTreeViewForm.DoSelectNone(Sender: TObject);
begin
  SelectAll(false);
end;

function TtiChartLegendTreeViewForm.SameSeriesName(
  const AChartLegendItemData: ItiChartLegendItemData; const ASeriesName: string): boolean;
begin
  Result := SameText(AChartLegendItemData.GetChartSeries(Chart).Name, ASeriesName);
end;

function TtiChartLegendTreeViewForm.SameSeriesTitle(
  const AChartLegendItemData: ItiChartLegendItemData; const ASeriesTitle: string): boolean;
begin
  Result := SameText(AChartLegendItemData.GetChartSeries(Chart).Title, ASeriesTitle);
end;

function TtiChartLegendTreeViewForm.FindNodeBySeriesProperty(
  const ACompare: TtiChartSeriesMatchByText; const APropertyValue: string;
  out ANode: PVirtualNode): boolean;
var
  LNode: PVirtualNode;
  LNodeData: TtiObject;
  LChartLegendItemData: ItiChartLegendItemData;
begin
  Result := false;
  LNode := FTreeView.VT.GetFirst;
  while Assigned(LNode) do
  begin
    LNodeData := FTreeView.GetObjectFromNode(LNode);
    Assert(LNodeData.TestValid(TtiObject), CTIErrorInvalidObject);
    if Supports(LNodeData, ItiChartLegendItemData, LChartLegendItemData) and
      ACompare(LChartLegendItemData, APropertyValue) then
    begin
      ANode := LNode;
      Result := true;
      break;
    end else
      LNode := FTreeView.VT.GetNext(LNode);
  end;
end;

function TtiChartLegendTreeViewForm.FindNodeBySeriesName(
  const ASeriesName: string; out ANode: PVirtualNode): boolean;
begin
  Result := FindNodeBySeriesProperty(SameSeriesName, ASeriesName, ANode);
end;

function TtiChartLegendTreeViewForm.FindNodeBySeriesTitle(
  const ASeriesTitle: string;  out ANode: PVirtualNode): boolean;
begin
  Result := FindNodeBySeriesProperty(SameSeriesTitle, ASeriesTitle, ANode);
end;

function TtiChartLegendTreeViewForm.GetOnSelectNode: TtiVTTVNodeEvent;
begin
  Result:= FTreeView.OnSelectNode;
end;

function TtiChartLegendTreeViewForm.GetSelectedElement: TtiObject;
begin
  if FTreeView.SelectedData is TtiObject then
    result := TtiObject(FTreeView.SelectedData)
  else
    result := nil;
end;

function TtiChartLegendTreeViewForm.GetSeriesVisible(
  const ASeriesName: string): boolean;
var
  LNode: PVirtualNode;
  LFound: boolean;
begin
  LFound := FindNodeBySeriesName(ASeriesName, LNode);
  Assert(LFound, 'SeriesName "' + ASeriesName + '" not found');
  Result := LFound and (LNode.CheckState in [csCheckedNormal, csCheckedPressed, csMixedPressed]);
end;

function TtiChartLegendTreeViewForm.GetShowEmptyRootNode: boolean;
begin
  Result := FTreeView.ShowEmptyRootNode;
end;

function TtiChartLegendTreeViewForm.IsSeriesVisibleByCaption(
  const ASeriesTitle: string): boolean;
var
  LNode: PVirtualNode;
  LFound: boolean;
begin
  LFound := FindNodeBySeriesTitle(ASeriesTitle, LNode);
  Assert(LFound, 'Series with Title "' + ASeriesTitle + '" not found');
  Result := LFound and (LNode.CheckState in [csCheckedNormal, csCheckedPressed, csMixedPressed]);
end;

procedure TtiChartLegendTreeViewForm.SetOnSelectNode(
  const ADelegate: TtiVTTVNodeEvent);
begin
  FTreeView.OnSelectNode := ADelegate;
end;

procedure TtiChartLegendTreeViewForm.SetSelectedElement(const AElement: TtiObject);
begin
  if AElement is TtiObject then
    FTreeView.SelectedData := AElement;
end;

procedure TtiChartLegendTreeViewForm.SetSeriesVisibleByText(
  const AFindNodeByText: TtiChartLegendTreeViewNodeByTextFinder; const AValue: string;
  const AVisible: boolean);
var
  LNode: PVirtualNode;
  LNodeData: TtiObject;
  LChartLegendItemData: ItiChartLegendItemData;
begin
  if AFindNodeByText(AValue, LNode) then
  begin
  // Setting LNode.CheckState indirectly in this way follows same code path as
  // a check-box click, setting parents' check states as a side-effect
    if AVisible then
      FTreeView.VT.CheckState[LNode] := csCheckedNormal
    else
      FTreeView.VT.CheckState[LNode] := csUncheckedNormal;

    LNodeData := FTreeView.GetObjectFromNode(LNode);
    Assert(LNodeData.TestValid(TtiObject), CTIErrorInvalidObject);

    if Supports(LNodeData, ItiChartLegendItemData, LChartLegendItemData) then
      FChart.ShowSeries(LChartLegendItemData.GetChartSeries(FChart), AVisible);

  end;

end;

procedure TtiChartLegendTreeViewForm.SetSeriesVisible(const ASeriesName: string;
  const AVisible: boolean);
begin
  SetSeriesVisibleByText(FindNodeBySeriesName, ASeriesName, AVisible);
end;

procedure TtiChartLegendTreeViewForm.SetSeriesVisibleByCaption(
  const ASeriesTitle: string; const AVisible: Boolean);
begin
  SetSeriesVisibleByText(FindNodeBySeriesTitle, ASeriesTitle, AVisible);
end;

procedure TtiChartLegendTreeViewForm.SetShowEmptyRootNode(const AShowNode: boolean);
begin
  FTreeView.ShowEmptyRootNode := AShowNode;
end;

procedure TtiChartLegendTreeViewForm.VTTVSelectNode(AtiVTTreeView: TtiVTTreeView;
  ANode: PVirtualNode; AData: TtiObject);
begin
  inherited;
  SelectedElement := AData;
end;


end.
