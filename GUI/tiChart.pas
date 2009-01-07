//TODO:
//ShowDataPointHint - CrossHairs not being redrawn properly when mousing over datapoints too quickly
unit tiChart;

interface

uses
  Classes, Controls, Contnrs, Forms, Graphics, Menus, Series, Types, StdCtrls,
  ExtCtrls, Grids,
  TeeProcs, TeEngine, Chart, Tabs, DockTabSet, ComCtrls,
  tiBaseObject, tiObject, tiSpeedButton, tiRoundedPanel, tiResources;

const
  cZoomPercent = 10;
  cChartLegendItemHeight = 28;
  cChartLegendItemCheckBoxLeft = 50;

type

  TtiChartLegendPanel = class;
  TtiChartLegendForm = class;

//tiClearPanel------------------------------------------------------------------
  TtiClearPanel = class(TCustomPanel)
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    property Canvas;
  end;

//tiChartPanel------------------------------------------------------------------
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

  TAssignGraphDataEvent = procedure (const AData : TObject; const AChart :
      TtiTimeSeriesChart) of object;
  TDataGapEvent = procedure (const ADataBeforeGap: TObject; const ADataAfterGap: TObject;
      const AChart: TtiTimeSeriesChart) of object;
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

//tiChartLegendPanel
  TtiChartLegendPosition = (clpLeft, clpRight);

  TtiChartLegendItem = class(TtiClearPanel)
  private
    FChartSeries: TLineSeries;
    FCheckBox: TCheckBox;
    FtiChart   : TtiTimeSeriesChart;
    procedure DoOnCheckBoxClick(Sender: TObject);
    function GetChecked: Boolean;
    procedure SetChecked(const AValue: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(const AOwner: TtiChartLegendForm;
      const AChart: TtiTimeSeriesChart; const ASeries: TLineSeries); reintroduce;
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

  TtiChartLegendPanel = class(TtiClearPanel)
  private
//    FDockTabSet: TDockTabSet;
//    FButonPanel: TtiClearPanel;
//    FLegendFormPanel: TtiClearPanel;
//    FlegendButton: TtiSpeedButton;
    FPageControl: TPageControl;
    FChartLegendForm: TtiChartLegendForm;
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
        ALegendPosition: TtiChartLegendPosition; Dummy : integer = 0); reintroduce; overload;
    destructor Destroy; override;
    procedure SelectUserPanel;
    property ChartLegendForm: TtiChartLegendForm read FChartLegendForm;
    property UserPanel: TtiChartUserPanel read FUserPanel;
  end;

  // Form to display a legend (a small graphic of the seris - with it's title)
  TtiChartLegendForm = class(TCustomForm)
  private
    FChart: TtiTimeSeriesChart;
    FList: TObjectList;
    FPopupMenu : TPopupMenu;
    FpmiSelectAll: TMenuItem;
    FpmiSelectNone: TMenuItem;
    function GetSeriesVisible(const ASeriesName: string): boolean;
    procedure SetSeriesVisible(const ASeriesName: string; const AValue: boolean);
    function FindBySeriesName(const ASeriesName: string): TtiChartLegendItem;
    function FindBySeriesTitle(const ASeriesTitle: string): TtiChartLegendItem;

    procedure DoSelectAll(Sender: TObject);
    procedure DoSelectNone(Sender: TObject);
  public
    Constructor CreateNew(const AChartLegendPanel : TtiChartLegendPanel;
      const AChart: TtiTimeSeriesChart); reintroduce; overload;
    destructor Destroy; override;
    procedure ClearSeries;
    procedure CreateSeries;
    property  SeriesVisible[const ASeriesName: string]: boolean read GetSeriesVisible write SetSeriesVisible;
    function IsSeriesVisibleByCaption(const ASeriesTitle: string): boolean;
  end;

//tiChartWithLegendPanel--------------------------------------------------------
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
        TtiChartLegendPosition); reintroduce; overload;
    destructor Destroy; override;
    property ChartLegendPanel: TtiChartLegendPanel read FChartLegendPanel;
    property ShowLegend: Boolean read GetShowLegend write SetShowLegend;
  end;

//tiChartButtonsPanel
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

//TtiDatapointHintForm----------------------------------------------------------

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

//TtiChart

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
    FChartLegendForm: TtiChartLegendForm;
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
    FOnCrossHair: TCrossHairEvent;
    FOnDataGap: TDataGapEvent;
    FOnRangeChange: TRangeChangeEvent;
    FOnGetDataByDateTime: TGetDataByDateTimeEvent;
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
    FSeriesList: TObjectList;
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
    procedure ConstrainChartAxesView(const AXAxisChange, AYAxisChange: Double);
    procedure RepositionChart;
    function GetZoomed: Boolean;
    procedure DoDrawChart(const ANewSeries: Boolean; const AZoomOut: Boolean);
    function GetUserPanel: TtiChartUserPanel;
  protected
    procedure ClearSeries;
    procedure AddSeries(const ASeries: TChartSeries);
    function AddBarSeries(const ATitle: string): TBarSeries;
    function AddLineSeries(const ATitle: string): TLineSeries;
    procedure ClearSeriesValues;
    procedure Loaded; override;
  public
    constructor Create(Owner : TComponent; const AButtonsPosition:
        TtiChartButtonsPosition; const ALegendPosition: TtiChartLegendPosition);
        reintroduce; overload;
    destructor Destroy; override;
    function AddDateTimeBarSeries(const ATitle : string): TBarSeries;
    procedure AddDateTimeGap(const ASeriesName: string; const AXBeforeGap:
        TDateTime; const AXAfterGap: TDateTime);
    function AddDateTimeLineSeries(const ATitle: string): TLineSeries;
    procedure AddDateTimeValues(const ASeriesTitle : string;
        const AX: TDateTime; const AY: real; const ALabel: string = '';
        const AColor: TColor = clDefault);
    procedure Clear;
    procedure RefreshSeries;
    procedure RedrawChart;
    function SeriesByName(const ASeriesName : string): TChartSeries;
    procedure ShowSeries(var ASeries: TLineSeries; const AVisible: Boolean);

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
    property XDataValueUnderMouse: Real read FXDataValueUnderMouse write
        FXDataValueUnderMouse;
    property XValueUnderMouse: Real read FXValueUnderMouse write
        FXValueUnderMouse;
    property YDataValueUnderMouse: Real read FYDataValueUnderMouse write
        FYDataValueUnderMouse;
    property YValueUnderMouse: Real read FYValueUnderMouse write
        FYValueUnderMouse;
    property Zoomed: Boolean read GetZoomed;
    function IsSeriesVisible(const ASeriesTitle: string): boolean;
    function PointsVisible: Integer;
    procedure SelectUserPanel;

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
    property OnCrossHair: TCrossHairEvent read FOnCrossHair write FOnCrossHair;
    property OnDataGap: TDataGapEvent read FOnDataGap write FOnDataGap;
    property OnDblClickChart: TNotifyEvent read GetOnDblClickChart write
        SetOnDblClickChart;
    property OnRangeChange: TRangeChangeEvent read FOnRangeChange
        write FOnRangeChange;
    property OnGetDataByDateTime: TGetDataByDateTimeEvent
        read FOnGetDataByDateTime write FOnGetDataByDateTime;
  end;

implementation
uses
  SysUtils
  ,Math
  ,tiImageMgr
  ,tiUtils
  ,tiConstants
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
    TtiTimeSeriesChart; ALegendPosition: TtiChartLegendPosition; Dummy : integer = 0);
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
//    Width := 20;
//    Align := alLeft;
//  end;
//  FlegendButton := TtiSpeedButton.Create(self);
//  with FLegendButton do
//  begin
//    Parent := FButonPanel;
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
//  FLegendFormPanel.Align := alClient;

  // Create a page control to hold the legend form and optional user form on
  // separate tab sheets. The tabs are hidden if the user form is not used.
  FPageControl := TPageControl.Create(nil);
  FPageControl.Align := alClient;
  FPageControl.Style := tsFlatButtons;
  FPageControl.Parent := Self;
  FLegendTabSheet := TTabSheet.Create(FPageControl);
  FLegendTabSheet.Caption := 'Legend';
  FLegendTabSheet.PageIndex := 0;
  FLegendTabSheet.TabVisible := False;
  FLegendTabSheet.BorderWidth := 0;
  FLegendTabSheet.PageControl := FPageControl;
  FUserPanelTabSheet := TTabSheet.Create(FPageControl);
  FUserPanelTabSheet.Caption := 'User';
  FUserPanelTabSheet.PageIndex := 1;
  FUserPanelTabSheet.TabVisible := False;
  FUserPanelTabSheet.BorderWidth := 0;
  FUserPanelTabSheet.PageControl := FPageControl;
  FPageControl.ActivePageIndex := 0;

  FChartLegendForm := TtiChartLegendForm.CreateNew(Self, FParenttiChart);
  FChartLegendForm.Name := 'FChartLegendForm';
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

constructor TtiChartLegendForm.CreateNew(const AChartLegendPanel : TtiChartLegendPanel;
      const AChart: TtiTimeSeriesChart);
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
  FList:= TObjectList.Create(False);

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

destructor TtiChartLegendForm.Destroy;
begin
  FList.Free;
  FpmiSelectNone.Free;
  FpmiSelectAll.Free;
  FPopupMenu.Free;
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
  const AValue: boolean);
var
  LChartLegendItem: TtiChartLegendItem;
begin
  LChartLegendItem:= FindBySeriesName(ASeriesName);
  Assert(Assigned(LChartLegendItem), 'SeriesName "' + ASeriesName + '" not found');
  LChartLegendItem.Checked:= AValue;
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
      const AChart: TtiTimeSeriesChart; const ASeries: TLineSeries);
begin
  inherited Create(AOwner);
  Parent:= AOwner;
  FtiChart:= AChart;
  FChartSeries:= ASeries;
  ControlStyle  := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
  Height := cChartLegendItemHeight;
  Color := clWhite;
  OnClick := DoOnCheckBoxClick;
  FCheckBox := TCheckBox.Create(Self);
  FCheckBox.Parent := Self;
  FCheckBox.Top := (cChartLegendItemHeight - FCheckBox.Height) div 2;
  FCheckBox.Left := cChartLegendItemCheckBoxLeft;
  // Set checked before assigning event handler to avoid Click event.
  FCheckBox.Checked := ASeries.Active;
  FCheckBox.OnClick := DoOnCheckBoxClick;
  FCheckBox.Caption := ASeries.Title;
  FCheckBox.Width:= Canvas.TextWidth(ASeries.Title) + 20;
  FCheckBox.Font.Color := ASeries.SeriesColor;
  Width:= FCheckBox.Left + FCheckBox.Width;
end;

destructor TtiChartLegendItem.Destroy;
begin
  FCheckBox.Free;
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
    FtiChart.ShowSeries(FChartSeries, FCheckBox.Checked);
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
  LRowWidth: Integer;
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
  LRow := cChartLegendItemHeight div 2;
  Canvas.PenPos := Point(4, LRow);
  Canvas.LineTo(cChartLegendItemCheckBoxLeft - 4, LRow);
  // Draw vertical line
  LCol := (cChartLegendItemCheckBoxLeft - 8) div 2;
  Canvas.PenPos := Point(LCol, 4);
  Canvas.LineTo(LCol, cChartLegendItemHeight - 8);
  // Restore the pen properties
  Canvas.Pen.Color := LColor;
  Canvas.Pen.Width := LPenWidth;

  LRowWidth := cChartLegendItemCheckBoxLeft - 4 - LRow;
  Width := FCheckBox.Width + LRowWidth + 20;
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
    TtiChartLegendPosition);
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

  FChartLegendPanel := TtiChartLegendPanel.CreateNew(self, FParenttiChart, FLegendPosition);

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
  FChartButtonDetailList.AddButton('Undo zoom', cResTI_Maximize, FParenttiChart.DoSBDefaultZoomClick);
  FChartButtonDetailList.AddButton('Copy to clipboard', cResTI_CopyToClipboard, FParenttiChart.DoSBCopyToClipBrdClick);

//  Height := CSBSize + CSBLeft * 2 + CSBVerticalSpace;
//  Width := CSBSize + CSBLeft * 2;

  FChartButtonDetailList.CreateButtons(self, FChartButtonsPosition);

//  FsbZoomIn     := TtiSpeedButton.Create(self);
//  with FsbZoomIn do begin
//    Parent := self;
//    Top := LSBTop;
//    Left := LSBLeft;
//    case FChartButtonsPosition of
//      cbpLeft: begin
//                 LSBTop := LSBTop + CSBLeft + CSBSize;
//               end;
//      cbpTop: begin
//                 LSBLeft := LSBLeft + CSBLeft + CSBSize;
//               end;
//    end;
//
//    Height := CSBSize;
//    Width := CSBSize;
//    Flat   := true;
//    ImageRes := tiRIZoomIn;
//    Hint  := 'Zoom in';
//    ShowHint := true;
//    OnClick := FParenttiChart.DoSBZoomInClick;
//  end;
//
//  FsbZoomOut    := TtiSpeedButton.Create(self);
//  With FsbZoomOut do begin
//    Parent := self;
//    Top := LSBTop;
//    Left := LSBLeft;
//    case FChartButtonsPosition of
//      cbpLeft: begin
//                 LSBTop := LSBTop + CSBLeft + CSBSize;
//               end;
//      cbpTop: begin
//                 LSBLeft := LSBLeft + CSBLeft + CSBSize;
//               end;
//    end;
//    Height := CSBSize;
//    Width := CSBSize;
//    Flat   := true;
//    Color  := Self.Color;
//    ImageRes := tiRIZoomOut;
//    Hint  := 'Zoom out';
//    ShowHint := true;
//    OnClick := FParenttiChart.DoSBZoomOutClick;
//  end;
//
//  FsbDefaultZoom := TtiSpeedButton.Create(self);
//  With FsbDefaultZoom do begin
//    Parent := self;
//    Top := LSBTop;
//    Left := LSBLeft;
//    case FChartButtonsPosition of
//      cbpLeft: begin
//                 LSBTop := LSBTop + CSBLeft + CSBSize;
//               end;
//      cbpTop: begin
//                 LSBLeft := LSBLeft + CSBLeft + CSBSize;
//               end;
//    end;
//    Height := CSBSize;
//    Width := CSBSize;
//    Flat   := true;
//    Color  := Self.Color;
//    ImageRes := tiRIMaximize;
//    Hint  := 'Undo zoom';
//    ShowHint := true;
//    OnClick := FParenttiChart.DoSBDefaultZoomClick;
//  end;
//
////  FsbViewLegend := TtiSpeedButton.Create(self);
////  With FsbViewLegend do begin
////    Parent := self;
////    Top := liSBTop;
////    Left := liSBLeft;
////    case FChartButtonsPosition of
////      cbpLeft: begin
////                 liSBTop := liSBTop + ciSBLeft + ciSBSize;
////               end;
////      cbpTop: begin
////                 liSBLeft := liSBLeft + ciSBLeft + ciSBSize;
////               end;
////    end;
////    Height := ciSBSize;
////    Width := ciSBSize;
////    Flat   := true;
////    Color  := Self.Color;
////    ImageRes := tiRIGraphLine;
////    Hint  := 'View legend';
////    ShowHint := true;
////    OnClick := FParenttiChart.DoSBViewLegendClick;
////  end;
//
//  {
//    FsbConfig     := TSpeedButton.Create(self);
//    With FsbConfig do begin
//      Parent := self;
//      Top   := liSBTop;
//      Left  := ciSBLeft;
//      Height := ciSBSize;
//      Width := ciSBSize;
//      liSBTop := liSBTop + ciSBLeft + ciSBSize;
//      Flat   := true;
//      Color := self.color;
//      Glyph.LoadFromResourceName(HInstance, 'tiChart_Configure');
//      Hint  := 'Configure';
//      ShowHint := true;
//      OnClick := DoSBConfigClick;
//    end;
//  }
//
//  FsbCopyToClipBrd := TtiSpeedButton.Create(self);
//  With FsbCopyToClipBrd do begin
//    Parent := self;
//    Top := LSBTop;
//    Left := LSBLeft;
//      //uncomment the following if you add more buttons
//  //    case ChartButtonsPosition of
//  //      cbpLeft: begin
//  //                 LSBTop := LSBTop + ciSBLeft + ciSBSize;
//  //               end;
//  //      cbpTop: begin
//  //                 LSBLeft := LSBLeft + ciSBLeft + ciSBSize;
//  //               end;
//  //    end;
//
//    Height := CSBSize;
//    Width := CSBSize;
//    Flat   := true;
//    Color := self.color;
//  //    gTIImageListMgr.LoadGlyphToTISpeedButton(cResTI_CopyToClipboard, FsbCopyToClipBrd);
//    ImageRes := tiRICopyToClipboard;
//    Hint  := 'Copy to clipboard';
//    ShowHint := true;
//    OnClick := FParenttiChart.DoSBCopyToClipBrdClick;
//
//  end;
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
  FPnl.Align:= alClient;
  FPnl.Color:= clInfoBk;
  FPnl.BorderColor:= clBlack;
  FPnl.BorderThickness:= 1;
  FPnl.CornerRadius:= 0;

  FGrid := TStringGrid.Create( self ) ;
  with FGrid do begin
    Parent := FPnl ;
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
****************************** TtiTimeSeriesChart ******************************
}
constructor TtiTimeSeriesChart.Create(Owner : TComponent; const AButtonsPosition:
    TtiChartButtonsPosition; const ALegendPosition: TtiChartLegendPosition);
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

  FChartWithLegendPanel := TtiChartWithLegendPanel.Create(self, FLegendPosition);
//  with FChartWithLegendPanel do
//  begin
//    Parent := self;
//    Align := alClient;
//  end;

  FChartLegendForm := FChartWithLegendPanel.ChartLegendPanel.ChartLegendForm;

  FDataPointHintForm := TtiDataPointHintForm.CreateNew(self);
  FDataPointHintForm.Parent := self;

  FSeriesList := TObjectList.Create;
  FSeriesList.OwnsObjects := True;

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

destructor TtiTimeSeriesChart.Destroy;
begin
  FTestData.Free;
  FButtonsPanel.Free;
  FChartWithLegendPanel.Free;
  FDataPointHintForm.Free;
  FChartDataMappings.Free;
  FShowDataPointHintTmr.Free;
  FSeriesList.Free;
  inherited;
end;

procedure TtiTimeSeriesChart.ClearSeries;
begin
  FChart.SeriesList.Clear;
  FSeriesList.Clear;
  FChartLegendForm.ClearSeries;
end;

procedure TtiTimeSeriesChart.AddSeries(const ASeries: TChartSeries);
begin
  FSeriesList.Add(ASeries);
  FChart.AddSeries(ASeries);
end;

function TtiTimeSeriesChart.AddBarSeries(const ATitle: string): TBarSeries;
var
  LSeries: TBarSeries;
begin
  LSeries := TBarSeries.Create(nil);
  LSeries.Title := ATitle;
  LSeries.Marks.Visible := false;
  LSeries.BarWidthPercent := 100; // Perhaps this should be a param ?
  AddSeries(LSeries);
  result := LSeries;
end;

procedure TtiTimeSeriesChart.AddButton(const AButtonHint: string;
  const AImageResName: string; const ADoButtonClick: TNotifyEvent;
  const AButtonOrder: Integer);
begin
  Assert(Assigned(FButtonsPanel));
  FButtonsPanel.AddButton(AButtonHint, AImageResName, ADoButtonClick, AButtonOrder);
end;

function TtiTimeSeriesChart.AddDateTimeBarSeries(const ATitle : string):
    TBarSeries;
begin
  TimeSeriesChart := true;
  result         := AddBarSeries(ATitle);
end;

procedure TtiTimeSeriesChart.AddDateTimeGap(const ASeriesName: string; const
    AXBeforeGap: TDateTime; const AXAfterGap: TDateTime);
var
  LSeries: TChartSeries;
begin
  LSeries := SeriesByName(SeriesTitleToName(ASeriesName));
  Assert(LSeries <> nil, 'Can not find series <' + ASeriesName + '>');
  LSeries.AddNullXY((AXBeforeGap + AXAfterGap) / 2.0, (FLeftAxisMin + FLeftAxisMax) / 2.0, '');
end;

function TtiTimeSeriesChart.AddDateTimeLineSeries(const ATitle: string): TLineSeries;
begin
  TimeSeriesChart := true;
  result := AddLineSeries(ATitle);
end;

procedure TtiTimeSeriesChart.AddDateTimeValues(const ASeriesTitle : string;
    const AX: TDateTime; const AY: real; const ALabel: string;
    const AColor: TColor);
var
  LSeries: TChartSeries;
  LColor: TColor;
begin
  FBottomAxisMin := Min(FBottomAxisMin, AX);
  FBottomAxisMax := Max(FBottomAxisMax, AX);

  FLeftAxisMin := Min(FLeftAxisMin, AY);
  FLeftAxisMax := Max(FLeftAxisMax, AY);

  LSeries := SeriesByName(SeriesTitleToName(ASeriesTitle));
  Assert(LSeries <> nil, 'Can not find series <' + ASeriesTitle + '>');
  LSeries.ColorEachPoint := True;
  if AColor = clDefault then
    LColor := LSeries.SeriesColor
  else
    LColor := AColor;
  LSeries.AddXY(AX, AY, ALabel, LColor);
end;

function TtiTimeSeriesChart.AddLineSeries(const ATitle: string): TLineSeries;
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
  AddSeries(LSeries);
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
                               Axis: TtiAxis);
    var
      LOnChange : TNotifyEvent;
      LScrollAxisRange: Double;
      LScrollBarRange: Integer;
    begin
      // We don't scroll the _entire_ axis range as that can put the data off
      // the graph at the maximum scroll.
      LScrollAxisRange := prMaxAxisRange - prCurrentAxisRange;
      if IsZero(LScrollAxisRange, 0.0000000001) then
      begin
        pScrollBar.Visible := False;
        Exit; //==>
      end;

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

begin
  // No scroll bars to show
  if FScrollStyle = ssNone then
    Exit; //==>

  if not Zoomed then
    LScrollStyle := ssNone
  else
    LScrollStyle := FScrollStyle;
  FChartPanel.ScrollBars := LScrollStyle;
  if LScrollStyle = ssNone then
    Exit; //==>

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
    _SetupScrollBar(FscbBottom, LMaxAxisRange, LCurrentAxisRange, LPosition, axHorizontal);
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
    _SetupScrollBar(FscbLeft, LMaxAxisRange, LCurrentAxisRange, LPosition, axVertical);
  end;
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
      LSeriesIndex := LSeries.GetCursorValueIndex;
// ToDo: Series.GetCursorValueIndex is returning a value offset by
//       1 for each NullXY value that has been added.
//       Action: TtiTimeSeriesChart.AddDateTimeValues and
//               TtiTimeSeriesChart.AddDateTimeGap build up a data structure
//               of the actual index values (Returned by calls to Series.AddXY
//               and Series.AddNullXY, and the actual index value (calculated by
//               keeping track of how many NullXY values have been added)

    if LSeriesIndex <> -1 then
      Break; //==>
  end;

  FDataUnderMouse := nil;

  // A data point was found close to the mouse cursor, so set some values
  if (LSeriesIndex <> -1) and
     (LSeries <> nil) then
  begin
    XDataValueUnderMouse := LSeries.XValues[LSeriesIndex];
    YDataValueUnderMouse := LSeries.YValues[LSeriesIndex];

    if  Assigned(FOnDataGap) then
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
      FChart.BottomAxis.DateTimeFormat := 'hh:mm:ss';
    // <= 250 milliseconds
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
end;

procedure TtiTimeSeriesChart.RepositionChart;
begin
  ConstrainChartAxesView(0, 0);
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

function TtiTimeSeriesChart.SeriesTitleToName(const ATitle: string): string;
var
  I: Integer;
begin
  Result := '';

  // Must only contain alphanumeric characters.
  for I := 1 to Length(ATitle) do
    if ATitle[I] in ['a'..'z','A'..'Z','0'..'9'] then
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
  DoDrawChart(True {ANewSeries}, True {AZoomOut});

end;

procedure TtiTimeSeriesChart.DoDrawChart(const ANewSeries: Boolean;
    const AZoomOut: Boolean);
var
  i: Integer;
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
          if Assigned(FOnDataGap) and (i > 0) then
            FOnDataGap(TObject(FData.Items[i - 1]), TObject(FData.Items[i]), Self);

          FOnAssignGraphData(TObject(FData.Items[i]),
                              Self);
        end;
    except
      on e:exception do
        raise exception.create('Error in TtiChart.SetData ' +
                                'Message: ' + e.message);
    end;

    // Call RefreshSeries for all series
    // This is necessary for moving average series
    if ANewSeries then
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
  DoDrawChart(False {ANewSeries}, not Zoomed);
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
    OnDataGap := FTestData.DataGap;
    AddLineSeries('Sin');
    AddLineSeries('Cos');
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

procedure TtiTimeSeriesChart.ShowSeries(var ASeries: TLineSeries;
  const AVisible: Boolean);
var
  LZoomed: Boolean;
begin
  // Save zoom state. Showing a series can change this.
  LZoomed := Zoomed;
  ASeries.Active := AVisible;
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
begin
  if AZoomPercent <= -100.0 then
    raise exception.create('Invalid zoom percent passed to TtiChart.Zoom');

  if AZoomPercent >= 0.0 then
    LZoomFactor := ((100.0 + AZoomPercent) / 100.0) - 1.0
  else
    LZoomFactor := 1.0 - (100.0 / (100.0 + AZoomPercent));

  LCurrentAxisRange := Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum;
  LYAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;
  LCurrentAxisRange := Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum;
  LXAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;
  ConstrainChartAxesView(LXAxisChange, LYAxisChange);

  RepositionChart;
  RangeChange;
end;

procedure TtiTimeSeriesChart.ConstrainChartAxesView(const AXAxisChange: Double; const AYAxisChange: Double);
var
  LMin: Double;
  LMax: Double;
begin
  LMin := Chart.BottomAxis.Minimum + AXAxisChange;
  LMax := Chart.BottomAxis.Maximum - AXAxisChange;
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

  LMin := Chart.LeftAxis.Minimum + AYAxisChange;
  LMax := Chart.LeftAxis.Maximum - AYAxisChange;
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

end.
