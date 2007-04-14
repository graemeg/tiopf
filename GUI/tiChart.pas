{
  ToDo:
    1. Remove the dependency on TChartPro
       a) The Open,Close,High,Low series
       b) The control for selecting a the series to view

    2. Better positioning of scroll bars
       a) Border below horizontal, and to left of vertical
       b) Rename from Bottom and Left to Horizontal and Vertical

    3. Add an AddLineSeries method that can be used like the addDateTimeLineSeries method

    4. Modify the RefreshSeries method to read from the list of series-property mappings
       (Have I given the names of the series in the collection editor logical name?)
       (How are we going to manage an X-value? - perhaps the series collection
        should have an ordinal property to XValue or YValue)

    5. Modify the OnCrossHairs to work with the new way of setting up data.

    6. When constraining view to data and zooming out adjacent to chart edge keep view
       proportions.
}

{$I tiDefines.inc}

unit tiChart;

interface

uses
   tiSpeedButton
  ,tiObject
  ,Controls
  ,Classes
  ,ExtCtrls
  ,Forms
  ,Graphics
  ,Windows
  ,ComCtrls
  ,StdCtrls
  ,Menus
  ,Contnrs
  ,Chart
  ,Series
  ,TeEngine
  ,TeeProcs
;

type

  TtiTimeSeriesChart = class;

  TtiChartTestDataItem = class(TObject)
  private
    FYValue2: real;
    FXValue: real;
    FYValue1: real;
  public
    property XValue : real read FXValue  write FXValue;
    property YValue1 : real read FYValue1 write FYValue1;
    property YValue2 : real read FYValue2 write FYValue2;
  end;

  TtiChartTestData = class(TObjectList)
  public
    constructor create; virtual;
    procedure   AssignGraphData(AData: TObject; pChart: TtiTimeSeriesChart);
    procedure   DataGap(ADataBeforeGap: TObject; ADataAfterGap: TObject; pChart: TtiTimeSeriesChart);
  end;

  TtiChartDataMapping = class(TCollectionItem)
  private
    FDisplayLabel: string;
    FPropertyName: string;
    procedure SetPropertyName(const AValue: string);
  protected
    function    GetDisplayName : string; override;
  published
    property    DisplayLabel : string read FDisplayLabel write FDisplayLabel;
    property    PropertyName : string read FPropertyName write SetPropertyName;
  public
    constructor Create(Collection : TCollection); override;
    destructor  Destroy; override;
    function    Clone : TtiChartDataMapping;
//    procedure   Assign(Source : TtiObject); override;
  end;

  TtiChartDataMappings = class(TCollection)
  private
    FOwner : TComponent;
    function  GetItem(Index : integer): TtiChartDataMapping;
    procedure SetItem(Index : integer; const AValue : TtiChartDataMapping);
  published
  protected
    function  GetOwner : TPersistent; override;
  public
    constructor Create(Owner : TComponent);
    destructor  Destroy; override;
    property  Items[Index: integer ]: TtiChartDataMapping
                read GetItem write SetItem;
    function  Add : TtiChartDataMapping;
    procedure NamesToStringList(pSL : TStringList);
    function  FindByFieldName(psFieldName : string): TtiChartDataMapping;
    procedure Clear; reintroduce;
  end;

  // This thread is responsible for keeping an eye the cross hairs and clearing
  // them if the mouse is moved off the draw region of the graph.
  TThrdGraphMonitor = class(TThread)
  private
    FtiChart : TtiTimeSeriesChart;
    procedure DoClearCrossHairs;
  public
    Constructor CreateExt(const pChart : TtiTimeSeriesChart);
    destructor  Destroy; override;
    procedure   Execute; override;
  end;

  TAssignGraphDataEvent = procedure (AData : TObject;
                                      pChart : TtiTimeSeriesChart) of object;
  TDataGapEvent = procedure (ADataBeforeGap: TObject;
                             ADataAfterGap: TObject;
                             pChart: TtiTimeSeriesChart) of object;

  TCrossHairEvent = procedure (pSeries : TChartSeries;
                                AIndex : integer;
                                AData : TObject;
                                AList : TList) of object;

  TtiChartLegendItem = class(TCustomPanel)
  private
    FChartSeries: TLineSeries;
    FCheckBox  : TCheckBox;
    FtiChart   : TtiTimeSeriesChart;
    procedure   SetChartSeries(const AValue: TLineSeries);
    procedure   DoOnCheckBoxClick(Sender: TObject);
  protected
    procedure   Paint; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;
    property    ChartSeries : TLineSeries read FChartSeries write SetChartSeries;
    property    tiChart : TtiTimeSeriesChart read FtiChart write FtiChart;
  end;

  // Form to display a legend (a small graphic of the seris - with it's title)
  TtiChartLegendForm = class(TForm)
  private
    FtiChart: TtiTimeSeriesChart;
    FDrawCrossHairs : boolean;
  protected
    procedure SetTIChart(AValue: TtiTimeSeriesChart);
    procedure DoOnDeactivate(Sender: TObject);
    procedure DoOnShow(Sender: TObject);
    procedure Paint; override;
  public
    Constructor CreateNew(owner : TComponent; Dummy : integer = 0); override;
    property    tiChart : TtiTimeSeriesChart read FtiChart write SetTIChart;
  end;

  TtiChartInternal = class(TChart)
  protected
    procedure Paint; override;
  end;


  // ToDo: Refactor into TtiChart & TtiChartTimeSeries
  TtiChart = class(TCustomPanel)
  end;

  TtiTimeSeriesChart = class(TtiChart)
  private
    FChart: TChart;
    FData: TList;
    FOnAssignGraphData: TAssignGraphDataEvent;
    FOnDataGap: TDataGapEvent;
    FOnCrossHair : TCrossHairEvent;
    FthrdMonitor : TThrdGraphMonitor;
    FTestData : TtiChartTestData;
    FChartDataMappings : TtiChartDataMappings;

    FiOldX : integer;
    FiOldY : integer;
    FiOldCircX : integer;
    FiOldCircY : integer;
    FCurrentData: TtiObject;

    FbZoomed        : boolean    ;
    FsbZoomIn       : TtiSpeedButton;
    FsbZoomOut      : TtiSpeedButton;
    FsbDefaultZoom  : TtiSpeedButton;
    FsbViewLegend   : TtiSpeedButton;
    //FsbConfig       : TtiSpeedButton;
    FsbCopyToClipBrd : TtiSpeedButton;

    FscbBottom      : TScrollBar ;
    FscbLeft        : TScrollBar ;

    FbCrossHairsDrawn : boolean;
    FbDrawCrossHairs     : boolean;
    FbDrawCrossHairsNow  : boolean;
    FbDrawCrossHairsSaved : boolean;

    FiSnapToDataSize: integer;
    FiSnapToDataRadius : integer;
    FbTimeSeriesChart : boolean;
    FrYValueUnderMouse: real;
    FDataUnderMouse: TObject;
    FrXValueUnderMouse: real;
    FrXDataValueUnderMouse: real;
    FrYDataValueUnderMouse: real;
    FbDisplayTestData: boolean;

    FrBottomAxisMin : real;
    FrBottomAxisMax : real;
    FrLeftAxisMin  : real;
    FrLeftAxisMax  : real;
    FScrollStyle: TScrollStyle;
    FbShowTestData: boolean;
    FChartLegendForm : TtiChartLegendForm;
    FConstrainViewToData: boolean;

    procedure SetData(const AValue: TList);
    function  GetView3D: boolean;
    procedure SetView3D(const AValue: boolean);
    function  GetChartColor: TColor;
    procedure SetChartColor(const AValue: TColor);
    procedure ClearCrossHairs;
    procedure DoChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ReSetCrossHairs;
    procedure DoSBZoomInClick(sender : TObject);
    procedure DoSBZoomOutClick(sender : TObject);
    procedure DoSBDefaultZoomClick(sender : TObject);
    procedure DoSBViewLegendClick(sender : TObject);
    procedure DoSBCopyToClipBrdClick(sender : TObject);
    procedure OnDrawKey(pSeries : TChartSeries;
                         AIndex : integer;
                         AData  : TObject;
                         AList  : TList);
    procedure DoOnZoom(Sender : TObject);
    procedure DoOnUndoZoom(Sender : TObject);
    procedure DoOnScroll(Sender : TObject);
    procedure DoOnAllowScroll(Sender: TChartAxis; var AMin: double; var AMax: double; var AllowScroll: boolean);
    function  IsFormFocused: boolean;
    function  GetAxisBottom : TChartAxis;
    function  GetAxisLeft  : TChartAxis;
    function  GetMouseInCrossHairRegion: boolean;
    procedure SetSnapToDataSize(const AValue: integer);
    function  GetChartPopupMenu: TPopupMenu;
    procedure SetChartPopupMenu(const AValue: TPopupMenu);
    function  GetNextSeriesColor : TColor;
    procedure DoOnBeforeDrawAxes(sender : TObject);
    procedure SetTimeSeriesChart(const AValue: boolean);
    procedure SnapEditDialogToButton(pForm: TForm; pSender: TObject);
    procedure SetDrawCrossHairsNow(const AValue: boolean);
    procedure DoDrawCrossHairs(piX, piY: Integer; pbDraw : boolean);
    procedure DoVerticalScroll(Sender : TObject);
    procedure DoHorizontalScroll(Sender : TObject);
    procedure AdjustScrollBarPositions;
    procedure SetScrollStyle(const AValue: TScrollStyle);
    procedure SetShowTestData(const AValue: boolean);
    function  GetOnDblClickChart: TNotifyEvent;
    procedure SetOnDblClickChart(const AValue: TNotifyEvent);
    function  GetOwnerForm(Control: TComponent): TForm;
    function  SeriesTitleToName(const psTitle: string): string;
    procedure SetDrawCrossHairs(const AValue: boolean);
    function  LineSeriesPointerVisible: Boolean;
    procedure SetSeriesPointerVisible(AValue: Boolean);
    function GetVisibleSeriesAsString: string;
    procedure SetVisibleSeriesAsString(const AValue: string);
    procedure SetConstrainViewToData(const Value: boolean);
    procedure Zoom(AZoomPercent: double);
    procedure ResetZoom;

    //procedure MouseToChartCoOrds(var piX, piY: integer);
    //procedure SetDisplayTestData(const AValue: boolean);

  protected
    function    AddLineSeries(const psTitle : string;
                               pbPointerVisible : boolean = true): TLineSeries;
    function    AddBarSeries(const psTitle : string): TBarSeries;
    procedure   Loaded; override;

  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
    property Color;
    property ChartPopupMenu : TPopupMenu   read GetChartPopupMenu  write SetChartPopupMenu;
    property OnDblClickChart : TNotifyEvent read GetOnDblClickChart write SetOnDblClickChart;

    property OnAssignGraphData : TAssignGraphDataEvent read FOnAssignGraphData write FOnAssignGraphData;
    property OnDataGap : TDataGapEvent read FOnDataGap write FOnDataGap;
    property OnCrossHair : TCrossHairEvent read FOnCrossHair write FOnCrossHair;

    property View3D : boolean read GetView3D write SetView3D default false;
    property ChartColor : TColor read GetChartColor write SetChartColor default clWhite;
    property AxisBottom : TChartAxis read GetAxisBottom;
    property AxisLeft  : TChartAxis read GetAxisLeft;
    // A flag the user can set at design time to control the drawing of XHairs
    property DrawCrossHairs   : boolean read FbDrawCrossHairs    write SetDrawCrossHairs default true;
    // An internal flat which is used to control the temporary turning on or off of XHairs
    property DrawCrossHairsNow : boolean read FbDrawCrossHairsNow write SetDrawCrossHairsNow;
    property SnapToDataSize   : integer read FiSnapToDataSize    write SetSnapToDataSize default 15;
    property ScrollBars : TScrollStyle read FScrollStyle write SetScrollStyle default ssBoth;
    property ConstrainViewToData: boolean read FConstrainViewToData write SetConstrainViewToData;
    property ShowTestData : boolean read FbShowTestData write SetShowTestData default false;
    property ChartDataMappings : TtiChartDataMappings read FChartDataMappings;

    //property DisplayTestData  : boolean read FbDisplayTestData   write SetDisplayTestData default false;
  public
    constructor Create(Owner : TComponent); override;
    destructor  Destroy; override;

    property    Chart : TChart read FChart write FChart;
    property    Data : TList  read FData  write SetData;
    property    CrossHairsDrawn : boolean read FbCrossHairsDrawn write FbCrossHairsDrawn default true;
    property    XValueUnderMouse    : real read FrXValueUnderMouse     write FrXValueUnderMouse   ;
    property    YValueUnderMouse    : real read FrYValueUnderMouse     write FrYValueUnderMouse   ;
    property    XDataValueUnderMouse : real read FrXDataValueUnderMouse write FrXDataValueUnderMouse;
    property    YDataValueUnderMouse : real read FrYDataValueUnderMouse write FrYDataValueUnderMouse;
    property    DataUnderMouse      : TObject read FDataUnderMouse   ;

    function    AddDateTimeLineSeries(const psTitle   : string;
                                       pbPointerVisible : boolean = true): TLineSeries;
    procedure   AddDateTimeValues(const psSeriesName : string;
                                   const pX: TDateTime;
                                   const pY: real);
    procedure   AddDateTimeGap(const psSeriesName: string;
                               const pXBeforeGap: TDateTime;
                               const pXAfterGap: TDateTime);

    function    AddDateTimeBarSeries(const psTitle : string): TBarSeries;

    function    SeriesByName(const psSeriesName : string): TChartSeries;
    property    MouseInCrossHairRegion : boolean read GetMouseInCrossHairRegion;
    property    VisibleSeriesAsString: string Read GetVisibleSeriesAsString Write SetVisibleSeriesAsString;
    property    TimeSeriesChart : boolean read FbTimeSeriesChart write SetTimeSeriesChart;
    procedure   Clear;
    procedure   RefreshSeries;

  end;

implementation
uses
  SysUtils
  ,Math
  ,tiImageMgr
  ,tiResources
  ,tiUtils
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
  ciSCBWidth = 16; // Must be 16, will be changed by delphi at runtime if not :(
  cuiAxisLabelSize = 40;
  cuiAxisTitleSize = 20;
  cChartLegendItemHeight = 28;
  cChartLegendItemCheckBoxLeft = 50;
  cPointerVisibleLimit = 100;
  cZoomPercent = 10;
  cScrollResolution = 400;

// Register with the component pallet
//procedure Register;
//begin
//  RegisterComponents('TechInsite',
//                      [  TtiChart
//                      ]);
//end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChart
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiTimeSeriesChart.Create(Owner: TComponent);
const
  ciSBLeft   =  4;
  ciSBSize   = 20;
var
  liSBTop : integer;

begin
  inherited Create(Owner);

  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
  Height     := 217;
  Width      := 285;
  Color      := clWhite;

  FChartDataMappings := TtiChartDataMappings.Create(Self);

  FChart := TtiChartInternal.Create(self);
  with FChart do begin
    Parent := self;
    //Align := alClient;
    Anchors    := [akLeft,akTop,akRight,akBottom];
    Top := 0;
    Left := ciSBLeft * 2 + ciSBSize;
    Height := self.Height - ciBorder - ciSCBWidth;
    Width := self.Width - Left - ciBorder - ciSCBWidth;

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

    OnMouseMove           := DoChartMouseMove;
    OnMouseDown           := DoMouseDown;
    OnMouseUp             := DoMouseUp;

    OnBeforeDrawAxes      := DoOnBeforeDrawAxes;
    OnZoom                := DoOnZoom;
    OnUndoZoom            := DoOnUndoZoom;
    OnScroll              := DoOnScroll;
    OnAllowScroll         := DoOnAllowScroll;
  end;

  FscbBottom := TScrollBar.Create(self);
  with FscbBottom do
  begin
    Parent  := self;
    TabStop := false;
    Kind    := sbHorizontal;
    Top     := FChart.Top + FChart.Height + ciBorder;
    Left    := FChart.Left;
    Height  := ciSCBWidth;
    Width   := FChart.Width;
    Anchors := [akLeft,akRight,akBottom];
    Min     := 0;
    Max     := cScrollResolution;
    Position := 0;
    PageSize := Max - Min;
    OnChange := DoHorizontalScroll;
    Visible := false;
  end;

  FscbLeft := TScrollBar.Create(self);
  with FscbLeft do
  begin
    Parent  := self;
    TabStop := false;
    Kind    := sbVertical;
    Top     := FChart.Top;
    Left    := FChart.Left + FChart.Width + ciBorder;
    Height  := FChart.Height;
    Width   := ciSCBWidth;
    Anchors := [akRight,akTop,akBottom];
    Min     := 0;
    Max     := cScrollResolution;
    Position := 0;
    PageSize := Max - Min;
    OnChange := DoVerticalScroll;
    Visible := false;
  end;

  liSBTop := 10;

  FsbZoomIn     := TtiSpeedButton.Create(self);
  with FsbZoomIn do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    liSBTop := liSBTop + ciSBLeft + ciSBSize;
    Flat   := true;
    ImageRes := tiRIZoomIn;
    Hint  := 'Zoom in';
    ShowHint := true;
    OnClick := DoSBZoomInClick;
  end;

  FsbZoomOut    := TtiSpeedButton.Create(self);
  With FsbZoomOut do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    liSBTop := liSBTop + ciSBLeft + ciSBSize;
    Flat   := true;
    Color  := Self.Color;
    ImageRes := tiRIZoomOut;
    Hint  := 'Zoom out';
    ShowHint := true;
    OnClick := DoSBZoomOutClick;
  end;

  FsbDefaultZoom := TtiSpeedButton.Create(self);
  With FsbDefaultZoom do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    liSBTop := liSBTop + ciSBLeft + ciSBSize;
    Flat   := true;
    Color  := Self.Color;
    ImageRes := tiRIMaximize;
    Hint  := 'Undo zoom';
    ShowHint := true;
    OnClick := DoSBDefaultZoomClick;
  end;

  FsbViewLegend := TtiSpeedButton.Create(self);
  With FsbViewLegend do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    liSBTop := liSBTop + ciSBLeft + ciSBSize;
    Flat   := true;
    Color  := Self.Color;
    ImageRes := tiRIGraphLine;
    Hint  := 'View legend';
    ShowHint := true;
    OnClick := DoSBViewLegendClick;
  end;

{
  FsbConfig     := TSpeedButton.Create(self);
  With FsbConfig do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    liSBTop := liSBTop + ciSBLeft + ciSBSize;
    Flat   := true;
    Color := self.color;
    Glyph.LoadFromResourceName(HInstance, 'tiChart_Configure');
    Hint  := 'Configure';
    ShowHint := true;
    OnClick := DoSBConfigClick;
  end;
}

  FsbCopyToClipBrd := TtiSpeedButton.Create(self);
  With FsbCopyToClipBrd do begin
    Parent := self;
    Top   := liSBTop;
    Left  := ciSBLeft;
    Height := ciSBSize;
    Width := ciSBSize;
    Flat   := true;
    Color := self.color;
//    gTIImageListMgr.LoadGlyphToTISpeedButton(cResTI_CopyToClipboard, FsbCopyToClipBrd);
    ImageRes := tiRICopyToClipboard;
    Hint  := 'Copy to clipboard';
    ShowHint := true;
    OnClick := DoSBCopyToClipBrdClick;

  end;

  FbDrawCrossHairs    := true;
  FbDrawCrossHairsNow := true;
  ResetCrossHairs;
  FbCrossHairsDrawn := false;
  SnapToDataSize   := 15;
  FbTimeSeriesChart := false;
  FbDisplayTestData := false;
  FConstrainViewToData := false;

  FthrdMonitor := TThrdGraphMonitor.CreateExt(Self);

  FrBottomAxisMin := MaxInt;
  FrBottomAxisMax := -MaxInt;
  FrLeftAxisMin  := MaxInt;
  FrLeftAxisMax  := -MaxInt;

  FScrollStyle   := ssBoth;
  FbShowTestData := false;
  FbZoomed       := false;

end;

destructor TtiTimeSeriesChart.Destroy;
begin
  FthrdMonitor.Terminate;
  FthrdMonitor.Free;
  FTestData.Free;
  FChartLegendForm.Free;
  FChartDataMappings.Free;
  inherited;
end;

function TtiTimeSeriesChart.GetChartColor: TColor;
begin
  result := FChart.Color;
end;

function TtiTimeSeriesChart.GetView3D: boolean;
begin
  Result := FChart.View3D;
end;

procedure TtiTimeSeriesChart.SetChartColor(const AValue: TColor);
begin
  FChart.Color := AValue;
  Color       := AValue;
end;

procedure TtiTimeSeriesChart.SetData(const AValue: TList);
var
  i : integer;
begin

  if not FThrdMonitor.Suspended then
    FthrdMonitor.Suspend;

  DrawCrossHairsNow := false;

  try
    // The data object is nil
    if AValue = nil then begin
      FChart.SeriesList.Clear;
      FData := nil;
      Exit; //==>
    end;

    FData := AValue;

    SetSeriesPointerVisible(LineSeriesPointerVisible);
    // If there are more than 100 data points then scan through all the series
    // and turn their check marks off.
//    if FData.Count > cPointerVisibleLimit then
//      SetSeriesPointerVisible(False);

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
    RefreshSeries;

    FbZoomed := false;
    AdjustScrollBarPositions;

    {$IFDEF DeactivateCrossHairs }
    if (not (csDesigning in ComponentState)) then
      FthrdMonitor.Resume;
    {$ENDIF}

  finally
    DrawCrossHairsNow := true;
  end;

end;


procedure TtiTimeSeriesChart.RefreshSeries;
var
  i : integer;
begin
  for i := 0 to FChart.SeriesCount - 1 do
    FChart.Series[i].RefreshSeries;
end;

procedure TtiTimeSeriesChart.SetView3D(const AValue: boolean);
begin
  FChart.View3D := AValue;
end;

procedure TtiTimeSeriesChart.ClearCrossHairs;
begin
  DoChartMouseMove(self,
                    [],
                    -1, -1);
end;

  // This procedure draws the crosshair lines
Procedure TtiTimeSeriesChart.DoDrawCrossHairs(piX,piY:Integer; pbDraw : boolean);
begin

  if FbCrossHairsDrawn = pbDraw then
    Exit; //==>

  if Screen.Cursor <> crNone then
    Screen.Cursor := crNone;

  FbCrossHairsDrawn := pbDraw;
  FChart.Canvas.Pen.Color := clYellow;
  FChart.Canvas.Pen.Mode := pmXor  ;
  FChart.Canvas.Pen.Style := psSolid;
  FChart.Canvas.Pen.Width := 1      ;

  // Draw the vertical line
  FChart.Canvas.MoveTo(piX,FChart.ChartRect.Top-FChart.Height3D);
  FChart.Canvas.LineTo(piX,FChart.ChartRect.Bottom-FChart.Height3D);

  // Draw the horizontal line
  FChart.Canvas.MoveTo(FChart.ChartRect.Left+FChart.Width3D,piY);
  FChart.Canvas.LineTo(FChart.ChartRect.Right+FChart.Width3D,piY);

end;

procedure TtiTimeSeriesChart.DoChartMouseMove(Sender: TObject;
                                     Shift: TShiftState;
                                     X, Y: Integer);

  Procedure DrawCircle(AX,AY:Integer);
  begin
    With Chart,Canvas do
    begin
      // You have to enter the complimentary colour of what you want !
      Pen.Color := clGray;
      Pen.Style := psSolid;
      Pen.Mode := pmXor  ;
      Pen.Width := 1      ;
      Brush.Style := bsClear;

      Ellipse(aX-FiSnapToDataRadius,
               aY-FiSnapToDataRadius,
               aX+FiSnapToDataRadius,
               aY+FiSnapToDataRadius);

      FiOldCircX := aX;
      FiOldCircY := aY;

    end;
  end;

  function GetDataObject(AIndex : Integer): TObject;
  begin
    if (FData <> nil) and
       (AIndex <= FData.Count-1) then
      result := TObject(FData.Items[AIndex])
    else
      result := nil;
  end;

Var
  lSeries      : TChartSeries;
  liDataIndex  : integer;
  i : integer;
  lPoint:TPoint;
begin

  if FData = nil then
    Exit; //==>

  try
    lPoint.X := Mouse.CursorPos.X;
    lPoint.Y := Mouse.CursorPos.Y;
  except
    // Ignore unable to get cursor pos (such as when screensaver is active
    // or workstation is locked)
    on EOSError do
      Exit; //==>
  end;
  lPoint  := FChart.ScreenToClient(lPoint);

  // Clear the values under the mouse cursor which are surfaced as
  // properties of TtiChart
  XValueUnderMouse    := 0;
  YValueUnderMouse    := 0;
  XDataValueUnderMouse := 0;
  YDataValueUnderMouse := 0;

  if Chart.SeriesCount = 0 then
    exit; //==>

  // Set the values under the mouse cursor, based on the first chart series
  XValueUnderMouse := Chart.Series[0].XScreenToValue(x);
  YValueUnderMouse := Chart.Series[0].YScreenToValue(x);

  FCurrentData := nil;

  // Erase the old cross hairs
  if (FiOldX <> -1) then begin
    DoDrawCrossHairs(FiOldX, FiOldY, false);
    FiOldX := -1;
  end;

  // Erase old circle
  if (FiOldCircX <> -1) then begin
    DrawCircle(FiOldCircX, FiOldCircY);
    FiOldCircX := -1;
  end;

  // type TShiftState = set of (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);

  // Check if mouse is inside Chart rectangle
  if (not (GetMouseInCrossHairRegion and
             DrawCrossHairs and
             DrawCrossHairsNow)) or
     (ssLeft in Shift) then begin
    Screen.Cursor := crDefault;
    OnDrawKey(nil, -1, nil, nil);
    Exit;
  end;

  // Draw crosshair at current position
  DoDrawCrossHairs(X, Y, true);

  // Store old position
  FiOldX := X;
  FiOldY := Y;

  // Scan all the series looking for the closest data point
  // If found, lSeries and liDataIndex will have been set.
  liDataIndex := -1;
  lSeries := nil;
  for i := 0 to FChart.SeriesCount - 1 do begin
    lSeries := FChart.Series[i];
    if lSeries.Active then
      liDataIndex := lSeries.GetCursorValueIndex;
    if liDataIndex <> -1 then
      Break; //==>
  end;

  FDataUnderMouse      := nil;

  // A data point was found close to the mouse cursor, so set some values
  if (liDataIndex <> -1) and
     (lSeries <> nil) then
  begin
    XDataValueUnderMouse := lSeries.XValues[liDataIndex];
    YDataValueUnderMouse := lSeries.YValues[liDataIndex];
    if liDataIndex < FData.Count then
      FDataUnderMouse      := TObject(FData.Items[liDataIndex]);

    DrawCircle(lSeries.CalcXPosValue(XDataValueUnderMouse),
                lSeries.CalcYPosValue(YDataValueUnderMouse));

    OnDrawKey(lSeries,
               liDataIndex,
               DataUnderMouse,
               FData);
  end else begin
    OnDrawKey(nil, -1, nil, nil);
  end;

  Application.ProcessMessages;

end;

procedure TtiTimeSeriesChart.ReSetCrossHairs;
begin
  FiOldX := -1;
  FiOldY := -1;
  FiOldCircX := -1;
  FiOldCircY := -1;
end;

procedure TtiTimeSeriesChart.DoSBDefaultZoomClick(sender: TObject);
begin
  FbDrawCrossHairsSaved := DrawCrossHairs;
  DrawCrossHairs := false;
  try
    AxisBottom.Automatic := true;
    ResetZoom;
  finally
    DrawCrossHairs := FbDrawCrossHairsSaved;
  end;
end;

procedure TtiTimeSeriesChart.SnapEditDialogToButton(pForm : TForm;
                                          pSender : TObject);
var
  lSB : TControl;
  lPoint : TPoint;
begin
  Assert(pSender is TControl, 'Sender not a TButton');
  lSB := TControl(pSender);
  lPoint.x := FChart.Left + FChart.LeftAxis.PosAxis - FChart.LeftAxis.MaxLabelsWidth - pForm.Width;
  lPoint.y := lSB.Top;
  lPoint := lSB.Parent.ClientToScreen(lPoint);
  pForm.Top := lPoint.Y;
  pForm.Left := lPoint.X;
end;

procedure TtiTimeSeriesChart.DoSBViewLegendClick(sender: TObject);
begin
  if FChartLegendForm = nil then
    FChartLegendForm := TtiChartLegendForm.CreateNew(nil);
  FChartLegendForm.TIChart := Self;
  SnapEditDialogToButton(FChartLegendForm, Sender);
  FChartLegendForm.Show;
end;

procedure TtiTimeSeriesChart.Zoom(AZoomPercent: double);
var
  LZoomFactor: double;
  LCurrentAxisRange: double;
  LAxisChange: double;
  LMin: double;
  LMax: double;
begin
  if AZoomPercent <= -100.0 then
    raise exception.create('Invalid zoom percent passed to TtiChart.Zoom');

  if AZoomPercent >= 0.0 then
    LZoomFactor := ((100.0 + AZoomPercent) / 100.0) - 1.0
  else
    LZoomFactor := 1.0 - (100.0 / (100.0 + AZoomPercent));

  LCurrentAxisRange := Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum;
  LAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;
  LMin := Chart.LeftAxis.Minimum + LAxisChange;
  if FConstrainViewToData and (LMin < FrLeftAxisMin) then
    LMin := FrLeftAxisMin;
  LMax := Chart.LeftAxis.Maximum - LAxisChange;
  if FConstrainViewToData and (LMax > FrLeftAxisMax) then
    LMax := FrLeftAxisMax;
  Chart.LeftAxis.SetMinMax(LMin, LMax);

  LCurrentAxisRange := Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum;
  LAxisChange := (LCurrentAxisRange * LZoomFactor) / 2.0;
  LMin := Chart.BottomAxis.Minimum + LAxisChange;
  if FConstrainViewToData and (LMin < FrBottomAxisMin) then
    LMin := FrBottomAxisMin;
  LMax := Chart.BottomAxis.Maximum - LAxisChange;
  if FConstrainViewToData and (LMax > FrBottomAxisMax) then
    LMax := FrBottomAxisMax;
  Chart.BottomAxis.SetMinMax(LMin, LMax);

  SetSeriesPointerVisible(LineSeriesPointerVisible);
  FbZoomed := true;
  AdjustScrollBarPositions;
end;

procedure TtiTimeSeriesChart.ResetZoom;
begin
  Chart.LeftAxis.SetMinMax(FrLeftAxisMin, FrLeftAxisMax);
  Chart.BottomAxis.SetMinMax(FrBottomAxisMin, FrBottomAxisMax);

  FbZoomed := False;
  SetSeriesPointerVisible(LineSeriesPointerVisible);
  AdjustScrollBarPositions;
end;

procedure TtiTimeSeriesChart.DoSBZoomInClick(sender: TObject);
begin
  Zoom(cZoomPercent);
end;

procedure TtiTimeSeriesChart.DoSBZoomOutClick(sender: TObject);
begin
  Zoom(-cZoomPercent);
end;

procedure TtiTimeSeriesChart.OnDrawKey(pSeries : TChartSeries;
                              AIndex : integer;
                              AData: TObject;
                              AList: TList);
begin
  if Assigned(FOnCrossHair) then
    FOnCrossHair(pSeries, AIndex, AData, AList);
end;

// Replace this with a call to the delphi func GetParentForm()
function TtiTimeSeriesChart.GetOwnerForm(Control : TComponent): TForm;
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

// Determine if the tiChart's parent form is currently focused.
function TtiTimeSeriesChart.IsFormFocused: boolean;
var
  lForm : TForm;
begin

  result := true;

  // This may AV when used in an ActiveX
  lForm := TForm(GetOwnerForm(self));

  // Added (and assert removed) for ActiveX deployment
  if lForm = nil then
    Exit; //==>
  //Assert(lForm <> nil, 'Owner form not found');

  // This will return the correct answere if
  // a) An MDIChildForm is active, and the application is deactiveate
  // b) An MDIChildForm is active, and another MIDChild form is made active
  // but will not return the correct answer if
  // c) An MIDChildForm is active, and a ModalDialog is activated over it.
  // Non MDIChildForms, non modal dialogs have not been tested.
  case (lForm.FormStyle) of
    fsMDIChild :
    begin
      try
        result := (Application.Active) and (Application.MainForm.ActiveMDIChild = lForm);
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
    //  fsMDIForm  : result := (lForm.Active) and (Application.Active);

    // This is not fool proof yet. The commented out code below will work for a
    // normal, mdi app. But will fail for an "Outlook" style ap where the client
    // forms are contained inside a main form.
    // fsNormal   : result := (lForm.Active);
    // The line below hacks around that problem.
    fsNormal   : result := true;

    fsStayOnTop : result := (lForm.Active);
  else
    raise exception.create('Invalid FormStyle passed to TtiChart.IsFormFocused');
  end;
end;

function TtiTimeSeriesChart.AddDateTimeLineSeries(const psTitle : string;
                                         pbPointerVisible : boolean = true): TLineSeries;
begin
  TimeSeriesChart := true;
  result := AddLineSeries(psTitle, pbPointerVisible);
end;

procedure TtiTimeSeriesChart.AddDateTimeValues(const psSeriesName : string;
                                      const pX: TDateTime;
                                      const pY: real);
var
  lSeries : TChartSeries;
begin
  FrBottomAxisMin := Min(FrBottomAxisMin, pX);
  FrBottomAxisMax := Max(FrBottomAxisMax, pX);

  FrLeftAxisMin := Min(FrLeftAxisMin, pY);
  FrLeftAxisMax := Max(FrLeftAxisMax, pY);

  lSeries := SeriesByName(psSeriesName);
  Assert(lSeries <> nil, 'Can not find series <' + psSeriesName + '>');
  lSeries.AddXY(pX, pY);
end;

procedure TtiTimeSeriesChart.AddDateTimeGap(const psSeriesName: string;
  const pXBeforeGap: TDateTime; const pXAfterGap: TDateTime);
var
  lSeries : TChartSeries;
begin
  lSeries := SeriesByName(psSeriesName);
  Assert(lSeries <> nil, 'Can not find series <' + psSeriesName + '>');
  lSeries.AddNullXY((pXBeforeGap + pXAfterGap) / 2.0, (FrLeftAxisMin + FrLeftAxisMax) / 2.0, '');
end;

function TtiTimeSeriesChart.SeriesTitleToName(const psTitle : string): string;
begin
  result := StringReplace(psTitle, ' ', '', [rfReplaceAll, rfIgnoreCase]);
end;

function TtiTimeSeriesChart.AddLineSeries(const psTitle : string;
                                 pbPointerVisible : boolean = true): TLineSeries;
var
  lSeries : TLineSeries;
begin

  lSeries := TLineSeries.Create(FChart);
  lSeries.Name := SeriesTitleToName(psTitle);
  lSeries.Title := psTitle;
  lSeries.Pointer.Style := psCross;
  lSeries.Pointer.Visible := pbPointerVisible;
  lSeries.Pointer.InflateMargins := false;
  lSeries.XValues.DateTime := true;

  lSeries.SeriesColor := GetNextSeriesColor;

  FChart.AddSeries(lSeries);
  result := lSeries;

end;

function TtiTimeSeriesChart.GetNextSeriesColor : TColor;
var
  liColorIndex : integer;
begin
  liColorIndex := FChart.SeriesCount;
  while liColorIndex > High(cuaSeriesColors) do
    liColorIndex := liColorIndex - High(cuaSeriesColors);
  result := cuaSeriesColors[liColorIndex];
end;

procedure TtiTimeSeriesChart.DoSBCopyToClipBrdClick(sender: TObject);
begin
  FChart.CopyToClipboardMetafile(True);  { <--- Enhanced Metafile = True }
end;

function TtiTimeSeriesChart.AddDateTimeBarSeries(const psTitle: string): TBarSeries;
begin
  TimeSeriesChart := true;
  result         := AddBarSeries(psTitle);
end;

function TtiTimeSeriesChart.AddBarSeries(const psTitle: string): TBarSeries;
var
  lSeries : TBarSeries;
begin
  lSeries := TBarSeries.Create(FChart);
  lSeries.Title := psTitle;
  lSeries.Marks.Visible := false;
  lSeries.BarWidthPercent := 100; // Perhaps this should be a param ?
  FChart.AddSeries(lSeries);
  result := lSeries;
end;

function TtiTimeSeriesChart.GetAxisBottom: TChartAxis;
begin
  result := FChart.BottomAxis;
end;

function TtiTimeSeriesChart.GetAxisLeft: TChartAxis;
begin
  result := FChart.LeftAxis;
end;

function TtiTimeSeriesChart.SeriesByName(const psSeriesName: string): TChartSeries;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FChart.SeriesCount - 1 do begin
    if SameText(FChart.Series[i].Name, psSeriesName) then begin
      result := FChart.Series[i];
      Break; //==>
    end;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdGraphMonitor
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TThrdGraphMonitor.CreateExt(const pChart : TtiTimeSeriesChart);
begin
  Assert(pChart <> nil, 'pChart not assigned');
  Create(true);
  self.Priority := tpLower;
  FreeOnTerminate := false;
  FtiChart := pChart;
end;

destructor TThrdGraphMonitor.Destroy;
begin
  inherited;
end;

procedure TThrdGraphMonitor.DoClearCrossHairs;
var
  lbIsFormFocused         : boolean;
  lbMouseInCrossHairRegion : boolean;
  lbDrawCrossHairs        : boolean;
begin
  lbDrawCrossHairs        := FtiChart.DrawCrossHairs;
  if not lbDrawCrossHairs then
  begin
    FtiChart.DrawCrossHairsNow := false;
    Exit; //==>
  end;

  lbIsFormFocused         := FtiChart.IsFormFocused;
  lbMouseInCrossHairRegion := FtiChart.MouseInCrossHairRegion;

  FtiChart.DrawCrossHairsNow :=
    lbMouseInCrossHairRegion and
    lbIsFormFocused and
    lbDrawCrossHairs;
end;

procedure TThrdGraphMonitor.Execute;
begin
  while not Terminated do begin
    Sleep(100);
    Synchronize(DoClearCrossHairs);
  end;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartLegendForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartLegendForm.CreateNew(owner: TComponent; Dummy : integer = 0);
begin
  inherited CreateNew(Owner);
  Color := clWhite;
  BorderStyle := bsNone;
  OnShow     := DoOnShow;
end;

procedure TtiChartLegendForm.DoOnDeactivate(Sender: TObject);
begin
  Hide;
  FtiChart.DrawCrossHairs:= FDrawCrossHairs;
end;

procedure TtiChartLegendForm.DoOnShow(Sender: TObject);
begin
  FDrawCrossHairs:= FtiChart.DrawCrossHairs;
  FtiChart.DrawCrossHairs:= False;
  OnDeactivate:= DoOnDeactivate;
end;

procedure TtiChartLegendForm.Paint;
var
  LColor: TColor;
begin
  inherited;
  LColor:= Canvas.Pen.Color;
  Canvas.Pen.Color:= clBlack;
  Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
  Canvas.Pen.Color:= LColor;
end;

procedure TtiChartLegendForm.SetTIChart(AValue: TtiTimeSeriesChart);
var
  i : integer;
  lWidth : integer;
  lHeight : integer;
  lSeriesEdit : TtiChartLegendItem;
begin
  FTIChart := AValue;
  lWidth := 0;
  lHeight := 0;
  for i := 0 to FTIChart.Chart.SeriesCount - 1 do
  begin
    lWidth := Max(lWidth, Canvas.TextWidth(FtiChart.Chart.Series[i].Title));
    lSeriesEdit := TtiChartLegendItem.Create(Self);
    lSeriesEdit.Parent := Self;
    lSeriesEdit.tiChart := FtiChart;
    lSeriesEdit.ChartSeries := FtiChart.Chart.Series[i] as TLineSeries;
    lSeriesEdit.Top:= LHeight+1;
    LSeriesEdit.Left:= 1;
    LSeriesEdit.Width:= 80+LWidth;
    Inc(lHeight, lSeriesEdit.Height);
  end;
  ClientWidth := 80 + lWidth+2;
  ClientHeight := lHeight+2;
end;

function TtiTimeSeriesChart.GetMouseInCrossHairRegion: boolean;
var
  lPoint : TPoint;
begin
  try
    lPoint.X := Mouse.CursorPos.X;
    lPoint.Y := Mouse.CursorPos.Y;
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
    lPoint  := FChart.ScreenToClient(lPoint);
    result  := IsFormFocused and
                  PtInRect(Chart.ChartRect,
                           Point(lPoint.X-Chart.Width3D,
                           lPoint.Y+Chart.Height3D));
  except
    on e: EInvalidOperation do
      result := False;
  end;
end;

procedure TtiTimeSeriesChart.Clear;
var
  i : integer;
begin
  for i := Chart.SeriesCount - 1 downto 0 do
    Chart.SeriesList.Delete(i);
  DoSBDefaultZoomClick(nil);
  FChart.Refresh;
end;

procedure TtiTimeSeriesChart.SetSnapToDataSize(const AValue: integer);
begin
  FiSnapToDataSize := AValue;
  FiSnapToDataRadius := FiSnapToDataSize div 2;
end;

function TtiTimeSeriesChart.GetChartPopupMenu: TPopupMenu;
begin
  result := FChart.PopupMenu;
end;

procedure TtiTimeSeriesChart.SetChartPopupMenu(const AValue: TPopupMenu);
begin
  FChart.PopupMenu := AValue;
end;

procedure TtiTimeSeriesChart.SetConstrainViewToData(const Value: boolean);
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
      LMaxAxisRange := FrLeftAxisMax - FrLeftAxisMin;
      if LMaxAxisRange > 0 then
      begin
        if LCurrentAxisRange > LMaxAxisRange then
          LCurrentAxisRange := LMaxAxisRange;
        if FChart.LeftAxis.Minimum < FrLeftAxisMin then
        begin
          FChart.LeftAxis.Minimum := FrLeftAxisMin;
          FChart.LeftAxis.Maximum := FChart.LeftAxis.Minimum + LCurrentAxisRange;
        end;
        if FChart.LeftAxis.Maximum > FrLeftAxisMax then
        begin
          FChart.LeftAxis.Maximum := FrLeftAxisMax;
          FChart.LeftAxis.Minimum := FChart.LeftAxis.Maximum - LCurrentAxisRange;
        end;
      end;

      // Keep horizontal view within data.
      LCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
      LMaxAxisRange := FrBottomAxisMax - FrBottomAxisMin;
      if LMaxAxisRange > 0 then
      begin
        if LCurrentAxisRange > LMaxAxisRange then
          LCurrentAxisRange := LMaxAxisRange;
        if FChart.BottomAxis.Minimum < FrBottomAxisMin then
        begin
          FChart.BottomAxis.Minimum := FrBottomAxisMin;
          FChart.BottomAxis.Maximum := FChart.BottomAxis.Minimum + LCurrentAxisRange;
        end;
        if FChart.BottomAxis.Maximum > FrBottomAxisMax then
        begin
          FChart.BottomAxis.Maximum := FrBottomAxisMax;
          FChart.BottomAxis.Minimum := FChart.BottomAxis.Maximum - LCurrentAxisRange;
        end;
      end;
    end;
  end;
end;

procedure TtiTimeSeriesChart.DoOnBeforeDrawAxes(sender: TObject);
const
  cdtSecond  = 1/24/60/60;
  cdtMinute  = 1/24/60  ;
  cdtHour    = 1/24     ;
  cdtDay     = 1.0      ;
  cdtMonth   = 354.75/12;
  cdtYear    = 364.75   ;
  cdt20Years = cdtYear*20;
var
  ldtPeriod : TDateTime;
begin

  if not FbTimeSeriesChart then
    Exit; //==>

  if FData = nil then
    Exit; //==>

  ldtPeriod := FChart.BottomAxis.Maximum -
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
  if ldtPeriod > cdt20Years then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneYear];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'yyyy';
    //Log('> 20 Years');
  // > 1 and       <= 20 years
  end else if (ldtPeriod > cdtYear) and (ldtPeriod <= cdt20Years) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneYear];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'yyyy';
    //Log('> 1 and       <= 20 years');
  // > 3 months  and <= 1 year
  end else if (ldtPeriod > cdtMonth*3) and (ldtPeriod <= cdtYear) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneMonth];
    FChart.BottomAxis.LabelsMultiline := true;
    FChart.BottomAxis.DateTimeFormat := 'mmm yy';
    //Log('> 1 month  and <= 1 year');
  // > 2 months <= 3 months
  end else if (ldtPeriod > cdtMonth*2) and (ldtPeriod <= cdtMonth*3) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneWeek];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    //Log('> 2 months <= 3 months');
  // > 1 months <= 2 months
  end else if (ldtPeriod > cdtMonth) and (ldtPeriod <= cdtMonth*2) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtThreeDays];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    //Log('> 1 month <= 3 months');
  // > 10 day    and <= 1 month
  end else if (ldtPeriod > cdtDay*10) and (ldtPeriod <= cdtMonth) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtTwoDays];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    //Log('> 10 days    and <= 1 month');
  // > 3 days    and <= 10 days
  end else if (ldtPeriod > cdtDay*3) and (ldtPeriod <= cdtDay*10) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneDay];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'dd mmm';
    //Log('> 3 day    and <= 10 days');
  // > 1 day    and <= 3 days
  end else if (ldtPeriod > cdtDay) and (ldtPeriod <= cdtDay*3) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtSixHours];
    FChart.BottomAxis.LabelsMultiline := true;
    FChart.BottomAxis.DateTimeFormat := 'hh:mm dd-mmm';
    //Log('> 1 day    and <= 3 days');
  // > 1 hour   and <= 1 day
  end else if (ldtPeriod > cdtHour) and (ldtPeriod <= cdtDay) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtOneHour];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'hh:mm';
    //Log('> 1 hour   and <= 1 day');
  // > 1 minute and <= 1 hour
  end else if (ldtPeriod > cdtMinute) and (ldtPeriod <= cdtHour) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtTenMinutes];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'hh:mm';
    //Log('> 1 minute and <= 1 hour');
  // > 1 second and <= 1 minute
  end else if (ldtPeriod > cdtSecond) and (ldtPeriod <= cdtMinute) then begin
    FChart.BottomAxis.Increment      := DateTimeStep[dtTenSeconds];
    FChart.BottomAxis.LabelsMultiline := false;
    FChart.BottomAxis.DateTimeFormat := 'mm:ss';
    //Log('> 1 second and <= 1 minute');
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

procedure TtiTimeSeriesChart.SetTimeSeriesChart(const AValue: boolean);
begin
  if (not FbTimeSeriesChart) and
     (AValue) then begin
    FbTimeSeriesChart := true;
    Exit; //==>
  end;

  FbTimeSeriesChart := AValue;
  //  FChart.BottomAxis.RoundFirstLabel := true;
  //  FChart.BottomAxis.ExactDateTime  := true;
  // FChart.BottomAxis.LabelsOnAxis := true;
  // FChart.BottomAxis.TickOnLabelsOnly := true;

end;

procedure TtiTimeSeriesChart.SetDrawCrossHairsNow(const AValue: boolean);
begin

  if FbDrawCrossHairsNow = AValue then
    Exit; //==>

  FbDrawCrossHairsNow := AValue;

  if FbDrawCrossHairsNow then begin
    OnMouseMove := DoChartMouseMove;
    // These two lines will cause the XHairs to be drawn when Alt+Tab back onto
    // the app, but will cause XHair litter when mousing over the chart region
    // for the first time. Requires more work...
    //MouseToChartCoOrds(liX, liY);
    //DoDrawCrossHairs( liX, liY, true);
  end else begin
    OnMouseMove := nil;
    ClearCrossHairs;
  end;

end;

{
procedure TtiChart.MouseToChartCoOrds(var piX, piY : integer);
var
  lPoint : TPoint;
begin
  lPoint.X := Mouse.CursorPos.X;
  lPoint.Y := Mouse.CursorPos.Y;
  lPoint  := FChart.ScreenToClient(lPoint);
  piX     := lPoint.X;
  piY     := lPoint.Y;
end;
}

procedure TtiTimeSeriesChart.DoOnZoom(Sender: TObject);
var
  lMpt: TPoint;
begin
  DrawCrossHairsNow := false;
  FbZoomed := true;
  AdjustScrollBarPositions;
  DrawCrossHairsNow := true;

  lMpt := point(FChart.Width, FChart.Height);
  lMPt.X := lMPt.X - {FChart.MarginLeft -} FChart.MarginRight {-
                   - FChart.LeftAxis.LabelsSize} {- FChart.LeftAxis.TitleSize}
                   {- FChart.RightAxis.LabelsSize}
                   - FChart.RightAxis.TitleSize + 5;
  lMPt.Y := lMPt.Y - FChart.MarginTop - FChart.BottomAxis.LabelsSize
                   - FChart.BottomAxis.TitleSize;
  lMpt := FChart.ClientToScreen(lMpt);
  SetSeriesPointerVisible(LineSeriesPointerVisible);
end;

procedure TtiTimeSeriesChart.DoOnUndoZoom(Sender: TObject);
begin
  DrawCrossHairsNow := false;
  ResetZoom;
  DrawCrossHairsNow := true;
end;

procedure TtiTimeSeriesChart.DoOnScroll(Sender: TObject);
begin
  FbZoomed := true;
  AdjustScrollBarPositions;
end;

procedure TtiTimeSeriesChart.DoOnAllowScroll(Sender: TChartAxis; var AMin,
  AMax: double; var AllowScroll: boolean);
begin
  AllowScroll := (FChart.Zoomed) or (not FConstrainViewToData);
end;

procedure TtiTimeSeriesChart.DoMouseDown(Sender: TObject;
                                Button: TMouseButton;
                                Shift: TShiftState;
                                X, Y: Integer);
begin
  if (Button = mbRight) then
  begin
    FbDrawCrossHairsSaved := DrawCrossHairs;
    DrawCrossHairs := false;
  end;
end;

procedure TtiTimeSeriesChart.DoMouseUp(Sender: TObject;
                              Button: TMouseButton;
                              Shift: TShiftState;
                              X, Y: Integer);
begin
  if (Button = mbRight) then
  begin
    DrawCrossHairs := FbDrawCrossHairsSaved;
  end;
end;

{
procedure TtiChart.Loaded;
begin
  inherited;
  if FbDisplayTestData then
    CreateTestData;
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//*TtiChartInternal
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiChartInternal.Paint;
var
  lbDrawCrossHairsSaved : boolean;
begin
  lbDrawCrossHairsSaved := TtiTimeSeriesChart(Owner).DrawCrossHairs;
  TtiTimeSeriesChart(Owner).DrawCrossHairs := false;
  try
    inherited;
  finally
    TtiTimeSeriesChart(Owner).DrawCrossHairs := lbDrawCrossHairsSaved;
  end;
end;

procedure TtiTimeSeriesChart.DoHorizontalScroll(Sender: TObject);
var
  LrMin: Double;
  LrMax: Double;
  LMaxAxisRange: Double;
  LCurrentAxisRange: Double;
  LScrollAxisRange: Double;
  LScrollBarRange: Integer;
  LScrollAxisAmount: Double;
begin
  LMaxAxisRange     := FrBottomAxisMax - FrBottomAxisMin;
  LCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
  // We don't scroll the _entire_ axis range as that can put the data off
  // the graph at the maximum scroll.
  LScrollAxisRange := LMaxAxisRange - LCurrentAxisRange;

  // Work with the scroll bar minimum and maximum to allow the scroll resolution to be changed.
  LScrollBarRange := FscbBottom.Max - (FscbBottom.PageSize - 1) - FscbBottom.Min;

  // Scroll amount
  LScrollAxisAmount := ((FscbBottom.Min + LScrollBarRange - FscbBottom.Position) / LScrollBarRange) * LScrollAxisRange;
  LrMin := (FrBottomAxisMax - LCurrentAxisRange) - LScrollAxisAmount;
  LrMax := LrMin + LCurrentAxisRange;

  FChart.BottomAxis.SetMinMax(LrMin, LrMax);
end;

procedure TtiTimeSeriesChart.DoVerticalScroll(Sender: TObject);
var
  LrMin: Double;
  LrMax: Double;
  LMaxAxisRange: Double;
  LCurrentAxisRange: Double;
  LScrollAxisRange: Double;
  LScrollBarRange: Integer;
  LScrollAxisAmount: Double;
begin
  LMaxAxisRange     := FrLeftAxisMax - FrLeftAxisMin;
  LCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum;
  // We don't scroll the _entire_ axis range as that can put the data off
  // the graph at the maximum scroll.
  LScrollAxisRange := LMaxAxisRange - LCurrentAxisRange;

  // Work with the scroll bar minimum and maximum to allow the scroll resolution to be changed.
  LScrollBarRange := FscbLeft.Max - (FscbLeft.PageSize - 1) - FscbLeft.Min;

  // Scroll amount
  LScrollAxisAmount := (FscbLeft.Position / LScrollBarRange) * LScrollAxisRange;
  LrMin := (FrLeftAxisMax - LCurrentAxisRange) - LScrollAxisAmount;
  LrMax := LrMin + LCurrentAxisRange;

  FChart.LeftAxis.SetMinMax(LrMin, LrMax);
end;

procedure TtiTimeSeriesChart.AdjustScrollBarPositions;
  procedure _SetupScrollBar(pScrollBar : TScrollBar;
                             prMaxAxisRange : real;
                             prCurrentAxisRange : real;
                             prPosition : real;
                             Axis: TtiAxis);
  var
    lOnChange : TNotifyEvent;
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

    lOnChange := pScrollBar.OnChange;
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
      pScrollBar.OnChange := lOnChange;
    end;
  end;

var
  lrMaxAxisRange : real;
  lrCurrentAxisRange : real;
  lrPosition : real;
  liBorder : integer;
begin

  // No scroll bars to show
  if FScrollStyle = ssNone then
    Exit; //==>

  // Check if we falsly think we're zoomed
  if FbZoomed and
      (FChart.LeftAxis.Minimum <= FrLeftAxisMin) and
      (FChart.LeftAxis.Maximum >= FrLeftAxisMax) and
      (FChart.BottomAxis.Minimum <= FrBottomAxisMin) and
      (FChart.BottomAxis.Maximum >= FrBottomAxisMax) then
  begin
    FChart.Zoomed := False;
    FbZoomed := False;
  end;
  
  if (not FbZoomed) or (not FChart.Zoomed) then
  begin
    FscbBottom.Visible := false;
    FscbLeft.Visible  := false;
    Exit; //==>
  end else
    SetScrollStyle(FScrollStyle);

  // Setup the bottom scrollbar
  if (FScrollStyle = ssBoth) or
     (FScrollStyle = ssHorizontal) then
  begin
    liBorder := FChart.Left + FChart.MarginLeft +
                FChart.LeftAxis.LabelsSize + FChart.LeftAxis.TitleSize;
    FscbBottom.Left := liBorder;
    FscbBottom.Width := FChart.Width - liBorder - FChart.MarginRight;

    // Set the position of the slider in the scroll bar
    lrMaxAxisRange    := FrBottomAxisMax           - FrBottomAxisMin;
    lrCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum;
    lrPosition        := FChart.BottomAxis.Minimum - FrBottomAxisMin;
    _SetupScrollBar(FscbBottom, lrMaxAxisRange, lrCurrentAxisRange, lrPosition, axHorizontal);
  end;

  // Setup the left scrollbar
  if (FScrollStyle = ssBoth) or
     (FScrollStyle = ssVertical) then
  begin
    liBorder := FChart.Top + FChart.MarginTop;
    FscbLeft.Top   := liBorder;
    FscbLeft.Height := FChart.Height - liBorder - FChart.MarginBottom;
    // Set the position of the slider in the scroll bar
    lrMaxAxisRange    := FrLeftAxisMax           - FrLeftAxisMin;
    lrCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum;
    lrPosition        := FChart.LeftAxis.Minimum - FrLeftAxisMin;
    _SetupScrollBar(FscbLeft, lrMaxAxisRange, lrCurrentAxisRange, lrPosition, axVertical);
  end;

end;

procedure TtiTimeSeriesChart.SetScrollStyle(const AValue: TScrollStyle);
begin

  FScrollStyle := AValue;

  if not FbZoomed then
  begin
    FscbBottom.Visible         := false;
    FscbLeft.Visible           := false;
    FChart.Height              := self.ClientHeight;
    FChart.Width               := self.Width - FChart.Left;
    FChart.LeftAxis.LabelsSize := 0;
    FChart.LeftAxis.TitleSize  := 0;
    FChart.RightAxis.LabelsSize := 0;
    FChart.RightAxis.TitleSize := 0;
    Exit; //==>
  end;

  case FScrollStyle of
  ssNone      : begin
                   FscbBottom.Visible  := false;
                   FscbLeft.Visible    := false;
                   FChart.Height       := self.ClientHeight;
                   FChart.Width        := self.Width - FChart.Left;
                   FChart.LeftAxis.LabelsSize := 0;
                   FChart.LeftAxis.TitleSize  := 0;
                   FChart.RightAxis.LabelsSize := 0;
                   FChart.RightAxis.TitleSize := 0;
                 end;
  ssHorizontal : begin
                   FscbBottom.Visible := true;
                   FscbLeft.Visible  := false;
                   FChart.Height     := self.Height - ciBorder - ciSCBWidth;
                   FChart.Width      := self.Width - FChart.Left;
                   FChart.LeftAxis.LabelsSize := 0;
                   FChart.LeftAxis.TitleSize  := 0;
                   FChart.RightAxis.LabelsSize := cuiAxisLabelSize;
                   FChart.RightAxis.TitleSize := cuiAxisTitleSize;
                 end;
  ssVertical  : begin
                   FscbBottom.Visible := false;
                   FscbLeft.Visible  := true;
                   FChart.Height     := self.ClientHeight;
                   FChart.Width      := self.Width - FChart.Left - ciBorder - ciSCBWidth;
                   FChart.LeftAxis.LabelsSize := cuiAxisLabelSize;
                   FChart.LeftAxis.TitleSize  := cuiAxisTitleSize;
                   FChart.RightAxis.LabelsSize := 0;
                   FChart.RightAxis.TitleSize := 0;
                 end;
  ssBoth      : begin
                   FscbBottom.Visible := true;
                   FscbLeft.Visible  := true;
                   FChart.Height     := self.Height - ciBorder - ciSCBWidth;
                   FChart.Width      := self.Width - FChart.Left - ciBorder - ciSCBWidth;
                   FChart.LeftAxis.LabelsSize := cuiAxisLabelSize;
                   FChart.LeftAxis.TitleSize  := cuiAxisTitleSize;
                   FChart.RightAxis.LabelsSize := cuiAxisLabelSize;
                   FChart.RightAxis.TitleSize := cuiAxisTitleSize;
                 end;
  end;
end;

procedure TtiTimeSeriesChart.Loaded;
begin
  inherited;
  if FbShowTestData then
    SetShowTestData(FbShowTestData);
  SetScrollStyle(FScrollStyle);
end;

procedure TtiTimeSeriesChart.SetShowTestData(const AValue: boolean);
begin
  FbShowTestData := AValue;
  if FbShowTestData then
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

{ TtiChartTestData }

procedure TtiChartTestData.AssignGraphData(AData: TObject; pChart: TtiTimeSeriesChart);
begin
  pChart.SeriesByName('Sin').AddXY(
    TtiChartTestDataItem(AData).XValue,
    TtiChartTestDataItem(AData).YValue1);
  pChart.SeriesByName('Cos').AddXY(
    TtiChartTestDataItem(AData).XValue,
    TtiChartTestDataItem(AData).YValue2);
end;

// Create a test gap as the Cos data goes above zero.
procedure TtiChartTestData.DataGap(ADataBeforeGap: TObject; ADataAfterGap: TObject; pChart: TtiTimeSeriesChart);
begin
  if (TtiChartTestDataItem(ADataBeforeGap).YValue2 <= 0) and
     (TtiChartTestDataItem(ADataAfterGap).YValue2 >= 0) then
    pChart.AddDateTimeGap('Cos', TtiChartTestDataItem(ADataBeforeGap).XValue, TtiChartTestDataItem(ADataAfterGap).XValue);
end;

constructor TtiChartTestData.create;
var
  i : integer;
  lData : TtiChartTestDataItem;
begin
  inherited;

  for i := 0 to 359 do begin
    lData := TtiChartTestDataItem.Create;
    lData.XValue := Date + i;
    lData.YValue1 := Sin(i/180*Pi);
    lData.YValue2 := Cos(i/180*Pi);
    Add(lData);
  end;

end;

function TtiTimeSeriesChart.GetOnDblClickChart: TNotifyEvent;
begin
  result := FChart.OnDblClick;
end;

procedure TtiTimeSeriesChart.SetOnDblClickChart(const AValue: TNotifyEvent);
begin
  FChart.OnDblClick := AValue;
end;

{ TtiChartDataMapping }

function TtiChartDataMapping.Clone: TtiChartDataMapping;
begin
  result := TtiChartDataMapping.Create(nil);
  result.DisplayLabel := DisplayLabel;
  result.PropertyName := PropertyName;
end;

constructor TtiChartDataMapping.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDisplayLabel := 'Caption';
  FPropertyName := 'Caption';
end;

destructor TtiChartDataMapping.Destroy;
begin
  inherited;
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

function TtiChartDataMappings.Add: TtiChartDataMapping;
begin
  result := TtiChartDataMapping(inherited add);
end;

procedure TtiChartDataMappings.Clear;
begin
  inherited;
end;

constructor TtiChartDataMappings.Create(Owner: TComponent);
begin
  inherited Create(TtiChartDataMapping);
  FOwner := Owner;
end;

destructor TtiChartDataMappings.Destroy;
begin
  inherited;
end;

function TtiChartDataMappings.FindByFieldName(psFieldName: string): TtiChartDataMapping;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].PropertyName = psFieldName then begin
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

procedure TtiChartDataMappings.NamesToStringList(pSL: TStringList);
var
  i : integer;
begin
  pSL.Clear;
  for i := 0 to count - 1 do
    pSL.Add(Items[i].PropertyName);
end;

procedure TtiChartDataMappings.SetItem(Index: integer; const AValue: TtiChartDataMapping);
begin
  inherited SetItem(Index, AValue);
end;

{ TtiChartLegendItem }

constructor TtiChartLegendItem.Create(Owner: TComponent);
begin
  inherited;
  ControlStyle  := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
  Height := cChartLegendItemHeight;
  Color := clWhite;
  OnClick := DoOnCheckBoxClick;
  FCheckBox := TCheckBox.Create(Self);
  FCheckBox.Parent := Self;
  FCheckBox.Top := (cChartLegendItemHeight - FCheckBox.Height) div 2;
  FCheckBox.Left := cChartLegendItemCheckBoxLeft;
  FCheckBox.OnClick := DoOnCheckBoxClick;
end;

destructor TtiChartLegendItem.Destroy;
begin
  inherited;
end;

procedure TtiChartLegendItem.DoOnCheckBoxClick(Sender: TObject);
begin
  Assert(FChartSeries<>nil, 'FChartSeries not assigned');
  Assert(FCheckBox<>nil, 'FCheckBox not assigned');
  Assert(FtiChart<>nil, 'FtiChart not assigned');
  if Sender is TtiChartLegendItem then
    FCheckBox.Checked := not FCheckBox.Checked;

  FChartSeries.Active := FCheckBox.Checked;
  FChartSeries.Pointer.Visible :=
    (FCheckBox.Checked and FtiChart.LineSeriesPointerVisible);
end;

procedure TtiChartLegendItem.Paint;
var
  lColor   : TColor;
  lRow     : integer;
  lCol     : integer;
  lPenWidth : integer;
begin
  inherited;
  if FChartSeries = nil then
    Exit; //==>
  // Setup the pen
  lColor := Canvas.Pen.Color;
  lPenWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FChartSeries.SeriesColor;
  // Draw horozontal line
  lRow := cChartLegendItemHeight div 2;
  Canvas.PenPos := Point(4, lRow);
  Canvas.LineTo(cChartLegendItemCheckBoxLeft - 4, lRow);
  // Draw vertical line
  lCol := (cChartLegendItemCheckBoxLeft - 8) div 2;
  Canvas.PenPos := Point(lCol, 4);
  Canvas.LineTo(lCol, cChartLegendItemHeight - 8);
  // Restore the pen properties
  Canvas.Pen.Color := lColor;
  Canvas.Pen.Width := lPenWidth;
end;

procedure TtiChartLegendItem.SetChartSeries(const AValue: TLineSeries);
begin
  FChartSeries := AValue;
  FCheckBox.Caption := AValue.Title;
  FCheckBox.Width:= Canvas.TextWidth(AValue.Title) + 20;
  FCheckBox.Checked := AValue.Active;
  FCheckBox.Font.Color := AValue.SeriesColor;
end;

procedure TtiTimeSeriesChart.SetDrawCrossHairs(const AValue: boolean);
begin
  FbDrawCrossHairs := AValue;
  Screen.Cursor := crDefault;
  DrawCrossHairsNow := AValue;
end;

function TtiTimeSeriesChart.LineSeriesPointerVisible: Boolean;
var
  lCount : Integer;
begin
  Result := (FData <> nil) and
            (FChart.Series[0] <> nil) and
            (FChart.Series[0] is TLineSeries);
  if not Result then
    Exit; //==>
  if FbZoomed then
  begin
    // Ensure visible count is correct if zoom level changed.
    FChart.Update;
    lCount := TLineSeries(FChart.Series[0]).VisibleCount;
    Result := (lCount <= cPointerVisibleLimit);
  end else
    Result := FData.Count <= cPointerVisibleLimit;
end;

procedure TtiTimeSeriesChart.SetSeriesPointerVisible(AValue: Boolean);
var
  i : Integer;
begin
  for i := 0 to FChart.SeriesCount - 1 do
    if (FChart.Series[i] is TLineSeries) then
      TLineSeries(FChart.Series[i]).Pointer.Visible := AValue and
      TLineSeries(FChart.Series[i]).Active;
end;

function TtiTimeSeriesChart.GetVisibleSeriesAsString: string;
var
  i: Integer;
  LSL: TStringList;
begin
  LSL:= TStringList.Create;
  try
    for i:= 0 to FChart.SeriesCount - 1 do
      if (FChart.Series[i] as TLineSeries).Active then
        LSL.Values[FChart.Series[i].Name]:= '1'
      else
        LSL.Values[FChart.Series[i].Name]:= '0';
    Result:= LSL.CommaText;
  finally
    LSL.Free;
  end;
end;

procedure TtiTimeSeriesChart.SetVisibleSeriesAsString(const AValue: string);
var
  i: Integer;
  LSL: TStringList;
  LSeries: TLineSeries;
begin
  LSL:= TStringList.Create;
  try
    LSL.CommaText:= AValue;
    for i:= 0 to FChart.SeriesCount - 1 do
    begin
      LSeries:= FChart.Series[i] as TLineSeries;
      if LSL.Values[FChart.Series[i].Name] = '0' then
      begin
        LSeries.Active := False;
        LSeries.Pointer.Visible := False;
      end else
      begin
        LSeries.Active := True;
        LSeries.Pointer.Visible := LineSeriesPointerVisible;
      end;
    end;
  finally
    LSL.Free;
  end;
end;

end.





