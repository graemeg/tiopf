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

  Created: Mid 1998

  Purpose: Wrapper for TChart which gives
           a) cross-hairs
           b) user configuration
           c) custom legend

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


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiChart;

interface

uses
  Controls
  ,Classes
  ,ExtCtrls
  ,Forms
  ,Graphics
  ,Buttons
  ,Windows
  ,ComCtrls
  ,StdCtrls
  ,Menus
  ,Contnrs
  ,Chart
  ,Series
  ,TeEngine
  ,TeeProcs
  ,TeeEdit
  ,TeeLisB
  ;

type

  TtiChart = class ;

  TtiChartTestDataItem = class( TObject )
  private
    FYValue2: real;
    FXValue: real;
    FYValue1: real;
  public
    property XValue  : real read FXValue  write FXValue  ;
    property YValue1 : real read FYValue1 write FYValue1 ;
    property YValue2 : real read FYValue2 write FYValue2 ;
  end ;

  TtiChartTestData = class( TObjectList )
  public
    constructor create ; virtual ;
    procedure   AssignGraphData(pData: TObject; pChart: TtiChart);
  end ;

  //----------------------------------------------------------------------------
  TtiChartDataMapping = class( TCollectionItem )
  private
    FDisplayLabel: string;
    FPropertyName: string;
    procedure SetPropertyName(const Value: string);
  protected
    function    GetDisplayName : string ; override ;
  published
    property    DisplayLabel : string read FDisplayLabel write FDisplayLabel ;
    property    PropertyName : string read FPropertyName write SetPropertyName ;
  public
    constructor Create( Collection : TCollection ) ; override ;
    destructor  Destroy ; override ;
    function    Clone : TtiChartDataMapping ;
//    procedure   Assign( Source : TPersistent ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TtiChartDataMappings = class( TCollection )
  private
    FOwner : TComponent ;
    function  GetItem( Index : integer ) : TtiChartDataMapping ;
    procedure SetItem( Index : integer ; const Value : TtiChartDataMapping ) ;
  published
  protected
    function  GetOwner : TPersistent ; override ;
  public
    constructor Create( Owner : TComponent ) ;
    destructor  Destroy ; override ;
    property  Items[Index: integer ] : TtiChartDataMapping
                read GetItem write SetItem ;
    function  Add : TtiChartDataMapping ;
    procedure NamesToStringList( pSL : TStringList ) ;
    function  FindByFieldName( psFieldName : string ) : TtiChartDataMapping ;
    procedure Clear ; reintroduce ;
  end ;

  // This thread is responsible for keeping an eye the cross hairs and clearing
  // them if the mouse is moved off the draw region of the graph.
  // ---------------------------------------------------------------------------
  TThrdGraphMonitor = class( TThread )
  private
    FtiChart : TtiChart ;
    procedure DoClearCrossHairs ;
  public
    Constructor CreateExt ;
    property    Chart : TtiChart read FtiChart write FtiChart ;
    procedure   Execute ; override ;
  end ;

  // ---------------------------------------------------------------------------
  TAssignGraphDataEvent = procedure ( pData  : TObject ;
                                      pChart : TtiChart ) of object;

  // ---------------------------------------------------------------------------
  TCrossHairEvent = procedure ( pSeries : TChartSeries ;
                                pIndex : integer ;
                                pData : TObject ;
                                pList : TList ) of object ;

  // An abstract base form for editing chart configuration, viewing legend, etc.
  // ---------------------------------------------------------------------------
  TtiChartEditForm = class( TForm )
  private
    FChart: TChart;
  protected
    procedure SetChart(Value: TChart); virtual ;
  public
    Constructor CreateNew( owner : TComponent ; Dummy : integer = 0 ) ; override ;
    property    Chart : TChart read FChart write SetChart ;
  end ;

  // Abstract base panel for displaying data in the RH pane of the edit form.
  // ---------------------------------------------------------------------------
  TtiChartChildPanel = class( TPanel )
  private
    FData : TObject ;
  protected
    function GetData : TObject ; virtual ;
    procedure SetData( Value : TObject ) ; virtual ;
  public
    Constructor Create( Owner : TComponent ) ; override ;
    property Data : TObject read GetData write SetData ;
  end ;

  // Child panel for editing chart level configuration
  // ---------------------------------------------------------------------------
  TtiChartChildPanelChart = class( TtiChartChildPanel )
  private
    FlblBottomAxis : TLabel ;
    FlblLeftAxis : TLabel ;
    FcbVGridLines : TCheckBox ;
    FcbHGridLines : TCheckBox ;
    procedure DoOnChange(Sender: TObject);
    procedure SetOnChange( pbValue : boolean ) ;
  protected
    procedure SetData( Value : TObject ) ; override ;
  public
    Constructor Create( Owner : TComponent ) ; override ;
  end ;

  // Child panel for editing series level configuration
  // ---------------------------------------------------------------------------
  TtiChartChildPanelLineSeries = class( TtiChartChildPanel )
  private
    FlblTitle : TLabel ;
    FcbSeriesVisible : TCheckBox ;
    //FgbPoints        : TGroupBox ;
    //FlblPointsStyle  : TLabel ;
    //FlblPointsColor  : TLabel ;
    //FcbPointStyle    : TComboBox ;
    //FcbPointColor    : TComboBox ;
    FcbPointsVisible : TCheckBox ;
    //FgbLine          : TGroupBox ;
    //FlblLineStyle    : TLabel ;
    //FlblLineColor    : TLabel ;
    //FcbLineStyle     : TComboBox ;
    //FcbLineColor     : TComboBox ;
    FcbLineVisible     : TCheckBox ;
    procedure DoOnChange(Sender: TObject);
    procedure SetControlEnabled;
    procedure SetOnChange( pbValue : boolean ) ;
  protected
    procedure SetData( Value : TObject ) ; override ;
  public
    Constructor Create( Owner : TComponent ) ; override ;
  end ;

  // Form to display a legend (a small graphic of the seris - with it's title)
  // ---------------------------------------------------------------------------
  TtiChartLegendForm = class( TtiChartEditForm )
  private
    FChartListBox : TChartListBox ;

//    procedure DrawLegendGraphic(pIndex: integer);
//    procedure DrawLegendText(pIndex: integer);
//    procedure DoOnPaint( sender : TObject ) ;
  protected
    procedure SetChart(Value: TChart); override ;
  public
    Constructor CreateNew( owner : TComponent ; Dummy : integer = 0 ) ; override ;
  end ;

  // Configure the appearance of the chart
  // ---------------------------------------------------------------------------
  TtiChartConfigForm = class( TtiChartEditForm )
  private
    FPnlButtons : TPanel ;
    FTV         : TTreeView ;
    FPnlParent  : TPanel ;
    FBtnClose   : TBitBtn ;
    FslPanels   : TStringList ;
    FCurrentChildPanel : TtiChartChildPanel ;

    procedure TVChange(Sender: TObject; Node: TTreeNode);
  protected
    procedure SetChart(Value: TChart); override ;
  public
    Constructor CreateNew( owner : TComponent ; Dummy : integer = 0 ) ; override ;
    Destructor  Destroy ; override ;
  end ;

  TtiChartInternal = class( TChart )
  protected
    procedure Paint ; override ;
  end ;


  // TtiChart class
  // ---------------------------------------------------------------------------
  TtiChart = class( TCustomPanel )
  private
    FChart: TChart ;
    FData: TList;
    FOnAssignGraphData: TAssignGraphDataEvent;
    FOnCrossHair : TCrossHairEvent ;
    FthrdMonitor : TThrdGraphMonitor ;
    FTestData : TtiChartTestData ;
    FChartDataMappings : TtiChartDataMappings ;

    FiOldX : integer ;
    FiOldY : integer ;
    FiOldCircX : integer ;
    FiOldCircY : integer ;
    FCurrentData: TPersistent;

    FbZoomed         : boolean      ;
    FsbZoomIn        : TSpeedButton ;
    FsbZoomOut       : TSpeedButton ;
    FsbDefaultZoom   : TSpeedButton ;
    FsbViewLegend    : TSpeedButton ;
    FsbConfig        : TSpeedButton ;
    FsbCopyToClipBrd : TSpeedButton ;

    FscbBottom       : TScrollBar   ;
    FscbLeft         : TScrollBar   ;

    FbCrossHairsDrawn : boolean ;
    FbDrawCrossHairs      : boolean;
    FbDrawCrossHairsNow   : boolean;
    FbDrawCrossHairsSaved : boolean ;

    FiSnapToDataSize: integer;
    FiSnapToDataRadius : integer ;
    FbTimeSeriesChart : boolean ;
    FrYValueUnderMouse: real;
    FDataUnderMouse: TObject ;
    FrXValueUnderMouse: real;
    FrXDataValueUnderMouse: real;
    FrYDataValueUnderMouse: real;
    FbDisplayTestData: boolean;

    FrBottomAxisMin : real ;
    FrBottomAxisMax : real ;
    FrLeftAxisMin   : real ;
    FrLeftAxisMax   : real ;
    FScrollStyle: TScrollStyle;
    FbShowTestData: boolean;

    procedure SetData(const Value: TList);
    function  GetView3D: boolean;
    procedure SetView3D(const Value: boolean);
    function  GetChartColor: TColor;
    procedure SetChartColor(const Value: TColor);
    procedure ClearCrossHairs ;
    procedure DoChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ReSetCrossHairs;
    procedure DoSBZoomInClick( sender : TObject ) ;
    procedure DoSBZoomOutClick( sender : TObject ) ;
    procedure DoSBDefaultZoomClick( sender : TObject ) ;
    procedure DoSBViewLegendClick( sender : TObject ) ;
    procedure DoSBConfigClick( sender : TObject ) ;
    procedure DoSBCopyToClipBrdClick( sender : TObject ) ;
    procedure OnDrawKey( pSeries : TChartSeries ;
                         pIndex  : integer ;
                         pData   : TObject ;
                         pList   : TList ) ;
    procedure DoOnZoom( Sender : TObject ) ;
    procedure DoOnScroll( Sender : TObject ) ;
    function  IsFormFocused: boolean;
    function  GetAxisBottom : TChartAxis;
    function  GetAxisLeft   : TChartAxis;
    function  GetMouseInCrossHairRegion: boolean;
    procedure SetSnapToDataSize(const Value: integer);
    function  GetChartPopupMenu: TPopupMenu;
    procedure SetChartPopupMenu(const Value: TPopupMenu);
    function  GetNextSeriesColor : TColor ;
    procedure DoOnBeforeDrawAxes( sender : TObject ) ;
    procedure SetTimeSeriesChart(const Value: boolean);
    procedure SnapEditDialogToButton(pForm: TForm; pSender: TObject);
    procedure SetDrawCrossHairsNow(const Value: boolean);
    procedure DoDrawCrossHairs(piX, piY: Integer ; pbDraw : boolean );
    procedure DoVerticalScroll( Sender : TObject ) ;
    procedure DoHorizontalScroll( Sender : TObject ) ;
    procedure AdjustScrollBarPositions;
    procedure SetScrollStyle(const Value: TScrollStyle);
    procedure SetShowTestData(const Value: boolean);
    function  GetOnDblClickChart: TNotifyEvent;
    procedure SetOnDblClickChart(const Value: TNotifyEvent);
    function  GetOwnerForm(Control: TComponent): TForm ;

    //procedure MouseToChartCoOrds(var piX, piY: integer);
    //procedure SetDisplayTestData(const Value: boolean);

  protected
    function    AddLineSeries( const psTitle : string ;
                               pbPointerVisible : boolean = true ) : TLineSeries ;
    function    AddBarSeries( const psTitle : string ) : TBarSeries ;
    procedure   Loaded ; override ;

  published
    property Align ;
    property Anchors ;
    property BevelInner  ;
    property BevelOuter  ;
    property BorderStyle ;
    property Color ;
    property ChartPopupMenu  : TPopupMenu   read GetChartPopupMenu  write SetChartPopupMenu ;
    property OnDblClickChart : TNotifyEvent read GetOnDblClickChart write SetOnDblClickChart ;

    property OnAssignGraphData : TAssignGraphDataEvent read FOnAssignGraphData write FOnAssignGraphData ;
    property OnCrossHair : TCrossHairEvent read FOnCrossHair write FOnCrossHair ;

    property View3D : boolean read GetView3D write SetView3D default false ;
    property ChartColor : TColor read GetChartColor write SetChartColor default clWhite ;
    property AxisBottom : TChartAxis read GetAxisBottom ;
    property AxisLeft   : TChartAxis read GetAxisLeft ;
    // A flag the user can set at design time to control the drawing of XHairs
    property DrawCrossHairs    : boolean read FbDrawCrossHairs    write FbDrawCrossHairs default true ;
    // An internal flat which is used to control the temporary turning on or off of XHairs
    property DrawCrossHairsNow : boolean read FbDrawCrossHairsNow write SetDrawCrossHairsNow ;
    property SnapToDataSize    : integer read FiSnapToDataSize    write SetSnapToDataSize default 15 ;
    property ScrollBars : TScrollStyle read FScrollStyle write SetScrollStyle default ssBoth ;
    property ShowTestData : boolean read FbShowTestData write SetShowTestData default false ;
    property ChartDataMappings : TtiChartDataMappings read FChartDataMappings ;

    //property DisplayTestData   : boolean read FbDisplayTestData   write SetDisplayTestData default false ;
  public
    constructor Create( Owner : TComponent ) ; override ;
    destructor  Destroy ; override ;

    property    Chart : TChart read FChart write FChart ;
    property    Data  : TList  read FData  write SetData ;
    property    CrossHairsDrawn : boolean read FbCrossHairsDrawn write FbCrossHairsDrawn default true ;
    property    XValueUnderMouse     : real read FrXValueUnderMouse     write FrXValueUnderMouse     ;
    property    YValueUnderMouse     : real read FrYValueUnderMouse     write FrYValueUnderMouse     ;
    property    XDataValueUnderMouse : real read FrXDataValueUnderMouse write FrXDataValueUnderMouse ;
    property    YDataValueUnderMouse : real read FrYDataValueUnderMouse write FrYDataValueUnderMouse ;
    property    DataUnderMouse       : TObject read FDataUnderMouse     ;

    function    AddDateTimeLineSeries( const psTitle    : string ;
                                       pbPointerVisible : boolean = true ) : TLineSeries ;
    procedure   AddDateTimeValues( const psSeriesName : string ;
                                   const pX: TDateTime;
                                   const pY: real);

    function    AddDateTimeBarSeries( const psTitle : string ) : TBarSeries ;

    function    SeriesByName( const psSeriesName : string ) : TChartSeries ;
    property    MouseInCrossHairRegion : boolean read GetMouseInCrossHairRegion ;
    property    TimeSeriesChart : boolean read FbTimeSeriesChart write SetTimeSeriesChart ;
    procedure   Clear ;
    procedure   RefreshSeries;

  end ;

//procedure Register ;

{$R tiChart.res}
//{$R tiPerAwareChart.dcr}

implementation
uses
  SysUtils
  ,Math
//  ,IEditCha
  ;

const
  // The TChart components assigns colours to the series in a strange order
  // This is the sequence of colour assignments.
  cuaSeriesColors : array[0..12] of TColor =
    ( clNavy, clRed, clGreen, clMaroon,
      clOlive, clPurple, clTeal, clLime,
      clBlue, clFuchsia, clAqua, clYellow,
      clBlack ) ;
  ciBorder   = 4  ;
  ciSCBWidth = 16 ; // Must be 16, will be changed by delphi at runtime if not :(
  cuiAxisLabelSize = 40 ;
  cuiAxisTitleSize = 20 ;

// Register with the component pallet
//------------------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents( 'TechInsite',
//                      [  TtiChart
//                      ]) ;
//end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChart
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChart.Create(Owner: TComponent);
const
  ciSBLeft   =  4 ;
  ciSBSize   = 20 ;
var
  liSBTop : integer ;

begin
  inherited Create( Owner ) ;

  ControlStyle := ControlStyle - [csSetCaption] ;
  BevelInner  := bvNone ;
  BevelOuter  := bvNone ;
  BorderStyle := bsNone ;
  Height      := 217 ;
  Width       := 285 ;
  Color       := clWhite ;

  FChartDataMappings := TtiChartDataMappings.Create( Self ) ;

  FChart := TtiChartInternal.Create( self ) ;
  with FChart do begin
    Parent := self ;
    //Align := alClient ;
    Anchors     := [akLeft,akTop,akRight,akBottom] ;
    Top := 0 ;
    Left := ciSBLeft * 2 + ciSBSize;
    Height := self.Height - ciBorder - ciSCBWidth ;
    Width  := self.Width - Left - ciBorder - ciSCBWidth ;

    BevelInner  := bvNone ;
    BevelOuter  := bvNone ;
    BorderStyle := bsNone ;
    Color       := clWhite ;
    Legend.Visible := false ;
    Title.Visible := false ;
    View3D := false ;

    // Set the BottomAxis properties
    BottomAxis.Title.Font.Color := clNavy ;
    BottomAxis.Grid.SmallDots := true ;
    BottomAxis.Grid.Color := clSilver ;
    BottomAxis.LabelsSize := cuiAxisLabelSize ;
    BottomAxis.TitleSize  := cuiAxisTitleSize ;

    // Set the LeftAxis properties
    LeftAxis.Title.Font.Color := clNavy ;
    LeftAxis.Grid.SmallDots := true ;
    LeftAxis.Grid.Color := clSilver ;
    LeftAxis.LabelsSize := cuiAxisLabelSize ;
    LeftAxis.TitleSize  := cuiAxisTitleSize ;

    ClipPoints  := true ;

    OnMouseMove            := DoChartMouseMove ;
    OnMouseDown            := DoMouseDown ;
    OnMouseUp              := DoMouseUp ;

    OnBeforeDrawAxes       := DoOnBeforeDrawAxes ;
    OnZoom                 := DoOnZoom ;
    OnScroll               := DoOnScroll ;
  end ;

  FscbBottom := TScrollBar.Create( self ) ;
  with FscbBottom do
  begin
    Parent   := self ;
    TabStop  := false ;
    Kind     := sbHorizontal ;
    Top      := FChart.Top + FChart.Height + ciBorder ;
    Left     := FChart.Left ;
    Height   := ciSCBWidth ;
    Width    := FChart.Width ;
    Anchors  := [akLeft,akRight,akBottom] ;
    Min      := 0 ;
    Max      := 100 ;
    Position := 0 ;
    PageSize := 100;
    OnChange := DoHorizontalScroll ;
    Visible  := false ;
  end ;

  FscbLeft := TScrollBar.Create( self ) ;
  with FscbLeft do
  begin
    Parent   := self ;
    TabStop  := false ;
    Kind     := sbVertical ;
    Top      := FChart.Top ;
    Left     := FChart.Left + FChart.Width + ciBorder ;
    Height   := FChart.Height ;
    Width    := ciSCBWidth ;
    Anchors  := [akRight,akTop,akBottom] ;
    Min      := 0 ;
    Max      := 100 ;
    Position := 0 ;
    PageSize := 100;
    OnChange := DoVerticalScroll ;
    Visible  := false ;
  end ;

  liSBTop := 10 ;

  FsbZoomIn      := TSpeedButton.Create( self ) ;
  with FsbZoomIn do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    liSBTop := liSBTop + ciSBLeft + ciSBSize ;
    Flat    := true ;
    Color   := Self.Color ;
    // If LoadFromResourceName is not working in packages, try this
    // FindResourceHInstance(SysInit.HInstance)
    Glyph.LoadFromResourceName( HInstance, 'tiChart_ZoomIn' ) ;
    Hint   := 'Zoom in' ;
    ShowHint := true ;
    OnClick := DoSBZoomInClick ;
  end ;

  FsbZoomOut     := TSpeedButton.Create( self ) ;
  With FsbZoomOut do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    liSBTop := liSBTop + ciSBLeft + ciSBSize ;
    Flat    := true ;
    Color   := Self.Color ;
    Glyph.LoadFromResourceName( HInstance, 'tiChart_ZoomOut' ) ;
    Hint   := 'Zoom out' ;
    ShowHint := true ;
    OnClick := DoSBZoomOutClick ;
  end ;

  FsbDefaultZoom := TSpeedButton.Create( self ) ;
  With FsbDefaultZoom do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    liSBTop := liSBTop + ciSBLeft + ciSBSize ;
    Flat    := true ;
    Color   := Self.Color ;
    Glyph.LoadFromResourceName( HInstance, 'tiChart_ZoomDefault' ) ;
    Hint   := 'Undo zoom' ;
    ShowHint := true ;
    OnClick := DoSBDefaultZoomClick ;
  end ;

  FsbViewLegend  := TSpeedButton.Create( self ) ;
  With FsbViewLegend do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    liSBTop := liSBTop + ciSBLeft + ciSBSize ;
    Flat    := true ;
    Color   := Self.Color ;
    Glyph.LoadFromResourceName( HInstance, 'tiChart_ViewLegend' ) ;
    Hint   := 'View legend' ;
    ShowHint := true ;
    OnClick := DoSBViewLegendClick ;
  end ;

  FsbConfig      := TSpeedButton.Create( self ) ;
  With FsbConfig do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    liSBTop := liSBTop + ciSBLeft + ciSBSize ;
    Flat    := true ;
    Color := self.color ;
    Glyph.LoadFromResourceName( HInstance, 'tiChart_Configure' ) ;
    Hint   := 'Configure' ;
    ShowHint := true ;
    OnClick := DoSBConfigClick ;
  end ;

  FsbCopyToClipBrd := TSpeedButton.Create( self ) ;
  With FsbCopyToClipBrd do begin
    Parent := self ;
    Top    := liSBTop ;
    Left   := ciSBLeft ;
    Height := ciSBSize ;
    Width  := ciSBSize ;
    Flat    := true ;
    Color := self.color ;
    Glyph.LoadFromResourceName( HInstance, 'tiChart_CopyToClipBrd' ) ;
    Hint   := 'Copy to clipboard' ;
    ShowHint := true ;
    OnClick := DoSBCopyToClipBrdClick ;

  end ;

  FbDrawCrossHairs     := true ;
  FbDrawCrossHairsNow  := true ;
  ResetCrossHairs ;
  FbCrossHairsDrawn := false ;
  SnapToDataSize    := 15 ;
  FbTimeSeriesChart := false ;
  FbDisplayTestData := false ;

  FthrdMonitor := TThrdGraphMonitor.CreateExt ;
  FthrdMonitor.Chart := self ;

  FrBottomAxisMin := MaxInt ;
  FrBottomAxisMax := -MaxInt ;
  FrLeftAxisMin   := MaxInt ;
  FrLeftAxisMax   := -MaxInt ;

  FScrollStyle    := ssBoth ;
  FbShowTestData  := false ;
  FbZoomed        := false ;

end;

// -----------------------------------------------------------------------------
destructor TtiChart.Destroy;
begin
  FthrdMonitor.Free ;
  FTestData.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
function TtiChart.GetChartColor: TColor;
begin
  result := FChart.Color ;
end;

// -----------------------------------------------------------------------------
function TtiChart.GetView3D: boolean;
begin
  Result := FChart.View3D;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetChartColor(const Value: TColor);
begin
  FChart.Color := Value ;
  Color        := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetData(const Value: TList);
var
  i : integer ;
begin

  if not FThrdMonitor.Suspended then
    FthrdMonitor.Suspend ;

  DrawCrossHairsNow := false ;

  try
    // The data object is nil
    if Value = nil then begin
      FChart.SeriesList.Clear ;
      FData := nil ;
      Exit ; //==>
    end;

    FData := Value;

    // If there are more than 100 data points then scan through all the series
    // and turn their check marks off.
    if ( FData <> nil ) and
       ( FData.Count >= 100 ) then
      for i := 0 to FChart.SeriesCount - 1 do
        if ( FChart.Series[i] is TLineSeries ) then
          TLineSeries(FChart.Series[i]).Pointer.Visible := false ;

    try
      if Assigned( FOnAssignGraphData ) then
        for i := 0 to FData.Count - 1 do
          FOnAssignGraphData( TObject( FData.Items[i] ),
                              Self ) ;
    except
      on e:exception do
        raise exception.create( 'Error in TtiChart.SetData ' +
                                'Message: ' + e.message ) ;
    end ;

    // Call RefreshSeries for all series
    // This is necessary for moving average series
    RefreshSeries ;

    FbZoomed := false ;
    AdjustScrollBarPositions ;

    {$IFDEF DeactivateCrossHairs }
    if ( not ( csDesigning in ComponentState )) then
      FthrdMonitor.Resume ;
    {$ENDIF}

  finally
    DrawCrossHairsNow := true ;
  end ;

end;


// -----------------------------------------------------------------------------
procedure TtiChart.RefreshSeries ;
var
  i : integer ;
begin
  for i := 0 to FChart.SeriesCount - 1 do
    FChart.Series[i].RefreshSeries ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChart.SetView3D(const Value: boolean);
begin
  FChart.View3D := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.ClearCrossHairs;
begin
  DoChartMouseMove( self,
                    [],
                    -1, -1 ) ;
end;

// -----------------------------------------------------------------------------
  // This procedure draws the crosshair lines
Procedure TtiChart.DoDrawCrossHairs(piX,piY:Integer ; pbDraw : boolean );
begin

  if FbCrossHairsDrawn = pbDraw then
    Exit ; //==>

  if Screen.Cursor <> crNone then
    Screen.Cursor := crNone ;

  FbCrossHairsDrawn := pbDraw ;
  FChart.Canvas.Pen.Color := clYellow ;
  FChart.Canvas.Pen.Mode  := pmXor    ;
  FChart.Canvas.Pen.Style := psSolid  ;
  FChart.Canvas.Pen.Width := 1        ;

  // Draw the vertical line
  FChart.Canvas.MoveTo(piX,FChart.ChartRect.Top-FChart.Height3D);
  FChart.Canvas.LineTo(piX,FChart.ChartRect.Bottom-FChart.Height3D);

  // Draw the horizontal line
  FChart.Canvas.MoveTo(FChart.ChartRect.Left+FChart.Width3D,piY);
  FChart.Canvas.LineTo(FChart.ChartRect.Right+FChart.Width3D,piY);

end;

procedure TtiChart.DoChartMouseMove( Sender: TObject;
                                     Shift: TShiftState;
                                     X, Y: Integer);

  Procedure DrawCircle(AX,AY:Integer);
  begin
    With Chart,Canvas do
    begin
      // You have to enter the complimentary colour of what you want !
      Pen.Color := clGray ;
      Pen.Style := psSolid  ;
      Pen.Mode  := pmXor    ;
      Pen.Width := 1        ;
      Brush.Style := bsClear ;

      Ellipse( aX-FiSnapToDataRadius,
               aY-FiSnapToDataRadius,
               aX+FiSnapToDataRadius,
               aY+FiSnapToDataRadius ) ;

      FiOldCircX := aX ;
      FiOldCircY := aY ;

    end;
  end;

  function GetDataObject( pIndex : Integer ) : TObject ;
  begin
    if ( FData <> nil ) and
       ( pIndex <= FData.Count-1 ) then
      result := TObject( FData.Items[pIndex] )
    else
      result := nil ;
  end ;

Var
  lSeries       : TChartSeries ;
  liDataIndex   : integer ;
  i : integer ;
  lPoint:TPoint;
begin

  if FData = nil then
    Exit ; //==>

  lPoint.X := Mouse.CursorPos.X ;
  lPoint.Y := Mouse.CursorPos.Y ;
  lPoint   := FChart.ScreenToClient( lPoint ) ;

  // Clear the values under the mouse cursor which are surfaced as
  // properties of TtiChart
  XValueUnderMouse     := 0 ;
  YValueUnderMouse     := 0 ;
  XDataValueUnderMouse := 0 ;
  YDataValueUnderMouse := 0 ;

  if Chart.SeriesCount = 0 then
    exit ; //==>

  // Set the values under the mouse cursor, based on the first chart series
  XValueUnderMouse := Chart.Series[0].XScreenToValue( x ) ;
  YValueUnderMouse := Chart.Series[0].YScreenToValue( x ) ;

  FCurrentData := nil ;

  // Erase the old cross hairs
  if ( FiOldX <> -1 ) then begin
    DoDrawCrossHairs( FiOldX, FiOldY, false );
    FiOldX := -1 ;
  end;

  // Erase old circle
  if ( FiOldCircX <> -1 ) then begin
    DrawCircle( FiOldCircX, FiOldCircY ) ;
    FiOldCircX := -1 ;
  end ;

  // type TShiftState = set of (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);

  // Check if mouse is inside Chart rectangle
  if ( not ( GetMouseInCrossHairRegion and
             DrawCrossHairs and
             DrawCrossHairsNow )) or
     ( ssLeft in Shift ) then begin
    Screen.Cursor := crDefault ;
    OnDrawKey( nil, -1, nil, nil ) ;
    Exit ;
  end ;

  // Draw crosshair at current position
  DoDrawCrossHairs( X, Y, true ) ;

  // Store old position
  FiOldX := X ;
  FiOldY := Y ;

  // Scan all the series looking for the closest data point
  // If found, lSeries and liDataIndex will have been set.
  liDataIndex := -1 ;
  lSeries := nil ;
  for i := 0 to FChart.SeriesCount - 1 do begin
    lSeries := FChart.Series[i] ;
    liDataIndex := lSeries.GetCursorValueIndex ;
    if liDataIndex <> -1 then
      Break ; //==>
  end ;

  FDataUnderMouse       := nil ;

  // A data point was found close to the mouse cursor, so set some values
  if ( liDataIndex <> -1 ) and
     ( lSeries <> nil ) then
  begin
    XDataValueUnderMouse := lSeries.XValues[liDataIndex] ;
    YDataValueUnderMouse := lSeries.YValues[liDataIndex] ;
    if liDataIndex < FData.Count then
      FDataUnderMouse       := TObject( FData.Items[liDataIndex]) ;

    DrawCircle( lSeries.CalcXPosValue( XDataValueUnderMouse ),
                lSeries.CalcYPosValue( YDataValueUnderMouse )) ;

    OnDrawKey( lSeries,
               liDataIndex,
               DataUnderMouse,
               FData ) ;
  end else begin
    OnDrawKey( nil, -1, nil, nil ) ;
  end ;

  Application.ProcessMessages ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.ReSetCrossHairs ;
begin
  FiOldX := -1 ;
  FiOldY := -1 ;
  FiOldCircX := -1 ;
  FiOldCircY := -1 ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBConfigClick(sender: TObject);
var
  lForm : TtiChartConfigForm ;
//  lChartEditor : TChartEditor ;
  lbDrawCrossHairs : boolean ;
begin

  lbDrawCrossHairs := DrawCrossHairs ;
  DrawCrossHairs := false ;
  try
    lForm := TtiChartConfigForm.CreateNew( nil ) ;
    try
      SnapEditDialogToButton( lForm, Sender ) ;
      lForm.Chart := Self.Chart ;
      lForm.ShowModal ;
    finally
      lForm.Free ;
    end ;

{
    lChartEditor := TChartEditor.Create( nil ) ;
    try
      with lChartEditor do
      begin
        HideTabs := [
//                      cetMain,
                      cetGeneral,
//                      cetAxis,
                      cetTitles,
                      cetLegend,
                      cetPanel,
                      cetPaging,
                      cetWalls,
                      cet3D,
                      cetSeriesGeneral,
                      cetSeriesMarks
                    ] ;
        DefaultTab := cetGeneral ;
        Options := [
//                      ceAdd,
//                      ceDelete,
                      ceChange,
                      ceClone,
//                      ceDataSource,
                      ceTitle
//                      ceHelp
                    ] ;
        Chart := FChart ;
        Execute ;
      end ;
    finally
      lChartEditor.Free ;
    end ;
}
  finally
    DrawCrossHairs := lbDrawCrossHairs ;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBDefaultZoomClick(sender: TObject);
begin
  FbDrawCrossHairsSaved := DrawCrossHairs ;
  DrawCrossHairs := false ;
  try
    AxisBottom.Automatic := true ;
    Chart.UnDoZoom ;
    FbZoomed := false ;
    AdjustScrollBarPositions ;
  finally
    DrawCrossHairs := FbDrawCrossHairsSaved ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.SnapEditDialogToButton( pForm : TForm ;
                                           pSender : TObject ) ;
var
  lSB : TControl ;
  lPoint : TPoint ;
begin
  Assert( pSender is TControl, 'Sender not a TButton' ) ;
  lSB := TControl( pSender ) ;
  lPoint.x := lSB.Left ;
  lPoint.y := lSB.Top ;
  lPoint := lSB.Parent.ClientToScreen( lPoint ) ;
  pForm.Top  := lPoint.Y ;
  pForm.Left := lPoint.X ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBViewLegendClick(sender: TObject);
var
  lForm : TtiChartLegendForm ;
  lbDrawCrossHairs : boolean ;
begin

  lbDrawCrossHairs := DrawCrossHairs ;
  DrawCrossHairs := false ;
  try
    lForm := TtiChartLegendForm.CreateNew( nil ) ;
    try
      SnapEditDialogToButton( lForm, Sender ) ;
      lForm.Chart := FChart ;
      lForm.ShowModal ;
    finally
      lForm.Free ;
    end ;
    Application.ProcessMessages ;
  finally
    DrawCrossHairs := lbDrawCrossHairs ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBZoomInClick(sender: TObject);
begin
  Chart.ZoomPercent( 105 ) ;
  AdjustScrollBarPositions ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBZoomOutClick(sender: TObject);
begin
  Chart.ZoomPercent( 90 ) ;
  AdjustScrollBarPositions ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.OnDrawKey( pSeries : TChartSeries ;
                              pIndex : integer ;
                              pData: TObject;
                              pList: TList);
begin
  if Assigned( FOnCrossHair ) then
    FOnCrossHair( pSeries, pIndex, pData, pList ) ;
end;

// Replace this with a call to the delphi func GetParentForm( )
// -----------------------------------------------------------------------------
function TtiChart.GetOwnerForm( Control : TComponent ) : TForm ;
  function _GetOwner( Control : TComponent ) : TComponent ;
  begin
    result := Control.Owner ;
    if ( result <> nil ) and
       ( not ( result is TForm )) then
      result := _GetOwner( result ) ;
  end ;
var
  lOwner : TComponent ;
begin
  lOwner := Control.Owner ;
  if ( lOwner <> nil ) and
     ( not ( lOwner is TForm )) then
    lOwner := GetOwnerForm( lOwner ) ;
  result := TForm( lOwner ) ;    
end ;

// Determine if the tiChart's parent form is currently focused.
// -----------------------------------------------------------------------------
function TtiChart.IsFormFocused: boolean;
var
  lForm : TForm ;
begin

  result := true ;

  // This may AV when used in an ActiveX
  lForm := TForm( GetOwnerForm( self )) ;

  // Added ( and assert removed ) for ActiveX deployment
  if lForm = nil then
    Exit ; //==>
  //Assert( lForm <> nil, 'Owner form not found' ) ;

  // This will return the correct answere if
  // a) An MDIChildForm is active, and the application is deactiveate
  // b) An MDIChildForm is active, and another MIDChild form is made active
  // but will not return the correct answer if
  // c) An MIDChildForm is active, and a ModalDialog is activated over it.
  // Non MDIChildForms, non modal dialogs have not been tested.
  case ( lForm.FormStyle ) of
  fsMDIChild  : result := ( Application.Active ) and
                          ( Application.MainForm.ActiveMDIChild = lForm ) ;
  // Form.Active will not work for an MDIForm. Must use Application.Active,
  // but this will return true if one of the child forms is active, but the
  // main form is not.
  //  fsMDIForm   : result := ( lForm.Active ) and ( Application.Active ) ;
  fsNormal    : result := ( lForm.Active ) ;
  fsStayOnTop : result := ( lForm.Active ) ;
  else
    raise exception.create( 'Invalid FormStyle passed to TtiChart.IsFormFocused' ) ;
  end ;
end;

// -----------------------------------------------------------------------------
function TtiChart.AddDateTimeLineSeries( const psTitle : string ;
                                         pbPointerVisible : boolean = true ) : TLineSeries ;
begin
  TimeSeriesChart := true ;
  result := AddLineSeries( psTitle, pbPointerVisible ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.AddDateTimeValues( const psSeriesName : string ;
                                      const pX: TDateTime;
                                      const pY: real);
begin
  FrBottomAxisMin := Min( FrBottomAxisMin, pX ) ;
  FrBottomAxisMax := Max( FrBottomAxisMax, pX ) ;

  FrLeftAxisMin := Min( FrLeftAxisMin, pY ) ;
  FrLeftAxisMax := Max( FrLeftAxisMax, pY ) ;

  SeriesByName( psSeriesName ).AddXY( pX, pY ) ;
end;

// -----------------------------------------------------------------------------
function TtiChart.AddLineSeries( const psTitle : string ;
                                 pbPointerVisible : boolean = true ) : TLineSeries ;
var
  lSeries : TLineSeries ;
begin

  lSeries := TLineSeries.Create( FChart ) ;
  lSeries.Title := psTitle ;
  lSeries.Pointer.Style := psCross ;
  lSeries.Pointer.Visible := pbPointerVisible ;
  lSeries.Pointer.InflateMargins := false ;
  lSeries.XValues.DateTime := true ;

  lSeries.SeriesColor := GetNextSeriesColor ;

  FChart.AddSeries( lSeries ) ;
  result := lSeries ;

end;

function TtiChart.GetNextSeriesColor : TColor ;
var
  liColorIndex : integer ;
begin
  liColorIndex := FChart.SeriesCount ;
  while liColorIndex > High( cuaSeriesColors ) do
    liColorIndex := liColorIndex - High( cuaSeriesColors ) ;
  result := cuaSeriesColors[liColorIndex] ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChart.DoSBCopyToClipBrdClick(sender: TObject);
begin
  FChart.CopyToClipboardMetafile(True);  { <--- Enhanced Metafile = True }
end;

// -----------------------------------------------------------------------------
function TtiChart.AddDateTimeBarSeries(const psTitle: string ): TBarSeries;
begin
  TimeSeriesChart := true ;
  result          := AddBarSeries( psTitle ) ;
end ;

// -----------------------------------------------------------------------------
function TtiChart.AddBarSeries(const psTitle: string ): TBarSeries;
var
  lSeries : TBarSeries ;
begin
  lSeries := TBarSeries.Create( FChart ) ;
  lSeries.Title := psTitle ;
  lSeries.Marks.Visible := false ;
  lSeries.BarWidthPercent := 100 ; // Perhaps this should be a param ?
  FChart.AddSeries( lSeries ) ;
  result := lSeries ;
end;

// -----------------------------------------------------------------------------
function TtiChart.GetAxisBottom: TChartAxis;
begin
  result := FChart.BottomAxis ;
end;

// -----------------------------------------------------------------------------
function TtiChart.GetAxisLeft: TChartAxis;
begin
  result := FChart.LeftAxis ;
end;

// -----------------------------------------------------------------------------
function TtiChart.SeriesByName(const psSeriesName: string): TChartSeries;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FChart.SeriesCount - 1 do begin
    if SameText( FChart.Series[i].Title, psSeriesName ) then begin
      result := FChart.Series[i] ;
      Break ; //==>
    end ;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdGraphMonitor
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TThrdGraphMonitor.CreateExt;
begin
  Create( true ) ;
  self.Priority := tpLower ;
end;

// -----------------------------------------------------------------------------
procedure TThrdGraphMonitor.DoClearCrossHairs;
var
  lbIsFormFocused          : boolean ;
  lbMouseInCrossHairRegion : boolean ;
  lbDrawCrossHairs         : boolean ;
begin
  lbDrawCrossHairs         := FtiChart.DrawCrossHairs ;
  if not lbDrawCrossHairs then
  begin
    Chart.DrawCrossHairsNow  := false ;
    Exit ; //==>
  end ;

  lbIsFormFocused          := FtiChart.IsFormFocused ;
  lbMouseInCrossHairRegion := FtiChart.MouseInCrossHairRegion ;

  Chart.DrawCrossHairsNow  :=
    lbMouseInCrossHairRegion and
    lbIsFormFocused and
    lbDrawCrossHairs ;
end;

// -----------------------------------------------------------------------------
procedure TThrdGraphMonitor.Execute;
begin
  while not Terminated do begin
    Sleep( 100 ) ;
    Synchronize( DoClearCrossHairs ) ;
  end ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartEditForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartEditForm.CreateNew(owner: TComponent; Dummy: integer);
begin
  inherited CreateNew( Owner, Dummy ) ;
  BorderStyle := bsDialog ;
  Position    := poDesigned ;
  BorderIcons := [biSystemMenu] ;
  Caption     := ' Configure chart' ;
end;

procedure TtiChartEditForm.SetChart( Value : TChart ) ;
begin
  FChart := Value ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartLegendForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartLegendForm.CreateNew(owner: TComponent ; Dummy : integer = 0 );
begin
  inherited CreateNew( Owner ) ;
//  BorderStyle := bsToolWindow ;
//  FormStyle   := fsStayOnTop ;
//  Canvas.Brush.Color := Color ;
//  OnPaint := DoOnPaint ;
//  Color := clWhite ;

  Height := 300 ;
  Width  := 200 ;

//  BorderStyle   := bsDialog ;
//  BorderIcons   := [biSystemMenu] ;
//  FPanel        := TPanel.Create( self ) ;
//  FPanel.Parent := self ;
//  FPanel.Align  := alClient ;

  FChartListBox := TChartListBox.Create( self ) ;
  FChartListBox.Parent := self ;
  FChartListBox.Align := alClient ;
  FChartListBox.ShowSeriesIcon := false ;
  
end;

// -----------------------------------------------------------------------------
{
procedure TtiChartLegendForm.DrawLegendGraphic( pIndex : integer ) ;
var
  liTop    : integer ;
  liLeft   : integer ;
  liWidth  : integer ;
  liAxis   : integer ;
  liHeight : integer ;
begin
  liTop    := pIndex * ( 8 + Canvas.TextHeight( 'M' )) + 8 ;
  liLeft   := 8 ;
  liWidth  := 20 ;
  liHeight := Canvas.TextHeight( 'M' ) ;
  liAxis   := liTop + liHeight div 2 ;

  Canvas.Pen.Color := Chart.Series[pIndex].SeriesColor ;
  Canvas.MoveTo( liLeft, liAxis ) ;
  Canvas.LineTo( liLeft + liWidth, liAxis ) ;
  Canvas.MoveTo( liLeft + ( liWidth div 2 ), liTop ) ;
  Canvas.LineTo( liLeft + ( liWidth div 2 ), liTop + liHeight ) ;

end;

// -----------------------------------------------------------------------------
procedure TtiChartLegendForm.DrawLegendText( pIndex : integer ) ;
var
  liTop : integer ;
begin
  liTop := pIndex * ( 8 + Canvas.TextHeight( 'M' )) + 8 ;
  Canvas.TextOut( 40, liTop, Chart.Series[pIndex].Title ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartLegendForm.DoOnPaint(sender: TObject);
var
  i : integer ;
begin
  for i := 0 to Chart.SeriesCount - 1 do begin
    DrawLegendGraphic( i ) ;
    DrawLegendText( i ) ;
  end ;
end ;
}

// -----------------------------------------------------------------------------
procedure TtiChartLegendForm.SetChart( Value: TChart);
var
  i : integer ;
  liWidth : integer ;
begin
  Inherited SetChart( Value ) ;
  FChart := Value;
  FChartListBox.Chart := FChart ;

  //ClientHeight := ( Chart.SeriesCount ) *
  //                ( Canvas.TextHeight( 'M' ) + 8 ) + 8 ;
  liWidth := 0 ;
  for i := 0 to Chart.SeriesCount - 1 do
    liWidth := Max( liWidth, Canvas.TextWidth( Chart.Series[i].Title )) ;
  ClientWidth := 58 + liWidth ;


end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartConfigForm
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartConfigForm.CreateNew(owner: TComponent;
                                         Dummy: integer = 0);
begin
  inherited CreateNew( Owner, Dummy ) ;
  Width  := 441 ;
  Height := 325 ;

  // Create a list to hold the child forms
  FslPanels := TStringList.Create;

  // Add an instance of each child form to the list
  FslPanels.AddObject( TLineSeries.ClassName, TtiChartChildPanelLineSeries.Create( nil )) ;
  FslPanels.AddObject( TChart.ClassName,      TtiChartChildPanelChart.Create( nil )) ;

  FPnlButtons := TPanel.Create( self ) ;
  with FPnlButtons do begin
    Parent := self ;
    Align       := alBottom ;
    BorderStyle := bsNone ;
    BevelOuter  := bvNone ;
    BevelInner  := bvNone ;
  end ;

  FBtnClose := TBitBtn.Create( self ) ;
  with FBtnClose do begin
    parent := FPnlButtons ;
    Top    := 4 ;
    Left   := FPnlButtons.ClientWidth - Width - 4 ;
    Kind   := bkCancel ;
    Caption := '&Close' ;
    Default := true ;
    Cancel  := true ;
  end ;

  FPnlButtons.ClientHeight := FBtnClose.Height + 8 ;

  FTV := TTreeView.Create( self ) ;
  with FTV do begin
    Parent := self ;
    Align := alLeft ;
    OnChange := TVChange ;
    Width := 150 ;
  end ;

  FPnlParent := TPanel.Create( self ) ;
  with FPnlParent do begin
    Parent := self ;
    Align       := alClient ;
    BorderStyle := bsNone ;
    BevelOuter  := bvNone ;
    BevelInner  := bvNone ;
  end ;

end;

// -----------------------------------------------------------------------------
destructor TtiChartConfigForm.Destroy;
var
  i : integer ;
begin
  for i := 0 to FslPanels.Count - 1 do
    TObject( FslPanels.Objects[i] ).Free ;
  FslPanels.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiChartConfigForm.SetChart( Value: TChart);
var
  lnodeChart  : TTreeNode ;
  lnodeSeries : TTreeNode ;
  i : integer ;
begin

  Inherited SetChart( Value ) ;

  // Clear any items already in the TreeView
  FTV.Items.Clear ;

  // Add the top node
  lNodeChart  := FTV.Items.AddObject( nil, 'Chart', FChart ) ;

  // Add a node for the series
  lNodeSeries := FTV.Items.AddChildObject( lNodeChart, 'Series', FChart.SeriesList ) ;

  // Add each series in the chart
  for i := 0 to FChart.SeriesList.Count - 1 do
    FTV.Items.AddChildObject( lNodeSeries, FChart.Series[i].Title, FChart.Series[i] ) ;

  // Expand the tree
  FTV.FullExpand ;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartChildPanel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartChildPanel.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  BorderStyle := bsNone ;
  BevelOuter  := bvNone ;
  BevelInner  := bvNone ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartChildPanelLineSeries
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartChildPanelLineSeries.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;

  FlblTitle := TLabel.Create( self ) ;
  with FlblTitle do begin
    Parent  := self ;
    Left    :=  8 ;
    Top     :=  4 ;
    Width   := 51 ;
    Height  := 13 ;
    Caption := 'Series title:' ;
  end ;

  FcbSeriesVisible := TCheckBox.Create( self ) ;
  with FcbSeriesVisible do begin
    Parent := self ;
    Left   := 8 ;
    Top := 24 ;
    Width := 97 ;
    Height := 17 ;
    Caption := '&Series visible' ;
    TabOrder := 3 ;
    OnClick := DoOnChange ;
  end ;

{
  FgbPoints:= TGroupBox.Create( self ) ;
  with FgbPoints do begin
    Parent := self ;
    Left := 28 ;
    Top := 72 ;
    Width := 173 ;
    Height := 81 ;
    Caption := ' Points ' ;
    TabOrder := 4 ;
  end ;

  FlblPointsStyle := TLabel.Create( self ) ;
  with FlblPointsStyle do begin
    Parent := FgbPoints ;
    Left := 12 ;
    Top := 24 ;
    Width := 23 ;
    Height := 13 ;
    Caption := '&Style' ;
  end ;

  FlblPointsColor := TLabel.Create( self ) ;
  with FlblPointsColor do begin
    Parent := FgbPoints ;
    Left := 12 ;
    Top := 48 ;
    Width := 30 ;
    Height := 13 ;
    Caption := '&Colour' ;
  end ;

  FcbPointStyle := TComboBox.Create( self ) ;
  with FcbPointStyle do begin
    Parent := FgbPoints ;
    Left := 52 ;
    Top := 20 ;
    Width := 109 ;
    Height := 21 ;
    Style := csDropDownList ;
    ItemHeight := 13 ;
    TabOrder := 0 ;
  end ;

  FcbPointColor := TComboBox.Create( self ) ;
  with FcbPointStyle do begin
    Parent := FgbPoints ;
    Left := 52 ;
    Top := 48 ;
    Width := 109 ;
    Height := 21 ;
    Style := csDropDownList ;
    ItemHeight := 13 ;
    TabOrder := 1 ;
  end ;
}

  FcbPointsVisible:= TCheckBox.Create( self ) ;
  with FcbPointsVisible do begin
    Parent := self ;
    Left := 16 ;
    Top := 48 ;
    Width := 97 ;
    Height := 17 ;
    Caption := '&Points visible' ;
    TabOrder := 5 ;
    OnClick := DoOnChange ;
  end ;

{
  FgbLine:= TGroupBox.Create( self ) ;
  with FgbLine do begin
    Parent := self ;
    Left := 224 ;
    Top := 72 ;
    Width := 173 ;
    Height := 81 ;
    Caption := ' Line ' ;
    TabOrder := 6 ;
  end ;

  FlblLineStyle := TLabel.Create( FgbLine ) ;
  with FlblLineStyle do begin
    Parent := FgbLine ;
    Left := 12 ;
    Top := 24 ;
    Width := 23 ;
    Height := 13 ;
    Caption := '&Style' ;
  end ;

  FlblLineColor := TLabel.Create( FgbLine ) ;
  with FlblLineColor do begin
    Parent := FgbLine ;
    Left := 12 ;
    Top := 48 ;
    Width := 30 ;
    Height := 13 ;
    Caption := '&Colour' ;
  end ;

  FcbLineStyle := TComboBox.Create( FgbLine ) ;
  with FcbLineStyle do begin
    Parent := FgbLine ;
    Left := 52 ;
    Top := 20 ;
    Width := 109 ;
    Height := 21 ;
    Style := csDropDownList ;
    ItemHeight := 13 ;
    TabOrder := 0 ;
  end ;

  FcbLineColor := TComboBox.Create( FgbLine ) ;
  with FcbLineColor do begin
    Parent := FgbLine ;
    Left := 52 ;
    Top := 48 ;
    Width := 109 ;
    Height := 21 ;
    Style := csDropDownList ;
    ItemHeight := 13 ;
    TabOrder := 1 ;
  end ;
}

  FcbLineVisible := TCheckBox.Create( self ) ;
  with FcbLineVisible do begin
    Parent := self ;
    Left := 16 ;
    //Top := 52 ;
    Top := 70 ;
    Width := 97 ;
    Height := 17 ;
    Caption := '&Line visible' ;
    TabOrder := 7 ;
    OnClick := DoOnChange ;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelLineSeries.SetData(Value: TObject);
begin
  Inherited SetData( Value ) ;
  Assert( Data is TLineSeries, 'Incorrect data type: ' + Data.ClassName ) ;
  SetOnChange( false ) ;
  try
    with Data as TLineSeries do begin
      FlblTitle.Caption := 'Series title: ' + Title ;
      FcbSeriesVisible.Checked := Pointer.Visible or LinePen.Visible ;
      FcbPointsVisible.Checked := Pointer.Visible ;
      FcbLineVisible.Checked   := LinePen.Visible ;
    end ;
  finally
    SetOnChange( true ) ;
  end ;
  SetControlEnabled ;

end ;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelLineSeries.DoOnChange( Sender : TObject ) ;
begin
  with Data as TLineSeries do begin
    if FcbSeriesVisible.Checked then begin
      Pointer.Visible := FcbPointsVisible.Checked ;
      LinePen.Visible := FcbLineVisible.Checked   ;
    end else begin
      Pointer.Visible := false ;
      LinePen.Visible := false ;
    end ;
    TChart( Owner ).RePaint ;
  end ;
  SetControlEnabled ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelLineSeries.SetControlEnabled ;
begin
  FcbPointsVisible.Enabled := FcbSeriesVisible.Checked ;
  FcbLineVisible.Enabled   := FcbSeriesVisible.Checked ;
end ;

// -----------------------------------------------------------------------------
procedure TtiChartConfigForm.TVChange(Sender: TObject; Node: TTreeNode);
var
  lData : TObject ;
  liIndex : integer ;
  i : integer ;
begin

  // Clear the current child panel
  if FCurrentChildPanel <> nil then
    FCurrentChildPanel.Visible := false ;
  FCurrentChildPanel := nil ;

  if Node = nil then
    Exit ; //==>

  if Node.Data = nil then
    Exit ; //==>

  lData := TObject( Node.Data ) ;

  // Locate the correct child panel
  liIndex := -1 ;
  for i := 0 to FslPanels.Count - 1 do begin
    if FslPanels.Strings[i] = lData.ClassName then begin
      liIndex := i ;
      Break ; //==>
    end ;
  end ;

  // There was no child form found
  if liIndex = -1 then
    Exit ; //==>

  FCurrentChildPanel := TtiChartChildPanel( FslPanels.Objects[liIndex] ) ;
  FCurrentChildPanel.Parent := FpnlParent ;
  FCurrentChildPanel.Align  := alClient ;
  FCurrentChildPanel.Data   := lData ;
  FCurrentChildPanel.Visible := true ;

end;

// -----------------------------------------------------------------------------
function TtiChartChildPanel.GetData: TObject;
begin
  result := FData ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanel.SetData(Value: TObject);
begin
  FData := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelLineSeries.SetOnChange(pbValue: boolean);
var
  lEvent : TNotifyEvent ;
begin
  if pbValue then
    lEvent := DoOnChange
  else
    lEvent := nil ;

  FcbSeriesVisible.OnClick := lEvent ;
  FcbPointsVisible.OnClick := lEvent ;
  FcbLineVisible.OnClick   := lEvent ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiChartChildPanelChart
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiChartChildPanelChart.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  FlblBottomAxis := TLabel.Create( self ) ;
  with FlblBottomAxis do begin
    Parent  := self ;
    Left    :=  8 ;
    Top     :=  4 ;
    Width   := 51 ;
    Height  := 13 ;
    Caption := 'Bottom axis:' ;
  end ;

  FcbVGridLines := TCheckBox.Create( self ) ;
  with FcbVGridLines do begin
    Parent := self ;
    Left := 16 ;
    Top := 24 ;
    Width   := 120 ;
    Caption := '&Verticle grid lines?' ;
    OnClick := DoOnChange ;
  end ;

  FlblLeftAxis := TLabel.Create( self ) ;
  with FlblLeftAxis do begin
    Parent  := self ;
    Left    :=  8 ;
    Top     := 84 ;
    Width   := 51 ;
    Height  := 13 ;
    Caption := 'Left axis:' ;
  end ;

  FcbHGridLines := TCheckBox.Create( self ) ;
  with FcbHGridLines do begin
    Parent  := self ;
    Left    := 16 ;
    Top     := 104 ;
    Width   := 120 ;
    Caption := '&Horizontal grid lines?' ;
    OnClick := DoOnChange ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelChart.DoOnChange(Sender: TObject);
begin
  Assert( FData is TChart, 'Incorrect data type passed.' ) ;
  with FData as TChart do begin
    BottomAxis.Grid.Visible := FcbVGridLines.Checked ;
    LeftAxis.Grid.Visible   := FcbHGridLines.Checked ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelChart.SetData(Value: TObject);
begin
  Inherited SetData( Value ) ;
  Assert( Data is TChart, 'Incorrect data type: ' + Data.ClassName ) ;
  SetOnChange( false ) ;
  try
    with Data as TChart do begin
      FcbVGridLines.Checked := BottomAxis.Grid.Visible ;
      FcbHGridLines.Checked := LeftAxis.Grid.Visible ;
    end ;
  finally
    SetOnChange( true ) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChartChildPanelChart.SetOnChange(pbValue: boolean);
var
  lEvent : TNotifyEvent ;
begin
  if pbValue then
    lEvent := DoOnChange
  else
    lEvent := nil ;

  FcbHGridLines.OnClick := lEvent ;
  FcbVGridLines.OnClick := lEvent ;

end;

// -----------------------------------------------------------------------------
function TtiChart.GetMouseInCrossHairRegion: boolean;
var
  lPoint : TPoint ;
begin
  lPoint.X := Mouse.CursorPos.X ;
  lPoint.Y := Mouse.CursorPos.Y ;
  lPoint   := FChart.ScreenToClient( lPoint ) ;
  result   := IsFormFocused and
                PtInRect( Chart.ChartRect,
                          Point( lPoint.X-Chart.Width3D,
                          lPoint.Y+Chart.Height3D )) ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.Clear;
var
  i : integer ;
begin
  for i := Chart.SeriesCount - 1 downto 0 do
    Chart.SeriesList.Delete( i ) ;
  DoSBDefaultZoomClick(nil);
  FChart.Refresh ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetSnapToDataSize(const Value: integer);
begin
  FiSnapToDataSize := Value;
  FiSnapToDataRadius := FiSnapToDataSize div 2 ;
end;

// -----------------------------------------------------------------------------
function TtiChart.GetChartPopupMenu: TPopupMenu;
begin
  result := FChart.PopupMenu ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetChartPopupMenu(const Value: TPopupMenu);
begin
  FChart.PopupMenu := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoOnBeforeDrawAxes(sender: TObject);
const
  cdtSecond  = 1/24/60/60 ;
  cdtMinute  = 1/24/60    ;
  cdtHour    = 1/24       ;
  cdtDay     = 1.0        ;
  cdtMonth   = 354.75/12  ;
  cdtYear    = 364.75     ;
  cdt20Years = cdtYear*20 ;
var
  ldtPeriod : TDateTime ;
begin

  if not FbTimeSeriesChart then
    Exit ; //==>

  if FData = nil then
    Exit ; //==>

  ldtPeriod := FChart.BottomAxis.Maximum -
               FChart.BottomAxis.Minimum ;

{
    TDateTimeStep = ( dtOneSecond, dtFiveSeconds, dtTenSeconds, dtFifteenSeconds,
                      dtThirtySeconds, dtOneMinute, dtFiveMinutes, dtTenMinutes,
                      dtFifteenMinutes, dtThirtyMinutes, dtOneHour, dtTwoHours,
                      dtSixHours, dtTwelveHours, dtOneDay, dtTwoDays, dtThreeDays,
                      dtOneWeek, dtHalfMonth, dtOneMonth, dtTwoMonths, dtSixMonths,
                      dtOneYear);
}

  // > 20 years
  if ldtPeriod > cdt20Years then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneYear] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'yyyy' ;
    //Log( '> 20 Years' ) ;
  // > 1 and       <= 20 years
  end else if ( ldtPeriod > cdtYear ) and ( ldtPeriod <= cdt20Years ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneYear] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'yyyy' ;
    //Log( '> 1 and       <= 20 years' ) ;
  // > 3 months  and <= 1 year
  end else if ( ldtPeriod > cdtMonth*3 ) and ( ldtPeriod <= cdtYear ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneMonth] ;
    FChart.BottomAxis.LabelsMultiline := true ;
    FChart.BottomAxis.DateTimeFormat  := 'mmm yy' ;
    //Log( '> 1 month  and <= 1 year' ) ;
  // > 2 months <= 3 months
  end else if ( ldtPeriod > cdtMonth*2 ) and ( ldtPeriod <= cdtMonth*3 ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneWeek] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'dd mmm' ;
    //Log( '> 2 months <= 3 months' ) ;
  // > 1 months <= 2 months
  end else if ( ldtPeriod > cdtMonth ) and ( ldtPeriod <= cdtMonth*2 ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtThreeDays] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'dd mmm' ;
    //Log( '> 1 month <= 3 months' ) ;
  // > 10 day    and <= 1 month
  end else if ( ldtPeriod > cdtDay*10 ) and ( ldtPeriod <= cdtMonth ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtTwoDays] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'dd mmm' ;
    //Log( '> 10 days    and <= 1 month' ) ;
  // > 3 days    and <= 10 days
  end else if ( ldtPeriod > cdtDay*3 ) and ( ldtPeriod <= cdtDay*10 ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneDay] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'dd mmm' ;
    //Log( '> 3 day    and <= 10 days' ) ;
  // > 1 day    and <= 3 days
  end else if ( ldtPeriod > cdtDay ) and ( ldtPeriod <= cdtDay*3 ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtSixHours] ;
    FChart.BottomAxis.LabelsMultiline := true ;
    FChart.BottomAxis.DateTimeFormat  := 'hh:mm dd-mmm' ;
    //Log( '> 1 day    and <= 3 days' ) ;
  // > 1 hour   and <= 1 day
  end else if ( ldtPeriod > cdtHour ) and ( ldtPeriod <= cdtDay ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtOneHour] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'hh:mm' ;
    //Log( '> 1 hour   and <= 1 day' ) ;
  // > 1 minute and <= 1 hour
  end else if ( ldtPeriod > cdtMinute ) and ( ldtPeriod <= cdtHour ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtTenMinutes] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'hh:mm' ;
    //Log( '> 1 minute and <= 1 hour' ) ;
  // > 1 second and <= 1 minute
  end else if ( ldtPeriod > cdtSecond ) and ( ldtPeriod <= cdtMinute ) then begin
    FChart.BottomAxis.Increment       := DateTimeStep[dtTenSeconds] ;
    FChart.BottomAxis.LabelsMultiline := false ;
    FChart.BottomAxis.DateTimeFormat  := 'mm:ss' ;
    //Log( '> 1 second and <= 1 minute' ) ;
  end else
    // Do Nothing ;
  ;
    //raise exception.create( 'Invalid axis range passed to TtiChart.DoOnBeforeDrawAxes' ) ;

//    BottomAxis.RoundFirstLabel := true ;
//    BottomAxis.ExactDateTime   := true ;
//    BottomAxis.LabelsOnAxis := true ;
//    BottomAxis.TickOnLabelsOnly := true ;
//    BottomAxis.MinorTickCount := 0 ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetTimeSeriesChart(const Value: boolean);
begin
  if ( not FbTimeSeriesChart ) and
     ( Value ) then begin
    FbTimeSeriesChart := true ;
    Exit ; //==>
  end ;

  FbTimeSeriesChart := Value;
  //  FChart.BottomAxis.RoundFirstLabel := true ;
  //  FChart.BottomAxis.ExactDateTime   := true ;
  // FChart.BottomAxis.LabelsOnAxis := true ;
  // FChart.BottomAxis.TickOnLabelsOnly := true ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.SetDrawCrossHairsNow(const Value: boolean);
begin

  if FbDrawCrossHairsNow = Value then
    Exit ; //==>

  FbDrawCrossHairsNow := value ;

  if FbDrawCrossHairsNow then begin
    OnMouseMove := DoChartMouseMove ;
    // These two lines will cause the XHairs to be drawn when Alt+Tab back onto
    // the app, but will cause XHair litter when mousing over the chart region
    // for the first time. Requires more work...
    //MouseToChartCoOrds( liX, liY ) ;
    //DoDrawCrossHairs(   liX, liY, true ) ;
  end else begin
    OnMouseMove := nil ;
    ClearCrossHairs ;
  end ;

end;

// -----------------------------------------------------------------------------
{
procedure TtiChart.MouseToChartCoOrds( var piX, piY : integer ) ;
var
  lPoint : TPoint ;
begin
  lPoint.X := Mouse.CursorPos.X ;
  lPoint.Y := Mouse.CursorPos.Y ;
  lPoint   := FChart.ScreenToClient( lPoint ) ;
  piX      := lPoint.X ;
  piY      := lPoint.Y ;
end ;
}
// -----------------------------------------------------------------------------
procedure TtiChart.DoOnZoom(Sender: TObject);
var
  lMpt: TPoint ;
begin
  DrawCrossHairsNow := false ;
  FbZoomed := true ;
  AdjustScrollBarPositions ;
  DrawCrossHairsNow := true ;

  lMpt := point( FChart.Width, FChart.Height ) ;
  lMPt.X := lMPt.X - {FChart.MarginLeft -} FChart.MarginRight {-
                   - FChart.LeftAxis.LabelsSize} {- FChart.LeftAxis.TitleSize}
                   {- FChart.RightAxis.LabelsSize}
                   - FChart.RightAxis.TitleSize + 5 ;
  lMPt.Y := lMPt.Y - FChart.MarginTop - FChart.BottomAxis.LabelsSize
                   - FChart.BottomAxis.TitleSize ;

  lMpt := FChart.ClientToScreen( lMpt ) ;
  SetCursorPos( lMpt.x, lMpt.y ) ;

end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoOnScroll(Sender: TObject);
begin
  FbZoomed := true ;
  AdjustScrollBarPositions ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoMouseDown( Sender: TObject;
                                Button: TMouseButton;
                                Shift: TShiftState;
                                X, Y: Integer);
begin
  if ( Button = mbRight ) then
  begin
    FbDrawCrossHairsSaved := DrawCrossHairs ;
    DrawCrossHairs := false ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiChart.DoMouseUp( Sender: TObject;
                              Button: TMouseButton;
                              Shift: TShiftState;
                              X, Y: Integer);
begin
  if ( Button = mbRight ) then
  begin
    DrawCrossHairs := FbDrawCrossHairsSaved ;
  end ;
end;

{
procedure TtiChart.Loaded;
begin
  inherited;
  if FbDisplayTestData then
    CreateTestData ;
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//*TtiChartInternal
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiChartInternal.Paint;
var
  lbDrawCrossHairsSaved : boolean ;
begin
  lbDrawCrossHairsSaved := TtiChart( Owner ).DrawCrossHairs ;
  TtiChart( Owner ).DrawCrossHairs := false ;
  try
    inherited;
  finally
    TtiChart( Owner ).DrawCrossHairs := lbDrawCrossHairsSaved ;
  end ;
end;

procedure TtiChart.DoHorizontalScroll(Sender: TObject);
var
  lrMin : real ;
  lrMax : real ;
begin

  lrMin :=
    FrBottomAxisMin +
    (( FrBottomAxisMax - FrBottomAxisMin ) * FscbBottom.Position / 100 ) ;

  lrMax :=
    FChart.BottomAxis.Minimum +
    (( FrBottomAxisMax - FrBottomAxisMin ) * FscbBottom.PageSize / 100 ) ;

  FChart.BottomAxis.SetMinMax( lrMin, lrMax ) ;

end;

procedure TtiChart.DoVerticalScroll(Sender: TObject);
var
  lrMin : real ;
  lrMax : real ;
begin

  lrMin :=
    FrLeftAxisMin +
    (( FrLeftAxisMax - FrLeftAxisMin ) * FscbLeft.Position / 100 ) ;

  lrMax :=
    FChart.LeftAxis.Minimum +
    (( FrLeftAxisMax - FrLeftAxisMin ) * FscbLeft.PageSize / 100 ) ;

  FChart.LeftAxis.SetMinMax( lrMin, lrMax ) ;

end;

procedure TtiChart.AdjustScrollBarPositions ;
  procedure _SetupScrollBar( pScrollBar : TScrollBar ;
                             prMaxAxisRange : real ;
                             prCurrentAxisRange : real ;
                             prPosition : real ) ;
  var
    lOnChange : TNotifyEvent ;
  begin
    lOnChange := pScrollBar.OnChange ;
    pScrollBar.OnChange := nil ;
    if prMaxAxisRange <> 0 then
      pScrollBar.Position := Trunc( prPosition * 100 / prMaxAxisRange )
    else
      pScrollBar.Position := 0 ;

    if prMaxAxisRange <> 0 then
      pScrollBar.PageSize := Trunc( prCurrentAxisRange * 100 / prMaxAxisRange )
    else
      pScrollBar.PageSize := 100 ;
    pScrollBar.OnChange := lOnChange ;
  end ;

var
  lrMaxAxisRange : real ;
  lrCurrentAxisRange : real ;
  lrPosition : real ;
  liBorder : integer ;
begin

  // No scroll bars to show
  if FScrollStyle = ssNone then
    Exit ; //==>

  if not FbZoomed then
  begin
    FscbBottom.Visible := false ;
    FscbLeft.Visible   := false ;
    Exit ; //==>
  end else
    SetScrollStyle( FScrollStyle ) ;

  // Setup the bottom scrollbar
  if ( FScrollStyle = ssBoth ) or
     ( FScrollStyle = ssHorizontal ) then
  begin
    liBorder := FChart.Left + FChart.MarginLeft +
                FChart.LeftAxis.LabelsSize + FChart.LeftAxis.TitleSize ;
    FscbBottom.Left  := liBorder ;
    FscbBottom.Width := FChart.Width - liBorder - FChart.MarginRight ;

    // Set the position of the slider in the scroll bar
    lrMaxAxisRange     := FrBottomAxisMax           - FrBottomAxisMin ;
    lrCurrentAxisRange := FChart.BottomAxis.Maximum - FChart.BottomAxis.Minimum ;
    lrPosition         := FChart.BottomAxis.Minimum - FrBottomAxisMin ;
    _SetupScrollBar( FscbBottom, lrMaxAxisRange, lrCurrentAxisRange, lrPosition ) ;
  end ;

  // Setup the left scrollbar
  if ( FScrollStyle = ssBoth ) or
     ( FScrollStyle = ssVertical ) then
  begin
    liBorder := FChart.Top + FChart.MarginTop ;
    FscbLeft.Top    := liBorder ;
    FscbLeft.Height := FChart.Height - liBorder - FChart.MarginBottom ;
    // Set the position of the slider in the scroll bar
    lrMaxAxisRange     := FrLeftAxisMax           - FrLeftAxisMin ;
    lrCurrentAxisRange := FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum ;
    lrPosition         := FChart.LeftAxis.Minimum - FrLeftAxisMin ;
    _SetupScrollBar( FscbLeft, lrMaxAxisRange, lrCurrentAxisRange, lrPosition ) ;
  end ;

end ;

procedure TtiChart.SetScrollStyle(const Value: TScrollStyle);
begin

  FScrollStyle := Value;

  if not FbZoomed then
  begin
    FscbBottom.Visible          := false ;
    FscbLeft.Visible            := false ;
    FChart.Height               := self.ClientHeight ;
    FChart.Width                := self.Width - FChart.Left ;
    FChart.LeftAxis.LabelsSize  := 0 ;
    FChart.LeftAxis.TitleSize   := 0 ;
    FChart.RightAxis.LabelsSize := 0 ;
    FChart.RightAxis.TitleSize  := 0 ;
    Exit ; //==>
  end ;

  case FScrollStyle of
  ssNone       : begin
                   FscbBottom.Visible   := false ;
                   FscbLeft.Visible     := false ;
                   FChart.Height        := self.ClientHeight ;
                   FChart.Width         := self.Width - FChart.Left ;
                   FChart.LeftAxis.LabelsSize  := 0 ;
                   FChart.LeftAxis.TitleSize   := 0 ;
                   FChart.RightAxis.LabelsSize := 0 ;
                   FChart.RightAxis.TitleSize  := 0 ;
                 end ;
  ssHorizontal : begin
                   FscbBottom.Visible := true  ;
                   FscbLeft.Visible   := false ;
                   FChart.Height      := self.Height - ciBorder - ciSCBWidth ;
                   FChart.Width       := self.Width - FChart.Left ;
                   FChart.LeftAxis.LabelsSize  := 0 ;
                   FChart.LeftAxis.TitleSize   := 0 ;
                   FChart.RightAxis.LabelsSize := cuiAxisLabelSize  ;
                   FChart.RightAxis.TitleSize  := cuiAxisTitleSize  ;
                 end ;
  ssVertical   : begin
                   FscbBottom.Visible := false ;
                   FscbLeft.Visible   := true ;
                   FChart.Height      := self.ClientHeight ;
                   FChart.Width       := self.Width - FChart.Left - ciBorder - ciSCBWidth ;
                   FChart.LeftAxis.LabelsSize  := cuiAxisLabelSize ;
                   FChart.LeftAxis.TitleSize   := cuiAxisTitleSize ;
                   FChart.RightAxis.LabelsSize := 0 ;
                   FChart.RightAxis.TitleSize  := 0 ;
                 end ;
  ssBoth       : begin
                   FscbBottom.Visible := true ;
                   FscbLeft.Visible   := true ;
                   FChart.Height      := self.Height - ciBorder - ciSCBWidth ;
                   FChart.Width       := self.Width - FChart.Left - ciBorder - ciSCBWidth ;
                   FChart.LeftAxis.LabelsSize  := cuiAxisLabelSize ;
                   FChart.LeftAxis.TitleSize   := cuiAxisTitleSize ;
                   FChart.RightAxis.LabelsSize := cuiAxisLabelSize ;
                   FChart.RightAxis.TitleSize  := cuiAxisTitleSize ;
                 end ;
  end ;
end;

procedure TtiChart.Loaded;
begin
  inherited;
  if FbShowTestData then
    SetShowTestData( FbShowTestData ) ;
  SetScrollStyle( FScrollStyle ) ;
end;

procedure TtiChart.SetShowTestData(const Value: boolean);
begin
  FbShowTestData := Value;
  if FbShowTestData then
  begin
    if FTestData = nil then
      FTestData := TtiChartTestData.Create ;
    OnAssignGraphData := FTestData.AssignGraphData ;
    AddLineSeries( 'Sin' ) ;
    AddLineSeries( 'Cos' ) ;
    Data := FTestData ;
  end
  else
  begin
    OnAssignGraphData := nil ;
    FData := nil ;
    Clear ;
  end ;
end;

{ TtiChartTestData }

procedure TtiChartTestData.AssignGraphData(pData: TObject; pChart: TtiChart);
begin
  pChart.SeriesByName( 'Sin' ).AddXY(
    TtiChartTestDataItem( pData ).XValue,
    TtiChartTestDataItem( pData ).YValue1 ) ;
  pChart.SeriesByName( 'Cos' ).AddXY(
    TtiChartTestDataItem( pData ).XValue,
    TtiChartTestDataItem( pData ).YValue2 ) ;
end;

constructor TtiChartTestData.create;
var
  i : integer ;
  lData : TtiChartTestDataItem ;
begin
  inherited ;

  for i := 0 to 359 do begin
    lData := TtiChartTestDataItem.Create ;
    lData.XValue := Date + i ;
    lData.YValue1 := Sin( i/180*Pi ) ;
    lData.YValue2 := Cos( i/180*Pi ) ;
    Add( lData ) ;
  end ;

end;

function TtiChart.GetOnDblClickChart: TNotifyEvent;
begin
  result := FChart.OnDblClick ;
end;

procedure TtiChart.SetOnDblClickChart(const Value: TNotifyEvent);
begin
  FChart.OnDblClick := Value ;
end;

{ TtiChartDataMapping }

function TtiChartDataMapping.Clone: TtiChartDataMapping;
begin
  result := TtiChartDataMapping.Create( nil ) ;
  result.DisplayLabel := DisplayLabel ;
  result.PropertyName := PropertyName ;
end;

constructor TtiChartDataMapping.Create(Collection: TCollection);
begin
  inherited Create( Collection ) ;
  FDisplayLabel := 'Caption' ;
  FPropertyName := 'Caption' ;
end;

destructor TtiChartDataMapping.Destroy;
begin
  inherited;
end;

function TtiChartDataMapping.GetDisplayName: string;
begin
  result := DisplayLabel ;
end;

procedure TtiChartDataMapping.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
end;

{ TtiChartDataMappings }

function TtiChartDataMappings.Add: TtiChartDataMapping;
begin
  result := TtiChartDataMapping( inherited add ) ;
end;

procedure TtiChartDataMappings.Clear;
begin
  inherited ;
end;

constructor TtiChartDataMappings.Create(Owner: TComponent);
begin
  inherited Create( TtiChartDataMapping ) ;
  FOwner := Owner ;
end;

destructor TtiChartDataMappings.Destroy;
begin
  inherited;
end;

function TtiChartDataMappings.FindByFieldName( psFieldName: string): TtiChartDataMapping;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].PropertyName = psFieldName then begin
      result := Items[i] ;
      break ; //==>
    end ;
end;

function TtiChartDataMappings.GetItem(Index: integer): TtiChartDataMapping;
begin
  result := TtiChartDataMapping( inherited GetItem( Index )) ;
end;

function TtiChartDataMappings.GetOwner: TPersistent;
begin
  result := FOwner ;
end;

procedure TtiChartDataMappings.NamesToStringList(pSL: TStringList);
var
  i : integer ;
begin
  pSL.Clear ;
  for i := 0 to count - 1 do
    pSL.Add( Items[i].PropertyName ) ;
end;

procedure TtiChartDataMappings.SetItem(Index: integer; const Value: TtiChartDataMapping);
begin
  inherited SetItem( Index, Value ) ;
end;

end.





