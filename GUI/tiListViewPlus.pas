{$I tiDefines.inc}

unit tiListViewPlus;

interface
uses
{$IFNDEF FPC}
  Messages
  ,CommCtrl
{$ELSE}
  LMessages
  ,LCLType
  ,LCLProc
{$ENDIF}
  ,tiListView
  ,tiSpeedButton
  ,tiObject
  ,ComCtrls
  ,Classes
  ,Menus
  ,Forms
  ,Buttons
  ,StdCtrls  // TComboBox
  ,ExtCtrls  // TCustomPanel
  ,Graphics
  ,TypInfo
  ,Controls
  ,Contnrs
 ;


const
  // Strings to join filter conditions
  FilterConjs : array[0..2] of string = ('',
                                          'And',
                                          'Or');

  // Conditions which can be tested by a filter
  FilterOps   : array[0..10] of string = ('',
                                          '=',
                                          '<>',
                                          '>',
                                          '>=',
                                          '<',
                                          '<=',
                                          'Contains',
                                          'Not Contains',
                                          'Like',
                                          'Not Like'
                                         );

  // Possible sort directions
  cSortDirections : array[0..1] of string = ('Ascending',
                                              'Descending');

type

  // Simple TypeKinds, as summary of the TTypeKinds available in TypInfo
  TtiLVTypeKind =  (tiTKInteger, tiTKFloat , tiTKString);

  // FilterConjunction types
  TFilterConj   = (fcNone, fcAnd, fcOr);

  // Filter operation types
  TFilterOp     = (foNone, foEqual, foNotEqual,
                    foGreater, foGreaterEqual,
                    foLess, foLessEqual,
                    foContains, foNotContains,
                    foLike, foNotLike);

  // OnSortCompare method
  TtiLVSortCompare = procedure (pData1, pData2 : TObject; var pResult : integer) of object;

  TtiLVConfigFeature = (lvcfCols, lvcfSort, lvcfFilter, lvcfFind, lvcfExport);
  TtiLVConfigFeatures = set of TtiLVConfigFeature;

  // TDragMultiSelection - used for selecting columns to view
  TtiLVMultiSelect = class(TCustomGroupBox)
  private
    FlblAvailable : TLabel;
    FlblSelected : TLabel;
    FlbAvailable : TListBox;
    FlbSelected : TListBox;
    FsbSelectMarked : TtiSpeedButton;
    FsbDeSelectMarked : TtiSpeedButton;
    FsbSelectAll : TtiSpeedButton;
    FsbDeSelectAll : TtiSpeedButton;

    FslAvailable : TStringList;
    FslSelected : TStringList;
    FbBorder: boolean;
    FOnChange: TNotifyEvent;

    {$IFNDEF FPC}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ELSE}
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    {$ENDIF}
    procedure sbSelectMarkedClick(sender: TObject);
    procedure sbDeSelectMarkedClick(sender: TObject);
    procedure sbSelectAllClick(sender: TObject);
    procedure sbDeSelectAllClick(sender: TObject);
    procedure lbDragOver( Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbDragDrop( Sender, Source: TObject; X, Y: Integer);
    procedure lbMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbAvailableKeyPress(Sender: TObject; var Key: Char);
    procedure lbSelectedKeyPress(Sender: TObject; var Key: Char);

    function  GetAvailableAsString: string;
    function  GetSelectedAsString: string;
    procedure SetAvailableAsString(const Value: string);
    procedure SetSelectedAsString(const Value: string);
    procedure SetBorder(const Value: boolean);
    procedure SetAvailable(const Value: TStringList);
    procedure SetSelected(const Value: TStringList);

  protected
    {$IFNDEF FPC}
    procedure Paint; override;
    {$ENDIF}
    property  Caption;
    property  Anchors;
    property  Available : TStringList read FslAvailable write SetAvailable;
    property  Selected : TStringList read FslSelected  write SetSelected ;
    property  SelectedAsString : string read GetSelectedAsString write SetSelectedAsString;
    property  AvailableAsString : string read GetAvailableAsString write SetAvailableAsString;
    property  OnChange : TNotifyEvent read FOnChange write FOnChange;
    property  Border : boolean read FbBorder write SetBorder;
    procedure SelectByCaption(const psCaption : string; const APos : integer = -1);
    procedure DeSelectByCaption(const psCaption : string; const APos : integer = -1);
  published
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   RefreshData; virtual;
    procedure   ClearSelected;
  end;

  TtiLVSort = class(TObject)
  private
    FsField: string;
    FDirection: TlvSortDirection;
    FbThenBy: boolean;
  public
    constructor Create;
    property    Field : string read FsField write FsField;
    property    Direction : TlvSortDirection read FDirection write FDirection;
    property    ThenBy : boolean read FbThenBy write FbThenBy;
    function    Clone : TtiLVSort;
  end;

  TtiLVFilter = class(TObject)
  private
    FFilterConj : TFilterConj;
    FFilterOp  : TFilterOp;
    FsValue    : string;
    FsField    : string;
    function   WildcardMatch(const ASource, pPattern: String): Boolean;
    function   GetSimplePropType(pPersistent : TtiObject; APropName : string): TtiLVTypeKind;
  public
    constructor Create;
    property Conj    : TFilterConj read FFilterConj write FFilterConj;
    property Operator : TFilterOp   read FFilterOp   write FFilterOp;
    property Value   : string      read FsValue     write FsValue;
    property Field   : string      read FsField     write FsField;
    function DoesDataPassFilter(AValue : TtiObject): boolean;
    function ValidateFilter(AValue : TtiObject): string;
    function Clone : TtiLVFilter;
  end;

  // Forward dec of TtiListViewPlus
  TtiListViewPlus = class;

  // Store a TtiListViewPlus configuration data
  TtiLVConfig = class(TObject)
  private
    FSorts          : TObjectList;
    FFilters        : TList;
    FFinds          : TList;
    FbApplyFilter   : boolean;
    FbApplySort     : boolean;

    FColsAvailable  : TStringList;
    FColsSelected   : TStringList;
    FColsNumeric    : TStringList;
    FLV             : TtiListViewPlus;

    procedure SetColsAvailable(const Value: TStringList);
    procedure SetColsSelected(const Value: TStringList);
    procedure SetFilters(const Value: TList);
    procedure SetFinds(const Value: TList);
    procedure SetSorts(const Value: TList);
    function  GetSorts: TList;
  public
    constructor Create;
    constructor CreateExt(pLV : TtiListViewPlus);
    destructor  Destroy; override;
    property    ColsAvailable : TStringList read FColsAvailable write SetColsAvailable;
    property    ColsSelected : TStringList read FColsSelected  write SetColsSelected;
    property    ColsNumeric  : TStringList Read FColsNumeric   write FColsNumeric;
    property    Filters      : TList       read FFilters       write SetFilters;
    property    Finds        : TList       read FFinds         write SetFinds;
    property    Sorts        : TList       read GetSorts         write SetSorts;
    property    LV           : TtiListViewPlus read FLV            write FLV;
    property    ApplyFilter  : boolean     read FbApplyFilter  write FbApplyFilter;
    property    ApplySort    : boolean     read FbApplySort    write FbApplySort;

    procedure   ClearFilters;
    procedure   ClearSorts;
    procedure   ClearFinds;
    procedure   Clear;

    procedure   Assign(const AData : TtiLVConfig);
    function    IsCompatible(const AData : TList): boolean;
    procedure   AssignAvailableColumns(AStrings : TStrings);
    procedure   AssignSelectedColumns(AStrings : TStrings);
  end;

  // Abstract TabSheet to hold config controls
  TtiTSAbs = class(TTabSheet)
  private
    FLVConfig : TtiLVConfig;
  protected
    procedure   SetLVConfig(const Value: TtiLVConfig); virtual;
  public
    Property    LVConfig : TtiLVConfig read FLVConfig write SetLVConfig;
    Constructor Create(AOwner : TComponent); override;
    Procedure   Apply(pLV : TtiListViewPlus); virtual;
  end;

  //  TabSheet to configure column selection
  TtiTSCols = class(TtiTSAbs)
  private
    // A tiDragMultiSelection, owned by FTabSheetCols for editing col layout
    FMultiSelect         : TtiLVMultiSelect;
  protected
    procedure   SetLVConfig(const Value: TtiLVConfig); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Property    MultiSelect : TtiLVMultiSelect read FMultiSelect;
    Procedure   Apply(pLV : TtiListViewPlus); override;
  end;

  TtiLVFieldEdit = class(TCustomPanel)
  private
    FcbField          : TComboBox;
    FiIndex           : integer;
    FOnNewFieldEdit   : TNotifyEvent;
    FOnChangeFieldEdit : TNotifyEvent;
    function    GetFieldDisplay : string;
    function    GetFieldPropName: string;
    procedure   SetFieldPropName(const Value: string);
  protected
    procedure   OnChangeField(sender : TObject); virtual;
    function    GetValid: boolean; virtual;
    function    GetShowNextField : boolean; virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    Property    FieldDisplay : string read GetFieldDisplay {write SetFieldDisplay };
    Property    FieldPropName : string read GetFieldPropName write SetFieldPropName;
    procedure   AssignAvailableCols(const pListColumns : TtiListColumns);
    Property    Index : integer read FiIndex write FiIndex;
    Property    Valid : boolean read GetValid;
    Property    ShowNextField : boolean read GetShowNextField;
    Property    OnNewFieldEdit   : TNotifyEvent read FOnNewFieldEdit    write FOnNewFieldEdit;
    Property    OnChangeFieldEdit : TNotifyEvent read FOnChangeFieldEdit write FOnChangeFieldEdit;
  end;

  // Define the GUI element to edit a single filter condition
  // eg: <Field> <Operator> <Value> <Conjunction-to next filter>
  TtiLVFilterEdit = class(TtiLVFieldEdit)
  private
    FcbConj    : TComboBox;
    FcbOperator : TComboBox;
    FeValue    : TEdit;
    FbShowConj         : boolean;
    procedure   OnChangeConj(sender : TObject);
    function    GetConj: TFilterConj;
    function    GetOperator: TFilterOp;
    function    GetValue: string;
    procedure   SetConj(const Value: TFilterConj);
    procedure   SetOperator(const Value: TFilterOp);
    procedure   SetValue(const Value: string);
  protected
    function    GetValid: boolean; override;
    function    GetShowNextField : boolean; override;
    procedure   OnChangeField(sender : TObject); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    Property    Conj : TFilterConj read GetConj write SetConj;
    Property    Operator : TFilterOp read GetOperator write SetOperator;
    Property    Value : string read GetValue write SetValue;
    Property    ShowConj : boolean read FbShowConj write FbShowConj;
  end;

  TtiLVSortEdit = class(TtiLVFieldEdit)
  private
    FcbDirection : TComboBox;
    FcbThenBy   : TCheckBox;
    Procedure   SetDirection(Value : TlvSortDirection);
    Function    GetDirection : TlvSortDirection;
    function    GetThenBy: boolean;
    procedure   SetThenBy(const Value: boolean);
  protected
    Function    GetValid : boolean; override;
    Function    GetShowNextField : boolean; override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    Property    Direction : TlvSortDirection read GetDirection write SetDirection;
    Property    ThenBy   : boolean read GetThenBy write SetThenBy;
  end;

  // Panel to hold a list of edit objects. There will be 1 edit object
  // begin displayed at startup, and additional edits can be added
  // dynamically.
  TtiEditConditionAbs = class(TCustomPanel)
  private
    // A list ot TtiFilterEdit components
    FEditList     : TObjectList; // TList of TtiLVFieldEdit
    FLVConfig     : TtiLVConfig;
    FOnChangeValid : TNotifyEvent;
    FcbApply      : TCheckBox;
    FiMaxFilterCount: integer;

    // Property Get/Sets
    function    GetApply      : boolean;
    procedure   SetApply(      const Value: boolean);
    function    GetEnableApply : boolean;
    procedure   SetEnableApply(const Value: boolean);

    Procedure   OnNewFieldEdit(   sender : TObject);
    Procedure   OnChangeFieldEdit(sender : TObject);
    Procedure   HideNextField(pFieldEdit : TtiLVFieldEdit);
    procedure   DoEnableApply;

  protected
    function    GetValid : boolean;
    Procedure   DoAddFieldEdit(pFieldEdit : TtiLVFieldEdit);

    // Implement these in the concrete
    Procedure   AddFieldEdit(AData : TObject      ); virtual;

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    function    FirstControl: TWinControl;
    Property    LVConfig     : TtiLVConfig  read FLVConfig      write FLVConfig;
    Property    Valid        : boolean      read GetValid;
    Property    OnChangeValid : TNotifyEvent read FOnChangeValid write FOnChangeValid;
    Property    EditList     : TObjectList  read FEditList;
    Property    Apply      : boolean read GetApply       write SetApply;
    Property    EnableApply : boolean read GetEnableApply write SetEnableApply;
    Property    MaxFilterCount : integer read FiMaxFilterCount write FiMaxFilterCount;
  end;

  // Panel to configure a filter condition
  TtiEditFilter = class(TtiEditConditionAbs)
  private
  protected
    Procedure   AddFieldEdit(AData   : TObject   ); override;
  public
    Constructor Create(AOwner : TComponent); override;
    // Additional proc
    Procedure   AssignFilters(AList : TList);
  end;

  // Panel to configure a sort condition
  TtiEditSort = class(TtiEditConditionAbs)
  private
  protected
    Procedure   AddFieldEdit(AData   : TObject   ); override;
  public
    Constructor Create(AOwner : TComponent); override;
    // Additional proc
    Procedure   AssignSorts(AList : TList);
  end;

  // TabSheet to configure a filter condition
  TtiTSFilter = class(TtiTSAbs)
  private
    FEditFilter : TtiEditFilter;
  protected
    procedure   SetLVConfig(const Value: TtiLVConfig); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Procedure   Apply(pLV : TtiListViewPlus); override;
  end;

  // TabSheet to configure a sort conditions
  TtiTSSort = class(TtiTSAbs)
  private
    FEditSort : TtiEditSort;
  protected
    procedure   SetLVConfig(const Value: TtiLVConfig); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Procedure   Apply(pLV : TtiListViewPlus); override;
  end;

  // A form to edit a tiListView's configuration.
  TtiLVConfigEdit = class(TForm)
  private
    // The main pageControl
    FPageControl   : TPageControl;
    // A TabSheet for editing cols
    FTSCols        : TtiTSCols;
    // A TabSheet for editing the filter
    FTSFilter      : TtiTSFilter;
    // A TabSheet for editing sort definitions
    FTSSort        : TtiTSSort;

    // OK and Cancel buttons
    FBtnOK               : TBitBtn;
    FBtnCancel           : TBitBtn;
    FBtnHelp             : TBitBtn;

    // A pointer into the LVConfig object
    FLVConfig            : TtiLVConfig;
    // The name of the page that is currently active
    FsActivePage: string;

    procedure   SetLVConfig(const Value: TtiLVConfig);
    procedure   BtnOKClick(sender : TObject);
    procedure   BtnHelpClick(sender : TObject);
    procedure   SetActivePage(const Value: string);
    function    GetDefaultPage: string;
    function    GetConfigHelpContext: THelpContext;
    procedure   SetConfigHelpContext(pHelpContext: THelpContext);
  protected
  public
    Constructor CreateNew(AOwner : TComponent; Dummy: Integer = 0); override;
    Destructor  Destroy; override;
    property    LVConfig : TtiLVConfig read FLVConfig write SetLVConfig;
    property    ActivePage : string read FsActivePage write SetActivePage;
    property    DefaultPage : string read GetDefaultPage;
    property    ConfigHelpContext : THelpContext read GetConfigHelpContext write SetConfigHelpContext;
  end;

  TtiLVMenu = class(TPopupMenu)
  private

    FpmiExpToCSV    : TMenuItem;
    FpmiExpToClip   : TMenuItem;
    FpmiExpToHTML   : TMenuItem;

    FpmiCols        : TMenuItem;
    FpmiFilter      : TMenuItem;
    FpmiSort        : TMenuItem;
    FpmiDiv1        : TMenuItem;
    FpmiFind        : TMenuItem;
    FpmiFindNext    : TMenuItem;
    FpmiDiv2        : TMenuItem;
    FpmiExport      : TMenuItem;

    procedure ConfigOnClick(sender: TObject);
    procedure FindOnClick(sender : TObject);
    procedure FindNextOnClick(sender : TObject);
    procedure ExpToCSVOnClick(sender : TObject);
    procedure ExpToClipOnClick(sender : TObject);
    procedure ExpToHTMLOnClick(sender : TObject);

    function  GetListView : TtiListViewPlus;
    property  LV : TtiListViewPlus read GetListView;

    procedure DoOnPopup(sender : TObject);

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
  end;

  TtiExpEval = class(TObject)
  private
    FbBool: boolean;
    FConj: TFilterConj;
  public
    property Bool : boolean read FbBool write FbBool;
    property Conj : TFilterConj read FConj write FConj;
  end;

  TtiLVFindDialog = class(TForm)
  private
    FBtnCancel  : TBitBtn;
    FBtnFind    : TBitBtn;
    FBtnFindNext : TBitBtn;
    FEditFilter : TtiEditFilter;
    FLVConfig   : TtiLVConfig;

    procedure OnBtnFindClick(sender : TObject);
    procedure OnBtnFindNextClick(sender : TObject);
    procedure OnBtnCloseClick(sender : TObject);
    procedure DoOnChangeValid(sender : TObject);

    procedure SetLVConfig(value : TtiLVConfig);
    function  GetLV: TtiListViewPlus;
    procedure DoFind(const piStart : integer);
    function  GetValid : boolean;

  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor  Destroy; override;
    property    LVConfig : TtiLVConfig read FLVConfig write SetLVConfig;
    property    LV : TtiListViewPlus read GetLV;
    procedure   Find;
    procedure   FindNext;
    property    Valid : boolean read GetValid;

  end;

  TtiListViewPlus = class(TtiCustomListView)
  private
    FLVConfig : TtiLVConfig;
    FPopupMenu : TtiLVMenu;
    FLVFindDialog : TtiLVFindDialog;
    FConfigFeatures: TtiLVConfigFeatures;
    FListViewCaption: string;
    FConfigHelpContext: integer;

    procedure ListToHTMLStream(AStream : TStream);

    procedure ListToStream(const AStream : TStream;
                            const pHeadingSepStart : string;
                            const pHeadingSepEnd  : string;
                            const pHeadingLineSep : string;
                            const pLineSepStart   : string;
                            const pDataSepStart   : string;
                            const pDataSepEnd     : string;
                            const pLineSepEnd     : string;
                            const pEmptyChar      : string);


    procedure DoCustomApplySort(const pRefresh : boolean);

    function  ValidateFilters(var AMessage : string; pFilters : TList): boolean;
    function  DoesDataPassFilter(AValue: TtiObject; pFilters : TList ): boolean;
    procedure DoOnFilterData(AData : TtiObject; var pbInclude : boolean);

    procedure ViewFile(const AFileName: string);
    function  GetINIFileIdent : string;
    function  GetCSVFileName: string;
    function  GetHTMLFileName: string;
    procedure SetCSVFileName(const Value: string);
    procedure SetHTMLFileName(const Value: string);
    procedure WriteTextToStream(AStream: TStream; psText: string);
    function  INIReadString(const pSection, pIdent, pDefault : string): string;
    procedure INIWriteString(const pSection, pIdent, AValue : string);

  protected
    procedure SetData(const Value: TList); override;
    procedure SetApplyFilter(const Value: boolean); override;
    property  LVFindDialog : TtiLVFindDialog read FLVFindDialog;
    procedure EditConfig(const psDefaultPage : string = '');
    procedure SetupCols; override;
    property  CSVFileName : string read GetCSVFileName  write SetCSVFileName;
    property  HTMLFileName : string read GetHTMLFileName write SetHTMLFileName;

  published
    property    DataInternal;
    property    RuntimeGenCols;

    property    Align        ;
    property    Anchors      ;
    property    MultiSelect  ;
    property    OnDblClick   ;
    property    OnChange     ;
    {$IFNDEF FPC}
    property    OnChanging   ;
    {$ENDIF}
    property    OnKeyDown    ;
    property    OnKeyPress   ;
    property    SmallImages  ;
    property    ViewStyle    ;
    property    RowSelect    ;
    property    Constraints  ;
    property    Visible      ;

    // These three properties are needed for drag-and-drop
    property    OnDragOver ;
    property    OnDragDrop ;
    property    OnMouseDown;

    property    OnLVClick;

    property    OnFilterData;
    property    OnGetFont;

    property    ApplyFilter;
    property    ApplySort;

    property    OnGetImageIndex;

    property    ListColumns;
    property    SortOrders;
    property    SortOnHeadingClick;
    property    AfterRefreshData;
    property    OnItemArive;
    property    OnItemLeave;
    property    SelectFirstRow;
    property    OnRuntimeGenCol;
    property    ConfigFeatures : TtiLVConfigFeatures read FConfigFeatures write FConfigFeatures default [ lvcfCols, lvcfSort, lvcfFilter, lvcfFind, lvcfExport ];
    property    ConfigHelpContext : integer read FConfigHelpContext write FConfigHelpContext;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    procedure   DoFind;
    procedure   DoFindNext;
    procedure   DoExpToClipboard;
    procedure   DoExpToCSV;
    procedure   DoExpToHTML;
    procedure   DoSort   ;
    procedure   DoQuery  ;
    procedure   DoColumns;
    property    OnVScroll;

    property    LVConfig : TtiLVConfig read FLVConfig;
    procedure   ClearFindDialog;
    procedure   ClearColumns; override;
    property    ListViewCaption : string read FListViewCaption write FListViewCaption;

  end;

const
  // Config tab notebook page headings.
  cgsPageNameColumns      = '&Columns'   ;
  cgsPageNameFilter       = '&Query'     ;
  cgsPageNameSort         = 'S&ort'      ;
  cgsPageNameFind         = '&Find first';
  cgsPageNameFindNext     = 'Find &next' ;

  // Maximum number of filter edits on a page
  cuiMaxFilterCount = 6;

procedure tiShowWithListView(AList : TList; const pCols : array of string);

implementation
uses
{$IFNDEF FPC}
  Windows
  ,ShellAPI
{$ELSE}
  LCLIntf
{$ENDIF}
  ,SysUtils
  ,Dialogs
  ,Math
  ,ClipBrd
  ,INIFiles
  ,tiResources
  ,tiImageMgr
  ,tiUtils
 ;

const
  cusColDelim          = #13 + #10;
  cuRegKeyColsSelected = 'ColsSelected';
  cuiBorder            = 4;

{
procedure _GetPropertyNames(pPersistent : TObject;
                             pSL        : TStringList;
                             APropFilter : TTypeKinds = ctkSimple);
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
      psl.add(lList[i].Name);
  finally
    FreeMem(lList, lSize);
  end;
end;
}

procedure ShowError(const AMessage : string);
begin
  MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure tiShowWithListView(AList : TList; const pCols : array of string);
var
  lForm : TForm;
  lLV : TtiListViewPlus;
  lLVCol : TtiListColumn;
  lLVSortOrder : TlvSortOrder;
  i : integer;
begin

  lForm := TForm.Create(nil);
  try
    lForm.Width := Screen.Width * 2 div 3;
    lForm.Height := Screen.Height * 2 div 3;
    lForm.Position := poScreenCenter;
    lForm.BorderStyle := bsSizeable;
    lForm.BorderIcons := [biSystemMenu,biMaximize];
    lForm.Caption := ' View list data';
    lLV := TtiListViewPlus.Create(lForm);
    lLV.Parent := lForm;
    lLV.Align := alClient;

    if Length(pCols) > 0 then
    begin
      lLV.RuntimeGenCols := false;
      lLV.ApplySort := true;
      for i := Low(pCols) to High(pCols) do
      begin
        lLVCol := lLV.ListColumns.Add;
        lLVCol.FieldName := pCols[i];
        lLVSortOrder := lLV.SortOrders.Add;
        lLVSortOrder.FieldName := pCols[i];
      end;
    end;

    lLV.Data := AList;
    lForm.ShowModal;
  finally
    lForm.Free;
  end;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiListViewPlus
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiListViewPlus.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  LV.ReadOnly := true;
  LV.HideSelection := false;
  FLVConfig := TtiLVConfig.CreateExt(self);

  // Create popup menu
  FPopupMenu := TtiLVMenu.Create(self);
  PopupMenu := FPopupMenu;
  RowSelect := true;
  ConfigFeatures := [ lvcfCols, lvcfSort, lvcfFilter, lvcfFind, lvcfExport ];

end;

destructor TtiListViewPlus.Destroy;
begin
  if FLVFindDialog <> nil then
    FLVFindDialog.Free;
  inherited;
  FLVConfig.Free;
end;

procedure TtiListViewPlus.SetData(const Value: TList);
begin
  // The order is important, LVConfig.Data must be set first as the selected
  // cols will be read during the setup.
  RunTimeSelectedCols := false;
  if not LVConfig.IsCompatible(Value) then
    ClearColumns;
  inherited SetData(Value);
  //LVConfig.Data := Value;
end;

procedure TtiListViewPlus.SetupCols;
var
  i : integer;
  lListCol : TtiListColumn;
  lsColName : string;
  lColsToRemove : TStringList;
begin

  if RunTimeSelectedCols then
  begin
    Columns.Clear;
    ListColsBeingDisplayed.Clear;
    AddImageColumn;

    // This code will select all the cols marked for selection in
    // the Cols define screen
    lColsToRemove := TStringList.Create;

    try
      for i := 0 to LVConfig.ColsSelected.Count - 1 do begin
        lsColName := LVConfig.ColsSelected.Strings[i];
        lListCol := ListColumns.FindByDisplayLabel(lsColName);
        if lListCol <> nil then
          DoAddColumn(lListCol)
        else
          lColsToRemove.Add(lsColName);
      end;

      // Now remove all the cols which could not be found from the selected list
      for i := 0 to lColsToRemove.Count - 1 do
        LVConfig.ColsSelected.Delete(
          LVConfig.ColsSelected.IndexOf(
            lColsToRemove.Strings[i]));

    finally
      lColsToRemove.Free;
    end;

    GetColWidths;

  end else
    inherited SetupCols;

end;

procedure TtiListViewPlus.SetApplyFilter(const Value: boolean);
begin
  if Value then
    OnFilterData := DoOnFilterData
  else
    OnFilterData := nil;
  inherited SetApplyFilter(Value);
end;

// Before applying a filter, check
// a) all the properties in the filter exist in the data object
// b) the data in the filter can be converted to the data type of the
//    property in the object.
function TtiListViewPlus.ValidateFilters(var AMessage : string; pFilters : TList): boolean;
var
  i : integer;
begin
  AMessage := '';
  for i := 0 to LVConfig.Filters.Count - 1 do begin
    if AMessage <> '' then AMessage := AMessage + #13;
    AMessage := AMessage +
      (TObject(FLVConfig.Filters[i]) as TtiLVFilter).ValidateFilter(TtiObject(Data.Items[i]));
  end;
  result := (AMessage = '');
end;

// Apply the group of filters contained in LVConfig.Filters to AValue
function TtiListViewPlus.DoesDataPassFilter(AValue: TtiObject; pFilters : TList): boolean;
  // Scan through the filters and add an ExpEval object to the list
  // for each filter.
  procedure FiltersToExpEval(pFilters, pResult : TList; AValue : TtiObject);
  var
    i : integer;
    lExpEval : TtiExpEval;
  begin

    for i := 0 to pFilters.Count - 1 do begin
      lExpEval := TtiExpEval.Create;
      lExpEval.Bool := (TObject(pFilters[i]) as TtiLVFilter).DoesDataPassFilter(AValue);
      lExpEval.Conj := (TObject(pFilters[i]) as TtiLVFilter).Conj;
      pResult.Add(lExpEval);
    end;
  end;

  // Scan pInput and join all TtiExpVal(s) linked with an 'and' together
  // create then add new TtiExpEval(s) to pOutput
  procedure MergExp(pInput, pOutput : TList; pConj : TFilterConj);
  var
    i : integer;
    lb : boolean;
    lExpEval : TtiExpEval;
  begin
    // lb := true;
    lb := (pConj = fcAnd);
    for i := 0 to pInput.Count - 1 do begin
      //lb := lb and TtiExpEval(pInput.Items[i]).Bool;
      case pConj of
      fcOr : lb := lb  or TtiExpEval(pInput.Items[i]).Bool;
      fcAnd : lb := lb and TtiExpEval(pInput.Items[i]).Bool;
      else
        raise exception.Create('Invalid pConj passed to EvalExp');
      end;
      if TtiExpEval(pInput.Items[i]).Conj <> pConj then begin
        lExpEval := TtiExpEval.Create;
        lExpEval.Bool := lb;
        lExpEval.Conj :=  TtiLVFilter(pInput[i]).Conj;
        pOutput.Add(lExpEval);
        lb := (pConj = fcAnd);
      end;
    end;
  end;

  // Assuming all elements in pInput are linked with an 'OR'
  // 'Or' them all together.
  function EvalExp(pInput : TList; pConj : TFilterConj): boolean;
  var
    i : integer;
    lb : boolean;
  begin
    lb := (pConj = fcAnd);
    for i := 0 to pInput.Count - 1 do begin
      case pConj of
      fcOr : lb := lb  or TtiExpEval(pInput.Items[i]).Bool;
      fcAnd : lb := lb and TtiExpEval(pInput.Items[i]).Bool;
      else
        raise exception.Create('Invalid pConj passed to EvalExp');
      end;
    end;
    result := lb;
  end;

var
  i : integer;
  lListExps : TList;
  lListTemp : TList;
begin

  if AValue = nil then
  begin
    result := false;
    Exit; //==>
  end;

  lListExps := TList.Create;
  try
    // Evaluate the filters, and copy the true/false and and/or values
    // to a list of objects for further evaluation.
    // Scan through each of the conditions (like name = peter and)
    // and turn them into an ExpEval object which contains two props
    // bool, conj
    FiltersToExpEval(pFilters, lListExps, AValue);
    lListTemp := TList.Create;
    try
      // Evaluate all expressions that are connected with a <he 3rd parameter>
      MergExp(lListExps, lListTemp, fcOr);
      // Evaluate all the remaining expressions connecting them with
      // <the 2nd parameter>
      result := EvalExp(lListTemp, fcAnd);
    finally
      for i := 0 to lListTemp.Count - 1 do
        TObject(lListTemp.Items[i]).Free;
      lListTemp.Free;
    end;
  finally
    for i := 0 to lListExps.Count - 1 do
      TObject(lListExps.Items[i]).Free;
    lListExps.Free;
  end;

end;

procedure TtiListViewPlus.DoOnFilterData(AData: TtiObject; var pbInclude: boolean);
begin
  pbInclude := DoesDataPassFilter(AData, FLVConfig.Filters);
end;

procedure TtiListViewPlus.DoCustomApplySort(const pRefresh : boolean);
var
  i : integer;
  lSortOrder : TlvSortOrder;
  lApplySort : boolean;
begin
  SortOrders.Clear;
  for i := 0 to LVConfig.Sorts.Count - 1 do
  begin
    lSortOrder := SortOrders.Add;
    lSortOrder.FieldName    := TtiLVSort(LVConfig.Sorts.Items[i]).Field;
    lSortOrder.SortDirection := TtiLVSort(LVConfig.Sorts.Items[i]).Direction;
  end;
  lApplySort := (SortOrders.Count > 0) and FLVConfig.ApplySort;
  SetApplySortNoRefresh(lApplySort, false);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiLVConfig
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVConfig.Create;
begin
  inherited;
  FSorts          := TObjectList.Create;
  FFilters        := TList.Create;
  FFinds          := TList.Create;
  FColsAvailable := TStringList.Create;
  FColsSelected := TStringList.Create;
  FColsNumeric  := TStringList.Create;
end;

constructor TtiLVConfig.CreateExt(pLV: TtiListViewPlus);
begin
  Create;
  LV := pLV;
end;

destructor TtiLVConfig.Destroy;
begin
  Clear;

  FSorts.Free;
  FFilters.Free;
  FFinds.Free;

  FColsAvailable.Free;
  FColsSelected.Free;
  FColsNumeric.Free;

  inherited;
end;

procedure TtiLVConfig.ClearFilters;
var
  i : integer;
begin
  if FLV <> nil then
    FLV.ApplyFilter := false;
  ApplyFilter := false;
  for i := 0 to FFilters.Count - 1 do
    TObject(FFilters.Items[i]).Free;
  FFilters.Clear;
end;

procedure TtiLVConfig.ClearSorts;
begin
  if FLV <> nil then
  begin
    FLV.SetApplySortNoRefresh(false, false);
    FLV.SortOrders.Clear;
  end;
  FSorts.Clear;
end;

procedure TtiLVConfig.SetColsAvailable(const Value: TStringList);
begin
  FColsAvailable.Assign(Value);
  FColsSelected.Assign(FColsAvailable);
end;

procedure TtiLVConfig.SetColsSelected(const Value: TStringList);
begin
  FColsSelected.Assign(Value);
end;

procedure TtiLVConfig.SetFilters(const Value: TList);
var
  i : integer;
begin
  FFilters.Clear;
  for i := 0 to Value.Count - 1 do
    FFilters.Add(Value.Items[i]);
end;

procedure TtiLVConfig.SetSorts(const Value: TList);
var
  i : integer;
begin
  FSorts.Clear;
  for i := 0 to Value.Count - 1 do
    FSorts.Add(Value.Items[i]);
end;

procedure TtiLVConfig.SetFinds(const Value: TList);
var
  i : integer;
begin
  FFinds.Clear;
  for i := 0 to Value.Count - 1 do
    FFinds.Add(Value.Items[i]);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVMenu
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Images := gTIImageListMgr.ILNormal16;
  OnPopup := DoOnPopup;

  // Create select columns menu item
  FpmiCols  := TMenuItem.Create(self);
  FpmiCols.Caption := cgsPageNameColumns;
  FpmiCols.OnClick := ConfigOnClick;
  FpmiCols.Shortcut := TextToShortcut('Ctrl+C');
  FpmiCols.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_SelectCols);

  Items.Add(FpmiCols);

  // Create Filter menu item
  FpmiFilter  := TMenuItem.Create(self);
  FpmiFilter.Caption := cgsPageNameFilter;
  FpmiFilter.OnClick := ConfigOnClick;
  FpmiFilter.Shortcut := TextToShortcut('Ctrl+Q');
  FpmiFilter.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Query);
  Items.Add(FpmiFilter);

  // Create Sort menu item
  FpmiSort  := TMenuItem.Create(self);
  FpmiSort.Caption := cgsPageNameSort;
  FpmiSort.OnClick := ConfigOnClick;
  FpmiSort.Shortcut := TextToShortcut('Ctrl+O');
  FpmiSort.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Sort);
  Items.Add(FpmiSort);

  // Create divider menu item
  FpmiDiv1  := TMenuItem.Create(self);
  FpmiDiv1.Caption := '-';
  Items.Add(FpmiDiv1);

  // Create Find menu item
  FpmiFind          := TMenuItem.Create(self);
  FpmiFind.Caption  := cgsPageNameFind;
  FpmiFind.OnClick  := FindOnClick;
  FpmiFind.Shortcut := TextToShortcut('Ctrl+F');
  FpmiFind.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Find);
  Items.Add(FpmiFind);

  // Create Find menu item
  FpmiFindNext          := TMenuItem.Create(self);
  FpmiFindNext.Caption  := cgsPageNameFindNext;
  FpmiFindNext.OnClick  := FindNextOnClick;
  FpmiFindNext.Shortcut := TextToShortcut('Ctrl+N');
  Items.Add(FpmiFindNext);

  // Create divider menu item
  FpmiDiv2  := TMenuItem.Create(self);
  FpmiDiv2.Caption := '-';
  Items.Add(FpmiDiv2);

  // Create Export menu item
  FpmiExport  := TMenuItem.Create(self);
  FpmiExport.Caption := 'E&xport';
  FpmiExport.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Export);
  Items.Add(FpmiExport);

  FpmiExpToCSV := TMenuItem.Create(self);
  FpmiExpToCSV.Caption := 'Export to CS&V file';
  FpmiExpToCSV.OnClick := ExpToCSVOnClick;
  FpmiExpToCSV.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_ExportToCSV);
  FpmiExport.Add(FpmiExpToCSV);

  FpmiExpToClip := TMenuItem.Create(self);
  FpmiExpToClip.Caption := 'Export to c&lip board';
  FpmiExpToClip.OnClick := ExpToClipOnClick;
  FpmiExpToClip.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CopyToClipBoard);
  FpmiExport.Add(FpmiExpToClip);

  FpmiExpToHTML        := TMenuItem.Create(self);
  FpmiExpToHTML.Caption := 'Export to &HTML';
  FpmiExpToHTML.OnClick := ExpToHTMLOnClick;
  FpmiExpToHTML.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_ExportToHTML);
  FpmiExport.Add(FpmiExpToHTML);

end;

destructor TtiLVMenu.Destroy;
begin
  inherited;

end;

function TtiLVMenu.GetListView : TtiListViewPlus;
begin
  result := (Owner as TtiListViewPlus);
end;

procedure TtiLVMenu.ConfigOnClick(sender: TObject);
begin
  LV.EditConfig((Sender as TMenuItem).Caption);
end;

procedure TtiLVMenu.ExpToClipOnClick(sender: TObject);
begin
  LV.DoExpToClipboard;
end;

procedure TtiLVMenu.ExpToHTMLOnClick(sender: TObject);
begin
  LV.DoExpToHTML;
end;

procedure TtiLVMenu.ExpToCSVOnClick(sender: TObject);
begin
  LV.DoExpToCSV;
end;

procedure TtiLVMenu.FindOnClick(sender: TObject);
begin
  LV.DoFind;
end;

procedure TtiLVMenu.FindNextOnClick(sender: TObject);
begin
  LV.DoFindNext;
end;

procedure TtiListViewPlus.EditConfig(const psDefaultPage: string);
var
  lForm : TtiLVConfigEdit;
  lsDefaultPage : string;
begin
  lForm := TtiLVConfigEdit.CreateNew(self);
  try
    lForm.LVConfig := LVConfig;

    if psDefaultPage <> '' then
      lsDefaultPage := psDefaultPage
    else
      lsDefaultPage := lForm.DefaultPage;

    lForm.ActivePage := lsDefaultPage;
    lForm.ConfigHelpContext := FConfigHelpContext;

    // Show the form
    lForm.ShowModal;

  finally
    lForm.Free;
  end;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVConfigEdit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiLVConfigEdit.BtnHelpClick(sender: TObject);
begin
  {$IFNDEF FPC}
  Application.helpcommand(HELP_CONTEXT, HelpContext);
  {$ELSE}
     {$WARNING TODO: Currently missing TApplication.HelpCommand - Fix it later}
  {$ENDIF}
end;

procedure TtiLVConfigEdit.BtnOKClick(sender: TObject);
begin
  Hide;
  // Assign the selected cols
  LVConfig.ColsSelected.Assign(FTSCols.MultiSelect.Selected);
  FtsFilter.Apply(FLVConfig.LV);
  FtsSort.Apply(  FLVConfig.LV);
  FtsCols.Apply(  FLVConfig.LV);
  LVConfig.LV.Refresh;
  ModalResult := mrOK;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTSCols
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiTSAbs.Apply(pLV: TtiListViewPlus);
begin
  // Do nothing, implement in the concrete.
end;

constructor TtiTSAbs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent     := AOwner as TPageControl;
  PageControl := AOwner as TPageControl;
end;

procedure TtiTSAbs.SetLVConfig(const Value: TtiLVConfig);
begin
  FLVConfig := Value;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTSCols
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiTSCols.Apply(pLV: TtiListViewPlus);
begin
  FLVConfig.LV.RunTimeSelectedCols := true;
end;

constructor TtiTSCols.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption    := cgsPageNameColumns;
  FMultiSelect := TtiLVMultiSelect.Create(self);
  FMultiSelect.Parent := self;
  FMultiSelect.Top   := 4;
  FMultiSelect.Left  := 4;
  FMultiSelect.Width := self.ClientWidth - 8;
  FMultiSelect.Height := self.ClientHeight - 8;
  FMultiSelect.Caption := '';
  FMultiSelect.Border := false;
end;

procedure TtiTSCols.SetLVConfig(const Value: TtiLVConfig);
begin
  inherited SetLVConfig(Value);
  Value.AssignAvailableColumns(FMultiSelect.Available);
  Value.AssignSelectedColumns(FMultiSelect.Selected);
  FMultiSelect.RefreshData;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiEditConditionAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiEditConditionAbs.AddFieldEdit(AData: TObject);
begin
  // Do nothing, implement in the concrete
end;

constructor TtiEditConditionAbs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditList              := TObjectList.Create;
  Caption                := '';
  BevelOuter             := bvNone;

  FcbApply         := TCheckBox.Create(self);
  FcbApply.TabOrder := 9999;
  FcbApply.Parent  := self;
  FcbApply.Caption := '&Apply this condition?';
  FcbApply.Enabled := false;
  FcbApply.Width   := 150;

  MaxFilterCount   := cuiMaxFilterCount;

end;

destructor TtiEditConditionAbs.Destroy;
begin
  FEditList.Free;
  inherited;
end;

procedure TtiEditFilter.AddFieldEdit(AData : TObject);
var
  lFilterEdit : TtiLVFilterEdit;
begin

  // Create the new tiLVFilterEdit, and assign it's parent
  lFilterEdit    := TtiLVFilterEdit.Create(self);

  // Assign the Fields value
  // lFilterEdit.Fields := LVConfig.ColsAvailable;
  lFilterEdit.AssignAvailableCols(LVConfig.LV.ListColumns);

  // Is the conjunction field to be shown
  lFilterEdit.ShowConj := EditList.Count < MaxFilterCount-1;

  // if a tiLVFilter is set, then assign the filter values
  if AData <> nil then
    with (AData as TtiLVFilter) do
    begin
      lFilterEdit.Conj         := Conj   ;
      lFilterEdit.Operator     := Operator;
      lFilterEdit.FieldPropName := Field  ;
      lFilterEdit.Value        := Value  ;
    end;

  DoAddFieldEdit(lFilterEdit);

end;

procedure TtiEditFilter.AssignFilters(AList: TList);
var
  i          : integer;
  lFilterEdit : TtiLVFilterEdit;
  lFilter    : TtiLVFilter;
begin

  // Assign the filter condition
  { ToDo 5 -ctiListView: This code is cloned in AssignFilters and AssignFinds. Fix}
  AList.Clear;
  for i := 0 to EditList.Count - 1 do
  begin
    lFilterEdit := (TObject(EditList.Items[i]) as TtiLVFilterEdit);
    if Valid and Visible then
    begin
      lFilter := TtiLVFilter.Create;
      lFilter.Conj    := lFilterEdit.Conj;
      lFilter.Operator := lFilterEdit.Operator;
      lFilter.Field   := lFilterEdit.FieldPropName;
      lFilter.Value   := lFilterEdit.Value;
      AList.Add(lFilter);
    end;
  end;

end;

function TtiEditConditionAbs.GetEnableApply : boolean;
begin
  result := FcbApply.Visible;
end;

procedure TtiEditConditionAbs.SetEnableApply(const Value: boolean);
begin
  FcbApply.Visible := Value;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTSSort
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiTSSort.Apply(pLV: TtiListViewPlus);
begin
  FEditSort.AssignSorts(LVConfig.Sorts);
  LVConfig.ApplySort := FEditSort.Apply;
  FLVConfig.LV.DoCustomApplySort(false);
end;

constructor TtiTSSort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption           := cgsPageNameSort;
  FEditSort         := TtiEditSort.Create(self);
  FEditSort.Parent  := self;
  FEditSort.Align   := alClient;
  FEditSort.LVConfig := LVConfig;
end;

procedure TtiTSSort.SetLVConfig(const Value: TtiLVConfig);
var
  i : integer;
begin
  inherited SetLVConfig(Value);
  FEditSort.LVConfig := LVConfig;

  for i := 0 to LVConfig.Sorts.Count - 1 do
    FEditSort.AddFieldEdit(TObject(FLVConfig.Sorts[i]));

  if FEditSort.EditList.Count = 0 then
    FEditSort.AddFieldEdit(nil);

  FEditSort.Apply := LVConfig.ApplySort;
  FEditSort.EnableApply := true;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVConfigEdit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVConfigEdit.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner,0);

  // Setup the form
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
  Caption    := ' Configure list view';
  Name       := ClassName;
  Position   := poScreenCenter;

  FPageControl := TPageControl.Create(self);
  FPageControl.Parent := self;
  FPageControl.Top   := 4;
  FPageControl.Left  := 4;
  FPageControl.Width := 375;
  FPageControl.Height := 290;
  FPageControl.Images := gTIImageListMgr.ILNormal16;

  FTSCols  := TtiTSCols.Create(FPageControl);
  FTSCols.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_SelectCols);

  FTSFilter := TtiTSFilter.Create(FPageControl);
  FTSFilter.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Query);

  FTSSort  := TtiTSSort.Create(FPageControl);
  FTSSort.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Sort);

  FBtnCancel   := TBitBtn.Create(self);
  FBtnCancel.Parent := self;
  FBtnCancel.Top := FPageControl.Top + FPageControl.Height + 4;
  FBtnCancel.Left := FPageControl.Left + FPageControl.Width - FBtnCancel.Width;
  FBtnCancel.Cancel := true;
  FBtnCancel.NumGlyphs  := 2;
  FBtnCancel.Caption := '&Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Glyph.LoadFromResourceName(HInstance, cResTI_Cross16ND);


  FBtnOK            := TBitBtn.Create(self);
  FBtnOK.Parent     := self;
  FBtnOK.Top        := FBtnCancel.Top;
  FBtnOK.Left       := FBtnCancel.Left - 4 - FBtnOK.Width;
  FBtnOK.Kind       := bkOK;
  FBtnOK.OnClick    := BtnOKClick;
  FBtnOK.Caption    := '&Apply';
  FBtnOK.Default    := true;
  FBtnOK.NumGlyphs  := 2;
  FBtnOK.Caption    := '&Apply';
  FBtnOK.Glyph.LoadFromResourceName(HInstance, cResTI_Tick16ND);

  FBtnHelp            := TBitBtn.Create(self);
  FBtnHelp.Parent     := self;
  FBtnHelp.Top        := FBtnCancel.Top;
  FBtnHelp.Left       := FBtnOK.Left - 4 - FBtnHelp.Width;
  FBtnHelp.OnClick    := BtnHelpClick;
  FBtnHelp.Caption    := '&Help';
  FBtnHelp.Default    := false;
  FBtnHelp.NumGlyphs  := 1;
  FBtnHelp.Glyph.LoadFromResourceName(HInstance, cResTI_Help + cResTI_16N);
  FBtnHelp.Visible := false;

  ClientWidth := FPageControl.Width + 8;
  ClientHeight := FBtnOK.Top + FBtnOK.Height + 4;

end;

destructor TtiLVConfigEdit.Destroy;
begin
  FBtnOK.Free;
  FBtnCancel.Free;
  inherited;
end;

function TtiLVConfigEdit.GetConfigHelpContext: THelpContext;
begin
  result := HelpContext;
end;

function TtiLVConfigEdit.GetDefaultPage: string;
begin
  result := FPageControl.Pages[0].Caption;
end;

procedure TtiLVConfigEdit.SetActivePage(const Value: string);
var
  i : integer;
begin
  FsActivePage := Value;
  for i := 0 to FPageControl.PageCount - 1 do
    if FPageControl.Pages[i].Caption = Value then begin
      FPageControl.ActivePage := FPageControl.Pages[i];
      Break; //==>
    end;
  if FPageControl.ActivePage <> nil then
    if (FPageControl.ActivePage.Controls[0] is TtiEditConditionAbs) then
      ActiveControl := (FPageControl.ActivePage.Controls[0] as TtiEditConditionAbs).FirstControl
    else if (FPageControl.ActivePage.Controls[0] is TtiLVMultiSelect) then
      ActiveControl :=  (FPageControl.ActivePage.Controls[0] as TtiLVMultiSelect).FlbAvailable;
end;

procedure TtiLVConfigEdit.SetConfigHelpContext(pHelpContext: THelpContext);
begin
  HelpContext := pHelpContext;
  FBtnHelp.Visible := pHelpContext <> 0;
end;

procedure TtiLVConfigEdit.SetLVConfig(const Value: TtiLVConfig);
var
  i : integer;
begin
  FLVConfig := Value;
  for i := 0 to FPageControl.PageCount - 1 do
    TtiTSAbs(FPageControl.Pages[i]).LVConfig := LVConfig;
  FTSCols.TabVisible  := lvcfCols in FLVConfig.LV.ConfigFeatures;
  FTSFilter.TabVisible := lvcfFilter in FLVConfig.LV.ConfigFeatures;
  FTSSort.TabVisible  := lvcfSort in FLVConfig.LV.ConfigFeatures;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVFilter
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVFilter.Create;
begin
  inherited;
  Conj    := fcNone;
  Operator := foNone;
  Value   := '';
  Field   := '';
end;

// Does this AValue (a TtiObject descendant) pass the filter ?
function TtiLVFilter.DoesDataPassFilter(AValue: TtiObject): boolean;
  // The property being filtered is a string
  function TestStringFilter(AValue : TtiObject): boolean;
  var
    lsProp : string;
    lsVal : string;
  begin
    try
      lsProp := upperCase(GetPropValue(AValue, Field));
    except
      on e:exception do
        raise exception.Create('Error reading property <' +
                                Field + '> ' + e.message +
                                ' Called in TestStringFilter');
    end;
    lsVal := upperCase(Value);
    case Operator of
    foEqual       : result := (lsProp =  lsVal);
    foNotEqual    : result := (lsProp <> lsVal);
    foGreater     : result := (lsProp >  lsVal);
    foGreaterEqual : result := (lsProp >= lsVal);
    foLess        : result := (lsProp <  lsVal);
    foLessEqual   : result := (lsProp <= lsVal);
    foContains    : result := WildcardMatch(lsProp, '*' + lsVal + '*');
    foNotContains : result := (not WildcardMatch(lsProp, '*' + lsVal + '*'));
    foLike        : result := WildcardMatch(lsProp, lsVal);
    foNotLike     : result := (not WildcardMatch(lsProp, lsVal));
    else
      raise exception.Create('Invalid operator passed to TestStringFilter');
    end;
  end;

  // The property being filtered is an integer
  function TestIntFilter(AValue : TtiObject): boolean;
  var
    lsProp : string;
    liProp : integer;
    liVal : integer;
  begin
    try
      lsProp := upperCase(GetPropValue(AValue, Field));
    except
      on e:exception do
        raise exception.Create('Error reading property <' +
                                Field + '> ' + e.message +
                                ' Called in TestIntFilter');
    end;
    try liProp := StrToInt(lsProp) except liProp := 0 end;
    try liVal := StrToInt(Value) except liVal := 0 end;
    case Operator of
    foEqual       : result := (liProp =  liVal);
    foNotEqual    : result := (liProp <> liVal);
    foGreater     : result := (liProp >  liVal);
    foGreaterEqual : result := (liProp >= liVal);
    foLess        : result := (liProp <  liVal);
    foLessEqual   : result := (liProp <= liVal);
    foContains    : result := WildcardMatch(lsProp, '*' + Value + '*');
    foNotContains : result := (not WildcardMatch(lsProp, '*' + Value + '*'));
    foLike        : result := WildcardMatch(lsProp, Value);
    foNotLike     : result := (not WildcardMatch(lsProp, Value));
    else
      raise exception.Create('Invalid operator passed to TestIntFilter');
    end;
  end;

  // The property being filtered is a float
  function TestFloatFilter(AValue : TtiObject): boolean;
  var
    lsProp : string;
    lrProp : Extended;
    lrVal : Extended;
    lRetCode : Integer;
    lValidNum : Boolean;
  begin
    try
      lsProp := upperCase(GetPropValue(AValue, Field));
    except
      on e:exception do
        raise exception.Create('Error reading property <' +
                                Field + '> ' + e.message +
                                ' Called in TestFloatFilter');
    end;

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
      case Operator of
      foEqual       : result := (lrProp =  lrVal);
      foNotEqual    : result := (lrProp <> lrVal);
      foGreater     : result := (lrProp >  lrVal);
      foGreaterEqual : result := (lrProp >= lrVal);
      foLess        : result := (lrProp <  lrVal);
      foLessEqual   : result := (lrProp <= lrVal);
      foContains    : result := WildcardMatch(lsProp, '*' + Value + '*');
      foNotContains : result := (not WildcardMatch(lsProp, '*' + Value + '*'));
      foLike        : result := WildcardMatch(lsProp, Value);
      foNotLike     : result := (not WildcardMatch(lsProp, Value));
      else
        raise exception.Create('Invalid operator passed to TestFloatFilter');
      end;
    End
    Else
      Result := False;
  end;

var
  lPropType : TtiLVTypeKind;
begin

  lPropType := GetSimplePropType(AValue, Field);
  case lPropType of
  tiTKInteger : result := TestIntFilter(   AValue);
  tiTKFloat  : result := TestFloatFilter( AValue);
  tiTKString : result := TestStringFilter(AValue);
  else
    raise exception.Create('Invalid property type passed to ' +
                            'TtiLVFilter.DoesDataPassFilter');
  end;
end;

function TtiLVFilter.WildcardMatch(const ASource,pPattern: String): Boolean;
var
  lSource: Array [0..255] of Char;
  lPattern: Array [0..255] of Char;

  function MatchPattern(const element, pattern: PChar): Boolean;
    function IsPatternWild(const pattern: PChar): Boolean;
    begin
      Result := StrScan(pattern,'*') <> nil;
     if not Result then Result := StrScan(pattern,'?') <> nil;
    end;

  begin
    if 0 = StrComp(pattern,'*') then
      Result := True
    else if (element^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result := False
    else if element^ = Chr(0) then
      Result := True
    else begin
      case pattern^ of
      '*': if MatchPattern(element,@pattern[1]) then
             Result := True
           else
             Result := MatchPattern(@element[1],pattern);
      '?': Result := MatchPattern(@element[1],@pattern[1]);
      else
        if element^ = pattern^ then
          Result := MatchPattern(@element[1],@pattern[1])
        else
          Result := False;
      end;
    end;
  end;

begin
  StrPCopy(lSource,  ASource);
  StrPCopy(lPattern, pPattern);
  Result := MatchPattern(lSource, lPattern);
end;

function TtiLVFilter.ValidateFilter(AValue : TtiObject): string;
begin
  result := '';
  // a) Check the <Field> exists in the object being tested
  if not IsPublishedProp(AValue, Field) then
    result := result + 'Data field <' + Field + '> does not exist.';

  // b) Check the <Value> can be converted to the correct data type.
  //    Currently, we are simply ignoring the filter is it can not be
  //    be converted to the correct data type. Should this be changed
  //    to clearing the filter?

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLFFilterEdit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVFilterEdit.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);

  FcbOperator := TComboBox.Create(self);
  FcbOperator.Parent := self;
  FcbOperator.Top := cuiBorder;
  FcbOperator.Left := FcbField.Left + FcbField.Width + cuiBorder;
  FcbOperator.Width := 87;
  FcbOperator.Style := csDropDownList;
  FcbOperator.DropdownCount := 9;
  FcbOperator.OnChange := OnChangeField;


  for i := low(FilterOps) to High(FilterOps) do
    FcbOperator.Items.Add(FilterOps[i]);
  FcbOperator.ItemIndex := ord(fcNone);

  FeValue    := TEdit.Create(self);
  FeValue.Parent := self;
  FeValue.Top := cuiBorder;
  FeValue.Left := FcbOperator.Left + FcbOperator.Width + cuiBorder;
  FeValue.Width := 101;

  FcbConj     := TComboBox.Create(self);
  FcbConj.Parent := self;
  FcbConj.Top := cuiBorder;
  FcbConj.Left := FeValue.Left + FeValue.Width + cuiBorder;
  FcbConj.Width := 49;
  FcbConj.Style := csDropDownList;
  FcbConj.Visible := false;
//  FcbConj.OnChange := OnChangeConj;
  FcbConj.OnChange := OnChangeField;

  for i := low(FilterConjs) to High(FilterConjs) do
    FcbConj.Items.Add(FilterConjs[i]);
  FcbConj.ItemIndex := ord(fcNone);

end;

destructor TtiLVFilterEdit.Destroy;
begin
  FcbConj.Free;
  FcbOperator.Free;
  FeValue.Free;
  inherited;
end;

function TtiLVFilterEdit.GetConj: TFilterConj;
begin
  result := TFilterConj(FcbConj.ItemIndex);
end;

function TtiLVFilterEdit.GetOperator: TFilterOp;
begin
  result := TFilterOp(FcbOperator.ItemIndex);
end;

function TtiLVFilterEdit.GetValue: string;
begin
  result := FeValue.Text;
end;

procedure TtiLVFilterEdit.SetConj(const Value: TFilterConj);
begin
  FcbConj.ItemIndex := Ord(Value);
end;

procedure TtiLVFilterEdit.SetOperator(const Value: TFilterOp);
begin
  FcbOperator.ItemIndex := Ord(Value);
  OnChangeField(nil);
end;

procedure TtiLVFilterEdit.SetValue(const Value: string);
begin
  FeValue.Text := Value;
end;

function TtiLVFilterEdit.GetValid: boolean;
begin
  result := (not Visible) or
            ((Operator <> foNone) and
             (FieldDisplay <> ''));
end;

function TtiLVFilterEdit.GetShowNextField: boolean;
begin
  result := Valid and
            (Conj <> fcNone);
end;

procedure TtiLVFilterEdit.OnChangeConj(sender: TObject);
begin
//  if Assigned(FOnNewFieldEdit) then
//    FOnNewFieldEdit(self);
end;

procedure TtiListViewPlus.DoFind;
begin
  if not (lvcfFind in FConfigFeatures) then
    Exit; //==>
  if FLVFindDialog = nil then
    FLVFindDialog := TtiLVFindDialog.CreateNew(nil);
  FLVFindDialog.LVConfig := LVConfig;
  FLVFindDialog.Find;
end;

procedure TtiListViewPlus.DoFindNext;
begin
  if not (lvcfFind in FConfigFeatures) then
    Exit; //==>

  Assert(FLVFindDialog <> nil, 'FLVFindDialog not assigned.');
  FLVFindDialog.FindNext ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVFindDialog
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVFindDialog.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  Caption    := ' Find data';
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu,biMinimize];
  FBtnCancel := TBitBtn.Create(self);
  Height     := 290;
  Width      := 370;
  Position   := poScreenCenter;

  with FBtnCancel do begin
    Parent := self;
    Left     := self.clientWidth - Width - cuiBorder;
    Top     := self.ClientHeight - Height - cuiBorder;
    Kind    := bkCancel;
    OnClick := OnBtnCloseClick;
    Caption := '&Cancel';
    TabOrder := 100;
    Glyph   := nil;
    Cancel  := true;
  end;

  FBtnFindNext := TBitBtn.Create(self);
  with FBtnFindNext do begin
    Parent     := Self;
    Left       := FBtnCancel.Left - Width - cuiBorder;
    Top        := self.ClientHeight - Height - cuiBorder;
    Kind       := bkOK;
    ModalResult := mrNone;
    OnClick    := OnBtnFindNextClick;
    Caption    := cgsPageNameFindNext;
    TabOrder   := 99;
    Glyph      := nil;
    Enabled    := false;
  end;

  FBtnFind     := TBitBtn.Create(self);
  with FBtnFind do begin
    Parent     := Self;
    Left       := FBtnFindNext.Left - Width - cuiBorder;
    Top        := self.ClientHeight - Height - cuiBorder;
    Kind       := bkOK;
    ModalResult := mrNone;
    OnClick    := OnBtnFindClick;
    Caption    := cgsPageNameFind;
    TabOrder   := 98;
    Glyph      := nil;
    Default    := true;
    Enabled    := false;
  end;

  FEditFilter := TtiEditFilter.Create(self);
  with FEditFilter do begin
    parent := self;
    Top   := cuiBorder;
    Left  := cuiBorder;
    Height := FBtnFind.Top - cuiBorder*2;
    Width := self.ClientWidth  - cuiBorder*2;
    OnChangeValid := DoOnChangeValid;
  end;

end;

destructor TtiLVFindDialog.Destroy;
begin
//  gReg.WriteFormState(self);
  inherited;
end;

procedure TtiLVFindDialog.Find;
begin
  ActiveControl := FEditFilter.FirstControl;
  if Visible then
  begin
    ShowWindow(Handle, SW_RESTORE	);
    SetForegroundWindow(Handle)
  end else
    Show;
end;

//procedure TtiLVFindDialog.PositionInListView(AIndex : Integer);
//var
//  liItemHeight : integer;
//begin

{
  liItemHeight := LV.Canvas.TextHeight('M') + 1;
  LV.Scroll(0, liItemHeight * LV.TopItem.Index * -1);

  if AIndex > LV.VisibleRowCount then
    LV.Scroll(0, liItemHeight * AIndex);

  LV.Selected := LV.Items[AIndex];
  LV.Selected.Focused := true;
}
//end;

procedure TtiLVFindDialog.OnBtnFindClick(sender: TObject);
begin
  DoFind(0);
end;

procedure TtiLVFindDialog.OnBtnFindNextClick(sender: TObject);
begin
  FindNext;
end;

procedure TtiLVFindDialog.OnBtnCloseClick(sender: TObject);
begin
  Close;
end;

procedure TtiLVFindDialog.FindNext;
var
  liIndex : integer;
begin
  if LV.Selected = nil then
    liIndex := 0
  else
    liIndex := LV.Selected.Index + 1;
  DoFind(liIndex);
end;

procedure TtiListViewPlus.DoExpToClipboard;
var
  lStream   : TStringStream;
  lBuffer   : PChar;
begin

  if not (lvcfExport in FConfigFeatures) then
    Exit; //==>

  lStream := TStringStream.Create('');
  try
    ListToStream(lStream,
                  '', ',', tiLineEnd,
                  '', '', ',', tiLineEnd, '');
    GetMem(lBuffer, lStream.Size);
    try
      lStream.Position := 0;
      lStream.ReadBuffer(lBuffer^, lStream.Size);
      Clipboard.SetTextBuf(lBuffer);
    finally
      FreeMem(lBuffer);
    end;
  finally
    lStream.Free;
  end;
  MessageDlg('Data has been copied to the clipboard.', mtInformation, [mbOK], 0);
end;

procedure TtiListViewPlus.DoExpToCSV;
var
  lSaveDialog : TSaveDialog;
  lStream   : TFileStream;
begin

  if not (lvcfExport in FConfigFeatures) then
    Exit; //==>

  lSaveDialog := TSaveDialog.Create(nil);
  try
    lSaveDialog.DefaultExt := 'CSV';
    lSaveDialog.FileName := CSVFileName;
    lSaveDialog.InitialDir := ExtractFilePath(lSaveDialog.FileName);
    lSaveDialog.Filter := 'CSV Files|*.CSV|All files|*.*';
    if lSaveDialog.Execute then
      CSVFileName := lSaveDialog.FileName
    else
      exit; //==>
  finally
    lSaveDialog.Free;
  end;

  if CSVFileName = '' then
    raise exception.Create('Invalid file name <' + CSVFileName + '>');

  lStream := TFileStream.Create(CSVFileName, fmCreate or fmShareDenyNone);
  try
    ListToStream(lStream,
                  '', ',', tiLineEnd,
                  '', '', ',', tiLineEnd, '');
  finally
    lStream.Free;
  end;

  ViewFile(CSVFileName);

end;

procedure TtiListViewPlus.ViewFile(const AFileName : string);
begin
{$IFNDEF FPC}
  if (MessageDlg(AFileName + ' has been created.' + #13 + #13 +
                 'Do you want to view it now?',
                 mtConfirmation, [mbYes,mbNo], 0) = mrYes) then
    ShellExecute(screen.activeForm.handle,
                  nil,
                  PChar(AFileName),
                  nil,
                  nil,
                  SW_SHOWNORMAL);
{$ELSE}
{$WARNING TODO: Fix it using TProcess ?}
MessageDlg(AFileName + ' has been created.' + #13 + #13 +
                 'You can to view it now.',
                 mtInformation, [mbYes], 0);
{$ENDIF}

end;

procedure TtiListViewPlus.DoExpToHTML;
var
  lSaveDialog : TSaveDialog;
  lStream   : TFileStream;
begin

  if not (lvcfExport in FConfigFeatures) then
    Exit; //==>

  lSaveDialog := TSaveDialog.Create(nil);
  try
    lSaveDialog.DefaultExt := 'HTM';
    lSaveDialog.FileName := HTMLFileName;
    lSaveDialog.InitialDir := ExtractFilePath(lSaveDialog.FileName);
    lSaveDialog.Filter := 'HTML Files|*.HTM|All files|*.*';
    if lSaveDialog.Execute then begin
      HTMLFileName := lSaveDialog.FileName;
    end else begin
      exit; //==>
    end;
  finally
    lSaveDialog.Free;
  end;

  if HTMLFileName = '' then
    raise exception.Create('Invalid file name <' + HTMLFileName + '>');

    lStream := TFileStream.Create(HTMLFileName, fmCreate or fmShareDenyNone);
    try
      ListToHTMLStream(lStream);
    finally
      lStream.Free;
    end;

  ViewFile(HTMLFileName);

end;

procedure TtiListViewPlus.ListToHTMLStream(AStream : TStream);
begin
  WriteTextToStream(AStream, '<html>');
  WriteTextToStream(AStream, '<head>');
  WriteTextToStream(AStream, '<title>' + Caption + '</title>');
  WriteTextToStream(AStream, '</head>');
  WriteTextToStream(AStream, '<body>');
  WriteTextToStream(AStream, '<P>');
  WriteTextToStream(AStream, '<h1><center>' + ListViewCaption + '</center></h1>');
  WriteTextToStream(AStream, '<p>');
  WriteTextToStream(AStream, '<table>');
  WriteTextToStream(AStream, '<table border=1 align="center">');
  WriteTextToStream(AStream, '<col align="center" span=' +
           IntToStr(ListColumns.Count) + '>');
  WriteTextToStream(AStream, '<tr>');
  ListToStream(     AStream,
                     '<th>', '</th>', '',
                     '<tr>', '<td>', '</td>', '</tr>', '&nbsp;');
  WriteTextToStream(AStream, '</table>');
  WriteTextToStream(AStream, '</body>');
  WriteTextToStream(AStream, '</html>');
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTSFilter
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiTSFilter.Create(AOwner: TComponent);
begin
  inherited;
  Caption             := cgsPageNameFilter;
  FEditFilter         := TtiEditFilter.Create(self);
  FEditFilter.Parent  := self;
  FEditFilter.Align   := alClient;
  FEditFilter.LVConfig := LVConfig;
end;

procedure TtiTSFilter.SetLVConfig(const Value: TtiLVConfig);
var
  i : integer;
begin
  inherited SetLVConfig(Value);
  FEditFilter.LVConfig := LVConfig;

  for i := 0 to LVConfig.Filters.Count - 1 do
    FEditFilter.AddFieldEdit(TObject(FLVConfig.Filters[i]));

  if FEditFilter.EditList.Count = 0 then
    FEditFilter.AddFieldEdit(nil);

  FEditFilter.Apply := LVConfig.ApplyFilter;
  FEditFilter.EnableApply := true;

end;

procedure TtiTSFilter.Apply(pLV: TtiListViewPlus);
begin
  // Filters must be applied accross different sets of data, so they are
  // applied at the listView level.
  // Clear any existing filters
  // Assign the filters from the edit panel to the LVConfig
  FEditFilter.AssignFilters(LVConfig.Filters);
  // Set the ApplyFilter flag
  LVConfig.ApplyFilter := FEditFilter.Apply;
  // Apply the filter to the data in the list view

  FLVConfig.LV.ApplyFilter := FEditFilter.Apply;

end;

procedure TtiLVConfig.ClearFinds;
var
  i : integer;
begin
  for i := 0 to FFinds.Count - 1 do
    TObject(FFinds.Items[i]).Free;
  FFinds.Clear;
end;

procedure TtiLVFindDialog.SetLVConfig(value: TtiLVConfig);
var
  i : integer;
  ls : string;
  lEdit: TtiLVFieldEdit;
begin

  FLVConfig := Value;
  FEditFilter.LVConfig := LVConfig;

  for i := 0 to FEditFilter.EditList.Count - 1 do
  begin
    lEdit:= FEditFilter.EditList.Items[i] as TtiLVFieldEdit;
    ls := lEdit.FcbField.Text;
    (lEdit).AssignAvailableCols(LVConfig.LV.ListColumns);
    lEdit.FcbField.ItemIndex := lEdit.FcbField.Items.IndexOf(ls);
  end;

//  for i := 0 to LVConfig.Finds.Count - 1 do
//    FEditFilter.AddFieldEdit(FLVConfig.Finds[i]);

  if FEditFilter.EditList.Count = 0 then
    FEditFilter.AddFieldEdit(nil);

  FEditFilter.EnableApply := false;

end;

function TtiLVFindDialog.GetLV: TtiListViewPlus;
begin
  result := LVConfig.LV;
end;

procedure TtiLVFindDialog.DoFind(const piStart: integer);
var
  i : integer;
  lsMessage : string;
  liIndex : integer;
  lStart : TObject;
begin

  FEditFilter.AssignFilters(LVConfig.Finds);

  lsMessage := '';
  if not LV.ValidateFilters(lsMessage,
                             LVConfig.Finds) then
    raise exception.Create('Error in find condition: ' +
                            lsMessage);

  if piStart <= LV.DataInternal.Count - 1 then
    i := piStart
  else
    i := 0;

  lStart := TObject(LV.DataInternal.Items[ i ]);
  liIndex := -1;


  while true do begin
    if not LV.DoesDataPassFilter(TtiObject(LV.DataInternal.Items[i]), LVConfig.Finds) then begin
      inc(i);
      if i > LV.DataInternal.Count - 1 then
        i := 0;
    end else begin
      liIndex := i;
      Break; //==>
    end;

    if TObject(LV.DataInternal.Items[i]) = lStart then
      Break; //==>

  end;

  if liIndex <> -1 then
    LV.PositionCursor(liIndex)
  else
    ShowError('There are no records matching your search filter.');
end;

procedure TtiLVMenu.DoOnPopup(sender: TObject);
begin
  FpmiFindNext.Enabled :=
    (LV.LVFindDialog <> nil) and
    (LV.LVFindDialog.Valid);

  FpmiCols.Visible    := (lvcfCols   in LV.ConfigFeatures);
  FpmiFilter.Visible  := (lvcfFilter in LV.ConfigFeatures);
  FpmiSort.Visible    := (lvcfSort   in LV.ConfigFeatures);
  FpmiFind.Visible    := (lvcfFind   in LV.ConfigFeatures);
  FpmiFindNext.Visible := (lvcfFind   in LV.ConfigFeatures);
  FpmiExport.Visible  := (lvcfExport in LV.ConfigFeatures);
//    FpmiExpToCSV    : TMenuItem;
//    FpmiExpToClip   : TMenuItem;
//    FpmiExpToHTML   : TMenuItem;


end;

function TtiLVFindDialog.GetValid: boolean;
begin
  result := FEditFilter.Valid;
end;

procedure TtiLVFindDialog.DoOnChangeValid(sender: TObject);
var
  lbValid : boolean;
begin
  lbValid := (sender as TtiEditFilter).Valid;
  FBtnFind.Enabled    := lbValid;
  FBtnFindNext.Enabled := lbValid;
  FBtnFind.Default    := lbValid;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVFieldEdit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiLVFieldEdit.AssignAvailableCols(const pListColumns: TtiListColumns);
var
  i : integer;
begin
  FcbField.Items.Clear;
  if pListColumns = nil then
  begin
    FcbField.ItemIndex := -1;
    Exit; //==>
  end;
  for i := 0 to pListColumns.Count - 1 do
    FcbField.Items.AddObject(pListColumns.Items[i].DisplayLabel, pListColumns.Items[i]);
  FcbField.Items.Insert(0, '');
  FcbField.ItemIndex := 0;
end;

constructor TtiLVFieldEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Visible := false;
  Parent := (AOwner as TWinControl);
  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner  := bvNone;
  BevelOuter  := bvNone;

  FcbField   := TComboBox.Create(self);
  FcbField.Parent := self;
  FcbField.Top := cuiBorder;
  FcbField.Left := cuiBorder;
  FcbField.Width := 97;
  FcbField.Style := csDropDownList;
  FcbField.OnChange := OnChangeField;

  ClientHeight := 22 + 4 + 4;
  ClientWidth := (AOwner as TWinControl).ClientWidth - 4 - 4;

end;

destructor TtiLVFieldEdit.Destroy;
begin
  FcbField.Free;
  inherited;
end;

function TtiLVFieldEdit.GetFieldDisplay: string;
begin
  result := FcbField.Items[ FcbField.ItemIndex ];
end;

function TtiLVFieldEdit.GetFieldPropName: string;
var
  lItem : TtiListColumn;
begin
  lItem := FcbField.Items.Objects[ FcbField.ItemIndex ] as TtiListColumn;
  result := lItem.FieldName;
end;

function TtiLVFieldEdit.GetShowNextField: boolean;
begin
  result := Valid;
end;

function TtiLVFieldEdit.GetValid: boolean;
begin
  // Do nothing, implement in the concrete.
  Result := false;
end;

procedure TtiLVFieldEdit.OnChangeField(sender: TObject);
begin
  if Assigned(FOnChangeFieldEdit) then
    FOnChangeFieldEdit(self);
  if ShowNextField and Assigned(FOnNewFieldEdit) then
    FOnNewFieldEdit(self);
end;

{
procedure TtiLVFieldEdit.SetFieldDisplay(const Value: string);
begin
  FcbField.ItemIndex :=
    FcbField.Items.IndexOf(Value);
  OnChangeField(nil);
end;
}

procedure TtiLVFilterEdit.OnChangeField(sender: TObject);
begin
  FcbConj.Visible :=
    (FcbField.ItemIndex = -1) or
    (FcbField.Items[FcbField.ItemIndex] <> '') and
    (FcbOperator.ItemIndex <> ord(foNone)) and
    (FbShowConj);

  if (FcbConj.ItemIndex <> -1) and
     (FbShowConj) then
    OnChangeConj(FcbConj);

  inherited OnChangeField(sender);

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVSortEdit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVSortEdit.Create(AOwner: TComponent);
var
  i : TlvSortDirection;
begin
  inherited;
  FcbDirection         := TComboBox.Create(self);
  FcbDirection.Parent  := self;
  FcbDirection.Top     := cuiBorder;
  FcbDirection.Left    := FcbField.Left + FcbField.Width + cuiBorder;
  FcbDirection.Width   := 100;
  FcbDirection.Style   := csDropDownList;
  FcbDirection.OnChange := OnChangeField;
  for i := low(TlvSortDirection) to High(TlvSortDirection) do
    FcbDirection.Items.Add(cSortDirections[ ord(i)]);
  FcbDirection.ItemIndex := ord(lvsdAscending);

  FcbThenBy         := TCheckBox.Create(self);
  FcbThenBy.Parent  := self;
  FcbThenBy.Top     := cuiBorder;
  FcbThenBy.Left    := FcbDirection.Left + FcbDirection.Width + cuiBorder;
  FcbThenBy.Width   := 100;
  FcbThenBy.Caption := 'Then by';
  FcbThenBy.OnClick := OnChangeField;

end;

destructor TtiLVSortEdit.Destroy;
begin
  FcbDirection.Free;
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiEditFilter
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiEditFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TtiEditConditionAbs.GetApply: boolean;
begin
  result := FcbApply.Checked;
end;

procedure TtiEditConditionAbs.SetApply(const Value: boolean);
begin
  FcbApply.Checked := Value;
end;

procedure TtiEditConditionAbs.DoAddFieldEdit(pFieldEdit: TtiLVFieldEdit);
begin
  if EditList.Count < cuiMaxFilterCount then begin

    // Hook into the NewFilterEdit and ChangeFilterEdit events
    pFieldEdit.OnNewFieldEdit   := OnNewFieldEdit;
    pFieldEdit.OnChangeFieldEdit := OnChangeFieldEdit;

    // The new filterEdit must be added to the list before Index is set.
    // Calcuate the top
    pFieldEdit.Top := (EditList.Count *
                        (pFieldEdit.Height + cuiBorder)) +
                      cuiBorder;

    // Set the index value
    pFieldEdit.Index := EditList.Count;

    pFieldEdit.Visible := true;

    EditList.Add(pFieldEdit);

    HideNextField(pFieldEdit);

    DoEnableApply;

  end else
    pFieldEdit.Free;

end;

function TtiEditConditionAbs.GetValid: boolean;
var
  i : integer;
begin
  result := true;
  for i := 0 to EditList.Count - 1 do begin
    if not (TObject(EditList.Items[i]) as TtiLVFieldEdit).Valid then begin
      result := false;
      break; //==>
    end;
  end;
end;

procedure TtiEditConditionAbs.DoEnableApply;
var
  lbValid : boolean;
begin
  lbValid := Valid;

  if EnableApply then begin
    FcbApply.Checked := lbValid;
    FcbApply.Enabled := lbValid;
  end;
end;

procedure TtiEditConditionAbs.OnChangeFieldEdit(sender: TObject);
begin

  DoEnableApply;

  // Send information that the valid state may have changed to who ever
  // is interested.
  if Assigned(FOnChangeValid) then
    FOnChangeValid(self);

  HideNextField(sender as TtiLVFieldEdit);

end;

procedure TtiEditConditionAbs.HideNextField(pFieldEdit: TtiLVFieldEdit);
var
  i : integer;
begin
  with pFieldEdit do begin
    for i := Index+1 to EditList.Count - 1 do
      TtiLVFieldEdit(EditList.Items[i]).Visible := False;
  end;

  // Move the ApplyFilter combobox to accomidate the new filterEdit
  FcbApply.Left := cuiBorder;
  FcbApply.Top := pFieldEdit.Top + pFieldEdit.Height + cuiBorder;

end;

procedure TtiEditConditionAbs.OnNewFieldEdit(sender: TObject);
var
//  i : integer;
  lNextEdit : TtiLVFieldEdit;
begin
  if EditList.Count = 0 then
    exit;

  with (sender as TtiLVFieldEdit) do begin
    if ShowNextField then begin
      if Index = EditList.Count - 1 then
        AddFieldEdit(nil)
      // There are more filterEdits in the list that have been hidden by
      // a preavious process, so show them
      else if ((Index+1) <= (EditList.Count - 1)) then begin
        lNextEdit := TtiLVFilterEdit(EditList.Items[Index+1]);
        lNextEdit.Visible := true;
        lNextEdit.OnNewFieldEdit(lNextEdit);
      end;
    // Invalid, so hide all remaining filter edits
    end else begin
      HideNextField(sender as TtiLVFieldEdit);
    end;
  end;
  // Send information that the valid state may have changed to who ever
  // is interested.
  if Assigned(FOnChangeValid) then
    FOnChangeValid(self);

end;

function TtiLVSortEdit.GetDirection: TlvSortDirection;
begin
  result := TlvSortDirection(FcbDirection.ItemIndex);
end;

function TtiLVSortEdit.GetShowNextField: boolean;
begin
  result := Visible and
            ThenBy  and
            Valid;
end;

function TtiLVSortEdit.GetThenBy: boolean;
begin
  result := FcbThenBy.Checked;
end;

function TtiLVSortEdit.GetValid: boolean;
begin
  result := (not Visible) or
             (FieldDisplay <> '');
end;

procedure TtiLVSortEdit.SetDirection(Value: TlvSortDirection);
begin
  FcbDirection.ItemIndex := ord(Value);
end;

{ TtiEditSort }

procedure TtiEditSort.AddFieldEdit(AData: TObject);
var
  lSortEdit : TtiLVSortEdit;
  lOld      : integer;
begin
  lSortEdit    := TtiLVSortEdit.Create(self);
  lSortEdit.AssignAvailableCols(LVConfig.LV.ListColumns);
  if AData <> nil then
    with (AData as TtiLVSort) do
    begin
      lSortEdit.FieldPropName := Field    ;
      lSortEdit.Direction := Direction;
    end;
  lOld := EditList.Count;
  DoAddFieldEdit(lSortEdit);
  if (lOld > 0) and (EditList.Count > lOld) then
    (EditList[Pred(lOld)] as TtiLVSortEdit).ThenBy := true;
end;

procedure TtiEditSort.AssignSorts(AList: TList);
var
  i      : integer;
  lSort  : TtiLVSort;
  lSortEdit : TtiLVSortEdit;
begin
  AList.Clear;
  for i := 0 to EditList.Count - 1 do
  begin
    lSortEdit := (TObject(EditList.Items[i]) as TtiLVSortEdit);
    if lSortEdit.Valid and lSortEdit.Visible then
    begin
      lSort := TtiLVSort.Create;
      lSort.Field    := lSortEdit.FieldPropName;
      lSort.Direction := lSortEdit.Direction;
      lSort.ThenBy   := lSortEdit.ThenBy;
      AList.Add(lSort);
    end;
  end;
end;

constructor TtiEditSort.Create(AOwner: TComponent);
begin
  inherited;
  MaxFilterCount   := 1;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVSort
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiLVSort.Clone: TtiLVSort;
begin
  result := TtiLVSort.Create;
  result.Field := Field;
  result.Direction := Direction;
  result.ThenBy := ThenBy;
end;

constructor TtiLVSort.Create;
begin
  inherited;
  Field := '';
  Direction := lvsdAscending;
end;

procedure TtiLVSortEdit.SetThenBy(const Value: boolean);
begin
  FcbThenBy.checked := Value;
end;

function TtiLVConfig.GetSorts: TList;
begin
  result := FSorts;
end;

{
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVClassMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVClassMapping.CreateExt(const pStrClassID: string;AClassRef: TtiLVClassRef);
begin
  Create;
  FStrClassID := pStrClassID;
  FClassRef  := AClassRef;
end;
}
{
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVFactoryAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLVFactoryAbs.Create;
begin
  inherited Create;
  FClassMappings := TStringList.Create;
  FObjectCache  := TStringList.Create;
end;

function TtiLVFactoryAbs.CreateInstance(const psClassID: string; owner : TComponent): TComponent;
var lIntCacheIndex  : integer;
    lIntMappingIndex : integer;
    lStrClassID     : string;
    lClassMapping   : TtiLVClassMapping;
begin

  // Get a temporary copy of ClassID, in upper case
  lStrClassID := upperCase(psClassID);

  // Does the class mapping exist?
  lIntMappingIndex := FClassMappings.IndexOf(lStrClassID);

  // If not, then raise an exception
  // We can raise an exception here as we are not likely to be inside
  // initialization code
  if lIntMappingIndex = -1 then
    Raise Exception.Create('Request for invalid class ' +
                            'name <' +
                            psClassID + '>' +
                            ' Called by factory: ' +
                            ClassName);

  // Is the object already in the cache?
  // Yes, then return the cahced copy
  // No, then create one
  lIntCacheIndex := FObjectCache.IndexOf(lStrClassID);

  // The object is not already in the cache
  if lIntCacheIndex = -1 then begin
    // Get a pointer to the correct class mapping
    lClassMapping := TtiLVClassMapping(
                FClassMappings.Objects[lIntMappingIndex]);

    result := TtiLVClassMapping(lClassMapping).ClassRef.Create(owner);
    FObjectCache.AddObject(lStrClassID, result);

  // The object is already in the cache
  end else begin
    // So return the existing copy
    result := TComponent(FObjectCache.Objects[ lIntCacheIndex ]);

  end;

end;

destructor TtiLVFactoryAbs.Destroy;
begin
  FClassMappings.Free;
  FObjectCache.Free;
  inherited;
end;

procedure TtiLVFactoryAbs.RegisterClass(const pStrClassID: string; AClassRef: TtiLVClassRef);
var i : integer;
    lClassMapping : TtiLVClassMapping;
    lStrClassID : string;
begin
  lStrClassID := upperCase(pStrClassID);

  // Does the class mapping alread exist?
  i := FClassMappings.IndexOf(lStrClassID);

  // If yes, report an error.
  // We do not raise an exception here as we may be inside an
  // initialization section.
  if i <> -1 then begin
    messageDlg('Registering a duplicate ' +
                'class mapping <' +
                pStrClassID + '>',
                mtInformation,
                [mbOK],
                0);
    Exit; //==>
  end;

  // Create the class mapping object
  lClassMapping := TtiLVClassMapping.CreateExt(
                      lStrClassID,
                      AClassRef);

  // Add the class mapping object to the list
  FClassMappings.AddObject(upperCase(pStrClassID),
                             lClassMapping);

end;
}

procedure TtiListViewPlus.WriteTextToStream(AStream : TStream; psText : string);
var
  lpcText : PChar;
begin
  lpcText := PChar(psText);
  AStream.WriteBuffer(lpcText^, length(lpcText));
end;

function TtiLVFilter.GetSimplePropType(pPersistent: TtiObject;
  APropName: string): TtiLVTypeKind;
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
  tkEnumeration : result := tiTKInteger;

  tkFloat      : result := tiTKFloat;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  tkWString    : result := tiTKString;

  else
    raise exception.Create('Invalid property type passed to ' +
                            'tiGetSimpleTypeKind');
  end;

end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiLVMultiSelect
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiLVMultiSelect.Create(AOwner : TComponent);
begin
  FbBorder         := true;

  inherited Create(AOwner);

  FlblAvailable    := TLabel.Create(self);
  FlblSelected     := TLabel.Create(self);
  FlbAvailable     := TListBox.Create(self);
  FlbSelected      := TListBox.Create(self);
  FsbSelectMarked  := TtiSpeedButton.Create(self);
  FsbDeSelectMarked := TtiSpeedButton.Create(self);
  FsbSelectAll     := TtiSpeedButton.Create(self);
  FsbDeSelectAll   := TtiSpeedButton.Create(self);

  FslAvailable     := TStringList.Create;
  FslSelected      := TStringList.Create;

  self.height     := 290 ;
  self.width      := 475;

  with FlblAvailable do begin
    parent := self;
    top   :=  16;
    left  :=   8;
    caption := '&Available';
    FocusControl := FlbAvailable;
  end;

  with FlblSelected do begin
    parent := self;
    top   :=  16;
    left  := 272;
    caption := '&Selected';
    FocusControl := FlbSelected;
  end;

  with FlbAvailable do begin
    parent := self;
    top   :=  40;
    left  :=   8;
    width := 193;
    height := 201;
    multiSelect := true;
    onDblClick := sbSelectMarkedClick;
    onDragDrop := lbDragDrop;
    onDragOver := lbDragOver;
    onMouseDown := lbMouseDown;
    onKeyPress := lbAvailableKeyPress;
    name       := 'lbAvailable';
  end;

  with FlbSelected do begin
    parent := self;
    top   :=  40;
    left  := 272;
    width := 193;
    height := 201;
    multiSelect := true;
    onDblClick := sbDeSelectMarkedClick;
    onDragDrop := lbDragDrop;
    onDragOver := lbDragOver;
    onMouseDown := lbMouseDown;
    onKeyPress := lbSelectedKeyPress;
    name       := 'lbSelected';
  end;

  with FsbSelectMarked do begin
    parent := self;
    top   :=  56;
    left  := 224;
    width :=  25;
    height :=  25;
    hint  := 'Select marked';
    showHint := true;
    onClick := sbSelectMarkedClick;
    Glyph.LoadFromResourceName(        HInstance, cResTI_Copy1Right + cResTI_16N);
    GlyphHot.LoadFromResourceName(     HInstance, cResTI_Copy1Right + cResTI_16N);
    GlyphDisabled.LoadFromResourceName(HInstance, cResTI_Copy1Right + cResTI_16N);
  end;

  with FsbDeSelectMarked do begin
    parent := self;
    top   :=  88;
    left  := 224;
    width :=  25;
    height :=  25;
    hint  := 'Deselect marked';
    showHint := true;
    onClick := sbDeSelectMarkedClick;
    Glyph.LoadFromResourceName(        HInstance, cResTI_Copy1Left + cResTI_16N);
    GlyphHot.LoadFromResourceName(     HInstance, cResTI_Copy1Left + cResTI_16N);
    GlyphDisabled.LoadFromResourceName(HInstance, cResTI_Copy1Left + cResTI_16N);
  end;

  with FsbSelectAll do begin
    parent := self;
    top   := 152;
    left  := 224;
    width :=  25;
    height :=  25;
    hint  := 'Select all';
    showHint := true;
    onClick := sbSelectAllClick;
    Glyph.LoadFromResourceName(        HInstance, cResTI_CopyAllRight + cResTI_16N);
    GlyphHot.LoadFromResourceName(     HInstance, cResTI_CopyAllRight + cResTI_16N);
    GlyphDisabled.LoadFromResourceName(HInstance, cResTI_CopyAllRight + cResTI_16N);
  end;

  with FsbDeSelectAll do begin
    parent := self;
    top   := 184;
    left  := 224;
    width :=  25;
    height :=  25;
    hint  := 'Deselect all';
    showHint := true;
    onClick := sbDeSelectAllClick;
    Glyph.LoadFromResourceName(        HInstance, cResTI_CopyAllLeft + cResTI_16N);
    GlyphHot.LoadFromResourceName(     HInstance, cResTI_CopyAllLeft + cResTI_16N);
    GlyphDisabled.LoadFromResourceName(HInstance, cResTI_CopyAllLeft + cResTI_16N);
  end;

  application.processMessages;
end;

destructor TtiLVMultiSelect.Destroy;
begin
  FslSelected.free ;
  FslAvailable.free;
  FlblAvailable.free  ;
  FlblSelected.free   ;
  FlbAvailable.free;
  FlbSelected.free ;
  inherited Destroy     ;
end;

{$IFNDEF FPC}
procedure TtiLVMultiSelect.WMSize(var Message: TWMSize);
{$ELSE}
procedure TtiLVMultiSelect.WMSize(var Message: TLMSize);
{$ENDIF}
const
  ciBorder = 8;
  ciSBWidth = 23;
begin
  FlbAvailable.Width := (ClientWidth - (ciBorder * 4) - ciSBWidth) div 2;
  FlbSelected.Width := FlbAvailable.Width;
  FlbAvailable.Left := ciBorder;
  FlbSelected.Left := ClientWidth - ciBorder - FlbSelected.Width;
  FlblSelected.Left := FlbSelected.Left;

  FsbSelectMarked.Left  := FlbAvailable.Left + FlbAvailable.Width + ciBorder;
  FsbDeSelectMarked.Left := FsbSelectMarked.Left;
  FsbSelectAll.Left     := FsbSelectMarked.Left;
  FsbDeSelectAll.Left   := FsbSelectMarked.Left;

  FlbAvailable.Height := ClientHeight - FlbAvailable.Top - ciBorder;
  FlbSelected.Height := FlbAvailable.Height;


end;

procedure TtiLVMultiSelect.sbSelectMarkedClick(sender: TObject);
var
  i : integer;
begin
  for i := FlbAvailable.items.count - 1 downto 0 do
    if FlbAvailable.selected[ i ] then
      SelectByCaption(FlbAvailable.items[ i ]);
end;

procedure TtiLVMultiSelect.sbDeSelectMarkedClick(sender: TObject);
var
  i : integer;
begin
  for i := FlbSelected.items.count - 1 downto 0 do
    if FlbSelected.selected[ i ] then
      DeSelectByCaption(FlbSelected.items[ i ]);
end;

procedure TtiLVMultiSelect.sbSelectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := FlbAvailable.items.count - 1 downto 0 do
    SelectByCaption(FlbAvailable.items[ i ], 0);
end;

procedure TtiLVMultiSelect.sbDeSelectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := FlbSelected.items.count - 1 downto 0 do
    DeSelectByCaption(FlbSelected.items[ i ], 0);
end;

procedure TtiLVMultiSelect.lbDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if source is TListBox then
    accept := true;
end;

procedure TtiLVMultiSelect.lbDragDrop(Sender, Source: TObject; X, Y: Integer);
var i,
    liRow : integer;
    lslItems : TStringList;
begin

  // Get a temporary list of selected rows
  lslItems := TStringList.Create;
  try
    for i := TListBox(source).items.count - 1 downto 0 do
      if TListBox(source).selected[ i ] then
        lslItems.add(TListBox(source).items[i]);

    // Get the row to insert moved values
    liRow := TListBox(sender).itemAtPos(point(x, y), true);
    if liRow > TListBox(sender).items.count
    then liRow := TListBox(sender).items.count;

    // Drag from selected to available
    if (TListBox(sender).Name = 'lbAvailable') and
       (TListBox(source).Name = 'lbSelected') then begin
     for i := lslItems.count - 1 downto 0 do
        DeSelectByCaption(lslItems.Strings[ i ], liRow);

    // Drag from available to selected
    end else if (TListBox(sender).Name = 'lbSelected') and
                (TListBox(source).Name = 'lbAvailable') then begin
      for i := lslItems.count - 1 downto 0 do
        SelectByCaption(lslItems.Strings[ i ], liRow);

    // Drag from selected to selected
    end else if (TListBox(sender).Name = 'lbSelected') and
                (TListBox(source).Name = 'lbSelected') then begin
      for i := lslItems.count - 1 downto 0 do begin
        DeSelectByCaption(lslItems.Strings[ i ]);
        SelectByCaption(lslItems.Strings[ i ], liRow);
      end;

    // Drag from available to available
    end else if (TListBox(sender).Name = 'lbAvailable') and
                (TListBox(source).Name = 'lbAvailable') then begin
      for i := lslItems.count - 1 downto 0 do begin
        SelectByCaption(lslItems.Strings[ i ]);
        DeSelectByCaption(lslItems.Strings[ i ], liRow);
      end;
    end;

  finally
    lslItems.free;
  end;
end;

procedure TtiLVMultiSelect.lbMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then
    with sender as TListBox do
      if itemAtPos(point(x,y), true) >= 0 then beginDrag(false);
end;

procedure TtiLVMultiSelect.lbAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  if key = ' ' then sbSelectMarkedClick(Sender);;
end;

procedure TtiLVMultiSelect.lbSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  if key = ' ' then sbDeSelectMarkedClick(Sender);;
end;

procedure TtiLVMultiSelect.RefreshData;
var
  i : integer;
begin
  FlbAvailable.Items.Assign(FslAvailable);
  for i := 0 to FslSelected.Count - 1 do
    SelectByCaption(FslSelected.Strings[i]);
end;

procedure TtiLVMultiSelect.DeSelectByCaption(const psCaption: string; const APos : integer = -1);
var
  i : integer;
  liPos : integer;
begin
  i := FlbSelected.Items.IndexOf(psCaption);
  if i = -1 then
    exit; //==>

  if APos = -1 then
    liPos := FlbAvailable.Items.Count
  else
    liPos := APos;

  FlbAvailable.Items.Insert(liPos, psCaption);
  FlbSelected.Items.Delete(i);
  i := FslSelected.IndexOf(psCaption);
  FslSelected.Delete(i);

  if Assigned(FOnChange) then
    FOnChange(self);

end;

procedure TtiLVMultiSelect.SelectByCaption(const psCaption: string; const APos : integer = -1);
var
  i : integer;
  liPos : integer;

begin
  i := FlbAvailable.Items.IndexOf(psCaption);
  if i = -1 then
    exit; //==>

  if APos = -1 then
    liPos := FlbSelected.Items.Count
  else
    liPos := APos;

  FlbSelected.Items.Insert(APos, psCaption);
  FlbAvailable.Items.Delete(i);

  i := FslSelected.IndexOf(psCaption);
  if i = -1 then
    FslSelected.Insert(liPos, psCaption);

  if Assigned(FOnChange) then
    FOnChange(self);

end;

function TtiLVMultiSelect.GetAvailableAsString: string;
begin
  result := Available.Text;
end;

function TtiLVMultiSelect.GetSelectedAsString: string;
begin
  result := Selected.Text;
end;

procedure TtiLVMultiSelect.SetAvailableAsString(const Value: string);
begin
  Available.Text := Value;
  RefreshData;
end;

procedure TtiLVMultiSelect.SetSelectedAsString(const Value: string);
begin
  Selected.Text := Value;
  RefreshData;
end;

// This Paint method is cloned from TCustomGroupBox, with sime
// modifications to hide the border if FbBorder is false
{$IFNDEF FPC}
procedure TtiLVMultiSelect.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
begin
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);

    if Ctl3D then begin
      Inc(R.Left);
      Inc(R.Top);

      if FbBorder then
        Brush.Color := clBtnHighlight
      else
        Brush.Color := Color;

      FrameRect(R);
      OffsetRect(R, -1, -1);

      if FbBorder then
        Brush.Color := clBtnShadow
      else
        Brush.Color := Color;

    end else begin

      if FbBorder then
        Brush.Color := clWindowFrame
      else
        Brush.Color := Color;

    end;

    FrameRect(R);

    if Text <> '' then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      DrawText(Handle, PChar(Text), Length(Text), R, Flags);
    end;
  end;
end;
{$ENDIF}


procedure TtiLVMultiSelect.SetBorder(const Value: boolean);
begin
  FbBorder := Value;
  {$IFNDEF FPC}Paint;{$ENDIF}
end;

procedure TtiLVMultiSelect.SetAvailable(const Value: TStringList);
begin
  FslAvailable.Assign(Value);
end;

procedure TtiLVMultiSelect.SetSelected(const Value: TStringList);
begin
  FslSelected.Assign(Value);
end;

procedure TtiLVMultiSelect.ClearSelected;
begin
  FslSelected.Clear;
  FlbSelected.Items.Clear;
  FlbAvailable.Items.Assign(FslAvailable);
end;

procedure TtiListViewPlus.DoColumns;
begin
  if lvcfCols in FConfigFeatures then
    EditConfig(cgsPageNameColumns);
end;

procedure TtiListViewPlus.DoQuery;
begin
  if lvcfFilter in FConfigFeatures then
    EditConfig(cgsPageNameFilter);
end;

procedure TtiListViewPlus.DoSort;
begin
  if lvcfSort in FConfigFeatures then
    EditConfig(cgsPageNameSort);
end;

procedure TtiLVConfig.Assign(const AData: TtiLVConfig);
var
  i : integer;
begin
  Assert(AData <> nil, 'AData not assigned');
  Sorts.Clear;
  for i := 0 to AData.Sorts.Count - 1 do
    Sorts.Add(TtiLVSort(AData.Sorts.Items[i]).Clone);

  Filters.Clear;
  for i := 0 to AData.Filters.Count - 1 do
    Filters.Add(TtiLVFilter(AData.Filters.Items[i]).Clone);

  Finds.Clear;
  for i := 0 to AData.Finds.Count - 1 do
    Finds.Add(TtiLVFilter(AData.Finds.Items[i]).Clone);

  ApplyFilter := AData.ApplyFilter;
  ApplySort  := AData.ApplySort;
  if FLV <> nil then
  begin
    FLV.ApplyFilter := ApplyFilter;
    FLV.DoCustomApplySort(false);
    //FLV.SetApplySortNoRefresh(ApplySort, false);
  end;

end;

function TtiLVFilter.Clone: TtiLVFilter;
begin
  result := TtiLVFilter.Create;
  result.Conj := Conj;
  result.Operator  := Operator;
  result.Value     := Value;
  result.Field     := Field;
end;

function TtiLVConfig.IsCompatible(const AData: TList): boolean;
var
  i : integer;
  lData : TtiObject;
begin

  result := true;
  if (AData = nil) or
     (AData.Count = 0) or
     (not (TObject(AData.Items[0]) is TtiObject)) then
    Exit; //==>

  result := false;
  lData := TObject(AData.Items[0]) as TtiObject;

  for i := 0 to Filters.Count - 1 do
    if not IsPublishedProp(lData, TtiLVFilter(Filters.Items[i]).Field) then
      Exit; //==>

  for i := 0 to Sorts.Count - 1 do
    if not IsPublishedProp(lData, TtiLVSort(Sorts.Items[i]).Field) then
      Exit; //==>

  for i := 0 to Finds.Count - 1 do
    if not IsPublishedProp(lData, TtiLVFilter(Finds.Items[i]).Field) then
      Exit; //==>
{
  for i := 0 to ColsSelected.Count - 1 do
    if not IsPublishedProp(lData, ColsSelected.Strings[i]) then
      Exit; //==>
}
  result := true;

end;

procedure TtiListViewPlus.ClearColumns;
begin
  inherited;
  LVConfig.Clear;
end;

procedure TtiLVConfig.Clear;
begin
  ClearFilters;
  ClearSorts;
  ClearFinds;
end;

procedure TtiLVConfig.AssignAvailableColumns(AStrings: TStrings);
var
  i : integer;
begin
  Assert(FLV <> nil, 'LV not assigned');
  AStrings.Clear;
  for i := 0 to FLV.ListColumns.Count - 1 do
    AStrings.AddObject(FLV.ListColumns.Items[i].DisplayLabel,
                       FLV.ListColumns.Items[i]);
end;

procedure TtiLVConfig.AssignSelectedColumns(AStrings: TStrings);
var
  i : integer;
begin
  Assert(FLV <> nil, 'LV not assigned');
  AStrings.Clear;
  for i := 0 to FLV.ListColsBeingDisplayed.Count - 1 do
    if FLV.ListColsBeingDisplayed.Items[i] <> nil then
    begin
      Assert(FLV.ListColsBeingDisplayed.Items[i] is TtiListColumn,
             'Items[i] not a TtiListColumn, it''s a ' + FLV.ListColsBeingDisplayed.Items[i].ClassName);
      AStrings.AddObject(TtiListColumn(FLV.ListColsBeingDisplayed.Items[i]).DisplayLabel,
                         FLV.ListColsBeingDisplayed.Items[i]);
    end;
end;

procedure TtiLVFieldEdit.SetFieldPropName(const Value: string);
var
  i : integer;
  lIndex : integer;
begin
  lIndex := -1;
  for i := 0 to FcbField.Items.Count - 1 do
    if (FcbField.Items.Objects[i] <> nil) and
       ((FcbField.Items.Objects[i] as TtiListColumn).FieldName = Value) then
    begin
      lIndex := i;
      Break; //==>
    end;

  FcbField.ItemIndex := lIndex;
  OnChangeField(nil);
end;

function TtiListViewPlus.GetCSVFileName: string;
begin
  result := INIReadString('ListExport', GetINIFileIdent + 'CSV' , '');
end;

function TtiListViewPlus.GetHTMLFileName: string;
begin
  result := INIReadString('ListExport', GetINIFileIdent + 'HTM', '');
end;

procedure TtiListViewPlus.SetCSVFileName(const Value: string);
begin
  INIWriteString('ListExport', GetINIFileIdent + 'CSV' , Value);
end;

procedure TtiListViewPlus.SetHTMLFileName(const Value: string);
begin
  INIWriteString('ListExport', GetINIFileIdent + 'HTM' , Value);
end;

function TtiListViewPlus.GetINIFileIdent: string;
  function _GetOwnerName(pObj : TComponent): string;
  begin
    if pObj <> nil then
      result := pObj.Name + _GetOwnerName(pObj.Owner);
  end;
begin
  result := Name + _GetOwnerName(Owner);
end;

procedure TtiListViewPlus.ListToStream(const AStream: TStream;
  const pHeadingSepStart, pHeadingSepEnd, pHeadingLineSep,
        pLineSepStart, pDataSepStart, pDataSepEnd, pLineSepEnd,
        pEmptyChar: string);
var
  i,j : integer;
  ls : string;
begin
  for i := 0 to ListColsBeingDisplayed.Count - 1 do
    if ListColsBeingDisplayed.Items[i] <> nil then
    begin
      WriteTextToStream(AStream, pHeadingSepStart);
      WriteTextToStream(AStream, (ListColsBeingDisplayed.Items[i] as TtiListColumn).DisplayLabel);
      WriteTextToStream(AStream, pHeadingSepEnd);
    end;

  WriteTextToStream(AStream, pHeadingLineSep);

  // Write the data
  for i := 0 to DataInternal.Count - 1 do
  begin
    WriteTextToStream(AStream, pLineSepStart);
    for j := 1 to Columns.Count - 1 do
    begin
      WriteTextToStream(AStream, pDataSepStart);
      ls := GetCellText(i, j-1);
      if pDataSepStart <> '' then
        ls := StringReplace(ls, pDataSepStart, '', [rfReplaceAll, rfIgnoreCase]);
      if pDataSepEnd <> '' then
        ls := StringReplace(ls, pDataSepEnd, '', [rfReplaceAll, rfIgnoreCase]);
      if ls = '' then
        ls := pEmptyChar;
      WriteTextToStream(AStream, ls + pDataSepEnd);
    end;
    WriteTextToStream(AStream, pLineSepEnd);
  end;

end;

function TtiListViewPlus.INIReadString(const pSection, pIdent,pDefault: string): string;
var
  lFileName : string;
  lINI : TINIFile;
begin
  lFileName := ChangeFileExt(ParamStr(0), '.INI');
  lINI := TINIFile.Create(lFileName);
  try
    result := lINI.ReadString(pSection, pIdent, pDefault);
  finally
    lINI.Free;
  end;
end;

procedure TtiListViewPlus.INIWriteString(const pSection, pIdent,AValue: string);
var
  lFileName : string;
  lINI : TINIFile;
begin
  lFileName := ChangeFileExt(ParamStr(0), '.INI');
  lINI := TINIFile.Create(lFileName);
  try
    lINI.WriteString(pSection, pIdent, AValue);
  finally
    lINI.Free;
  end;
end;

function TtiEditConditionAbs.FirstControl: TWinControl;
begin
  if FEditList.Count >= 1 then
    Result := TtiLVFieldEdit(FEditList.Items[0]).FcbField
  else
    Result := nil;
end;

procedure TtiListViewPlus.ClearFindDialog;
begin
  FreeAndNil(FLVFindDialog);
end;

end.

