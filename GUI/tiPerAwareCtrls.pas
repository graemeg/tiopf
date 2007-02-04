{$I tiDefines.inc}

{
  TODO: Stretch icon
}

unit tiPerAwareCtrls;

interface
uses
 {$IFNDEF FPC}
  Windows
  ,OleCtnrs
  ,Messages
  {$ELSE}
  lmessages
  ,lcltype
  ,lclintf
  ,interfacebase
  ,editbtn
  ,variants
  {$ENDIF}
  ,registry
  ,ActnList
  ,SysUtils
  ,Classes
  ,Graphics
  ,Controls
  ,Forms
  ,StdCtrls
  ,extCtrls
  ,comctrls
  ,Buttons
  ,Menus
  ,tiFocusPanel
  ,tiResources
  ,tiObject
 ;

const
  // Error messages
  cErrorInvalidLabelStyle        = 'Invalid label style';
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';
  cErrorPropTypeNotClassOrString = 'FieldName property not an object or string';

  // Default values
  cuiDefaultLabelWidth    = 80;
  cDefaultHeightSingleRow = 24;
  cDefaultHeightMultiRow  = 41;
  {$IFDEF GUI_FIXED_FONT}
  cDefaultFixedFontName   = 'Courier New';
  {$ELSE}
  cDefaultFixedFontName   = 'MS Sans Serif';
  {$ENDIF}
  cCaption = 'Caption';

type
  TtiAction = class(TAction)
  end;

  // LabelStyle can have these values
  TLabelStyle = (lsNone, lsTop, lsLeft, lsTopLeft, lsRight);
  
  {$IFNDEF FPC}
   TTranslateString = string;
  {$ENDIF}



  // Abstract base class

  { TtiPerAwareAbs }

  TtiPerAwareAbs = class(TtiFocusPanel)
  private
    FHint : TTranslateString;
    function GetLabelFont: TFont;
    function GetLabelParentFont: Boolean;
    procedure SetLabelFont(const AValue: TFont);
    procedure SetLabelParentFont(const AValue: Boolean);
    procedure SetLayout(const AValue: TTextLayout);
  protected
    FLabelStyle : TLabelStyle;
    FLabel     : TLabel;
    FWinControl : TWinControl;
    FLayout : TTextLayout;
    FbCenterWhenLabelIsLeft : boolean;
    FsFieldName: string;
    FData: TtiObject;
    FOnChange: TNotifyEvent;
    FiLabelWidth : integer;
    FbDirty : boolean;
    FbReadOnly: Boolean;
    FbError : boolean;
    FErrorColor : TColor;
    FGreyWhenReadOnly: boolean;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDown: TKeyEvent;

    procedure   SetLabelStyle(const AValue: TLabelStyle); virtual;
    function    GetCaption: TCaption; virtual;
    procedure   SetCaption(const AValue: TCaption); virtual;
    procedure   SetData(const AValue: TtiObject); virtual;
    {$IFNDEF FPC}
    procedure   WMSize(var Message: TWMSize); message WM_SIZE;
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure   DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
    procedure   FontChanged(Sender: TObject); override;
    {$ENDIF}
    procedure   PositionLabel; virtual;
    procedure   PositionWinControl; virtual;
    procedure   SetLabelWidth(const AValue: Integer); virtual;
    procedure   SetFieldName(const AValue: string); virtual;
    procedure   SetWordWrap(const AValue : Boolean);
    function    GetWordWrap : Boolean;

    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure   Loaded; override;
    procedure   DataToWinControl; virtual; abstract;
    procedure   WinControlToData; virtual; abstract;
    procedure   DoChange(Sender : TObject); virtual;
    procedure   SetOnChangeActive(AValue : boolean); virtual; abstract;
    procedure   SetEnabled(AValue : boolean); Override;
    procedure   SetHint(const AValue : TTranslateString);{$IFDEF FPC} override;{$ENDIF}
    procedure   SetReadOnly(const AValue: Boolean);virtual;
    procedure   SetControlColor; virtual;
    procedure   SetError(const AValue: boolean); virtual;
    procedure   SetErrorColor(const AValue: TColor); virtual;
    procedure   SetGreyWhenReadOnly(const AValue: boolean); virtual;
    procedure   DoOnClick(Sender : TObject); override;
    procedure   DoOnKeyPress(Sender : TObject; var Key : Char); virtual;
    procedure   DoOnKeyDown(Sender : TObject; var Key: Word; Shift: TShiftState);

    property    CenterWhenLabelIsLeft: boolean read FbCenterWhenLabelIsLeft write FbCenterWhenLabelIsLeft;
    property    OnKeyPress : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property    OnKeyDown : TKeyEvent read FOnKeyDown write FOnKeyDown;

    function    DataAndPropertyValid : boolean;
    function    IsPropReadOnly : boolean; virtual;

  published
    property    Align;
    property    Anchors;
    property    Constraints;
    property    Enabled;
    property    Visible;
    property    Font;
    property    Color;
    property    TabOrder;
    property    OnEnter;
    property    OnExit;
    property    ShowFocusRect;
    property    ShowHint;
    property    ParentColor;
    property    ParentCtl3D;
    property    ParentFont;
    {$IFDEF FPC}
    property    Hint;
    {$ELSE}
    property    Hint read FHint write SetHint;
    {$ENDIF}

    property    LabelStyle : TLabelStyle read FLabelStyle   write SetLabelStyle default lsLeft;
    property    LabelLayout: TTextLayout read FLayout write SetLayout default tlCenter;
    property    LabelWordWrap : Boolean read GetWordWrap write SetWordWrap default false;
    property    Caption   : TCaption    read GetCaption    write SetCaption;
    property    LabelWidth : Integer     read FiLabelWidth  write SetLabelWidth default cuiDefaultLabelWidth;
    property    LabelFont : TFont       read GetLabelFont  write SetLabelFont;
    property    LabelParentFont : Boolean read GetLabelParentFont write SetLabelParentFont;
    property    ReadOnly  : Boolean     read FbReadOnly    write SetReadOnly;

    property    FieldName : string      read FsFieldName   write SetFieldName;

    property    Error : boolean read FbError write SetError default False;
    property    ErrorColor : TColor read FErrorColor write SetErrorColor default clYellow;
    property    GreyWhenReadOnly : boolean read FGreyWhenReadOnly write SetGreyWhenReadOnly default true;

    // TWinControl has these, so they can be implemented with a direct mapping
    //property    OnEnter   : TNotifyEvent read FOnEnter write SetOnExit;
    //property    OnExit    : TNotifyEvent read FOnExit  write SetOnExit;
    //property    OnKeyDown
    //property    OnKeyPress
    //property    OnKeyUp

    // TWinControl does not have this, so it must be implemented for each type of control
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   SetFocus; override;
    property    Data      : TtiObject read FData         write SetData;
    procedure   LinkToData(AData : TtiObject; const AFieldName : string); virtual;
    property    Dirty : boolean read FbDirty;
    procedure   Refresh; virtual;
    property    WinControl: TWinControl read FWinControl write FWinControl;
    function    Focused: Boolean; override;
  end;

  // A wrapper for the TEdit control
  TtiPerAwareEdit = class(TtiPerAwareAbs)
  private
    function  GetValue: String;
    procedure SetValue(const AValue: String);
    function  GetMaxLength: integer;
    procedure SetMaxLength(const AValue: integer);
    function  GetCharCase: TEditCharCase;
    procedure SetCharCase(const AValue: TEditCharCase);
    function  GetPasswordChar: Char;
    procedure SetPasswordChar(const AValue: Char);
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  published
    property Value : String read GetValue write SetValue;
    property MaxLength : integer read GetMaxLength write SetMaxLength;
    property CharCase : TEditCharCase read GetCharCase write SetCharCase;
    property PasswordChar : Char read GetPasswordChar write SetPasswordChar;
    property OnKeyPress;
    property OnKeyDown;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  // A wrapper for the TMemo control
  TtiPerAwareMemo = class(TtiPerAwareAbs)
  private
    function  GetScrollBars: TScrollStyle;
    procedure SetScrollBars(const AValue: TScrollStyle);
    function  GetValue: string;
    procedure SetValue(const AValue: string);
    function  GetWordWrap: boolean;
    procedure SetWordWrap(const AValue: boolean);
    function  GetMaxLength: integer;
    procedure SetMaxLength(const AValue: integer);
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  published
    property ScrollBars : TScrollStyle read GetScrollBars write SetScrollBars;
    property Value : string read GetValue write SetValue;
    property WordWrap : boolean read GetWordWrap write SetWordWrap;
    property MaxLength : integer read GetMaxLength write SetMaxLength;
    property OnKeyPress;
    property OnKeyDown;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  // An abstract wrapper for the TComboBox control
  TtiPerAwareComboBoxAbs = class(TtiPerAwareAbs)
  private
    function  GetDropDownCount: integer;
    procedure SetDropDownCount(const AValue: integer);
    function  GetItemIndex: integer;
    procedure SetItemIndex(const AValue: integer);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const AValue: TEditCharCase);
  protected
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
    procedure   DoOnExit(Sender: TObject); virtual;
//    procedure   DoOnClick(Sender : TObject); override;
    function    ComboBox : TComboBox;
  published
    property    DropDownCount : integer read GetDropDownCount write SetDropDownCount;
    property    CharCase : TEditCharCase read GetCharCase write SetCharCase;
  public
    constructor Create(AOwner : TComponent); override;
    property    ItemIndex : integer read GetItemIndex write SetItemIndex;
    procedure   DoOnKeyPress(Sender : TObject; var Key : Char); override;
  end;

  // A wrapper for the TComboBox control that has items entered at design time
  TtiPerAwareComboBoxStatic = class(TtiPerAwareComboBoxAbs)
  protected
    function    GetValue: String; virtual;
    procedure   SetValue(const AValue: String); virtual;
    function    GetItems: TStrings; virtual;
    procedure   SetItems(const AValue: TStrings); virtual;
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
  published
    property Value : String read GetValue write SetValue;
    property Items : TStrings read GetItems write SetItems;
  end;

  TtiPerAwareComboBoxHistory = class(TtiPerAwareComboBoxStatic)
  private
    FOnValidate : TNotifyEvent;
    FiHistoryCount: integer;
    FPopupMenu : TPopupMenu;
    FpmiClear : TMenuItem;

    function  GetRegINIFile : TRegINIFile;
    procedure SetHistoryCount(const iValue: integer);
    procedure pmiClearOnClick(sender : TObject);

  protected
    procedure   SetValue(const AValue: String); override;
    procedure   Loaded; override;
    procedure   DoOnExit(Sender : TObject); override;

  published
    property HistoryCount : integer read FiHistoryCount write setHistoryCount;
    property OnValidate : TNotifyEvent read FOnValidate write FOnValidate;
    property OnKeyPress;
    property OnKeyDown;

  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    procedure   Save;
    procedure   Read;

  end;

  // For backward compatability with existing systems.
  TtiHistoryComboBox = class(TtiPerAwareComboBoxHistory);

  TOnSetObjectPropEvent = procedure(const AData : TtiObject) of object;
  TOnGetObjectPropEvent = procedure(var   AData : TtiObject) of object;
  // A wrapper for the TComboBox control that has items populated from a
  // TList of TtiObject(s)
  TtiPerAwareComboBoxDynamic = class(TtiPerAwareComboBoxAbs)
  private
    FList      : TList;
    FsFieldNameDisplay: string;
    FOnGetObjectProp: TOnGetObjectPropEvent;
    FOnSetObjectProp: TOnSetObjectPropEvent;
    FValueAsString : string;
    function    GetValue: TtiObject;
    procedure   SetValue(const AValue: TtiObject);
    procedure   SetList(const AValue: TList);
    procedure   SetFieldNameDisplay(const AValue: string);
    function    GetValueAsString: string;
    procedure   SetValueAsString(const AValue: string);
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   DoOnExit(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent);override;
    property    Value : TtiObject read GetValue write SetValue;
    property    ValueAsString : string read GetValueAsString write SetValueAsString;
    property    List : TList read FList write SetList;
    procedure   Refresh; override;
  published
    property    FieldNameDisplay : string read FsFieldNameDisplay write SetFieldNameDisplay;
    property    OnGetObjectProp : TOnGetObjectPropEvent read FOnGetObjectProp write FOnGetObjectProp;
    property    OnSetObjectProp : TOnSetObjectPropEvent read FOnSetObjectProp write FOnSetObjectProp;
  end;


  {$IFDEF FPC}
  TDTDateMode = (dmComboBox, dmUpDown);
  TDateTimeKind = (dtkDate, dtkTime);
  TDateTimePicker = TDateEdit;
  {$ENDIF}


  // A wrapper for the TDateTimePicker control
  TtiPerAwareDateTimePicker = class(TtiPerAwareAbs)
  private
    function  GetValue: TDateTime;
    procedure SetValue(const AValue: TDateTime);
    procedure DoOnExit(Sender: TObject);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$IFNDEF FPC}
    function  GetMaxDate: TDateTime;
    function  GetMinDate: TDateTime;
    procedure SetMaxDate(const AValue: TDateTime);
    procedure SetMinDate(const AValue: TDateTime);
    {$ENDIF}
    function  GetDateMode: TDTDateMode;
    function  GetKind: TDateTimeKind;
    procedure SetDateMode(const AValue: TDTDateMode);
    procedure SetKind(const AValue: TDateTimeKind);
  protected
    procedure   SetControlColor; override;
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  published
    property Value : TDateTime read GetValue write SetValue;
    {$IFNDEF FPC}
    property MaxDate : TDateTime read GetMaxDate write SetMaxDate;
    property MinDate : TDateTime read GetMinDate write SetMinDate;
    {$ENDIF}
    property Kind    : TDateTimeKind read GetKind write SetKind;
    property DateMode : TDTDateMode read GetDateMode write SetDateMode;
  public
    constructor Create(AOwner : TComponent); override;
    procedure   Loaded; override;
  end;

  // This control should be re-built using custom images for the check
  // box so the check box can be painted in grey when disabled.
  TtiPerAwareCheckBox = class(TtiPerAwareAbs)
  private
    function  GetValue: boolean;
    procedure SetValue(const AValue: boolean);
  protected
    procedure   SetControlColor; override;
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    procedure   DoOnClick(Sender: TObject);override;
    procedure   DoLabelClick(Sender: TObject);
  published
    property    Value : boolean read GetValue write SetValue;
  public
    constructor Create(AOwner : TComponent); override;
  end;


  // The TtiPerAwareFloatEdit can be of these types
  TtiFloatEditStyle = (fesUser, fesInteger, fesFloat, fesCurrency, fesPercent);


  // A wrapper for the TEdit control, with some additional methods to implement
  // number editing.
  TtiPerAwareFloatEdit   = class(TtiPerAwareAbs)
  private
    FsEditMask  : string;
    FiPrecision : integer;
    FsTextBefore : string;
    FsTextAfter : string;
    FsTextUnknown: string;
    FrMinValue  : double;
    FrMaxValue  : double;
    FrUnknownValue  : double;
    FsBeforeApplyKey : string;
    FFloatEditStyle: TtiFloatEditStyle;

    procedure _DoClick(sender : TObject);
    procedure _DoEnter(sender : TObject);
    procedure _DoExit(sender : TObject);
    procedure _DoKeyPress(Sender: TObject; var Key: Char);
    procedure _DoChange(sender : TObject);

    function  RemoveFormatChr(sValue : string): string;
    function  IsValidFloat(sValue : string): boolean;
    function  WithinMinMaxLimits(value : double): boolean;
    function  CustomStrToFloat(var AValue: string): double;

    function  GetValueAsString : string;
    procedure SetValueAsString(sValue: string);
    procedure SetValue(rValue : double);
    function  GetValue : double;
    procedure SetPrecision(iValue : integer);
    procedure SetTextAfter(sValue : string);
    procedure SetTextBefore(sValue : string);
    procedure SetTextUnknown(const sValue: string);
    procedure SetMinValue(rValue : double);
    procedure SetMaxValue(rValue : double);
    procedure SetFloatEditStyle(const AValue: TtiFloatEditStyle);
    procedure SetUnknownValue(const rValue: double);
    function GetIsKnown: boolean;
    procedure SetIsKnown(const bValue: boolean);
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  published
    property    TextBefore : string read FsTextBefore write setTextBefore;
    property    TextAfter : string read FsTextAfter  write setTextAfter;
    property    TextUnknown : string read FsTextUnknown  write setTextUnknown;
    property    ValueAsString : string read GetValueAsString write SetValueAsString;
    property    Value    : double    read GetValue   write SetValue ;
    property    Precision : integer read FiPrecision write setPrecision;
    property    MinValue : double    read FrMinValue write setMinValue;
    property    MaxValue : double    read FrMaxValue write setMaxValue;
    property    UnknownValue : double    read FrUnknownValue write SetUnknownValue;
    property    IsKnown : boolean    read GetIsKnown write SetIsKnown;
    property    Style    : TtiFloatEditStyle read FFloatEditStyle write SetFloatEditStyle;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TtiPerAwareImageEditOnLoadFromFile = procedure(Sender : TObject; var AFileName : string; var pDefaultAction : boolean) of object;

  TtiImageEditVisibleButton = (ievbLoadFromFile, ievbSaveToFile, ievbPasteFromClip, ievbCopyToClip,
                                ievbEdit, ievbClear, ievbStretch);
  TtiImageEditVisibleButtons = set of TtiImageEditVisibleButton;

  TtiPerAwareImageEdit = class;

  TtiPerAwareImageEditAction = class(TtiAction)
  private
    FImageControl: TtiPerAwareImageEdit;
    procedure SetImageControl(const AValue: TtiPerAwareImageEdit);
  protected
    function IsDisabledIfNoHandler: Boolean;
    function IsEnabledReadOnly: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;

  published
    {:If ImageIndex is -2, then the appropriate image index is discovered via tiImageMgr at runtime.}
    property ImageIndex;

    property ImageControl: TtiPerAwareImageEdit read FImageControl write SetImageControl;
  end;

  TtiImageLoadAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TtiImageSaveAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TtiImagePasteFromClipboardAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TtiImageCopyToClipboardAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  // Edit needs to be handled by the form, no default editing behaviour.
  TtiImageEditAction = class(TtiPerAwareImageEditAction)
  private
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    //procedure UpdateTarget(Target: TObject); override;
  end;

  // New needs to be handled by form, no default editing behaviour.
  TtiImageNewAction = class(TtiPerAwareImageEditAction)
  private
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TtiImageClearAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TtiImageStretchAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  {: View needs to be handled by the form, no default view behaviour.}
  TtiImageViewAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  {: Export image needs to be handled by the form, no default view behaviour.}
  TtiImageExportAction = class(TtiPerAwareImageEditAction)
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TtiPerAwareImageEdit = class(TtiPerAwareAbs)
  private
    FScrollBox : TScrollBox;
    FScrollBars : TScrollStyle;
    FImage     : TImage;
    FbtnLoadFromFile  : TSpeedButton;
    FbtnSaveToFile    : TSpeedButton;
    FbtnPasteFromClip : TSpeedButton;
    FBtnCopyToClip    : TSpeedButton;
    FbtnEdit          : TSpeedButton;
    FbtnClear         : TSpeedButton;
    FbtnStretch       : TSpeedButton;

    FsFileName : string;
    FsInitialDir : string;
    FOnEdit: TNotifyEvent;
    FOnLoadFromFile: TtiPerAwareImageEditOnLoadFromFile;
    FFilter: string;
    FVisibleButtons: TtiImageEditVisibleButtons;

    procedure   SetScrollBars(const AValue: TScrollStyle);
    function    GetValue: TPicture;
    procedure   SetValue(const AValue: TPicture);
    function    GetStretch: boolean;
    procedure   SetStretch(const AValue: boolean);
    procedure   DoLoadFromFile(  Sender : TObject);
    procedure   DoSaveToFile(    Sender : TObject);
    procedure   DoCopyToClip(    Sender : TObject);
    procedure   DoPasteFromClip( Sender : TObject);
    procedure   DoStretch(       Sender : TObject);
    procedure   DoEdit(          Sender : TObject);
    procedure   DoClear(         Sender : TObject);
    procedure   SetVisibleButtons(const AValue: TtiImageEditVisibleButtons);
    procedure   SetOnEdit(const AValue: TNotifyEvent);

    function CanPasteFromClipboard: Boolean;
    function GetProportional: Boolean;
    procedure SetProportional(const AValue: Boolean);
  protected
    {$IFNDEF FPC}
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
    procedure   SetReadOnly(const AValue: Boolean);override;
    procedure   SetControlColor; override;
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   PositionWinControl; override;
    function    IsPropReadOnly : boolean; override;
  public
    function    IsEmpty: Boolean;
  published
    property    Proportional: Boolean read GetProportional write SetProportional;
    property    ScrollBars : TScrollStyle read FScrollBars write SetScrollBars;
    // ToDo: Make Value property a TStream
    property    Value : TPicture read GetValue write SetValue;
    property    Stretch : boolean read GetStretch write SetStretch;
    property    InitialDir : String read FsInitialDir write FsInitialDir;
    property    OnEdit : TNotifyEvent read FOnEdit write SetOnEdit;
    property    OnLoadFromFile : TtiPerAwareImageEditOnLoadFromFile read FOnLoadFromFile write FOnLoadFromFile;
    property    Filter : string read FFilter write FFilter;
    property    VisibleButtons : TtiImageEditVisibleButtons read FVisibleButtons write SetVisibleButtons;
  public
    constructor Create(AOwner : TComponent); override;
  end;

{
  TtiPerAwareImageEditForm = class(TForm)
  private
    FMenu : TMainMenu;
    FOleContainer : TOleContainer;
    FFileName : TFileName;
    procedure DoOnShow(Sender : TObject);
    procedure DoOnClose(Sender : TObject; var Action : TCloseAction);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    function    Execute(const
    AFileName : TFileName): boolean;
  end;
}

implementation
uses
  Dialogs
  ,ExtDlgs
  ,TypInfo
  ,ClipBrd
  // If the unit below, GifImage is uncommented, then comment it out (I have
  // been testing and forgot to clean up my mess. GifImage is a download from
  // http://www.melander.dk/delphi/gifimage/ and extent's Delphi's TImage
  // component with GIF support.
  //,GifImage
{$IFNDEF FPC}
  ,ShellAPI
{$ELSE}
  ,lclproc
{$ENDIF}
  ,tiImageMgr
  // ,tiUtils // For debugging
  // ,tiLog
  ,tiExcept
  ,tiUtils
  ,tiConstants
 ;

// Added by Chris Latta (andromeda@froggy.com.au) because the values of some
// constants are changed inside the Initialization section.
{$IFNDEF FPC}
{$J+}
{$ENDIF}

const
  cValidFloatChrs : set of char = [ '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ];
  cIntEditMask        =    '#0' ;  //ipk 2001-03-01
  cFloatEditMask     : string = '' ;
//  cusImageFilters     = 'Bitmap files|*.bmp|GIF files|*.gif|JPeg files|*.jpg|All files|*.*';
  cusImageDefaultExt  = '.bmp';

var
  uStyles : array[boolean] of TComboBoxStyle = (csDropDown, csSimple);

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareAbs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnChange     := nil;

  Constraints.MinHeight := cDefaultHeightSingleRow;
  OnClick       := DoOnClick;

  FLabelStyle   := lsLeft;


  FLabel        := TLabel.Create(Self);
  FLabel.Parent := self;
  FLabel.ParentFont := true;
  // ToDo: Default Label.Caption to the component's name
  FLabel.Caption := 'Enter label &name';


  FLabel.AutoSize := false;
  FLabel.Width   := cuiDefaultLabelWidth;
  FLabel.OnClick := DoOnClick;

  Assert(FWinControl <> nil, 'FWinControl not assigned');
  FWinControl.Parent := self;
  FLabel.FocusControl := FWinControl;

  {$IFDEF FPC}
  Include(FLabel.ComponentStyle, csSubComponent);
  Include(FLabel.ControlStyle, csNoDesignSelectable);
  Include(FWinControl.ComponentStyle, csSubComponent);
  Include(FWinControl.ControlStyle, csNoDesignSelectable);
  {$ENDIF}
  FLabel.FreeNotification(Self);
  FWinControl.FreeNotification(Self);


  FiLabelWidth := cuiDefaultLabelWidth;


  FbError    := False;
  FErrorColor := clYellow;
  FGreyWhenReadOnly := true;
end;

destructor TtiPerAwareAbs.Destroy;
begin
  inherited;
end;

procedure TtiPerAwareAbs.Refresh;
begin
  DataToWinControl;
end;

procedure TtiPerAwareAbs.SetFocus;
begin
  inherited;
  if (Enabled) and (FWinControl.Enabled) then
    FWinControl.SetFocus;
end;

function TtiPerAwareAbs.GetCaption: TCaption;
begin
  result := FLabel.Caption;
end;

function TtiPerAwareAbs.GetLabelFont: TFont;
begin
  result := fLabel.Font;
end;

function TtiPerAwareAbs.GetLabelParentFont: Boolean;
begin
  result := fLabel.ParentFont;
end;

procedure TtiPerAwareAbs.Loaded;
begin
  inherited;
  {$IFNDEF FPC}
  PositionLabel;
  PositionWinControl;
  {$ENDIF}
end;

procedure TtiPerAwareAbs.PositionLabel;
begin

  // A little redundant, but here goes anyway...
  case LabelStyle of
  lsNone    : begin
                FLabel.Visible := false ;
              end ;
  lsTop     : begin
                FLabel.AutoSize := true;
                FLabel.Visible := true ;
                FLabel.Top     := 1 ;
                FLabel.Left    := 1 ;
              end ;
  lsLeft    : begin
                FLabel.AutoSize := false ;
                FLabel.Width   := FiLabelWidth ;
                FLabel.Visible := true ;
                FLabel.Left    := 1 ;
                if FbCenterWhenLabelIsLeft then
                  FLabel.Top     := (Height-2-FLabel.Height) div 2
                else
                  FLabel.Top     := 1 ;
              end ;
  lsTopLeft : begin
                FLabel.AutoSize := true ;
                FLabel.Visible := true ;
                FLabel.Top     := 1 ;
                FLabel.Left    := 1 ;
              end ;
  lsRight    : begin
                FLabel.AutoSize := true ;
                FLabel.Visible := true ;
                FLabel.Left   := LabelWidth + 3 ;
                if FbCenterWhenLabelIsLeft then
                  FLabel.Top     := (Height-2-FLabel.Height) div 2
                else
                  FLabel.Top     := 1 ;
              end ;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
  end ;
end;

procedure TtiPerAwareAbs.PositionWinControl ;
begin

  case LabelStyle of
  lsNone    : begin
                //FWinControl.Align := alClient;
                FWinControl.SetBounds(1,1, Self.Width-2, Self.Height-2);
              end ;
  lsTop     : begin
                FWinControl.Top    := FLabel.Top + FLabel.Height + 2 ;
                FWinControl.Left   := 1 ;
                FWinControl.Height := Height - FWinControl.Top - 4 ;
                FWinControl.Width  := Width  - FWinControl.Left - 4  ;
              end ;
  lsLeft    : begin
                FWinControl.Top    := 1 ;
                FWinControl.Left   := LabelWidth + 3;
                FWinControl.Height := Height - FWinControl.Top - 4;
                FWinControl.Width  := Width  - FWinControl.Left - 4 ;
              end ;
  lsTopLeft : begin
                FWinControl.Top    := FLabel.Top + FLabel.Height + 3;
                FWinControl.Left   := 24 ;
                FWinControl.Height := Height - FWinControl.Top - 4 ;
                FWinControl.Width  := Width  - FWinControl.Left - 4 ;
              end ;
  lsRight   : begin
                FWinControl.Top    := 1 ;
                FWinControl.Left   := 1 ;
                FWinControl.Height := Height - FWinControl.Top - 4;
                FWinControl.Width  := LabelWidth ;
              end ;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
  end ;

end;

procedure TtiPerAwareAbs.SetCaption(const AValue: TCaption);
begin
  FLabel.Caption := AValue;
end;

procedure TtiPerAwareAbs.SetLabelFont(const AValue: TFont);
begin
  FLabel.Font.Assign(AValue);
end;

procedure TtiPerAwareAbs.SetLabelParentFont(const AValue: Boolean);
begin
  FLabel.ParentFont := AValue;
end;

procedure TtiPerAwareAbs.SetLayout(const AValue: TTextLayout);
begin
if  FLayout <> AValue then
 begin
  FLabel.Layout := AValue;
  FLayout := AValue;
  end;
end;

procedure TtiPerAwareAbs.SetLabelStyle(const AValue: TLabelStyle);
var
  lDefaultHeight: Boolean;
begin
  case LabelStyle of
    lsNone   : lDefaultHeight := Height = cDefaultHeightSingleRow;
    lsTop    : lDefaultHeight := Height = cDefaultHeightMultiRow;
    lsLeft   : lDefaultHeight := Height = cDefaultHeightSingleRow;
    lsTopLeft : lDefaultHeight := Height = cDefaultHeightMultiRow;
    lsRight  : lDefaultHeight := Height = cDefaultHeightSingleRow;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
  end;
  FLabelStyle := AValue;
  if lDefaultHeight then
    case LabelStyle of
      lsNone   : Height := cDefaultHeightSingleRow;
      lsTop    : Height := cDefaultHeightMultiRow;
      lsLeft   : Height := cDefaultHeightSingleRow;
      lsTopLeft : Height := cDefaultHeightMultiRow;
      lsRight  : Height := cDefaultHeightSingleRow;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
    end;
  PositionLabel;
  PositionWinControl;
end;

procedure TtiPerAwareAbs.SetLabelWidth(const AValue: Integer);
begin
  if FLabel.Width = AValue then Exit;
  FiLabelWidth := AValue;
  FLabel.Width := AValue;
  PositionLabel;
  PositionWinControl;
end;


procedure TtiPerAwareAbs.SetEnabled(AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    inherited SetEnabled(AValue);
    FWinControl.Enabled := AValue;
    SetControlColor;
    FWinControl.Refresh;
  end;
end;

procedure TtiPerAwareAbs.SetHint(const AValue: TTranslateString);
begin
  {$IFDEF FPC}
  inherited SetHint(AValue);
  {$ENDIF}
  FLabel.Hint := AValue;
  FWinControl.Hint := AValue;
  FHint := AValue;
end;


{$IFNDEF FPC}
procedure TtiPerAwareAbs.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not Assigned(FLabel) then Exit;
  FLabel.Font.Assign(Self.Font);
  {Later use Realign when FWinControl will be also aligned};
  PositionLabel;
  PositionWinControl;
end;

procedure TtiPerAwareAbs.WMSize(var Message: TWMSize);
begin
  PositionLabel;
  PositionWinControl;
end;
{$ELSE}
procedure TtiPerAwareAbs.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if not Assigned(FLabel) then Exit;
  if not Assigned(FWinControl) then Exit;
  FLabel.Font.Assign(Self.Font);
  FWinControl.Font.Assign(Self.Font);
  PositionLabel;
  PositionWinControl;
end;

procedure TtiPerAwareAbs.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  PositionLabel;
  PositionWinControl;
end;
{$ENDIF}

procedure TtiPerAwareAbs.SetData(const AValue: TtiObject);
begin
  FbDirty := false;
  FData := AValue;
  DataToWinControl;
end;

procedure TtiPerAwareAbs.SetFieldName(const AValue: string);
begin
  FsFieldName := AValue;
  DataToWinControl;
end;

procedure TtiPerAwareAbs.SetWordWrap(const AValue: Boolean);
begin
  FLabel.WordWrap := AValue;
end;

function TtiPerAwareAbs.GetWordWrap: Boolean;
begin
 Result := FLabel.WordWrap;
end;



procedure TtiPerAwareAbs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FLabel) and (Operation = opRemove) then
    FLabel := nil;
  if (AComponent = FWinControl) and (Operation = opRemove) then
    FWinControl := nil;
end;


procedure TtiPerAwareAbs.DoChange(Sender: TObject);
begin
  FbDirty := true;
  WinControlToData;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

function TtiPerAwareAbs.DataAndPropertyValid: boolean;
begin

  result :=
    (FData <> nil) and
    (FsFieldName <> '');
  if not result then
    Exit; //==>

  result := (IsPublishedProp(FData, FsFieldName));

  if not result then
    raise exception.CreateFmt('<%s> is not a property of <%s>',
                               [FsFieldName, FData.ClassName ]);

  ReadOnly := ReadOnly or IsPropReadOnly;

end;

procedure TtiPerAwareAbs.LinkToData(AData: TtiObject;const AFieldName: string);
begin
  Data := AData;
  FieldName := AFieldName;
end;

procedure TtiPerAwareAbs.DoOnClick(Sender : TObject);
begin
  if (Enabled) and (FWinControl.Enabled) then
    FWinControl.SetFocus
  else
    SetFocus;
end;

procedure TtiPerAwareAbs.DoOnKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Sender, Key);
end;

procedure TtiPerAwareAbs.DoOnKeyDown(Sender : TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TtiPerAwareAbs.SetReadOnly(const AValue: Boolean);
begin
  FbReadOnly := AValue;
{ Handling ShowFocusRect here is a bit dodgy...
  a) Not changing it will make the control look weird if ReadOnly
  b) Blindly making it the opposite to ReadOnly will override any
     custom logic the programmer intended.
  c) Setting it if ReadOnly will behave better, but not turn it
     *back* on (if that was its' previous state) if ReadOnly is
     subsequently turned off.
  I've adopted c) as a compromise.  What I'd prefer is something
  like "ParentShowFocusRect", with the behaviour like ParentFont.
  --- IK 01/11/2003 }
  if FbReadOnly then               // <<<=== IK 01/11/2003
    ShowFocusRect := False;       // <<<=== PH 26/10/2003
  FWinControl.TabStop := not FbReadOnly;  // <<<=== IK 05/12/2001
  SetControlColor;
end;

procedure TtiPerAwareAbs.SetControlColor;
begin
  // the control is in error
  if Error then
  begin
    FLabel.Font.Color := clBlack;
    FWinControl.Brush.Color := FErrorColor;
    FWinControl.Refresh;
    Exit; //==>
  end;

  // control is read only
  if ReadOnly and GreyWhenReadOnly then
  begin
    FLabel.Font.Color := clBlack;
    FWinControl.Brush.Color := clBtnFace;
    FWinControl.Refresh;
    Exit; //==>
  end;

  // the control is not enabled.
  if not Enabled then
  begin
    FLabel.Font.Color := clGray;
    FWinControl.Brush.Color := clBtnFace;
    FWinControl.Refresh;
    Exit; //==>
  end;

  FLabel.Font.Color := clBlack;
  FWinControl.Brush.Color := clWindow;
  FWinControl.Refresh;

end;

procedure TtiPerAwareAbs.SetError(const AValue: boolean);
begin
  FbError := AValue;
  SetControlColor;
end;

procedure TtiPerAwareAbs.SetErrorColor(const AValue: TColor);
begin
  FErrorColor := AValue;
  SetControlColor;
end;

function TtiPerAwareAbs.IsPropReadOnly: boolean;
var
  lPropInfo : PPropInfo;
begin
  lPropInfo := GetPropInfo(FData, FsFieldName);
  {$IFNDEF FPC}
  result   := (lPropInfo.SetProc = nil);
  {$ELSE}
  result   := (lPropInfo^.SetProc = nil);
  {$ENDIF}
end;

function TtiPerAwareAbs.Focused: Boolean;
begin
  result := (inherited Focused) or
            (FWinControl.Focused);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareEdit.Create(AOwner: TComponent);
begin
  FWinControl := TEdit.Create(self);
  TEdit(FWinControl).OnChange  := DoChange;
  TEdit(FWinControl).OnKeyPress := DoOnKeyPress;
  TEdit(FWinControl).OnKeyDown := DoOnKeyDown;
  FbCenterWhenLabelIsLeft := true;
  inherited;
  TEdit(FWinControl).ParentFont := true;
  Height := cDefaultHeightSingleRow;
end;

function TtiPerAwareEdit.GetValue: String;
begin
  result := TEdit(FWinControl).Text;
end;

procedure TtiPerAwareEdit.SetValue(const AValue: String);
begin
  SetOnChangeActive(false);
  TEdit(FWinControl).Text := AValue;
  WinControlToData;
  SetOnChangeActive(true);
end;

procedure TtiPerAwareEdit.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
{ NB 3rd param of GetPropValue below is boolean for 'PreferStrings'.
     Up to and including D2005, this has a default value of True.
     In D2006 we must explicitly state it.  (Thank you Borland) }
  {$IFNDEF FPC}
   TEdit(FWinControl).Text := GetPropValue(FData, FsFieldName);
  {$ELSE}
  {$Note Fixme.I'm not sure if this is correct}
   TEdit(FWinControl).Text := VarToStr(GetPropValue(FData, FsFieldName));
  {$ENDIF}
  SetOnChangeActive(true);
end;

procedure TtiPerAwareEdit.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(FData, FsFieldName, TEdit(FWinControl).Text);
end;

procedure TtiPerAwareEdit.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TEdit(FWinControl).OnChange := DoChange
  else
    TEdit(FWinControl).OnChange := nil;
end;

procedure TtiPerAwareEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  TEdit(FWinControl).ReadOnly := AValue;
end;

function TtiPerAwareEdit.GetMaxLength: integer;
begin
  result := TEdit(FWinControl).MaxLength;
end;

procedure TtiPerAwareEdit.SetMaxLength(const AValue: integer);
begin
  TEdit(FWinControl).MaxLength := AValue;
end;

function TtiPerAwareEdit.GetCharCase: TEditCharCase;
begin
  result := TEdit(FWinControl).CharCase;
end;

procedure TtiPerAwareEdit.SetCharCase(const AValue: TEditCharCase);
begin
  TEdit(FWinControl).CharCase := AValue;
end;

function TtiPerAwareEdit.GetPasswordChar: Char;
begin
  result := TEdit(FWinControl).PasswordChar;
end;

procedure TtiPerAwareEdit.SetPasswordChar(const AValue: Char);
begin
  TEdit(FWinControl).PasswordChar := AValue;
end;

{$IFNDEF FPC}
procedure TtiPerAwareEdit.CMFontChanged(var Message: TMessage);
begin
 TEdit(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareMemo
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareMemo.Create(AOwner: TComponent);
begin
  FWinControl := TMemo.Create(self);
  TMemo(FWinControl).OnChange := DoChange;
  TMemo(FWinControl).OnKeyPress := DoOnKeyPress;
  TMemo(FWinControl).OnKeyDown := DoOnKeyDown;
  FbCenterWhenLabelIsLeft := false;
  inherited;
  TMemo(FWinControl).ParentFont := true;
end;


procedure TtiPerAwareMemo.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  TMemo(FWinControl).Lines.Text := GetPropValue(FData, FsFieldName, True);
  SetOnChangeActive(true);
end;


function TtiPerAwareMemo.GetMaxLength: integer;
begin
  result := TMemo(FWinControl).MaxLength;
end;


function TtiPerAwareMemo.GetScrollBars: TScrollStyle;
begin
  result := TMemo(FWinControl).ScrollBars;
end;


function TtiPerAwareMemo.GetValue: string;
begin
  result := TMemo(FWinControl).Lines.Text;
end;


function TtiPerAwareMemo.GetWordWrap: boolean;
begin
  result := TMemo(FWinControl).WordWrap;
end;


procedure TtiPerAwareMemo.SetMaxLength(const AValue: integer);
begin
  TMemo(FWinControl).MaxLength := AValue;
end;


procedure TtiPerAwareMemo.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TMemo(FWinControl).OnChange := DoChange
  else
    TMemo(FWinControl).OnChange := nil;
end;


procedure TtiPerAwareMemo.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  TMemo(FWinControl).ReadOnly := AValue;
end;


procedure TtiPerAwareMemo.SetScrollBars(const AValue: TScrollStyle);
begin
  TMemo(FWinControl).ScrollBars := AValue;
end;


procedure TtiPerAwareMemo.SetValue(const AValue: string);
begin
  SetOnChangeActive(false);
  TMemo(FWinControl).Lines.Text := AValue;
  WinControlToData;
  SetOnChangeActive(true);
end;


procedure TtiPerAwareMemo.SetWordWrap(const AValue: boolean);
begin
  TMemo(FWinControl).WordWrap := AValue;
end;


procedure TtiPerAwareMemo.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(FData, FsFieldName, TMemo(FWinControl).Lines.Text);
end;


{$IFNDEF FPC}
procedure TtiPerAwareMemo.CMFontChanged(var Message: TMessage);
begin
 TMemo(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}


{ TtiPerAwareDateTimePicker }

constructor TtiPerAwareDateTimePicker.Create(AOwner: TComponent);
begin
  FWinControl := TDateTimePicker.Create(self);
  {$IFNDEF FPC}TDateTimePicker(FWinControl).Time := 0;{$ENDIF}
  FbCenterWhenLabelIsLeft := true;
  inherited;
  TDateTimePicker(FWinControl).ParentFont := true;
  Height := cDefaultHeightSingleRow;
end;


procedure TtiPerAwareDateTimePicker.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  {$IFNDEF FPC}
  TDateTimePicker(FWinControl).DateTime :=
    Trunc(GetPropValue(FData, FsFieldName, True));
  {$ELSE}
  TDateTimePicker(FWinControl).Date :=
    Trunc(GetPropValue(FData, FsFieldName, True));
  {$ENDIF}
  SetOnChangeActive(true);
end;


procedure TtiPerAwareDateTimePicker.DoKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN] then
    DoChange(Sender);
end;


procedure TtiPerAwareDateTimePicker.DoOnExit(Sender: TObject);
begin
  DoChange(Sender);
end;


function TtiPerAwareDateTimePicker.GetValue: TDateTime;
begin
  if Kind = dtkDate then
    result := Trunc(TDateTimePicker(FWinControl).Date)
  else
    result := TDateTimePicker(FWinControl).Date;
end;


procedure TtiPerAwareDateTimePicker.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
  begin
    TDateTimePicker(FWinControl).{$IFNDEF FPC}OnCloseUp{$ELSE}OnChange{$ENDIF} := DoChange;
    TDateTimePicker(FWinControl).OnExit    := DoOnExit;
    TDateTimePicker(FWinControl).OnKeyUp   := DoKeyUp   ;
  end
  else
  begin
    TDateTimePicker(FWinControl).{$IFNDEF FPC}OnCloseUp{$ELSE}OnChange{$ENDIF}:= nil;
    TDateTimePicker(FWinControl).OnExit   := nil;
    TDateTimePicker(FWinControl).OnKeyUp  := nil;
  end;
end;


procedure TtiPerAwareDateTimePicker.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  if AValue then
    TDateTimePicker(FWinControl).Enabled := false
  else
    TDateTimePicker(FWinControl).Enabled := Enabled;
  SetControlColor;
end;


procedure TtiPerAwareDateTimePicker.SetValue(const AValue: TDateTime);
var
  lDate: TDateTime;
begin
  SetOnChangeActive(false);
  lDate := trunc(AValue);
{$IFNDEF FPC}
  if (TDateTimePicker(FWinControl).MaxDate <> 0) and
     (lDate > TDateTimePicker(FWinControl).MaxDate) then
    TDateTimePicker(FWinControl).MaxDate := lDate
  else if (lDate < TDateTimePicker(FWinControl).MinDate) and
           (TDateTimePicker(FWinControl).MinDate <> 0) then
    TDateTimePicker(FWinControl).MinDate := lDate;
{$ENDIF}
  TDateTimePicker(FWinControl).Date := lDate;
  WinControlToData;
  SetOnChangeActive(true);
end;


procedure TtiPerAwareDateTimePicker.WinControlToData;
var
  liValue : integer;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
 {$IFNDEF FPC}
  liValue := Trunc(TDateTimePicker(FWinControl).DateTime);
 {$ELSE}
  liValue := Trunc(TDateTimePicker(FWinControl).Date);
 {$ENDIF}

  SetPropValue(FData, FsFieldName, liValue);
end;


{$IFNDEF FPC}
function TtiPerAwareDateTimePicker.GetMaxDate: TDateTime;
begin
  result := TDateTimePicker(FWinControl).MaxDate;
end;


function TtiPerAwareDateTimePicker.GetMinDate: TDateTime;
begin
  result := TDateTimePicker(FWinControl).MinDate;
end;


procedure TtiPerAwareDateTimePicker.SetMaxDate(const AValue: TDateTime);
begin
  TDateTimePicker(FWinControl).MaxDate := (Trunc(AValue) + 1) - cdtOneMiliSecond ;
end;


procedure TtiPerAwareDateTimePicker.SetMinDate(const AValue: TDateTime);
begin
  TDateTimePicker(FWinControl).MinDate := Trunc(AValue);
end;
{$ENDIF}


procedure TtiPerAwareDateTimePicker.SetControlColor;
begin
  inherited;
  if Enabled and (not ReadOnly) and (not GreyWhenReadOnly)then
    TDateTimePicker(FWinControl).Color := clWindow
  else
    TDateTimePicker(FWinControl).Color := clBtnFace;
end;


{$IFNDEF FPC}
procedure TtiPerAwareDateTimePicker.CMFontChanged(var Message: TMessage);
begin
 TDateTimePicker(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}


{
procedure TtiPerAwareDateTimePicker.DoOnClick(Sender: TObject);
begin
  // Only call inherited if not ReadOnly because the comboBox will have been
  // disabled if it is ReadOnly.
  if not ReadOnly then
    Inherited DoOnClick(Sender);
end;
}


{ TtiPerAwareCheckBox }

constructor TtiPerAwareCheckBox.Create(AOwner: TComponent);
begin
  FWinControl := TCheckBox.Create(self);
  TCheckBox(FWinControl).Caption := '';
  TCheckBox(FWinControl).OnClick := DoChange;
  TCheckBox(FWinControl).Width := 13;
  FbCenterWhenLabelIsLeft := true;
  inherited;
  FLabel.OnClick := DoLabelClick;
  Height := 17;
end;


procedure TtiPerAwareCheckBox.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  TCheckBox(FWinControl).Checked := GetPropValue(FData, FsFieldName, True);
  SetOnChangeActive(true);
end;


function TtiPerAwareCheckBox.GetValue: boolean;
begin
  result := TCheckBox(FWinControl).Checked;
end;


procedure TtiPerAwareCheckBox.SetControlColor;
var
  lBrushColor : TColor;
begin
  lBrushColor := FWinControl.Brush.Color;
  inherited SetControlColor;
  FWinControl.Brush.Color := lBrushColor;
  FWinControl.Refresh;
end;


procedure TtiPerAwareCheckBox.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TCheckBox(FWinControl).OnClick := DoChange
  else
    TCheckBox(FWinControl).OnClick := nil;
end;


procedure TtiPerAwareCheckBox.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  if AValue then
    TCheckBox(FWinControl).Enabled := False
  else
    TCheckBox(FWinControl).Enabled := Enabled;
end;


procedure TtiPerAwareCheckBox.DoOnClick(Sender: TObject);
begin
  Inherited DoOnClick(Sender);
  DoLabelClick(nil);
end;


procedure TtiPerAwareCheckBox.SetValue(const AValue: boolean);
begin
  SetOnChangeActive(false);
  TCheckBox(FWinControl).Checked := AValue;
  WinControlToData;
  SetOnChangeActive(true);
end;


procedure TtiPerAwareCheckBox.WinControlToData;
var
  li : Integer;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  li := Ord(TCheckBox(FWinControl).Checked);
  SetPropValue(FData, FsFieldName, li);
end;


{ TtiPerAwareFloatEdit }

constructor TtiPerAwareFloatEdit.Create(AOwner : TComponent);
begin
  FWinControl := TEdit.Create(self);
  FbCenterWhenLabelIsLeft := true;
  inherited;
  Height := cDefaultHeightSingleRow;


  TEdit(FWinControl).OnEnter     := _DoEnter;
  TEdit(FWinControl).OnExit      := _DoExit;
  TEdit(FWinControl).OnKeyPress  := _DoKeyPress;
  TEdit(FWinControl).OnChange    := _DoChange;
  TEdit(FWinControl).OnClick     := _DoClick;

  FsEditMask  := cFloatEditMask;
  FsTextBefore := '';
  FsTextAfter := '';
  FrMinValue  := 0;
  FrMaxValue  := 0;
  FrUnknownValue  := -1;
  FsBeforeApplyKey := '';
  Precision := 0;
  Value := 0;
  TEdit(FWinControl).ParentFont := true;
end;


procedure TtiPerAwareFloatEdit.setValueAsString(sValue : string);
begin
  if sValue = '' then begin
    Value := 0;
    exit;
  end;

  if not isValidFloat(sValue) then begin
    Value := 0;
    exit;
  end;

  SetOnChangeActive(false);
  TEdit(FWinControl).text := sValue;
  Value := Value;
  SetOnChangeActive(true);

end;


function  TtiPerAwareFloatEdit.getValueAsString : string;
begin
  result := TEdit(FWinControl).text;
end;


function TtiPerAwareFloatEdit.isValidFloat(sValue: string): boolean;
var
  rValue: double;
begin
  try
    rValue := strToFloat(RemoveFormatChr(sValue));
    {$IFNDEF FPC}
     if rValue < rValue + 1 then; // To trick compiler warnings
    {$ENDIF}
    result := true;
  except
    result := false;
  end;
end;


function TtiPerAwareFloatEdit.getValue : double;
var
  lStr : string;
begin
  lStr := TEdit(FWinControl).text;
  if (FsTextUnknown <> '')
  and SameText(lStr, FsTextUnknown) then
    result := FrUnknownValue
  else
    result := CustomStrToFloat(lStr);
end;


function TtiPerAwareFloatEdit.customStrToFloat(var AValue : string): double;
var lStrValue : string;
begin
  lStrValue := RemoveFormatChr(AValue);
  if lStrValue = '' then begin
    result := 0;
    exit; //==>
  end;

  try
    result := strToFloat(lStrValue);
  except
    result := 0;
  end;
end;


procedure TtiPerAwareFloatEdit.setValue(rValue: double);
var
  sValue : string;
  sMinValue : string;
  sMaxValue : string;
begin
  SetOnChangeActive(false);

  if  (FrUnknownValue <> 0)
  and (rValue = FrUnknownValue) then
    sValue := FsTextUnknown
  else
    sValue := FsTextBefore + formatFloat(FsEditMask, rValue) + FsTextAfter;

  if not WithinMinMaxLimits(rValue) then begin
    sMinValue := FsTextBefore + formatFloat(FsEditMask, FrMinValue) + FsTextAfter;
    sMaxValue := FsTextBefore + formatFloat(FsEditMask, FrMaxValue) + FsTextAfter;
    raise ERangeError.Create('The value you entered, ' + sValue +
                                 ' is out of range.' + #13 +
                                 'Please enter a value between ' +
                                 sMinValue + ' and ' +
                                 sMaxValue);
  end;
  TEdit(FWinControl).text := sValue;
  WinControlToData;
  SetOnChangeActive(true);
end;


function TtiPerAwareFloatEdit.withinMinMaxLimits(value : double): boolean;
begin
  result := not (((FrMinValue <> 0) and (value < FrMinValue)) or
                 ((FrMaxValue <> 0) and (value > FrMaxValue)));
  // What if one of our FsM??Values are 0 ?
  // Require some code to handle these situations
end;


procedure TtiPerAwareFloatEdit.setMinValue(rValue : double);
begin
  if (FrMaxValue <> 0) and (rValue >= FrMaxValue) then rValue := 0;
  FrMinValue := rValue;
  Value := Value;
end;


procedure TtiPerAwareFloatEdit.setMaxValue(rValue : double);
begin
  if (FrMinValue <> 0) and (rValue <= FrMinValue) then rValue := 0;
  FrMaxValue := rValue;
  Value := Value;
end;


procedure TtiPerAwareFloatEdit.SetUnknownValue(const rValue: double);
begin
  FrUnknownValue := rValue;
  Value := rValue;
end;


procedure TtiPerAwareFloatEdit._DoEnter(sender : TObject);
var
  lSaveOnChange : TNotifyEvent;
begin
  if ReadOnly then
  begin
    TEdit(FWinControl).selectAll;
    Exit; //==>
  end;

  lSaveOnChange := FOnChange;
  FOnChange := nil;

  if (FsTextUnknown <> '')
  and SameText(TEdit(FWinControl).text, FsTextUnknown) then
    TEdit(FWinControl).text := FloatToStr(FrUnknownValue)
  else
    TEdit(FWinControl).text := RemoveFormatChr(TEdit(FWinControl).text);

  TEdit(FWinControl).selectAll;
  FOnChange := lSaveOnChange;
end;


procedure TtiPerAwareFloatEdit._DoExit(sender : TObject);
var
  rValue: double;
  lSaveOnChange: TNotifyEvent;
begin
  if ReadOnly then
    Exit; //==>

  lSaveOnChange := FOnChange;
  FOnChange := nil;
  try
    Value := Value;
  except
    on e : ERangeError do begin
      messageDlg(e.message, mtError,
                  [mbOK], 0);
      TEdit(FWinControl).SetFocus;
    end else begin
      TEdit(FWinControl).setFocus;
      raise;
    end;
  end;
  rValue := Value;
  if rValue <> Value then Value := rValue;
  FOnChange := lSaveOnChange;

end;


function TtiPerAwareFloatEdit.removeFormatChr(sValue : string): string;
var i : integer;
begin
  result := '';
  for i := 1 to length(sValue) do begin
    if sValue[i] in cValidFloatChrs then begin
      result := result + sValue[i];
    end;
  end;
end;


procedure TtiPerAwareFloatEdit.setPrecision(iValue : integer);
var
  i : integer;
  lFrac : string;
begin
  if FiPrecision <> iValue then              //ipk 2001-03-01
  begin
    FiPrecision := iValue;
    FFloatEditStyle := fesUser;
  end;                                       //ipk 2001-03-01

  if FFloatEditStyle = fesInteger then       //ipk 2001-03-01
    FsEditMask := cIntEditMask               //ipk 2001-03-01
  else                                       //ipk 2001-03-01
    FsEditMask := cFloatEditMask;

  if FiPrecision > 0 then
  begin
    if AnsiPos(FsEditMask, DecimalSeparator) <> 0 then
      FsEditMask := Copy(FsEditMask, 1, Pos(FsEditMask, '.') - 1);
    lFrac := '';
    for i := 1 to FiPrecision do
      lFrac := lFrac + '0';
    FsEditMask := FsEditMask + DecimalSeparator + lFrac;
  end;
  Value := Value;

end;


procedure TtiPerAwareFloatEdit._DoKeyPress(Sender: TObject;var Key: Char);
begin
  FsBeforeApplyKey := TEdit(FWinControl).text;

  // A non character key?
  if (ord(key) < 32) or (ord(key) > 132) then begin
    exit;
  end;

  // A numeric key?
  if not (key in cValidFloatChrs) then
  begin
    key := char(0);
  end;
end;


procedure TtiPerAwareFloatEdit._DoChange(sender : TObject);
var
  lReal: Extended;
  lIntPos: integer;
begin
  lReal := Value;
  if not WithinMinMaxLimits(lReal) then begin
    lIntPos := TEdit(FWinControl).selStart;
    TEdit(FWinControl).text := FsBeforeApplyKey;
    TEdit(FWinControl).selStart := lIntPos;
    Exit; //==>
  end;
  WinControlToData;
  if Assigned(FOnChange) then
    FOnChange(self);
end;


procedure TtiPerAwareFloatEdit.setTextAfter(sValue : string);
begin
  if FsTextAfter <> sValue then         //ipk 2001-03-01
  begin
    FsTextAfter := sValue;
    FFloatEditStyle := fesUser;
  end;                                  //ipk 2001-03-01
  Value := Value;

//  FsTextAfter := sValue;
//  FFloatEditStyle := fesUser;
//  AValue := AValue;
end;


procedure TtiPerAwareFloatEdit.setTextBefore(sValue : string);
begin
  if FsTextBefore <> sValue then        //ipk 2001-03-01
  begin
    FsTextBefore := sValue;
    FFloatEditStyle := fesUser;
  end;                                  //ipk 2001-03-01
  Value := Value;
//  FsTextBefore := sValue;
//  FFloatEditStyle := fesUser;
//  AValue := AValue;
end;


procedure TtiPerAwareFloatEdit.setTextUnknown(const sValue: string);
begin
  FsTextUnknown := sValue;
  Value := Value;
end;


procedure TtiPerAwareFloatEdit._DoClick(sender : TObject);
begin
  TEdit(FWinControl).SelectAll;
end;


procedure TtiPerAwareFloatEdit.SetFloatEditStyle(const AValue: TtiFloatEditStyle);
begin
  FFloatEditStyle := AValue;
  case AValue of
  fesUser    :;// Do nothing
  fesInteger : begin
                  TextBefore := '';
                  Precision :=  0;
                  TextAfter := '';
                end;
  fesFloat   : begin
                  TextBefore := '';
                  Precision :=  3;
                  TextAfter := '';
                end;
  fesCurrency : begin
                  TextBefore := '$ ';
                  Precision :=  2;
                  TextAfter := '';
                end;
  fesPercent : begin
                  TextBefore := '';
                  Precision :=  1;
                  TextAfter := ' %';
                end;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
  end;
  FFloatEditStyle := AValue; //ipk 2001-03-01  Likely to be changed to fesUser
end;


procedure TtiPerAwareFloatEdit.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  Value := GetPropValue(FData, FsFieldName, True);
  SetOnChangeActive(true);
end;


procedure TtiPerAwareFloatEdit.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TEdit(FWinControl).OnChange := _DoChange
  else
    TEdit(FWinControl).OnChange := nil;
end;


procedure TtiPerAwareFloatEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  TEdit(FWinControl).ReadOnly := AValue;
end;


procedure TtiPerAwareFloatEdit.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(FData, FsFieldName, Value);
end;


function TtiPerAwareFloatEdit.GetIsKnown: boolean;
begin
  result := Value <> UnknownValue;
end;

procedure TtiPerAwareFloatEdit.SetIsKnown(const bValue: boolean);
begin
  if bValue then
    Value := 0  // assumes 0 is not = FrUnknownValue
  else
    Value := FrUnknownValue;
end;


{$IFNDEF FPC}
procedure TtiPerAwareFloatEdit.CMFontChanged(var Message: TMessage);
begin
 TEdit(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareImageEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiPerAwareImageEdit.Create(AOwner: TComponent);
var
  i : integer;
  lLastControl : TControl;
begin
  FWinControl := TPanel.Create(Self);
  FbCenterWhenLabelIsLeft := false;
  inherited;
  TPanel(FWinControl).BevelInner := bvNone  ; //bvRaised;
  TPanel(FWinControl).BevelOuter := bvNone  ;
  TPanel(FWinControl).BorderStyle := bsNone  ;
  TPanel(FWinControl).ParentFont := true    ;

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.Parent := FWinControl;
  FScrollBox.Top   := 16;
  FScrollBox.Left  := 0;
  FScrollBox.Color := clWindow;
  FScrollBox.Align := alNone;
{$IFNDEF FPC}
  FScrollBox.VertScrollBar.Tracking := True;
  FScrollBox.HorzScrollBar.Tracking := True;
{$ENDIF}
  FScrollBars      := ssBoth;

  FImage       := TImage.Create(Self);
  FImage.Parent := FScrollBox;
  FImage.Top := 0;
  FImage.Left := 0;
  FImage.Stretch := False;
  FImage.AutoSize := true;

  FbtnLoadFromFile       := TSpeedButton.Create(Self);
  with FbtnLoadFromFile do
  begin
    Parent := FWinControl;
    ParentFont := true;
    Top   := 0;
    Left  := 0;
    Height := 12;
    Width := 12;
    Hint  := 'Load from file';
    ShowHint := true;
    OnClick := DoLoadFromFile;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'PAILOADFROMFILE');
    {$ELSE}
    Glyph.LoadFromLazarusResource('PAILOADFROMFILE');
    {$ENDIF}
  end;
  lLastControl := FbtnLoadFromFile;

  FbtnSaveToFile         := TSpeedButton.Create(Self);
  with FbtnSaveToFile do
  begin
    Parent  := FWinControl;
    ParentFont := true;
    Top     := 0;
    Left    := lLastControl.Left + lLastControl.Width + 4;
    Height  := 12;
    Width   := 12;
    Hint    := 'Save to file';
    ShowHint := true;
    OnClick := DoSaveToFile;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiSaveToFile');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiSaveToFile');
    {$ENDIF}
  end;
  lLastControl := FbtnSaveToFile;

  FBtnCopyToClip         := TSpeedButton.Create(Self);
  with FbtnCopyToClip do
  begin
    Parent  := FWinControl;
    ParentFont := true;
    Top     := 0;
    Left    := lLastControl.Left + lLastControl.Width + 4;
    Height  := 12;
    Width   := 12;
    Hint    := 'Copy to clipboard';
    ShowHint := true;
    OnClick := DoCopyToClip;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiCopyToClipBoard');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiCopyToClipBoard');
    {$ENDIF}
  end;
  lLastControl := FbtnCopyToClip;

  FbtnPasteFromClip       := TSpeedButton.Create(Self);
  with FbtnPasteFromClip do
  begin
    Parent := FWinControl;
    ParentFont := true;
    Top   := 0;
    Left  := lLastControl.Left + lLastControl.Width + 4;
    Height := 12;
    Width := 12;
    Hint  := 'Paste from clipboard';
    ShowHint := true;
    OnClick := DoPasteFromClip;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiPasteFromClipboard');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiPasteFromClipboard');
    {$ENDIF}
  end;
  lLastControl := FbtnPasteFromClip;

{
  FbtnViewFullScreen := TSpeedButton.Create(Self);
  with FbtnViewFullScreen do
  begin
    Parent := FWinControl;
    Top   := 0;
    Left  := lLastControl.Left + lLastControl.Width + 4;
    Height := 12;
    Width := 12;
    Hint  := 'View full screen';
    ShowHint := true;
    OnClick := DoViewFullScreen;
    Glyph.LoadFromResourceName(HInstance, 'paiViewFullScreen');
  end;
  lLastControl := FbtnViewFullScreen;
}

  FbtnEdit          := TSpeedButton.Create(Self);
  with FbtnEdit do
  begin
    Parent := FWinControl;
    ParentFont := true;
    Top   := 0;
    Left  := lLastControl.Left + lLastControl.Width + 4;
    Height := 12;
    Width := 12;
    Hint  := 'Edit';
    ShowHint := true;
    OnClick := DoEdit;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiEdit');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiEdit');
    {$ENDIF}
  end;
  lLastControl := FbtnEdit;

  FbtnClear          := TSpeedButton.Create(Self);
  with FbtnClear do
  begin
    Parent := FWinControl;
    ParentFont := true;
    Top   := 0;
    Left  := lLastControl.Left + lLastControl.Width + 4;
    Height := 12;
    Width := 12;
    Hint  := 'Clear';
    ShowHint := true;
    OnClick := DoClear;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiClear');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiClear');
    {$ENDIF}
  end;
  lLastControl := FbtnClear;

  FbtnStretch       := TSpeedButton.Create(Self);
  with FbtnStretch do
  begin
    Parent := FWinControl;
    ParentFont := true;
    Top   := 0;
    Left  := lLastControl.Left + lLastControl.Width + 4;
    Height := 12;
    Width := 12;
    Hint  := 'Stretch';
    ShowHint := true;
    OnClick := DoStretch;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, 'paiStretch');
    {$ELSE}
    Glyph.LoadFromLazarusResource('paiStretch');
    {$ENDIF}
  end;

  if not(csDesigning in ComponentState) then
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TSpeedButton then
        TSpeedButton(Components[i]).Flat := true;

  FFilter := GraphicFilter(TGraphic);
  FsFileName := '';
end;

procedure TtiPerAwareImageEdit.DataToWinControl;
var
  lData : TObject;
begin
  if not DataAndPropertyValid then
  begin
    FImage.Picture.Assign(nil);
    Exit; //==>
  end;
  lData := GetObjectProp(FData, FsFieldName);
  if not (lData is TGraphic) then
    raise exception.CreateFmt(
      'Property %s.%s is not of type TGraphic',
      [FData.ClassName, FsFieldName]);

  FImage.Picture.Assign(TGraphic(lData));

end;

procedure TtiPerAwareImageEdit.DoPasteFromClip(Sender: TObject);
begin
  FImage.Picture.Assign(Clipboard);
  DoChange(self);
end;

procedure TtiPerAwareImageEdit.DoLoadFromFile(Sender: TObject);
var
  lSD : TOpenPictureDialog;
  lDefaultAction : boolean;
begin
  lSD := TOpenPictureDialog.Create(nil);
  try
    lSD.Filter := FFilter;
    lSD.FileName := FsFileName;
    if FsFileName = '' then
      lSD.InitialDir := FsInitialDir;
    lSD.DefaultExt := cusImageDefaultExt;
    if lSD.Execute then
    begin
      FsFileName := lSD.FileName;
      lDefaultAction := true;
      if Assigned(FOnLoadFromFile) then
        FOnLoadFromFile(Self, FsFileName, lDefaultAction);
      if lDefaultAction then
      begin
        FImage.Picture.LoadFromFile(FsFileName);
        DoChange(self);
      end;
      FsInitialDir := ExtractFilePath(lSD.FileName);
    end;
  finally
    lSD.Free;
  end;
end;

procedure TtiPerAwareImageEdit.DoCopyToClip(Sender: TObject);
begin
  Clipboard.Assign(FImage.Picture);
end;

procedure TtiPerAwareImageEdit.DoSaveToFile(Sender: TObject);
var
  lSD : TSavePictureDialog;
begin
  lSD := TSavePictureDialog.Create(nil);
  try
    lSD.Filter    := GraphicFilter(TGraphic);
    lSD.FileName  := FsFileName;
    lSD.InitialDir := FsInitialDir;
    lSD.DefaultExt := cusImageDefaultExt;
    if lSD.Execute then
    begin
      FsFileName := lSD.FileName;
      FImage.Picture.SaveToFile(FsFileName);
      {$IFNDEF FPC}
      FsInitialDir := ExtractFilePath(FsFileName);
      if MessageDlg('Do you want to edit this file now?',
                     mtConfirmation,
                     [mbYes, mbNo],
                     0) = mrYes then
         ShellExecute(screen.activeForm.handle,
                       nil,
                       PChar(FsFileName),
                       nil,
                       nil,
                       SW_SHOWNORMAL);
      {$ENDIF}
    end;
  finally
    lSD.Free;
  end;
end;

function TtiPerAwareImageEdit.GetStretch: boolean;
begin
  result := FImage.Stretch;
end;

function TtiPerAwareImageEdit.GetValue: TPicture;
begin
  result := FImage.Picture;
end;

procedure TtiPerAwareImageEdit.PositionWinControl;
begin
  inherited;

  if FLabel.Visible or (VisibleButtons <> []) then
  begin
    //FScrollBox.Align := alNone;
    FScrollBox.Height := FWinControl.Height - 2 - FScrollBox.Top;
    FScrollBox.Width := FWinControl.Width - 2 ;
  end
  else
  begin
    //FScrollBox.Align := alClient;
    FScrollBox.SetBounds(1, 1,  FScrollBox.Parent.Width-2, FScrollBox.Parent.Height-2);
  end;
end;

procedure TtiPerAwareImageEdit.SetControlColor;
begin
  Inherited;
  FScrollBox.Color         := FWinControl.Brush.Color;
  FbtnLoadFromFile.Enabled := FbtnLoadFromFile.Visible  and Enabled and Not ReadOnly;
  FbtnSaveToFile.Enabled   := FbtnSaveToFile.Visible    and Enabled and Not ReadOnly;
  FbtnPasteFromClip.Enabled := FbtnPasteFromClip.Visible and Enabled and Not ReadOnly;
  FBtnCopyToClip.Enabled   := FBtnCopyToClip.Visible    and Enabled and Not ReadOnly;
  FbtnEdit.Enabled         := FbtnEdit.Visible          and Enabled and Not ReadOnly and Assigned(FOnEdit);
  FbtnClear.Enabled        := FbtnClear.Visible         and Enabled and Not ReadOnly;
end;

procedure TtiPerAwareImageEdit.SetOnChangeActive(AValue: boolean);
begin
  // Do nothing
end;

procedure TtiPerAwareImageEdit.SetScrollBars(const AValue: TScrollStyle);
begin
  FScrollBars := AValue;
  case AValue of
  ssNone      : begin
                   FScrollBox.VertScrollBar.Visible := false;
                   FScrollBox.HorzScrollBar.Visible := false;
                 end;
  ssHorizontal : begin
                   FScrollBox.VertScrollBar.Visible := false;
                   FScrollBox.HorzScrollBar.Visible := true;
                 end;
  ssVertical  : begin
                   FScrollBox.VertScrollBar.Visible := true;
                   FScrollBox.HorzScrollBar.Visible := false;
                 end;
  ssBoth      : begin
                   FScrollBox.VertScrollBar.Visible := true;
                   FScrollBox.HorzScrollBar.Visible := true;
                 end;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidLabelStyle);
  end;

end;

procedure TtiPerAwareImageEdit.SetStretch(const AValue: boolean);
begin
  FImage.Stretch := AValue;
  if FImage.Stretch then
  begin
    FImage.AutoSize := False;
    FImage.Align := alClient;
  end
  else
  begin
    FImage.Align := alNone;
    FImage.AutoSize := True;
  end;
end;

procedure TtiPerAwareImageEdit.SetValue(const AValue: TPicture);
begin
  // ToDo: There is some kind of problem with Stretch - the image will be empty if SetValue is called
  //       and Stretch = True
  // Stretch := True;
  FImage.Picture.Assign(AValue);
  // Stretch := False;
end;

procedure TtiPerAwareImageEdit.WinControlToData;
var
  lData  : TObject;
begin
  if not DataAndPropertyValid then
    Exit; //==>

  if ReadOnly then
    Exit; //==>

  lData := GetObjectProp(FData, FsFieldName);
  if not (lData is TGraphic) then
    raise exception.CreateFmt(
      'Property %s.%s is not of type TGraphic',
      [FData.ClassName, FsFieldName]);
  TGraphic(lData).Assign(FImage.Picture.Graphic);

end;

// This will always edit the image as a bitmap - which is probably not
// what we want. Better come up with some way of doing this so we can edit
// any sort of file with the default editor for that file type.
procedure TtiPerAwareImageEdit.DoEdit(Sender: TObject);
begin
  Assert(Assigned(FOnEdit), 'FOnEdit not assigned');
  FOnEdit(Self)
end;

procedure TtiPerAwareImageEdit.DoClear(Sender: TObject);
begin
  FImage.Picture.Assign(nil);
  DoChange(self);
end;

procedure TtiPerAwareImageEdit.DoStretch(Sender: TObject);
begin
  Stretch := Not Stretch;
end;

function TtiPerAwareImageEdit.CanPasteFromClipboard: Boolean;
var
  Format: Word;
begin
  Clipboard.Open;
  try
  {$IFNDEF FPC}
    Format := EnumClipboardFormats(0);
    while Format <> 0 do
    begin
      if FImage.Picture.SupportsClipboardFormat(Format) then
      begin
        Result := True;
        Exit; //==>
      end;
      Format := EnumClipboardFormats(Format);
    end;
    {$ELSE}
     {$WARNING Not sure how to fix that}
    {$ENDIF}
    Result := False;
  finally
    Clipboard.Close;
  end;
end;


{
procedure TtiPerAwareImageEdit.DoViewFullScreen(Sender: TObject);
var
  lsFileName : TFileName;
begin
  lsFileName := GetTempFileName;
  FImage.Picture.SaveToFile(lsFileName);
  try
    EditFile(lsFileName);
    Sleep(1000);
  finally
    DeleteFile(lsFileName);
  end;
end;}

{
function TtiPerAwareImageEdit.GetTempFileName: TFileName;
const
  cMaxPathLen = 255;
var
  pcTemp : array[0..cMaxPathLen] of char;
  pcApp : array[0..cMaxPathLen] of char;
  pcPath : array[0..cMaxPathLen] of char;
begin
  strPCopy(pcApp, copy(extractFileName(application.exeName), 1, 3));
  getTempPath(cMaxPathLen, pcPath);
  Windows.getTempFileName(pcPath, pcApp, 0, pcTemp);
  result := strPas(pcTemp);
  DeleteFile(result);
  result := ChangeFileExt(result, '.BMP');
  if FileExists(result) then
    DeleteFile(result);
end;
}

{
procedure TtiPerAwareImageEdit.EditFile(const AFileName: TFileName);
var
  lForm     : TtiPerAwareImageEditForm;
begin
  lForm := TtiPerAwareImageEditForm.CreateNew(nil);
  try
    lForm.Execute(AFileName);
  finally
    lForm.Free;
  end;
end;
}

function TtiPerAwareImageEdit.IsPropReadOnly: boolean;
begin
  // This is necessary because IsPropReadOnly in the abstract TtiPerAware will
  // check that the property has a Set method as well as a Get method. It is
  // possible for an image property to have a Get method only, so
  // IsPropReadOnly would return true, which is not what we want in this case.
  result := false;
end;

function TtiPerAwareImageEdit.IsEmpty: Boolean;
begin
  Result :=
    not Assigned(FImage.Picture.Graphic)
    or
    (Assigned(FImage.Picture.Graphic) and FImage.Picture.Graphic.Empty);
end;

function TtiPerAwareImageEdit.GetProportional: Boolean;
begin
  Result := FImage.Proportional;
end;

procedure TtiPerAwareImageEdit.SetProportional(const AValue: Boolean);
begin
  FImage.Proportional := AValue;
end;

{$IFNDEF FPC}
procedure TtiPerAwareImageEdit.CMFontChanged(var Message: TMessage);
begin
 TPanel(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareImageEditForm
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
constructor TtiPerAwareImageEditForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited;
  Caption := ' Edit an image';
  Width := Screen.Width div 2;
  Height := Screen.Height div 2;
  Position := poScreenCenter;
  OnShow := DoOnShow;
  OnClose := DoOnClose;

  FMenu := TMainMenu.Create(Self);

  FOleContainer := TOleContainer.Create(self);
  with FOleContainer do
  begin
    Parent := Self;
    Align := alClient;
    AutoActivate := aaManual;
  end;
end;

procedure TtiPerAwareImageEditForm.DoOnClose(Sender: TObject; Var Action : TCloseAction);
begin
end;

procedure TtiPerAwareImageEditForm.DoOnShow(Sender: TObject);
begin
  FOleContainer.DoVerb(ovShow);
end;

function TtiPerAwareImageEditForm.Execute(const AFileName: TFileName): boolean;
begin
  FFileName := AFileName;
  FOleContainer.CreateObjectFromFile(FFileName, False);
  result := ShowModal = mrOK;
  // I can't make this work reliably. Got any ideas?
  FOleContainer.SaveAsDocument(FFileName);
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxStatic
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiPerAwareComboBoxStatic.DataToWinControl;
var
  lsValue : string;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  lsValue := GetPropValue(FData, FsFieldName, True);
  TComboBox(FWinControl).ItemIndex :=
    TComboBox(FWinControl).Items.IndexOf(lsValue);
  SetOnChangeActive(true);
end;

function TtiPerAwareComboBoxStatic.GetItems: TStrings;
begin
  result := TComboBox(FWinControl).Items;
end;

function TtiPerAwareComboBoxStatic.GetValue: String;
begin
  result := TComboBox(FWinControl).Text;
end;

procedure TtiPerAwareComboBoxStatic.SetItems(const AValue: TStrings);
begin
  TComboBox(FWinControl).Items.Assign(AValue);
end;

procedure TtiPerAwareComboBoxStatic.SetValue(const AValue: String);
begin
  SetOnChangeActive(false);
  TComboBox(FWinControl).ItemIndex :=
    TComboBox(FWinControl).Items.IndexOf(AValue);
  WinControlToData;
  SetOnChangeActive(true);
end;

procedure TtiPerAwareComboBoxStatic.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(FData, FsFieldName, TComboBox(FWinControl).Text);
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxDynamic
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiPerAwareComboBoxDynamic.Refresh;
var
  lItems: TStrings;
  I: Integer;
begin
  lItems:= TComboBox(WinControl).Items;
  lItems.Clear;

  if (FList = nil) or
     (FList.Count < 1) or
     (SameText(FsFieldNameDisplay, EmptyStr)) then
    Exit; //==>
  try
    for I := 0 to FList.Count - 1 do
      lItems.AddObject(GetPropValue(TObject(FList.Items [ I ]), FsFieldNameDisplay, True),
                        TObject(FList.Items[ I ]));
  except
    on e:exception do
      raise Exception.CreateFmt('Error adding list items to combobox ' +
                                 'Message: %s, Item Property Name: %s',
                                 [e.message,
                                  FsFieldNameDisplay]);

  end;

  inherited;

end;

procedure TtiPerAwareComboBoxDynamic.DataToWinControl;
var
  lValue : TtiObject;
  lPropType : TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  lPropType := PropType(Data, FieldName);
  if lPropType = tkClass then
  begin
    lValue := TtiObject(GetObjectProp(Data, FieldName));
  end else if (lPropType = tkString) or (lPropType = tkLString) then
  begin
    Assert(Assigned(FOnGetObjectProp), 'OnGetObjectProp not assigned');
    lValue := nil;
    FOnGetObjectProp(lValue);
  end else
    raise EtiOPFProgrammerException.Create(cErrorPropTypeNotClassOrString);

  SetValue (lValue);

  SetOnChangeActive(true);
end;

function TtiPerAwareComboBoxDynamic.GetValue: TtiObject;
begin
  with TComboBox(WinControl) do
    if (ItemIndex >= 0) and (ItemIndex < FList.Count) then
      Result:= TtiObject(FList [ ItemIndex ])
    else
      Result := nil;
end;

procedure TtiPerAwareComboBoxDynamic.SetList(const AValue: TList);
begin
  if FList = AValue then
    Exit; //==>

  FList := AValue;

  Refresh;
end;

procedure TtiPerAwareComboBoxDynamic.SetValue(const AValue: TtiObject);
var
  I: Integer;
begin
//  TList doesn't have an IndexOfObject - simulate it...
//  Set the index only (We're assuming the item is present in the list)
  TComboBox(WinControl).ItemIndex := -1;
  if AValue = nil then
    Exit; //==>

  if not Assigned(FList) then
    raise EtiOPFProgrammerException.Create(cErrorListHasNotBeenAssigned);

  for I := 0 to FList.Count - 1 do
    if TtiObject(FList.Items[ I ]) = AValue then
    begin
      TComboBox(WinControl).ItemIndex := I;
      Break; //==>
    end;
end;

procedure TtiPerAwareComboBoxDynamic.WinControlToData;
var
  lValue : TtiObject;
  lPropType : TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  if ComboBox.ItemIndex < 0 then
    Exit; //==>

  lValue := tTiObject(FList.Items[ComboBox.ItemIndex]);

  lPropType := PropType(Data, FieldName);
  if lPropType = tkClass then
  begin
    SetObjectProp(Data, FieldName, lValue);
  end else if (lPropType = tkString) or (lPropType = tkLString) then
  begin
    Assert(Assigned(FOnSetObjectProp), 'OnSetObjectProp not assigned and it must be when accessing list by a string property');
    OnSetObjectProp(lValue);
  end else
    raise EtiOPFProgrammerException.Create(cErrorPropTypeNotClassOrString);

end;

procedure TtiPerAwareComboBoxDynamic.SetFieldNameDisplay(const AValue: string);
begin
  FsFieldNameDisplay := AValue;
  Refresh;
end;

procedure TtiPerAwareComboBoxDynamic.DoOnExit( Sender : TObject ) ;
begin
  if (CompareStr(ComboBox.Text, ComboBox.Items[ComboBox.ItemIndex]) <> 0)
  and (TComboBox( WinControl ).ItemIndex >= 0) then // valid entry (and changed)
  begin
// clean up case after typing
    SetOnChangeActive(false);
    ComboBox.Text := ComboBox.Items[ComboBox.ItemIndex];
    SetOnChangeActive(true);
  end;

  if TComboBox( WinControl ).ItemIndex < 0 then
  begin
    ComboBox.ItemIndex := 0;
    ComboBox.Text := '';
    WinControlToData ;
  end;
  inherited;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareComboBoxAbs.Create(AOwner: TComponent);
begin
  FWinControl := TComboBox.Create(self);
  TComboBox(FWinControl).OnChange := DoChange;
  TComboBox(FWinControl).Style := csDropDownList;
  TComboBox(FWinControl).OnKeyPress := DoOnKeyPress;
  TComboBox(FWinControl).OnKeyDown := DoOnKeyDown;
  TComboBox(FWinControl).OnExit := DoOnExit ;
  FbCenterWhenLabelIsLeft := true;
  inherited;
  TComboBox(FWinControl).ParentFont := true;
  Height := cDefaultHeightSingleRow;
end;

{
procedure TtiPerAwareComboBoxAbs.DoOnClick(Sender: TObject);
begin
  // Only call inherited if not ReadOnly because the comboBox will have been
  // disabled if it is ReadOnly.
  if not ReadOnly then
    Inherited DoOnClick(Sender);
end;
}

function TtiPerAwareComboBoxAbs.GetDropDownCount: integer;
begin
  result := TComboBox(WinControl).DropDownCount;
end;

function TtiPerAwareComboBoxAbs.GetItemIndex: integer;
begin
  result := TComboBox(FWinControl).ItemIndex;
end;

procedure TtiPerAwareComboBoxAbs.SetDropDownCount(const AValue: integer);
begin
  TComboBox(WinControl).DropDownCount := AValue;
end;

procedure TtiPerAwareComboBoxAbs.SetItemIndex(const AValue: integer);
begin
  TComboBox(FWinControl).ItemIndex := AValue;
end;

procedure TtiPerAwareComboBoxAbs.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
  begin
    TComboBox(WinControl).OnClick := DoChange;
    TComboBox(WinControl).OnExit  := DoOnExit;
  end
  else
  begin
    TComboBox(WinControl).OnClick := nil;
    TComboBox(WinControl).OnExit  := nil;
  end;
end;

procedure TtiPerAwareComboBoxAbs.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  if HandleAllocated then
  begin
  {$IFNDEF FPC}
    SendMessage(Handle, EM_SETREADONLY, Ord(AValue), 0);
  {$ENDIF}
    TComboBox(WinControl).Style := uStyles[ AValue ];
  end else if AValue then
    TComboBox(WinControl).Enabled := False
  else
    TComboBox(WinControl).Enabled := Enabled;
end;

{ TtiPerAwareComboBoxHistory }

constructor TtiPerAwareComboBoxHistory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TComboBox(FWinControl).Style := csDropDown;
  TComboBox(FWinControl).OnExit := DoOnExit;
  FiHistoryCount := 5;

  FPopupMenu := TPopupMenu.Create(nil);
  PopupMenu := FPopupMenu;

  // Clear menu item
  FpmiClear         := TMenuItem.Create(nil);
  FpmiClear.Caption := '&Clear';
  FpmiClear.OnClick := pmiClearOnClick;
  FPopupMenu.Items.Add(FpmiClear);
end;

destructor TtiPerAwareComboBoxHistory.Destroy;
begin
  FpmiClear.Free;
  FPopupMenu.Free;
  inherited Destroy;
end;

procedure TtiPerAwareComboBoxHistory.DoOnExit(Sender : TObject);
begin
  Save;
  inherited;
end;

function TtiPerAwareComboBoxHistory.GetRegINIFile: TRegINIFile;
begin
  result := TRegINIFile.Create(
              ChangeFileExt(ExtractFileName(application.exeName), ''));
end;

procedure TtiPerAwareComboBoxHistory.Loaded;
begin
  inherited loaded;
  Read;
  // Select the last selected value
  if TComboBox(WinControl).Items.Count >= 2 then
    TComboBox(WinControl).ItemIndex := 1;
end;

procedure TtiPerAwareComboBoxHistory.pmiClearOnClick(sender: TObject);
begin
  TComboBox(FWinControl).Items.Clear;
  TComboBox(FWinControl).Items.Add('');
end;

procedure TtiPerAwareComboBoxHistory.Read;
var
  lReg : TRegINIFile;
  ls  : string;
  lsText : string;
begin
  lsText := Value;
  TComboBox(WinControl).Items.clear;
  lReg := GetRegINIFile;
  try
    ls := lReg.ReadString(Owner.Name, Name, '');
    TComboBox(WinControl).Items.CommaText := ls;
    TComboBox(WinControl).Items.Insert(0, '');
  finally
    lReg.Free;
  end;
  Value := lsText;
end;

procedure TtiPerAwareComboBoxHistory.Save;
var
  lReg : TRegINIFile;
  ls : string;
  lsText : string;
  i : integer;
begin
  lsText := Value;
  if lsText <> '' then begin
    // Is the current item already in the history list ?
    for i := 0 to Items.Count - 1 do
      if UpperCase(Items[i]) = UpperCase(lsText) then begin
        Items.Delete(i);
        Break; //==>
      end;

    Items.Insert(1, lsText);

  end;

  // If we have more items in the history, then delete the last one
  if Items.Count > HistoryCount + 1 then
    Items.Delete(Items.Count - 1);

  ls := Items.CommaText;

  if ls = '""' then
    ls := '';
  ls := Copy(ls, 2, length(ls) - 1);
  lReg := GetRegINIFile;
  try
    lReg.WriteString(Owner.Name, Name, ls);
  finally
    lReg.Free;
  end;

  Value := lsText;

end;

procedure TtiPerAwareComboBoxHistory.SetHistoryCount(const iValue: integer);
begin
  if iValue < 5 then begin
    FiHistoryCount := 5;
    exit;
  end;
  if iValue > 20 then begin
    FiHistoryCount := 20;
    exit;
  end;
  FiHistoryCount := iValue;
end;

function TtiPerAwareDateTimePicker.GetDateMode: TDTDateMode;
begin
  {$IFNDEF FPC}
  result := TDateTimePicker(WinControl).DateMode;
  {$ELSE}
  result := dmComboBox;
  {$ENDIF}
end;

function TtiPerAwareDateTimePicker.GetKind: TDateTimeKind;
begin
  {$IFNDEF FPC}
  result := TDateTimePicker(WinControl).Kind;
  {$ELSE}
  result := dtkDate;
  {$ENDIF}
end;

procedure TtiPerAwareDateTimePicker.SetDateMode(const AValue: TDTDateMode);
begin
{$IFNDEF FPC} TDateTimePicker(WinControl).DateMode := AValue;{$ENDIF}
end;

procedure TtiPerAwareDateTimePicker.SetKind(const AValue: TDateTimeKind);
begin
{$IFNDEF FPC}  TDateTimePicker(WinControl).Kind := AValue;{$ENDIF}
end;

procedure TtiPerAwareComboBoxHistory.SetValue(const AValue: String);
var
  i : integer;
  lStrings : TStrings;
  lFoundIndex : integer;
begin
  SetOnChangeActive(false);
  lStrings := TComboBox(FWinControl).Items;
  lFoundIndex := -1;
  for i := 0 to lStrings.Count - 1 do
    if lStrings[i] = AValue then
    begin
      lFoundIndex := i;
      Break; //==>
    end;

  if lFoundIndex = -1 then
  begin
    lStrings.Insert(1, AValue);
    lFoundIndex := 1;
  end;

  TComboBox(FWinControl).ItemIndex := lFoundIndex;

  WinControlToData;
  SetOnChangeActive(true);
end;

function TtiPerAwareComboBoxDynamic.GetValueAsString: string;
begin
  result := FValueAsString;
end;
procedure TtiPerAwareCheckBox.DoLabelClick(Sender: TObject);
begin
  if Not Enabled then
    Exit; //==>
  if not Focused then
    SetFocus;
  if ReadOnly then
    Exit; //==>
  TCheckBox(FWinControl).Checked := not TCheckBox(FWinControl).Checked;
end;

procedure TtiPerAwareComboBoxDynamic.SetValueAsString(const AValue: string);
begin
  if not Assigned(FList) then
    raise EtiOPFProgrammerException.Create(cErrorListHasNotBeenAssigned);

  Assert(Assigned(FOnGetObjectProp), 'OnGetObjectProp not assigned and it must be when accessing list by a string property');

  FValueAsString := AValue;
  SetStrProp(FData, FieldName, AValue);
  DataToWinControl;
end;

function TtiPerAwareComboBoxAbs.ComboBox: TComboBox;
begin
  result := TComboBox(WinControl);
end;

//procedure TtiPerAwareComboBoxDynamic.SetFieldNameSelectBy(const AValue: string);
//begin
//  FFieldNameSelectBy := AValue;
//  Refresh;
//end;

function TtiPerAwareComboBoxAbs.GetCharCase: TEditCharCase;
begin
  {$IFNDEF FPC}
  result := TComboBox(FWinControl).CharCase;
  {$ELSE}
  result := ecNormal;
  {$ENDIF}
end;

procedure TtiPerAwareComboBoxAbs.SetCharCase(const AValue: TEditCharCase);
begin
  {$IFNDEF FPC}TComboBox(FWinControl).CharCase := AValue;{$ENDIF}
end;

procedure TtiPerAwareComboBoxAbs.DoOnExit(Sender: TObject);
begin
  // Handled by subclass
end;

procedure TtiPerAwareComboBoxAbs.DoOnKeyPress(Sender: TObject; var Key: Char);
begin
  if not ReadOnly then
    Inherited DoOnKeyPress(Sender, Key)
  else
    Key := #0;
end;

{$IFNDEF FPC}
procedure TtiPerAwareComboBoxAbs.CMFontChanged(var Message: TMessage);
begin
 TComboBox(FWinControl).Font.Assign(Self.Font);
 inherited;
end;
{$ENDIF}

procedure TtiPerAwareImageEdit.SetVisibleButtons(const AValue: TtiImageEditVisibleButtons);
begin
  FVisibleButtons := AValue;
  FbtnLoadFromFile.Visible := ievbLoadFromFile in AValue;
  FbtnSaveToFile.Visible   := ievbSaveToFile in AValue;
  FbtnPasteFromClip.Visible := ievbPasteFromClip in AValue;
  FBtnCopyToClip.Visible   := ievbCopyToClip in AValue;
  FbtnEdit.Visible         := ievbEdit in AValue;
  FbtnClear.Visible        := ievbClear in AValue;
  FbtnStretch.Visible      := ievbStretch in AValue;
end;

procedure TtiPerAwareAbs.SetGreyWhenReadOnly(const AValue: boolean);
begin
  FGreyWhenReadOnly := AValue;
  SetControlColor;
end;

procedure TtiPerAwareImageEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  if (not ReadOnly) and (Assigned(FOnEdit)) then
    FImage.OnDblClick := DoEdit
  else
    FImage.OnDblClick := nil;
end;

procedure TtiPerAwareImageEdit.SetOnEdit(const AValue: TNotifyEvent);
begin
  FOnEdit := AValue;
  if (not ReadOnly) and (Assigned(FOnEdit)) then
    FImage.OnDblClick := DoEdit
  else
    FImage.OnDblClick := nil;
end;

procedure TtiPerAwareDateTimePicker.Loaded;
begin
  inherited;
end;

constructor TtiPerAwareComboBoxDynamic.Create(AOwner: TComponent);
begin
  inherited;
  FsFieldNameDisplay := 'Caption';
end;

{ TtiPerAwareImageEditAction }

constructor TtiPerAwareImageEditAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := False;
end;

function TtiPerAwareImageEditAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

function TtiPerAwareImageEditAction.IsDisabledIfNoHandler: Boolean;
begin
  Result := DisableIfNoHandler and not Assigned(OnExecute);
end;

function TtiPerAwareImageEditAction.IsEnabledReadOnly: Boolean;
begin
  Result := Assigned(FImageControl) and not FImageControl.ReadOnly;
end;

procedure TtiPerAwareImageEditAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = ImageControl) then ImageControl := nil;
end;

procedure TtiPerAwareImageEditAction.SetImageControl(const AValue: TtiPerAwareImageEdit);
begin
  if AValue <> FImageControl then
  begin
    FImageControl := AValue;
    if AValue <> nil then
      AValue.FreeNotification(Self);
  end;
end;

procedure TtiPerAwareImageEditAction.UpdateTarget(Target: TObject);
begin
  Enabled := IsEnabledReadOnly and not IsDisabledIfNoHandler;
end;



{ TtiImageLoadAction }

procedure TtiImageLoadAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoLoadFromFile(Self);
end;

procedure TtiImageLoadAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
   if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_OpenImage);
{$ENDIF}
end;


{ TtiImageSaveAction }

procedure TtiImageSaveAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoSaveToFile(Self);
end;

procedure TtiImageSaveAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Save);
{$ENDIF}
end;

procedure TtiImageSaveAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
end;


{ TtiImagePasteFromClipboardAction }

procedure TtiImagePasteFromClipboardAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoPasteFromClip(Self);
end;

procedure TtiImagePasteFromClipboardAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_PasteFromClipboard);
{$ENDIF}
end;

procedure TtiImagePasteFromClipboardAction.UpdateTarget(Target: TObject);
begin
  Enabled := IsEnabledReadOnly and ImageControl.CanPasteFromClipboard;
end;


{ TtiImageCopyToClipboardAction }

procedure TtiImageCopyToClipboardAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoCopyToClip(Self);
end;

procedure TtiImageCopyToClipboardAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CopyToClipboard);
{$ENDIF}
end;

procedure TtiImageCopyToClipboardAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
end;


{ TtiImageEditAction }

constructor TtiImageEditAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := True;
end;

procedure TtiImageEditAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Edit);
{$ENDIF}
end;

{procedure TtiImageEditAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
end;}


{ TtiImageNewAction }

constructor TtiImageNewAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := True;
end;

procedure TtiImageNewAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Insert);
{$ENDIF}
end;


{ TtiImageClearAction }

procedure TtiImageClearAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoClear(Self);
end;

procedure TtiImageClearAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Deletex);
{$ENDIF}
end;


procedure TtiImageClearAction.UpdateTarget(Target: TObject);
begin
  Enabled := IsEnabledReadOnly and not ImageControl.IsEmpty;
end;


{ TtiImageStretchAction }

procedure TtiImageStretchAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(OnExecute) then
    inherited
  else
    ImageControl.DoStretch(Self);
end;

procedure TtiImageStretchAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Maximize);
{$ENDIF}
end;

procedure TtiImageStretchAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
  if Enabled then
    if Checked <> ImageControl.FImage.Stretch then
      Checked := ImageControl.FImage.Stretch;
end;

{ TtiImageViewAction }

constructor TtiImageViewAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := True;
end;

procedure TtiImageViewAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_ZoomIn);
{$ENDIF}
end;

procedure TtiImageViewAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
end;

{ TtiImageExportAction }

constructor TtiImageExportAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := True;
end;

procedure TtiImageExportAction.ReadState(Reader: TReader);
begin
  inherited;
{$IFNDEF PACKAGE}
  if not (csDesigning in ComponentState) and (ImageIndex = -2) then
    ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Export);
{$ENDIF}
end;

procedure TtiImageExportAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(ImageControl) and not ImageControl.IsEmpty;
end;

initialization
  // 02/01/2002, Ha-Hoe, Made change to decimal separator
  cFloatEditMask     := '#' + ThousandSeparator  + '##0' ;
  cValidFloatChrs    :=
    [ '-', DecimalSeparator ,
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ];

end.

