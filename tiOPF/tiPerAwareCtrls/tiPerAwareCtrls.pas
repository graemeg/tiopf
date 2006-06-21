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
    November 2000, Peter Hinrichsen, Made open source
    Feb 2001, Ian Krigsman, Added TtiPerAwareComboBoxDynamic
    Jan 2002, Ha-Hoe, Made change to decimal separator
    Jan 2000, Ian Krigsman, Surfaced OnEnter and OnExit in tiPerAwareAbs
    Aug 2002, Ian Krigsman, Added Error and ErrorColor properties
    Jul 2003, Andrew Denton, Added ShowFocusRect property.

  Purpose:
    A family of TPersistent aware controls

  Classes:
    TLabelStyle               - LabelStyle can have these values
    TtiPerAwareAbs            - Abstract base class
    TtiPerAwareEdit           - A wrapper for the TEdit control
    TtiPerAwareMemo           - A wrapper for the TMemo control
    TtiPerAwareDateTimePicker - A wrapper for the TDateTimePicker control
    TtiPerAwareCheckBox       - This control should be re-built using custom
                                images for the check box so the check box
                                can be painted in grey when disabled.
    TtiFloatEditStyle         - The TtiPerAwareFloatEdit can be of these
                                types
    TtiPerAwareFloatEdit      - A wrapper for the TEdit control, with some
                                additional methods to implement number editing.


  ToDo:
    1. Come up with some way of showing focus arround the entire control
    2. TtiPerAwareCheckBox to be custom bulid from a Image so it can be
       greyed when not focused. Also, better drawing of a FocusRect when
       selected.
    3. Create some sort of datalink or observer so changes in the underlying
       data class are reflected in the control when they are made.
    4. Why is WinControlToData being called when LinkToData is set?
    5. ComboBox, CheckBox and DateTimePicker, when read only are drawn in the
       wrong colour. Fix this.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerAwareCtrls;

interface
uses
  Windows
  ,SysUtils
  ,Messages
  ,Classes
  ,Graphics
  ,Controls
  ,Forms
  ,StdCtrls
  ,extCtrls
  ,comctrls
  ,Buttons
  ,OleCtnrs
  ,Menus
  ,registry
  ,tiFocusPanel
  ;

const
  cuiDefaultLabelWidth = 80 ;
  cDefaultHeight       = 23 ;

resourcestring
  sListHasNotBeenAssigned = 'List has not been assigned';
  sPropTypeNotClassOrString = 'FieldName property not an object or string' ;
  sCaption = 'Caption';


type

  // LabelStyle can have these values
  // ---------------------------------------------------------------------------
  TLabelStyle = ( lsNone, lsTop, lsLeft, lsTopLeft, lsRight ) ;

  // Abstract base class
  // ---------------------------------------------------------------------------
  TtiPerAwareAbs = class( TtiFocusPanel )
  protected
    FLabelStyle : TLabelStyle;
    FLabel      : TLabel ;
    FWinControl : TWinControl ;
    FbCenterWhenLabelIsLeft : boolean ;
    FsFieldName: string;
    FData: TPersistent;
    FOnChange: TNotifyEvent;
    FiLabelWidth : integer ;
    FbDirty : boolean ;
    FbReadOnly: Boolean;
    FbError : boolean;
    FErrorColor : TColor;
    FGreyWhenReadOnly: boolean;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDown: TKeyEvent;

    procedure   SetLabelStyle(const Value: TLabelStyle); virtual ;
    function    GetCaption: TCaption; virtual ;
    procedure   SetCaption(const Value: TCaption); virtual ;
    procedure   SetData(const Value: TPersistent); virtual ;
    procedure   WMSize( var Message: TWMSize ) ; message WM_SIZE ;
    procedure   PositionLabel ; virtual ;
    procedure   PositionWinControl ; virtual ;
    procedure   SetLabelWidth(const Value: Integer); virtual ;
    procedure   SetFieldName(const Value: string); virtual ;

    procedure   Loaded ; override ;
    procedure   DataToWinControl ; virtual ; abstract ;
    procedure   WinControlToData ; virtual ; abstract ;
    procedure   DoChange( Sender : TObject ) ; virtual ;
    procedure   SetOnChangeActive( Value : boolean ) ; virtual ; abstract ;
    procedure   SetEnabled( Value : boolean ) ; Override ;
    procedure   SetReadOnly(const Value: Boolean);virtual;
    procedure   SetControlColor ; virtual ;
    procedure   SetError(const Value: boolean); virtual;
    procedure   SetErrorColor(const Value: TColor); virtual;
    procedure   SetGreyWhenReadOnly(const Value: boolean); virtual;
    procedure   DoOnClick( Sender : TObject ) ; virtual ;
    procedure   DoOnKeyPress( Sender : TObject ; var Key : Char ) ; virtual ;
    procedure   DoOnKeyDown( Sender : TObject ; var Key: Word; Shift: TShiftState ) ;

    property    CenterWhenLabelIsLeft: boolean read FbCenterWhenLabelIsLeft write FbCenterWhenLabelIsLeft;
    property    OnKeyPress : TKeyPressEvent read FOnKeyPress write FOnKeyPress ;
    property    OnKeyDown  : TKeyEvent read FOnKeyDown write FOnKeyDown ;

    function    DataAndPropertyValid : boolean ;
    function    IsPropReadOnly : boolean ; virtual ;

  published
    property    Align ;
    property    Anchors ;
    property    Constraints ;
    property    Enabled ;
    property    Font ;
    property    Color ;
    property    TabOrder ;
    property    OnEnter ;
    property    OnExit ;
    property    ShowFocusRect;
    property    ShowHint ;
    property    ParentColor;
    property    ParentCtl3D;

    property    LabelStyle : TLabelStyle read FLabelStyle   write SetLabelStyle default lsLeft ;
    property    Caption    : TCaption    read GetCaption    write SetCaption ;
    property    LabelWidth : Integer     read FiLabelWidth  write SetLabelWidth default cuiDefaultLabelWidth ;
    property    ReadOnly   : Boolean     read FbReadOnly    write SetReadOnly ;

    property    FieldName  : string      read FsFieldName   write SetFieldName ;

    property    Error : boolean read FbError write SetError default False;
    property    ErrorColor : TColor read FErrorColor write SetErrorColor default clYellow;
    property    GreyWhenReadOnly : boolean read FGreyWhenReadOnly write SetGreyWhenReadOnly default true ;

    // TWinControl has these, so they can be implemented with a direct mapping
    //property    OnEnter    : TNotifyEvent read FOnEnter write SetOnExit ;
    //property    OnExit     : TNotifyEvent read FOnExit  write SetOnExit ;
    //property    OnKeyDown
    //property    OnKeyPress
    //property    OnKeyUp

    // TWinControl does not have this, so it must be implemented for each type of control
    property OnChange : TNotifyEvent read FOnChange write FOnChange ;

  public
    constructor Create( Owner : TComponent ) ; override ;
    destructor  Destroy ; override ;
    procedure   SetFocus; override ;
    property    Data       : TPersistent read FData         write SetData ;
    procedure   LinkToData( pData : TPersistent ; const pFieldName : string ) ; virtual ;
    property    Dirty : boolean read FbDirty ;
    procedure   Refresh ; virtual ;
    property    WinControl: TWinControl read FWinControl write FWinControl;
    function    Focused: Boolean; override ;
  end ;
                                          
  // A wrapper for the TEdit control
  // ---------------------------------------------------------------------------
  TtiPerAwareEdit = class( TtiPerAwareAbs )
  private
    function  GetValue: String;
    procedure SetValue(const Value: String);
    function  GetMaxLength: integer;
    procedure SetMaxLength(const Value: integer);
    function  GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    function  GetPasswordChar: Char;
    procedure SetPasswordChar(const Value: Char);
  protected
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
  published
    property Value : String read GetValue write SetValue ;
    property MaxLength : integer read GetMaxLength write SetMaxLength ;
    property CharCase  : TEditCharCase read GetCharCase write SetCharCase ;
    property PasswordChar : Char read GetPasswordChar write SetPasswordChar ;
    property OnKeyPress ;
    property OnKeyDown ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  // A wrapper for the TMemo control
  // ---------------------------------------------------------------------------
  TtiPerAwareMemo = class( TtiPerAwareAbs )
  private
    function  GetScrollBars: TScrollStyle;
    procedure SetScrollBars(const Value: TScrollStyle);
    function  GetValue: string;
    procedure SetValue(const Value: string);
    function  GetWordWrap: boolean;
    procedure SetWordWrap(const Value: boolean);
    function  GetMaxLength: integer;
    procedure SetMaxLength(const Value: integer);
  protected
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
  published
    property ScrollBars : TScrollStyle read GetScrollBars write SetScrollBars ;
    property Value : string read GetValue write SetValue ;
    property WordWrap : boolean read GetWordWrap write SetWordWrap ;
    property MaxLength : integer read GetMaxLength write SetMaxLength ;
    property OnKeyPress ;
    property OnKeyDown ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  // An abstract wrapper for the TComboBox control
  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxAbs = class( TtiPerAwareAbs )
  private
    function  GetDropDownCount: integer;
    procedure SetDropDownCount(const Value: integer);
    function  GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
  protected
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
//    procedure   DoOnClick( Sender : TObject ) ; override ;
    function    ComboBox : TComboBox ;
  published
    property    DropDownCount : integer read GetDropDownCount write SetDropDownCount ;
    property    CharCase  : TEditCharCase read GetCharCase write SetCharCase ;
  public
    constructor Create( Owner : TComponent ) ; override ;
    property    ItemIndex : integer read GetItemIndex write SetItemIndex ;
    procedure   DoOnKeyPress( Sender : TObject ; var Key : Char ) ; override ;
  end ;

  // A wrapper for the TComboBox control that has items entered at design time
  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxStatic = class( TtiPerAwareComboBoxAbs )
  protected
    function    GetValue: String; virtual ;
    procedure   SetValue(const Value: String); virtual ;
    function    GetItems: TStrings; virtual ;
    procedure   SetItems(const Value: TStrings); virtual ;
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
  published
    property Value : String read GetValue write SetValue ;
    property Items : TStrings read GetItems write SetItems ;
  end ;

  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxHistory = class( TtiPerAwareComboBoxStatic )
  private
    FOnValidate : TNotifyEvent ;
    FiHistoryCount: integer;
    FPopupMenu : TPopupMenu ;
    FpmiClear  : TMenuItem ;

    function  GetRegINIFile : TRegINIFile ;
    procedure SetHistoryCount(const iValue: integer);
    procedure pmiClearOnClick( sender : TObject ) ;

  protected
    procedure   SetValue(const Value: String); override ;
    procedure   Loaded ; override ;
    procedure   DoOnExit( Sender : TObject ) ;

  published
    property HistoryCount : integer read FiHistoryCount write setHistoryCount ;
    property OnValidate : TNotifyEvent read FOnValidate write FOnValidate ;
    property OnKeyPress ;
    property OnKeyDown ;

  public
    constructor create(owner: TComponent);override;
    destructor  destroy ; override ;
    procedure   Save ;
    procedure   Read ;

  end ;

  // For backward compatability with existing systems.
  TtiHistoryComboBox = class( TtiPerAwareComboBoxHistory ) ;

  TOnSetObjectPropEvent = procedure( const pData : TPersistent ) of object ;
  TOnGetObjectPropEvent = procedure( var   pData : TPersistent ) of object ;
  // A wrapper for the TComboBox control that has items populated from a
  // TList of TPersistent(s)
  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxDynamic = class( TtiPerAwareComboBoxAbs )
  private
    FList       : TList ;
    FsFieldNameDisplay: string;
    FOnGetObjectProp: TOnGetObjectPropEvent;
    FOnSetObjectProp: TOnSetObjectPropEvent;
    FValueAsString : string ;
    function    GetValue: TPersistent;
    procedure   SetValue(const Value: TPersistent);
    procedure   SetList(const Value: TList);
    procedure   SetFieldNameDisplay(const Value: string);
    function    GetValueAsString: string;
    procedure   SetValueAsString(const Value: string);
  protected
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
  public
    property    Value : TPersistent read GetValue write SetValue ;
    property    ValueAsString : string read GetValueAsString write SetValueAsString ;
    property    List : TList read FList write SetList ;
    procedure   Refresh ; override ;
  published
    property    FieldNameDisplay : string read FsFieldNameDisplay write SetFieldNameDisplay ;
    property    OnGetObjectProp  : TOnGetObjectPropEvent read FOnGetObjectProp write FOnGetObjectProp ;
    property    OnSetObjectProp  : TOnSetObjectPropEvent read FOnSetObjectProp write FOnSetObjectProp ;
  end;


  // A wrapper for the TDateTimePicker control
  // ---------------------------------------------------------------------------
  TtiPerAwareDateTimePicker = class( TtiPerAwareAbs )
  private
    function  GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
    procedure DoOnExit(Sender: TObject);
    procedure DoKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState);
    function  GetMaxDate: TDateTime;
    function  GetMinDate: TDateTime;
    procedure SetMaxDate(const Value: TDateTime);
    procedure SetMinDate(const Value: TDateTime);
    function  GetDateMode: TDTDateMode;
    function  GetKind: TDateTimeKind;
    procedure SetDateMode(const Value: TDTDateMode);
    procedure SetKind(const Value: TDateTimeKind);
  protected
    procedure   SetControlColor ; override ;
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
  published
    property Value : TDateTime read GetValue write SetValue ;
    property MaxDate : TDateTime read GetMaxDate write SetMaxDate ;
    property MinDate : TDateTime read GetMinDate write SetMinDate ;
    property Kind     : TDateTimeKind read GetKind write SetKind ;
    property DateMode : TDTDateMode read GetDateMode write SetDateMode ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  // This control should be re-built using custom images for the check
  // box so the check box can be painted in grey when disabled.
  // ---------------------------------------------------------------------------
  TtiPerAwareCheckBox = class( TtiPerAwareAbs )
  private
    function  GetValue: boolean;
    procedure SetValue(const Value: boolean);
  protected
    procedure   SetControlColor ; override ;
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
    procedure   DoOnClick(Sender: TObject);override;
    procedure   DoLabelClick(Sender: TObject);
  published
    property    Value : boolean read GetValue write SetValue ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;


  // The TtiPerAwareFloatEdit can be of these types
  //----------------------------------------------------------------------------
  TtiFloatEditStyle = ( fesUser, fesInteger, fesFloat, fesCurrency, fesPercent ) ;

  // A wrapper for the TEdit control, with some additional methods to implement
  // number editing.
  //----------------------------------------------------------------------------
  TtiPerAwareFloatEdit   = class( TtiPerAwareAbs )
  private
    FsEditMask   : string ;
    FiPrecision  : integer ;
    FsTextBefore : string ;
    FsTextAfter  : string ;
    FsTextUnknown: string ;
    FrMinValue   : real ;
    FrMaxValue   : real ;
    FrUnknownValue   : real ;
    FsBeforeApplyKey : string ;
    FFloatEditStyle: TtiFloatEditStyle;

    procedure _DoClick( sender : TObject ) ;
    procedure _DoEnter( sender : TObject ) ;
    procedure _DoExit( sender : TObject ) ;
    procedure _DoKeyPress( Sender: TObject; var Key: Char );
    procedure _DoChange( sender : TObject ) ;

    function  RemoveFormatChr( sValue : string ) : string ;
    function  IsValidFloat( sValue : string ) : boolean ;
    function  WithinMinMaxLimits( value : real ) : boolean;
    function  CustomStrToFloat(var pStrValue: string): real;

    function  GetValueAsString : string ;
    procedure SetValueAsString( sValue: string);
    procedure SetValue( rValue : real ) ;
    function  GetValue : real ;
    procedure SetPrecision( iValue : integer ) ;
    procedure SetTextAfter( sValue : string ) ;
    procedure SetTextBefore( sValue : string ) ;
    procedure SetTextUnknown(const sValue: string);
    procedure SetMinValue( rValue : real ) ;
    procedure SetMaxValue( rValue : real ) ;
    procedure SetFloatEditStyle(const Value: TtiFloatEditStyle);
    procedure SetUnknownValue(const rValue: real);
    function GetIsKnown: boolean;
    procedure SetIsKnown(const bValue: boolean);

  protected
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
  published

    property    TextBefore : string read FsTextBefore write setTextBefore ;
    property    TextAfter  : string read FsTextAfter  write setTextAfter ;
    property    TextUnknown  : string read FsTextUnknown  write setTextUnknown ;
    property    ValueAsString  : string read GetValueAsString write SetValueAsString ;
    property    Value     : real    read GetValue   write SetValue  ;
    property    Precision : integer read FiPrecision write setPrecision ;
    property    MinValue  : real    read FrMinValue write setMinValue ;
    property    MaxValue  : real    read FrMaxValue write setMaxValue ;
    property    UnknownValue  : real    read FrUnknownValue write SetUnknownValue ;
    property    IsKnown  : boolean    read GetIsKnown write SetIsKnown;
    property    Style     : TtiFloatEditStyle read FFloatEditStyle write SetFloatEditStyle ;

  public
    constructor create( owner : TComponent ) ; override ;
  end ;

  TtiPerAwareImageEditOnLoadFromFile = procedure( Sender : TObject ; var pFileName : string ; var pDefaultAction : boolean ) of object ;

  TtiImageEditVisibleButton = ( ievbLoadFromFile, ievbSaveToFile, ievbPasteFromClip, ievbCopyToClip,
                                ievbEdit, ievbClear, ievbStretch ) ;
  TtiImageEditVisibleButtons = set of TtiImageEditVisibleButton ;

  //----------------------------------------------------------------------------
  TtiPerAwareImageEdit = class( TtiPerAwareAbs )
  private
    FScrollBox  : TScrollBox ;
    FScrollBars : TScrollStyle ;
    FImage      : TImage ;
    FbtnLoadFromFile   : TSpeedButton ;
    FbtnSaveToFile     : TSpeedButton ;
    FbtnPasteFromClip  : TSpeedButton ;
    FBtnCopyToClip     : TSpeedButton ;
    FbtnEdit           : TSpeedButton ;
    FbtnClear          : TSpeedButton ;
    FbtnStretch        : TSpeedButton ;

    FsFileName : string ;
    FsInitialDir : string ;
    FOnEdit: TNotifyEvent;
    FOnLoadFromFile: TtiPerAwareImageEditOnLoadFromFile;
    FFilter: string;
    FVisibleButtons: TtiImageEditVisibleButtons;

    procedure   SetScrollBars(const Value: TScrollStyle);
    function    GetValue: TPicture;
    procedure   SetValue(const Value: TPicture);
    function    GetStretch: boolean;
    procedure   SetStretch(const Value: boolean);
    procedure   DoLoadFromFile(   Sender : TObject ) ;
    procedure   DoSaveToFile(     Sender : TObject ) ;
    procedure   DoCopyToClip(     Sender : TObject ) ;
    procedure   DoPasteFromClip(  Sender : TObject ) ;
    procedure   DoStretch(        Sender : TObject ) ;
    procedure   DoEdit(           Sender : TObject ) ;
    procedure   DoClear(          Sender : TObject ) ;
    procedure   SetVisibleButtons(const Value: TtiImageEditVisibleButtons);
    procedure SetOnEdit(const Value: TNotifyEvent);
  protected
    procedure   SetReadOnly(const Value: Boolean);override ;
    procedure   SetControlColor ; override ;
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   PositionWinControl ; override ;
    function    IsPropReadOnly : boolean ; override ;
  published
    property    ScrollBars : TScrollStyle read FScrollBars write SetScrollBars ;
    property    Value : TPicture read GetValue write SetValue ;
    property    Stretch : boolean read GetStretch write SetStretch ;
    property    InitialDir : String read FsInitialDir write FsInitialDir ;
    property    OnEdit : TNotifyEvent read FOnEdit write SetOnEdit ;
    property    OnLoadFromFile : TtiPerAwareImageEditOnLoadFromFile read FOnLoadFromFile write FOnLoadFromFile ;
    property    Filter : string read FFilter write FFilter ;
    property    VisibleButtons : TtiImageEditVisibleButtons read FVisibleButtons write SetVisibleButtons;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

{
  TtiPerAwareImageEditForm = class( TForm )
  private
    FMenu : TMainMenu ;
    FOleContainer : TOleContainer ;
    FFileName : TFileName ;
    procedure DoOnShow( Sender : TObject ) ;
    procedure DoOnClose( Sender : TObject ; var Action : TCloseAction ) ;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0) ; override ;
    function    Execute( const
    pFileName : TFileName ) : boolean ;
  end ;
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
  ,ShellAPI
  // ,tiUtils // For debugging
  // ,tiLog
  ;

// Added by Chris Latta (andromeda@froggy.com.au) because the values of some
// constants are changed inside the Initialization section.
{$J+}

const
  cValidFloatChrs : set of char = [ '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] ;
  cIntEditMask        =    '#0'  ;  //ipk 2001-03-01
  cFloatEditMask      : string = ''  ;
//  cusImageFilters     = 'Bitmap files|*.bmp|GIF files|*.gif|JPeg files|*.jpg|All files|*.*' ;
  cusImageDefaultExt  = '.bmp' ;

{$R tiPerAwareCtrls.res}

var
  uStyles : array[boolean] of TComboBoxStyle = (csDropDown, csSimple);

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareAbs.Create(Owner: TComponent);
begin
  inherited ;
  FOnChange      := nil ;

  BevelInner     := bvNone ;
  BevelOuter     := bvNone ;
  BorderStyle    := bsNone ;
  Constraints.MinHeight := cDefaultHeight ;
  OnClick        := DoOnClick ;

  FLabelStyle    := lsLeft ;

  FLabel         := TLabel.Create( Self ) ;
  FLabel.Parent  := self ;
  // ToDo: Default Label.Caption to the component's name
  FLabel.Caption := 'Enter label &name' ;

  FLabel.AutoSize := false ;
  FLabel.Width    := cuiDefaultLabelWidth ;
  FLabel.OnClick  := DoOnClick ;

  Assert( FWinControl <> nil, 'FWinControl not assigned' ) ;
  FWinControl.Parent := self ;
  FLabel.FocusControl := FWinControl ;

  FiLabelWidth := cuiDefaultLabelWidth ;

  FbError     := False;
  FErrorColor := clYellow;
  FGreyWhenReadOnly := true;
end;

//------------------------------------------------------------------------------
destructor TtiPerAwareAbs.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.Refresh;
begin
  DataToWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetFocus;
begin
  inherited;
  if ( Enabled ) and ( FWinControl.Enabled ) then
    FWinControl.SetFocus ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareAbs.GetCaption: TCaption;
begin
  result := FLabel.Caption ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.Loaded;
begin
  inherited;
  PositionLabel ;
  PositionWinControl ;
end;

//------------------------------------------------------------------------------
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
                  FLabel.Top     := (ClientHeight-FLabel.Height) div 2
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
                  FLabel.Top     := (ClientHeight-FLabel.Height) div 2
                else
                  FLabel.Top     := 1 ;
              end ;
  else
    raise exception.CreateFmt(
      'Invalid label style. Called in %s.%s',
      [ClassName, 'PositionLabel'] ) ;
  end ;


end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.PositionWinControl ;
begin

  case LabelStyle of
  lsNone    : begin
                FWinControl.Top    := 1 ;
                FWinControl.Left   := 1 ;
                FWinControl.Height := ClientHeight-2 ;
                FWinControl.Width  := ClientWidth-2  ;
              end ;
  lsTop     : begin
                FWinControl.Top    := FLabel.Top + FLabel.Height + 2 ;
                FWinControl.Left   := 1 ;
                FWinControl.Height := ClientHeight - FWinControl.Top - 2 ;
                FWinControl.Width  := ClientWidth  - FWinControl.Left - 2  ;
              end ;
  lsLeft    : begin
                FWinControl.Top    := 1 ;
                FWinControl.Left   := LabelWidth + 3;
                FWinControl.Height := ClientHeight - FWinControl.Top - 2;
                FWinControl.Width  := ClientWidth  - FWinControl.Left - 2 ;
              end ;
  lsTopLeft : begin
                FWinControl.Top    := FLabel.Top + FLabel.Height + 3;
                FWinControl.Left   := 24 ;
                FWinControl.Height := ClientHeight - FWinControl.Top - 2 ;
                FWinControl.Width  := ClientWidth  - FWinControl.Left - 2 ;
              end ;
  lsRight   : begin
                FWinControl.Top    := 1 ;
                FWinControl.Left   := 1 ;
                FWinControl.Height := ClientHeight - FWinControl.Top - 2;
                FWinControl.Width  := LabelWidth ;
              end ;
  else
    raise exception.CreateFmt(
      'Invalid label style. Called in %s.%s',
      [ClassName, 'PositionWinControl'] ) ;
  end ;

end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetCaption(const Value: TCaption);
begin
  FLabel.Caption := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetLabelStyle(const Value: TLabelStyle);
begin
  FLabelStyle := Value;
  PositionLabel ;
  PositionWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetLabelWidth(const Value: Integer);
begin
  FiLabelWidth := Value ;
  FLabel.Width := Value ;
  PositionLabel ;
  PositionWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetEnabled( Value: Boolean);
begin
  inherited SetEnabled( Value ) ;
  FWinControl.Enabled := Value ;
  SetControlColor ;
  FWinControl.Refresh ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.WMSize(var Message: TWMSize);
begin
  PositionLabel ;
  PositionWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetData(const Value: TPersistent);
begin
  FbDirty := false ;
  FData := Value;
  DataToWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetFieldName(const Value: string);
begin
  FsFieldName := Value;
  DataToWinControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.DoChange(Sender: TObject);
begin
  FbDirty := true ;
  WinControlToData ;
  if Assigned( FOnChange ) then
    FOnChange( self ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareAbs.DataAndPropertyValid: boolean;
begin

  result :=
    ( FData <> nil ) and
    ( FsFieldName <> '' ) ;
  if not result then
    Exit ; //==>

  result := ( IsPublishedProp( FData, FsFieldName )) ;

  if not result then
    raise exception.createFmt( '<%s> is not a property of <%s>',
                               [FsFieldName, FData.ClassName ]) ;

  ReadOnly := ReadOnly or IsPropReadOnly ;

end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.LinkToData(pData: TPersistent;const pFieldName: string);
begin
  Data := pData ;
  FieldName := pFieldName ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.DoOnClick( Sender : TObject );
begin
  if ( Enabled ) and ( FWinControl.Enabled ) then
    FWinControl.SetFocus
  else
    SetFocus ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.DoOnKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned( FOnKeyPress ) then
    FOnKeyPress( Sender, Key ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.DoOnKeyDown(Sender : TObject ; var Key: Word; Shift: TShiftState );
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( Sender, Key, Shift ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetReadOnly(const Value: Boolean);
begin
  FbReadOnly := Value;
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
    ShowFocusRect := False ;       // <<<=== PH 26/10/2003
  FWinControl.TabStop := not FbReadOnly;  // <<<=== IK 05/12/2001
  SetControlColor ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetControlColor;
begin
  // the control is in error
  if Error then
  begin
    FLabel.Font.Color := clBlack ;
    FWinControl.Brush.Color := FErrorColor ;
    FWinControl.Refresh ;
    Exit ; //==>
  end ;

  // control is read only
  if ReadOnly and GreyWhenReadOnly then
  begin
    FLabel.Font.Color := clBlack ;
    FWinControl.Brush.Color := clBtnFace ;
    FWinControl.Refresh ;
    Exit ; //==>
  end ;

  // the control is not enabled.
  if not Enabled then
  begin
    FLabel.Font.Color := clGray ;
    FWinControl.Brush.Color := clBtnFace ;
    FWinControl.Refresh ;
    Exit ; //==>
  end ;

  FLabel.Font.Color := clBlack ;
  FWinControl.Brush.Color := clWindow ;
  FWinControl.Refresh ;

end;

procedure TtiPerAwareAbs.SetError(const Value: boolean);
begin
  FbError := Value;
  SetControlColor ;
end;

procedure TtiPerAwareAbs.SetErrorColor(const Value: TColor);
begin
  FErrorColor := Value;
  SetControlColor ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareAbs.IsPropReadOnly: boolean;
var
  lPropInfo : PPropInfo ;
begin
  lPropInfo := GetPropInfo( FData, FsFieldName ) ;
  result    := ( lPropInfo.SetProc = nil ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareAbs.Focused: Boolean;
begin
  result := ( inherited Focused ) or
            ( FWinControl.Focused ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareEdit.Create(Owner: TComponent);
begin
  FWinControl := TEdit.Create( self ) ;
  TEdit( FWinControl ).OnChange   := DoChange ;
  TEdit( FWinControl ).OnKeyPress := DoOnKeyPress ;
  TEdit( FWinControl ).OnKeyDown  := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetValue: String;
begin
  result := TEdit( FWinControl ).Text ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetValue(const Value: String);
begin
  SetOnChangeActive( false ) ;
  TEdit( FWinControl ).Text := Value ;
  WinControlToData ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TEdit( FWinControl ).Text := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TEdit( FWinControl ).Text ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TEdit( FWinControl ).OnChange := DoChange
  else
    TEdit( FWinControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TEdit( FWinControl ).ReadOnly := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetMaxLength: integer;
begin
  result := TEdit( FWinControl ).MaxLength ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetMaxLength(const Value: integer);
begin
  TEdit( FWinControl ).MaxLength := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetCharCase: TEditCharCase;
begin
  result := TEdit( FWinControl ).CharCase ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetCharCase(const Value: TEditCharCase);
begin
  TEdit( FWinControl ).CharCase := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetPasswordChar: Char;
begin
  result := TEdit( FWinControl ).PasswordChar ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetPasswordChar(const Value: Char);
begin
  TEdit( FWinControl ).PasswordChar := Value ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareMemo
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareMemo.Create(Owner: TComponent);
begin
  FWinControl := TMemo.Create( self ) ;
  TMemo( FWinControl ).OnChange := DoChange ;
  TMemo( FWinControl ).OnKeyPress := DoOnKeyPress ;
  TMemo( FWinControl ).OnKeyDown := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := false ;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TMemo( FWinControl ).Lines.Text := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetMaxLength: integer;
begin
  result := TMemo( FWinControl ).MaxLength ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetScrollBars: TScrollStyle;
begin
  result := TMemo( FWinControl ).ScrollBars ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetValue: string;
begin
  result := TMemo( FWinControl ).Lines.Text ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetWordWrap: boolean;
begin
  result := TMemo( FWinControl ).WordWrap ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetMaxLength(const Value: integer);
begin
  TMemo( FWinControl ).MaxLength := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TMemo( FWinControl ).OnChange := DoChange
  else
    TMemo( FWinControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TMemo( FWinControl ).ReadOnly := Value ;
end;

procedure TtiPerAwareMemo.SetScrollBars(const Value: TScrollStyle);
begin
  TMemo( FWinControl ).ScrollBars := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetValue(const Value: string);
begin
  SetOnChangeActive( false ) ;
  TMemo( FWinControl ).Lines.Text := Value ;
  WinControlToData ;
  SetOnChangeActive( false ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetWordWrap(const Value: boolean);
begin
  TMemo( FWinControl ).WordWrap := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TMemo( FWinControl ).Lines.Text ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareDateTimePicker
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareDateTimePicker.Create(Owner: TComponent);
begin
  FWinControl := TDateTimePicker.Create( self ) ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TDateTimePicker( FWinControl ).DateTime :=
    Trunc( GetPropValue( FData, FsFieldName )) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.DoKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN] then
    DoChange( Sender ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.DoOnExit(Sender: TObject);
begin
  DoChange( Sender ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareDateTimePicker.GetValue: TDateTime;
begin
  result := TDateTimePicker( FWinControl ).Date ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetOnChangeActive(Value: boolean);
begin
  if Value then
  begin
    TDateTimePicker( FWinControl ).OnCloseUp  := DoChange ;
    TDateTimePicker( FWinControl ).OnExit     := DoOnExit ;
    TDateTimePicker( FWinControl ).OnKeyUp    := DoKeyUp    ;
  end
  else
  begin
    TDateTimePicker( FWinControl ).OnCloseUp := nil ;
    TDateTimePicker( FWinControl ).OnExit    := nil ;
    TDateTimePicker( FWinControl ).OnKeyUp   := nil ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  if Value then
    TDateTimePicker( FWinControl ).Enabled := false
  else
    TDateTimePicker( FWinControl ).Enabled := Enabled ;
  SetControlColor;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetValue(const Value: TDateTime);
begin
  SetOnChangeActive( false ) ;
  TDateTimePicker( FWinControl ).Date := trunc( Value ) ;
  WinControlToData ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.WinControlToData;
var
  liValue : integer ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  liValue := Trunc( TDateTimePicker( FWinControl ).DateTime ) ;

  SetPropValue( FData, FsFieldName, liValue ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareDateTimePicker.GetMaxDate: TDateTime;
begin
  result := TDateTimePicker( FWinControl ).MaxDate ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareDateTimePicker.GetMinDate: TDateTime;
begin
  result := TDateTimePicker( FWinControl ).MinDate ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetMaxDate(const Value: TDateTime);
begin
  TDateTimePicker( FWinControl ).MaxDate := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetMinDate(const Value: TDateTime);
begin
  TDateTimePicker( FWinControl ).MinDate := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareDateTimePicker.SetControlColor;
begin
  inherited ;
  if Enabled and (not ReadOnly) and (not GreyWhenReadOnly)then
    TDateTimePicker( FWinControl ).Color := clWindow
  else
    TDateTimePicker( FWinControl ).Color := clBtnFace ;
end;

// -----------------------------------------------------------------------------
{
procedure TtiPerAwareDateTimePicker.DoOnClick(Sender: TObject);
begin
  // Only call inherited if not ReadOnly because the comboBox will have been
  // disabled if it is ReadOnly.
  if not ReadOnly then
    Inherited DoOnClick( Sender ) ;
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareCheckBox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareCheckBox.Create(Owner: TComponent);
begin
  FWinControl := TCheckBox.Create( self ) ;
  TCheckBox( FWinControl ).Caption := '' ;
  TCheckBox( FWinControl ).OnClick := DoChange ;
  TCheckBox( FWinControl ).Width := 13 ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  FLabel.OnClick := DoLabelClick ;
  Height := 17 ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TCheckBox( FWinControl ).Checked := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareCheckBox.GetValue: boolean;
begin
  result := TCheckBox( FWinControl ).Checked ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetControlColor;
var
  lBrushColor : TColor ;
begin
  lBrushColor := FWinControl.Brush.Color ;
  inherited SetControlColor ;
  FWinControl.Brush.Color := lBrushColor ;
  FWinControl.Refresh ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TCheckBox( FWinControl ).OnClick := DoChange
  else
    TCheckBox( FWinControl ).OnClick := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  if Value then
    TCheckBox( FWinControl ).Enabled := False
  else
    TCheckBox( FWinControl ).Enabled := Enabled ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.DoOnClick(Sender: TObject);
begin
  Inherited DoOnClick( Sender ) ;
  DoLabelClick(nil);
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetValue(const Value: boolean);
begin
  SetOnChangeActive( false ) ;
  TCheckBox( FWinControl ).Checked := Value ;
  WinControlToData ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.WinControlToData;
var
  li : Integer ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  li := Ord( TCheckBox( FWinControl ).Checked ) ;
  SetPropValue( FData, FsFieldName, li ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareFloatEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareFloatEdit.create( owner : TComponent ) ;
begin
  FWinControl := TEdit.Create( self ) ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;

  TEdit( FWinControl ).OnEnter      := _DoEnter ;
  TEdit( FWinControl ).OnExit       := _DoExit ;
  TEdit( FWinControl ).OnKeyPress   := _DoKeyPress ;
  TEdit( FWinControl ).OnChange     := _DoChange ;
  TEdit( FWinControl ).OnClick      := _DoClick ;

  FsEditMask   := cFloatEditMask ;
  FsTextBefore := '' ;
  FsTextAfter  := '' ;
  FrMinValue   := 0 ;
  FrMaxValue   := 0 ;
  FrUnknownValue   := -1 ;
  FsBeforeApplyKey := '' ;
  Precision := 0 ;
  Value := 0 ;

end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setValueAsString( sValue : string ) ;
begin
  if sValue = '' then begin
    Value := 0 ;
    exit ;
  end ;

  if not isValidFloat( sValue ) then begin
    Value := 0 ;
    exit ;
  end ;

  SetOnChangeActive( false ) ;
  TEdit( FWinControl ).text := sValue ;
  Value := Value ;
  SetOnChangeActive( true ) ;

end ;

//------------------------------------------------------------------------------
function  TtiPerAwareFloatEdit.getValueAsString : string ;
begin
  result := TEdit( FWinControl ).text ;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.isValidFloat( sValue : string ) : boolean ;
var rValue : real ;
begin
  try
    rValue := strToFloat( RemoveFormatChr( sValue )) ;
    if rValue < rValue + 1 then ; // To trick compiler warnings
    result := true ;
  except
    result := false ;
  end ;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.getValue : real ;
var lStr : string ;
begin
  lStr := TEdit( FWinControl ).text ;
  if (FsTextUnknown <> '')
  and SameText(lStr, FsTextUnknown) then
    result := FrUnknownValue
  else
    result := CustomStrToFloat( lStr ) ;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.customStrToFloat( var pStrValue : string ) : real ;
var lStrValue : string ;
begin
  lStrValue := RemoveFormatChr( pStrValue ) ;
  if lStrValue = '' then begin
    result := 0 ;
    exit ; //==>
  end ;

  try
    result := strToFloat( lStrValue ) ;
  except
    result := 0 ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setValue( rValue : real ) ;
var sValue : string ;
    sMinValue : string ;
    sMaxValue : string ;
begin
  SetOnChangeActive( false ) ;

  if  (FrUnknownValue <> 0)
  and (rValue = FrUnknownValue) then
    sValue := FsTextUnknown
  else
    sValue := FsTextBefore + formatFloat( FsEditMask, rValue ) + FsTextAfter ;

  if not WithinMinMaxLimits( rValue ) then begin
    sMinValue := FsTextBefore + formatFloat( FsEditMask, FrMinValue ) + FsTextAfter ;
    sMaxValue := FsTextBefore + formatFloat( FsEditMask, FrMaxValue ) + FsTextAfter ;
    raise ERangeError.create( 'The value you entered, ' + sValue +
                                 ' is out of range.' + #13 +
                                 'Please enter a value between ' +
                                 sMinValue + ' and ' +
                                 sMaxValue ) ;
  end ;
  TEdit( FWinControl ).text := sValue ;
  WinControlToData ;
  SetOnChangeActive( true ) ;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.withinMinMaxLimits( value : real ) : boolean ;
begin
  result := not ((( FrMinValue <> 0 ) and ( value < FrMinValue )) or
                 (( FrMaxValue <> 0 ) and ( value > FrMaxValue ))) ;
  // What if one of our FsM??Values are 0 ?
  // Require some code to handle these situations
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setMinValue( rValue : real ) ;
begin
  if (FrMaxValue <> 0 ) and (rValue >= FrMaxValue) then rValue := 0 ;
  FrMinValue := rValue ;
  Value := Value ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setMaxValue( rValue : real ) ;
begin
  if (FrMinValue <> 0) and (rValue <= FrMinValue) then rValue := 0 ;
  FrMaxValue := rValue ;
  Value := Value ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.SetUnknownValue(const rValue: real);
begin
  FrUnknownValue := rValue;
  Value := rValue ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoEnter( sender : TObject ) ;
var
  lSaveOnChange : TNotifyEvent ;
begin
  if ReadOnly then
  begin
    TEdit( FWinControl ).selectAll ;
    Exit ; //==>
  end ;

  lSaveOnChange := FOnChange ;
  FOnChange := nil ;

  if (FsTextUnknown <> '')
  and SameText(TEdit( FWinControl ).text, FsTextUnknown) then
    TEdit( FWinControl ).text := FloatToStr( FrUnknownValue )
  else
    TEdit( FWinControl ).text := RemoveFormatChr( TEdit( FWinControl ).text ) ;
    
  TEdit( FWinControl ).selectAll ;
  FOnChange := lSaveOnChange ;

end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoExit( sender : TObject ) ;
var
  rValue : real ;
  lSaveOnChange : TNotifyEvent ;
begin

  if ReadOnly then
    Exit ; //==============================================> ReadOnly

  lSaveOnChange := FOnChange ;
  FOnChange := nil ;
  try
    Value := Value ;
  except
    on e : ERangeError do begin
      messageDlg( e.message, mtError,
                  [mbOK], 0 ) ;
      TEdit( FWinControl ).SetFocus ;
    end else begin
      TEdit( FWinControl ).setFocus ;
      raise ;
    end ;
  end ;
  rValue := Value ;
  if rValue <> Value then Value := rValue ;
  FOnChange := lSaveOnChange ;

end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.removeFormatChr( sValue : string ) : string ;
var i : integer ;
begin
  result := '' ;
  for i := 1 to length( sValue ) do begin
    if sValue[i] in cValidFloatChrs then begin
      result := result + sValue[i] ;
    end ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setPrecision( iValue : integer ) ;
var
  i : integer ;
  lFrac : string ;
begin
  if FiPrecision <> iValue then              //ipk 2001-03-01
  begin
    FiPrecision := iValue ;
    FFloatEditStyle := fesUser ;
  end;                                       //ipk 2001-03-01

  if FFloatEditStyle = fesInteger then       //ipk 2001-03-01
    FsEditMask := cIntEditMask               //ipk 2001-03-01
  else                                       //ipk 2001-03-01
    FsEditMask := cFloatEditMask;

  if FiPrecision > 0 then
  begin
    if Pos( FsEditMask, DecimalSeparator ) <> 0 then
      FsEditMask := Copy( FsEditMask, 1, Pos( FsEditMask, '.' ) - 1 ) ;
    lFrac := '' ;
    for i := 1 to FiPrecision do
      lFrac := lFrac + '0' ;
    FsEditMask := FsEditMask + DecimalSeparator + lFrac ;
  end ;
  Value := Value ;

end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoKeyPress(Sender: TObject;var Key: Char);
begin

  FsBeforeApplyKey := TEdit( FWinControl ).text ;

  // A non character key?
  if ( ord( key ) < 32 ) or ( ord( key ) > 132 ) then begin
    exit ;
  end ;

  // A numeric key?
  if not ( key in cValidFloatChrs ) then begin
    key := char( 0 ) ;
  end ;

end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoChange( sender : TObject ) ;
var lReal : real ;
    lIntPos : integer ;
begin
  lReal := Value ;
  if not WithinMinMaxLimits( lReal ) then begin
    lIntPos := TEdit( FWinControl ).selStart ;
    TEdit( FWinControl ).text := FsBeforeApplyKey ;
    TEdit( FWinControl ).selStart := lIntPos ;
    Exit ; //==>
  end ;
  WinControlToData ;
  if Assigned( FOnChange ) then
    FOnChange( self ) ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setTextAfter( sValue : string ) ;
begin
  if FsTextAfter <> sValue then         //ipk 2001-03-01
  begin
    FsTextAfter := sValue ;
    FFloatEditStyle := fesUser ;
  end;                                  //ipk 2001-03-01
  Value := Value ;

//  FsTextAfter := sValue ;
//  FFloatEditStyle := fesUser ;
//  Value := Value ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setTextBefore( sValue : string ) ;
begin
  if FsTextBefore <> sValue then        //ipk 2001-03-01
  begin
    FsTextBefore := sValue ;
    FFloatEditStyle := fesUser ;
  end;                                  //ipk 2001-03-01
  Value := Value ;
//  FsTextBefore := sValue ;
//  FFloatEditStyle := fesUser ;
//  Value := Value ;
end ;

// -----------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setTextUnknown(const sValue: string);
begin
  FsTextUnknown := sValue;
  Value := Value ;
end;


// -----------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoClick( sender : TObject ) ;
begin
  TEdit( FWinControl ).SelectAll ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.SetFloatEditStyle(const Value: TtiFloatEditStyle);
begin
  FFloatEditStyle := Value;
  case Value of
  fesUser     : ;// Do nothing
  fesInteger  : begin
                  TextBefore := '' ;
                  Precision  :=  0 ;
                  TextAfter  := '' ;
                end ;
  fesFloat    : begin
                  TextBefore := '' ;
                  Precision  :=  3 ;
                  TextAfter  := '' ;
                end ;
  fesCurrency : begin
                  TextBefore := '$ ' ;
                  Precision  :=  2 ;
                  TextAfter  := '' ;
                end ;
  fesPercent  : begin
                  TextBefore := '' ;
                  Precision  :=  1 ;
                  TextAfter  := ' %' ;
                end ;
  else
    raise exception.CreateFmt(
      'Invalid TtiPerAwareFloatEditStyle passed to %s.SetFloatEditStyle',
      [ClassName]);

  end ;
  FFloatEditStyle := Value; //ipk 2001-03-01  Likely to be changed to fesUser
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  Value := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TEdit( FWinControl ).OnChange := _DoChange
  else
    TEdit( FWinControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TEdit( FWinControl ).ReadOnly := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, Value ) ;
end;

function TtiPerAwareFloatEdit.GetIsKnown: boolean;
begin
  result := Value <> UnknownValue ;
end;

procedure TtiPerAwareFloatEdit.SetIsKnown(const bValue: boolean);
begin
  if bValue then
    Value := 0  // assumes 0 is not = FrUnknownValue
  else
    Value := FrUnknownValue ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareImageEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareImageEdit.Create(Owner: TComponent);
var
  i : integer ;
  lLastControl : TControl ;
begin
  FWinControl := TPanel.Create( Self ) ;
  FbCenterWhenLabelIsLeft := false ;
  inherited;
  TPanel( FWinControl ).BevelInner  := bvNone   ; //bvRaised ;
  TPanel( FWinControl ).BevelOuter  := bvNone   ;
  TPanel( FWinControl ).BorderStyle := bsNone   ;

  FScrollBox  := TScrollBox.Create( Self ) ;
  FScrollBox.Parent := FWinControl ;
  FScrollBox.Top    := 16 ;
  FScrollBox.Left   := 0 ;
  FScrollBox.Color  := clWindow ;
  FScrollBars       := ssBoth ;

  FImage        := TImage.Create( Self ) ;
  FImage.Parent := FScrollBox ;
  FImage.Top  := 0 ;
  FImage.Left := 0 ;
  FImage.AutoSize := true ;

  FbtnLoadFromFile        := TSpeedButton.Create( Self ) ;
  with FbtnLoadFromFile do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := 0 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'Load from file' ;
    ShowHint := true ;
    OnClick  := DoLoadFromFile ;
    Glyph.LoadFromResourceName( HInstance, 'PAILOADFROMFILE' ) ;
  end ;
  lLastControl := FbtnLoadFromFile ;

  FbtnSaveToFile          := TSpeedButton.Create( Self ) ;
  with FbtnSaveToFile do
  begin
    Parent   := FWinControl ;
    Top      := 0 ;
    Left     := lLastControl.Left + lLastControl.Width + 4 ;
    Height   := 12 ;
    Width    := 12 ;
    Hint     := 'Save to file' ;
    ShowHint := true ;
    OnClick  := DoSaveToFile ;
    Glyph.LoadFromResourceName( HInstance, 'paiSaveToFile' ) ;
  end ;
  lLastControl := FbtnSaveToFile ;

  FBtnCopyToClip          := TSpeedButton.Create( Self ) ;
  with FbtnCopyToClip do
  begin
    Parent   := FWinControl ;
    Top      := 0 ;
    Left     := lLastControl.Left + lLastControl.Width + 4 ;
    Height   := 12 ;
    Width    := 12 ;
    Hint     := 'Copy to clipboard' ;
    ShowHint := true ;
    OnClick  := DoCopyToClip ;
    Glyph.LoadFromResourceName( HInstance, 'paiCopyToClipBoard' ) ;
  end ;
  lLastControl := FbtnCopyToClip ;

  FbtnPasteFromClip        := TSpeedButton.Create( Self ) ;
  with FbtnPasteFromClip do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := lLastControl.Left + lLastControl.Width + 4 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'Paste from clipboard' ;
    ShowHint := true ;
    OnClick := DoPasteFromClip ;
    Glyph.LoadFromResourceName( HInstance, 'paiPasteFromClipboard' ) ;
  end ;
  lLastControl := FbtnPasteFromClip ;

{
  FbtnViewFullScreen := TSpeedButton.Create( Self ) ;
  with FbtnViewFullScreen do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := lLastControl.Left + lLastControl.Width + 4 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'View full screen' ;
    ShowHint := true ;
    OnClick := DoViewFullScreen ;
    Glyph.LoadFromResourceName( HInstance, 'paiViewFullScreen' ) ;
  end ;
  lLastControl := FbtnViewFullScreen ;
}

  FbtnEdit           := TSpeedButton.Create( Self ) ;
  with FbtnEdit do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := lLastControl.Left + lLastControl.Width + 4 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'Edit' ;
    ShowHint := true ;
    OnClick := DoEdit ;
    Glyph.LoadFromResourceName( HInstance, 'paiEdit' ) ;
  end ;
  lLastControl := FbtnEdit ;

  FbtnClear           := TSpeedButton.Create( Self ) ;
  with FbtnClear do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := lLastControl.Left + lLastControl.Width + 4 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'Clear' ;
    ShowHint := true ;
    OnClick := DoClear ;
    Glyph.LoadFromResourceName( HInstance, 'paiClear' ) ;
  end ;
  lLastControl := FbtnClear ;

  FbtnStretch        := TSpeedButton.Create( Self ) ;
  with FbtnStretch do
  begin
    Parent := FWinControl ;
    Top    := 0 ;
    Left   := lLastControl.Left + lLastControl.Width + 4 ;
    Height := 12 ;
    Width  := 12 ;
    Hint   := 'Stretch' ;
    ShowHint := true ;
    OnClick := DoStretch ;
    Glyph.LoadFromResourceName( HInstance, 'paiStretch' ) ;
  end ;

  if not( csDesigning in ComponentState ) then
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TSpeedButton then
        TSpeedButton( Components[i] ).Flat := true ;

  FFilter := GraphicFilter( TGraphic ) ;
  FsFileName := '' ;

end;

procedure TtiPerAwareImageEdit.DataToWinControl;
var
  lData : TObject ;
begin
  if not DataAndPropertyValid then
  begin
    FImage.Picture.Assign(nil);
    Exit ; //==>
  end;
  lData := GetObjectProp( FData, FsFieldName ) ;
  if not ( lData is TGraphic ) then
    raise exception.CreateFmt(
      'Property %s.%s is not of type TGraphic',
      [FData.ClassName, FsFieldName] ) ;

  FImage.Picture.Assign( TGraphic( lData )) ;

end;

procedure TtiPerAwareImageEdit.DoPasteFromClip(Sender: TObject);
begin
  FImage.Picture.Assign( Clipboard ) ;
  DoChange( self ) ;
end;

procedure TtiPerAwareImageEdit.DoLoadFromFile(Sender: TObject);
var
  lSD : TOpenPictureDialog ;
  lDefaultAction : boolean ;
begin
  lSD := TOpenPictureDialog.Create( nil ) ;
  try
    lSD.Filter := FFilter ;
    lSD.FileName := FsFileName ;
    if FsFileName = '' then
      lSD.InitialDir := FsInitialDir ;
    lSD.DefaultExt := cusImageDefaultExt ;
    if lSD.Execute then
    begin
      FsFileName := lSD.FileName ;
      lDefaultAction := true ;
      if Assigned( FOnLoadFromFile ) then
        FOnLoadFromFile( Self, FsFileName, lDefaultAction ) ;
      if lDefaultAction then
      begin
        FImage.Picture.LoadFromFile( FsFileName ) ;
        DoChange( self ) ;
      end ;
      FsInitialDir := ExtractFilePath( lSD.FileName ) ;
    end;
  finally
    lSD.Free ;
  end;
end;

procedure TtiPerAwareImageEdit.DoCopyToClip(Sender: TObject);
begin
  Clipboard.Assign( FImage.Picture ) ;
end;

procedure TtiPerAwareImageEdit.DoSaveToFile(Sender: TObject);
var
  lSD : TSavePictureDialog ;
begin
  lSD := TSavePictureDialog.Create( nil ) ;
  try
    lSD.Filter     := GraphicFilter( TGraphic ) ;
    lSD.FileName   := FsFileName ;
    lSD.InitialDir := FsInitialDir ;
    lSD.DefaultExt := cusImageDefaultExt ;
    if lSD.Execute then
    begin
      FsFileName := lSD.FileName ;
      FImage.Picture.SaveToFile( FsFileName ) ;
      FsInitialDir := ExtractFilePath( FsFileName ) ;
      if MessageDlg( 'Do you want to edit this file now?',
                     mtConfirmation,
                     [mbYes, mbNo],
                     0 ) = mrYes then
         ShellExecute( screen.activeForm.handle,
                       nil,
                       PChar( FsFileName ),
                       nil,
                       nil,
                       SW_SHOWNORMAL ) ;
    end;
  finally
    lSD.Free ;
  end;
end;

function TtiPerAwareImageEdit.GetStretch: boolean;
begin
  result := FImage.Stretch ;
end;

function TtiPerAwareImageEdit.GetValue: TPicture;
begin
  result := FImage.Picture ;
end;

procedure TtiPerAwareImageEdit.PositionWinControl;
begin
  inherited;
  FScrollBox.Height := FWinControl.ClientHeight - FScrollBox.Top ;
  FScrollBox.Width  := FWinControl.ClientWidth  ;
end;

procedure TtiPerAwareImageEdit.SetControlColor;
begin
  Inherited ;
  FScrollBox.Color          := FWinControl.Brush.Color ;
  FbtnLoadFromFile.Enabled  := FbtnLoadFromFile.Visible  and Enabled and Not ReadOnly ;
  FbtnSaveToFile.Enabled    := FbtnSaveToFile.Visible    and Enabled and Not ReadOnly ;
  FbtnPasteFromClip.Enabled := FbtnPasteFromClip.Visible and Enabled and Not ReadOnly ;
  FBtnCopyToClip.Enabled    := FBtnCopyToClip.Visible    and Enabled and Not ReadOnly ;
  FbtnEdit.Enabled          := FbtnEdit.Visible          and Enabled and Not ReadOnly and Assigned( FOnEdit ) ;
  FbtnClear.Enabled         := FbtnClear.Visible         and Enabled and Not ReadOnly ;
end;

procedure TtiPerAwareImageEdit.SetOnChangeActive(Value: boolean);
begin
  // Do nothing
end;

procedure TtiPerAwareImageEdit.SetScrollBars(const Value: TScrollStyle);
begin
  FScrollBars := Value ;
  case Value of
  ssNone       : begin
                   FScrollBox.VertScrollBar.Visible := false ;
                   FScrollBox.HorzScrollBar.Visible := false ;
                 end ;
  ssHorizontal : begin
                   FScrollBox.VertScrollBar.Visible := false ;
                   FScrollBox.HorzScrollBar.Visible := true ;
                 end ;
  ssVertical   : begin
                   FScrollBox.VertScrollBar.Visible := true ;
                   FScrollBox.HorzScrollBar.Visible := false ;
                 end ;
  ssBoth       : begin
                   FScrollBox.VertScrollBar.Visible := true ;
                   FScrollBox.HorzScrollBar.Visible := true ;
                 end ;
  else
    raise exception.create( 'Invalid ScrollBar type passed to ' + ClassName +
                            '.SetScrollBars' ) ;
  end ;

end;

procedure TtiPerAwareImageEdit.SetStretch(const Value: boolean);
begin
  FImage.Stretch  := Value ;
  FImage.AutoSize := not Value ;
  FbtnStretch.Down := Value ;
  if Value then
    FImage.Align  := alClient
  else
    FImage.Align  := alNone ;
end;

procedure TtiPerAwareImageEdit.SetValue(const Value: TPicture);
begin
  FImage.Picture.Assign( Value ) ;
end;

procedure TtiPerAwareImageEdit.WinControlToData;
var
  lData   : TObject ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>

  if ReadOnly then
    Exit ; //==>

  lData := GetObjectProp( FData, FsFieldName ) ;
  if not ( lData is TGraphic ) then
    raise exception.CreateFmt(
      'Property %s.%s is not of type TGraphic',
      [FData.ClassName, FsFieldName] ) ;
  TGraphic( lData ).Assign( FImage.Picture.Graphic ) ;

end;

// This will always edit the image as a bitmap - which is probably not
// what we want. Better come up with some way of doing this so we can edit
// any sort of file with the default editor for that file type.
procedure TtiPerAwareImageEdit.DoEdit(Sender: TObject);
begin
  Assert( Assigned( FOnEdit ), 'FOnEdit not assigned' ) ;
  FOnEdit( Self )
end;

procedure TtiPerAwareImageEdit.DoClear(Sender: TObject);
begin
  FImage.Picture.Assign( nil ) ;
  DoChange( self ) ;
end;

procedure TtiPerAwareImageEdit.DoStretch(Sender: TObject);
begin
  Stretch := Not Stretch ;
end;

{
procedure TtiPerAwareImageEdit.DoViewFullScreen(Sender: TObject);
var
  lsFileName : TFileName ;
begin
  lsFileName := GetTempFileName ;
  FImage.Picture.SaveToFile( lsFileName ) ;
  try
    EditFile( lsFileName ) ;
    Sleep( 1000 ) ;
  finally
    DeleteFile( lsFileName ) ;
  end;
end;}

{
function TtiPerAwareImageEdit.GetTempFileName: TFileName;
const
  cMaxPathLen = 255 ;
var
  pcTemp : array[0..cMaxPathLen] of char ;
  pcApp  : array[0..cMaxPathLen] of char ;
  pcPath : array[0..cMaxPathLen] of char ;
begin
  strPCopy( pcApp, copy( extractFileName( application.exeName ), 1, 3 ) ) ;
  getTempPath( cMaxPathLen, pcPath ) ;
  Windows.getTempFileName( pcPath, pcApp, 0, pcTemp ) ;
  result := strPas( pcTemp ) ;
  DeleteFile( result ) ;
  result := ChangeFileExt( result, '.BMP' ) ;
  if FileExists( result ) then
    DeleteFile( result ) ;
end;
}

{
procedure TtiPerAwareImageEdit.EditFile(const pFileName: TFileName);
var
  lForm      : TtiPerAwareImageEditForm ;
begin
  lForm := TtiPerAwareImageEditForm.CreateNew( nil ) ;
  try
    lForm.Execute( pFileName ) ;
  finally
    lForm.Free ;
  end ;
end;
}

//------------------------------------------------------------------------------
function TtiPerAwareImageEdit.IsPropReadOnly: boolean;
begin
  // This is necessary because IsPropReadOnly in the abstract TtiPerAware will
  // check that the property has a Set method as well as a Get method. It is
  // possible for an image property to have a Get method only, so
  // IsPropReadOnly would return true, which is not what we want in this case.
  result := false ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareImageEditForm
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
constructor TtiPerAwareImageEditForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited;
  Caption := ' Edit an image' ;
  Width := Screen.Width div 2 ;
  Height := Screen.Height div 2 ;
  Position := poScreenCenter ;
  OnShow := DoOnShow ;
  OnClose := DoOnClose ;

  FMenu := TMainMenu.Create( Self ) ;

  FOleContainer := TOleContainer.Create( self ) ;
  with FOleContainer do
  begin
    Parent := Self ;
    Align := alClient ;
    AutoActivate := aaManual ;
  end ;
end;

procedure TtiPerAwareImageEditForm.DoOnClose(Sender: TObject ; Var Action : TCloseAction);
begin
end;

procedure TtiPerAwareImageEditForm.DoOnShow(Sender: TObject);
begin
  FOleContainer.DoVerb( ovShow ) ;
end;

function TtiPerAwareImageEditForm.Execute(const pFileName: TFileName): boolean ;
begin
  FFileName := pFileName ;
  FOleContainer.CreateObjectFromFile( FFileName, False );
  result := ShowModal = mrOK ;
  // I can't make this work reliably. Got any ideas?
  FOleContainer.SaveAsDocument( FFileName ) ;
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxStatic
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiPerAwareComboBoxStatic.DataToWinControl;
var
  lsValue : string ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  lsValue := GetPropValue( FData, FsFieldName ) ;
  TComboBox( FWinControl ).ItemIndex :=
    TComboBox( FWinControl ).Items.IndexOf( lsValue ) ;
  SetOnChangeActive( true ) ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxStatic.GetItems: TStrings;
begin
  result := TComboBox( FWinControl ).Items ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxStatic.GetValue: String;
begin
  result := TComboBox( FWinControl ).Text ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.SetItems(const Value: TStrings);
begin
  TComboBox( FWinControl ).Items.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.SetValue(const Value: String);
begin
  SetOnChangeActive( false ) ;
  TComboBox( FWinControl ).ItemIndex :=
    TComboBox( FWinControl ).Items.IndexOf( Value ) ;
  WinControlToData ;
  SetOnChangeActive( false ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TComboBox( FWinControl ).Text ) ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxDynamic
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiPerAwareComboBoxDynamic.Refresh ;
var
  lItems: TStrings;
  I: Integer;
begin
  lItems:= TComboBox( WinControl ).Items ;
  lItems.Clear;

  if (FList = nil) or
     (FList.Count < 1) or
     (SameText( FsFieldNameDisplay, EmptyStr )) then
    Exit ; //==>

  try
    for I := 0 to FList.Count - 1 do
      lItems.AddObject( GetPropValue( FList.Items [ I ], FsFieldNameDisplay),
                        FList.Items[ I ]);
  except
    on e:exception do
      raise Exception.CreateFmt( 'Error adding list items to combobox ' +
                                 'Message: %s, Item Property Name: %s',
                                 [e.message,
                                  FsFieldNameDisplay] ) ;

  end ;

  inherited ;

end ;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.DataToWinControl;
var
  lValue : TPersistent ;
  lPropType : TTypeKind ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  lPropType := PropType( Data, FieldName ) ;
  if lPropType = tkClass then
  begin
    lValue := TPersistent(GetObjectProp( Data, FieldName )) ;
  end else if (lPropType = tkString) or (lPropType = tkLString) then
  begin
    Assert( Assigned( FOnGetObjectProp ), 'OnGetObjectProp not assigned' ) ;
    lValue := nil ;
    FOnGetObjectProp(lValue);
  end else
    raise Exception.Create( sPropTypeNotClassOrString ) ;

  SetValue ( lValue ) ;

  SetOnChangeActive( true ) ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxDynamic.GetValue: TPersistent;
begin
  with TComboBox( WinControl ) do
    if (ItemIndex >= 0) and (ItemIndex < FList.Count) then
      Result:= FList [ ItemIndex ]
    else
      Result := nil;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.SetList(const Value: TList);
begin
  if FList = Value then
    Exit ; //==>

  FList := Value ;

  Refresh ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.SetValue(const Value: TPersistent);
var
  I: Integer;
begin
//  TList doesn't have an IndexOfObject - simulate it...
//  Set the index only (We're assuming the item is present in the list)
  TComboBox( WinControl ).ItemIndex := -1;
  if Value = nil then
    Exit ; //==>
  Assert( Assigned( FList ), sListHasNotBeenAssigned);  // <<<<< change

  for I := 0 to FList.Count - 1 do
    if FList.Items[ I ] = Value then
    begin
      TComboBox( WinControl ).ItemIndex := I;
      Break; //==>
    end;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.WinControlToData;
var
  lValue : TPersistent ;
  lPropType : TTypeKind ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>

  lValue := FList.Items[ComboBox.ItemIndex];

  lPropType := PropType( Data, FieldName ) ;
  if lPropType = tkClass then
  begin
    SetObjectProp(Data, FieldName, lValue );
  end else if (lPropType = tkString) or (lPropType = tkLString) then
  begin
    Assert( Assigned( FOnSetObjectProp ), 'OnSetObjectProp not assigned and it must be when accessing list by a string property' ) ;
    OnSetObjectProp(lValue);
  end else
    raise Exception.Create( sPropTypeNotClassOrString ) ;

end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.SetFieldNameDisplay(const Value: string);
begin
  FsFieldNameDisplay := Value;
  Refresh ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareComboBoxAbs.Create(Owner: TComponent);
begin
  FWinControl := TComboBox.Create( self ) ;
  TComboBox( FWinControl ).OnChange := DoChange ;
  TComboBox( FWinControl ).Style := csDropDownList ;
  TComboBox( FWinControl ).OnKeyPress := DoOnKeyPress ;
  TComboBox( FWinControl ).OnKeyDown := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;
end;

// -----------------------------------------------------------------------------
{
procedure TtiPerAwareComboBoxAbs.DoOnClick(Sender: TObject);
begin
  // Only call inherited if not ReadOnly because the comboBox will have been
  // disabled if it is ReadOnly.
  if not ReadOnly then
    Inherited DoOnClick( Sender ) ;
end;
}

function TtiPerAwareComboBoxAbs.GetDropDownCount: integer;
begin
  result := TComboBox( WinControl ).DropDownCount ;
end;

function TtiPerAwareComboBoxAbs.GetItemIndex: integer;
begin
  result := TComboBox( FWinControl ).ItemIndex ;
end;

procedure TtiPerAwareComboBoxAbs.SetDropDownCount(const Value: integer);
begin
  TComboBox( WinControl ).DropDownCount := Value ;
end;

procedure TtiPerAwareComboBoxAbs.SetItemIndex(const Value: integer);
begin
  TComboBox( FWinControl ).ItemIndex := Value ;
end;

procedure TtiPerAwareComboBoxAbs.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TComboBox( WinControl ).OnClick := DoChange
  else
    TComboBox( WinControl ).OnClick := nil ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  if HandleAllocated then
  begin
    SendMessage(Handle, EM_SETREADONLY, Ord(Value), 0);
    TComboBox( WinControl ).Style := uStyles[ Value ];
  end else if Value then
    TComboBox( WinControl ).Enabled := False
  else
    TComboBox( WinControl ).Enabled := Enabled ;
end;

{ TtiPerAwareComboBoxHistory }

constructor TtiPerAwareComboBoxHistory.create(owner: TComponent);
begin
  inherited create( owner ) ;
  TComboBox( FWinControl ).Style := csDropDown ;
  TComboBox( FWinControl ).OnExit := DoOnExit ;
  FiHistoryCount := 5 ;

  FPopupMenu := TPopupMenu.Create( nil ) ;
  PopupMenu  := FPopupMenu ;

  // Clear menu item
  FpmiClear          := TMenuItem.Create( nil ) ;
  FpmiClear.Caption  := '&Clear' ;
  FpmiClear.OnClick  := pmiClearOnClick ;
  FPopupMenu.Items.Add( FpmiClear ) ;
end;

destructor TtiPerAwareComboBoxHistory.destroy;
begin
  FpmiClear.Free ;
  FPopupMenu.Free ;
  inherited Destroy ;
end;

procedure TtiPerAwareComboBoxHistory.DoOnExit( Sender : TObject ) ;
begin
  Save ;
  inherited;
end;

function TtiPerAwareComboBoxHistory.GetRegINIFile: TRegINIFile;
begin
  result := TRegINIFile.create(
              ChangeFileExt( ExtractFileName( application.exeName ), '' )) ;
end;

procedure TtiPerAwareComboBoxHistory.Loaded;
begin
  inherited loaded ;
  Read ;
  // Select the last selected value
  if TComboBox( WinControl ).Items.Count >= 2 then
    TComboBox( WinControl ).ItemIndex := 1 ;
end;

procedure TtiPerAwareComboBoxHistory.pmiClearOnClick(sender: TObject);
begin
  TComboBox( FWinControl ).Items.Clear ;
  TComboBox( FWinControl ).Items.Add( '' ) ;
end;

procedure TtiPerAwareComboBoxHistory.Read;
var
  lReg : TRegINIFile ;
  ls   : string ;
  lsText : string ;
begin
  lsText := Value ;
  TComboBox( WinControl ).Items.clear ;
  lReg := GetRegINIFile ;
  try
    ls := lReg.ReadString( Owner.Name, Name, '' ) ;
    TComboBox( WinControl ).Items.CommaText := ls ;
    TComboBox( WinControl ).Items.Insert( 0, '' ) ;
  finally
    lReg.Free ;
  end ;
  Value := lsText ;
end;

procedure TtiPerAwareComboBoxHistory.Save;
var
  lReg : TRegINIFile ;
  ls : string ;
  lsText : string ;
  i : integer ;
begin
  lsText := Value ;
  if lsText <> '' then begin
    // Is the current item already in the history list ?
    for i := 0 to Items.Count - 1 do
      if UpperCase( Items[i] ) = UpperCase( lsText ) then begin
        Items.Delete( i ) ;
        Break ; //==>
      end ;

    Items.Insert( 1, lsText ) ;

  end ;

  // If we have more items in the history, then delete the last one
  if Items.Count > HistoryCount + 1 then
    Items.Delete( Items.Count - 1 ) ;

  ls := Items.CommaText ;

  if ls = '""' then
    ls := '' ;
  ls := Copy( ls, 2, length( ls ) - 1 ) ;
  lReg := GetRegINIFile ;
  try
    lReg.WriteString( Owner.Name, Name, ls ) ;
  finally
    lReg.Free ;
  end ;

  Value := lsText ;

end;

procedure TtiPerAwareComboBoxHistory.SetHistoryCount(const iValue: integer);
begin
  if iValue < 5 then begin
    FiHistoryCount := 5 ;
    exit ;
  end ;
  if iValue > 20 then begin
    FiHistoryCount := 20 ;
    exit ;
  end ;
  FiHistoryCount := iValue ;
end;

function TtiPerAwareDateTimePicker.GetDateMode: TDTDateMode;
begin
  result := TDateTimePicker( WinControl ).DateMode ;
end;

function TtiPerAwareDateTimePicker.GetKind: TDateTimeKind;
begin
  result := TDateTimePicker( WinControl ).Kind ;
end;

procedure TtiPerAwareDateTimePicker.SetDateMode(const Value: TDTDateMode);
begin
  TDateTimePicker( WinControl ).DateMode := Value ;
end;

procedure TtiPerAwareDateTimePicker.SetKind(const Value: TDateTimeKind);
begin
  TDateTimePicker( WinControl ).Kind := Value ;
end;

procedure TtiPerAwareComboBoxHistory.SetValue(const Value: String);
var
  i : integer ;
  lStrings : TStrings ;
  lFoundIndex : integer;
begin
  SetOnChangeActive( false ) ;
  lStrings := TComboBox( FWinControl ).Items ;
  lFoundIndex := -1 ;
  for i := 0 to lStrings.Count - 1 do
    if lStrings[i] = Value then
    begin
      lFoundIndex := i ;
      Break ; //==>
    end ;

  if lFoundIndex = -1 then
  begin
    lStrings.Insert(1, Value);
    lFoundIndex := 1 ;
  end ;

  TComboBox( FWinControl ).ItemIndex := lFoundIndex ;

  WinControlToData ;
  SetOnChangeActive( false ) ;
  
end;

function TtiPerAwareComboBoxDynamic.GetValueAsString: string;
begin
  result := FValueAsString ;
end;
procedure TtiPerAwareCheckBox.DoLabelClick(Sender: TObject);
begin
  if Not Enabled then
    Exit ; //==>
  if not Focused then
    SetFocus ;
  if ReadOnly then
    Exit ; //==>
  TCheckBox( FWinControl ).Checked := not TCheckBox( FWinControl ).Checked ;
end;
                                    
procedure TtiPerAwareComboBoxDynamic.SetValueAsString(const Value: string);
begin
  Assert( Assigned( FList ), sListHasNotBeenAssigned);  // <<<<< change
  Assert( Assigned( FOnGetObjectProp ), 'OnGetObjectProp not assigned and it must be when accessing list by a string property' ) ;
  FValueAsString := Value ;
  SetStrProp(FData, FieldName, Value );
  DataToWinControl ;
end;

function TtiPerAwareComboBoxAbs.ComboBox: TComboBox;
begin
  result := TComboBox( WinControl ) ;
end;

//procedure TtiPerAwareComboBoxDynamic.SetFieldNameSelectBy(const Value: string);
//begin
//  FFieldNameSelectBy := Value;
//  Refresh ;
//end;

function TtiPerAwareComboBoxAbs.GetCharCase: TEditCharCase;
begin
  result := TComboBox( FWinControl ).CharCase ;
end;

procedure TtiPerAwareComboBoxAbs.SetCharCase(const Value: TEditCharCase);
begin
  TComboBox( FWinControl ).CharCase := Value ;
end;

procedure TtiPerAwareComboBoxAbs.DoOnKeyPress(Sender: TObject; var Key: Char);
begin
  if not ReadOnly then
    Inherited DoOnKeyPress( Sender, Key )
  else
    Key := #0;
end;

procedure TtiPerAwareImageEdit.SetVisibleButtons(const Value: TtiImageEditVisibleButtons);
begin
  FVisibleButtons := Value;
  FbtnLoadFromFile.Visible  := ievbLoadFromFile in Value ;
  FbtnSaveToFile.Visible    := ievbSaveToFile in Value ;
  FbtnPasteFromClip.Visible := ievbPasteFromClip in Value ;
  FBtnCopyToClip.Visible    := ievbCopyToClip in Value ;
  FbtnEdit.Visible          := ievbEdit in Value ;
  FbtnClear.Visible         := ievbClear in Value ;
  FbtnStretch.Visible       := ievbStretch in Value ;
end;

procedure TtiPerAwareAbs.SetGreyWhenReadOnly(const Value: boolean);
begin
  FGreyWhenReadOnly := Value;
  SetControlColor ;
end;

procedure TtiPerAwareImageEdit.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly(Value);
  if (not ReadOnly) and (Assigned(FOnEdit)) then
    FImage.OnDblClick := DoEdit
  else
    FImage.OnDblClick := nil;
end;

procedure TtiPerAwareImageEdit.SetOnEdit(const Value: TNotifyEvent);
begin
  FOnEdit := Value;
  if (not ReadOnly) and (Assigned(FOnEdit)) then
    FImage.OnDblClick := DoEdit
  else
    FImage.OnDblClick := nil;
end;

initialization

  // 02/01/2002, Ha-Hoe, Made change to decimal separator
  cFloatEditMask      := '#' + ThousandSeparator  + '##0'  ;
  cValidFloatChrs     :=
    [ '-', DecimalSeparator ,
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] ;

end.





