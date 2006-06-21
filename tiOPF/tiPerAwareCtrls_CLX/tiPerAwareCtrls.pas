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
    Mar 2004, Graeme Geldenhuys, Ported to CLX (Linux only)

  Purpose:

  Classes:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerAwareCtrls;

interface

uses
  SysUtils
  ,Classes
  ,QControls
  ,QExtCtrls
  ,tiFocusPanel
  ,QStdCtrls
  ,QGraphics
  ,QTypes
  ,IniFiles
  ,QMenus
  ,QForms
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
  TtiPerAwareAbs = class(TtiFocusPanel)
  protected
    FLabelStyle: TLabelStyle;
    FLabel: TLabel;
    FWidgetControl: TWidgetControl;
    FbCenterWhenLabelIsLeft: Boolean;
    FsFieldName: string;
    FData: TPersistent;
    FOnChange: TNotifyEvent;
    FiLabelWidth: Integer ;
    FbDirty: Boolean ;
    FbReadOnly: Boolean;
    FbError: Boolean;
    FErrorColor: TColor;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDown: TKeyEvent;
    procedure   SetLabelStyle(const Value: TLabelStyle); virtual;
    function    GetCaption: TCaption; virtual;
    procedure   SetCaption(const Value: TCaption); virtual;
    procedure   SetData(const Value: TPersistent); virtual;
    procedure   PositionLabel ; virtual ;
    procedure   PositionWidgetControl ; virtual ;
    procedure   SetLabelWidth(const Value: Integer); virtual ;
    procedure   SetFieldName(const Value: string); virtual ;
    procedure   BoundsChanged; override;
    procedure   Loaded ; override ;
    procedure   DataToWidgetControl ; virtual ; abstract ;
    procedure   WidgetControlToData ; virtual ; abstract ;
    procedure   DoChange( Sender : TObject ) ; virtual ;
    procedure   SetOnChangeActive( Value : boolean ) ; virtual ; abstract ;
    procedure   SetEnabled(const Value: Boolean); Override;
    procedure   SetReadOnly(const Value: Boolean);virtual;
    procedure   SetControlColor ; virtual ;
    procedure   SetError(const Value: boolean); virtual;
    procedure   SetErrorColor(const Value: TColor); virtual;
    procedure   DoOnClick( Sender : TObject ) ; virtual ;
    procedure   DoOnKeyPress( Sender : TObject ; var Key : Char ) ; virtual ;
    procedure   DoOnKeyDown( Sender : TObject ; var Key: Word; Shift: TShiftState ) ;

    property    CenterWhenLabelIsLeft: boolean read FbCenterWhenLabelIsLeft write FbCenterWhenLabelIsLeft;
    property    OnKeyPress : TKeyPressEvent read FOnKeyPress write FOnKeyPress ;
    property    OnKeyDown  : TKeyEvent read FOnKeyDown write FOnKeyDown ;

    function    DataAndPropertyValid : boolean ;
    function    IsPropReadOnly : boolean ; virtual ;

  published
    property    Align;
    property    Anchors;
    property    Constraints;
    property    Enabled;
    property    Font;
    property    Color;
    property    TabOrder;
    property    OnEnter;
    property    OnExit;
    property    ShowFocusRect;
    property    ShowHint ;
    property    ParentColor;

    property    LabelStyle : TLabelStyle read FLabelStyle   write SetLabelStyle default lsLeft ;
    property    Caption    : TCaption    read GetCaption    write SetCaption ;
    property    LabelWidth : Integer     read FiLabelWidth  write SetLabelWidth default cuiDefaultLabelWidth ;
    property    ReadOnly   : Boolean     read FbReadOnly    write SetReadOnly ;

    property    FieldName  : string      read FsFieldName   write SetFieldName ;

    property    Error : boolean read FbError write SetError default False;
    property    ErrorColor : TColor read FErrorColor write SetErrorColor default clYellow;

    // TWidgetControl does not have this, so it must be implemented for each type of control
    property OnChange : TNotifyEvent read FOnChange write FOnChange ;

  public
    constructor Create( Owner : TComponent ) ; override ;
    destructor  Destroy ; override ;
    procedure   SetFocus; override ;
    property    Data       : TPersistent read FData         write SetData ;
    procedure   LinkToData( pData : TPersistent ; const pFieldName : string ) ; virtual ;
    property    Dirty : boolean read FbDirty ;
    procedure   Refresh ; virtual ;
    property    WidgetControl: TWidgetControl read FWidgetControl write FWidgetControl;
    function    Focused: Boolean; override ;
  end;


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
    function  GetEchoMode: TEchoMode;
    procedure SetEchoMode(const Value: TEchoMode);
  protected
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
  published
    property Value : String read GetValue write SetValue ;
    property MaxLength : integer read GetMaxLength write SetMaxLength ;
    property CharCase  : TEditCharCase read GetCharCase write SetCharCase ;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
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
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
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
    function  GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
  protected
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
    function    ComboBox : TComboBox;
  published
    property    DropDownCount : integer read GetDropDownCount write SetDropDownCount ;
    property    CharCase  : TEditCharCase read GetCharCase write SetCharCase ;
  public
    constructor Create( Owner : TComponent ) ; override ;
    property    ItemIndex : integer read GetItemIndex write SetItemIndex ;
  end ;

  // A wrapper for the TComboBox control that has items entered at design time
  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxStatic = class( TtiPerAwareComboBoxAbs )
  protected
    function    GetValue: String; virtual ;
    procedure   SetValue(const Value: String); virtual ;
    function    GetItems: TStrings; virtual ;
    procedure   SetItems(const Value: TStrings); virtual ;
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
  published
    property Value : String read GetValue write SetValue ;
    property Items : TStrings read GetItems write SetItems ;
  end ;

  // A wrapper for the TComboBox control with support for history items.
  // ---------------------------------------------------------------------------
  TtiPerAwareComboBoxHistory = class( TtiPerAwareComboBoxStatic )
  private
    FOnValidate : TNotifyEvent ;
    FiHistoryCount: Integer;
    FPopupMenu : TPopupMenu ;
    FpmiClear  : TMenuItem ;

    function  GetINIFile : TINIFile ;
    procedure SetHistoryCount(const iValue: integer);
    procedure pmiClearOnClick( sender : TObject ) ;

  protected
    procedure   SetValue(const Value: String); override ;
    procedure   Loaded ; override ;
    procedure   DoOnExit( Sender : TObject ) ;

  published
    property HistoryCount : Integer read FiHistoryCount write setHistoryCount ;
    property OnValidate : TNotifyEvent read FOnValidate write FOnValidate ;
    property OnKeyPress ;
    property OnKeyDown ;

  public
    constructor Create(owner: TComponent);override;
    destructor  Destroy ; override ;
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
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
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

  // This control should be re-built using custom images for the check
  // box so the check box can be painted in grey when disabled.
  // ---------------------------------------------------------------------------
  TtiPerAwareCheckBox = class( TtiPerAwareAbs )
  private
    function  GetValue: boolean;
    procedure SetValue(const Value: boolean);
  protected
    procedure   SetControlColor ; override ;
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
    procedure   DoOnClick(Sender: TObject) ; override;
    procedure   DoLabelClick(Sender: TObject);
    procedure   PositionWidgetControl ; override ;
  published
    property    Value : boolean read GetValue write SetValue ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

  // The TtiPerAwareFloatEdit can be of these types
  // ---------------------------------------------------------------------------
  TtiFloatEditStyle = ( fesUser, fesInteger, fesFloat, fesCurrency, fesPercent ) ;

  // A wrapper for the TEdit control, with some additional methods to implement
  // number editing.
  // ---------------------------------------------------------------------------
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
    function  IsValidFloat( sValue : string ) : Boolean ;
    function  WithinMinMaxLimits( value : real ) : Boolean;
    function  CustomStrToFloat(var pStrValue: string): Real;

    function  GetValueAsString : string ;
    procedure SetValueAsString( sValue: string);
    procedure SetValue( rValue : Real ) ;
    function  GetValue : Real ;
    procedure SetPrecision( iValue : Integer ) ;
    procedure SetTextAfter( sValue : string ) ;
    procedure SetTextBefore( sValue : string ) ;
    procedure SetTextUnknown(const sValue: string);
    procedure SetMinValue( rValue : Real ) ;
    procedure SetMaxValue( rValue : Real ) ;
    procedure SetFloatEditStyle(const Value: TtiFloatEditStyle);
    procedure SetUnknownValue(const rValue: Real);
    function  GetIsKnown: Boolean;
    procedure SetIsKnown(const bValue: Boolean);

  protected
    procedure   DataToWidgetControl ; override ;
    procedure   WidgetControlToData ; override ;
    procedure   SetOnChangeActive( Value : Boolean ) ; override ;
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
    constructor Create( Owner : TComponent ) ; override ;
  end ;
  


implementation

uses
  TypInfo
  ,QDialogs
  ;


// Added by Chris Latta (andromeda@froggy.com.au) because the values of some
// constants are changed inside the Initialization section.
{$J+}

const
  cValidFloatChrs : set of char = [ '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] ;
  cIntEditMask        =    '#0'  ;
  cFloatEditMask      : string = ''  ;
//  cusImageFilters     = 'Bitmap files|*.bmp|GIF files|*.gif|JPeg files|*.jpg|All files|*.*' ;
  cusImageDefaultExt  = '.bmp' ;
  

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

//*
//* TtiPerAwareAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareAbs.Create(Owner: TComponent);
begin
  inherited ;
  
  FOnChange      := nil ;

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

  Assert( FWidgetControl <> nil, 'FWidgetControl not assigned' ) ;
  FWidgetControl.Parent := self ;
  FLabel.FocusControl := FWidgetControl ;

  FiLabelWidth := cuiDefaultLabelWidth ;

  FbError     := False;
  FErrorColor := clYellow;

//  FWidgetControl.Repaint;
end;

//------------------------------------------------------------------------------
destructor TtiPerAwareAbs.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.Refresh;
begin
  DataToWidgetControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetFocus;
begin
  inherited;
  if ( Enabled ) and ( FWidgetControl.Enabled ) then
    FWidgetControl.SetFocus ;
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
  PositionWidgetControl ;
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
                FLabel.Left   := ClientWidth - (LabelWidth + 3) ;
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
procedure TtiPerAwareAbs.PositionWidgetControl ;
begin

  case LabelStyle of
  lsNone    : begin
                FWidgetControl.Top    := 1 ;
                FWidgetControl.Left   := 1 ;
                FWidgetControl.Height := ClientHeight-2 ;
                FWidgetControl.Width  := ClientWidth-2  ;
              end ;
  lsTop     : begin
                FWidgetControl.Top    := FLabel.Top + FLabel.Height + 2 ;
                FWidgetControl.Left   := 1 ;
                FWidgetControl.Height := ClientHeight - FWidgetControl.Top - 2 ;
                FWidgetControl.Width  := ClientWidth  - FWidgetControl.Left - 2  ;
              end ;
  lsLeft    : begin
                FWidgetControl.Top    := 1 ;
                FWidgetControl.Left   := LabelWidth + 3;
                FWidgetControl.Height := ClientHeight - FWidgetControl.Top - 2;
                FWidgetControl.Width  := ClientWidth  - FWidgetControl.Left - 2 ;
              end ;
  lsTopLeft : begin
                FWidgetControl.Top    := FLabel.Top + FLabel.Height + 3;
                FWidgetControl.Left   := 24 ;
                FWidgetControl.Height := ClientHeight - FWidgetControl.Top - 2 ;
                FWidgetControl.Width  := ClientWidth  - FWidgetControl.Left - 2 ;
              end ;
  lsRight   : begin
                FWidgetControl.Top    := 1 ;
                FWidgetControl.Left   := 1 ;
                FWidgetControl.Height := ClientHeight - FWidgetControl.Top - 2 ;
                FWidgetControl.Width  := FLabel.Left - 3;
              end ;
  else
    raise exception.CreateFmt(
      'Invalid label style. Called in %s.%s',
      [ClassName, 'PositionWidgetControl'] ) ;
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
  PositionWidgetControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetLabelWidth(const Value: Integer);
begin
  FiLabelWidth := Value ;
  FLabel.Width := Value ;
  PositionLabel ;
  PositionWidgetControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled( Value ) ;
  FWidgetControl.Enabled := Value ;
  SetControlColor ;
  FWidgetControl.Refresh ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.BoundsChanged;
begin
  PositionLabel ;
  PositionWidgetControl ;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetData(const Value: TPersistent);
begin
  FbDirty := false ;
  FData := Value;
  DataToWidgetControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetFieldName(const Value: string);
begin
  FsFieldName := Value;
  DataToWidgetControl ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.DoChange(Sender: TObject);
begin
  FbDirty := true ;
  WidgetControlToData ;
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
  if ( Enabled ) and ( FWidgetControl.Enabled ) then
    FWidgetControl.SetFocus
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
  FWidgetControl.TabStop := not FbReadOnly;  // <<<=== IK 05/12/2001
  SetControlColor ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetControlColor;
begin
  // the control is in error
  if Error then
  begin
    FLabel.Font.Color := clBlack ;
    FWidgetControl.Brush.Color := FErrorColor ;
  end ;

  // control is read only
  if ReadOnly then
  begin
    FLabel.Font.Color := clBlack ;
    FWidgetControl.Brush.Color := clBtnFace ;
  end ;

  // the control is not enabled.
  if not Enabled then
  begin
    FLabel.Font.Color := clGray ;
    FWidgetControl.Brush.Color := clBtnFace ;
  end ;

  // the control is enabled, not read only and not in error
  if Enabled and (not ReadOnly) and (not Error) then
  begin
    FLabel.Font.Color := clBlack ;
    FWidgetControl.Brush.Color := clWindow ;
  end ;
  FWidgetControl.Invalidate;
//  FWidgetControl.Refresh ;

end;

//------------------------------------------------------------------------------
procedure TtiPerAwareAbs.SetError(const Value: boolean);
begin
  FbError := Value;
  SetControlColor ;
end;

//------------------------------------------------------------------------------
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
            ( FWidgetControl.Focused ) ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareEdit.Create(Owner: TComponent);
begin
  FWidgetControl := TEdit.Create(self) ;
  TEdit(FWidgetControl).OnChange   := DoChange ;
  TEdit(FWidgetControl).OnKeyPress := DoOnKeyPress ;
  TEdit(FWidgetControl).OnKeyDown  := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := True ;
  inherited;
  Height := cDefaultHeight ;
//  FWidgetControl.Repaint;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetValue: String;
begin
  Result := TEdit( FWidgetControl ).Text ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetValue(const Value: String);
begin
  SetOnChangeActive( false ) ;
  TEdit( FWidgetControl ).Text := Value ;
  WidgetControlToData ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.DataToWidgetControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TEdit( FWidgetControl ).Text := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.WidgetControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TEdit( FWidgetControl ).Text ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TEdit( FWidgetControl ).OnChange := DoChange
  else
    TEdit( FWidgetControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TEdit( FWidgetControl ).ReadOnly := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetMaxLength: integer;
begin
  Result := TEdit( FWidgetControl ).MaxLength ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetMaxLength(const Value: integer);
begin
  TEdit( FWidgetControl ).MaxLength := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetCharCase: TEditCharCase;
begin
  Result := TEdit( FWidgetControl ).CharCase ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetCharCase(const Value: TEditCharCase);
begin
  TEdit(FWidgetControl).CharCase := Value ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareEdit.GetEchoMode: TEchoMode;
begin
  Result := TEdit(FWidgetControl).EchoMode;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareEdit.SetEchoMode(const Value: TEchoMode);
begin
  TEdit(FWidgetControl).EchoMode := Value ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareMemo
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareMemo.Create(Owner: TComponent);
begin
  FWidgetControl := TMemo.Create( self ) ;
  TMemo( FWidgetControl ).OnChange := DoChange ;
  TMemo( FWidgetControl ).OnKeyPress := DoOnKeyPress ;
  TMemo( FWidgetControl ).OnKeyDown := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := false ;
  inherited;
//  FWidgetControl.Repaint;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.DataToWidgetControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TMemo( FWidgetControl ).Lines.Text := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetMaxLength: integer;
begin
  result := TMemo( FWidgetControl ).MaxLength ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetScrollBars: TScrollStyle;
begin
  result := TMemo( FWidgetControl ).ScrollBars ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetValue: string;
begin
  result := TMemo( FWidgetControl ).Lines.Text ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMemo.GetWordWrap: boolean;
begin
  result := TMemo( FWidgetControl ).WordWrap ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetMaxLength(const Value: integer);
begin
  TMemo( FWidgetControl ).MaxLength := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TMemo( FWidgetControl ).OnChange := DoChange
  else
    TMemo( FWidgetControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TMemo( FWidgetControl ).ReadOnly := Value ;
end;

procedure TtiPerAwareMemo.SetScrollBars(const Value: TScrollStyle);
begin
  TMemo( FWidgetControl ).ScrollBars := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetValue(const Value: string);
begin
  SetOnChangeActive( false ) ;
  TMemo( FWidgetControl ).Lines.Text := Value ;
  WidgetControlToData ;
  SetOnChangeActive( false ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.SetWordWrap(const Value: boolean);
begin
  TMemo( FWidgetControl ).WordWrap := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMemo.WidgetControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TMemo( FWidgetControl ).Lines.Text ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareComboBoxAbs.Create(Owner: TComponent);
begin
  FWidgetControl := TComboBox.Create( self ) ;
  TComboBox( FWidgetControl ).OnChange := DoChange ;
  TComboBox( FWidgetControl ).Style := csDropDownList ;
  TComboBox( FWidgetControl ).OnKeyPress := DoOnKeyPress ;
  TComboBox( FWidgetControl ).OnKeyDown := DoOnKeyDown ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxAbs.GetDropDownCount: integer;
begin
  result := TComboBox( FWidgetControl ).DropDownCount ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxAbs.GetItemIndex: integer;
begin
  result := TComboBox( FWidgetControl ).ItemIndex ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetDropDownCount(const Value: integer);
begin
  TComboBox( FWidgetControl ).DropDownCount := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetItemIndex(const Value: integer);
begin
  TComboBox( FWidgetControl ).ItemIndex := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TComboBox( FWidgetControl ).OnClick := DoChange
  else
    TComboBox( FWidgetControl ).OnClick := nil ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  if Value then
    TComboBox( FWidgetControl ).Enabled := False
  else
    TComboBox( FWidgetControl ).Enabled := Enabled ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxAbs.ComboBox : TComboBox;
begin
  Result := TComboBox(FWidgetControl);
end;

//------------------------------------------------------------------------------
function TtiPerAwareComboBoxAbs.GetCharCase: TEditCharCase;
begin
  Result := TComboBox( FWidgetControl ).CharCase ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareComboBoxAbs.SetCharCase(const Value: TEditCharCase);
begin
  TComboBox(FWidgetControl).CharCase := Value ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxStatic
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiPerAwareComboBoxStatic.DataToWidgetControl;
var
  lsValue : string ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  lsValue := GetPropValue( FData, FsFieldName ) ;
  TComboBox( FWidgetControl ).ItemIndex :=
    TComboBox( FWidgetControl ).Items.IndexOf( lsValue ) ;
  SetOnChangeActive( true ) ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxStatic.GetItems: TStrings;
begin
  result := TComboBox( FWidgetControl ).Items ;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxStatic.GetValue: String;
begin
  result := TComboBox( FWidgetControl ).Text ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.SetItems(const Value: TStrings);
begin
  TComboBox( FWidgetControl ).Items.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.SetValue(const Value: String);
begin
  SetOnChangeActive( false ) ;
  TComboBox( FWidgetControl ).ItemIndex :=
    TComboBox( FWidgetControl ).Items.IndexOf( Value ) ;
  WidgetControlToData ;
  SetOnChangeActive( false ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxStatic.WidgetControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( FData, FsFieldName, TComboBox( FWidgetControl ).Text ) ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareComboBoxHistory
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareComboBoxHistory.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  TComboBox( FWidgetControl ).Style := csDropDown ;
  TComboBox( FWidgetControl ).OnExit := DoOnExit ;
  FiHistoryCount := 5 ;

  FPopupMenu := TPopupMenu.Create( nil ) ;
  PopupMenu  := FPopupMenu ;

  // Clear menu item
  FpmiClear          := TMenuItem.Create( nil ) ;
  FpmiClear.Caption  := '&Clear' ;
  FpmiClear.OnClick  := pmiClearOnClick ;
  FPopupMenu.Items.Add( FpmiClear ) ;
end;

// -----------------------------------------------------------------------------
destructor TtiPerAwareComboBoxHistory.destroy;
begin
  FpmiClear.Free ;
  FPopupMenu.Free ;
  inherited Destroy ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.DoOnExit( Sender : TObject ) ;
begin
  Save ;
  inherited;
end;

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxHistory.GetINIFile: TINIFile;
begin
  result := TINIFile.create(
              ChangeFileExt( ExtractFileName( Application.ExeName ), '.history' )) ;
{ Default Linux applications don't have file extentions. }                
//              ChangeFileExt( ExtractFileName( Application.ExeName ), '' )) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.Loaded;
begin
  inherited loaded ;
  Read ;
  // Select the last selected value
  if TComboBox( WidgetControl ).Items.Count >= 2 then
    TComboBox( WidgetControl ).ItemIndex := 1 ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.pmiClearOnClick(sender: TObject);
begin
  TComboBox( FWidgetControl ).Items.Clear ;
  TComboBox( FWidgetControl ).Items.Add( '' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.Read;
var
  lHist : TINIFile ;
  ls   : string ;
  lsText : string ;
begin
  lsText := Value ;
  TComboBox( WidgetControl ).Items.Clear ;
  lHist := GetINIFile ;
  try
    ls := lHist.ReadString( Owner.Name, Name, '' ) ;
    TComboBox( WidgetControl ).Items.CommaText := ls ;
    TComboBox( WidgetControl ).Items.Insert( 0, '' ) ;
  finally
    lHist.Free ;
  end ;
  Value := lsText ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.Save;
var
  lHist : TINIFile ;
  ls : string ;
  lsText : string ;
  i : Integer ;
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
  lHist := GetINIFile ;
  try
    lHist.WriteString( Owner.Name, Name, ls ) ;
  finally
    lHist.Free ;
  end ;

  Value := lsText ;
end;

// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxHistory.SetValue(const Value: String);
var
  lStrings : TStrings ;
  lFoundIndex : Integer;
begin
  SetOnChangeActive( False ) ;
  lStrings := TComboBox( FWidgetControl ).Items ;
  lFoundIndex := lStrings.IndexOf(Value);

  if lFoundIndex = -1 then
  begin
    lStrings.Insert(1, Value);
    lFoundIndex := 1 ;
  end ;

  TComboBox( FWidgetControl ).ItemIndex := lFoundIndex ;

  WidgetControlToData ;
  SetOnChangeActive( False ) ;
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
  lItems:= TComboBox( WidgetControl ).Items ;
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
    on E:Exception do
      raise Exception.CreateFmt( 'Error adding list items to combobox ' +
                                 'Message: %s, Item Property Name: %s',
                                 [E.Message,
                                  FsFieldNameDisplay] ) ;

  end ;
  inherited ;
end ;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.DataToWidgetControl;
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
  with TComboBox( WidgetControl ) do
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
  TComboBox( WidgetControl ).ItemIndex := -1;
  if Value = nil then
    Exit ; //==>
  Assert( Assigned( FList ), sListHasNotBeenAssigned);

  for I := 0 to FList.Count - 1 do
    if FList.Items[ I ] = Value then
    begin
      TComboBox( WidgetControl ).ItemIndex := I;
      Break; //==>
    end;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.WidgetControlToData;
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

// -----------------------------------------------------------------------------
function TtiPerAwareComboBoxDynamic.GetValueAsString: string;
begin
  Result := FValueAsString ;
end;

// -----------------------------------------------------------------------------
procedure TtiPerAwareComboBoxDynamic.SetValueAsString(const Value: string);
begin
  Assert( Assigned( FList ), sListHasNotBeenAssigned);
  Assert( Assigned( FOnGetObjectProp ), 'OnGetObjectProp not assigned and it must be when accessing list by a string property' ) ;
  FValueAsString := Value ;
  SetStrProp(FData, FieldName, Value );
  DataToWidgetControl ;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareCheckBox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareCheckBox.Create(Owner: TComponent);
begin
  FWidgetControl := TCheckBox.Create( self ) ;
  TCheckBox( FWidgetControl ).Caption := '' ;
  TCheckBox( FWidgetControl ).OnClick := DoChange ;
  TCheckBox( FWidgetControl ).Width := 13 ;
  TCheckBox( FWidgetControl ).Constraints.MaxWidth := 13;
  TCheckBox( FWidgetControl ).Color := clGreen;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  FLabel.OnClick := DoLabelClick ;
  Height := 17 ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.DataToWidgetControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TCheckBox( FWidgetControl ).Checked := GetPropValue( FData, FsFieldName ) ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareCheckBox.GetValue: boolean;
begin
  result := TCheckBox( FWidgetControl ).Checked ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetControlColor;
var
  lBrushColor : TColor ;
begin
  lBrushColor := FWidgetControl.Brush.Color ;
  inherited SetControlColor ;
  FWidgetControl.Brush.Color := lBrushColor ;
  FWidgetControl.Refresh ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TCheckBox( FWidgetControl ).OnClick := DoChange
  else
    TCheckBox( FWidgetControl ).OnClick := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  if Value then
    TCheckBox( FWidgetControl ).Enabled := False
  else
    TCheckBox( FWidgetControl ).Enabled := Enabled ;
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
  TCheckBox( FWidgetControl ).Checked := Value ;
  WidgetControlToData ;
  SetOnChangeActive( true ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.WidgetControlToData;
var
  li : Integer ;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  li := Ord( TCheckBox( FWidgetControl ).Checked ) ;
  SetPropValue( FData, FsFieldName, li ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.DoLabelClick(Sender: TObject);
begin
  if Not Enabled then
    Exit ; //==>
  if not Focused then
    FWidgetControl.SetFocus ;
  if ReadOnly then
    Exit ; //==>
  TCheckBox( FWidgetControl ).Checked := not TCheckBox( FWidgetControl ).Checked ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareCheckBox.PositionWidgetControl;
begin
  inherited;

end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareFloatEdit
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerAwareFloatEdit.Create( Owner : TComponent ) ;
begin
  FWidgetControl := TEdit.Create( self ) ;
  FbCenterWhenLabelIsLeft := true ;
  inherited;
  Height := cDefaultHeight ;

  TEdit( FWidgetControl ).OnEnter      := _DoEnter ;
  TEdit( FWidgetControl ).OnExit       := _DoExit ;
  TEdit( FWidgetControl ).OnKeyPress   := _DoKeyPress ;
  TEdit( FWidgetControl ).OnChange     := _DoChange ;
  TEdit( FWidgetControl ).OnClick      := _DoClick ;

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
  TEdit( FWidgetControl ).text := sValue ;
  Value := Value ;
  SetOnChangeActive( true ) ;

end ;

//------------------------------------------------------------------------------
function  TtiPerAwareFloatEdit.getValueAsString : string ;
begin
  result := TEdit( FWidgetControl ).text ;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.isValidFloat( sValue : string ) : boolean ;
var
  rValue : Real ;
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
var
  lStr : string ;
begin
//  try
    lStr := TEdit( FWidgetControl ).text ;
    if (FsTextUnknown <> '')
    and SameText(lStr, FsTextUnknown) then
      result := FrUnknownValue
    else
      result := CustomStrToFloat( lStr ) ;
//  except
//    on E: Exception do
//    begin
//      MessageDlg(E.Message, mtError, [mbOK], 0 );
//      Result := FrUnKnownValue;
//    end;
//  end;
end ;

//------------------------------------------------------------------------------
function TtiPerAwareFloatEdit.customStrToFloat( var pStrValue : string ) : real ;
var
  lStrValue : string ;
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
var
  sValue : string ;
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
  TEdit( FWidgetControl ).Text := sValue ;
  WidgetControlToData ;
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
    TEdit( FWidgetControl ).SelectAll ;
    Exit ; //==>
  end ;

  lSaveOnChange := FOnChange ;
  FOnChange := nil ;

  if (FsTextUnknown <> '')
  and SameText(TEdit( FWidgetControl ).Text, FsTextUnknown) then
    TEdit( FWidgetControl ).Text := FloatToStr( FrUnknownValue )
  else
    TEdit( FWidgetControl ).Text := RemoveFormatChr( TEdit( FWidgetControl ).Text ) ;
    
  TEdit( FWidgetControl ).SelectAll ;
  FOnChange := lSaveOnChange ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit._DoExit( sender : TObject ) ;
var
  rValue : real ;
  lSaveOnChange : TNotifyEvent ;
begin

  if ReadOnly then
    Exit ; //===>

  lSaveOnChange := FOnChange ;
  FOnChange := nil ;
  try
    Value := Value ;
  except
    on E : ERangeError do begin
      MessageDlg( E.message, mtError,
                  [mbOK], 0 ) ;
      TEdit( FWidgetControl ).SetFocus ;
    end else begin
      TEdit( FWidgetControl ).setFocus ;
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
  if FiPrecision <> iValue then
  begin
    FiPrecision := iValue ;
    FFloatEditStyle := fesUser ;
  end;

  if FFloatEditStyle = fesInteger then
    FsEditMask := cIntEditMask
  else
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
  FsBeforeApplyKey := TEdit( FWidgetControl ).Text ;

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
var
  lReal : Real ;
  lIntPos : Integer ;
begin
  lReal := Value ;
  if not WithinMinMaxLimits( lReal ) then begin
    lIntPos := TEdit( FWidgetControl ).SelStart ;
    TEdit( FWidgetControl ).Text := FsBeforeApplyKey ;
    TEdit( FWidgetControl ).SelStart := lIntPos ;
    Exit ; //==>
  end ;
  WidgetControlToData ;
  if Assigned( FOnChange ) then
    FOnChange( self ) ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setTextAfter( sValue : string ) ;
begin
  if FsTextAfter <> sValue then
  begin
    FsTextAfter := sValue ;
    FFloatEditStyle := fesUser ;
  end;
  Value := Value ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.setTextBefore( sValue : string ) ;
begin
  if FsTextBefore <> sValue then
  begin
    FsTextBefore := sValue ;
    FFloatEditStyle := fesUser ;
  end;
  Value := Value ;
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
  TEdit( FWidgetControl ).SelectAll ;
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
    raise Exception.CreateFmt(
      'Invalid TtiPerAwareFloatEditStyle passed to %s.SetFloatEditStyle',
      [ClassName]);

  end ;
  FFloatEditStyle := Value;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.DataToWidgetControl;
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
    TEdit( FWidgetControl ).OnChange := _DoChange
  else
    TEdit( FWidgetControl ).OnChange := nil ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TEdit( FWidgetControl ).ReadOnly := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareFloatEdit.WidgetControlToData;
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

initialization
  cFloatEditMask      := '#' + ThousandSeparator  + '##0'  ;
  cValidFloatChrs     :=
    [ '-', DecimalSeparator ,
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] ;

end.
