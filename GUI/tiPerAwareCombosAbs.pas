unit tiPerAwareCombosAbs;

interface
uses
  tiFocusPanel
  ,StdCtrls
  ,tiSpeedButton
  ,Classes
  ,Messages
  ,tiPerAwareCtrls
  ,ExtCtrls
  ;

type

  TtiPickerAbs = class( TCustomPanel )
  private
    FEdit   : TEdit ;
    FSpeedButton : TtiSpeedButton ;
    FOnChange : TNotifyEvent ;
    FbReadOnly: Boolean;
    procedure   WMSize( var Message: TWMSize ) ; message WM_SIZE ;
    function    GetActOnEditClick: boolean;
    procedure   SetActOnEditClick(const Value: boolean);
  protected
    procedure   DoOnChange( sender : TObject ) ; virtual;
    procedure   DoButtonClick( sender : TObject ) ; virtual;
    procedure   SetText( sValue : string ) ; virtual ;
    function    GetText : string ; virtual ;
    procedure   SetEnabled( Value : boolean ) ; Override ;
    procedure   SetReadOnly(const Value: Boolean);virtual;
    property    OnClick ;
  published
    property    Anchors ;
    property    ActOnEditClick : boolean read GetActOnEditClick write SetActOnEditClick ;
    property    Text : string read getText write setText ;
    property    Font ;
    property    Enabled ;
    property    ReadOnly   : Boolean     read FbReadOnly    write SetReadOnly ;
    property    OnChange : TNotifyEvent read FOnChange write FOnChange ;
  public
    constructor Create( owner : TComponent ) ; override ;
    procedure   SetFocus ; override ;
    property    Edit : TEdit read FEdit;
    property    SpeedButton: TtiSpeedButton read FSpeedButton;
  end ;

  TtiPerAwarePickAbs = class( TtiPerAwareAbs )
  private
    function    GetValue: String;
    procedure   SetValue(const Value: String);
  protected
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
    procedure   SetControlColor ; override ;
  published
    property    Value : String read GetValue write SetValue ;
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;


const
  cuiMinHeight          =  23 ;
  cuiMinWidth           = 121 ;
  cuiDefaultHeight      =  23 ;
  cuiDefaultWidth       = 145 ;

implementation
uses
  Controls
  ,Graphics
  ,Forms
  ,TypInfo
  ,Windows
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPickerAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPickerAbs.Create( Owner : TComponent ) ;
begin
  inherited Create( Owner ) ;
  ControlStyle := ControlStyle - [csSetCaption] ;

  caption     := ' ' ;
  Color       := clBtnFace ;
  BevelOuter  := bvNone ;
  BorderWidth := 1 ;
  BorderStyle := bsSingle ;
  height      := cuiDefaultHeight  ;
  width       := cuiDefaultWidth   ;

  FSpeedButton := TtiSpeedButton.Create( self ) ;
  FSpeedButton.top  := 0 ;
  FSpeedButton.width := 16 ;
  FSpeedButton.parent := self ;
  FSpeedButton.glyph.LoadFromResourceName( HInstance, 'PAITHREEDOTS' ) ;
  FSpeedButton.glyphHot.LoadFromResourceName( HInstance, 'PAITHREEDOTS' ) ;
  FSpeedButton.GlyphDisabled.LoadFromResourceName( HInstance, 'PAITHREEDOTS_D' ) ;
  FSpeedButton.onClick  := DoButtonClick ;

  FEdit := TEdit.Create( self ) ;
  FEdit.left := 0 ;
  FEdit.top  := 0 ;
  FEdit.parent := self ;
  FEdit.ParentColor := true ;
  FEdit.borderStyle := bsNone ;
  FEdit.ctl3D := false ;
  FEdit.onChange := DoOnChange ;
  FEdit.TabStop := true ;
  FEdit.TabOrder := 1 ;
  FEdit.OnDblClick := DoButtonClick;
  FEdit.Font.Name := cDefaultFixedFontName;

end ;

procedure TtiPickerAbs.DoButtonClick( sender : TObject ) ;
begin
  SetFocus ;
  if Assigned( OnExit ) then
    OnExit( self ) ;
end;

procedure TtiPickerAbs.DoOnChange(sender: TObject);
begin
  if assigned( FOnChange ) then
    FOnChange( sender ) ;
end;

procedure TtiPickerAbs.setText( sValue : string ) ;
begin
  FEdit.text := sValue ;
end ;

function  TtiPickerAbs.getText : string ;
begin
  result := FEdit.text ;
end ;

procedure TtiPickerAbs.WMSize( var Message : TWMSize );
begin
  inherited;
  FSpeedButton.left := self.clientWidth - FSpeedButton.Width ;
  FSpeedButton.height := self.ClientHeight ;
  FEdit.height := self.clientHeight ;
  FEdit.width  := self.clientWidth - FSpeedButton.width - 1 ;
end;

function TtiPickerAbs.GetActOnEditClick: boolean;
begin
  result := Assigned( FEdit.onClick ) ;
end;

procedure TtiPickerAbs.SetActOnEditClick(const Value: boolean);
begin
  if Value then
    FEdit.onClick := DoButtonClick
  else
    FEdit.onClick := nil ;
end;

procedure TtiPickerAbs.SetFocus;
begin
  inherited;
  FEdit.SetFocus ;
  FEdit.SelectAll ;
end;

procedure TtiPickerAbs.SetReadOnly(const Value: Boolean);
begin
  FbReadOnly           := Value;
  FEdit.ReadOnly       := Value ;
  FSpeedButton.Enabled := ( not Value ) and Enabled ;
end;

procedure TtiPickerAbs.SetEnabled(Value: boolean);
begin
  inherited SetEnabled( Value ) ;
  FEdit.Enabled        := Value ;
  FSpeedButton.Enabled := Value and ( not FbReadOnly ) ;
end;

{ TtiPerAwarePickAbs }

constructor TtiPerAwarePickAbs.Create(Owner: TComponent);
begin
  CenterWhenLabelIsLeft := true ;
  inherited;
  Height := cuiMinHeight ;
end;

procedure TtiPerAwarePickAbs.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TtiPickerAbs( WinControl ).Text := GetPropValue( Data, FieldName ) ;
  SetOnChangeActive( true ) ;
end;

function TtiPerAwarePickAbs.GetValue: String;
begin
  result := TtiPickerAbs( WinControl ).Text ;
end;

procedure TtiPerAwarePickAbs.SetControlColor;
begin
  inherited ;

  // First condition, control is read only
  if ReadOnly then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clBtnFace ;
  end ;

  // Second condition, the control is not enabled.
  if not Enabled then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clBtnFace ;
  end ;

  // Third condition, the control is enabled, and not read only
  if Enabled and Not ReadOnly then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clWindow ;
  end ;

  TtiPickerAbs( FWinControl ).Edit.Refresh ;
end;

procedure TtiPerAwarePickAbs.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TtiPickerAbs( WinControl ).OnChange := DoChange
  else
    TtiPickerAbs( WinControl ).OnChange := nil ;
end;

procedure TtiPerAwarePickAbs.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TtiPickerAbs( WinControl ).ReadOnly := Value ;
end;

procedure TtiPerAwarePickAbs.SetValue(const Value: String);
begin
  TtiPickerAbs( WinControl ).Text := Value ;
end;

procedure TtiPerAwarePickAbs.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  if ReadOnly then
    Exit ; //==>
  SetPropValue( Data, FieldName, TtiPickerAbs( WinControl ).Text ) ;
end;

end.
