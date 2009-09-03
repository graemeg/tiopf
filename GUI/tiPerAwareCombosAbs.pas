unit tiPerAwareCombosAbs;

{$I tiDefines.inc}

interface
uses
{$IFNDEF FPC}
  Messages
{$ELSE}
  lmessages
  ,lcltype
{$ENDIF}
  ,Controls
  ,tiFocusPanel
  ,StdCtrls
  ,tiSpeedButton
  ,Classes
  ,tiPerAwareCtrls
  ,ExtCtrls
 ;

type

  {$IFNDEF FPC}
   TTranslateString = string;
  {$ENDIF}


  { TtiPickerAbs }

  TtiPickerAbs = class(TCustomPanel)
  private
    FHint: TTranslateString;
    FEdit: TEdit;
    FSpeedButton: TtiSpeedButton;
    FOnChange: TNotifyEvent;
    FbReadOnly: Boolean;
    function    GetActOnEditClick: boolean;
    procedure   SetActOnEditClick(const AValue: boolean);
  protected
    procedure   DoOnChange(sender : TObject); virtual;
    procedure   DoButtonClick(sender : TObject); virtual;
    procedure   SetText(sValue : string); virtual;
    function    GetText : string; virtual;
    procedure   SetEnabled(AValue : boolean); Override;
    procedure   SetReadOnly(const AValue: Boolean);virtual;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   SetHint(const AValue : TTranslateString);{$IFDEF FPC} override;{$ENDIF}
    {$IFNDEF FPC}
    procedure   WMSize(var Message: TWMSize); message WM_SIZE;
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure   DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
    procedure   FontChanged(Sender: TObject); override;
    {$ENDIF}
    property    OnClick;

  published
    property    Anchors;
    property    ActOnEditClick : boolean read GetActOnEditClick write SetActOnEditClick;
    property    Text : string read getText write setText;
    property    Font;
    {$IFDEF FPC}
    property    Hint;
    {$ELSE}
    property    Hint read FHint write SetHint;
    {$ENDIF}
    property    ShowHint;
    property    ParentFont;
    property    Enabled;
    property    ReadOnly  : Boolean     read FbReadOnly    write SetReadOnly;
    property    OnChange : TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner : TComponent); override;
    procedure   SetFocus; override;
    property    Edit : TEdit read FEdit;
    property    SpeedButton: TtiSpeedButton read FSpeedButton;
  end;

  TtiPerAwarePickAbs = class(TtiPerAwareAbs)
  private
    function    GetValue: String;
    procedure   SetValue(const AValue: String);
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
    procedure   SetControlColor; override;
    {$IFNDEF FPC}
  //  procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  published
    property    Value : String read GetValue write SetValue;
  public
    constructor Create(Owner : TComponent); override;
  end;


const
  cuiMinHeight          =  23;
  cuiMinWidth           = 121;
  cuiDefaultHeight      =  23;
  cuiDefaultWidth       = 145;

implementation
uses
  Graphics
  ,Forms
  ,TypInfo
{$IFDEF FPC}
  ,Buttons
  ,LCLIntf
  ,tiResources
  ,LResources
{$ELSE}
  ,Windows
{$ENDIF}
 ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPickerAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPickerAbs.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];

  caption    := ' ';
  Color      := clBtnFace;
  BevelOuter := bvNone;
  BorderWidth := 1;
  Ctl3D := false;
  BorderStyle := bsSingle;

  FSpeedButton := TtiSpeedButton.Create(self);
  FSpeedButton.top := 0;
  FSpeedButton.width := 22;
  FSpeedButton.parent := self;
  FSpeedButton.ParentFont := true;
  {$IFNDEF FPC}
  FSpeedButton.glyph.LoadFromResourceName(HInstance, 'PAITHREEDOTS');
  FSpeedButton.glyphHot.LoadFromResourceName(HInstance, 'PAITHREEDOTS');
  FSpeedButton.GlyphDisabled.LoadFromResourceName(HInstance, 'PAITHREEDOTS_D');
  {$ELSE}
  FSpeedButton.Layout := blGlyphRight;
  FSpeedButton.glyph.LoadFromLazarusResource('PAITHREEDOTS');
  FSpeedButton.glyphHot.LoadFromLazarusResource('PAITHREEDOTS');
  FSpeedButton.GlyphDisabled.LoadFromLazarusResource('PAITHREEDOTS_D');
  {$ENDIF}
  FSpeedButton.onClick := DoButtonClick;


  FEdit := TEdit.Create(self);
  FEdit.left := 0;
  FEdit.top := 0;
  FEdit.parent := self;

  {$IFNDEF FPC}
  FEdit.ParentColor := true;
  FEdit.ctl3D := false;
  FEdit.OnDblClick := DoButtonClick;
  {$ENDIF}

  FEdit.borderStyle := bsNone;
  FEdit.onChange := DoOnChange;
  FEdit.TabStop := true;
  FEdit.TabOrder := 1;
  FEdit.ParentFont := true;

  {$IFDEF FPC}
  Include(FEdit.ComponentStyle, csSubComponent);
  Include(FEdit.ControlStyle, csNoDesignSelectable);
  Include(FSpeedButton.ComponentStyle, csSubComponent);
  Include(FSpeedButton.ControlStyle, csNoDesignSelectable);
  {$ENDIF}
  FEdit.FreeNotification(Self);
  FSpeedButton.FreeNotification(Self);

  height     := cuiDefaultHeight ;
  width      := cuiDefaultWidth  ;

end;

procedure TtiPickerAbs.DoButtonClick(sender : TObject);
begin
  SetFocus;
  if Assigned(OnExit) then
    OnExit(self);
end;

procedure TtiPickerAbs.DoOnChange(sender: TObject);
begin
  if assigned(FOnChange) then
    FOnChange(sender);
end;

procedure TtiPickerAbs.setText(sValue : string);
begin
  FEdit.text := sValue;
end;

function  TtiPickerAbs.getText : string;
begin
  result := FEdit.text;
end;


{$IFNDEF FPC}
procedure TtiPickerAbs.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.Font.Assign(Self.Font);
  FSpeedButton.Font.Assign(Self.Font);
end;

procedure TtiPickerAbs.WMSize(var Message: TWMSize);
var
 iHeight,iWidth : Integer;
begin
  inherited;
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  iWidth := self.clientWidth - FSpeedButton.Width;
  iHeight := self.ClientHeight;
  if (iWidth<=1) or (iHeight<=0) then Exit;
  FSpeedButton.left := iWidth;
  FSpeedButton.height := iHeight;
  FEdit.height := iHeight;
  FEdit.width := iWidth - 1;
end;
{$ELSE}
procedure TtiPickerAbs.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.Font.Assign(Self.Font);
  FSpeedButton.Font.Assign(Self.Font);
end;

procedure TtiPickerAbs.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
 iHeight,iWidth : Integer;
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  iWidth := self.clientWidth - FSpeedButton.Width;
  iHeight := self.ClientHeight;
  if (iWidth<=1) or (iHeight<=0) then Exit;
  FSpeedButton.left := iWidth;
  FSpeedButton.height := iHeight;
  FEdit.height := iHeight;
  FEdit.width := iWidth;
end;
{$ENDIF}






function TtiPickerAbs.GetActOnEditClick: boolean;
begin
  result := Assigned(FEdit.onClick);
end;

procedure TtiPickerAbs.SetActOnEditClick(const AValue: boolean);
begin
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  if AValue then
    FEdit.onClick := DoButtonClick
  else
    FEdit.onClick := nil;
end;

procedure TtiPickerAbs.SetFocus;
begin
  inherited;
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.SetFocus;
  FEdit.SelectAll;
end;

procedure TtiPickerAbs.SetReadOnly(const AValue: Boolean);
begin
  FbReadOnly          := AValue;
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.ReadOnly      := AValue;
  FSpeedButton.Enabled := (not AValue) and Enabled;
end;

procedure TtiPickerAbs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEdit) and (Operation = opRemove) then
    FEdit := nil;
  if (AComponent = FSpeedButton) and (Operation = opRemove) then
    FSpeedButton := nil;
end;

procedure TtiPickerAbs.SetHint(const AValue: TTranslateString);
begin
  {$IFDEF FPC}
  inherited SetHint(AValue);
  {$ENDIF}
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.Hint := AValue;
  FSpeedButton.Hint := AValue;
  FHint := AValue;
end;

procedure TtiPickerAbs.SetEnabled(AValue: boolean);
begin
  inherited SetEnabled(AValue);
  if (FEdit=nil) or (FSpeedButton=nil) then Exit;
  FEdit.Enabled       := AValue;
  FSpeedButton.Enabled := AValue and (not FbReadOnly);
end;

{ TtiPerAwarePickAbs }

constructor TtiPerAwarePickAbs.Create(Owner: TComponent);
begin
  CenterWhenLabelIsLeft := true;
  inherited;
  Height := cuiMinHeight;
end;

procedure TtiPerAwarePickAbs.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  TtiPickerAbs(WinControl).Text := GetPropValue(Data, FieldName);
  SetOnChangeActive(true);
end;

function TtiPerAwarePickAbs.GetValue: String;
begin
  result := TtiPickerAbs(WinControl).Text;
end;

procedure TtiPerAwarePickAbs.SetControlColor;
begin
  inherited;

  // First condition, control is read only
  if ReadOnly then
  begin
    TtiPickerAbs(FWinControl).Edit.Brush.Color := clBtnFace;
  end;

  // Second condition, the control is not enabled.
  if not Enabled then
  begin
    TtiPickerAbs(FWinControl).Edit.Brush.Color := clBtnFace;
  end;

  // Third condition, the control is enabled, and not read only
  if Enabled and Not ReadOnly then
  begin
    TtiPickerAbs(FWinControl).Edit.Brush.Color := clWindow;
  end;

  TtiPickerAbs(FWinControl).Edit.Refresh;
end;

procedure TtiPerAwarePickAbs.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TtiPickerAbs(WinControl).OnChange := DoChange
  else
    TtiPickerAbs(WinControl).OnChange := nil;
end;

procedure TtiPerAwarePickAbs.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  TtiPickerAbs(WinControl).ReadOnly := AValue;
end;

procedure TtiPerAwarePickAbs.SetValue(const AValue: String);
begin
  TtiPickerAbs(WinControl).Text := AValue;
end;

procedure TtiPerAwarePickAbs.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(Data, FieldName, TtiPickerAbs(WinControl).Text);
end;


end.
