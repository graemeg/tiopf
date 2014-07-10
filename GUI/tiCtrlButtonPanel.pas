unit tiCtrlButtonPanel;

{$I tiDefines.inc}

interface
uses
   Classes
  ,Controls
  ,ExtCtrls
  ,tiSpeedButton
  ,Buttons
 ;

const
  cButtonHeightMicro  = 12;
  cButtonHeightNormal = 24;
  cButtonWidthLabel   = 57;
  cButtonHeightLarge  = 32;
  cBtnSpace           =  4;
  cButtonMargin       =  2;
  cCaptionView        = 'View';
  cCaptionNew         = 'New';
  cCaptionEdit        = 'Edit';
  cCaptionDelete      = 'Delete';

type

  // ToDo: Remove the LV from the names of these types
  TtiLVBtnVis = (tiLVBtnVisView, tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete);
  TtiLVVisibleButtons = set of TtiLVBtnVis;
  TtiLogicalEvent = function : Boolean of object;
  TtiButtonGetEnabledEvent = procedure(AButton: TtiSpeedButton; var AEnabled: Boolean) of object;
  TLVButtonStyle = (lvbsNoButtons, lvbsMicroButtons, lvbsNormalButtons, lvbsButtonsAndLabels, lvbsLargeButtons);

  TtiCtrlBtnPnlAbs = class(TCustomPanel)
  private
    FLVVisibleButtons: TtiLVVisibleButtons;
    FLVAllowedButtons: TtiLVVisibleButtons;
    FOnCanView: TtiLogicalEvent;
    FOnCanInsert: TtiLogicalEvent;
    FOnCanEdit: TtiLogicalEvent;
    FOnCanDelete: TtiLogicalEvent;
    procedure SetOnCanEdit(const AValue: TtiLogicalEvent);
  protected
    procedure   SetName(const NewName: TComponentName); override;
    procedure   SetChildControlNames; virtual;
    function    GetButtonStyle: TLVButtonStyle;virtual; abstract;
    procedure   SetVisibleButtons(const AValue: TtiLVVisibleButtons); virtual;
    procedure   SetAllowedButtons(const AValue: TtiLVVisibleButtons); virtual;
    function    GetOnView: TNotifyEvent; virtual; abstract;
    function    GetOnDelete: TNotifyEvent; virtual; abstract;
    function    GetOnEdit: TNotifyEvent;virtual; abstract;
    function    GetOnNew: TNotifyEvent;virtual; abstract;
    procedure   SetOnView(const AValue: TNotifyEvent);virtual; abstract;
    procedure   SetOnDelete(const AValue: TNotifyEvent);virtual; abstract;
    procedure   SetOnEdit(const AValue: TNotifyEvent);virtual; abstract;
    procedure   SetOnNew(const AValue: TNotifyEvent);virtual; abstract;
  public
    constructor Create(Owner : TComponent); override;
    property    VisibleButtons : TtiLVVisibleButtons read FLVVisibleButtons write SetVisibleButtons;
    property    AllowedButtons : TtiLVVisibleButtons read FLVAllowedButtons write SetAllowedButtons;
    property    OnView    : TNotifyEvent    read GetOnView    Write SetOnView;
    property    OnNew     : TNotifyEvent    read GetOnNew     Write SetOnNew;
    property    OnEdit    : TNotifyEvent    read GetOnEdit    Write SetOnEdit;
    property    OnDelete  : TNotifyEvent    read GetOnDelete  Write SetOnDelete;
    property    OnCanView : TtiLogicalEvent read FOnCanView   Write FOnCanView;
    property    OnCanInsert: TtiLogicalEvent read FOnCanInsert Write FOnCanInsert;
    property    OnCanEdit : TtiLogicalEvent read FOnCanEdit   Write SetOnCanEdit;
    property    OnCanDelete: TtiLogicalEvent read FOnCanDelete Write FOnCanDelete;
    property    ButtonStyle: TLVButtonStyle  read GetButtonStyle;
    procedure   DrawButtons; virtual; abstract;
    procedure   EnableButtons; virtual; abstract;
    procedure   RefreshButtons;
  published
    property BorderWidth;
  end;

  TtiCtrlBtnPnlButton = class(TtiCtrlBtnPnlAbs)
  private
    FBtnHeight : integer;
    FBtnWidth : integer;
    FBtnNew   : TtiSpeedButton;
    FBtnEdit  : TtiSpeedButton;
    FBtnDelete : TtiSpeedButton;
    FBtnView  : TtiSpeedButton;
    FBtnCustom: TtiSpeedButton;
    FOnCustomButtonEnabled: TtiButtonGetEnabledEvent;
  protected
    procedure   SetChildControlNames; override;
    procedure   SetupSpeedButton(const ABtn: TSpeedButton; const AWidth: Integer); virtual;
    function    GetOnView: TNotifyEvent; override;
    function    GetOnDelete: TNotifyEvent; override;
    function    GetOnEdit: TNotifyEvent; override;
    function    GetOnNew: TNotifyEvent; override;
    procedure   SetOnView(const AValue: TNotifyEvent); override;
    procedure   SetOnDelete(const AValue: TNotifyEvent); override;
    procedure   SetOnEdit(const AValue: TNotifyEvent); override;
    procedure   SetOnNew(const AValue: TNotifyEvent); override;
  public
    constructor Create(Owner : TComponent); override;
    procedure   DrawButtons; override;
    procedure   EnableButtons; override;
    function    AddCustomButton(const AName: string; const ACaption: string;
        const AHint: string; const AOnClick: TNotifyEvent;
        const AWidth: Integer = cButtonWidthLabel;
        const AGlyphResourceName: string = '';
        const AOnGetEnabled: TtiButtonGetEnabledEvent = nil): TtiSpeedButton;

    property BtnNew: TtiSpeedButton read FBtnNew;
    property BtnEdit: TtiSpeedButton read FBtnEdit;
    property BtnDelete: TtiSpeedButton read FBtnDelete;
    property BtnView: TtiSpeedButton read FBtnView;
  end;

  TtiCtrlBtnPnlNoButton = class(TtiCtrlBtnPnlButton)
  protected
    function    GetButtonStyle: TLVButtonStyle;override;
  public
    constructor Create(Owner : TComponent); override;
  end;

  TtiCtrlBtnPnlMicroButton = class(TtiCtrlBtnPnlButton)
  protected
    function    GetButtonStyle: TLVButtonStyle;override;
  public
    constructor Create(Owner : TComponent); override;
  end;

  TtiCtrlBtnPnlNormalButtonAbs = class(TtiCtrlBtnPnlButton)
  public
    constructor Create(Owner : TComponent); override;
  end;

  TtiCtrlBtnPnlNormalButton = class(TtiCtrlBtnPnlNormalButtonAbs)
  protected
    function    GetButtonStyle: TLVButtonStyle;override;
  public
    constructor Create(Owner : TComponent); override;
  end;

  TtiCtrlBtnPnlButtonAndLabel = class(TtiCtrlBtnPnlNormalButtonAbs)
  protected
    function    GetButtonStyle: TLVButtonStyle;override;
  public
    constructor Create(Owner : TComponent); override;
  end;

  TtiCtrlBtnPnlLargeButton = class(TtiCtrlBtnPnlButton)
  protected
    function    GetButtonStyle: TLVButtonStyle;override;
  public
    constructor Create(Owner : TComponent); override;
  end;

procedure CreateCtrlBtnPnl(var pCtrlBtnPnl: TtiCtrlBtnPnlAbs;
                            pBtnPnlStyle: TLVButtonStyle;
                            const pParent : TWinControl;
                            const pCanView, pCanInsert, pCanEdit, pCanDelete : TtiLogicalEvent);



implementation
uses
  tiLog
  ,tiUtils
  ,tiGUIUtils
  ,tiResources
  ,tiImageMgr
  ,SysUtils
 ;

procedure CreateCtrlBtnPnl(var pCtrlBtnPnl: TtiCtrlBtnPnlAbs;
                            pBtnPnlStyle: TLVButtonStyle;
                            const pParent : TWinControl;
                            const pCanView, pCanInsert, pCanEdit, pCanDelete : TtiLogicalEvent);
var
  lVisibleButtons : TtiLVVisibleButtons;
begin
  Assert(pParent <> nil, 'pParent not assigned');

  if pCtrlBtnPnl <> nil then
    lVisibleButtons := pCtrlBtnPnl.VisibleButtons
  else
    lVisibleButtons := [tiLVBtnVisNew,tiLVBtnVisEdit,tiLVBtnVisDelete];

  case pBtnPnlStyle of
  lvbsNoButtons :begin
                      pCtrlBtnPnl.Free;
                      pCtrlBtnPnl := TtiCtrlBtnPnlNoButton.Create(pParent);
                    end;
  lvbsMicroButtons :begin
                      pCtrlBtnPnl.Free;
                      pCtrlBtnPnl := TtiCtrlBtnPnlMicroButton.Create(pParent);
                    end;
  lvbsNormalButtons :begin
                      pCtrlBtnPnl.Free;
                      pCtrlBtnPnl := TtiCtrlBtnPnlNormalButton.Create(pParent);
                    end;
  lvbsButtonsAndLabels :begin
                      pCtrlBtnPnl.Free;
                      pCtrlBtnPnl := TtiCtrlBtnPnlButtonAndLabel.Create(pParent);
                    end;
  lvbsLargeButtons :begin
                      pCtrlBtnPnl.Free;
                      pCtrlBtnPnl := TtiCtrlBtnPnlLargeButton.Create(pParent);
                    end;
  else
    raise exception.Create('Invalid button style');
  end;
  pCtrlBtnPnl.Name := tiGetUniqueComponentNameFromParent(pParent, 'CtrlBtnPnl');
  pCtrlBtnPnl.Caption := '';
  pCtrlBtnPnl.Parent := pParent;
  pCtrlBtnPnl.Top := 1;
  pCtrlBtnPnl.Left := 1;
  pCtrlBtnPnl.OnCanView     := pCanView;
  pCtrlBtnPnl.OnCanInsert   := pCanInsert;
  pCtrlBtnPnl.OnCanEdit     := pCanEdit;
  pCtrlBtnPnl.OnCanDelete   := pCanDelete;
  pCtrlBtnPnl.VisibleButtons := lVisibleButtons;

end;

{ TtiCtrlBtnPnlAbs }

constructor TtiCtrlBtnPnlAbs.Create(Owner: TComponent);
begin
  inherited;
  Self.Caption := '';
  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
//  if (csDesigning in ComponentState) then
  ControlStyle := ControlStyle - [csAcceptsControls];
  ControlStyle := ControlStyle - [csSetCaption];
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BevelOuter := bvNone;
  FLVAllowedButtons := [tiLVBtnVisView, tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]; // All allowed by default
end;

procedure TtiCtrlBtnPnlAbs.RefreshButtons;
begin
  DrawButtons;
  EnableButtons;
end;

procedure TtiCtrlBtnPnlAbs.SetOnCanEdit(const AValue: TtiLogicalEvent);
begin
  FOnCanEdit := AValue;
end;

procedure TtiCtrlBtnPnlAbs.SetVisibleButtons(const AValue: TtiLVVisibleButtons);
begin
  FLVVisibleButtons := AValue;
  DrawButtons;
end;

procedure TtiCtrlBtnPnlAbs.SetAllowedButtons(const AValue: TtiLVVisibleButtons);
begin
  FLVAllowedButtons := AValue;
  EnableButtons;
end;

procedure TtiCtrlBtnPnlAbs.SetChildControlNames;
begin
  // All controls should have a name to facilitate automated GUI testing etc.
  // Override in descendant if there are child controls created at runtime.
  // To generate unique names use tiGetUniqueComponentNameFromParent()
end;

procedure TtiCtrlBtnPnlAbs.SetName(const NewName: TComponentName);
begin
  inherited;
  SetChildControlNames;
end;

{ TtiCtrlBtnPnlButton }

function TtiCtrlBtnPnlButton.AddCustomButton(const AName: string;
  const ACaption: string; const AHint: string; const AOnClick: TNotifyEvent;
  const AWidth: Integer; const AGlyphResourceName: string;
  const AOnGetEnabled: TtiButtonGetEnabledEvent): TtiSpeedButton;
begin
  Assert(FBtnCustom = nil, 'Only one custom button is supported');
  Assert(AName <> '', 'Name must be assigned');
  Assert(ACaption <> '', 'Caption must be assigned');
  Assert(Assigned(AOnClick), 'OnClick must be assigned');

  FBtnCustom := TtiSpeedButton.Create(Self);
  result := FBtnCustom;
  SetupSpeedButton(FBtnCustom, AWidth);
  FBtnCustom.Name := tiGetUniqueComponentNameFromParent(Self, AName);
  FBtnCustom.Caption := ACaption;
  FBtnCustom.Hint := AHint;
  FBtnCustom.ControlStyle := FBtnCustom.ControlStyle + [csNoDesignVisible];
  FBtnCustom.OnClick := AOnClick;
  if AGlyphResourceName <> '' then
  begin
    FBtnCustom.Glyph.LoadFromResourceName(HInstance, AGlyphResourceName + cResTI_StateNormal);
    FBtnCustom.GlyphHot.LoadFromResourceName(HInstance, AGlyphResourceName + cResTI_StateHot);
    FBtnCustom.GlyphDisabled.LoadFromResourceName(HInstance, AGlyphResourceName + cResTI_StateDisabled);
  end;

  FOnCustomButtonEnabled := AOnGetEnabled;
  RefreshButtons;
end;

constructor TtiCtrlBtnPnlButton.Create(Owner: TComponent);
begin
  inherited;
  Height := FBtnHeight;
  Width := (FBtnWidth + cBtnSpace) * 4;

  FBtnView           := TtiSpeedButton.Create(self);
  SetupSpeedButton(FBtnView, FBtnWidth);
  FBtnView.Hint    := 'View [Enter]';
  FBtnView.ControlStyle := FBtnView.ControlStyle + [csNoDesignVisible];

  FBtnEdit           := TtiSpeedButton.Create(self);
  SetupSpeedButton(FBtnEdit, FBtnWidth);
  FBtnEdit.Hint      := 'Edit [Enter]';
  FBtnEdit.ControlStyle := FBtnEdit.ControlStyle + [csNoDesignVisible];

  FbtnNew            := TtiSpeedButton.Create(self);
  SetupSpeedButton(FbtnNew, FBtnWidth);
  FBtnNew.Hint       := 'New [Ins]';
  FBtnNew.ControlStyle := FBtnNew.ControlStyle + [csNoDesignVisible];

  FBtnDelete         := TtiSpeedButton.Create(self);
  SetupSpeedButton(FBtnDelete, FBtnWidth);
  FBtnDelete.Hint    := 'Delete [Del]';
  FBtnDelete.ControlStyle := FBtnDelete.ControlStyle + [csNoDesignVisible];

  FBtnCustom := nil;

  Visible := false;
end;

procedure TtiCtrlBtnPnlButton.DrawButtons;
var
  liBtnCount : integer;
begin

  liBtnCount := 0;

  FBtnView.Visible  := false;
  FBtnNew.Visible   := false;
  FBtnEdit.Visible  := false;
  FBtnDelete.Visible := false;
  if Assigned(FBtnCustom) then
    FBtnCustom.Visible := false;

  FBtnNew.ControlStyle := FBtnNew.ControlStyle + [csNoDesignVisible];
  FBtnEdit.ControlStyle := FBtnEdit.ControlStyle + [csNoDesignVisible];
  FBtnDelete.ControlStyle := FBtnDelete.ControlStyle + [csNoDesignVisible];
  if Assigned(FBtnCustom) then
    FBtnCustom.ControlStyle := FBtnCustom.ControlStyle + [csNoDesignVisible];

  if ((csDesigning in ComponentState) or Assigned(FBtnView.OnClick)) and
     (tiLVBtnVisView in FLVVisibleButtons)then
  begin
    FBtnView.Visible := true;
    FBtnView.Left := 1 + (liBtnCount * (FBtnView.Width + cBtnSpace));
    FBtnView.ControlStyle := FBtnView.ControlStyle - [csNoDesignVisible];
    inc(liBtnCount);
  end;

  if ((csDesigning in ComponentState) or Assigned(FBtnEdit.OnCLick)) and
     (tiLVBtnVisEdit in FLVVisibleButtons) then
  begin
    FBtnEdit.Visible := true;
    FBtnEdit.Left := 1 + (liBtnCount * (FBtnEdit.Width + cBtnSpace));
    FBtnEdit.ControlStyle := FBtnEdit.ControlStyle - [csNoDesignVisible];
    inc(liBtnCount);
  end;

  if ((csDesigning in ComponentState) or Assigned(FBtnNew.OnClick)) and
     (tiLVBtnVisNew in FLVVisibleButtons)then
  begin
    FBtnNew.Visible := true;
    FBtnNew.Left := 1 + (liBtnCount * (FBtnNew.Width + cBtnSpace));
    FBtnNew.ControlStyle := FBtnNew.ControlStyle - [csNoDesignVisible];
    inc(liBtnCount);
  end;

  if ((csDesigning in ComponentState) or Assigned(FBtnDelete.OnClick)) and
     (tiLVBtnVisDelete in FLVVisibleButtons) then
  begin
    FBtnDelete.Visible := true;
    FBtnDelete.Left := 1 + (liBtnCount * (FBtnDelete.Width + cBtnSpace));
    FBtnDelete.ControlStyle := FBtnDelete.ControlStyle - [csNoDesignVisible];
    inc(liBtnCount);
  end;

  if Assigned(FBtnCustom) then
  begin
    FBtnCustom.Visible := true;
    FBtnCustom.Left := 1 + (liBtnCount * (FBtnCustom.Width + cBtnSpace));
    FBtnCustom.ControlStyle := FBtnCustom.ControlStyle - [csNoDesignVisible];
  end;

  Visible :=
      FBtnView.Visible or
      FBtnNew.Visible or
      FBtnEdit.Visible or
      FBtnDelete.Visible or
      (Assigned(FBtnCustom) and FBtnCustom.Visible);

  FBtnNew.Invalidate;
  FBtnEdit.Invalidate;
  FBtnDelete.Invalidate;
  if Assigned(FBtnCustom) then
    FBtnCustom.Invalidate;
  Invalidate;
  (Owner as TControl).Invalidate;
end;

procedure TtiCtrlBtnPnlButton.EnableButtons;
var
  LDesigning : boolean;
  LIsEnabled: Boolean;
begin
  LDesigning := (csDesigning in ComponentState);
  Assert(Assigned(OnCanView),   'OnCanView not assigned');
  Assert(Assigned(OnCanEdit),   'OnCanEdit   not assigned');
  Assert(Assigned(OnCanInsert), 'OnCanInsert not assigned');
  Assert(Assigned(OnCanDelete), 'OnCanDelete not assigned');
  Assert(Assigned(Parent), 'Parent not assigned');

  LIsEnabled :=
    FBtnView.Visible and
    (tiLVBtnVisView in AllowedButtons) and
    (OnCanView or LDesigning) and
    Parent.Enabled;

  if LIsEnabled <> FBtnView.Enabled then
    FBtnView.Enabled := LIsEnabled;

  LIsEnabled := FBtnEdit.Visible and
    (tiLVBtnVisEdit in AllowedButtons) and
    (OnCanEdit or LDesigning);
  if LIsEnabled <> FBtnEdit.Enabled then
    FBtnEdit.Enabled := LIsEnabled;

  LIsEnabled := FBtnNew.Visible and
    (tiLVBtnVisNew in AllowedButtons) and
    (OnCanInsert or LDesigning);
  if LIsEnabled <> FBtnNew.Enabled then
    FBtnNew.Enabled := LIsEnabled;

  LIsEnabled := FBtnDelete.Visible and
    (tiLVBtnVisDelete in AllowedButtons) and
    (OnCanDelete or LDesigning);
  if LIsEnabled <> FBtnDelete.Enabled then
    FBtnDelete.Enabled := LIsEnabled;

  if Assigned(FBtnCustom) then
  begin
    LIsEnabled := FBtnCustom.Visible;
    if Assigned(FOnCustomButtonEnabled) then
      FOnCustomButtonEnabled(FBtnCustom, LIsEnabled);
    if LIsEnabled <> FBtnCustom.Enabled then
      FBtnCustom.Enabled := LIsEnabled;
  end;
end;

function TtiCtrlBtnPnlButton.GetOnDelete: TNotifyEvent;
begin
  Result := FBtnDelete.OnClick;
end;

function TtiCtrlBtnPnlButton.GetOnEdit: TNotifyEvent;
begin
  Result := FBtnEdit.OnClick;
end;

function TtiCtrlBtnPnlButton.GetOnNew: TNotifyEvent;
begin
  Result := FBtnNew.OnClick;
end;

function TtiCtrlBtnPnlButton.GetOnView: TNotifyEvent;
begin
  Result := FBtnView.OnClick;
end;

procedure TtiCtrlBtnPnlButton.SetChildControlNames;

  procedure _SetName(AButton: TSpeedButton; const AName: string);
  var
    LCaptionSet: Boolean;
  begin
    if Assigned(AButton) then
    begin
      LCaptionSet := AButton.Caption <> '';
      AButton.Name := tiGetUniqueComponentNameFromParent(Self, AName);
      // Undo auto caption same as name
      if not LCaptionSet then
        AButton.Caption := '';
    end;
  end;

begin
  inherited;
  _SetName(FBtnView, 'BtnView');
  _SetName(FBtnEdit, 'BtnEdit');
  _SetName(FbtnNew, 'btnNew');
  _SetName(FBtnDelete, 'BtnDelete');
end;

procedure TtiCtrlBtnPnlButton.SetOnDelete(const AValue: TNotifyEvent);
begin
  FBtnDelete.OnClick := AValue;
end;

procedure TtiCtrlBtnPnlButton.SetOnEdit(const AValue: TNotifyEvent);
begin
  FBtnEdit.OnClick := AValue;
end;

procedure TtiCtrlBtnPnlButton.SetOnNew(const AValue: TNotifyEvent);
begin
  FBtnNew.OnClick := AValue;
end;

procedure TtiCtrlBtnPnlButton.SetOnView(const AValue: TNotifyEvent);
begin
  FBtnView.OnClick := AValue;
end;

procedure TtiCtrlBtnPnlButton.SetupSpeedButton(const ABtn: TSpeedButton;
  const AWidth: Integer);
begin
  ABtn.Parent  := Self;
  ABtn.Caption := '';
  ABtn.Visible := false;
  ABtn.ShowHint := true;
  ABtn.Flat    := true;
  ABtn.Top     := 0;
  ABtn.Height  := FBtnHeight;
  ABtn.Width   := AWidth;
end;

{ TtiCtrlBtnPnlMicroButton }

constructor TtiCtrlBtnPnlMicroButton.Create(Owner: TComponent);
begin
  FBtnHeight := cButtonHeightMicro;
  FBtnWidth := cButtonHeightMicro;
  inherited;

  FBtnView.Glyph.LoadFromResourceName(           HInstance, cResTI_View08N);
  FBtnView.GlyphHot.LoadFromResourceName(        HInstance, cResTI_View08H);
  FBtnView.GlyphDisabled.LoadFromResourceName(   HInstance, cResTI_View08D);

  FBtnNew.Glyph.LoadFromResourceName(           HInstance, cResTI_Insert08N);
  FBtnNew.GlyphHot.LoadFromResourceName(        HInstance, cResTI_Insert08H);
  FBtnNew.GlyphDisabled.LoadFromResourceName(   HInstance, cResTI_Insert08D);

  FBtnEdit.Glyph.LoadFromResourceName(          HInstance, cResTI_Edit08N);
  FBtnEdit.GlyphHot.LoadFromResourceName(       HInstance, cResTI_Edit08H);
  FBtnEdit.GlyphDisabled.LoadFromResourceName(  HInstance, cResTI_Edit08D);

  FBtnDelete.Glyph.LoadFromResourceName(        HInstance, cResTI_Delete08N);
  FBtnDelete.GlyphHot.LoadFromResourceName(     HInstance, cResTI_Delete08H);
  FBtnDelete.GlyphDisabled.LoadFromResourceName(HInstance, cResTI_Delete08D);

end;

function TtiCtrlBtnPnlMicroButton.GetButtonStyle: TLVButtonStyle;
begin
  Result := lvbsMicroButtons;
end;

{ TtiCtrlBtnPnlLargeButton }

constructor TtiCtrlBtnPnlLargeButton.Create(Owner: TComponent);
begin
  FBtnHeight := cButtonHeightLarge;
  FBtnWidth := cButtonHeightLarge;
  inherited;
  FBtnView.Glyph.LoadFromResourceName(           HInstance, cResTI_View24N);
  FBtnView.GlyphHot.LoadFromResourceName(        HInstance, cResTI_View24H);
  FBtnView.GlyphDisabled.LoadFromResourceName(   HInstance, cResTI_View24D);

  FBtnNew.Glyph.LoadFromResourceName(           HInstance, cResTI_Insert24N    );
  FBtnNew.GlyphHot.LoadFromResourceName(        HInstance, cResTI_Insert24H   );
  FBtnNew.GlyphDisabled.LoadFromResourceName(   HInstance, cResTI_Insert24D   );

  FBtnEdit.Glyph.LoadFromResourceName(          HInstance, cResTI_Edit24N   );
  FBtnEdit.GlyphHot.LoadFromResourceName(       HInstance, cResTI_Edit24H  );
  FBtnEdit.GlyphDisabled.LoadFromResourceName(  HInstance, cResTI_Edit24D  );

  FBtnDelete.Glyph.LoadFromResourceName(        HInstance, cResTI_Delete24N );
  FBtnDelete.GlyphHot.LoadFromResourceName(     HInstance, cResTI_Delete24H);
  FBtnDelete.GlyphDisabled.LoadFromResourceName(HInstance, cResTI_Delete24D);
end;

{ TtiCtrlBtnPnlNormalButton3 }

constructor TtiCtrlBtnPnlNormalButton.Create(Owner: TComponent);
begin
  FBtnHeight := cButtonHeightNormal;
  FBtnWidth := cButtonHeightNormal;
  inherited;
end;

function TtiCtrlBtnPnlNormalButton.GetButtonStyle: TLVButtonStyle;
begin
  Result := lvbsNormalButtons
end;

function TtiCtrlBtnPnlLargeButton.GetButtonStyle: TLVButtonStyle;
begin
  Result := lvbsLargeButtons;
end;

{ TtiCtrlBtnPnlNoButton }

constructor TtiCtrlBtnPnlNoButton.Create(Owner: TComponent);
begin
  inherited;
  Height := 0;
end;

function TtiCtrlBtnPnlNoButton.GetButtonStyle: TLVButtonStyle;
begin
  Result := lvbsNoButtons;
end;

{ TtiCtrlBtnPnlButtonAndLabel }

constructor TtiCtrlBtnPnlButtonAndLabel.Create(Owner: TComponent);
begin
  FBtnHeight := cButtonHeightNormal;
  FBtnWidth := cButtonWidthLabel;
  inherited;
  FBtnView.Margin  := cButtonMargin;
  FBtnNew.Margin   := cButtonMargin;
  FBtnEdit.Margin  := cButtonMargin;
  FBtnDelete.Margin := cButtonMargin;

  FBtnView.Caption  := cCaptionView;
  FBtnNew.Caption   := cCaptionNew;
  FBtnEdit.Caption  := cCaptionEdit;
  FBtnDelete.Caption := cCaptionDelete;
end;

function TtiCtrlBtnPnlButtonAndLabel.GetButtonStyle: TLVButtonStyle;
begin
  Result := lvbsButtonsAndLabels;
end;

{ TtiCtrlBtnPnlNormalButtonAbs }

constructor TtiCtrlBtnPnlNormalButtonAbs.Create(Owner: TComponent);
begin
  inherited;
  FBtnView.Glyph.LoadFromResourceName(          HInstance, cResTI_View16N);
  FBtnView.GlyphHot.LoadFromResourceName(       HInstance, cResTI_View16H);
  FBtnView.GlyphDisabled.LoadFromResourceName(  HInstance, cResTI_View16D);

  FBtnNew.Glyph.LoadFromResourceName(           HInstance, cResTI_Insert16N    );
  FBtnNew.GlyphHot.LoadFromResourceName(        HInstance, cResTI_Insert16H   );
  FBtnNew.GlyphDisabled.LoadFromResourceName(   HInstance, cResTI_Insert16D   );

  FBtnEdit.Glyph.LoadFromResourceName(          HInstance, cResTI_Edit16N   );
  FBtnEdit.GlyphHot.LoadFromResourceName(       HInstance, cResTI_Edit16H  );
  FBtnEdit.GlyphDisabled.LoadFromResourceName(  HInstance, cResTI_Edit16D  );

  FBtnDelete.Glyph.LoadFromResourceName(        HInstance, cResTI_Delete16N );
  FBtnDelete.GlyphHot.LoadFromResourceName(     HInstance, cResTI_Delete16H);
  FBtnDelete.GlyphDisabled.LoadFromResourceName(HInstance, cResTI_Delete16D);
end;

end.
