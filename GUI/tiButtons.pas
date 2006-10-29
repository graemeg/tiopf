unit tiButtons;

{$I tiDefines.inc}

interface

uses
  SysUtils
  ,Classes
  ,Controls
  ,buttons
  ,extCtrls
  ,comctrls
 ;

type

  // A TCustomPanel with the border set to none, and the caption turned off.
  // This control is not registered with the component pallet, as it
  // is intended for use as the starting point for composite controls.
  TtiPanel = class(TCustomPanel)
  public
    Constructor Create(Aowner : TComponent); override;
  end;

  // An exception which is used in the tiFloat, tiInt, tiCurrency edits
  RangeException = class(Exception);

  // TtiToolBar
  TtiToolBar = class(TToolBar)
  private
  protected
  published
  public
    constructor Create(Aowner : TComponent); override;
  end;

  // TtiButtonPanel
  TtiButtonPanel = class(TCustomPanel)
  private
    FOnBtn2Click: TNotifyEvent;
    FOnBtn1Click: TNotifyEvent;
    FBtn1 : TBitBtn;
    FBtn2 : TBitBtn;
    procedure SetOnBtn1Click(const AValue: TNotifyEvent);
    procedure SetOnBtn2Click(const AValue: TNotifyEvent);
    function GetBtn1Enabled: boolean;
    function GetBtn2Enabled: boolean;
    procedure SetBtn1Enabled(const AValue: boolean);
    procedure SetBtn2Enabled(const AValue: boolean);
  protected
  published
    property OnBtn1Click : TNotifyEvent read FOnBtn1Click write SetOnBtn1Click;
    property OnBtn2Click : TNotifyEvent read FOnBtn2Click write SetOnBtn2Click;
    property Btn1Enabled : boolean      read GetBtn1Enabled write SetBtn1Enabled;
    property Btn2Enabled : boolean      read GetBtn2Enabled write SetBtn2Enabled;
    property Visible;
  public
    Constructor Create(Aowner : TComponent); override;
    Destructor  Destroy; override;
    procedure   DoBtn1Click(sender : TObject); virtual;
    procedure   DoBtn2Click(sender : TObject); virtual;
  end;

  TtiMicroButton = class(TSpeedButton)
  public
    constructor Create(Aowner : TComponent); override;
  end;

implementation
uses
  Forms
  ,tiResources
 ;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiToolBar
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiToolBar.Create(Aowner: TComponent);
begin
  inherited create(Aowner);
  Flat    := true;
  Height  := 25;
  ShowHint := true;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiPanel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPanel.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiButtonPanel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{ ToDo 5 -cComponents: TtiButtonPanel: Add variable number of buttons, with var captions and glyphs }
constructor TtiButtonPanel.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  Width := 253;
  Height := 33 ;
  Align := alBottom;
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];

  FBtn1 := TBitBtn.Create(nil);
  with FBtn1 do begin
    Parent     := self;
    Left       := 94;
    Top        := 4 ;
    Width      := 75;
    Height     := 25;
    Anchors    := [akRight, akBottom];
    TabOrder   := 0 ;
    Caption    := '&OK';
    OnClick    := DoBtn1Click;
    Default    := true;
    ModalResult := mrOK;
    NumGlyphs  := 2;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, cResTI_Tick16ND);
    {$ENDIF}
  end;

  FBtn2 := TBitBtn.Create(nil);
  with FBtn2 do begin
    Parent     := self;
    Left       := 174;
    Top        := 4;
    Width      := 75;
    Height     := 25;
    Anchors    := [akRight, akBottom];
    TabOrder   := 1;
    Caption    := '&Cancel';
    OnClick    := DoBtn2Click;
    Cancel     := true;
    ModalResult := mrCancel;
    NumGlyphs  := 2;
    {$IFNDEF FPC}
    Glyph.LoadFromResourceName(HInstance, cResTI_Cross16ND);
    {$ENDIF}
  end;
end;

destructor TtiButtonPanel.Destroy;
begin
  FBtn1.Free;
  FBtn2.Free;
  inherited;
end;

procedure TtiButtonPanel.DoBtn1Click(sender: TObject);
begin
  if Assigned(FOnBtn1Click) then
    FOnBtn1Click(self);
end;

procedure TtiButtonPanel.DoBtn2Click(sender: TObject);
begin
  if Assigned(FOnBtn2Click) then
    FOnBtn2Click(self);
end;

function TtiButtonPanel.GetBtn1Enabled: boolean;
begin
  result := FBtn1.Enabled;
end;

function TtiButtonPanel.GetBtn2Enabled: boolean;
begin
  result := FBtn2.Enabled;
end;

procedure TtiButtonPanel.SetBtn1Enabled(const AValue: boolean);
begin
  FBtn1.Enabled := AValue;
end;

procedure TtiButtonPanel.SetBtn2Enabled(const AValue: boolean);
begin
  FBtn2.Enabled := AValue;
end;

procedure TtiButtonPanel.SetOnBtn1Click(const AValue: TNotifyEvent);
begin
  FOnBtn1Click := AValue;
  if Assigned(FOnBtn1Click) then
    FBtn1.ModalResult := mrNone
  else
    FBtn1.ModalResult := mrOK;
end;

procedure TtiButtonPanel.SetOnBtn2Click(const AValue: TNotifyEvent);
begin
  FOnBtn2Click := AValue;
  if Assigned(FOnBtn2Click) then
    FBtn2.ModalResult := mrNone
  else
    FBtn2.ModalResult := mrCancel;
end;

{ TtiMicroButton }

constructor TtiMicroButton.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  Height := 12;
  Width := 12;
  Flat  := true;
end;

end.

