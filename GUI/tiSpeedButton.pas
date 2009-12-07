unit tiSpeedButton;

{$I tiDefines.inc}

interface

uses
  Classes
  {$IFNDEF FPC}
  ,Messages
  {$ELSE}
  ,LMessages
  ,lcltype
  ,lclintf
  ,interfacebase
  {$ENDIF}
  ,Graphics
  ,Controls
  ,Buttons
  ,ActnList
  ,tiPerAwareCtrls
  ,tiResources
  ,Menus
 ;

type

 {$IFDEF FPC}
  TMessage = TLMessage;
 {$ENDIF}

  TtiSpeedButton = class(TSpeedButton)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOver: Boolean;
    FGlyphHot: TBitmap;
    FGlyphNormal: TBitmap;
    FGlyphDisabled: TBitmap;
    FImageResName: TtiImageRes;
    FColorAvailable: TColor;
    FColorHilight: TColor;
    procedure SetGlyphHot(AValue: TBitmap);
    procedure SetGlyphDisabled(const AValue: TBitmap);
    procedure SetImageResName(const AValue: TtiImageRes);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetEnabled(AValue: Boolean); override;
    procedure ShowCorrectGlyph;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ClearGlyphs;
    procedure   ShowPopupMenu(const pPopupMenu: TPopupMenu);
  published
    property    Align;
    property    ImageRes : TtiImageRes read FImageResName write SetImageResName;
    property    GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property    GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property    ColorAvailable: TColor read FColorAvailable Write FColorAvailable default clNavy;
    property    ColorHilight: TColor read FColorHilight Write FColorHilight default clRed;
  end;

implementation
uses
  tiImageMgr
  ,Types
 ;

constructor TtiSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOver          := False;
  FGlyphHot      := TBitmap.Create;
  FGlyphNormal   := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  Flat           := True;
  FColorAvailable := clNavy;
  FColorHilight  := clRed;
  Font.Style     := [fsUnderline];
  Font.Color     := FColorAvailable;
  Cursor         := crHandPoint;
end;

destructor TtiSpeedButton.Destroy;
begin
  FGlyphHot.Free;
  FGlyphNormal.Free;
  FGlyphDisabled.Free;
  inherited Destroy;
end;

procedure TtiSpeedButton.SetGlyphHot(AValue: TBitmap);
begin
  FGlyphHot.Assign(AValue);
end;

procedure TtiSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FOver := True;
    ShowCorrectGlyph;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TtiSpeedButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    ShowCorrectGlyph;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TtiSpeedButton.SetEnabled(AValue: Boolean);
var
  lOldValue: Boolean;
begin
  lOldValue := Enabled;
  inherited;
  if AValue <> lOldValue then
  begin
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    FOver := False;
    ShowCorrectGlyph;
  end;
end;

procedure TtiSpeedButton.ShowCorrectGlyph;
begin
  if FGlyphNormal.Empty and (not Glyph.Empty) then
    FGlyphNormal.Assign(Glyph);

  if (Not Enabled) and (not FGlyphDisabled.Empty) then
  begin
    Glyph.FreeImage;
    Glyph.Height := FGlyphDisabled.Height;
    Glyph.Width := FGlyphDisabled.Width * 2;
    Glyph.Canvas.Draw(FGlyphDisabled.Width,0,FGlyphDisabled);
    NumGlyphs := 2;
    Font.Color  := FColorAvailable;
  end
  else if FOver then
  begin
    Glyph.Assign(FGlyphHot);
    NumGlyphs := 1;
    Font.Color  := FColorHilight;
  end else
  begin
    Glyph.Assign(FGlyphNormal);
    NumGlyphs := 1;
    Font.Color  := FColorAvailable;
  end;
end;               

procedure TtiSpeedButton.SetGlyphDisabled(const AValue: TBitmap);
begin
  FGlyphDisabled.Assign(AValue);
end;

procedure TtiSpeedButton.SetImageResName(const AValue: TtiImageRes);
var
  lResName : string;
  lImageIndex : integer;
begin
  FImageResName := AValue;
  lResName := cImageRes[FImageResName];
  if FImageResName = tiRINone then
    ClearGlyphs
  else
  begin
    lImageIndex := gTIImageListMgr.ImageIndex16(lResName);
    if lImageIndex = -1 then
      SetImageResName(tiRINone)
    else
    begin
      ClearGlyphs;
      gTIImageListMgr.ILNormal16.GetBitmap(lImageIndex, Glyph);
      gTIImageListMgr.ILHot16.GetBitmap(lImageIndex, GlyphHot);
      gTIImageListMgr.ILDisabled16.GetBitmap(lImageIndex, GlyphDisabled);
    end;
  end;
end;

procedure TtiSpeedButton.ClearGlyphs;
begin
  FGlyphNormal.Assign(nil);
  Glyph.Assign(nil);
  GlyphHot.Assign(nil);
  GlyphDisabled.Assign(nil);
end;

procedure TtiSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TtiAction then
  begin
    if TtiAction(Sender).ImageIndex >= 0 then
    begin
      ClearGlyphs;
      gTIImageListMgr.ILNormal16.GetBitmap(TtiAction(Sender).ImageIndex, Glyph);
      gTIImageListMgr.ILHot16.GetBitmap(TtiAction(Sender).ImageIndex, GlyphHot);
      gTIImageListMgr.ILDisabled16.GetBitmap(TtiAction(Sender).ImageIndex, GlyphDisabled);
    end;
  end;
end;

procedure TtiSpeedButton.ShowPopupMenu(const pPopupMenu: TPopupMenu);
var
  p: TPoint;
begin
  Assert(pPopupMenu <> nil, 'pPopupMenu not Assigned');
  if pPopupMenu=nil then Exit;
  p.x := BoundsRect.TopLeft.X;
  p.y := BoundsRect.BottomRight.Y;
  p  := Parent.ClientToScreen(p);
  pPopupMenu.Popup(p.x, p.y);
end;

end.

