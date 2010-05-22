unit tiHyperlinkWithImage;

{$I tiDefines.inc}

interface
uses
   Classes
  ,StdCtrls
  ,Graphics
  ,Messages
  ,Controls
  ,ExtCtrls
  ,tiHyperLink
  ,tiHotImage
  ,ActnList
  {$IFDEF FPC}
  ,LMessages
  {$ENDIF}
 ;

type

  TtiHyperlinkWithImage = class;

  TtiHyperLinkWithImageHintEvent = procedure (Sender: TtiHyperLinkWithImage; const AHint: string) of object;

  TtiHyperlinkWithImage = class(TCustomPanel)
  private
    FHL: TtiHyperLink;
    FImage: TtiHotImage;
    FGlyphHot: TBitmap;
    FGlyphNormal: TBitmap;
    FGlyphDisabled: TBitmap;
    FMouseOver: boolean;
    FHint: string;
    FOnHint: TtiHyperLinkWithImageHintEvent;
    FMargin: integer;
    procedure SetMargin(const AValue: integer);

  protected
    procedure SetMouseOver(AValue: Boolean);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    function  GetCaption: string;
    procedure SetCaption(const AValue: string);
    function  GetAction: TAction; reintroduce;
    procedure SetAction(const AValue: TAction);
    procedure SetGlyphDisabled(const AValue: TBitmap);
    procedure SetGlyphHot(const AValue: TBitmap);
    procedure SetGlyphNormal(const AValue: TBitmap);
    procedure DoOnChangeGlyph(Sender: TObject);
    procedure ShowCorrectGlyph;
    procedure DoOnMouseEnterImage(Sender: TObject);
    procedure DoOnMouseLeaveImage(Sender: TObject);
    procedure DoOnPanelClick(Sender: TObject);
    procedure DoOnHint(AMouseOver: boolean);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Image: TtiHotImage read FImage;
  published
    property Caption: string read GetCaption write SetCaption;
    property Action: TAction read GetAction write SetAction;
    property GlyphNormal: TBitmap read FGlyphNormal write SetGlyphNormal;
    property GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property Hint: string read FHint write FHint;
    property OnHint: TtiHyperLinkWithImageHintEvent read FOnHint write FOnHint;
    property Margin: integer read FMargin write SetMargin;
    property Font;
    property Height;
  end;

implementation

{ TtiHyperlinkWithImage }

const
  cHeight = 16;
  cBorder = 8;

procedure TtiHyperlinkWithImage.CMMouseEnter(var Msg: TMessage);
begin
  SetMouseOver(True);
end;

procedure TtiHyperlinkWithImage.CMMouseLeave(var Msg: TMessage);
begin
  SetMouseOver(False);
end;

constructor TtiHyperlinkWithImage.Create(AOwner: TComponent);
begin
  inherited;
  FMargin:= cHeight;
  BevelOuter:= bvNone;
  Height:= cHeight;
  Cursor := crHandPoint;
  OnClick:= DoOnPanelClick;
  ParentColor:= True;
  ParentFont:= True;
  Caption:= Name;

  FImage:= TtiHotImage.Create(Self);
  FImage.Parent:= Self;
  FImage.Align:= alLeft;
  FImage.Top:= 0;
  FImage.Left:= 0;
  FImage.Height:= cHeight;
  FImage.Width:= cHeight;
  FImage.Transparent := true;

  FImage.Stretch := true;
  FImage.AutoSize:= false;
  FImage.Cursor := crHandPoint;
  FImage.OnMouseEnter:= DoOnMouseEnterImage;
  FImage.OnMouseLeave:= DoOnMouseLeaveImage;

  FHL := TtiHyperLink.Create(Self);
  FHL.Parent := Self;
  FHL.Top:= 0;
  FHL.Left:= FImage.Width + cBorder;
  FHL.ParentFont:= True;

  FGlyphHot      := TBitmap.Create;
  FGlyphHot.OnChange:= DoOnChangeGlyph;
  FGlyphNormal   := TBitmap.Create;
  FGlyphNormal.OnChange:= DoOnChangeGlyph;
  FGlyphDisabled := TBitmap.Create;
  FGlyphDisabled.OnChange:= DoOnChangeGlyph;
  FMouseOver:= false;

end;

destructor TtiHyperlinkWithImage.Destroy;
begin
  FGlyphHot.Free;
  FGlyphNormal.Free;
  FGlyphDisabled.Free;
  inherited;
end;

function TtiHyperlinkWithImage.GetAction: TAction;
begin
  result:= FHL.Action as TAction;
end;

function TtiHyperlinkWithImage.GetCaption: string;
begin
  Result:= FHL.Caption;
end;

procedure TtiHyperlinkWithImage.Resize;
begin
  inherited;
  FImage.Height:= Height;
  FImage.Width:= Height;
  FHL.Left:= Height + FMargin;
  FHL.Top:= (Height - FHL.Height) div 2;
end;

procedure TtiHyperlinkWithImage.DoOnChangeGlyph(Sender: TObject);
begin
  ShowCorrectGlyph;
end;

procedure TtiHyperlinkWithImage.DoOnHint(AMouseOver: boolean);
begin
  if not Assigned(FOnHint) then
    Exit; //==>
  if AMouseOver then
    FOnHint(Self, FHint)
  else
    FOnHint(Self, '');
end;

procedure TtiHyperlinkWithImage.DoOnMouseEnterImage(Sender: TObject);
begin
  SetMouseOver(True);
end;

procedure TtiHyperlinkWithImage.DoOnMouseLeaveImage(Sender: TObject);
begin
  SetMouseOver(False);
end;

procedure TtiHyperlinkWithImage.DoOnPanelClick(Sender: TObject);
begin
  if Assigned(FHL.Action) then
   FHL.Action.OnExecute(Self);
end;

procedure TtiHyperlinkWithImage.SetAction(const AValue: TAction);
begin
  FImage.Action:= AValue;
  FHL.Action:= AValue;
  if AValue <> nil then
    FHint:= AValue.Hint
  else
    FHint:= '';
end;

procedure TtiHyperlinkWithImage.SetCaption(const AValue: string);
begin
  FHL.Caption:= AValue;
end;

procedure TtiHyperlinkWithImage.SetGlyphDisabled(const AValue: TBitmap);
begin
  FGlyphDisabled.Assign(AValue);
end;

procedure TtiHyperlinkWithImage.SetGlyphHot(const AValue: TBitmap);
begin
  FGlyphHot.Assign(AValue);
end;

procedure TtiHyperlinkWithImage.SetGlyphNormal(const AValue: TBitmap);
begin
  FGlyphNormal.Assign(AValue);
end;

procedure TtiHyperlinkWithImage.SetMargin(const AValue: integer);
begin
  FMargin := AValue;
  Resize;
end;

procedure TtiHyperlinkWithImage.SetMouseOver(AValue: Boolean);
begin
  if AValue then
  begin
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    if not FMouseOver then
    begin
      FMouseOver := True;
      FHL.SetMouseOver(True);
      ShowCorrectGlyph;
      DoOnHint(True);
    end;
  end else
  begin
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    if FMouseOver then
    begin
      FMouseOver := False;
      FHL.SetMouseOver(False);
      ShowCorrectGlyph;
      DoOnHint(False);
    end;
  end;
  FMouseOver:= AValue;

end;

procedure TtiHyperlinkWithImage.ShowCorrectGlyph;
begin
  if (Not Enabled) and (not FGlyphDisabled.Empty) then
    FImage.Picture.Assign(FGlyphDisabled)
  else if FMouseOver and (not FGlyphHot.Empty)then
    FImage.Picture.Assign(FGlyphHot)
  else if (not FMouseOver) and (not FGlyphNormal.Empty)then
    FImage.Picture.Assign(FGlyphNormal)
  else
    FImage.Picture.Assign(nil);
end;

end.

