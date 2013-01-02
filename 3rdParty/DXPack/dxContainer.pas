
{*******************************************************************}
{                                                                   }
{   Design eXperince Visual Component Library                       }
{   Container Component (dxContainer)                               }
{                                                                   }
{   Copyright (c) 2002 APRIORI business solutions AG                }
{   (W)ritten by M. Hoffmann - ALL RIGHTS RESERVED.                 }
{                                                                   }
{   DEVELOPER NOTES:                                                }
{   ==========================================================      }
{   This file is part of a component suite called Design            }
{   eXperience and may be used in freeware- or commercial           }
{   applications. The package itself is distributed as              }
{   freeware with full sourcecodes.                                 }
{                                                                   }
{   Feel free to fix bugs or include new features if you are        }
{   familiar with component programming. If so, please email        }
{   me your modifications, so it will be possible for me to         }
{   include nice improvements in further releases:                  }
{                                                                   }
{*******************************************************************}

unit dxContainer;

{$I dxVer.inc}

interface

uses
{$IFDEF DELPHI6}
  TypInfo,
{$ENDIF}
  Windows, Classes, Controls, Graphics, StdCtrls, dxCore, dxCoreUtils;

type
{ TdxPaintEvent }

  TdxPaintEvent = procedure(Sender: TObject; Rect: TRect; ACanvas: TCanvas;
    AFont: TFont) of object;

{ TdxEnabledMode }

{$IFDEF DELPHI6}
  TdxEnabledMode = (emAffectChilds, emNormal);
{$ENDIF}

{ TdxCustomContainer }

  TdxCustomContainer = class(TdxCustomControl)
  private
    FAlignment: TAlignment;
    FBorderWidth: TBorderWidth;
    FBoundColor: TColor;
    FBoundLines: TdxBoundLines;
  {$IFDEF DELPHI6}
    FEnabledMode: TdxEnabledMode;
  {$ENDIF}
    FFocusable: Boolean;
    FGlyph: TBitmap;
    FGlyphLayout: TdxGlyphLayout;
    FLayout: TTextLayout;
    FShowBoundLines: Boolean;
    FShowCaption: Boolean;
    FSpacing: Byte;
    FWordWrap: Boolean;
  {$IFDEF DELPHI6}
    FOnEnabledChanged: TNotifyEvent;
  {$ENDIF}
    FOnPaint: TdxPaintEvent;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBoundColor(Value: TColor);
    procedure SetBoundLines(Value: TdxBoundLines);
  {$IFDEF DELPHI6}
    procedure SetEnabledMode(Value: TdxEnabledMode);
  {$ENDIF}
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphLayout(Value: TdxGlyphLayout);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShowBoundLines(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetSpacing(Value: Byte);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
  {$IFDEF DELPHI6}
    procedure HookEnabledChanged; override;
  {$ENDIF}
    procedure HookMouseDown; override;
    procedure HookPosChanged; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BoundColor: TColor read FBoundColor write SetBoundColor default clGray;
    property BoundLines: TdxBoundLines read FBoundLines write SetBoundLines default [];
  {$IFDEF DELPHI6}
    property EnabledMode: TdxEnabledMode read FEnabledMode write SetEnabledMode
      default emNormal;
  {$ENDIF}
    property Focusable: Boolean read FFocusable write FFocusable default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphLayout: TdxGlyphLayout read FGlyphLayout write SetGlyphLayout
      default glCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property Height default 41;
    property ShowBoundLines: Boolean read FShowBoundLines write SetShowBoundLines
      default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption
      default False;
    property Spacing: Byte read FSpacing write SetSpacing default 5;
    property Width default 185;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  {$IFDEF DELPHI6}
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
  {$ENDIF}
    property OnPaint: TdxPaintEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

{ TdxContainer }

  TdxContainer = class(TdxCustomContainer)
  published
    property Alignment;
    property AutoSize;
    property BorderWidth;
    property BoundColor;
    property BoundLines;
    property Caption;
    property Color;
    property Enabled;
  {$IFDEF DELPHI6}
    property EnabledMode;
  {$ENDIF}
    property Focusable;
    property Glyph;
    property GlyphLayout;
    property Layout;
    property ParentColor;
    property ShowBoundLines;
    property ShowCaption;
    property Spacing;
    property WordWrap;
  {$IFDEF DELPHI6}
    property OnEnabledChanged;
  {$ENDIF}
    property OnDblClick;
    property OnPaint;
    property OnResize;
  end;

implementation

{ TdxCustomContainer }

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.Create
  Author:    mh
  Date:      20-Aug-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxCustomContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Height := 41;
  Width := 185;
  FAlignment := taCenter;
  FBoundColor := clGray;
  FBoundLines := [];
{$IFDEF DELPHI6}
  FEnabledMode := emNormal;
{$ENDIF}
  FFocusable := False;
  FGlyph := TBitmap.Create;
  FGlyph.Assign(nil);
  FGlyphLayout := glCenter;
  FLayout := tlCenter;
  FShowBoundLines := True;
  FShowCaption := False;
  FSpacing := 5;
  FWordWrap := False;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.Destroy
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxCustomContainer.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.CreateParams
  Author:    mh
  Date:      20-Aug-2002
  Arguments: var Params: TCreateParams
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.HookEnabledChanged
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

{$IFDEF DELPHI6}
procedure TdxCustomContainer.HookEnabledChanged;
var
  i: Integer;
begin
  inherited;
  if FEnabledMode = emAffectChilds then
  for i := 0 to ControlCount - 1 do
    if IsPublishedProp(Controls[i], 'Enabled') then
      SetPropValue(Controls[i], 'Enabled', Enabled);
  if Assigned(FOnEnabledChanged) then
    FOnEnabledChanged(Self);
end;
{$ENDIF}

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.HookMouseDown
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.HookMouseDown;
begin
  case FFocusable of
    True:
      inherited;
    False:
      begin
        DrawState := DrawState + [dsClicked];
        InternalRedraw;
      end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.HookPosChanged
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.HookPosChanged;
begin
  inherited;
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.AdjustClientRect
  Author:    mh
  Date:      20-Aug-2002
  Arguments: var Rect: TRect
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  dxAdjustBoundRect(BorderWidth, FShowBoundLines, FBoundLines, Rect);
  if not FGlyph.Empty then
    Inc(Rect.Left, FGlyph.Width);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetAlignment
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TAlignment
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetBoundColor
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetBoundColor(Value: TColor);
begin
  if Value <> FBoundColor then
  begin
    FBoundColor := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetBoundLines
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TdxBoundLines
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetBoundLines(Value: TdxBoundLines);
begin
  if Value <> FBoundLines then
  begin
    FBoundLines := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetBorderWidth
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TBorderWidth
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetBorderWidth(Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetEnabledMode
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TdxEnabledMode
  Result:    None
-----------------------------------------------------------------------------}

{$IFDEF DELPHI6}
procedure TdxCustomContainer.SetEnabledMode(Value: TdxEnabledMode);
begin
  if Value <> FEnabledMode then
  begin
    FEnabledMode := Value;
    HookEnabledChanged;
  end;
end;
{$ENDIF}

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetGlyph
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TBitmap
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetGlyph(Value: TBitmap);
begin
  if Value <> FGlyph then
  begin
    FGlyph.Assign(Value);
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetGlyphLayout
  Author:    mh
  Date:      22-Aug-2002
  Arguments: Value: TdxGlyphLayout
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetGlyphLayout(Value: TdxGlyphLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetLayout
  Author:    mh
  Date:      22-Aug-2002
  Arguments: Value: TTextLayout
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetShowBoundLines
  Author:    M. Hoffmann
  Date:      03-Feb-2003
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetShowBoundLines(Value: Boolean);
begin
  if Value <> FShowBoundLines then
  begin
    FShowBoundLines := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetShowCaption
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetSpacing
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Byte
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetSpacing(Value: Byte);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.SetWordWrap
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    InternalRedraw;
  end;
end;

procedure dxDrawText(AParent: TdxCustomControl; ACaption: string; AFont: TFont;
  AAlignment: TAlignment; ALayout: TTextLayout; AWordWrap: Boolean; var ARect: TRect);
  procedure DoDrawText(Handle: THandle; ACaption: string; var ARect: TRect;
    Flags: Integer);
  begin
    DrawText(Handle, PChar(ACaption), -1, ARect, Flags);
  end;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DrawStyle: LongInt;
  CalcRect: TRect;
begin
  with AParent, Canvas do
  begin
    DrawStyle := Alignments[AAlignment];
    if (DrawStyle <> DT_LEFT) and (ARect.Right - ARect.Left < TextWidth(ACaption)) then
      DrawStyle := DT_LEFT;
    DrawStyle := DrawStyle or DT_EXPANDTABS or WordWraps[AWordWrap] or DT_END_ELLIPSIS;
    if ALayout <> tlTop then
    begin
      CalcRect := ARect;
      DoDrawText(Handle, ACaption, CalcRect, DrawStyle or DT_CALCRECT);
      if ALayout = tlBottom then
        OffsetRect(ARect, 0, ARect.Bottom - CalcRect.Bottom)
      else
        OffsetRect(ARect, 0, (ARect.Bottom - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Handle, ACaption, ARect, DrawStyle);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomContainer.Paint
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomContainer.Paint;
var
  Rect: TRect;
begin
  with Canvas do
  begin
    Rect := GetClientRect;
    Brush.Color := Self.Color;
    FillRect(Rect);
    if csDesigning in ComponentState then
      DrawFocusRect(Rect);
    Brush.Style := bsClear;
    if (FShowBoundLines) and (FBoundLines <> []) then
      dxDrawBoundLines(Self.Canvas, FBoundLines, FBoundColor, Rect);
    dxAdjustBoundRect(BorderWidth, FShowBoundLines, FBoundLines, Rect);
    if Assigned(FOnPaint) then
      FOnPaint(Self, Rect, Self.Canvas, Font);
    if not FGlyph.Empty then
    begin
      FGlyph.Transparent := True;
      if FGlyphLayout = glBottom then
        Draw(Rect.Left, Rect.Bottom - FGlyph.Height, FGlyph);
      if FGlyphLayout = glCenter then
        Draw(Rect.Left, ((Rect.Bottom - Rect.Top) - FGlyph.Height) div 2 + 1, FGlyph);
      if FGlyphLayout = glTop then
        Draw(Rect.Left, Rect.Top, FGlyph);
      Inc(Rect.Left, FGlyph.Width);
    end;
    if FShowCaption then
    begin
      Font.Assign(Self.Font);
      InflateRect(Rect, -FSpacing, -1);
      if csDesigning in ComponentState then
      begin
        Pen.Color := clGray;
        Pen.Style := psInsideFrame;
        MoveTo(Rect.Left, Rect.Top);
        LineTo(Rect.Left, Rect.Bottom);
        MoveTo(Rect.Right, Rect.Top);
        LineTo(Rect.Right, Rect.Bottom);
      end;
      dxDrawText(Self, Caption, Font, FAlignment, FLayout, FWordWrap, Rect);
      //dxPlaceText(Self, Canvas, Caption, Font, Enabled, False, FAlignment,
      //  FWordWrap, Rect);
    end;
  end;
end;

end.
