unit TBXDefaultTheme;

// TBX Package
// Copyright 2001-2003 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXDefaultTheme.pas,v 1.62 2003/05/24 06:16:25 Alex Exp $

interface

{$DEFINE NARROWCOMBOBUTTON}
{*$DEFINE OFFICE2K_COMBOS} // alternative border painting in popups of combo boxes
{*$DEFINE NOEDITCAPTIONINDENT} // do not indent edit item caption in popups
{$I TB2Warn.inc}

uses
  Windows, Messages, Graphics, TBXThemes, ImgList;

type
  TTBXDefaultTheme = class(TTBXTheme)
  private
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    ToolbarColor: TColor;
    procedure SetupColorCache; virtual;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics Access, etc. }
    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;

    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(AViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

    { Painting routines }
    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer); override;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated: Boolean); override;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(Canvas: TCanvas; var ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(Canvas: TCanvas; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintGlyph(Canvas: TCanvas; R: TRect; Kind, State: Integer); override;
    procedure PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer); override;
  end;

implementation

uses
  Classes, Controls, CommCtrl, TBXUtils, TBXUxThemes, TB2Common, TB2Item, TBX, Forms;

var
  SmCaptionFont: TFont;

procedure InitializeStock;
var
  NonClientMetrics: TNonClientMetrics;
begin
  SmCaptionFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    SmCaptionFont.Handle := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);
end;

procedure FinalizeStock;
begin
  SmCaptionFont.Free;
  SmCaptionFont := nil;
end;

procedure DrawButtonBitmap(Canvas: TCanvas; R: TRect);
const
  ROP_DSPDxax = $00E20746;
  Pattern: array [0..15] of Byte =
    ($C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0, 0, 0);
var
  Bmp: TBitmap;
  W, H: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := CreateBitmap(8, 8, 1, 1, @Pattern);
    SetTextColor(Canvas.Handle, clBlack);
    SetBkColor(Canvas.Handle, clWhite);
    SelectObject(Canvas.Handle, GetSysColorBrush(COLOR_BTNTEXT));
    W := 7;
    H := 6;
    with R do
    begin
      BitBlt(Canvas.Handle, (Left + Right - W) div 2, (Top + Bottom - H) div 2, W, H,
        Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
  finally
    Bmp.Free;
  end;
end;

{ TTBXDefaultTheme }

constructor TTBXDefaultTheme.Create;
begin
  inherited;
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXDefaultTheme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  inherited;
end;

function TTBXDefaultTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT:    Result := False;
    TMB_EDITCAPTIONINDENTED:       Result := {$IFNDEF NOEDITCAPTIONINDENT}True{$ELSE}False{$ENDIF};
    TMB_EDITHEIGHTEVEN:            Result := False;
    TMB_PAINTDOCKBACKGROUND:       Result := USE_THEMES;
    TMB_SOLIDTOOLBARS:             Result := True;
  else
    Result := False;
  end;
end;

function TTBXDefaultTheme.GetIntegerMetrics(Index: Integer): Integer;
const
  DEFAULT = -1;
var
  Sz: TSize;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:
      if USE_THEMES then
      begin
        if GetThemePartSize(TOOLBAR_THEME, StockCompatibleBitmap.Canvas.Handle,
          TP_SPLITBUTTONDROPDOWN, TS_NORMAL, nil, TS_TRUE, Sz) = S_OK then
        begin
          Result := Sz.cx + 2;
        end
        else Result := 11;
      end
      else Result := 11;

    TMI_DROPDOWN_ARROWWIDTH:         Result := 8;
    TMI_DROPDOWN_ARROWMARGIN:        Result := 3;

    TMI_MENU_VMARGIN:                Result := 2;
    TMI_MENU_HMARGIN:                Result := 0;
    TMI_MENU_IMGTEXTSPACE:           Result := 1;
    TMI_MENU_LCAPTIONMARGIN:         Result := 2;
    TMI_MENU_RCAPTIONMARGIN:         Result := 3;
    TMI_MENU_SEPARATORSIZE:          Result := DEFAULT;
    TMI_MENU_MDI_DW:                 Result := 2;
    TMI_MENU_MDI_DH:                 Result := 4;

    TMI_LINE_SPACING:                Result := 6;

    TMI_TLBR_SEPARATORSIZE:          Result := DEFAULT;

    TMI_EDIT_FRAMEWIDTH:             Result := 2;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 1;
    TMI_EDIT_MENUMIDWIDTH:           Result := 4;
    TMI_EDIT_BTNWIDTH:               Result := 13;
  else
    Result := DEFAULT;
  end;
end;

function TTBXDefaultTheme.GetViewColor(AViewType: Integer): TColor;
var
  DC: HDC;
begin
  Result := ToolbarColor;
  if (AViewType and VT_TOOLBAR) = VT_TOOLBAR then Result := ToolbarColor
  else if (AViewType and VT_POPUP) = VT_POPUP then
  begin
    if (AViewType and PVT_POPUPMENU) = PVT_POPUPMENU then Result := clPopup
    else if (AViewType and PVT_LISTBOX) = PVT_LISTBOX then Result := clWindow
    else if (AViewType and PVT_TOOLBOX) = PVT_TOOLBOX then Result := ToolbarColor
    else if (AViewType and PVT_CHEVRONMENU) = PVT_CHEVRONMENU then Result := clPopup;
  end
  else if (AViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    DC := GetDC(0);
    Result := NearestMixedColor(ToolbarColor, clWindow, 64);
    ReleaseDC(0, DC);
  end;
end;

function TTBXDefaultTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
var
  IsMenuItem: Boolean;
begin
  with ItemInfo do
  begin
    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and ((ItemOptions and IO_TOOLBARSTYLE) = 0);
    if not USE_THEMES then
    begin
      if IsMenuItem and (ItemInfo.HoverKind <> hkNone) then Result := clHighlight
      else Result := GetViewColor(ItemInfo.ViewType);
    end
    else
    begin
      Result := GetViewColor(ItemInfo.ViewType);
    end;
  end;
end;

function TTBXDefaultTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
var
  InMenuBar, ToolbarStyle, ShowInactive: Boolean;
begin
  with ItemInfo do
  begin
    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    ShowInactive := InMenubar and not Boolean(ItemOptions and IO_APPACTIVE);

    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then Result := clGrayText
    else if Enabled then
    begin
      if not ToolbarStyle or (InMenuBar and USE_FLATMENUS) then
      begin
        if HoverKind <> hkNone then Result := clHighlightText
        else if ShowInactive then Result := clGrayText
        else Result := clPopupText
      end
      else if ShowInactive then Result := clGrayText
      else Result := clBtnText;
    end
    else Result := clGrayText;
  end;
end;

function TTBXDefaultTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
var
  IsFlatMenuItem, InFlatMenuBar, InFlatChevronBar: Boolean;
begin
  with ItemInfo do
  begin
    InFlatMenuBar := ((ViewType and TVT_MENUBAR) = TVT_MENUBAR) and USE_FLATMENUS;
    InFlatChevronBar := ((ViewType and PVT_CHEVRONMENU) = PVT_CHEVRONMENU) and USE_FLATMENUS;
    IsFlatMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and ((ItemOptions and IO_TOOLBARSTYLE) = 0) and USE_FLATMENUS;

    if InFlatMenuBar and (HoverKind <> hkNone) then Result := clHighlight
    else if InFlatChevronBar or IsFlatMenuItem and (HoverKind <> hkNone) then Result := ToolbarColor
    else Result := GetViewColor(ViewType);
  end;
end;

procedure TTBXDefaultTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array [Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array [Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;
  Sz: Integer;
begin
  Sz := 0;
  if (ViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (ViewType and TVT_FLOATING) = TVT_FLOATING then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) = TVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]);
      Border.Y := GetSystemMetrics(YMetrics[Resizable]);
      Exit;
    end
    else Sz := 2;
  end
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
{$IFNDEF OFFICE2K_COMBOS}
    if (ViewType and PVT_LISTBOX) = PVT_LISTBOX then Sz := 1
    else Sz := 3;
{$ELSE}
    Sz := 3;
{$ENDIF}
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if (ViewType and DPVT_FLOATING) = DPVT_FLOATING then
    begin
      Resizable := (ViewType and DPVT_RESIZABLE) = DPVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]);
      Border.Y := GetSystemMetrics(YMetrics[Resizable]);
      Exit;
    end
    else Sz := 2;
  end;
  Border.X := Sz;
  Border.Y := Sz;
end;

procedure TTBXDefaultTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
var
  R, R2: TRect;
begin
  with Margins do
    case MarginID of
      MID_TOOLBARITEM:
        begin
          LeftWidth := 2;
          RightWidth := 2;
          TopHeight := 2;
          BottomHeight := 2;
          if USE_THEMES then
            GetThemeMargins(TOOLBAR_THEME, StockBitmap1.Canvas.Handle, TP_BUTTON, TS_HOT, TMT_CAPTIONMARGINS,
              nil, TMargins(Margins));
        end;

      MID_STATUSPANE:
        begin
          if USE_THEMES then
          begin
            R := Rect(0, 0, 100, 100);
            GetThemeBackgroundContentRect(STATUSBAR_THEME, StockBitmap1.Canvas.Handle, SP_PANE, 0, R, @R2);
            LeftWidth := R2.Left - R.Left;
            RightWidth := R.Right - R2.Right;
            TopHeight := R2.Top - R.Top;
            BottomHeight := R.Bottom - R2.Bottom;
          end
          else
          begin
            LeftWidth := 1;
            RightWidth := 3;
            TopHeight := 1;
            BottomHeight := 1;
          end;
        end;
    else
      LeftWidth := 0;
      RightWidth := 0;
      TopHeight := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXDefaultTheme.PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; Transparent: Boolean; AViewType: Integer);
var
  R: TRect;
begin
  if not Transparent then
  begin
    IntersectRect(R, ARect, AClipRect);
    FillRectEx(Canvas, R, AColor);
  end;
end;

procedure TTBXDefaultTheme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  R: TRect;
  C: TColor;
  InMenuBar, ToolbarStyle: Boolean;

  procedure _Draw(Color: TColor);
  begin
    Canvas.Font.Color := Color;
    if not Rotated then Windows.DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), R, AFormat)
    else DrawRotatedText(Canvas.Handle, ACaption, R, AFormat);
  end;

begin
  with ItemInfo, Canvas do
  begin
    R := ARect;
    C := Font.Color;
    { Apply theme-dependent color only when Font.Color = clNone }
    if C = clNone then C := GetItemTextColor(ItemInfo);
    Brush.Style := bsClear;
    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then _Draw(C)
    else if Enabled then
    begin
      if ToolbarStyle and (Pushed or Selected) and not (InMenuBar and USE_FLATMENUS) then
        OffsetRect(R, 1, 1);
      _Draw(C);
    end
    else if USE_THEMES then _Draw(C)
    else
    begin
      OffsetRect(R, 1, 1);
      _Draw(clBtnHighlight);
      OffsetRect(R, -1, -1);
      _Draw(clBtnShadow);
    end;
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXDefaultTheme.PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
begin
  X := (ARect.Left + ARect.Right) div 2 - 1;
  Y := (ARect.Top + ARect.Bottom) div 2 + 2;
  if ItemInfo.Enabled then Canvas.Pen.Color := clBtnText
  else Canvas.Pen.Color := clGrayText;
  Canvas.Polyline([Point(X-2, Y-2), Point(X, Y), Point(X+4, Y-4),
    Point(X+4, Y-3), Point(X, Y+1), Point(X-2, Y-1), Point(X-2, Y-2)]);
  if ItemInfo.Enabled then
  begin
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.Polyline([Point(X-3, Y-2), Point(X-3, Y-1), Point(X, Y+2),
      Point(X+5, Y-3), Point(X+5, Y-5)]);
  end;
end;

procedure TTBXDefaultTheme.PaintChevron(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo);
const
  Pattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  DC: HDC;
  R2: TRect;
  TempBmp: TBitmap;

  procedure DrawPattern (const Color, Offset: Integer);
  begin
    SelectObject (DC, GetSysColorBrush(Color));
    BitBlt (DC, R2.Left, R2.Top + Offset, R2.Right - R2.Left,
      R2.Bottom - R2.Top, TempBmp.Canvas.Handle, 0, 0, $00E20746 {ROP_DSPDxax});
  end;

begin
  DC := Canvas.Handle;
  R2 := ARect;
  PaintButton(Canvas, R2, ItemInfo);

  if not ItemInfo.IsVertical then
  begin
    R2.Top := 4;
    R2.Bottom := R2.Top + 5;
    Inc (R2.Left, 2);
    R2.Right := R2.Left + 8;
  end
  else
  begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    Inc (R2.Top, 2);
    R2.Bottom := R2.Top + 8;
  end;
  if ItemInfo.Pushed then OffsetRect (R2, 1, 1);
  TempBmp := TBitmap.Create;
  try
    TempBmp.Handle := CreateBitmap(8, 8, 1, 1, @Pattern[ItemInfo.IsVertical]);
    SetTextColor (DC, clBlack);
    SetBkColor (DC, clWhite);
    if ItemInfo.Enabled then DrawPattern(COLOR_BTNTEXT, 0)
    else
    begin
      DrawPattern(COLOR_BTNHIGHLIGHT, 1);
      DrawPattern(COLOR_BTNSHADOW, 0);
    end;
  finally
    TempBmp.Free;
  end;
end;

procedure TTBXDefaultTheme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
var
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  StateFlags: Integer;
  R, BR: TRect;
  C: TColor;
  X, Y: Integer;

  procedure DrawEnabled(var R: TRect);
  begin
    if BtnPressed then
      Windows.DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST)
    else if BtnHot then
      Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST)
    else if not Embedded then
      FrameRectEx(Canvas, R, clWindow);
  end;

  procedure DrawUp;
  begin
    X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
    Y := (R.Top * 3 + R.Bottom + 3) div 4 + Ord(BtnPressed);
    if not BtnDisabled then
    begin
      if Boolean(ItemInfo.ItemOptions and IO_TOOLBARSTYLE) then C := clPopupText
      else C := clBtnText;
    end
    else with Canvas do
    begin
      Inc(X); Inc(Y);
      Pen.Color := clBtnHighlight; Brush.Color := clBtnHighlight;
      Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y - 2)]);
      Dec(X); Dec(Y);
      C := clBtnShadow;
    end;
    Canvas.Pen.Color := C; Canvas.Brush.Color := C;
    Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y - 2)])
  end;

  procedure DrawDn;
  begin
    X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
    Y := (R.Top + R.Bottom * 3 - 4) div 4 + Ord(BtnPressed);
    if not BtnDisabled then
    begin
      if Boolean(ItemInfo.ItemOptions and IO_TOOLBARSTYLE) then C := clPopupText
      else C := clBtnText;
    end
    else with Canvas do
    begin
      Inc(X); Inc(Y);
      Pen.Color := clBtnHighlight; Brush.Color := clBtnHighlight;
      Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
      Dec(X); Dec(Y);
      C := clBtnShadow;
    end;
    Canvas.Pen.Color := C; Canvas.Brush.Color := C;
    Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)])
  end;

begin
  R := ARect;
  with Canvas, ItemInfo do
  begin
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      { DropDown button }
      BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
      if USE_THEMES then
      begin
        if BtnDisabled then StateFlags := CBXS_DISABLED
        else if BtnPressed then StateFlags := CBXS_PRESSED
        else if BtnHot then StateFlags := CBXS_HOT
        else StateFlags := CBXS_NORMAL;
        if BtnHot then InflateRect(R, 1, 1);
        DrawThemeBackground(COMBO_THEME, Handle, CP_DROPDOWNBUTTON, StateFlags, R, nil);
      end
      else
      begin
        Inc(R.Left, 2);
        if not BtnDisabled then with R do
        begin
          if BtnPressed or BtnHot then
            DrawLineEx(Canvas, Left - 1, Top - 1, Left - 1, Bottom + 1, ToolbarColor)
          else if Embedded then
            DrawLineEx(Canvas, Left - 1, Top, Left - 1, Bottom, clBtnShadow)
          else
            DrawLineEx(Canvas, Left - 1, Top, Left - 1, Bottom, clWindow);
          DrawEnabled(R);
        end;
        PaintDropDownArrow(Canvas, R, ItemInfo);
      end;
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      { Paint spin buttons }
      BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;
      if USE_THEMES then
      begin
        if BtnHot then InflateRect(R, 1, 1);

        { Upper with XP themes }
        BR := R;
        BR.Bottom := (R.Top + R.Bottom - 1) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
        if BtnDisabled then StateFlags := UPS_DISABLED
        else if BtnPressed then StateFlags := UPS_PRESSED
        else if BtnHot then StateFlags := UPS_HOT
        else StateFlags := UPS_NORMAL;
        DrawThemeBackground(SPIN_THEME, Handle, SPNP_UP, StateFlags, BR, nil);

        { Lower with XP themes }
        BR := R;
        BR.Top := (R.Top + R.Bottom) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
        if BtnDisabled then StateFlags := DNS_DISABLED
        else if BtnPressed then StateFlags := DNS_PRESSED
        else if BtnHot then StateFlags := DNS_HOT
        else StateFlags := DNS_NORMAL;
        DrawThemeBackground(SPIN_THEME, Handle, SPNP_DOWN, StateFlags, BR, nil);
      end
      else
      begin
        Inc(R.Left, 2);

        if not BtnDisabled then with R do
          if BtnPressed or BtnHot then
            DrawLineEx(Canvas, Left - 1, Top - 1, Left - 1, Bottom + 1, ToolbarColor)
          else if Embedded then
            DrawLineEx(Canvas, Left - 1, Top, Left - 1, Bottom, clBtnShadow)
          else
            DrawLineEx(Canvas, Left - 1, Top, Left - 1, Bottom, clWindow);


        BR := R;
        BR.Bottom := (R.Top + R.Bottom + 1) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
        if BtnHot or BtnPressed then Dec(BR.Bottom);
        if not BtnDisabled then DrawEnabled(BR);
        DrawUp;

        BR := R;
        BR.Top := (R.Top + R.Bottom) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
        if BtnHot or BtnPressed then Inc(BR.Top);
        if not BtnDisabled then DrawEnabled(BR);
        DrawDn;
      end;
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintEditFrame(Canvas: TCanvas;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
begin
  R := ARect;
  PaintFrame(Canvas, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);
  Canvas.Brush.Color := clWindow;
  if ItemInfo.Enabled then Canvas.FillRect(R);
  with EditInfo do if LeftBtnWidth > 0 then Inc(R.Left, LeftBtnWidth - 2);
  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXDefaultTheme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;

  procedure Draw(AColor: TColor);
  begin
    Canvas.Pen.Color := AColor;
    Canvas.Brush.Color := AColor;
    if ItemInfo.IsVertical then Canvas.Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
    else Canvas.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
  end;

begin
  with ItemInfo, ARect do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;

    if (Pushed or Selected) and (ComboPart <> cpSplitRight) then
    begin
      Inc(X); Inc(Y);
    end;

    if Enabled then
    begin
      if Boolean(ItemOptions and IO_TOOLBARSTYLE) then Draw(clPopupText)
      else Draw(clBtnText);
    end
    else
    begin
      Inc(X); Inc(Y);
      Draw(clBtnHighlight);
      Dec(X); Dec(Y);
      Draw(clBtnShadow);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  XPPart: array [TTBXComboPart] of Integer = (TP_BUTTON, TP_DROPDOWNBUTTON,
    TP_SPLITBUTTON, TP_SPLITBUTTONDROPDOWN);
  Edge: array [Boolean] of Integer = (BDR_RAISEDINNER, EDGE_RAISED);
var
  R: TRect;
  Flags, RegionFlags: Cardinal;
  InMenuBar, ShowHover, Embedded, ShowFlatSL: Boolean;
  Region: HRGN;
begin
  R := ARect;
  with ItemInfo do
  begin
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));

    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

    if not InMenuBar and USE_THEMES then
    begin
      { The support for XP themes in menus is not yet implemented since standard
        XP themes seem to have no theming for menus }

      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then Flags := TS_HOT
        else Flags := TS_DISABLED;
      end
      else if ItemInfo.Pushed then Flags := TS_PRESSED
      else if ItemInfo.Selected then
      begin
        if HoverKind <> hkNone then Flags := TS_HOTCHECKED
        else Flags := TS_CHECKED;
      end
      else if HoverKind <> hkNone then Flags := TS_HOT
      else Flags := TS_NORMAL;

      if Embedded or Boolean(ItemOptions and IO_DESIGNING) then
      begin
        { There is no state for non-transparent normal toolbar button, trying to
          simulate it with regions... }
        RegionFlags := TS_HOT;
        if ComboPart = cpSplitRight then Dec(R.Left);
        GetThemeBackgroundRegion(TOOLBAR_THEME, Canvas.Handle, XPPart[ComboPart], RegionFlags, R, Region);
        if ComboPart = cpSplitRight then Inc(R.Left);
        if Embedded or not Boolean(ItemOptions and IO_DESIGNING) then
        begin
          Canvas.Brush.Color := ToolbarColor;
          FillRgn(Canvas.Handle, Region, Canvas.Brush.Handle);
        end;
        Canvas.Brush.Color := clBtnShadow;
        FrameRgn(Canvas.Handle, Region, Canvas.Brush.Handle, 1, 1);
        DeleteObject(Region);
      end;

      DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, XPPart[ComboPart], Flags, R, nil);
    end
    else
    begin
      if InMenuBar and USE_FLATMENUS then
      begin
        if ((Pushed or Selected) and Enabled) or ShowHover then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.FillRect(R);
        end;
        Exit;
      end;
{$IFDEF NARROWCOMBOBUTTON}
      if (ItemInfo.ComboPart = cpSplitRight) and not (InMenuBar or USE_THEMES) then Dec(R.Right, 2);
{$ENDIF}
      if USE_FLATMENUS and (((Pushed or Selected) and Enabled) or ShowHover) then
      begin
        Canvas.Brush.Color := ToolbarColor;
        Canvas.FillRect(R);
      end;
      if Embedded then with Canvas do
      begin
        Flags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        if not ShowHover or (Pushed or Selected or not Enabled) then Flags := Flags or BF_FLAT;
        ShowFlatSL := (ComboPart = cpSplitLeft) and not (ShowHover or Pushed);
        if ShowFlatSL then Inc(R.Right);
        Windows.DrawEdge(Handle, R, EDGE_RAISED, Flags);
        if Selected and Enabled and (HoverKind = hkNone) then
          DitherRect(Canvas, R, ToolbarColor, clBtnHighlight);
        if ShowFlatSL then Dec(R.Right);
      end
      else if (Pushed or Selected) and Enabled then with Canvas do
      begin
        Windows.DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
        if not Pushed and (HoverKind = hkNone) then
          DitherRect(Canvas, R, ToolbarColor, clBtnHighlight);
      end
      else if ShowHover or Boolean(ItemOptions and IO_DESIGNING) then
        Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
      if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo);
const
  SPI_GETGRADIENTCAPTIONS = $1008;
  DC_GRADIENT = $20;
  ActiveCaptionFlags: array [Boolean] of Integer = (0, DC_ACTIVE);
  GradientCaptionFlags: array [Boolean] of Integer = (0, DC_GRADIENT);
  CaptionBkColors: array [Boolean, Boolean] of Integer =
    ((COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION),
    (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION));
  ButtonStateFlags: array [Boolean] of Integer = (0, DFCS_PUSHED);
var
  R, R2: TRect;
  DC: HDC;
  Flags: Integer;
  Gradient, ShowCloseBtn: Boolean;
  B: BOOL;
begin
  DC := Canvas.Handle;

  with WindowInfo do
    if not USE_THEMES then
    begin
      R := ARect;
      if (WRP_BORDER and RedrawPart) <> 0 then
      begin
        R2 := R;
        with FloatingBorderSize do InflateRect(R2, -X, -Y);
        SaveDC(DC);
        with R2 do ExcludeClipRect(DC, Left, Top, Right, Bottom);
        Windows.DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE);
        RestoreDC(DC, -1);
      end;

      if not WindowInfo.ShowCaption then Exit;
      Gradient := SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @B, 0) and B;
      ShowCloseBtn := (CDBS_VISIBLE and CloseButtonState) <> 0;
      R := GetTBXCloseButtonRect(WindowInfo, True);

      if (WRP_CAPTION and RedrawPart) <> 0 then
      begin
        if ShowCloseBtn then
        begin
          SaveDC(DC);
          with R do ExcludeClipRect(DC, Left, Top, Right, Bottom);
        end;
        R2 := GetTBXCaptionRect(WindowInfo, True, ShowCloseBtn);
        DrawCaption(ParentHandle, DC, R2, DC_TEXT or DC_SMALLCAP or
          ActiveCaptionFlags[Active] or GradientCaptionFlags[Gradient]);
        if ShowCloseBtn then RestoreDC(DC, -1);
        R2 := GetTBXCaptionRect(WindowInfo, True, False);
        R2.Top := R2.Bottom;
        Inc(R2.Bottom);
        FillRect(DC, R2, GetSysColorBrush(COLOR_BTNFACE));
      end;

      if ShowCloseBtn then
      begin
        R2 := R;
        InflateRect(R2, -2, -2);
        if (WRP_CAPTION and RedrawPart) <> 0 then
        begin
          SaveDC(DC);
          with R2 do ExcludeClipRect(DC, Left, Top, Right, Bottom);
          FillRect(DC, R, GetSysColorBrush(CaptionBkColors[Gradient, WindowInfo.Active]));
          RestoreDC(DC, -1);
        end;
        if (WRP_CLOSEBTN and RedrawPart) <> 0 then
          DrawFrameControl(DC, R2, DFC_CAPTION, DFCS_CAPTIONCLOSE or
            ButtonStateFlags[(CDBS_PRESSED and CloseButtonState) <> 0]);
      end;
    end
    else { Use WindowsXP visual styles }
    begin
      if (WRP_BORDER and RedrawPart) <> 0 then
      begin
        if Active then Flags := FS_ACTIVE else Flags := FS_INACTIVE;
        R := ARect;
        R.Top := R.Bottom - FloatingBorderSize.Y;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMEBOTTOM, Flags, R, nil);
        R.Top := ARect.Top;
        R.Bottom := R.Top + FloatingBorderSize.Y;
        {if WindowInfo.ShowCaption then} { TODO : how to paint a captionless window frame }
          Inc(R.Bottom, GetSystemMetrics(SM_CYSMCAPTION));
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLCAPTION, Flags, R, nil);
        R.Top := R.Bottom;
        R.Bottom := ARect.Bottom - FloatingBorderSize.Y;
        R.Right := R.Left + FloatingBorderSize.X;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMELEFT, Flags, R, nil);
        R.Right := ARect.Right;
        R.Left := R.Right - FloatingBorderSize.X;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLFRAMERIGHT, Flags, R, nil);
      end;

      if not ShowCaption then Exit;

      { Get the caption area }
      R := ARect;
      with FloatingBorderSize do InflateRect(R, -X, -Y);
      Dec(R.Bottom, ClientHeight);

      if (WRP_CAPTION and RedrawPart) <> 0 then
      begin
        R2 := R;
        if ((CDBS_VISIBLE and CloseButtonState) <> 0) and ((WRP_CLOSEBTN and RedrawPart) <> 0) then
          Dec(R2.Right, GetSystemMetrics(SM_CYSMCAPTION));

        Canvas.Font.Assign(SmCaptionFont);
        if Active then Canvas.Font.Color := clCaptionText
        else Canvas.Font.Color := clInactiveCaptionText;
        Canvas.Brush.Style := bsClear;

       { This is strange... the DrawThemeText function refuses to work...
         Use standard API...   }
        DrawText(Canvas.Handle, WindowInfo.Caption, -1, R2,
          DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX);
      
        Canvas.Brush.Style := bsSolid;
      end;

      if (CDBS_VISIBLE and CloseButtonState) <> 0 then
      begin
        Dec(R.Bottom);
        R.Left := R.Right - R.Bottom + R.Top;
        InflateRect(R, -2, -2);
        if (CDBS_PRESSED and CloseButtonState) <> 0 then Flags := CBS_PUSHED
        else if (CDBS_HOT and CloseButtonState) <> 0 then Flags := CBS_HOT
        else Flags := CBS_NORMAL;
        DrawThemeBackground(WINDOW_THEME, DC, WP_SMALLCLOSEBUTTON, Flags, R, nil);
      end;
    end;
end;

procedure TTBXDefaultTheme.PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  E, Embedded: Boolean;
  Flags, Border: Integer;
begin
  R := ARect;
  with Canvas, ItemInfo do
  begin
    E := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
    if USE_THEMES then
    begin
      InflateRect(R, -1, -1);
      if Embedded then Brush.Color := clBtnShadow
      else Brush.Color := ToolbarColor;
      FrameRect(R);
      InflateRect(R, 1, 1);
      if Pushed or Selected or E or ((ItemOptions and IO_DESIGNING) <> 0)
        then DrawThemeBackground(COMBO_THEME, Handle, 0, 0, R, nil);
      InflateRect(R, -2, -2);
      Brush.Color := clWindow;
      FrameRect(R);
    end
    else
    begin
      if Embedded then
      begin
        Flags := BF_RECT;
        if not (Pushed or Selected or E) then
        begin
          InflateRect(R, -1, -1);
          Flags := Flags or BF_FLAT;
          Border := BDR_SUNKENOUTER;
        end
        else Border := EDGE_SUNKEN;
        Windows.DrawEdge(Handle, R, Border, Flags);
        if (Pushed or Selected or E) then InflateRect(R, -1, -1);
      end
      else
      begin
        if Pushed or Selected or E or ((ItemOptions and IO_DESIGNING) <> 0) then
          Windows.DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT);
        InflateRect(R, -1, -1);
        FrameRectEx(Canvas, R, ToolbarColor, True);
        FrameRectEx(Canvas, R, clWindow);
      end;
    end;
  end;
end;

function TTBXDefaultTheme.GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ImageList: TCustomImageList): TPoint;
const
  Offsets: array [Boolean] of TPoint = ((X:0; Y:0), (X:1; Y:1));
begin
  with ItemInfo do
    Result := Offsets[Pushed or Selected];
end;

procedure TTBXDefaultTheme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  BgColor: TColor;
  HiContrast: Boolean;
  IsMenuItem: Boolean;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0);

    if (IsMenuItem and USE_FLATMENUS) or (not IsMenuItem and USE_THEMES) then
    begin
    { The icon painting here is not really made by the uxtheme.dll, this is
      just a simulation until I figure out how to work with DrawThemedIcon function }
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      BgColor := GetItemImageBackground(ItemInfo);
      HiContrast := not IsMenuItem and IsDarkColor(BGColor);
      if not Enabled then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else if Selected or Pushed or (HoverKind <> hkNone) then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else if HiContrast or TBXHiContrast or TBXLoColor then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else
        HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 178);
    end
    else
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      ImageList.Draw(Canvas, ARect.Left, ARect.Top, ImageIndex, Enabled);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
const
  PushedFlags: array[Boolean] of UINT = (0, DFCS_PUSHED);
var
  XPPart, XPFlags: Cardinal;
begin
  if USE_THEMES then
  begin
    case ButtonKind of
      DFCS_CAPTIONMIN: XPPart := WP_MDIMINBUTTON;
      DFCS_CAPTIONRESTORE: XPPart := WP_MDIRESTOREBUTTON;
      DFCS_CAPTIONCLOSE: XPPart := WP_MDICLOSEBUTTON;
    else
      XPPart := 0;
    end;
    if ItemInfo.Pushed then XPFlags := CBS_PUSHED
    else if ItemInfo.HoverKind <> hkNone then XPFlags := CBS_HOT
    else XPFlags := CBS_NORMAL;
    DrawThemeBackground(WINDOW_THEME, Canvas.Handle, XPPart, XPFLags, ARect, nil);
  end
  else
  begin
    DrawFrameControl(Canvas.Handle, ARect, DFC_CAPTION,
      ButtonKind or PushedFlags[ItemInfo.Pushed]);
  end;
end;

procedure TTBXDefaultTheme.PaintMenuItemFrame(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
begin
  with ItemInfo do if (Enabled and (HoverKind <> hkNone)) or
    (not Enabled and (HoverKind = hkKeyboardHover)) then
    FillRectEx(Canvas, ARect, clHighlight);
end;

procedure TTBXDefaultTheme.PaintMenuItem(Canvas: TCanvas; var ARect: TRect;
  var ItemInfo: TTBXItemInfo);
var
  R: TRect;
  ShowImageOrCheck: Boolean;
  ShowHover: Boolean;
  IsComboItem: Boolean;
  X, Y: Integer;
  ArrowWidth: Integer;

  procedure DrawArrow(AColor: TColor);
  begin
    Canvas.Pen.Color := AColor; Canvas.Brush.Color := AColor;
    Canvas.Polygon([Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)]);
  end;

begin
  with ItemInfo do
  begin
    ShowImageOrCheck := (ImageWidth > 0) or Selected;
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);

    R := ARect;
    if ShowImageOrCheck then Inc(R.Left, ItemInfo.PopupMargin + MenuImageTextSpace);
    IsComboItem := ((ItemOptions and IO_COMBO) <> 0);
    if IsComboItem and Enabled then Dec(R.Right, ArrowWidth);

    PaintMenuItemFrame(Canvas, R, ItemInfo);

    if IsComboItem then
    begin
      R.Left := ARect.Right - ArrowWidth;
      R.Right := ARect.Right;
      if Enabled and (HoverKind <> hkNone) then
        Windows.DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT)
      else
      begin
        Dec(R.Left);
        if not ShowHover then DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_LEFT)
        else DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_LEFT);
      end;
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3 - 2;
      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then DrawArrow(clBtnShadow)
        else
        begin
          Inc(X); Inc(Y);
          DrawArrow(clBtnHighlight);
          Dec(X); Dec(Y);
          DrawArrow(clBtnShadow);
        end;
      end
      else if (HoverKind <> hkNone) and not IsComboItem then DrawArrow(clHighlightText)
      else DrawArrow(clPopupText);
    end;

    if Enabled and ShowImageOrCheck and ((HoverKind <> hkNone) or Selected) then
    begin
      R.Left := ARect.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      if USE_FLATMENUS then FillRectEx(Canvas, R, ToolbarColor);
      PaintButton(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo);
begin
{$IFNDEF OFFICE2K_COMBOS}
  if (PopupInfo.ViewType and PVT_LISTBOX) = PVT_LISTBOX then
  begin
    FrameRectEx(Canvas, R, clWindowFrame, True);
    FrameRectEx(Canvas, R, clWindow, True);
    FrameRectEx(Canvas, R, clWindow);
  end
  else
{$ENDIF}
  if USE_FLATMENUS and ((PopupInfo.ViewType and PVT_TOOLBOX) <> PVT_TOOLBOX) then
  begin
    FrameRectEx(Canvas, R, clBtnShadow, True);
    FrameRectEx(Canvas, R, clPopup, True);
    FrameRectEx(Canvas, R, clPopup);
  end
  else if (PopupInfo.ViewType and PVT_TOOLBOX) = PVT_TOOLBOX then
  begin
    Windows.DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
    FrameRectEx(Canvas, R, ToolbarColor);
  end
  else
  begin
    Windows.DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
    FrameRectEx(Canvas, R, clPopup);
  end;
end;

procedure TTBXDefaultTheme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
const
  XPFlags: array [Boolean] of Integer = (TP_SEPARATOR, TP_SEPARATORVERT);
var
  D: Integer;
begin
  { Note: for blank separators, Enabled = False }
  with ItemInfo, ARect do if Enabled then
  begin
    if not USE_THEMES or ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) then
    begin
      D := 0;
      if LineSeparator then
        if (ViewType and TVT_FLOATING) <> 0 then D := 1
        else D := 4;

      if Horizontal then
      begin
        if (ItemOptions and IO_TOOLBARSTYLE) = 0 then D := 12;
        Top := Bottom div 2 - 1;
        Inc(Left, D); Dec(Right, D);
        Windows.DrawEdge(Canvas.Handle, ARect, EDGE_ETCHED, BF_TOP);
      end
      else
      begin
        Left := Right div 2 - 1;
        Inc(Top, D); Dec(Bottom, D);
        Windows.DrawEdge(Canvas.Handle, ARect, EDGE_ETCHED, BF_LEFT);
      end;
    end
    else
      DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, XPFlags[Horizontal], TS_NORMAL, ARect, nil);
  end;
end;

procedure TTBXDefaultTheme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleSizes: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((9, 0, 6), (14, 14, 14));
  DragHandleOffsets: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0, 2), (3, 0, 5));
  GripperPart: array [Boolean] of Cardinal = (RP_GRIPPER, RP_GRIPPERVERT);
var
  Sz: Integer;
  R2: TRect;
  Flags: Cardinal;
  Z: Integer;
//  SaveColor: TColor;
//  SaveStyle: TBrushStyle;
  BtnVisible, Horz, CloseButtondown, CloseButtonHover: Boolean;

  procedure DrawButtonBitmap;
  const
    ROP_DSPDxax = $00E20746;
    Pattern: array [0..15] of Byte =
      (0, 0, $CC, 0, $78, 0, $30, 0, $78, 0, $CC, 0, 0, 0, 0, 0);
  var
    Bmp: TBitmap;
    W, H: Integer;
    OldBrush: HBRUSH;
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Handle := CreateBitmap(8, 8, 1, 1, @Pattern);
      SetTextColor(Canvas.Handle, clBlack);
      SetBkColor(Canvas.Handle, clWhite);
      OldBrush := SelectObject(Canvas.Handle, GetSysColorBrush(COLOR_BTNTEXT));
      W := 7;
      H := 7;
      with R2 do
        BitBlt(Canvas.Handle, (Left + Right - W + 1) div 2, (Top + Bottom - H) div 2, W, H,
          Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      SelectObject(Canvas.Handle, OldBrush);
    finally
      Bmp.Free;
    end;
  end;

begin
//  SaveColor := Canvas.Brush.Color;
//  SaveStyle := Canvas.Brush.Style;

  Canvas.FillRect(R);

  { Border }
  if USE_THEMES then
  begin
    if ToolbarInfo.BorderStyle = bsSingle then
      DrawThemeEdge(TOOLBAR_THEME, Canvas.Handle, RP_BAND, 0, R,
        BDR_RAISEDINNER, BF_RECT or BF_ADJUST, @R);
  end
  else
  begin
    if ToolbarInfo.BorderStyle = bsSingle then
      Windows.DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);
  end;

  if not ToolbarInfo.AllowDrag then Exit;

  BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
  Sz := GetTBXDragHandleSize(ToolbarInfo);
  Horz := not ToolbarInfo.IsVertical;
  if Horz then R.Right := R.Left + Sz
  else R.Bottom := R.Top + Sz;

  { Drag handle area }
  if ToolbarInfo.DragHandleStyle <> DHS_NONE then
  begin
    if USE_THEMES then
    begin
      R2 := R;
      if BtnVisible then
        if Horz then Inc(R2.Top, Sz - 1)
        else Dec(R2.Right, Sz - 1);

      if Horz then
      begin
        R2.Left := (R2.Left + R2.Right - 6) div 2;
        R2.Right := R2.Left + 6;
        Dec(R2.Bottom);
      end
      else
      begin
        R2.Top := (R2.Top + R2.Bottom - 6) div 2;
        R2.Bottom := R2.Top + 6;
        Dec(R2.Right);
      end;
      DrawThemeBackground(REBAR_THEME, Canvas.Handle,
        GripperPart[ToolbarInfo.IsVertical], 0, R2, nil)
    end
    else
    begin
      R2 := R;
      if Horz then
      begin
        Inc(R2.Left, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Inc(R2.Top, Sz - 2);
        R2.Right := R2.Left + 3;
        InflateRect(R2, 0, -1);
      end
      else
      begin
        Inc(R2.Top, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Dec(R2.Right, Sz - 2);
        R2.Bottom := R2.Top + 3;
        InflateRect(R2, -1, 0);
      end;

      Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
      Canvas.Pixels[R2.Left, R2.Bottom - 1] := clBtnHighlight;
      if ToolbarInfo.DragHandleStyle = DHS_DOUBLE then
      begin
        if Horz then OffsetRect(R2, 3, 0)
        else OffsetRect(R2, 0, 3);
        Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
        Canvas.Pixels[R2.Left, R2.Bottom - 1] := clBtnHighlight;
      end;
    end;
  end;

  { Close Button }
  if BtnVisible then
  begin
    CloseButtonDown := (ToolbarInfo.CloseButtonState and CDBS_PRESSED) <> 0;
    CloseButtonHover := (ToolbarInfo.CloseButtonState and CDBS_HOT) <> 0;
    R2 := GetTBXDockedCloseButtonRect(ToolbarInfo);
    Z := 2;
    if USE_THEMES then Z := 1;
    if Horz then
    begin
      Dec(R2.Bottom, Z);
      Dec(R2.Right, Z);
    end
    else
    begin
      Dec(R2.Bottom, Z);
      Inc(R2.Left, Z);
    end;
    if USE_THEMES then
    begin
      Flags := TS_NORMAL;
      if CloseButtonDown then Flags := TS_PRESSED
      else if CloseButtonHover then Flags := TS_HOT;
      DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, TP_BUTTON, Flags, R2, nil);
      if CloseButtonDown then OffsetRect(R2, 1, 1);
      DrawButtonBitmap;
    end
    else
    begin
      if CloseButtonDown then
      begin
        Windows.DrawEdge(Canvas.Handle, R2, BDR_SUNKENOUTER, BF_RECT);
        OffsetRect(R2, 1, 1);
      end
      else if CloseButtonHover then
        Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
      DrawButtonBitmap;
    end;
  end;
end;

procedure TTBXDefaultTheme.PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer);
begin
  if not USE_THEMES then Exit;
  DrawThemeBackground(REBAR_THEME, Canvas.Handle, 0, 0, DockRect, nil);
end;

procedure TTBXDefaultTheme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo);
var
  Sz: Integer;
  R2: TRect;
  Flags: Integer;
  CloseButtonDown, CloseButtonHover: Boolean;
begin
  with Canvas, DockPanelInfo do
  begin
    Sz := GetSystemMetrics(SM_CYSMCAPTION);

    { Border }
    FrameRectEx(Canvas, R, ToolbarColor, True);
    R2 := R;
    if ShowCaption then
      if IsVertical then Inc(R2.Top, Sz)
      else Inc(R2.Left, Sz);
    Brush.Color := clWindow;
    FrameRect(R2);

    if not ShowCaption then Exit;

    { Caption area }
    if IsVertical then R.Bottom := R.Top + Sz
    else R.Right := R.Left + Sz;
    Windows.DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);

    { Close button }
    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      CloseButtonDown := (CloseButtonState and CDBS_PRESSED) <> 0;
      CloseButtonHover := (CloseButtonState and CDBS_HOT) <> 0;
      R2 := R;
      Brush.Color := ToolbarColor;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz;
        R.Right := R2.Left;
        FillRect(R2);
        InflateRect(R2, -1, -1);
        Inc(R2.Left);
      end
      else
      begin
        R2.Top := R2.Bottom - Sz;
        R.Bottom := R2.Top;
        FillRect(R2);
        InflateRect(R2, -1, -1);
        Dec(R2.Bottom);
      end;

      if USE_THEMES then
      begin
        Flags := TS_NORMAL;
        if CloseButtonDown then Flags := TS_PRESSED
        else if CloseButtonHover then Flags := TS_HOT;
        DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, TP_BUTTON, Flags, R2, nil);
        if CloseButtonDown then OffsetRect(R2, 1, 1);
        InflateRect(R2, -2, -2);
      end
      else
      begin
        if CloseButtonDown then
        begin
          Windows.DrawEdge(Canvas.Handle, R2, BDR_SUNKENOUTER, BF_RECT);
          OffsetRect(R2, 1, 1);
        end
        else if CloseButtonHover then
          Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
        InflateRect(R2, -2, -2);
      end;
      DrawButtonBitmap(Canvas, R2);
    end;

    { Caption }
    Brush.Color := ToolbarColor;
    FillRect(R);
    if IsVertical then InflateRect(R, -2, 0)
    else Inflaterect(R, 0, -2);
    Font.Assign(SmCaptionFont);
    Font.Color := clBtnText;
    Brush.Style := bsClear;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then DrawText(Canvas.Handle, Caption, -1, R, Flags)
    else DrawRotatedText(Canvas.Handle, string(Caption), R, Flags);
    Brush.Style := bsSolid;
  end;
end;

function TTBXDefaultTheme.GetPopupShadowType: Integer;
begin
  Result := PST_WINDOWSXP;
end;

procedure TTBXDefaultTheme.GetViewMargins(ViewType: Integer;
  out Margins: TTBXMargins);
begin
  with Margins do
    if ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_FLOATING) = TVT_FLOATING) then
    begin
      LeftWidth := 4;
      TopHeight := 2;
      RightWidth := 4;
      BottomHeight := 1;
    end
    else
    begin
      LeftWidth := 0;
      TopHeight := 0;
      RightWidth := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXDefaultTheme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  Flags: Integer;
  X, Y, Sz: Integer;
begin
  R := ARect;
  if USE_THEMES then
  begin
    if Hot then Flags := TS_PRESSED
    else Flags := TS_HOT;
    DrawThemeBackground(TOOLBAR_THEME, Canvas.Handle, TP_BUTTON, Flags, ARect, nil);
    X := (R.Left + R.Right) div 2;
    Y := (R.Top + R.Bottom) div 2;
    Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
    if Hot then Canvas.Pen.Color := clBtnText;
    Canvas.Brush.Color := Canvas.Pen.Color;
    case ButtonType of
      PSBT_UP:
        begin
          Inc(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
        end;
      PSBT_DOWN:
        begin
          Y := (R.Top + R.Bottom - 1) div 2;
          Dec(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
        end;
      PSBT_LEFT:
        begin
          Inc(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
        end;
      PSBT_RIGHT:
        begin
          X := (R.Left + R.Right - 1) div 2;
          Dec(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
        end;
    end;
  end
  else
  begin
    if Hot then Flags := DFCS_FLAT
    else Flags := 0;
    case ButtonType of
      PSBT_UP: Flags := Flags or DFCS_SCROLLUP;
      PSBT_DOWN: Flags := Flags or DFCS_SCROLLDOWN;
      PSBT_LEFT: Flags := Flags or DFCS_SCROLLLEFT;
      PSBT_RIGHT: Flags := Flags or DFCS_SCROLLRIGHT;
    end;
    Windows.DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
  end;
end;

procedure TTBXDefaultTheme.PaintGlyph(Canvas: TCanvas; R: TRect; Kind, State: Integer);
var
  X, Y, Flags: Integer;
  C: TColor;

  function FrameColor: TColor;
  begin
    if Boolean(State and GS_DISABLED) then Result := clBtnShadow
    else if Boolean(State and (GS_PUSHED or GS_HOT)) then Result := clNone
    else Result := clBtnShadow;
  end;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(Canvas, Right - 2 - X, Bottom - 2, Right - 1, Bottom - X - 3, C);
    Inc(X);
  end;

begin
  with Canvas do case Kind of
    GK_CHECKBOX:
      begin
        if USE_THEMES then
        begin
          if Boolean(State and GS_CHECKED) then Flags := CBS_CHECKEDNORMAL
          else if Boolean(State and GS_MIXED) then Flags := CBS_MIXEDNORMAL
          else Flags := CBS_UNCHECKEDNORMAL;
          if Boolean(State and GS_DISABLED) then Inc(Flags, 3)
          else if Boolean(State and GS_PUSHED) then Inc(Flags, 2)
          else if Boolean(State and GS_HOT) then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, Handle, BP_CHECKBOX, Flags, R, nil);
        end
        else
        begin
          C := FrameColor;
          if C = clNone then
          begin
            if Boolean(State and GS_MIXED) then Flags := DFCS_BUTTON3STATE or DFCS_CHECKED
            else Flags := DFCS_BUTTONCHECK;
            if Boolean(State and GS_CHECKED) then Flags := Flags or DFCS_CHECKED;
            if Boolean(State and GS_PUSHED) then Flags := Flags or DFCS_PUSHED;
            DrawFrameControl(Handle, R, DFC_BUTTON, Flags);
          end
          else
          begin
            InflateRect(R, -1, -1);
            FrameRectEx(Canvas, R, C, True);
            if Boolean(State and (GS_DISABLED or GS_PUSHED)) then FillRectEx(Canvas, R, clBtnFace)
            else if Boolean(State and GS_MIXED) then DitherRect(Canvas, R, clWindow, clBtnFace)
            else FillRectEx(Canvas, R, clWindow);
          
            if Boolean(State and (GS_CHECKED or GS_MIXED)) then
            begin
              X := (R.Left + R.Right) div 2 - 1;
              Y := (R.Top + R.Bottom) div 2 + 1;
              if Boolean(State and GS_DISABLED) then Pen.Color := clGrayText
              else if Boolean(State and GS_MIXED) then Pen.Color := clBtnShadow
              else Pen.Color := clBtnText;
              Brush.Color := Pen.Color;
              Polygon([Point(X-2, Y), Point(X, Y+2), Point(X+4, Y-2),
                Point(X+4, Y-4), Point(X, Y), Point(X-2, Y-2), Point(X-2, Y)]);
            end;
          end;
        end;
      end;
    GK_RADIOBUTTON:
      begin
        if USE_THEMES then
        begin
          if Boolean(State and GS_CHECKED) then Flags := RBS_CHECKEDNORMAL
          else Flags := RBS_UNCHECKEDNORMAL;
          if Boolean(State and GS_DISABLED) then Inc(Flags, 3)
          else if Boolean(State and GS_PUSHED) then Inc(Flags, 2)
          else if Boolean(State and GS_HOT) then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, Handle, BP_RADIOBUTTON, Flags, R, nil);
        end
        else
        begin
          C := FrameColor;
          if C = clNone then
          begin
            Flags := DFCS_BUTTONRADIO;
            if Boolean(State and GS_CHECKED) then Flags := Flags or DFCS_CHECKED;
            if Boolean(State and GS_PUSHED) then Flags := Flags or DFCS_PUSHED;
            DrawFrameControl(Handle, R, DFC_BUTTON, Flags);
          end
          else
          begin
            Pen.Color := C;
            if Boolean(State and (GS_DISABLED or GS_PUSHED)) then Brush.Color := clBtnFace
            else Brush.Color := clWindow;
            InflateRect(R, -1, -1); Inc(R.Left); Dec(R.Bottom);
            with R do RoundRect(Left, Top, Right, Bottom, Right - Left - 2, Bottom - Top - 2);

            if Boolean(State and GS_CHECKED) then
            begin
              InflateRect(R, -3, -3);
              Brush.Color := clBtnText;
              Pen.Color := clBtnText;
              with R do RoundRect(Left, Top, Right, Bottom, Right - Left, Bottom - Top);
            end;
          end;
        end;
      end;
  end;
end;

procedure TTBXDefaultTheme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);
var
  D, Sz, I, Flags: Integer;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(Canvas, Right - 2 - D, Bottom - 2, Right - 1, Bottom - D - 3, C);
    Inc(D);
  end;

begin
  with Canvas do
    case Part of
      SBP_BODY:
        begin
          if USE_THEMES then
            DrawThemeBackground(STATUSBAR_THEME, Canvas.Handle, 0, 0, R, nil)
          else
            FillRectEx(Canvas, R, clBtnFace);
        end;
      SBP_PANE, SBP_LASTPANE:
        begin
          if USE_THEMES then
          begin
            if Part = SBP_LASTPANE then Flags := SP_GRIPPERPANE
            else Flags := SP_PANE;
            DrawThemeBackground(STATUSBAR_THEME, Canvas.Handle, Flags, 0, R, nil);
          end
          else
          begin
            if Part = SBP_PANE then Dec(R.Right, 2);
            Frame3D(Canvas, R, clBtnShadow, clBtnHighlight);
          end;
        end;
      SBP_GRIPPER:
        begin
          if USE_THEMES then
            DrawThemeBackground(STATUSBAR_THEME, Canvas.Handle, SP_GRIPPER, 0, R, nil)
          else
          begin
            D := 0;
            Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
            for I := 1 to 3 do
              case Sz of
                0..8:
                  begin
                    DiagLine(clBtnShadow);
                    DiagLine(clBtnHighlight);
                  end;
                9..11:
                  begin
                    DiagLine(clBtnFace);
                    DiagLine(clBtnShadow);
                    DiagLine(clBtnHighlight);
                  end;
                12..14:
                  begin
                    DiagLine(clBtnShadow);
                    DiagLine(clBtnShadow);
                    DiagLine(clBtnHighlight);
                  end;
              else
                DiagLine(clBtnFace);
                DiagLine(clBtnShadow);
                DiagLine(clBtnShadow);
                DiagLine(clBtnHighlight);
              end;

            with Canvas, R do
            begin
              Pen.Color := clBtnFace;
              MoveTo(Right - D - 1, Bottom - 1);
              LineTo(Right - 1, Bottom - 1);
              LineTo(Right - 1, Bottom - D - 2);
            end;
          end;
        end;
    end;
end;

procedure TTBXDefaultTheme.SetupColorCache;
begin
  ToolbarColor := clToolbar;
end;

procedure TTBXDefaultTheme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
