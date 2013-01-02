unit TBXUtils;

// TBX Package
// Copyright 2001-2002 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXUtils.pas,v 1.31 2003/05/31 19:58:22 Alex Exp $

interface

{$I TB2Warn.inc}
{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, Forms, ImgList;

procedure GetRGB(C: TColor; out R, G, B: Integer);
function MixColors(C1, C2: TColor; W1: Integer): TColor;
function SameColors(C1, C2: TColor): Boolean;
function Lighten(C: TColor; Amount: Integer): TColor;
function NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
function ColorIntensity(C: TColor): Integer;
function IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
function Blend(C1, C2: TColor; W1: Integer): TColor;
procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
procedure DitherRect(Canvas: TCanvas; const R: TRect; C1, C2: TColor);
procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor);
function  FillRectEx(Canvas: TCanvas; const Rect: TRect; Color: TColor): Boolean;
function  FillRectEx2(DC: HDC; const Rect: TRect; Color: TColor): Boolean;
function  FrameRectEx(Canvas: TCanvas; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean;
procedure DrawLineEx(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure DrawDraggingOutline(DC: HDC; const NewRect, OldRect: TRect);
function  GetBGR(C: TColor): Cardinal;
procedure FillLongword(var X; Count: Integer; Value: Longword);
procedure MoveLongword(const Source; var Dest; Count: Integer);

procedure DrawTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HiContrast: Boolean);
procedure BlendTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Opacity: Byte);
procedure HighlightTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HighlightColor: TColor; Amount: Byte);
procedure DrawTBXIconShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Density: Integer);
procedure DrawTBXIconFlatShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
procedure DrawTBXIconFullShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);

function GetClientSizeEx(Control: TWinControl): TPoint;

const
  SHD_DENSE = 0;
  SHD_LIGHT = 1;

{ An additional declaration for D4 compiler }
type
  PColor = ^TColor;

{ Stock Objects }
var
  StockBitmap1, StockBitmap2: TBitmap;
  StockMonoBitmap, StockCompatibleBitmap: TBitmap;
  SmCaptionFont: TFont;

const
  ROP_DSPDxax = $00E20746;

{$DEFINE WIN4_SHADOW} // Uncomment to disable shadow simulation in Win98/ME
{ Support for window shadows }
type
  TShadowEdges = set of (seTopLeft, seBottomRight);
  TShadowStyle = (ssFlat, ssLayered{$IFDEF WIN4_SHADOW}, ssAlphaBlend{$ENDIF});

  TShadow = class(TCustomControl)
  protected
    FOpacity: Byte;
    FBuffer: TBitmap;
    FClearRect: TRect;
    FEdges: TShadowEdges;
    FStyle: TShadowStyle;
    FSaveBits: Boolean;
    procedure GradR(const R: TRect);
    procedure GradB(const R: TRect);
    procedure GradBR(const R: TRect);
    procedure GradTR(const R: TRect);
    procedure GradBL(const R: TRect);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FillBuffer; virtual; abstract;
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
{$IFDEF WIN4_SHADOW}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
{$ENDIF}
  public
    constructor Create(const Bounds: TRect; Opacity: Byte; LoColor: Boolean; Edges: TShadowEdges); reintroduce;
    procedure Clear(const R: TRect);
    procedure Render;
    procedure Show(ParentHandle: HWND);
  end;

  THorzShadow = class(TShadow)
  protected
    procedure FillBuffer; override;
  end;

  TVertShadow = class(TShadow)
  protected
    procedure FillBuffer; override;
  end;

  TShadows = class
  private
    FSaveBits: Boolean;
    procedure SetSaveBits(Value: Boolean);
  protected
    V1: TShadow;
    H1: TShadow;
    V2: TShadow;
    H2: TShadow;
    V3: TShadow;
    H3: TShadow;
  public
    constructor Create(R1, R2: TRect; ASize: Integer; Opacity: Byte; LoColor: Boolean);
    destructor Destroy; override;
    procedure Show(ParentHandle: HWND);
    property SaveBits: Boolean read FSaveBits write SetSaveBits;
  end;

procedure RecreateStock;

type
  PBlendFunction = ^TBlendFunction;
  TBlendFunction = packed record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

  TUpdateLayeredWindow = function(
    hWnd : hWnd;
    hdcDst : hDC;
    pptDst : PPoint;
    psize : PSize;
    hdcSrc : hDC;
    pptSrc : PPoint;
    crKey : TColorRef;
    pblend : PBlendFunction;
    dwFlags : Integer
  ): Integer; stdcall;

var
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
{$IFDEF WIN4_SHADOW}
  MSIMG_MODULE: THandle;
  AlphaBlend: function(
    hdcDest: HDC;
    nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer;
    hdcSrc: HDC;
    nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer;
    blendFunction: TBlendFunction
  ): BOOL; stdcall;
{$ENDIF}

implementation

{$R-}{$Q-}

uses TB2Common;

const
  WeightR: single = 0.764706;
  WeightG: single = 1.52941;
  WeightB: single = 0.254902;

procedure GetRGB(C: TColor; out R, G, B: Integer);
begin
  if Integer(C) < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF;
  G := C shr 8 and $FF;
  B := C shr 16 and $FF;
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

function SameColors(C1, C2: TColor): Boolean;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $000000FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := C1 = C2;
end;

function Lighten(C: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  if C < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function NearestMixedColor(C1, C2: TColor; W1: Integer): TColor;
begin
  Result := MixColors(C1, C2, W1);
  Result := GetNearestColor(StockCompatibleBitmap.Canvas.Handle, Result);
end;

function ColorIntensity(C: TColor): Integer;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Result := ((C shr 16 and $FF) * 28 + (C shr 8 and $FF) * 151 +
    (C and $FF) * 77) shr 8;
end;

function IsDarkColor(C: TColor; Threshold: Integer = 100): Boolean;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  Threshold := Threshold shl 8;
  Result := ((C and $FF) * 76 + (C shr 8 and $FF) * 150 + (C shr 16 and $FF) * 30 ) < Threshold;
end;

function Blend(C1, C2: TColor; W1: Integer): TColor;
var
  W2, A1, A2, D, F, G: Integer;
begin
  if C1 < 0 then C1 := GetSysColor(C1 and $FF);
  if C2 < 0 then C2 := GetSysColor(C2 and $FF);

  if W1 >= 100 then D := 1000
  else D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * W2;
  A1 := C1 shr 16 * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * W2;
  A1 := (C1 shr 8 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * W2;
  A1 := (C1 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function ColorDistance(C1, C2: Integer): Single;
var
  DR, DG, DB: Integer;
begin
  DR := (C1 and $FF) - (C2 and $FF);
  Result := Sqr(DR * WeightR);
  DG := (C1 shr 8 and $FF) - (C2 shr 8 and $FF);
  Result := Result + Sqr(DG * WeightG);
  DB := (C1 shr 16) - (C2 shr 16);
  Result := Result + Sqr(DB * WeightB);
  Result := SqRt(Result);
end;

function GetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
  if BkgndIntensity < 220 then Result := (2 - BkgndIntensity / 220) * Threshold
  else Result := Threshold;
end;

function IsContrastEnough(AColor, ABkgndColor: Integer;
  DoAdjustThreshold: Boolean; Threshold: Single): Boolean;
begin
  if DoAdjustThreshold then
    Threshold := GetAdjustedThreshold(ColorDistance(ABkgndColor, $000000), Threshold);
  Result := ColorDistance(ABkgndColor, AColor) > Threshold;
end;

procedure AdjustContrast(var AColor: Integer; ABkgndColor: Integer; Threshold: Single);
var
  x, y, z: Single;
  r, g, b: Single;
  RR, GG, BB: Integer;
  i1, i2, s, q, w: Single;
  DoInvert: Boolean;
begin
  i1 := ColorDistance(AColor, $000000);
  i2 := ColorDistance(ABkgndColor, $000000);
  Threshold := GetAdjustedThreshold(i2, Threshold);

  if i1 > i2 then DoInvert := i2 < 442 - Threshold
  else DoInvert := i2 < Threshold;  

  x := (ABkgndColor and $FF) * WeightR;
  y := (ABkgndColor shr 8 and $FF) * WeightG;
  z := (ABkgndColor shr 16) * WeightB;

  r := (AColor and $FF) * WeightR;
  g := (AColor shr 8 and $FF) * WeightG;
  b := (AColor shr  16) * WeightB;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b := 65 - b;
    x := 195 - x;
    y := 390 - y;
    z := 65 - z;
  end;

  s := Sqrt(Sqr(b) + Sqr(g) + Sqr(r));
  if s < 0.01 then s := 0.01;

  q := (r * x + g * y + b * z) / S;

  x := Q / S * r - x;
  y := Q / S * g - y;
  z := Q / S * b - z;

  w :=  Sqrt(Sqr(Threshold) - Sqr(x) - Sqr(y) - Sqr(z));

  r := (q - w) * r / s;
  g := (q - w) * g / s;
  b := (q - w) * b / s;

  if DoInvert then
  begin
    r := 195 - r;
    g := 390 - g;
    b :=  65 - b;
  end;

  if r < 0 then r := 0 else if r > 195 then r := 195;
  if g < 0 then g := 0 else if g > 390 then g := 390;
  if b < 0 then b := 0 else if b >  65 then b :=  65;

  RR := Trunc(r * (1 / WeightR) + 0.5);
  GG := Trunc(g * (1 / WeightG) + 0.5);
  BB := Trunc(b * (1 / WeightB) + 0.5);

  if RR > $FF then RR := $FF else if RR < 0 then RR := 0;
  if GG > $FF then GG := $FF else if GG < 0 then GG := 0;
  if BB > $FF then BB := $FF else if BB < 0 then BB := 0;

  AColor := (BB and $FF) shl 16 or (GG and $FF) shl 8 or (RR and $FF);
end;

procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
var
  t: Single;
begin
  if Color < 0 then Color := GetSysColor(Color and $FF);
  if BkgndColor < 0 then BkgndColor := GetSysColor(BkgndColor and $FF);
  t := Threshold;
  if not IsContrastEnough(Color, BkgndColor, True, t) then
    AdjustContrast(Integer(Color), BkgndColor, t);
end;

procedure DitherRect(Canvas: TCanvas; const R: TRect; C1, C2: TColor);
var
  B: TBitmap;
  Brush: HBRUSH;
begin
  B := AllocPatternBitmap(C1, C2);
  B.HandleType := bmDDB;
  Brush := CreatePatternBrush(B.Handle);
  FillRect(Canvas.Handle, R, Brush);
  DeleteObject(Brush);
end;

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor);
var
  TopRight, BottomLeft: TPoint;
begin
  with Canvas, Rect do
  begin
    Pen.Width := 1;
    Dec(Bottom); Dec(Right);
    TopRight.X := Right;
    TopRight.Y := Top;
    BottomLeft.X := Left;
    BottomLeft.Y := Bottom;
    Pen.Color := TopColor;
    PolyLine([BottomLeft, TopLeft, TopRight]);
    Pen.Color := BottomColor;
    Dec(BottomLeft.X);
    PolyLine([TopRight, BottomRight, BottomLeft]);
    Inc(Left); Inc(Top);
  end;
end;

function FillRectEx(Canvas: TCanvas; const Rect: TRect; Color: TColor): Boolean;
var
  Brush: HBRUSH;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then
    begin
      Brush := GetSysColorBrush(Color and $000000FF);
      Windows.FillRect(Canvas.Handle, Rect, Brush);
    end
    else
    begin
      Brush := CreateSolidBrush(Color);
      Windows.FillRect(Canvas.Handle, Rect, Brush);
      DeleteObject(Brush);
    end;
  end;
end;

function FillRectEx2(DC: HDC; const Rect: TRect; Color: TColor): Boolean;
var
  Brush: HBRUSH;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then
    begin
      Brush := GetSysColorBrush(Color and $000000FF);
      Windows.FillRect(DC, Rect, Brush);
    end
    else
    begin
      Brush := CreateSolidBrush(Color);
      Windows.FillRect(DC, Rect, Brush);
      DeleteObject(Brush);
    end;
  end;
end;

function FrameRectEx(Canvas: TCanvas; var Rect: TRect; Color: TColor; Adjust: Boolean = False): Boolean;
var
  Brush: HBRUSH;
begin
  Result := Color <> clNone;
  if Result then
  begin
    if Color < 0 then
    begin
      Brush := GetSysColorBrush(Color and $000000FF);
      Windows.FrameRect(Canvas.Handle, Rect, Brush);
    end
    else
    begin
      Brush := CreateSolidBrush(Color);
      Windows.FrameRect(Canvas.Handle, Rect, Brush);
      DeleteObject(Brush);
    end;
  end;
  if Adjust then with Rect do
  begin
    Inc(Left); Dec(Right);
    Inc(Top); Dec(Bottom);
  end;
end;

procedure DrawLineEx(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
begin
  Canvas.Pen.Color := Color;
  MoveToEx(Canvas.Handle, X1, Y1, nil);
  LineTo(Canvas.Handle, X2, Y2);
end;

procedure DrawDraggingOutline(DC: HDC; const NewRect, OldRect: TRect);
var
  Sz: TSize;
begin
  Sz.CX := 3; Sz.CY := 2;
  DrawHalftoneInvertRect(DC, @NewRect, @OldRect, Sz, Sz);
end;

function GetBGR(C: TColor): Cardinal;
asm
        MOV     ECX,EAX         // this function swaps R and B bytes in ABGR
        SHR     EAX,16
        XCHG    AL,CL
        MOV     AH,$00          // and writes $FF into A component
        SHL     EAX,16
        MOV     AX,CX
end;

procedure FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X;  EDX = Count; ECX = Value
        PUSH    EDI
        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit
        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;

procedure MoveLongword(const Source; var Dest; Count: Integer);
asm
// EAX = Source; EDX = Dest; ECX = Count
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX         // Source
        MOV     EDI,EDX         // Destination
        MOV     EAX,ECX         // Counter
        CMP     EDI,ESI
        JE      @exit
        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
end;

procedure DrawTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HiContrast: Boolean);
const
  CWeirdColor = $00203241;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: PColor;
  S, C: TColor;
begin
  if not HiContrast then
  begin
    ImageList.Draw(Canvas, R.Left, R.Top, ImageIndex);
    Exit;
  end;

  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^ and $00FFFFFF;
      if S <> CWeirdColor then
      begin
        C := (S and $FF0000) shr 16 * 76 + (S and $00FF00) shr 8 * 150 +
          (S and $0000FF) * 29;
        if C > $FD00 then S := $000000
        else if C < $6400 then S := $FFFFFF;
        Dst^ := Lighten(S, 32);
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure BlendTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Opacity: Byte);
const
  CWeirdColor = $00203241;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
  Wt1, Wt2: Cardinal;
begin
  Wt2 := Opacity;
  Wt1 := 255 - Wt2;
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^;
      if S <> Dst^ then
      begin
        CBRB := (Dst^ and $00FF00FF) * Wt1;
        CBG  := (Dst^ and $0000FF00) * Wt1;
        C := ((S and $FF00FF) * Wt2 + CBRB) and $FF00FF00 + ((S and $00FF00) * Wt2 + CBG) and $00FF0000;
        Dst^ := C shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure HighlightTBXIcon(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; HighlightColor: TColor; Amount: Byte);
const
  CWeirdColor = $00203241;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: PColor;
  S, C: Cardinal;
  CBRB, CBG: Cardinal;
  W1, W2: Cardinal;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex);

  W2 := Amount;
  W1 := 255 - W2;
  HighlightColor := GetBGR(ColorToRGB(HighlightColor));
  CBRB := (Cardinal(HighlightColor) and $00FF00FF) * W1;
  CBG  := (Cardinal(HighlightColor) and $0000FF00) * W1;

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^ and $00FFFFFF;
      if S <> CWeirdColor then
      begin
        C := ((S and $FF00FF) * W2 + CBRB) and $FF00FF00 + ((S and $00FF00) * W2 + CBG) and $00FF0000;
        Dst^ := C shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure DrawTBXIconShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; Density: Integer);
const
  D_DIV: array [0..2] of Cardinal = (3, 8, 20);
  D_ADD: array [0..2] of Cardinal = (255 - 255 div 3, 255 - 255 div 8, 255 - 255 div 20);
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
begin
  Assert(Density in [0..2]);

  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap1.Width := ImageWidth;
  StockBitmap1.Height := ImageHeight;
  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;

  BitBlt(StockBitmap1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    Canvas.Handle, R.Left, R.Top, SRCCOPY);
  BitBlt(StockBitmap2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    Src := StockBitmap2.ScanLine[J];
    Dst := StockBitmap1.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      S := Src^;
      if S <> Dst^ then
      begin
        CBRB := Dst^ and $00FF00FF;
        CBG  := Dst^ and $0000FF00;
        C := ((S and $FF0000) shr 16 * 29 + (S and $00FF00) shr 8 * 150 +
          (S and $0000FF) * 76) shr 8;
        C := C div D_DIV[Density] + D_ADD[Density];
        Dst^ := ((CBRB * C and $FF00FF00) or (CBG * C and $00FF0000)) shr 8;
      end;
      Inc(Src);
      Inc(Dst);
    end;
  end;
  BitBlt(Canvas.Handle, R.Left, R.Top, ImageWidth, ImageHeight,
    StockBitmap1.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure DrawTBXIconFlatShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
const
  CShadowThreshold = 180 * 256;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  P: ^Cardinal;
  C: Cardinal;
  SrcDC, DstDC: HDC;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;
  StockBitmap2.Canvas.Brush.Color := clWhite;
  StockBitmap2.Canvas.FillRect(Rect(0, 0, ImageWidth, ImageHeight));
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    P := StockBitmap2.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      C := P^ and $00FFFFFF;
      if C <> $0 then
      begin
        C := (C and $FF0000) shr 16 * 76 + (C and $00FF00) shr 8 * 150 +
          (C and $0000FF) * 29;
        if C > CShadowThreshold then P^ := $00FFFFFF
        else P^ := $00000000;
      end;
      Inc(P);
    end;
  end;

  StockMonoBitmap.Width := ImageWidth;
  StockMonoBitmap.Height := ImageHeight;
  StockMonoBitmap.Canvas.Brush.Color := clBlack;
  BitBlt(StockMonoBitmap.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap2.Canvas.Handle, 0, 0, SRCCOPY);

  SrcDC := StockMonoBitmap.Canvas.Handle;
  Canvas.Brush.Color := ColorToRGB(ShadowColor);
  DstDC := Canvas.Handle;
  Windows.SetTextColor(DstDC, clWhite);
  Windows.SetBkColor(DstDC, clBlack);
  BitBlt(DstDC, R.Left, R.Top, ImageWidth, ImageHeight, SrcDC, 0, 0, ROP_DSPDxax);
end;

procedure DrawTBXIconFullShadow(Canvas: TCanvas; const R: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer; ShadowColor: TColor);
const
  CWeirdColor = $00203241;
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  P: ^Cardinal;
  C: Cardinal;
  SrcDC, DstDC: HDC;
begin
  ImageWidth := R.Right - R.Left;
  ImageHeight := R.Bottom - R.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  StockBitmap2.Width := ImageWidth;
  StockBitmap2.Height := ImageHeight;
  for J := 0 to ImageHeight - 1 do
    FillLongWord(StockBitmap2.ScanLine[J]^, ImageWidth, CWeirdColor);
  ImageList.Draw(StockBitmap2.Canvas, 0, 0, ImageIndex, True);

  for J := 0 to ImageHeight - 1 do
  begin
    P := StockBitmap2.ScanLine[J];
    for I := 0 to ImageWidth - 1 do
    begin
      C := P^ and $00FFFFFF;
      if C <> CWeirdColor then P^ := $00000000
      else P^ := $00FFFFFF;
      Inc(P);
    end;
  end;

  StockMonoBitmap.Width := ImageWidth;
  StockMonoBitmap.Height := ImageHeight;
  StockMonoBitmap.Canvas.Brush.Color := clBlack;
  BitBlt(StockMonoBitmap.Canvas.Handle, 0, 0, ImageWidth, ImageHeight,
    StockBitmap2.Canvas.Handle, 0, 0, SRCCOPY);

  SrcDC := StockMonoBitmap.Canvas.Handle;
  Canvas.Brush.Color := ColorToRGB(ShadowColor);
  DstDC := Canvas.Handle;
  Windows.SetTextColor(DstDC, clWhite);
  Windows.SetBkColor(DstDC, clBlack);
  BitBlt(DstDC, R.Left, R.Top, ImageWidth, ImageHeight, SrcDC, 0, 0, ROP_DSPDxax);
end;

type
  TCustomFormAccess = class(TCustomForm);

function GetClientSizeEx(Control: TWinControl): TPoint;
var
  R: TRect;
begin
  if (Control is TCustomForm) and (TCustomFormAccess(Control).FormStyle = fsMDIForm)
    and not (csDesigning in Control.ComponentState) then
    GetWindowRect(TCustomFormAccess(Control).ClientHandle, R)
  else
    R := Control.ClientRect;
  Result.X := R.Right - R.Left;
  Result.Y := R.Bottom - R.Top;
end;

procedure InitializeStock;
var
  NonClientMetrics: TNonClientMetrics;
begin
  StockBitmap1 := TBitmap.Create;
  StockBitmap1.PixelFormat := pf32bit;
  StockBitmap2 := TBitmap.Create;
  StockBitmap2.PixelFormat := pf32bit;
  StockMonoBitmap := TBitmap.Create;
  StockMonoBitmap.Monochrome := True;
  StockCompatibleBitmap := TBitmap.Create;
  StockCompatibleBitmap.Width := 8;
  StockCompatibleBitmap.Height := 8;
  SmCaptionFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    SmCaptionFont.Handle := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);
end;

procedure FinalizeStock;
begin
  SmCaptionFont.Free;
  SmCaptionFont := nil;
  StockCompatibleBitmap.Free;
  StockMonoBitmap.Free;
  StockBitmap2.Free;
  StockBitmap1.Free;
end;

procedure RecreateStock;
begin
  FinalizeStock;
  InitializeStock;
end;

{ TShadow }

procedure TShadow.Clear(const R: TRect);
begin
  FClearRect := R;
end;

constructor TShadow.Create(const Bounds: TRect; Opacity: Byte; LoColor: Boolean; Edges: TShadowEdges);
begin
  inherited Create(nil);
  Hide;
  ParentWindow := Application.Handle;
  BoundsRect := Bounds;
  Color := clBtnShadow;
  FOpacity := Opacity;
  FEdges := Edges;
  FSaveBits := False;

  if LoColor then FStyle := ssFlat
  else if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    FStyle := ssLayered
{$IFDEF WIN4_SHADOW}
  else if @AlphaBlend <> nil then
    FStyle := ssAlphaBlend
{$ENDIF}
  else
    FStyle := ssFlat;
end;

procedure TShadow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := (Style and not (WS_CHILD or WS_GROUP or WS_TABSTOP)) or WS_POPUP;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    if FSaveBits then WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TShadow.GradB(const R: TRect);
var
  J, W, H: Integer;
  V: Cardinal;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    V := (255 - J shl 8 div H) shl 24;
    FillLongword(P^, W, V);
  end;
end;

procedure TShadow.GradBL(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := DMax - 1;
  CY := H - DMax;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.GradBR(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := W - DMax;
  CY := H - DMax;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.GradR(const R: TRect);
var
  I, J, W: Integer;
  P: ^Cardinal;
  ScanLine: array of Cardinal;
begin
  W := R.Right - R.Left;
  SetLength(ScanLine, W);
  for I := 0 to W - 1 do
    ScanLine[I] :=(255 - I shl 8 div W) shl 24;

  for J := R.Top to R.Bottom - 1 do
  begin
    P := FBuffer.ScanLine[J];
    Inc(P, R.Left);
    MoveLongword(ScanLine[0], P^, W);
  end;
end;

procedure TShadow.GradTR(const R: TRect);
var
  I, J, W, H, CX, CY, D, DMax, A, B: Integer;
  P: ^Cardinal;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  DMax := W;
  if H > W then DMax := H;
  CX := W - DMax;
  CY := DMax - 1;
  for J := 0 to H - 1 do
  begin
    P := FBuffer.ScanLine[J + R.Top];
    Inc(P, R.Left);
    for I := 0 to W - 1 do
    begin
      A := Abs(I - CX);
      B := Abs(J - CY);
      D := A;
      if B > A then D := B;
      D := (A + B + D) * 128 div DMax;
      if D < 255 then P^ := (255 - D) shl 24
      else P^ := 0;
      Inc(P);
    end;
  end;
end;

procedure TShadow.Render;
var
  DstDC: HDC;
  SrcPos, DstPos: TPoint;
  LSize: TSize;
  BlendFunc: TBlendFunction;
begin
  if FStyle <> ssLayered then Exit;

  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or $00080000{WS_EX_LAYERED});
  DstDC := GetDC(0);
  try
    SrcPos := Point(0, 0);
    with BoundsRect do
    begin
      DstPos := Point(Left, Top);
      LSize.cx := Right - Left;
      LSize.cy := Bottom - Top;
    end;
    BlendFunc.BlendOp := 0;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := FOpacity;
    BlendFunc.AlphaFormat := 1;

    FBuffer := TBitmap.Create;
    FBuffer.PixelFormat := pf32bit;
    FBuffer.Width := LSize.cx;
    FBuffer.Height := LSize.cy;

    FillBuffer;

    UpdateLayeredWindow(
      Handle,
      DstDC,
      @DstPos,
      @LSize,
      FBuffer.Canvas.Handle,
      @SrcPos,
      0,
      @BlendFunc,
      $00000002{ULW_ALPHA});

    FBuffer.Free;
  finally
    ReleaseDC(0, DstDC);
  end;
end;

procedure TShadow.Show(ParentHandle: HWND);
begin
  SetWindowPos(Handle, ParentHandle, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOSENDCHANGING or SWP_NOMOVE or
    SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_SHOWWINDOW);
end;

{$IFDEF WIN4_SHADOW}
procedure TShadow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  SrcPos, DstPos: TPoint;
  LSize: TSize;
  BlendFunc: TBlendFunction;
begin
  if FStyle = ssAlphaBlend then
  begin
    { Dispatch all the painting messages }
    ProcessPaintMessages;

    SrcPos := Point(0, 0);
    with BoundsRect do
    begin
      DstPos := Point(Left, Top);
      LSize.cx := Right - Left;
      LSize.cy := Bottom - Top;
    end;

    FBuffer := TBitmap.Create;
    FBuffer.PixelFormat := pf32bit;
    FBuffer.Width := LSize.cx;
    FBuffer.Height := LSize.cy;

    FillBuffer;

    { Blend the buffer directly into the screen }
    BlendFunc.BlendOp := 0;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := FOpacity;
    BlendFunc.AlphaFormat := 1;
    AlphaBlend(Message.DC, 0, 0, LSize.cx, LSize.cy,
      FBuffer.Canvas.Handle, 0, 0, LSize.cx, LSize.cy, BlendFunc);
    FBuffer.Free;

    Message.Result := 1;
  end
  else inherited;
end;
{$ENDIF}

procedure TShadow.WMNCHitTest(var Message: TMessage);
begin
  Message.Result := HTTRANSPARENT;
end;

{ THorzShadow }

procedure THorzShadow.FillBuffer;
var
  R: TRect;
  L1, L2, L3: Integer;
begin
  if seTopLeft in FEdges then L1 := Height else L1 := 0;
  if seBottomRight in FEdges then L3 := Height else L3 := 0;
  if L1 + L3 > Width then
  begin
    if (L1 > 0) and (L3 > 0) then
    begin
      L1 := Width div 2;
      L3 := L1;
    end
    else if L1 > 0 then L1 := Width
    else if L3 > 0 then L3 := Width;
  end;
  L2 := Width - L1 - L3;
  R := Rect(0, 0, Width, Height);
  R.Right := R.Left + L1;
  if L1 > 0 then GradBL(R);
  R.Left := R.Right;
  R.Right := R.Left + L2;
  if L2 > 0 then GradB(R);
  if L3 > 0 then
  begin
    R.Left := R.Right;
    R.Right := R.Left + L3;
    GradBR(R);
  end;
end;

{ TVertShadow }

procedure TVertShadow.FillBuffer;
var
  R: TRect;
  L1, L2, L3: Integer;
begin
  if seTopLeft in FEdges then L1 := Width else L1 := 0;
  if seBottomRight in FEdges then L3 := Width else L3 := 0;
  if L1 + L3 > Height then
  begin
    if (L1 > 0) and (L3 > 0) then
    begin
      L1 := Height div 2;
      L3 := L1;
    end
    else if L1 > 0 then L1 := Height
    else if L3 > 0 then L3 := Height;
  end;
  L2 := Height - L1 - L3;

  R := Rect(0, 0, Width, Height);
  R.Bottom := R.Top + L1;
  if L1 > 0 then GradTR(R);
  R.Top := R.Bottom;
  R.Bottom :=  R.Top + L2;
  if L2 > 0 then GradR(R);
  if L3 > 0 then
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Top + L3;
    GradBR(R);
  end;
end;

{ TShadows }

constructor TShadows.Create(R1, R2: TRect; ASize: Integer; Opacity: Byte; LoColor: Boolean);
var
  R: TRect;
  R1Valid, R2Valid: Boolean;
begin
  if LoColor or
    ((@UpdateLayeredWindow = nil) {$IFDEF WIN4_SHADOW}and (@AlphaBlend = nil){$ENDIF}) then
  begin
    ASize := ASize div 2;
  end;

  R1Valid := not IsRectEmpty(R1);
  R2Valid := not IsRectEmpty(R2);
  if not (R1Valid or R2Valid) then Exit;

  if R1Valid xor R2Valid then
  begin
    { A simple square shadow }
    if R1Valid then R := R1 else R:= R2;
    with R do
    begin
      V1 := TVertShadow.Create(Rect(Right, Top + ASize, Right + ASize, Bottom), Opacity, LoColor, [seTopLeft]);
      H1 := THorzShadow.Create(Rect(Left + ASize, Bottom, Right + ASize, Bottom + ASize), Opacity, LoColor, [seTopLeft, seBottomRight])
    end;
  end
  else
  begin

    if (R1.Bottom <= R2.Top + 2) or (R1.Top >= R2.Bottom - 2) then
    begin
      if R1.Top > R2.Top then
      begin
        R := R2;
        R2 := R1;
        R1 := R;
      end;
      if R1.Left + ASize < R2.Left then
        H1 := THorzShadow.Create(Rect(R1.Left + ASize, R1.Bottom, R2.Left, R1.Bottom + ASize), Opacity, LoColor, [seTopLeft]);
      H2 := THorzShadow.Create(Rect(R2.Left + ASize, R2.Bottom, R2.Right + ASize, R2.Bottom + ASize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      V1 := TVertShadow.Create(Rect(R1.Right, R1.Top + ASize, R1.Right + ASize, R1.Bottom), Opacity, LoColor, [seTopLeft]);
      if R1.Right > R2.Right then
        H3 := THorzShadow.Create(Rect(R2.Right, R1.Bottom, R1.Right + ASize, R1.Bottom + ASize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      if R1.Right + ASize < R2.Right then
        V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + ASize, R2.Right + ASize, R2.Bottom), Opacity, LoColor, [seTopLeft])
      else
        V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + 1, R2.Right + ASize, R2.Bottom), Opacity, LoColor, []);
    end
    else if (R1.Right <= R2.Left + 2) or (R1.Left >= R2.Right - 2) then
    begin
      if R1.Left > R2.Left then
      begin
        R := R2;
        R2 := R1;
        R1 := R;
      end;
      if R1.Top + ASize < R2.Top then
        V1 := TVertShadow.Create(Rect(R1.Right, R1.Top + ASize, R1.Right + ASize, R2.Top), Opacity, LoColor, [seTopLeft]);
      V2 := TVertShadow.Create(Rect(R2.Right, R2.Top + ASize, R2.Right + ASize, R2.Bottom + ASize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      H1 := THorzShadow.Create(Rect(R1.Left + ASize, R1.Bottom, R1.Right, R1.Bottom + ASize), Opacity, LoColor, [seTopLeft]);
      if R1.Bottom > R2.Bottom then
        V3 := TVertShadow.Create(Rect(R1.Right, R2.Bottom, R1.Right + ASize, R1.Bottom + ASize), Opacity, LoColor, [seTopLeft, seBottomRight]);
      if R1.Bottom + ASize < R2.Bottom then
        H2 := THorzShadow.Create(Rect(R2.Left + ASize, R2.Bottom, R2.Right, R2.Bottom + ASize), Opacity, LoColor, [seTopLeft])
      else
        H2 := THorzShadow.Create(Rect(R2.Left, R2.Bottom, R2.Right, R2.Bottom + ASize), Opacity, LoColor, []);
    end;
  end;

  if V1 <> nil then V1.Render;
  if H1 <> nil then H1.Render;
  if V2 <> nil then V2.Render;
  if H2 <> nil then H2.Render;
  if V3 <> nil then V3.Render;
  if H3 <> nil then H3.Render;

  SetSaveBits(True);
end;

destructor TShadows.Destroy;
begin
  H3.Free;
  V3.Free;
  H2.Free;
  V2.Free;
  H1.Free;
  V1.Free;
  inherited;
end;

procedure TShadows.SetSaveBits(Value: Boolean);
begin
  FSaveBits := Value;
  if V1 <> nil then V1.FSaveBits := Value;
  if H1 <> nil then H1.FSaveBits := Value;
  if V2 <> nil then V2.FSaveBits := Value;
  if H2 <> nil then H2.FSaveBits := Value;
  if V3 <> nil then V3.FSaveBits := Value;
  if H3 <> nil then H3.FSaveBits := Value;
end;

procedure TShadows.Show(ParentHandle: HWND);
begin
  if V1 <> nil then V1.Show(ParentHandle);
  if H1 <> nil then H1.Show(ParentHandle);
  if V2 <> nil then V2.Show(ParentHandle);
  if H2 <> nil then H2.Show(ParentHandle);
  if V3 <> nil then V3.Show(ParentHandle);
  if H3 <> nil then H3.Show(ParentHandle);
end;


initialization

InitializeStock;
@UpdateLayeredWindow := GetProcAddress(GetModuleHandle(user32), 'UpdateLayeredWindow');

{$IFDEF WIN4_SHADOW}
MSIMG_MODULE := LoadLibrary('msimg32.dll');
if MSIMG_MODULE <> 0 then
  @AlphaBlend := GetProcAddress(MSIMG_MODULE, 'AlphaBlend')
else
begin
  FreeLibrary(MSIMG_MODULE);
  MSIMG_MODULE := 0;
end;
{$ENDIF}

finalization

{$IFDEF WIN4_SHADOW}
if MSIMG_MODULE <> 0 then FreeLibrary(MSIMG_MODULE);
{$ENDIF}

FinalizeStock;

end.
