
{*******************************************************************}
{                                                                   }
{   dxCoreUtils (Design eXperience)                                 }
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

unit dxCoreUtils;

interface

{$I dxWarn.inc}

uses
  Windows, Graphics, SysUtils, Classes, Controls, TypInfo, Types, dxCore;

{ dxMethodsEqual }

function dxMethodsEqual(const Method1, Method2: TMethod): Boolean;

{ dxDrawLine }

procedure dxDrawLine(const ACanvas: TCanvas; const x1, y1, x2, y2: Integer);

{ dxCreateGradientRect }

procedure dxCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const Colors: TdxGradientColors; const Style: TdxGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);

{ dxAdjustBoundRect }

procedure dxAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TdxBoundLines;
  var Rect: TRect);

{ dxDrawBoundLines }
procedure dxDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TdxBoundLines;
  const AColor: TColor; const Rect: TRect);

//
// attic!
//

procedure dxConvertToGray2(Bitmap: TBitmap);
procedure dxRenderText(const AParent: TControl; const ACanvas: TCanvas;
  AText: string; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var Rect: TRect; Flags: Integer);
procedure dxFrame3d(const ACanvas: TCanvas; const Rect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);
procedure dxColorizeBitmap(Bitmap: TBitmap; const AColor: TColor);
procedure dxSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
procedure dxPlaceText(const AParent: TControl; const ACanvas: TCanvas;
  const AText: string; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  const AAlignment: TAlignment; const AWordWrap: Boolean; var Rect: TRect);

implementation

{-----------------------------------------------------------------------------
  Procedure: dxMethodsEqual
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const M1, M2: TMethod
  Result:    Boolean
-----------------------------------------------------------------------------}

function dxMethodsEqual (const Method1, Method2: TMethod): Boolean;
begin
  Result :=
    (Method1.Code = Method2.Code) and (Method1.Data = Method2.Data);
end;

{-----------------------------------------------------------------------------
  Procedure: dxCreateGradientRect
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const AWidth, AHeight: Integer; const StartColor, EndColor: TColor; const Colors: TdxGradientColors; const Style: TdxGradientStyle; const Dithered: Boolean; var Bitmap: TBitmap
  Result:    None
-----------------------------------------------------------------------------}

procedure dxCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const Colors: TdxGradientColors; const Style: TdxGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
const
  PixelCountMax = 32768;
type
  TGradientBand = array[0..255] of TColor;
  TRGBMap = packed record
    case boolean of
      True: (RGBVal: DWord);
      False: (R, G, B, D: Byte);
  end;
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..PixelCountMax-1] of TRGBTriple;
const
  DitherDepth = 16;
var
  iLoop, xLoop, yLoop, XX, YY: Integer;
  iBndS, iBndE: Integer;
  GBand: TGradientBand;
  Row:  pRGBTripleArray;
  procedure CalculateGradientBand;
  var
    rR, rG, rB: Real;
    lCol, hCol: TRGBMap;
    iStp: Integer;
  begin
    if Style in [gsLeft, gsTop] then
    begin
      lCol.RGBVal := ColorToRGB(StartColor);
      hCol.RGBVal := ColorToRGB(EndColor);
    end
    else
    begin
      lCol.RGBVal := ColorToRGB(EndColor);
      hCol.RGBVal := ColorToRGB(StartColor);
    end;
    rR := (hCol.R - lCol.R) / (Colors - 1);
    rG := (hCol.G - lCol.G) / (Colors - 1);
    rB := (hCol.B - lCol.B) / (Colors - 1);
    for iStp := 0 to (Colors - 1) do
      GBand[iStp] := RGB(
        lCol.R + Round(rR * iStp),
        lCol.G + Round(rG * iStp),
        lCol.B + Round(rB * iStp)
        );
  end;
begin
  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;

  if Bitmap.PixelFormat <> pf24bit then
    Bitmap.PixelFormat := pf24bit;

  CalculateGradientBand;

  with Bitmap.Canvas do
  begin
    Brush.Color := StartColor;
    FillRect(Bounds(0, 0, AWidth, AHeight));
    if Style in [gsLeft, gsRight] then
    begin
      for iLoop := 0 to Colors - 1 do
      begin
        iBndS := MulDiv(iLoop, AWidth, Colors);
        iBndE := MulDiv(iLoop + 1, AWidth, Colors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, iBndS, 0, iBndE, AHeight, PATCOPY);
        if (iLoop > 0) and (Dithered) then
          for yLoop := 0 to DitherDepth - 1 do if (yLoop < AHeight)  then
            begin
            Row := Bitmap.Scanline[yLoop];
            for xLoop := 0 to AWidth div (Colors - 1) do
              begin
              XX:= iBndS + Random(xLoop);
              if (XX < AWidth) and (XX > -1) then
               with Row[XX] do
                begin
                rgbtRed := GetRValue(GBand[iLoop - 1]);
                rgbtGreen := GetGValue(GBand[iLoop - 1]);
                rgbtBlue := GetBValue(GBand[iLoop - 1]);
                end;
              end;
            end;
      end;
      for yLoop := 1 to AHeight div DitherDepth do
        CopyRect(Bounds(0, yLoop * DitherDepth, AWidth, DitherDepth),
          Bitmap.Canvas, Bounds(0, 0, AWidth, DitherDepth));
    end
    else
    begin
      for iLoop := 0 to Colors - 1 do
      begin
        iBndS := MulDiv(iLoop, AHeight, Colors);
        iBndE := MulDiv(iLoop + 1, AHeight, Colors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, 0, iBndS, AWidth, iBndE, PATCOPY);
        if (iLoop > 0) and (Dithered) then
          for yLoop := 0 to AHeight div (Colors - 1) do
            begin
            YY:=iBndS + Random(yLoop);
            if (YY < AHeight) and (YY > -1) then
             begin
             Row := Bitmap.Scanline[YY];
             for xLoop := 0 to DitherDepth - 1 do if (xLoop < AWidth)  then with Row[xLoop] do
               begin
               rgbtRed := GetRValue(GBand[iLoop - 1]);
               rgbtGreen := GetGValue(GBand[iLoop - 1]);
               rgbtBlue := GetBValue(GBand[iLoop - 1]);
               end;
             end;
            end;
      end;
      for xLoop := 0 to AWidth div DitherDepth do
        CopyRect(Bounds(xLoop * DitherDepth, 0, DitherDepth, AHeight),
          Bitmap.Canvas, Bounds(0, 0, DitherDepth, AHeight));
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: dxDrawLine
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const ACanvas: TCanvas; const x1, y1, x2, y2: Integer; const AutoCorrect: Boolean = False
  Result:    None
-----------------------------------------------------------------------------}

procedure dxDrawLine(const ACanvas: TCanvas; const x1, y1, x2, y2: Integer);
begin
  with ACanvas do
  begin
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: dxAdjustBoundRect
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: const BorderWidth: Byte; const ShowBoundLines: Boolean; const BoundLines: TdxBoundLines; var Rect: TRect
  Result:    None
-----------------------------------------------------------------------------}

procedure dxAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TdxBoundLines;
  var Rect: TRect);
begin
  InflateRect(Rect, -BorderWidth, -BorderWidth);
  if not ShowBoundLines then
    Exit;
  if blLeft in BoundLines then
    Inc(Rect.Left);
  if blRight in BoundLines then
    Dec(Rect.Right);
  if blTop in BoundLines then
    Inc(Rect.Top);
  if blBottom in BoundLines then
    Dec(Rect.Bottom);
end;

{-----------------------------------------------------------------------------
  Procedure: dxDrawBoundLines
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: const ACanvas: TCanvas; const BoundLines: TdxBoundLines;
    const AColor: TColor; const Rect: TRect
  Result:    None
-----------------------------------------------------------------------------}

procedure dxDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TdxBoundLines;
  const AColor: TColor; const Rect: TRect);
begin
  with ACanvas do
  begin
    Pen.Color := AColor;
    Pen.Style := psSolid;
    if blLeft in BoundLines then
      dxDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom - 1);
    if blTop in BoundLines then
      dxDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Right, Rect.Top);
    if blRight in BoundLines then
      dxDrawLine(ACanvas, Rect.Right - 1, Rect.Top, Rect.Right - 1, Rect.Bottom - 1);
    if blBottom in BoundLines then
      dxDrawLine(ACanvas, Rect.Top, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
  end;
end;

//
// attic
//

procedure dxConvertToGray2(Bitmap: TBitmap);
var
  x, y, c: Integer;
  PxlColor: TColor;
begin
  for x := 0 to Bitmap.Width - 1 do
    for y := 0 to Bitmap.Height - 1 do
    begin
      PxlColor := ColorToRGB(Bitmap.Canvas.Pixels[x, y]);
      c := (PxlColor shr 16 + ((PxlColor shr 8) and $00FF) + PxlColor and $0000FF) div 3 + 100;
      if c > 255 then c := 255;
      Bitmap.Canvas.Pixels[x, y] := RGB(c, c, c);
    end;
end;

procedure dxRenderText(const AParent: TControl; const ACanvas: TCanvas;
  AText: string; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var Rect: TRect; Flags: Integer); overload;
  procedure DoDrawText;
  begin
    DrawText(ACanvas.Handle, PChar(AText), -1, Rect, Flags);
  end;
begin
  if (Flags and DT_CALCRECT <> 0) and ((AText = '') or AShowAccelChar
    and (AText[1] = '&') and (AText[2] = #0)) then
    AText := AText + ' ';
  if not AShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Flags := AParent.DrawTextBiDiModeFlags(Flags);
  with ACanvas do
  begin
    Font.Assign(AFont);
    if not AEnabled then
      Font.Color := dxColor_Msc_Dis_Caption_WXP;
    if not AEnabled then
    begin
      OffsetRect(Rect, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText;
      OffsetRect(Rect, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText;
    end
    else
      DoDrawText;
  end;
end;

procedure dxFrame3d(const ACanvas: TCanvas; const Rect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);
var
  ATopColor, ABottomColor: TColor;
begin
  ATopColor := TopColor;
  ABottomColor := BottomColor;
  if Swapped then
  begin
    ATopColor := BottomColor;
    ABottomColor := TopColor;
  end;
  with ACanvas do
  begin
    Pen.Color := ATopColor;
    Polyline([
      Point(Rect.Left, Rect.Bottom - 1),
      Point(Rect.Left, Rect.Top),
      Point(Rect.Right - 1, Rect.Top)]);
    Pen.Color := ABottomColor;
    Polyline([
      Point(Rect.Right - 1, Rect.Top + 1),
      Point(Rect.Right - 1 , Rect.Bottom - 1),
      Point(Rect.Left, Rect.Bottom - 1)]);
  end;
end;

procedure dxColorizeBitmap(Bitmap: TBitmap; const AColor: TColor);
var
  ColorMap: TBitmap;
  Rect: TRect;
begin
  Rect := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
  ColorMap := TBitmap.Create;
  try
    ColorMap.Assign(Bitmap);
    Bitmap.Dormant;
    Bitmap.FreeImage;
    with ColorMap.Canvas do
    begin
      Brush.Color := AColor;
      BrushCopy(Rect, Bitmap, Rect, clBlack);
    end;
    Bitmap.Assign(ColorMap);
    ColorMap.ReleaseHandle;
  finally
    ColorMap.Free;
  end;
end;

procedure dxSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
begin
  Flags := DT_END_ELLIPSIS;
  case AAlignment of
    taLeftJustify:
      Flags := Flags or DT_LEFT;
    taCenter:
      Flags := Flags or DT_CENTER;
    taRightJustify:
      Flags := Flags or DT_RIGHT;
  end;
  if not AWordWrap then
    Flags := Flags or DT_SINGLELINE
  else
    Flags := Flags or DT_WORDBREAK;
end;

procedure dxPlaceText(const AParent: TControl; const ACanvas: TCanvas; const AText: string;
  const AFont: TFont; const AEnabled, AShowAccelChar: Boolean; const AAlignment: TAlignment;
  const AWordWrap: Boolean; var Rect: TRect);
var
  Flags, dx, OH, OW: Integer;
begin
  OH := Rect.Bottom - Rect.Top;
  OW := Rect.Right - Rect.Left;
  dxSetDrawFlags(AAlignment, AWordWrap, Flags);
  dxRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect,
    Flags or DT_CALCRECT);
  if AAlignment = taRightJustify then
    dx := OW - (Rect.Right + Rect.Left)
  else if AAlignment = taCenter then
    dx := (OW - Rect.Right) div 2
  else
    dx := 0;
  OffsetRect(Rect, dx, (OH - Rect.Bottom) div 2);
  dxRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect, Flags);
end;

end.

