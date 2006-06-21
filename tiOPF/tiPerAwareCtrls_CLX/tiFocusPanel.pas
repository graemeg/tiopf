{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    Mar 2004, Graeme Geldenhuys, Ported to CLX for Linux

  Purpose:

  Classes:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiFocusPanel;

interface

uses
  Classes
  ,QGraphics
  ,QControls
  ;

type

  { Adds support for the focus rectangle around our controls. We inherit
    from TCustomControl rather than TCustomPanel, as the later causes
    errors when we override the BoundsChanged() method. }
  TtiFocusPanel = class( TCustomControl )
  private
    FShowFocusRect : boolean;
    FFocusRectColor: TColor;
    function IsFocused: boolean;
  protected
    procedure DoDrawFocusRect( pDraw : boolean ); virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShowFocusRect : boolean read FShowFocusRect write FShowFocusRect;
    property FocusRectColor: TColor read FFocusRectColor write FFocusRectColor default clBlack;
  end;

implementation

uses
  QForms
  ,Types
  ,Qt
  ;

{ TtiFocusPanel }

constructor TtiFocusPanel.Create(AOwner: TComponent);
begin
  inherited;
  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable, csNoFocus];

  if (csDesigning in ComponentState) then
    ControlStyle := ControlStyle - [csAcceptsControls];

  FShowFocusRect := true ;
  Color := clBackground;
  // Don't use Width, because it causes errors when dropping the component
  // on a form. Don't understand why. Something todo with BoundsChanged().
  FWidth := 185;
end;

//------------------------------------------------------------------------------
procedure TtiFocusPanel.DoDrawFocusRect(pDraw: boolean);
var
  lRect : TRect ;
  lPenColor : TColor ;
  lPenStyle : TPenStyle ;
begin
  if not FShowFocusRect then
    Exit ; //==>
  lRect := GetClientRect ;
  lPenColor := Canvas.Pen.Color ;
  try
    lPenStyle := Canvas.Pen.Style ;
    try
      Canvas.Pen.Style := psDot ;
      if pDraw then
        Canvas.Pen.Color := FFocusRectColor
      else
        Canvas.Pen.Color := Color ;
      Canvas.Rectangle(lRect);
    finally
      Canvas.Pen.Style := lPenStyle ;
    end ;
  finally
    Canvas.Pen.Color := lPenColor ;
  end;
end;

//------------------------------------------------------------------------------
// Check to see if any owned controls have focus.
// Not sure if this will be required.
procedure TtiFocusPanel.DoEnter;
begin
  DoDrawFocusRect( true );
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiFocusPanel.DoExit;
begin
  DoDrawFocusRect( false );
  inherited;
end;

//------------------------------------------------------------------------------
function TtiFocusPanel.IsFocused: boolean;
var
  i : integer ;
  lActiveControl : TWidgetControl ;
begin
  result := Focused ;
  if Focused then
    Exit ; //==>

  lActiveControl := Screen.ActiveControl ;
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] = lActiveControl then
    begin
      result := true ;
      Exit ; //==>
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TtiFocusPanel.Paint;
var
  Rect: TRect;
begin
  Rect := GetClientRect;
  with Canvas do
  begin
    if (Bitmap = nil) or Bitmap.Empty then
    begin
      Brush.Color := Color;
      if Color <> clBackground then
        FillRect(Rect)
      else
        QWidget_erase(Self.Handle, @Rect);
    end;
  end;
  DoDrawFocusRect(IsFocused);
  inherited;
end;

end.


