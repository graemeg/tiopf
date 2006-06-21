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

  Purpose:

  Classes:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiFocusPanel;

interface

uses
  Windows
  ,Classes
  ,Graphics
  ,Controls
  ,extCtrls
  ,Messages
  ;

type

  TtiFocusPanel = class( TCustomPanel )
  private
    FShowFocusRect : boolean ;
    function IsFocused: boolean;
  protected
    procedure DoEnter ; override ;
    procedure DoExit  ; override ;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Paint; override;
    procedure   DoDrawFocusRect( pDraw : boolean )  ; virtual ;
  published
    property    ShowFocusRect : boolean read FShowFocusRect write FShowFocusRect ;
  end ;

implementation
uses
  Forms
  ;

{ TtiFocusPanel }

constructor TtiFocusPanel.Create(AOwner: TComponent);
begin
  inherited;

  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
  if ( csDesigning in ComponentState ) then
    ControlStyle   := ControlStyle - [csAcceptsControls] ;

  ControlStyle   := ControlStyle - [csSetCaption] ;
  FShowFocusRect := true ;
end;

procedure TtiFocusPanel.DoDrawFocusRect( pDraw : boolean ) ;
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
        Canvas.Pen.Color := clBlack
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

procedure TtiFocusPanel.DoEnter;
begin
  DoDrawFocusRect(true) ;
  inherited;
end;

procedure TtiFocusPanel.DoExit;
begin
  DoDrawFocusRect(false) ;
  inherited;
end;

// Check to see if any owned controls have focus.
// Not sure if this will be required.
function TtiFocusPanel.IsFocused: boolean;
var
  i : integer ;
  lActiveControl : TWinControl ;
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

procedure TtiFocusPanel.Paint;
begin
  inherited;
  DoDrawFocusRect(IsFocused);
end;

end.
