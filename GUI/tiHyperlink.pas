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

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiHyperlink;

interface
uses
   Classes
  ,StdCtrls
  ,Graphics
{$IFNDEF FPC}
  ,Messages
{$ENDIF}
  ,Controls
  ;

type

  TtiHyperLink = class( TCustomLabel )
  private
    FColorAvailable: TColor;
    FColorHilight: TColor;
    FOnMouseLeave1: TNotifyEvent;
    FOnMouseEnter1: TNotifyEvent;
    procedure SetColorAvailable(const Value: TColor);
  protected
{$IFDEF VER130}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
    procedure DoMouseEnter( Sender : TObject ) ; virtual ;
    procedure DoMouseLeave( Sender : TObject ) ; virtual ;
  public
    constructor Create(AOwner : TComponent ) ; override ;
  published
    property Action ;
    property Anchors ;
    property OnClick ;
    property Caption ;
    property Hint ;
    property ShowHint ;
    property Enabled ;
    property ParentFont;
    property Font ;
    property ColorAvailable : TColor read FColorAvailable write SetColorAvailable default clNavy ;
    property ColorHilight   : TColor read FColorHilight   write FColorHilight default clRed ;
    property OnMouseEnter1  : TNotifyEvent read FOnMouseEnter1 write FOnMouseEnter1 ;
    property OnMouseLeave1  : TNotifyEvent read FOnMouseLeave1 write FOnMouseLeave1 ;
  end ;

implementation

{ TtiHyperLink }

{$IFDEF VER130}
procedure TtiHyperLink.CMMouseEnter(var Message: TMessage);
begin
  DoMouseEnter(Self);
end;

procedure TtiHyperLink.CMMouseLeave(var Message: TMessage);
begin
  DoMouseLeave(Self);
end;
{$ENDIF}

constructor TtiHyperLink.Create(AOwner: TComponent);
begin
  inherited;
  FColorAvailable := clNavy ;
  FColorHilight := clRed ;
  Cursor       := crHandPoint ;
  Font.Color   := FColorAvailable ;
  Font.Style   := [fsUnderline] ;
{$IFNDEF VER130}
  if not ( csDesigning in ComponentState ) then
  begin
    OnMouseEnter := DoMouseEnter ;
    OnMouseLeave := DoMouseLeave ;
  end ;
{$ENDIF}
end;

//{$IFNDEF VER130}
procedure TtiHyperLink.DoMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Color := FColorHilight ;
  if Assigned(FOnMouseEnter1) then
    FOnMouseEnter1(Self);
end;

procedure TtiHyperLink.DoMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Color := FColorAvailable ;
  if Assigned(FOnMouseLeave1) then
    FOnMouseLeave1(Self);
end;
//{$ENDIF}

procedure TtiHyperLink.SetColorAvailable(const Value: TColor);
begin
  FColorAvailable := Value;
  Font.Color := FColorAvailable ;
end;

end.

