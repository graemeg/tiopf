unit tiHyperlink;

{$I tiDefines.inc}

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

  TtiHyperLink = class(TCustomLabel)
  private
    FColorAvailable: TColor;
    FColorHilight: TColor;
    FOnMouseLeave1: TNotifyEvent;
    FOnMouseEnter1: TNotifyEvent;
    FMouseOver: boolean;
    procedure SetColorAvailable(const AValue: TColor);
  protected
{$IFDEF VER130}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
    procedure DoMouseEnter(Sender : TObject); virtual;
    procedure DoMouseLeave(Sender : TObject); virtual;
  public
    constructor Create(Owner : TComponent); override;
    procedure   SetMouseOver(AValue: boolean);
  published
    property Action;
    property Anchors;
    property OnClick;
    property Caption;
    property Hint;
    property ShowHint;
    property Enabled;
    property Font;
    property ParentFont;
    property ColorAvailable : TColor read FColorAvailable write SetColorAvailable default clNavy;
    property ColorHilight  : TColor read FColorHilight   write FColorHilight default clRed;
    property OnMouseEnter1 : TNotifyEvent read FOnMouseEnter1 write FOnMouseEnter1;
    property OnMouseLeave1 : TNotifyEvent read FOnMouseLeave1 write FOnMouseLeave1;
  end;

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

constructor TtiHyperLink.Create(Owner: TComponent);
begin
  inherited;
  FColorAvailable := clNavy;
  FColorHilight := clRed;
  Cursor      := crHandPoint;
  Font.Color  := FColorAvailable;
  Font.Style  := [fsUnderline];
{$IFNDEF VER130}
  if not (csDesigning in ComponentState) then
  begin
    OnMouseEnter := DoMouseEnter;
    OnMouseLeave := DoMouseLeave;
  end;
{$ENDIF}
end;

//{$IFNDEF VER130}
procedure TtiHyperLink.DoMouseEnter(Sender: TObject);
begin
  SetMouseOver(True);
end;

procedure TtiHyperLink.DoMouseLeave(Sender: TObject);
begin
  SetMouseOver(False);
end;
//{$ENDIF}

procedure TtiHyperLink.SetColorAvailable(const AValue: TColor);
begin
  FColorAvailable := AValue;
  Font.Color := FColorAvailable;
end;

procedure TtiHyperLink.SetMouseOver(AValue: boolean);
begin
  FMouseOver:= AValue;
  if FMouseOver then
  begin
    Font.Color := FColorHilight;
    if Assigned(FOnMouseEnter1) then
      FOnMouseEnter1(Self);
  end else
  begin
    Font.Color := FColorAvailable;
    if Assigned(FOnMouseLeave1) then
      FOnMouseLeave1(Self);
  end;
end;

end.

