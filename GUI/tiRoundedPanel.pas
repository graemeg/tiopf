unit tiRoundedPanel;

interface

{$I tiDefines.inc}

uses
  ExtCtrls
  ,Classes
  ,Graphics
  ;


type

  TtiRoundedPanel = class(TCustomPanel)
  private
    FCornerRadius: Byte;
    FBorderColor: TColor;
    FBorderThickness: Byte;
    procedure SetCornerRadius(const Value: Byte);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Byte);
    procedure CalcBorderWidth;
  public
    constructor Create(Owner : TComponent); override ;
    procedure Paint; override;
    property  DockManager;
  published
    property CornerRadius : Byte read FCornerRadius write SetCornerRadius ;
    property BorderColor : TColor read FBorderColor write SetBorderColor ;
    property BorderThickness : Byte read FBorderThickness write SetBorderThickness;
    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property Caption;
    property Constraints;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    {$IFDEF DELPHI7ORABOVE}
    property ParentBackground;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation
uses
  Windows
  ,Controls
  ;

{ TtiRoundedPanel }

constructor TtiRoundedPanel.Create(Owner: TComponent);
begin
  inherited;
  FCornerRadius := 5;
  FBorderColor := clActiveBorder;
  FBorderThickness := 1;
  CalcBorderWidth;
end;

procedure TtiRoundedPanel.Paint;
  procedure _DrawBorder(const pRect : TRect ; const pColor : TColor ;pPenFactor : integer);
  begin
    Canvas.Pen.Color := pColor ;
    Canvas.Pen.Width := FBorderThickness * pPenFactor;
    Canvas.RoundRect(pRect.Left, pRect.Top, pRect.Right, pRect.Bottom,
                     FCornerRadius*2, FCornerRadius*2);
    Canvas.Font.Color := clActiveCaption;
    if Caption <> '' then
      Canvas.TextOut(FCornerRadius*2, -2, ' ' + Caption + ' ');
  end;
var
  lRect: TRect;
begin
  lRect := Rect(0,0,Width,Height);
  if ( Parent <> nil ) then
  begin
    Canvas.Brush.Color := Parent.Brush.Color;
    Canvas.FillRect(lRect);
  end;
  Canvas.Brush.Color := Color;
  InflateRect(lRect,-FBorderThickness,-FBorderThickness);
  _DrawBorder(lRect, Color, 2);
  InflateRect(lRect,-FBorderThickness,-FBorderThickness);
  _DrawBorder(lRect, BorderColor, 1);
end;

procedure TtiRoundedPanel.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TtiRoundedPanel.SetBorderThickness(const Value: Byte);
begin
  FBorderThickness := Value;
  CalcBorderWidth;
  Invalidate;
end;

procedure TtiRoundedPanel.SetCornerRadius(const Value: Byte);
begin
  FCornerRadius := Value;
  CalcBorderWidth;
  Invalidate;
end;


procedure TtiRoundedPanel.CalcBorderWidth;
begin
  BorderWidth := (FCornerRadius div 2 + BorderThickness * 2) + 1 ;
end;

end.
