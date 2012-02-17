unit tiSplitter;

{$I tiDefines.inc}

interface
uses
{$IFNDEF FPC}
  Windows
  ,Messages
{$ELSE}
   LMessages
  ,LCLIntf
  ,LCLProc
{$ENDIF}
  ,Classes
  ,Graphics
  ,Controls
  ,ExtCtrls
  ,Forms
 ;

const
  cbKeepSplitterPosPercent = false;
  cuColorGrabBar = $00FE9E83; // Pale navy blue

type
 {$IFDEF FPC}
  TMessage = TLMessage;
 {$ENDIF}



  TtiSplitter = class(TSplitter)
  private
    FbMouseOver : boolean;
    FColorGrabBar: TColor;
    procedure SetColorGrabBar(const AValue: TColor);
  protected
    procedure Paint; override;
    Procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    {$IFNDEF FPC}
    Procedure CMDesignHitTest(Var Message : TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF}
    Procedure DrawGrabBar(pRect : TRect);
  published
    Property  ColorGrabBar : TColor read FColorGrabBar write SetColorGrabBar default cuColorGrabBar;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TtiSplitterOrientation = (spoVertical, spoHorizontal);

  TtiSplitterPanelStyle = (spsNone, spsUser, spsLowered, spsRaised,
                            spsFramed, spsShadow, spsBump);

  TtiSplitterPane = class(TPanel)
  protected
    procedure Paint; override;
  end;

  TtiSplitterPanel = class;
  TMoveSplitterEvent = procedure(pTISplitterPanel : TtiSplitterPanel) of object;

  TtiSplitterPanel = class(TCustomPanel)
  private
    FPanel1  : TtiSplitterPane;
    FSplitter : TtiSplitter;
    FPanel2  : TtiSplitterPane;
    FSplitterOrientation : TtiSplitterOrientation;
    FslOwnedControls1 : TStringList;
    FslOwnedControls2 : TStringList;
    FbKeepSplitterPosPercent: boolean;
    FiSaveSplitterPosPercent : integer;
    FPanelBorderStyle : TtiSplitterPanelStyle;
    FSaveAlign : TAlign;
    FOnMoveSplitter: TMoveSplitterEvent;
    procedure SetSplitterOrientation(const AValue : TtiSplitterOrientation);
    function  GetBevelInnerSubPanels: TPanelBevel;
    function  GetBevelOuterSubPanels: TPanelBevel;
    function  GetBorderStyleSubPanels: TBorderStyle;
    procedure SetBevelInnerSubPanels(const AValue: TPanelBevel);
    procedure SetBevelOuterSubPanels(const AValue: TPanelBevel);
    procedure SetBorderStyleSubPanels(const AValue: TBorderStyle);
    function  GetPosition: integer;
    procedure SetPosition(const AValue: integer);
    function  GetPositionPercent: integer;
    procedure SetPositionPercent(const AValue: integer);

    // Custom streaming methods to managed the controls owned by the two panels.
    // See paper by Ray Konopka titled 'Advanced component development' on the
    // 1998 BorCon CD for a description on how the custom streaming works.
    procedure DoReadOwnedControls(lsl: TStringList; Reader: TReader);
    procedure DoWriteOwnedControls(lPanel: TPanel; Writer: TWriter);
    procedure ReadOwnedControls1(Reader: TReader);
    procedure ReadOwnedControls2(Reader: TReader);
    procedure WriteOwnedControls1(Writer: TWriter);
    procedure WriteOwnedControls2(Writer: TWriter);

    function  GetColorGrabBar: TColor;
    procedure SetColorGrabBar(const AValue: TColor);
    procedure SetPanelStyle(const AValue: TtiSplitterPanelStyle);
    procedure DoSplitterOnMoved(Sender : TObject);
    procedure SetKeepSplitterPosPercent(const AValue: boolean);
    function  GetColorPanel: TColor;
    procedure SetColorPanel(const AValue: TColor);
    function  GetAligned: TAlign;
    procedure SetAligned(const AValue: TAlign);
    procedure ExecuteOnMoveSplitter;
  protected

    // Custom streaming methods to managed the controls owned by the two panels.
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    {$IFNDEF FPC}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ELSE}
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    {$ENDIF}
//    procedure DoResize(Sender: TObject);
//    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    Property    Panel1 : TtiSplitterPane read FPanel1;
    Property    Panel2 : TtiSplitterPane read FPanel2;

  published
//    property Align;
    property Aligned               : TAlign       read GetAligned                write SetAligned;
    property Anchors;
    property ColorGrabBar          : TColor       read GetColorGrabBar           write SetColorGrabBar;
    property ColorPanel            : TColor       read GetColorPanel             write SetColorPanel;
    property BevelInnerSubPanels   : TPanelBevel  read GetBevelInnerSubPanels    write SetBevelInnerSubPanels default bvRaised;
    property BevelOuterSubPanels   : TPanelBevel  read GetBevelOuterSubPanels    write SetBevelOuterSubPanels default bvLowered;
    property BorderStyleSubPanels  : TBorderStyle read GetBorderStyleSubPanels   write SetBorderStyleSubPanels default bsNone;
    property PanelStyle      : TtiSplitterPanelStyle
               read  FPanelBorderStyle
               write SetPanelStyle;

    // The order of these (SplitterOrientation, KeepSplitterPosPercent,
    // SplitterPos and SplitterPosPercent are important as they control the
    // streaming process
    property SplitterOrientation   : TtiSplitterOrientation read FSplitterOrientation write SetSplitterOrientation;
    property KeepSplitterPosPercent : boolean      read FbKeepSplitterPosPercent  write SetKeepSplitterPosPercent default cbKeepSplitterPosPercent;
    property SplitterPos           : integer      read GetPosition               write SetPosition;
    property SplitterPosPercent    : integer      read GetPositionPercent        write SetPositionPercent;
    property Visible;
    property OnMoveSplitter : TMoveSplitterEvent read FOnMoveSplitter write FOnMoveSplitter;
  end;

const
  ctiSplitterWidth = 8;

implementation
uses
  SysUtils
  ,Math
 ;

const
  cuiMinSize = 3;
  cuiDefaultHeight = 250;
  cuiDefaultWidth  = 250;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitter
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Width := ctiSplitterWidth;
  FbMouseOver := false;
  ResizeStyle := rsUpdate;
  AutoSnap := false;
  FColorGrabBar := cuColorGrabBar;
end;


procedure TtiSplitter.CMMouseEnter(var Message: TMessage);
begin
  FbMouseOver := true;
  Paint;
end;


procedure TtiSplitter.CMMouseLeave(var Message: TMessage);
begin
  FbMouseOver := false;
  Paint;
end;

{$IFNDEF FPC}
procedure TtiSplitter.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;
{$ENDIF}

procedure TtiSplitter.Paint;
var
  lRect : TRect;
begin
  inherited;
  case Align of
  alRight,
  alLeft : begin
               lRect.Top   := Height div 4;
               lRect.Bottom := Height div 4 * 3;
               lRect.Left  := 1;
               lRect.Right := 7;
             end;
  alTop,
  alBottom : begin
               lRect.Left  := Width div 4;
               lRect.Right := Width div 4 * 3;
               lRect.Top   := 1;
               lRect.Bottom := 7;
             end;
  end;
  DrawGrabBar(lRect);
end;

procedure TtiSplitter.DrawGrabBar(pRect : TRect);
var
  lFillRect : TRect;
  lSaveColor : TColor;
begin
  lSaveColor := Canvas.Brush.Color;

  // Draw the outline of the rectangle
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(pRect);

  // If the mouse is over the splitter bar, then fill the grab bar part
  // with colour.
  if FbMouseOver then
  begin
    lFillRect.Top     := pRect.Top    + 1;
    lFillRect.Left    := pRect.Left   + 1;
    lFillRect.Bottom  := pRect.Bottom - 2;
    lFillRect.Right   := pRect.Right  - 2;
    Canvas.Brush.Color := FColorGrabBar;
    Canvas.FillRect(lFillRect);
  end;

  // Draw a shadow around the inside of the grab bar
  Canvas.Pen.Color := clWhite;
  Canvas.MoveTo(pRect.Left  + 1,  pRect.Top    + 1);
  Canvas.LineTo(pRect.Right - 1,  pRect.Top    + 1);
  Canvas.MoveTo(pRect.Left  + 1,  pRect.Top    + 1);
  Canvas.LineTo(pRect.Left  + 1,  pRect.Bottom - 1);

  // Draw some texture inside the grab bar
  Canvas.Pen.Style := psDot;
  if Align in [alLeft, alRight] then
  begin
    Canvas.MoveTo(pRect.Left + 3,  pRect.Top + 15   );
    Canvas.LineTo(pRect.Left + 3,  pRect.Bottom - 15);
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(pRect.Left + 4,  pRect.Top + 16   );
    Canvas.LineTo(pRect.Left + 4,  pRect.Bottom - 16);
  end
  else
  begin
    Canvas.MoveTo(pRect.Left  + 15, pRect.Top + 3);
    Canvas.LineTo(pRect.Right - 15, pRect.Top + 3);
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(pRect.Left + 16,  pRect.Top + 4);
    Canvas.LineTo(pRect.Right - 16, pRect.Top + 4);
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clBlack;

  case Align of
  alRight   : begin
                 // Draw the top triangle
                 Canvas.Polygon([ Point(pRect.Left + 2,  pRect.Top + 5),
                                  Point(pRect.Left + 2,  pRect.Top + 10),
                                  Point(pRect.Left + 4,  pRect.Top + 7 )]);

                 // Draw the bottom triangle
                 Canvas.Polygon([ Point(pRect.Left + 2,  pRect.Bottom - 5),
                                  Point(pRect.Left + 2,  pRect.Bottom - 10),
                                  Point(pRect.Left + 4,  pRect.Bottom - 7 )]);
               end;
  alLeft    : begin
                 // Draw the top triangle
                 Canvas.Polygon([ Point(pRect.Right - 2,  pRect.Top + 5),
                                  Point(pRect.Right - 2,  pRect.Top + 10),
                                  Point(pRect.Right - 4,  pRect.Top + 7 )]);

                 // Draw the bottom triangle
                 Canvas.Polygon([ Point(pRect.Right - 2,  pRect.Bottom - 5),
                                  Point(pRect.Right - 2,  pRect.Bottom - 10),
                                  Point(pRect.Right - 4,  pRect.Bottom - 7 )]);
               end;
  alBottom  : begin
                 // Draw the top triangle
                 Canvas.Polygon([ Point(pRect.Left + 5,  pRect.Top + 2),
                                  Point(pRect.Left + 10, pRect.Top + 2),
                                  Point(pRect.Left + 7,  pRect.Top + 4)]);

                 // Draw the bottom triangle
                 Canvas.Polygon([ Point(pRect.Right - 5,  pRect.Top + 2),
                                  Point(pRect.Right - 10, pRect.Top + 2),
                                  Point(pRect.Right - 7,  pRect.Top + 4)]);
               end;
  alTop     : begin
                 // Draw the top triangle
                 Canvas.Polygon([ Point(pRect.Left + 5,  pRect.Bottom - 2),
                                  Point(pRect.Left + 10, pRect.Bottom - 2),
                                  Point(pRect.Left + 7,  pRect.Bottom - 4)]);

                 // Draw the bottom triangle
                 Canvas.Polygon([ Point(pRect.Right - 5,  pRect.Bottom - 2),
                                  Point(pRect.Right - 10, pRect.Bottom - 2),
                                  Point(pRect.Right - 7,  pRect.Bottom - 4)]);
               end;
  end;
  Canvas.Brush.Color := lSaveColor;

end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitterPanel
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiSplitterPanel.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;

  Height := cuiDefaultHeight;
  Width := cuiDefaultWidth ;

  FSplitterOrientation := spoVertical;

  FPanel1 := TtiSplitterPane.Create(Self);
  FPanel1.Parent := Self;
  FPanel1.ControlStyle := ControlStyle - [csSetCaption];
  FPanel1.BevelInner := bvRaised;
  FPanel1.BevelOuter := bvLowered;
  FPanel1.BorderStyle := bsNone;
  FPanel1.Align := alLeft;
  FPanel1.Width := cuiDefaultWidth div 2 - 4;

  FSplitter := TtiSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Left := cuiDefaultWidth div 2 - 4;
  FSplitter.OnMoved := DoSplitterOnMoved;

  FPanel2 := TtiSplitterPane.Create(Self);
  FPanel2.Parent := Self;
  FPanel2.ControlStyle := ControlStyle - [csSetCaption];
  FPanel2.BevelInner := bvRaised;
  FPanel2.BevelOuter := bvLowered;
  FPanel2.BorderStyle := bsNone;
  FPanel2.Align := alClient;

  FslOwnedControls1 := TStringList.Create;
  FslOwnedControls2 := TStringList.Create;

  FbKeepSplitterPosPercent := cbKeepSplitterPosPercent;
  FiSaveSplitterPosPercent := 50;
  FPanelBorderStyle := spsFramed;

//  OnResize := DoResize;
end;

destructor TtiSplitterPanel.Destroy;
begin
  FslOwnedControls1.Free;
  FslOwnedControls2.Free;
  inherited;
end;

function TtiSplitterPanel.GetBevelInnerSubPanels: TPanelBevel;
begin
  result := FPanel1.BevelInner;
end;

function TtiSplitterPanel.GetBevelOuterSubPanels: TPanelBevel;
begin
  result := FPanel1.BevelOuter;
end;

function TtiSplitterPanel.GetBorderStyleSubPanels: TBorderStyle;
begin
  result := FPanel1.BorderStyle;
end;

function TtiSplitterPanel.GetPosition: integer;
begin
  case FSplitterOrientation of
  spoVertical  : result := FPanel1.Width;
  spoHorizontal : result := FPanel1.Height;
  else
    raise exception.CreateFmt('Invalid SplitterOrientation in %s.GetPosition',
                            [ClassName]);
  end;
end;

function TtiSplitterPanel.GetPositionPercent: integer;
begin
  case FSplitterOrientation of
  spoVertical  : result := FPanel1.Width * 100 div (Self.ClientWidth - FSplitter.Width);
  spoHorizontal : result := FPanel1.Height * 100 div (Self.ClientHeight - FSplitter.Height);
  else
    raise exception.CreateFmt('Invalid SplitterOrientation in %s.GetPosition',
                            [ClassName]);
  end;
end;

procedure TtiSplitterPanel.Loaded;
  procedure _DoLoaded(lsl : TStringList; lPanel : TPanel);
  var
    i : integer;
    c : TComponent;
  begin
    for i := 0 to lsl.Count - 1 do
    begin
      c := GetParentForm(Self).FindComponent(lsl[ i ]);
      if c <> nil then
          TControl(c).Parent := lPanel;
    end;
  end;
begin
  inherited Loaded;

  _DoLoaded(FslOwnedControls1, FPanel1);
  _DoLoaded(FslOwnedControls2, FPanel2);
  FiSaveSplitterPosPercent := SplitterPosPercent;
  Align := FSaveAlign;

end;

procedure TtiSplitterPanel.SetBevelInnerSubPanels(const AValue: TPanelBevel);
begin
  FPanel1.BevelInner := AValue;
  FPanel2.BevelInner := AValue;
  FPanelBorderStyle       := spsUser;
end;

procedure TtiSplitterPanel.SetBevelOuterSubPanels(const AValue: TPanelBevel);
begin
  FPanel1.BevelOuter := AValue;
  FPanel2.BevelOuter := AValue;
  FPanelBorderStyle       := spsUser;
end;

procedure TtiSplitterPanel.SetBorderStyleSubPanels(const AValue: TBorderStyle);
begin
  FPanel1.BorderStyle := AValue;
  FPanel2.BorderStyle := AValue;
  FPanelBorderStyle       := spsUser;
end;

procedure TtiSplitterPanel.SetPosition(const AValue: integer);
var
  LValue: integer;
begin
  case FSplitterOrientation of
  spoVertical  :  begin
                   LValue:= Min(AValue, Trunc(ClientWidth * 0.9));
                   FPanel1.Width := LValue;
                  end;
  spoHorizontal : begin
                    LValue:= Min(AValue, Trunc(ClientHeight * 0.9));
                    FPanel1.Height := LValue;
                  end;
  end;
  FiSaveSplitterPosPercent := SplitterPosPercent;
end;

procedure TtiSplitterPanel.SetPositionPercent(const AValue: integer);
var
  LValue: integer;
begin
  LValue:= Min(AValue, 95);
  if SplitterPosPercent = LValue then
    Exit; //==>

  // NOTE: Setting panel width or height messes with the auto alignment
  // resizing so we need to set both and call Realign to put it all together.
  case FSplitterOrientation of
  spoVertical:
    begin
      FPanel1.Width := (Self.ClientWidth  - FSplitter.Width) * LValue div 100;
      FPanel1.Height := Self.ClientHeight;
    end;
  spoHorizontal:
    begin
      FPanel1.Height := (Self.ClientHeight - FSplitter.Height) * LValue div 100;
      FPanel1.Width := Self.ClientWidth;
    end;
  end;
  FPanel1.Realign;

  if FiSaveSplitterPosPercent <> LValue then
    FiSaveSplitterPosPercent := SplitterPosPercent;
end;

procedure TtiSplitterPanel.SetSplitterOrientation(const AValue: TtiSplitterOrientation);
begin
  FSplitterOrientation := AValue;
  FPanel2.Align := alNone;
  FPanel1.Align := alNone;
  case AValue of
  spoVertical : begin
                     FPanel1.Align := alLeft;
                     FPanel1.Width := Self.ClientWidth div 2;
                     FSplitter.Align := alLeft;
                     FSplitter.Left := FPanel1.Width;
                   end;
  spoHorizontal : begin
                     FPanel1.Align := alTop;
                     FPanel1.Height := Self.ClientHeight div 2;
                     FSplitter.Align := alTop;
                     FSplitter.Top := FPanel1.Height;
                   end;
  end;

  FPanel2.Align := alClient;

end;

procedure TtiSplitterPanel.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Panel1Controls', ReadOwnedControls1, WriteOwnedControls1, True);
  Filer.DefineProperty('Panel2Controls', ReadOwnedControls2, WriteOwnedControls2, True);
end;

procedure TtiSplitterPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
  procedure _DoGetChildren(lPanel : TPanel);
  var
    i : integer;
    c : TControl;
    f : TForm;
  begin
    f := TForm(GetParentForm(Self));
    for i := 0 to lPanel.ControlCount - 1 do
    begin
      c := lPanel.Controls[i];
      if c.Owner = f then
        Proc(c);
    end;
  end;
begin
  inherited GetChildren(Proc, Root);
  _DoGetChildren(FPanel1);
  _DoGetChildren(FPanel2);
end;

procedure TtiSplitterPanel.ReadOwnedControls1(Reader: TReader);
begin
  DoReadOwnedControls(FslOwnedControls1, Reader);
end;

procedure TtiSplitterPanel.ReadOwnedControls2(Reader: TReader);
begin
  DoReadOwnedControls(FslOwnedControls2, Reader);
end;

procedure TtiSplitterPanel.DoReadOwnedControls(lsl : TStringList; Reader : TReader);
begin
  lsl.Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    lsl.Add(Reader.ReadIdent);
  Reader.ReadListEnd;
end;

procedure TtiSplitterPanel.WriteOwnedControls1(Writer: TWriter);
begin
  DoWriteOwnedControls(FPanel1, Writer);
end;

procedure TtiSplitterPanel.WriteOwnedControls2(Writer: TWriter);
begin
  DoWriteOwnedControls(FPanel2, Writer);
end;

procedure TtiSplitterPanel.DoWriteOwnedControls(lPanel : TPanel; Writer : TWriter);
var
  i : integer;
begin
  Writer.WriteListBegin;
  for i := 0 to lPanel.ControlCount - 1 do
    Writer.WriteIdent(lPanel.controls[i].Name);
  Writer.WriteListEnd;
end;

{$IFNDEF FPC}
procedure TtiSplitterPanel.WMSize(var Message: TWMSize);
{$ELSE}
procedure TtiSplitterPanel.WMSize(var Message: TLMSize);
{$ENDIF}
begin
  inherited;
  if FbKeepSplitterPosPercent and
     (FiSaveSplitterPosPercent <> -1) then
  begin
    SplitterPosPercent := FiSaveSplitterPosPercent;
    ExecuteOnMoveSplitter;
  end;
end;
//procedure TtiSplitterPanel.AlignControls(AControl: TControl; var Rect: TRect);
//procedure TtiSplitterPanel.DoResize(Sender: TObject);
//begin
//  inherited;
//  if FbKeepSplitterPosPercent and
//     (FiSaveSplitterPosPercent <> -1) then
//  begin
//    SplitterPosPercent := FiSaveSplitterPosPercent;
//    ExecuteOnMoveSplitter;
//  end;
//end;

procedure TtiSplitterPane.Paint;
var
  lPenStyle : TPenStyle;
  lPenColor : TColor;
begin
  inherited;
  if (csDesigning in ComponentState ) and
     (BevelInner  = bvNone) and
     (BevelOuter  = bvNone) and
     (BorderStyle = bsNone) then
  begin
    lPenStyle := Canvas.Pen.Style;
    lPenColor := Canvas.Pen.Color;
    Canvas.Pen.Style := psDashDot;
    Canvas.Pen.Color := clGray;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Pen.Style := lPenStyle;
    Canvas.Pen.Color := lPenColor;
  end;
end;

procedure TtiSplitter.SetColorGrabBar(const AValue: TColor);
begin
  FColorGrabBar := AValue;
  Paint;
end;

function TtiSplitterPanel.GetColorGrabBar: TColor;
begin
  result := FSplitter.ColorGrabBar;
end;

procedure TtiSplitterPanel.SetColorGrabBar(const AValue: TColor);
begin
  FSplitter.ColorGrabBar := AValue;
end;

procedure TtiSplitterPanel.SetPanelStyle(const AValue: TtiSplitterPanelStyle);
begin
  FPanelBorderStyle := AValue;
  case FPanelBorderStyle of
  spsNone   : begin
                 FPanel1.BevelInner := bvNone ;
                 FPanel2.BevelInner := bvNone;
                 FPanel1.BevelOuter := bvNone;
                 FPanel2.BevelOuter := bvNone;
                 FPanel1.BorderStyle := bsNone;
                 FPanel2.BorderStyle := bsNone;
               end;
  spsUser   :;// Do nothing
  spsLowered : begin
                 FPanel1.BevelInner := bvLowered ;
                 FPanel2.BevelInner := bvLowered;
                 FPanel1.BevelOuter := bvNone;
                 FPanel2.BevelOuter := bvNone;
                 FPanel1.BorderStyle := bsNone;
                 FPanel2.BorderStyle := bsNone;
               end;
  spsRaised : begin
                 FPanel1.BevelInner := bvRaised ;
                 FPanel2.BevelInner := bvRaised;
                 FPanel1.BevelOuter := bvNone;
                 FPanel2.BevelOuter := bvNone;
                 FPanel1.BorderStyle := bsNone;
                 FPanel2.BorderStyle := bsNone;
               end;
  spsFramed : begin
                 FPanel1.BevelInner := bvRaised ;
                 FPanel2.BevelInner := bvRaised;
                 FPanel1.BevelOuter := bvLowered;
                 FPanel2.BevelOuter := bvLowered;
                 FPanel1.BorderStyle := bsNone;
                 FPanel2.BorderStyle := bsNone;
               end;
  spsShadow : begin
                 FPanel1.BevelInner := bvLowered ;
                 FPanel2.BevelInner := bvLowered;
                 FPanel1.BevelOuter := bvNone;
                 FPanel2.BevelOuter := bvNone;
                 FPanel1.BorderStyle := bsSingle;
                 FPanel2.BorderStyle := bsSingle;
               end;
  spsBump   : begin
                 FPanel1.BevelInner := bvLowered ;
                 FPanel2.BevelInner := bvLowered;
                 FPanel1.BevelOuter := bvRaised;
                 FPanel2.BevelOuter := bvRaised;
                 FPanel1.BorderStyle := bsNone;
                 FPanel2.BorderStyle := bsNone;
               end;
  end;

end;

procedure TtiSplitterPanel.DoSplitterOnMoved(Sender: TObject);
begin
  FiSaveSplitterPosPercent := SplitterPosPercent;
  ExecuteOnMoveSplitter;
end;

procedure TtiSplitterPanel.ExecuteOnMoveSplitter;
begin
  // Tell the designer that there has been a change so the property editor will
  // be updated, and the control will be marked as dirty so the for will be
  // saved.
  if (csDesigning in ComponentState ) and
     (not (csLoading in ComponentState )) and
     (FindRootDesigner(Self) <> nil) then
    FindRootDesigner(Self).Modified;

  if Assigned(FOnMoveSplitter) then
    FOnMoveSplitter(Self);

end;

procedure TtiSplitterPanel.SetKeepSplitterPosPercent(const AValue: boolean);
begin
  FbKeepSplitterPosPercent := AValue;
//  FiSaveSplitterPosPercent := SplitterPosPercent;
end;

function TtiSplitterPanel.GetColorPanel: TColor;
begin
  result := Color;
end;

procedure TtiSplitterPanel.SetColorPanel(const AValue: TColor);
begin
  Color := AValue;
  FPanel1.Color := AValue;
  FPanel2.Color := AValue;
end;

function TtiSplitterPanel.GetAligned: TAlign;
begin
  result := Align;
end;

procedure TtiSplitterPanel.SetAligned(const AValue: TAlign);
begin
  if (csLoading in ComponentState ) then
  begin
    FSaveAlign := AValue;
  end
  else
    Align := AValue;
end;

end.


