
{*******************************************************************}
{                                                                   }
{   dxCore (Design eXperience)                                      }
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
{   Contact: mhoffmann@apriori.de                                   }
{                                                                   }
{   HISTORY:                                                        }
{   =============================================================== }
{   Version 2.0.0 (2003/02/06)                                      }
{     + Second Release (major update).                              }
{                                                                   }
{   Hint:                                                           }
{     Earlier versions are not reported.                            }
{                                                                   }
{*******************************************************************}

unit dxCore;

interface

{$I dxWarn.inc}

uses
  Windows, Messages, Classes, Controls, Graphics, Forms;

const
  { color constants.

    these constants are used as default colors for descendant controls
    and may be replaced with other (common) values.

    syntax: dxColor_[Control]_[Enabled: Enb, Dis]_[Type]_[Theme: WXP, OXP]     }

  { button colors - WindowsXP }
  dxColor_Btn_Enb_Border_WXP   = $00733800; // border line
  dxColor_Btn_Dis_Border_WXP   = $00BDC7CE; // border line (disabled)
  dxColor_Btn_Enb_Edges_WXP    = $00AD9E7B; // border edges
  dxColor_Btn_Dis_Edges_WXP    = $00BDC7CE; // border edges (disabled)
  dxColor_Btn_Enb_BgFrom_WXP   = $00FFFFFF; // background from
  dxColor_Btn_Enb_BgTo_WXP     = $00E7EBEF; // background to
  dxColor_Btn_Enb_CkFrom_WXP   = $00C6CFD6; // clicked from
  dxColor_Btn_Enb_CkTo_WXP     = $00EBF3F7; // clicked to
  dxColor_Btn_Enb_FcFrom_WXP   = $00FFE7CE; // focused from
  dxColor_Btn_Enb_FcTo_WXP     = $00EF846D; // focused to
  dxColor_Btn_Enb_HlFrom_WXP   = $00CEF3FF; // highlight from
  dxColor_Btn_Enb_HlTo_WXP     = $000096E7; // highlight to

  { checkbox colors - WindowsXP }
  dxColor_Chk_Enb_Border_WXP   = $00845118; // border line
  dxColor_Chk_Enb_NmSymb_WXP   = $0021A621; // symbol normal

  { misc colors - WindowsXP }
  dxColor_Msc_Dis_Caption_WXP  = $0094A6A5; // caption color (disabled)

  dxColor_DotNetFrame          = $00F7FBFF; // $00E7EBEF;
  dxColor_BorderLineOXP        = $00663300;
  dxColor_BgOXP                = $00D6BEB5;
  dxColor_BgCkOXP              = $00CC9999;

type
{ forward declarations }

  TdxCustomStyleControl = class;

{ TdxBoundLines }

  TdxBoundLines = set of (
    blLeft,                             // left line
    blTop,                              // top line
    blRight,                            // right line
    blBottom                            // bottom line
  );

{ TdxControlStyle }

  TdxControlStyle = set of (
    csRedrawCaptionChanged,             // (default)
    csRedrawBorderChanged,              //
    csRedrawEnabledChanged,             // (default)
    csRedrawFocusedChanged,             // (default)
    csRedrawMouseDown,                  // (default)
    csRedrawMouseEnter,                 // (default)
    csRedrawMouseLeave,                 // (default)
    csRedrawMouseMove,                  //
    csRedrawMouseUp,                    // (default)
    csRedrawParentColorChanged,         // (default)
    csRedrawParentFontChanged,          //
    csRedrawPosChanged,                 //
    csRedrawResized                     //
  );

{ TdxDrawState }

  TdxDrawState = set of (
    dsDefault,                          // default
    dsHighlight,                        // highlighted
    dsClicked,                          // clicked
    dsFocused                           // focused
  );

{ TdxGlyphLayout }

  TdxGlyphLayout = (
    glBottom,                           // bottom glyph
    glCenter,                           // centered glyph
    glTop                               // top glyph
  );

{ TdxTheme }

  TdxTheme = (
    WindowsXP,                          // WindowsXP theme
    OfficeXP                            // OfficeXP theme
  );

{ TdxCustomComponent

  baseclass for non-focusable component descendants. }

  TdxCustomComponent = class(TComponent)
  private
    { Private desclarations }
    FVersion: string;
    procedure SetVersion(Value: string);
  protected
    { Protected desclarations }
  public
    { Public desclarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published desclarations }
    property Version: string read FVersion write SetVersion stored False;
  end;

{ TdxWinControl }

  TdxWinControl = class(TWinControl)
  published
    { Published declarations }
    property Color;
  end;

{ TdxCustomControl

  baseclass for focusable control descendants. }

  TdxCustomControl = class(TCustomControl)
  private
    { Private desclarations }
    FClicking: Boolean;
    FDrawState: TdxDrawState;
    FIsLocked: Boolean;
    FIsSibling: Boolean;
    FModalResult: TModalResult;
    FVersion: string;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    procedure SetVersion(Value: string);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    { Protected desclarations }
    ExControlStyle: TdxControlStyle;
    procedure InternalRedraw; dynamic;
    procedure HookBorderChanged; dynamic;
    procedure HookEnabledChanged; dynamic;
    procedure HookFocusedChanged; dynamic;
    procedure HookMouseDown; dynamic;
    procedure HookMouseEnter; dynamic;
    procedure HookMouseLeave; dynamic;
    procedure HookMouseMove(X: Integer = 0; Y: Integer = 0); dynamic;
    procedure HookMouseUp; dynamic;
    procedure HookParentColorChanged; dynamic;
    procedure HookParentFontChanged; dynamic;
    procedure HookPosChanged; dynamic;
    procedure HookResized; dynamic;
    procedure HookTextChanged; dynamic;
    procedure MouseDown(Button:TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X, Y: Integer); override;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
  public
    { Public desclarations }
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure BeginUpdate; dynamic;
    procedure EndUpdate; dynamic;
    property Canvas;
    property DrawState: TdxDrawState read FDrawState write FDrawState;
    property IsLocked: Boolean read FIsLocked write FIsLocked;
    property IsSibling: Boolean read FIsSibling write FIsSibling;
  published
    { Published declarations }
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BiDiMode;
    //property Ctl3D;
    //property DockSite;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    //property TabOrder;
    //property TabStop;
    //property UseDockManager default True;
    property Align;
    property Anchors;
    //property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    //property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Version: string read FVersion write SetVersion stored False;
    property Visible;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnEndDock;
    //property OnGetSiteInfo;
    //property OnStartDock;
    //property OnUnDock;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
  {$IFDEF VER140} // Borland Delphi 6.0
    property OnContextPopup;
  {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TdxUnlimitedControl }

  TdxUnlimitedControl = class(TdxCustomControl);

{ TdxStyle }

  TdxStyle = class(TPersistent)
  private
    { Private declarations }
    FTheme: TdxTheme;
    FUseStyleManager: Boolean;
  protected
    { Protected declarations }
    Parent: TdxCustomStyleControl;
    procedure SetTheme(Value: TdxTheme); virtual;
    procedure SetUseStyleManager(Value: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);
    function GetTheme: TdxTheme;
  published
    { Published declarations }
    property Theme: TdxTheme read FTheme write SetTheme default WindowsXP;
    property UseStyleManager: Boolean read FUseStyleManager write SetUseStyleManager default True;
  end;

{ TdxStyleManager }

  TdxStyleManager = class(TdxCustomComponent)
  private
    { Private desclarations }
    FControls: TList;
    FTheme: TdxTheme;
    FOnThemeChanged: TNotifyEvent;
    procedure InvalidateControls;
  protected
    { Protected desclarations }
    procedure SetTheme(Value: TdxTheme); virtual;
  public
    { Public desclarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterControls(const AControls: array of TdxCustomControl);
    procedure UnregisterControls(const AControls: array of TdxCustomControl);
  published
    { Published desclarations }
    property Theme: TdxTheme read FTheme write SetTheme default WindowsXP;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

{ TdxCustomStyleControl }

  TdxCustomStyleControl = class(TdxCustomControl)
  private
    { Private desclarations }
    FStyle: TdxStyle;
    FStyleManager: TdxStyleManager;
  public
    { Public desclarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    { Protected desclarations }
    procedure SetStyleManager(Value: TdxStyleManager); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    { Published desclarations }
    property Style: TdxStyle read FStyle write FStyle;
    property StyleManager: TdxStyleManager read FStyleManager write SetStyleManager;
  end;

{ TdxGradient }

  TdxGradientColors = 2..255;

  TdxGradientStyle = (gsLeft, gsTop, gsRight, gsBottom);

  TdxGradient = class(TPersistent)
  private
    { Private-Deklarationen }
    FColors: TdxGradientColors;
    FDithered: Boolean;
    FEnabled: Boolean;
    FEndColor: TColor;
    FStartColor: TColor;
    FGradientStyle: TdxGradientStyle;
  protected
    { Protected declarations }
    Parent: TdxCustomControl;
    procedure SetDithered(Value: Boolean); virtual;
    procedure SetColors(Value: TdxGradientColors); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetEndColor(Value: TColor); virtual;
    procedure SetGradientStyle(Value: TdxGradientStyle); virtual;
    procedure SetStartColor(Value: TColor); virtual;
  public
    { Public declarations }
    Bitmap: TBitmap;
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure RecreateBands; virtual;
  published
    { Published-Deklarationen }
    property Dithered: Boolean read FDithered write SetDithered default True;
    property Colors: TdxGradientColors read FColors write SetColors default 16;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property EndColor: TColor read FEndColor write SetEndColor default clSilver;
    property StartColor: TColor read FStartColor write SetStartColor default clGray;
    property Style: TdxGradientStyle read FGradientStyle write SetGradientStyle default gsLeft;
  end;

{$R dxCore.res}

procedure Register;

implementation

uses
  dxCoreUtils;

resourcestring
  SVersion = '2.0.1'; // always increase version number on new releases!

{-----------------------------------------------------------------------------
  Procedure: Register
  Author:    mh
  Date:      24-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('Design eXperience II', [TdxStyleManager]);
end;

{ TdxCustomComponent }

{-----------------------------------------------------------------------------
  Procedure: TdxCustomComponent.Create
  Author:    mh
  Date:      24-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxCustomComponent.Create(AOwner: TComponent);
begin
  inherited;
  FVersion := 'Design eXperience. © 2002 M. Hoffmann Version ' + SVersion;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomComponent.SetVersion
  Author:    mh
  Date:      24-Jun-2002
  Arguments: Value: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomComponent.SetVersion(Value: string);
begin
  ; // don't enable overwriting this constant.
end;

{ TdxCustomControl }

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.Create
  Author:    mh
  Date:      22-Feb-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  DoubleBuffered := True;
  ExControlStyle := [csRedrawEnabledChanged, csRedrawFocusedChanged,
    csRedrawMouseDown, csRedrawMouseEnter, csRedrawMouseLeave, csRedrawMouseUp,
    csRedrawParentColorChanged, csRedrawCaptionChanged];
  FClicking := False;
  FDrawState := [dsDefault];
  FIsLocked := False;
  FIsSibling := False;
  FModalResult := 0;
  FVersion := 'Design eXperience II - © 2002 M. Hoffmann Version ' + SVersion;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.SetVersion
  Author:    mh
  Date:      07-Mrz-2002
  Arguments: Value: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.SetVersion(Value: string);
begin
  ; // disallow changing this property.
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.BeginUpdate
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.BeginUpdate;
begin
  FIsLocked := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.EndUpdate
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.EndUpdate;
begin
  FIsLocked := False;
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.InternalRedraw
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.InternalRedraw;
begin
  if not FIsLocked then
    Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMDialogChar
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Message: TCMDialogChar
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  if IsAccel(CharCode, Caption) and CanFocus and (Focused or
    ((GetKeyState(VK_MENU) and $8000) <> 0)) then
  begin
    Click;
    Result := 1;
  end
  else
    inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMBorderChanged
  Author:    mh
  Date:      13-Aug-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMBorderChanged(var Message: TMessage);
begin
  // deligate message "BorderChanged" to hook.
  inherited;
  HookBorderChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMEnabledChanged
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMEnabledChanged(var Message: TMessage);
begin
  // deligate message "EnabledChanged" to hook.
  inherited;
  HookEnabledChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMFocusChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMFocusChanged(var Message: TMessage);
begin
  // deligate message "FocusChanged" to hook.
  inherited;
  HookFocusedChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMMouseEnter
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMMouseEnter(var Message: TMessage);
begin
  // deligate message "MouseEnter" to hook.
  inherited;
  HookMouseEnter;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMMouseLeave
  Author:    mh
  Date:      21-Feb-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMMouseLeave(var Message: TMessage);
begin
  // deligate message "MouseLeave" to hook.
  inherited;
  HookMouseLeave;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMParentColorChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMParentColorChanged(var Message: TMessage);
begin
  // deligate message "ParentColorChanged" to hook.
  inherited;
  HookParentColorChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMParentFontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMParentFontChanged(var Message: TMessage);
begin
  // deligate message "ParentFontChanged" to hook.
  inherited;
  HookParentFontChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.CMTextChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.CMTextChanged(var Message: TMessage);
begin
  // deligate message "TextChanged" to hook.
  inherited;
  HookTextChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.WMMouseMove
  Author:    mh
  Date:      10-Mai-2002
  Arguments: var Message: TWMMouse
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.WMMouseMove(var Message: TWMMouse);
begin
  // deligate message "MouseMove" to hook.
  inherited;
  HookMouseMove(Message.XPos, Message.YPos);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.WMSize
  Author:    mh
  Date:      22-Feb-2002
  Arguments: var Message: TWMSize
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.WMSize(var Message: TWMSize);
begin
  // deligate message "Size" to hook.
  inherited;
  HookResized;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.WMWindowPosChanged
  Author:    mh
  Date:      16-Aug-2002
  Arguments: var Message: TWMWindowPosChanged
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  // deligate message "WindowPosChanged" to hook.
  inherited;
  HookPosChanged;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.MouseDown
  Author:    mh
  Date:      10-Mai-2002
  Arguments: Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // deligate message "MouseDown" to hook.
  inherited;
  if Button = mbLeft then
  begin
    FClicking := True;
    HookMouseDown;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.MouseUp
  Author:    mh
  Date:      10-Mai-2002
  Arguments: Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // deligate message "MouseUp" to hook.
  inherited;
  if FClicking then
  begin
    FClicking := False;
    HookMouseUp;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.Click
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.Click;
//var
//  Form: TCustomForm;
begin
// Hacked out to investigate AV when closing a form
//  Form := GetParentForm(Self);
//  if Form <> nil then
//    Form.ModalResult := ModalResult;
  inherited Click;
end;

//
// hooks are used to interrupt default windows messages in an easier
// way - it's possible to override them in descedant classes.
// Beware of multiple redraw calls - if you know that the calling
// hooks always redraws the component, use the lock i.e. unlock methods.
//

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookBorderChanged
  Author:    mh
  Date:      13-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookBorderChanged;
begin
  // this hook is called, if the border property was changed.
  // in that case we normaly have to redraw the control.
  if csRedrawBorderChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookEnabledChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookEnabledChanged;
begin
  // this hook is called, if the enabled property was switched.
  // in that case we normaly have to redraw the control.
  if csRedrawEnabledChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookFocusedChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookFocusedChanged;
begin
  // this hook is called, if the currently focused control was changed.
  if Focused then
    Include(FDrawState, dsFocused)
  else
  begin
    Exclude(FDrawState, dsFocused);
    Exclude(FDrawState, dsClicked);
  end;
  FIsSibling := GetParentForm(Self).ActiveControl is TdxCustomControl;
  if csRedrawFocusedChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookMouseEnter
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookMouseEnter;
begin
  // this hook is called, if the user moves (hover) the mouse over the control.
  Include(FDrawState, dsHighlight);
  if csRedrawMouseEnter in ExControlStyle then
    InternalRedraw;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookMouseLeave
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookMouseLeave;
begin
  // this hook is called, if the user moves the mouse away (unhover) from
  // the control.
  Exclude(FDrawState, dsHighlight);
  if csRedrawMouseLeave in ExControlStyle then
    InternalRedraw;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookMouseMove
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookMouseMove(X: Integer = 0; Y: Integer = 0);
begin
  // this hook is called if the user moves the mouse inside the control.
  if csRedrawMouseMove in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookMouseDown
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookMouseDown;
begin
  // this hook is called, if the user presses the left mouse button over the
  // controls.
  if not Focused and CanFocus then
    SetFocus;
  Include(FDrawState, dsClicked);
  if csRedrawMouseDown in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookMouseUp
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookMouseUp;
var
  cPos: TPoint;
  NewControl: TWinControl;
begin
  // this hook is called, if the user releases the left mouse button.
  begin
    Exclude(FDrawState, dsClicked);
    if csRedrawMouseUp in ExControlStyle then
      InternalRedraw;

    // does the cursor is over another supported control?
    GetCursorPos(cPos);
    NewControl := FindVCLWindow(cPos);
    if (NewControl <> nil) and (NewControl <> Self) and
      (NewControl.InheritsFrom(TdxCustomControl)) then
      TdxCustomControl(NewControl).HookMouseEnter;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookParentColorChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookParentColorChanged;
begin
  // this hook is called if, the parent color was changed.
  if csRedrawParentColorChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookParentFontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookParentFontChanged;
begin
  // this hook is called if, the parent font was changed.
  if csRedrawParentFontChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookPosChanged
  Author:    mh
  Date:      16-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookPosChanged;
begin
  // this hook is called, if the window position was changed.
  if csRedrawPosChanged in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookResized
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookResized;
begin
  // this hook is called, if the control was resized.
  if csRedrawResized in ExControlStyle then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomControl.HookTextChanged
  Author:    mh
  Date:      22-Feb-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomControl.HookTextChanged;
begin
  // this hook is called, if the caption was changed.
  if csRedrawCaptionChanged in ExControlStyle then
    InternalRedraw;
end;

{ TdxStyle }

{-----------------------------------------------------------------------------
  Procedure: TdxStyle.Create
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxStyle.Create(AOwner: TComponent);
begin
  inherited Create;
  Parent := TdxCustomStyleControl(AOwner);
  FTheme := WindowsXP;
  FUseStyleManager := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyle.SetTheme
  Author:    mh
  Date:      04-Jul-2002
  Arguments: Value: TdxTheme
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyle.SetTheme(Value: TdxTheme);
begin
  if Value <> FTheme then
  begin
    FTheme := Value;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyle.GetTheme
  Author:    mh
  Date:      05-Jul-2002
  Arguments: None
  Result:    TdxTheme
-----------------------------------------------------------------------------}

function TdxStyle.GetTheme: TdxTheme;
begin
  Result := FTheme;
  if FUseStyleManager and Assigned(Parent.StyleManager) then
    Result := Parent.StyleManager.Theme;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyle.SetUseStyleManager
  Author:    mh
  Date:      05-Jul-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyle.SetUseStyleManager(Value: Boolean);
begin
  if Value <> FUseStyleManager then
  begin
    FUseStyleManager := Value;
    Parent.InternalRedraw;
  end;
end;

{ TdxStyleManager }

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxStyleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TList.Create;
  FTheme := WindowsXP;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxStyleManager.Destroy;
begin
  InvalidateControls;
  FControls.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.InvalidateControls
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyleManager.InvalidateControls;
var
  i: Integer;
begin
  for i := 0 to FControls.Count - 1 do
  with TdxCustomControl(FControls[i]) do
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.SetTheme
  Author:    mh
  Date:      25-Jun-2002
  Arguments: Value: TdxTheme
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyleManager.SetTheme(Value: TdxTheme);
begin
  if Value <> FTheme then
  begin
    FTheme := Value;
    if Assigned(FOnThemeChanged) then
      FOnThemeChanged(Self);
    InvalidateControls;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.RegisterControls
  Author:    mh
  Date:      05-Jul-2002
  Arguments: const AControls: array of TdxCustomControl
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyleManager.RegisterControls(const AControls: array of TdxCustomControl);
var
  i: Integer;
begin
  for i := Low(AControls) to High(AControls) do
  if FControls.IndexOf(AControls[i]) = -1 then
    FControls.Add(AControls[i]);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxStyleManager.UnregisterControls
  Author:    mh
  Date:      05-Jul-2002
  Arguments: const AControls: array of TdxCustomControl
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxStyleManager.UnregisterControls(const AControls: array of TdxCustomControl);
var
  i: Integer;
begin
  for i := Low(AControls) to High(AControls) do
  if FControls.IndexOf(AControls[i]) <> -1 then
    FControls.Delete(FControls.IndexOf(AControls[i]));
end;

{ TdxCustomStyleControl }

{-----------------------------------------------------------------------------
  Procedure: TdxCustomStyleControl.Create
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxCustomStyleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := TdxStyle.Create(Self);
  FStyleManager := nil;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomStyleControl.Destroy
  Author:    mh
  Date:      04-Jul-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxCustomStyleControl.Destroy;
begin
  if FStyleManager <> nil then
    FStyleManager.UnregisterControls([Self]);
  FStyle.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomStyleControl.Notification
  Author:    mh
  Date:      04-Jul-2002
  Arguments: AComponent: TComponent; Operation: TOperation
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomStyleControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent is TdxStyleManager) and (Operation = opRemove) then
    FStyleManager := nil;
  inherited Notification(AComponent, Operation);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomStyleControl.SetStyleManager
  Author:    mh
  Date:      04-Jul-2002
  Arguments: Value: TdxStyleManager
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomStyleControl.SetStyleManager(Value: TdxStyleManager);
begin
  if Value <> FStyleManager then
  begin
    if Value <> nil then
      Value.RegisterControls([Self])
    else
      FStyleManager.UnregisterControls([Self]);
    FStyleManager := Value;
    InternalRedraw;
  end;
end;

{ TdxGradient }

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.Create
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: AOwner: TControl
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxGradient.Create(AOwner: TControl);
begin
  inherited Create;
  Parent := TdxCustomControl(AOwner);
  Bitmap := TBitmap.Create;
  FColors := 16;
  FDithered := True;
  FEnabled := False;
  FEndColor := clSilver;
  FGradientStyle := gsLeft;
  FStartColor := clGray;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.Destroy
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxGradient.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.RecreateBands
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.RecreateBands;
begin
  if Assigned(Bitmap) then
    dxCreateGradientRect(Parent.Width, Parent.Height, FStartColor, FEndColor,
      FColors, FGradientStyle, FDithered, Bitmap);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetDithered
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetDithered(Value: Boolean);
begin
  if FDithered <> Value then
  begin
    FDithered := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetColors
  Author:    mh
  Date:      05-Jul-2002
  Arguments: Value: TdxGradientColors
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetColors(Value: TdxGradientColors);
begin
  if FColors <> Value then
  begin
    FColors := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetEnabled
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetEndColor
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetEndColor(Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetGradientStyle
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TdxGradientStyle
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetGradientStyle(Value: TdxGradientStyle);
begin
  if FGradientStyle <> Value then
  begin
    FGradientStyle := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxGradient.SetStartColor
  Author:    M. Hoffmann
  Date:      07-Nov-2001
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxGradient.SetStartColor(Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    RecreateBands;
    Parent.InternalRedraw;
  end;
end;

end.

