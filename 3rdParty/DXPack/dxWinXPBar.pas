
{*******************************************************************}
{                                                                   }
{   Design eXperince Visual Component Library                       }
{   WinXPBar (dxWinXPBar)                                           }
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
{   HISTORY:                                                        }
{   =============================================================== }
{   2003/02/06                                                      }
{     + First Release.                                              }
{                                                                   }
{*******************************************************************}

unit dxWinXPBar;

interface

{$I dxWarn.inc}

uses
  Forms, Windows, Classes, Controls, Graphics, SysUtils, ImgList, ActnList,
  dxCore, dxCoreUtils;

{$R dxWinXPBar.res}

type
{ TdxWinXPBarRollDirection

  Warning: Never change order of enumeration because of
           hardcoded type castes! }

  TdxWinXPBarRollDirection = (
    rdExpand,           // expand roll
    rdCollapse          // collapse roll
  );

{ TdxWinXPBarRollMode }

  TdxWinXPBarRollMode = (
    rmFixed,            // fixed mode (default)
    rmShrink            // shrink mode
  );

{ TdxWinXPBarHitTest }

  TdxWinXPBarHitTest = (
    htNone,             // mouse is inside non-supported rect
    htHeader,           // mouse is inside header
    htRollButton        // mouse is inside rollbutton
  );

{ forward declarations }

  TdxWinXPBarItem = class;
  TdxWinXPBarItems = class;
  TdxCustomWinXPBar = class;

{ TdxWinXPBarOnCanChangeEvent }

  TdxWinXPBarOnCanChangeEvent = procedure(Sender: TObject; Item: TdxWinXPBarItem;
    var AllowChange: Boolean) of object;

{ TdxWinXPBarOnDrawItemEvent }

  TdxWinXPBarOnDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    Rect: TRect; State: TdxDrawState; Item: TdxWinXPBarItem; Bitmap: TBitmap) of object;

{ TdxWinXPBarOnItemClickEvent }

  TdxWinXPBarOnItemClickEvent = procedure(Sender: TObject; Item: TdxWinXPBarItem) of object;

{ TdxWinXPBarItemActionLink }

  TdxWinXPBarItemActionLink = class(TActionLink)
  protected
    FClient: TdxWinXPBarItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

{ TdxWinXPBarItemActionLinkClass }

  TdxWinXPBarItemActionLinkClass = class of TdxWinXPBarItemActionLink;

{ TdxWinXPBarItem }

  TdxWinXPBarItem = class(TCollectionItem)
  private
    FActionLink: TdxWinXPBarItemActionLink;
    FCollection: TdxWinXPBarItems;
    FCaption: TCaption;
    FData: Pointer;
    FDataObject: TObject;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: TImageIndex;
    FImageList: TCustomImageList;
    FName: string;
    FWinXPBar: TdxCustomWinXPBar;
    FTag: Integer;
    FVisible: Boolean;
    FOnClick: TNotifyEvent;
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsOnClickStored: Boolean;
    function GetImages: TCustomImageList;
    procedure DoActionChange(Sender: TObject);
    procedure SetAction(Value: TBasicAction);
    procedure SetCaption(Value: TCaption);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetName(Value: string);
    procedure SetVisible(Value: Boolean);
  protected
    function GetActionLinkClass: TdxWinXPBarItemActionLinkClass; dynamic;
    function GetAction: TBasicAction; virtual;
    function GetDisplayName: string; override;
    procedure Notification(AComponent: TComponent); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    property ActionLink: TdxWinXPBarItemActionLink read FActionLink write FActionLink;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: TObject read FDataObject write FDataObject;
    property Images: TCustomImageList read GetImages;
    property WinXPBar: TdxCustomWinXPBar read FWinXPBar;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption: TCaption read FCaption write SetCaption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored
      default True;
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      stored IsImageIndexStored default -1;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property Name: string read FName write SetName;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored
      default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
  end;

{ TdxWinXPBarItems }

  TdxWinXPBarItems = class(TCollection)
  private
    FWinXPBar: TdxCustomWinXPBar;
    function GetItem(Index: Integer): TdxWinXPBarItem;
    procedure SetItem(Index: Integer; Value: TdxWinXPBarItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(WinXPBar: TdxCustomWinXPBar);
    function Add: TdxWinXPBarItem; overload;
    function Add(Action: TBasicAction): TdxWinXPBarItem; overload;
    function Add(DataObject: TObject): TdxWinXPBarItem; overload;
    function Insert(Index: Integer): TdxWinXPBarItem; overload;
    function Insert(Index: Integer; Action: TBasicAction): TdxWinXPBarItem; overload;
    function Insert(Index: Integer; DataObject: TObject): TdxWinXPBarItem; overload;
    function Find(const AName: string): TdxWinXPBarItem; overload;
    function Find(const Action: TBasicAction): TdxWinXPBarItem; overload;
    function Find(const DataObject: TObject): TdxWinXPBarItem; overload;
    property Items[Index: Integer]: TdxWinXPBarItem read GetItem write SetItem; default;
  end;

{ TdxWinXPBarVisibleItems }

  TdxWinXPBarVisibleItems = class(TPersistent)
  private
    FItems: TList;
    FWinXPBar: TdxCustomWinXPBar;
    function Exists(Item: TdxWinXPBarItem): Boolean;
    function GetItem(Index: Integer): TdxWinXPBarItem;
    procedure Add(Item: TdxWinXPBarItem);
    procedure Delete(Item: TdxWinXPBarItem);
    procedure Clear;
  public
    constructor Create(WinXPBar: TdxCustomWinXPBar);
    destructor Destroy; override;
    function Count: Integer;
    property Items[Index: Integer]: TdxWinXPBarItem read GetItem; default;
  end;

{ TdxFadeThread }

  TdxFadeThread = class(TThread)
  private
    FWinXPBar: TdxCustomWinXPBar;
    FRollDirection: TdxWinXPBarRollDirection;
  public
    constructor Create(WinXPBar: TdxCustomWinXPBar; RollDirection: TdxWinXPBarRollDirection);
    procedure Execute; override;
  end;

{ TdxCustomWinXPBar }

  TdxCustomWinXPBar = class(TdxCustomControl)
  private
    FBodyColor: TColor;
    FCollapsed: Boolean;
    FFadeThread: TdxFadeThread;
    FFont: TFont;
    FFontChanging: Boolean;
    FGradient: TBitmap;
    FGradientFrom: TColor;
    FGradientTo: TColor;
    FGradientWidth: Integer;
    FHeaderFont: TFont;
    FHitTest: TdxWinXPBarHitTest;
    FHotTrack: Boolean;
    FHotTrackColor: TColor;
    FHoverIndex: Integer;
    FIcon: TIcon;
    FImageList: TCustomImageList;
    FItemHeight: Integer;
    FItems: TdxWinXPBarItems;
    FVisibleItems: TdxWinXPBarVisibleItems;
    FRolling: Boolean;
    FRollMode: TdxWinXPBarRollMode;
    FRollOffset: Integer;
    FSeperatorLine: TColor;
    FShowLinkCursor: Boolean;
    FShowRollButton: Boolean;
    FOnCanChange: TdxWinXPBarOnCanChangeEvent;
    FOnDrawItem: TdxWinXPBarOnDrawItemEvent;
    FOnItemClick: TdxWinXPBarOnItemClickEvent;
    function IsFontStored: Boolean;
    procedure FontChanged(Sender: TObject);
    procedure SetCollapsed(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TdxWinXPBarItems);
    procedure SetRollOffset(const Value: Integer);
    procedure SetShowRollButton(Value: Boolean);
    procedure ResizeToMaxHeight;
  protected
    function GetHitTestRect(const HitTest: TdxWinXPBarHitTest): TRect;
    function GetItemRect(Index: Integer): TRect; virtual;
    procedure ItemVisibilityChanged(Item: TdxWinXPBarItem); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HookMouseDown; override;
    procedure HookMouseMove(X: Integer = 0; Y: Integer = 0); override;
    procedure HookParentFontChanged; override;
    procedure HookResized; override;
    procedure SortVisibleItems(const Redraw: Boolean);
    procedure DoDrawItem(const Index: Integer; State: TdxDrawState); virtual;
    procedure Paint; override;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont stored IsFontStored;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default $00FF7C35;
    property Icon: TIcon read FIcon write SetIcon;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 20;
    property Items: TdxWinXPBarItems read FItems write SetItems;
    property Rolling: Boolean read FRolling default False;
    property RollMode: TdxWinXPBarRollMode read FRollMode write FRollMode default rmShrink;
    property RollOffset: Integer read FRollOffset write SetRollOffset;
    property ShowLinkCursor: Boolean read FShowLinkCursor write FShowLinkCursor default True;
    property ShowRollButton: Boolean read FShowRollButton write SetShowRollButton default True;
    property OnCanChange: TdxWinXPBarOnCanChangeEvent read FOnCanChange write FOnCanChange;
    property OnDrawItem: TdxWinXPBarOnDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TdxWinXPBarOnItemClickEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHitTestAt(X, Y: Integer): TdxWinXPBarHitTest;
    procedure EndUpdate; override;
    procedure Click; override;
    property Height default 46;
    property VisibleItems: TdxWinXPBarVisibleItems read FVisibleItems;
    property Width default 153;
    procedure Clear;
  end;

{ TdxWinXPBar }

  TdxWinXPBar = class(TdxCustomWinXPBar)
  published
    property Caption;
    property Collapsed;
    property Font;
    property HeaderFont;
    property HotTrack;
    property HotTrackColor;
    property Icon;
    property ImageList;
    property ItemHeight;
    property Items;
    property RollMode;
    property ShowLinkCursor;
    property ShowRollButton;
    property OnCanChange;
    property OnDrawItem;
    property OnItemClick;
  end;

implementation

const
  FC_HEADER_HEIGHT = 34;
  FC_ITEM_MARGIN   = 8;

{-----------------------------------------------------------------------------
  Procedure: SortByIndex
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item1, Item2: Pointer
  Result:    Integer
-----------------------------------------------------------------------------}

function SortByIndex(Item1, Item2: Pointer): Integer;
var
  Idx1, Idx2: Integer;
begin
  Idx1 := TCollectionItem(Item1).Index;
  Idx2 := TCollectionItem(Item2).Index;
  if Idx1 < Idx2 then
    Result := -1
  else if Idx1 = Idx2 then
    Result := 0
  else
    Result := 1;
end;

{ TdxWinXPBarItemActionLink }

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.AssignClient
  Author:    mh
  Date:      25-Okt-2002
  Arguments: AClient: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TdxWinXPBarItem;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsCaptionLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsEnabledLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsHintLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsImageIndexLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsVisibleLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.IsOnExecuteLinked
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    dxMethodsEqual(TMethod(FClient.OnClick), TMethod(Action.OnExecute));
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetCaption
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const Value: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetEnabled
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetHint
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const Value: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetImageIndex
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetVisible
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItemActionLink.SetOnExecute
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: TNotifyEvent
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;

{ TdxWinXPBarItem }

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.Create
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Collection: TCollection
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxWinXPBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := TdxWinXPBarItems(Collection);
  FCaption := '';
  FData := nil;
  FDataObject := nil;
  FEnabled := True;
  FImageIndex := -1;
  FImageList := nil;
  FHint := '';
  FName := '';
  FWinXPBar := FCollection.FWinXPBar;
  FTag := 0;
  FVisible := True;
  FWinXPBar.ItemVisibilityChanged(Self);
  FWinXPBar.ResizeToMaxHeight;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.Destroy
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxWinXPBarItem.Destroy;
begin
  FVisible := False;  // required to remove from visible list!
  FWinXPBar.ItemVisibilityChanged(Self);
  FActionLink.Free;
  FActionLink := nil;
  inherited;
  FWinXPBar.ResizeToMaxHeight;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.Notification
  Author:    mh
  Date:      29-Okt-2002
  Arguments: AComponent: TComponent; Operation: TOperation
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.Notification(AComponent: TComponent);
begin
  if AComponent = Action then
    Action := nil;
  if AComponent = FImageList then
    FImageList := nil;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.GetDisplayName
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    string
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.GetDisplayName: string;
var
  DisplayName, ItemName: string;
begin
  DisplayName := FCaption;
  if DisplayName = '' then
    DisplayName := 'untitled';
  ItemName := FName;
  if ItemName <> '' then
    DisplayName := DisplayName + ' [' + ItemName + ']';
  if not FVisible then
    DisplayName := DisplayName + '*';
  Result := DisplayName;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.GetImages
  Author:    mh
  Date:      29-Okt-2002
  Arguments: None
  Result:    TCustomImageList
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.GetImages: TCustomImageList;
begin
  Result := nil;
  if Assigned(FImageList) then
    Result := FImageList
  else if Assigned(Action) and Assigned(TAction(Action).ActionList.Images) then
    Result := TAction(Action).ActionList.Images
  else if Assigned(FWinXPBar.FImageList) then
    Result := FWinXPBar.FImageList;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.ActionChange
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Sender: TObject; CheckDefaults: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.GetActionLinkClass
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    TdxWinXPBarItemActionLinkClass
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.GetActionLinkClass: TdxWinXPBarItemActionLinkClass;
begin
  Result := TdxWinXPBarItemActionLink;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.Assign
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Source: TPersistent
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.Assign(Source: TPersistent);
begin
  if Source is TdxWinXPBarItem then
  begin
    Action.Assign(TdxWinXPBarItem(Source).Action);
    Caption := TdxWinXPBarItem(Source).Caption;
    Data := TdxWinXPBarItem(Source).Data;
    DataObject := TdxWinXPBarItem(Source).DataObject;
    Enabled := TdxWinXPBarItem(Source).Enabled;
    Hint := TdxWinXPBarItem(Source).Hint;
    ImageList.Assign(TdxWinXPBarItem(Source).ImageList);
    ImageIndex := TdxWinXPBarItem(Source).ImageIndex;
    Name := TdxWinXPBarItem(Source).Name;
    Tag := TdxWinXPBarItem(Source).Tag;
    Visible := TdxWinXPBarItem(Source).Visible;
    OnClick := TdxWinXPBarItem(Source).OnClick;
    Exit;
  end;
  inherited Assign(Source);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsCaptionStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsEnabledStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsHintStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsImageIndexStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsVisibleStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.IsOnClickStored
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.DoActionChange
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.GetAction
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    TBasicAction
-----------------------------------------------------------------------------}

function TdxWinXPBarItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetAction
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: TBasicAction
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
    FWinXPBar.InternalRedraw;  // redraw image
  end else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(FWinXPBar);  // deligates notification to WinXPBar!
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetCaption
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const Value: TCaption
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetEnabled
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetImageIndex
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetImageList
  Author:    mh
  Date:      28-Okt-2002
  Arguments: Value: TCustomImageList
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetName
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: string
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetName(Value: string);
begin
  if (Value <> FName) and (FCollection.Find(Value) = nil) then
    FName := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItem.SetVisible
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    FWinXPBar.ItemVisibilityChanged(Self);
    FWinXPBar.ResizeToMaxHeight;
  end;
end;

{ TdxWinXPBarItems }

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Create
  Author:    mh
  Date:      25-Okt-2002
  Arguments: WinXPBar: TdxCustomWinXPBar
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxWinXPBarItems.Create(WinXPBar: TdxCustomWinXPBar);
begin
  inherited Create(TdxWinXPBarItem);
  FWinXPBar := WinXPBar;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Add
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Add: TdxWinXPBarItem;
begin
  Result := TdxWinXPBarItem(inherited Add);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Add (Action)
  Author:    mh
  Date:      29-Okt-2002
  Arguments: Action: TBasicAction
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Add(Action: TBasicAction): TdxWinXPBarItem;
begin
  Result := Add;
  Result.Action := Action;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Add (DataObject)
  Author:    mh
  Date:      29-Okt-2002
  Arguments: DataObject: TObject
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Add(DataObject: TObject): TdxWinXPBarItem;
begin
  Result := Add;
  Result.DataObject := DataObject;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Insert
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Index: Integer
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Insert(Index: Integer): TdxWinXPBarItem;
begin
  Result := TdxWinXPBarItem(inherited Insert(Index));
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Insert (Action)
  Author:    mh
  Date:      29-Okt-2002
  Arguments: Index: Integer; DataObject: TObject
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Insert(Index: Integer; Action: TBasicAction): TdxWinXPBarItem;
begin
  Result := Insert(Index);
  Result.Action := Action;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Insert (DataObject)
  Author:    mh
  Date:      29-Okt-2002
  Arguments: Index: Integer; DataObject: TObject
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Insert(Index: Integer; DataObject: TObject): TdxWinXPBarItem;
begin
  Result := Insert(Index);
  Result.DataObject := DataObject;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.GetOwner
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    TPersistent
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.GetOwner: TPersistent;
begin
  Result := FWinXPBar;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.GetItem
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Index: Integer
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.GetItem(Index: Integer): TdxWinXPBarItem;
begin
  Result := TdxWinXPBarItem(inherited GetItem(Index));
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.SetItem
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Index: Integer; Value: TdxWinXPBarItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItems.SetItem(Index: Integer; Value: TdxWinXPBarItem);
begin
  inherited SetItem(Index, Value);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Update
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item: TCollectionItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarItems.Update(Item: TCollectionItem);
begin
  FWinXPBar.SortVisibleItems(True);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Find (Name)
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const AName: string
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Find(const AName: string): TdxWinXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  if Items[i].Name = AName then
  begin
    Result := Items[i];
    Break;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Find (Action)
  Author:    mh
  Date:      30-Okt-2002
  Arguments: const Action: TBasicAction
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Find(const Action: TBasicAction): TdxWinXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  if Items[i].Action = Action then
  begin
    Result := Items[i];
    Break;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarItems.Find (ByObject)
  Author:    mh
  Date:      29-Okt-2002
  Arguments: const DataObject: TObject
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarItems.Find(const DataObject: TObject): TdxWinXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  if Items[i].DataObject = DataObject then
  begin
    Result := Items[i];
    Break;
  end;
end;

{ TdxWinXPBarVisibleItems }

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Create
  Author:    mh
  Date:      25-Okt-2002
  Arguments: WinXPBar: TdxCustomWinXPBar
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxWinXPBarVisibleItems.Create(WinXPBar: TdxCustomWinXPBar);
begin
  inherited Create;
  FItems := TList.Create;
  FWinXPBar := WinXPBar;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Destroy
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxWinXPBarVisibleItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.GetItem
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Index: Integer
  Result:    TdxWinXPBarItem
-----------------------------------------------------------------------------}

function TdxWinXPBarVisibleItems.GetItem(Index: Integer): TdxWinXPBarItem;
begin
  Result := nil;
  if Index < FItems.Count then
    Result := FItems[Index];
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Count
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    Integer
-----------------------------------------------------------------------------}

function TdxWinXPBarVisibleItems.Count: Integer;
begin
  Result := FItems.Count;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Exists
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item: TdxWinXPBarItem
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxWinXPBarVisibleItems.Exists(Item: TdxWinXPBarItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  if Items[i] = Item then
  begin
    Result := True;
    Break;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Add
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item: TdxWinXPBarItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarVisibleItems.Add(Item: TdxWinXPBarItem);
begin
  if Exists(Item) then
    Exit;
  FItems.Add(Item);
  FWinXPBar.SortVisibleItems(False);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxWinXPBarVisibleItems.Delete
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item: TdxWinXPBarItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxWinXPBarVisibleItems.Delete(Item: TdxWinXPBarItem);
begin
  if not Exists(Item) then
    Exit;
  FItems.Delete(FItems.IndexOf(Item));
end;

{ TdxFadeThread }

{-----------------------------------------------------------------------------
  Procedure: TdxFadeThread.Create
  Author:    mh
  Date:      25-Okt-2002
  Arguments: WinXPBar: TdxCustomWinXPBar; FadeDirection: TdxWinXPBarRollDirection
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxFadeThread.Create(WinXPBar: TdxCustomWinXPBar;
  RollDirection: TdxWinXPBarRollDirection);
begin
  inherited Create(False);
  FWinXPBar := WinXPBar;
  FRollDirection := RollDirection;
  FreeOnTerminate := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxFadeThread.Execute
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxFadeThread.Execute;
const
  RollSteps = 3;
var
  NewOffset: Integer;
begin
  while not Terminated do
  try
    FWinXPBar.FRolling := True;

    { calculate new roll offset }
    if FRollDirection = rdCollapse then
      NewOffset := FWinXPBar.RollOffset - RollSteps
    else
      NewOffset := FWinXPBar.RollOffset + RollSteps;

    { validate offset ranges }
    if NewOffset < 0 then
      NewOffset := 0;
    if NewOffset > FWinXPBar.FItemHeight then
      NewOffset := FWinXPBar.FItemHeight;
    FWinXPBar.RollOffset := NewOffset;

    { terminate on 'out-of-range' }
    if ((FRollDirection = rdCollapse) and (NewOffset = 0)) or
       ((FRollDirection = rdExpand) and (NewOffset = FWinXPBar.FItemHeight)) then
      Terminate;

    { idle process }
    Sleep(25);
  finally
    FWinXPBar.FRolling := False;
  end;

  { redraw button state }
  FWinXPBar.FCollapsed := FRollDirection = rdCollapse;
  if FWinXPBar.FShowRollButton then
    FWinXPBar.InternalRedraw;

  { update inspector }
  if csDesigning in FWinXPBar.ComponentState then
    TCustomForm(FWinXPBar.Owner).Designer.Modified;
end;

{ TdxCustomWinXPBar }

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.Create
  Author:    mh
  Date:      20-Aug-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TdxCustomWinXPBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  ExControlStyle := [csRedrawCaptionChanged];
  Height := 46;
  HotTrack := True;  // initialize mouse events
  Width := 153;
  FBodyColor := $00F7DFD6;
  FCollapsed := False;
  FFadeThread := nil;
  FFont := TFont.Create;
  FFont.Color := $00E75100;
  FFont.Size := 10;
  FFont.OnChange := FontChanged;
  FGradient := TBitmap.Create;
  FGradientFrom := clWhite;
  FGradientTo := $00F7D7C6;
  FGradientWidth := 0;
  FHeaderFont := TFont.Create;
  FHeaderFont.Color := $00E75100;
  FHeaderFont.Size := 10;
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := FontChanged;
  FHitTest := htNone;
  FHotTrackColor := $00FF7C35;
  FHoverIndex := -1;
  FIcon := TIcon.Create;
  FItemHeight := 20;
  FItems := TdxWinXPBarItems.Create(Self);
  FRolling := False;
  FRollMode := rmShrink;
  FRollOffset := FItemHeight;
  FSeperatorLine := $00F7D7C6;
  FShowLinkCursor := True;
  FShowRollButton := True;
  FVisibleItems := TdxWinXPBarVisibleItems.Create(Self);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.Destroy
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TdxCustomWinXPBar.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  FGradient.Free;
  FIcon.Free;
  FItems.Free;
  FVisibleItems.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.Notification
  Author:    mh
  Date:      25-Okt-2002
  Arguments: AComponent: TComponent; Operation: TOperation
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  if not(csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = FImageList then
      FImageList := nil;
    for i := 0 to FItems.Count - 1 do
      FItems[i].Notification(AComponent);
  end;
  inherited Notification(AComponent, Operation);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.IsFontStored
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    Boolean
-----------------------------------------------------------------------------}

function TdxCustomWinXPBar.IsFontStored: Boolean;
begin
  Result := not ParentFont and not DesktopFont;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.FontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.FontChanged(Sender: TObject);
begin
  if (not FFontChanging) and not(csLoading in ComponentState) then
    ParentFont := False;
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.ResizeToMaxHeight
  Author:    mh
  Date:      29-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.ResizeToMaxHeight;
var
  NewHeight: Integer;
begin
  { TODO: Check this!!! }
  if IsLocked then
    Exit;

  NewHeight := FC_HEADER_HEIGHT + FVisibleItems.Count * FRollOffset + FC_ITEM_MARGIN + 1;

  { full collapsing }
  if (FRolling and not FCollapsed) or (not FRolling and FCollapsed) or
    (FVisibleItems.Count = 0) then Dec(NewHeight, FC_ITEM_MARGIN);

  Height := NewHeight;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.GetHitTestAt
  Author:    mh
  Date:      05-Nov-2002
  Arguments: X, Y: Integer
  Result:    TdxWinXPBarHitTest
-----------------------------------------------------------------------------}

function TdxCustomWinXPBar.GetHitTestAt(X, Y: Integer): TdxWinXPBarHitTest;
begin
  Result := htNone;
  if PtInRect(GetHitTestRect(htHeader), Point(X, Y)) then
    Result := htHeader;
  if PtInRect(GetHitTestRect(htRollButton), Point(X, Y)) then
    Result := htRollButton;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.GetItemRect
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Index: Integer
  Result:    TRect
-----------------------------------------------------------------------------}

function TdxCustomWinXPBar.GetItemRect(Index: Integer): TRect;
begin
  Result.Left := 3;
  Result.Right := Width - 8;
  if FRollMode = rmShrink then
    Result.Top := FC_HEADER_HEIGHT + FC_ITEM_MARGIN div 2 + Index * FRollOffset + 1
  else
    Result.Top := FC_HEADER_HEIGHT + FC_ITEM_MARGIN div 2 + Index * FItemHeight + 1;
  Result.Bottom := Result.Top + FItemHeight;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.GetHitTestRect
  Author:    mh
  Date:      05-Nov-2002
  Arguments: const HitTest: TdxWinXPBarHitTest
  Result:    TRect
-----------------------------------------------------------------------------}

function TdxCustomWinXPBar.GetHitTestRect(const HitTest: TdxWinXPBarHitTest): TRect;
begin
  case HitTest of
    htHeader:
      Result := Bounds(0, 5, Width, 28);
    htRollButton:
      Result := Bounds(Width - 24, 10, 18, 18);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SortVisibleItems
  Author:    mh
  Date:      29-Okt-2002
  Arguments: const Redraw: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SortVisibleItems(const Redraw: Boolean);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FVisibleItems.FItems.Sort(@SortByIndex);
  if Redraw then
    InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.ItemVisibilityChanged
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Item: TdxWinXPBarItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.ItemVisibilityChanged(Item: TdxWinXPBarItem);
begin
  // update visible-item list
  if Item.Visible then
    FVisibleItems.Add(Item)
  else
    FVisibleItems.Delete(Item);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.HookMouseDown
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.HookMouseDown;
var
  Rect: TRect;
begin
  inherited;  // update drawstate
  if FHitTest = htRollButton then
  begin
    Rect := GetHitTestRect(htRollButton);
    InvalidateRect(Handle, @Rect, False);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.HookMouseMove
  Author:    mh
  Date:      25-Okt-2002
  Arguments: X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.HookMouseMove(X, Y: Integer);
var
  Rect: TRect;
  OldHitTest: TdxWinXPBarHitTest;
  NewIndex, Header: Integer;
begin
  OldHitTest := FHitTest;
  FHitTest := GetHitTestAt(X, Y);
  if FHitTest <> OldHitTest then
  begin
    Rect := Bounds(0, 5, Width, 28);    // header
    InvalidateRect(Handle, @Rect, False);
    if FShowLinkCursor then
    begin
      if FHitTest <> htNone then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
    end;
  end;
  Header := FC_HEADER_HEIGHT + FC_ITEM_MARGIN;
  if (Y < Header) or (Y > Height - FC_ITEM_MARGIN) then
    NewIndex := -1
  else
    NewIndex := (Y - Header) div ((Height - Header) div FVisibleItems.Count);
  if NewIndex <> FHoverIndex then
  begin
    if FHoverIndex <> -1 then
      DoDrawItem(FHoverIndex, []);
    FHoverIndex := NewIndex;
    if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex].Enabled) then
    begin
      DoDrawItem(FHoverIndex, [dsHighlight]);
      if FShowLinkCursor then
        Cursor := crHandPoint;
    end else
    if FShowLinkCursor then
      Cursor := crDefault;
  end;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.HookParentFontChanged
  Author:    mh
  Date:      30-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.HookParentFontChanged;
begin
  if ParentFont then
  begin
    FFontChanging := True;
    try
      FFont.Color := $00E75100;
      FFont.Name := inherited Font.Name;
      FFont.Size := 10;
      FFont.Style := inherited Font.Style;
      FHeaderFont.Color := $00E75100;
      FHeaderFont.Name := Font.Name;
      FHeaderFont.Size := 10;
      FHeaderFont.Style := [fsBold];
    finally
      FFontChanging := False;
    end;
    inherited;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.HookResized
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.HookResized;
begin
  // perform actions only on 'width'-change
  if FGradientWidth <> Width then
  begin
    FGradientWidth := Width;

    // recreate gradient rect
    dxCreateGradientRect(Width, 28, clWhite, $00F7D7C6, 32, gsLeft, True,
      FGradient);
  end;

  // resize to maximum height
  ResizeToMaxHeight;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetCollapsed
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetCollapsed(Value: Boolean);
begin
  if Value <> FCollapsed then
    FFadeThread := TdxFadeThread.Create(Self, TdxWinXPBarRollDirection(Value));
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetFont
  Author:    mh
  Date:      30-Okt-2002
  Arguments: Value: TFont
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetHeaderFont
  Author:    mh
  Date:      30-Okt-2002
  Arguments: Value: TFont
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetHotTrack
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetHotTrack(Value: Boolean);
const
  MouseEvents: TdxControlStyle = [csRedrawMouseEnter, csRedrawMouseLeave];
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    if FHotTrack then
      ExControlStyle := ExControlStyle + MouseEvents
    else
      ExControlStyle := ExControlStyle - MouseEvents;
    if not(csLoading in ComponentState) then
      InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetHotTrackColor
  Author:    mh
  Date:      30-Okt-2002
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetHotTrackColor(Value: TColor);
begin
  if Value <> FHotTrackColor then
  begin
    FHotTrackColor := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetIcon
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: TIcon
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetIcon(Value: TIcon);
begin
  if Value <> FIcon then
  begin
    FIcon.Assign(Value);
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetImageList
  Author:    mh
  Date:      29-Okt-2002
  Arguments: Value: TCustomImageList
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetItemHeight
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetItemHeight(Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    if not FCollapsed then
      RollOffset := FItemHeight;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetItems
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: TdxWinXPBarItem
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetItems(Value: TdxWinXPBarItems);
begin
  FVisibleItems.Clear;
  FItems.Assign(Value);
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetRollOffset
  Author:    mh
  Date:      25-Okt-2002
  Arguments: const Value: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetRollOffset(const Value: Integer);
begin
  if Value <> FRollOffset then
  begin
    FRollOffset := Value;
    ResizeToMaxHeight;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.SetShowRollButton
  Author:    mh
  Date:      25-Okt-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.SetShowRollButton(Value: Boolean);
begin
  if Value <> FShowRollButton then
  begin
    FShowRollButton := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.EndUpdate
  Author:    mh
  Date:      06-Nov-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.EndUpdate;
begin
  inherited;
  ResizeToMaxHeight;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.Click
  Author:    mh
  Date:      25-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.Click;
var
  AllowChange: Boolean;
begin
  if (FShowRollButton) and (FHitTest <> htNone) then
    Collapsed := not Collapsed;
  if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex].Enabled) then
  begin
    AllowChange := True;
    if Assigned(FOnCanChange) then
      FOnCanChange(Self, FVisibleItems[FHoverIndex], AllowChange);
    if not AllowChange then
      Exit;
    if Assigned(FOnItemClick) then
      FOnItemClick(Self, FVisibleItems[FHoverIndex]);
    if Assigned(FVisibleItems[FHoverIndex].FOnClick) then
    begin
      { set linked 'action' as sender }
      if Assigned(FVisibleItems[FHoverIndex].Action) then
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex].Action)
      else
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex]);
    end;
  end;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.DoDrawItem
  Author:    mh
  Date:      29-Okt-2002
  Arguments: const Index: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.DoDrawItem(const Index: Integer; State: TdxDrawState);
var
  Bitmap: TBitmap;
  ItemCaption: string;
  ItemRect: TRect;
  HasImages: Boolean;
begin
  Assert( Index <= FVisibleItems.Count - 1, 'Index outside range of FVisibleItems');

  Bitmap := TBitmap.Create;
  with Canvas do
  try
    Bitmap.Assign(nil);
    Font.Assign(Self.Font);

    if not FVisibleItems[Index].Enabled then
      Font.Color := clGray
    else if dsHighlight in State then
    begin
      Font.Color := FHotTrackColor;
      Font.Style := Font.Style + [fsUnderline];
    end;
    ItemRect := GetItemRect(Index);
    HasImages := FVisibleItems[Index].Images <> nil;
    if HasImages then
      FVisibleItems[Index].Images.GetBitmap(FVisibleItems[Index].ImageIndex, Bitmap);
    Bitmap.Transparent := True;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Canvas, ItemRect, State, FVisibleItems[Index], Bitmap)
    else begin
      if HasImages then
      begin
        // TJK 2005-03-07: By copying to a bitmap - they loose transparency detail. Fixed here.
        FVisibleItems[Index].Images.Draw(
          Canvas,
          ItemRect.Left, ItemRect.Top + (FItemHeight - FVisibleItems[Index].Images.Height) div 2,
          FVisibleItems[Index].ImageIndex);
        //Draw(ItemRect.Left, ItemRect.Top + (FItemHeight - Bitmap.Height) div 2, Bitmap);
      end;
      ItemCaption := FVisibleItems[Index].Caption;
      if ItemCaption = '' then
        ItemCaption := Format('(untitled %d)', [Index]);
      Inc(ItemRect.Left, 20);
      DrawText(Handle, PChar(ItemCaption), -1, ItemRect, DT_SINGLELINE or
        DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
    end;
  finally
    Bitmap.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TdxCustomWinXPBar.Paint
  Author:    mh
  Date:      29-Okt-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TdxCustomWinXPBar.Paint;
var
  Rect: TRect;
  Bitmap: TBitmap;
  Index, i: Integer;
  OwnColor: TColor;
begin
  with Canvas do
  begin
    { get client rect }
    Rect := GetClientRect;

    { fill non-client area }
    Inc(Rect.Top, 5);
    Brush.Color := FBodyColor; //$00F7DFD6;
    FillRect(Rect);

    { draw header }
    dxCreateGradientRect(Width, 28, FGradientFrom, FGradientTo, 32, gsLeft, True,
      FGradient);
    Draw(0, Rect.Top, FGradient);

    { draw frame... }
    Brush.Color := clWhite;
    FrameRect(Rect);

    { ...with cutted edges }
    OwnColor := TdxWinControl(Parent).Color;
    Pixels[0, Rect.Top] := OwnColor;
    Pixels[0, Rect.Top + 1] := OwnColor;
    Pixels[1, Rect.Top] := OwnColor;
    Pixels[1, Rect.Top + 1] := clWhite;
    Pixels[Width - 1, Rect.Top] := OwnColor;
    Pixels[Width - 2, Rect.Top] := OwnColor;
    Pixels[Width - 1, Rect.Top + 1] := OwnColor;
    Pixels[Width - 2, Rect.Top + 1] := clWhite;

    { draw Button }
    if (FShowRollButton) and (Width >= 115) then
    begin
      Bitmap := TBitmap.Create;
      try
        Index := 0;
        if FHitTest = htRollButton then
        begin
          if dsHighlight in DrawState then
            Index := 1;
          if (dsClicked in DrawState) and (dsHighlight in DrawState) then
            Index := 2;
        end;
        if FCollapsed then
          Bitmap.Handle := LoadBitmap(hInstance, PChar('EXPAND' + IntToStr(Index)))
        else
          Bitmap.Handle := LoadBitmap(hInstance, PChar('COLLAPSE' + IntToStr(Index)));
        Draw(Rect.Right - 24, Rect.Top + 5, Bitmap);
      finally
        Bitmap.Free;
      end;
      Dec(Rect.Right, 25);
    end;

    { draw seperator line }
    Pen.Color := FSeperatorLine;
    dxDrawLine(Canvas, 1, Rect.Top + 28, Width - 1, Rect.Top + 28);

    { draw icon }
    Inc(Rect.Left, 22);
    if not FIcon.Empty then
    begin
      Draw(2, 0, FIcon);
      Inc(Rect.Left, 16);
    end;

    { draw caption }
    SetBkMode(Handle, Transparent);
    Font.Assign(FHeaderFont);
    if FHotTrack and (dsHighlight in DrawState) and (FHitTest <> htNone) then
      Font.Color := FHotTrackColor;
    Rect.Bottom := Rect.Top + 28;
    Dec(Rect.Right, 3);
    DrawText(Handle, PChar(Caption), -1, Rect, DT_SINGLELINE or DT_VCENTER or
      DT_END_ELLIPSIS or DT_NOPREFIX);

    { draw visible items }
    Brush.Color := FBodyColor;
    if not FCollapsed or FRolling then
    for i := 0 to FVisibleItems.Count - 1 do
    begin
      if (i <> FHoverIndex) or not(dsHighlight in DrawState) then
        DoDrawItem(i, [])
      else
        DoDrawItem(i, [dsHighlight]);
    end;
  end;
end;

procedure TdxCustomWinXPBar.Clear;
begin
  FVisibleItems.Clear;
  FItems.Clear;
  FHoverIndex := -1;
end;

procedure TdxWinXPBarVisibleItems.Clear;
begin
  FItems.Clear;
end;

end.

