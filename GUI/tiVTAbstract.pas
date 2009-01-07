unit tiVTAbstract;

{$I tiDefines.inc}

interface
uses
  tiVirtualTrees,
  tiFocusPanel,
  tiObject,
  Graphics,
  Classes;

type

  TtiVTAbstract = class;

  TtiVTOnGetHint = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    var AHintText: WideString) of object;

  TtiVTOnPaintText = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    const ACanvas : TCanvas) of object;

  // ToDo: Gradual migration of TtiVTListView and TtiVTTreeView source that's
  //       duplicated into TtiVTAbstract
  TtiVTAbstract = class(TtiFocusPanel)
  private
    FVT: TVirtualStringTree;
    FOnPaintText: TtiVTOnPaintText;
    FOnGetNodeHint: TtiVTOnGetHint;
    procedure SetOnPaintText(const AValue: TtiVTOnPaintText);
    procedure DoOnPaintText(ASender: TBaseVirtualTree;
      const ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
      ATextType: TVSTTextType);
    procedure SetOnGetNodeHint(const AValue: TtiVTOnGetHint);
    procedure DoOnGetHint(Sender: TBaseVirtualTree; ANode: PVirtualNode;
      Column: TColumnIndex; var ALineBreakStyle: TVTTooltipLineBreakStyle;
      var AHintText: WideString);
    function GetScrollEvent: TVTScrollEvent;
    procedure SetScrollEvent(const AValue: TVTScrollEvent);
    function GetOnCollapsed: TVTChangeEvent;
    function GetOnExpanded: TVTChangeEvent;
    procedure SetOnCollapsed(const Value: TVTChangeEvent);
    procedure SetOnExpanded(const Value: TVTChangeEvent);
  protected
    function GetObjectFromNode(Node: PVirtualNode): TtiObject; virtual; abstract;
    procedure SetVT(const AVT: TVirtualStringTree);

    property OnPaintText: TtiVTOnPaintText read FOnPaintText Write SetOnPaintText;
    property OnGetNodeHint: TtiVTOnGetHint read FOnGetNodeHint write SetOnGetNodeHint;
    property OnScroll: TVTScrollEvent read GetScrollEvent write SetScrollEvent;
    property OnCollapsed: TVTChangeEvent read GetOnCollapsed write SetOnCollapsed;
    property OnExpanded: TVTChangeEvent read GetOnExpanded write SetOnExpanded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property VT: TVirtualStringTree read FVT;
  end;

implementation
  
{ TtiVTAbstract }

constructor TtiVTAbstract.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TtiVTAbstract.Destroy;
begin
  inherited;
end;

procedure TtiVTAbstract.DoOnPaintText(ASender: TBaseVirtualTree;
  const ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
  ATextType: TVSTTextType);
var
  LData: TtiObject;
begin
  if Assigned(FOnPaintText) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnPaintText(Self, ANode, LData, ATargetCanvas);
  end;
end;

function TtiVTAbstract.GetOnCollapsed: TVTChangeEvent;
begin
  result := VT.OnCollapsed;
end;

function TtiVTAbstract.GetOnExpanded: TVTChangeEvent;
begin
  result := VT.OnExpanded;
end;

function TtiVTAbstract.GetScrollEvent: TVTScrollEvent;
begin
  result := VT.OnScroll;
end;

procedure TtiVTAbstract.SetOnPaintText(const AValue: TtiVTOnPaintText);
begin
  FOnPaintText := AValue;
  if Assigned(FOnPaintText) then
    FVT.OnPaintText := DoOnPaintText
  else
    FVT.OnPaintText := nil;
end;

procedure TtiVTAbstract.SetScrollEvent(const AValue: TVTScrollEvent);
begin
  VT.OnScroll := AValue;
end;

procedure TtiVTAbstract.SetVT(const AVT: TVirtualStringTree);
begin
  FVT:= AVT;
end;

procedure TtiVTAbstract.SetOnCollapsed(const Value: TVTChangeEvent);
begin
  VT.OnCollapsed := Value;
end;

procedure TtiVTAbstract.SetOnExpanded(const Value: TVTChangeEvent);
begin
  VT.OnExpanded := Value;
end;

procedure TtiVTAbstract.SetOnGetNodeHint(const AValue: TtiVTOnGetHint);
begin
  FOnGetNodeHint := AValue;
  if Assigned(FOnGetNodeHint) then
  begin
    VT.OnGetHint:= DoOnGetHint;
    VT.HintMode:= hmHint;
    VT.ShowHint:= True;
  end
  else begin
    VT.OnGetHint:= nil;
    VT.HintMode:= hmDefault;
    VT.ShowHint:= False;
  end;
end;

procedure TtiVTAbstract.DoOnGetHint(Sender: TBaseVirtualTree;
  ANode: PVirtualNode; Column: TColumnIndex;
  var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: WideString);
var
  LData: TtiObject;
begin
  if Assigned(FOnGetNodeHint) then
  begin
//    hlbDefault,           // Use multi-line style of the node.
//    hlbForceSingleLine,   // Use single line hint.
//    hlbForceMultiLine     // Use multi line hint.
//    ALineBreakStyle:= hlbForceSingleLine;
    LData := GetObjectFromNode(ANode);
    FOnGetNodeHint(Self, ANode, LData, AHintText);
    AHintText := AHintText;
  end;
end;

end.

