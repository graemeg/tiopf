unit tiVTAbstract;

{$I tiDefines.inc}

interface

{$IFDEF VIRTUAL_TREEVIEW}

uses
  VirtualTrees,
  tiVirtualTreesNEW,
  tiFocusPanel,
  tiObject,
  Graphics,
  Classes,
  tiVTSearch;

type

  TtiVTAbstract = class;

  TtiVTOnGetHint = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    var AHintText: UnicodeString) of object;

  TtiVTOnPaintText = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    const ACanvas : TCanvas) of object;

  TtiVTOnAdvancedPaintText = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    const ACanvas : TCanvas;
    const NodeDisabled: boolean;
    const NodeSelectedAndUnfocused: boolean) of object;

  TtiVTGetImageIndexEvent = procedure(
    const AtiVT: TtiVTAbstract;
    const ANode: PVirtualNode;
    const AData: TtiObject;
    const AKind: TVTImageKind;
    const AColumn: TColumnIndex;
    var AGhosted: Boolean;
    var AImageIndex: Integer) of object;

  // ToDo: Gradual migration of TtiVTListView and TtiVTTreeView source that's
  //       duplicated into TtiVTAbstract
  TtiVTAbstract = class(TtiFocusPanel)
  private
    FVT: TtiVirtualStringTree;
    FOnPaintText: TtiVTOnPaintText;
    FOnAdvancedPaintText: TtiVTOnAdvancedPaintText;
    FOnGetNodeHint: TtiVTOnGetHint;
///
    FSP: TtiVTSearchPanel;
    FLastSearchedNode, FLastMatchedNode: PVirtualNode;
    FSPFindNext: TGetNextNodeProc;
    FSPWrapToNode: TGetFirstNodeProc;
    procedure SPFindText(const ASender: TtiVTSearchPanel;
      const AFindText: string; out ATextFound: boolean);
    procedure SPFindTextChange(const ASender: TtiVTSearchPanel;
      const AFindText: string);
    procedure SPFindNext(Sender: TObject);
    procedure SPFindPrevious(Sender: TObject);
    function WrapToBottom: PVirtualNode;
    function WrapToTop: PVirtualNode;
    function GetLastNode: PVirtualNode;
///
    procedure SetOnPaintText(const AValue: TtiVTOnPaintText);
    procedure SetOnAdvancedPaintText(const Value: TtiVTOnAdvancedPaintText);
    procedure DoOnPaintText(ASender: TBaseVirtualTree;
      const ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
      ATextType: TVSTTextType);
    procedure DoOnAdvancedPaintText(ASender: TBaseVirtualTree;
      const ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
      ATextType: TVSTTextType; NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean);
    procedure SetOnGetNodeHint(const AValue: TtiVTOnGetHint);
    procedure DoOnGetHint(Sender: TBaseVirtualTree; ANode: PVirtualNode;
      Column: TColumnIndex; var ALineBreakStyle: TVTTooltipLineBreakStyle;
      var AHintText: UnicodeString);
    function GetScrollEvent: TVTScrollEvent;
    procedure SetScrollEvent(const AValue: TVTScrollEvent);
    function GetOnCollapsed: TVTChangeEvent;
    function GetOnExpanded: TVTChangeEvent;
    procedure SetOnCollapsed(const Value: TVTChangeEvent);
    procedure SetOnExpanded(const Value: TVTChangeEvent);
  protected
///
    FLastNode: PVirtualNode;
///
    procedure SetChildControlNames; override;
    function GetObjectFromNode(Node: PVirtualNode): TtiObject; virtual; abstract;
    procedure SetVT(const AVT: TtiVirtualStringTree);
////
    procedure ClearSearchState;
    procedure SPEnterFindEdit(Sender: TObject); virtual;
    procedure SPExitFindEdit(Sender: TObject); virtual;
    procedure SPEnterKey(Sender: TObject); virtual;
    procedure SPShowing(const ASender: TtiVTSearchPanel; const AIsShowing: Boolean); virtual;
    procedure VTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VTSearchInsideNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      const SearchText: string; out Result: boolean); virtual;
////
    property OnPaintText: TtiVTOnPaintText read FOnPaintText Write SetOnPaintText;
    property OnAdvancedPaintText: TtiVTOnAdvancedPaintText read FOnAdvancedPaintText Write SetOnAdvancedPaintText;
    property OnGetNodeHint: TtiVTOnGetHint read FOnGetNodeHint write SetOnGetNodeHint;
    property OnScroll: TVTScrollEvent read GetScrollEvent write SetScrollEvent;
    property OnCollapsed: TVTChangeEvent read GetOnCollapsed write SetOnCollapsed;
    property OnExpanded: TVTChangeEvent read GetOnExpanded write SetOnExpanded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property VT: TtiVirtualStringTree read FVT;
    property SP: TtiVTSearchPanel read FSP;
  end;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

uses
  SysUtils,
  tiGUIUtils;

{ TtiVTAbstract }

procedure TtiVTAbstract.ClearSearchState;
begin

  if SP.Showing then
  begin

    // hide last matched node
    if Assigned(FLastMatchedNode) and (FLastMatchedNode <> VT.FocusedNode) then
    begin
      FLastMatchedNode.States := FLastMatchedNode.States - [vsSelected];
      VT.InvalidateNode(FLastMatchedNode);
    end;

    FLastMatchedNode := nil;
    FLastSearchedNode := VT.FocusedNode;
  end;

end;

constructor TtiVTAbstract.Create(AOwner: TComponent);
begin
  inherited;
  FSP := TtiVTSearchPanel.Create(Self);
  FSP.Parent := Self;
  FSP.OnFindTextChange := SPFindTextChange;
  FSP.OnShowing := SPShowing;
  FSP.OnFindNext := SPFindNext;
  FSP.OnFindPrevious := SPFindPrevious;
  FSP.OnReturnKey := SPEnterKey;
  FSP.OnEnterFindEdit:= SPEnterFindEdit;
  FSP.OnExitFindEdit:= SPExitFindEdit;
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

procedure TtiVTAbstract.DoOnAdvancedPaintText(ASender: TBaseVirtualTree;
  const ATargetCanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
  ATextType: TVSTTextType; NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean);
var
  LData: TtiObject;
begin
  if Assigned(FOnAdvancedPaintText) then
  begin
    LData := GetObjectFromNode(ANode);
    FOnAdvancedPaintText(Self, ANode, LData, ATargetCanvas, NodeDisabled,
        NodeSelectedAndUnfocused);
  end;
end;

function TtiVTAbstract.GetLastNode: PVirtualNode;
var
 LNode: PVirtualNode;

begin
  Result := VT.RootNode.LastChild;
  LNode := Result;

  while Assigned(Result) do
  begin
    LNode := Result;
    Result := LNode.LastChild;
  end;

  Result := LNode;
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

procedure TtiVTAbstract.SetChildControlNames;
begin
  inherited;
  if Assigned(FSP) then
    FSP.Name := tiGetUniqueComponentNameFromParent(Self, 'SearchPanel');
end;

procedure TtiVTAbstract.SetOnAdvancedPaintText(
  const Value: TtiVTOnAdvancedPaintText);
begin
  FOnAdvancedPaintText := Value;
  if Assigned(FOnAdvancedPaintText) then
    FVT.OnAdvancedPaintText := DoOnAdvancedPaintText
  else
    FVT.OnAdvancedPaintText := nil;
end;

procedure TtiVTAbstract.SetScrollEvent(const AValue: TVTScrollEvent);
begin
  VT.OnScroll := AValue;
end;

procedure TtiVTAbstract.SetVT(const AVT: TtiVirtualStringTree);
begin
  FVT:= AVT;
end;

procedure TtiVTAbstract.SPEnterFindEdit(Sender: TObject);
begin
  // Do nothing. Implement in concrete class
end;

procedure TtiVTAbstract.SPEnterKey(Sender: TObject);
begin
  // Do nothing. Implement in the concrete class
end;

procedure TtiVTAbstract.SPExitFindEdit(Sender: TObject);
begin
  // Do nothing. Implement in concrete class
end;

procedure TtiVTAbstract.SPFindNext(Sender: TObject);
var
  LTextFound: boolean;

begin
  FSPFindNext := VT.GetNext;
  FSPWrapToNode := WrapToTop;

  if Assigned(FSPFindNext(FLastMatchedNode)) then
    FLastSearchedNode := FSPFindNext(FLastMatchedNode)
  else
    FLastSearchedNode := FSPWrapToNode;

  SPFindText(SP, SP.SearchText, LTextFound);
  SP.TextMatching := LTextFound;
end;

procedure TtiVTAbstract.SPFindPrevious(Sender: TObject);
var
  LTextFound: boolean;

begin
  FSPFindNext := VT.GetPrevious;
  FSPWrapToNode := WrapToBottom;

  if Assigned(FSPFindNext(FLastMatchedNode)) then
    FLastSearchedNode := FSPFindNext(FLastMatchedNode)
  else
    FLastSearchedNode := FSPWrapToNode;

  SPFindText(SP, SP.SearchText, LTextFound);
  SP.TextMatching := LTextFound;
end;

procedure TtiVTAbstract.SPFindText(const ASender: TtiVTSearchPanel;
  const AFindText: string; out ATextFound: boolean);
var
  LLength: integer;
  LLapGuardNode: PVirtualNode;

begin
  LLength := Length(AFindText);
  ATextFound := false;

  if LLength > 0 then
  begin
    // we have search text

    // start search from tree root if no prior search
    if not Assigned(FLastSearchedNode) then
      FLastSearchedNode := FSPWrapToNode;

    LLapGuardNode := FLastSearchedNode;

    // search through visible nodes from FLastSearchedNode in direction
    // dictated by FSPFindNext - method point set in caller
    while (not ATextFound) and Assigned(FLastSearchedNode) do
    begin
      VTSearchInsideNode(VT, FLastSearchedNode, AFindText, ATextFound);

      if not ATextFound then
      begin
        FLastSearchedNode.States := FLastSearchedNode.States - [vsSelected];
        FLastSearchedNode := FSPFindNext(FLastSearchedNode);

        if not Assigned(FLastSearchedNode) then
          FLastSearchedNode := FSPWrapToNode;

        if  FLastSearchedNode = LLapGuardNode then
          FLastSearchedNode := nil;

      end
      else
      begin
        // we have a match

        // clean up current matched node
        if Assigned(FLastMatchedNode) then
        begin
          FLastMatchedNode.States := FLastMatchedNode.States - [vsSelected];
          VT.InvalidateNode(FLastMatchedNode);
          // VT.FocusedNode := nil;
        end;

        // update matched node reference
        FLastMatchedNode := FLastSearchedNode;
      end;

    end;

  end
  else
  begin
    // search text is empty

    if Assigned(FLastMatchedNode) then
    begin
      FLastMatchedNode.States := FLastMatchedNode.States - [vsSelected];
      VT.InvalidateNode(FLastMatchedNode);
//      VT.FocusedNode := nil;
    end;

    FLastSearchedNode := VT.GetFirst;
    FLastMatchedNode := nil;
    SP.TextMatching := true;
  end;

  if ATextFound then
  begin
    VT.Selected[FLastSearchedNode] := true;
    VT.FocusedNode := FLastSearchedNode;
  end
  // search failed - set next search start point to last success
  else if Assigned(FLastMatchedNode) then
    FLastSearchedNode := FLastMatchedNode;

end;

procedure TtiVTAbstract.SPFindTextChange(const ASender: TtiVTSearchPanel;
  const AFindText: string);
var
  LTextFound: boolean;

begin
  FSPFindNext := VT.GetNext;
  FSPWrapToNode := WrapToTop;
  SPFindText(ASender, AFindText, LTextFound);
  SP.TextMatching := LTextFound or (Length(AFindText) = 0);
end;

procedure TtiVTAbstract.SPShowing(const ASender: TtiVTSearchPanel;
  const AIsShowing: Boolean);
begin

  if AIsShowing then
  begin

    if Assigned(VT.FocusedNode) then
      VT.FocusedNode.States := VT.FocusedNode.States - [vsSelected];

    VT.FocusedNode := nil;
  end
  else
  begin
    FLastSearchedNode := nil;
    FLastMatchedNode := nil;
  end;

end;

procedure TtiVTAbstract.VTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LTextFound: boolean;

begin

  if SP.Showing then
    begin
      ClearSearchState;

      if Assigned(FLastSearchedNode) then
        VTSearchInsideNode(VT, FLastSearchedNode, SP.SearchText, LTextFound)
      else
        LTextFound := false;

      if LTextFound then
        FLastMatchedNode := FLastSearchedNode;

    end;

//  SP.TextMatching := LTextFound;
end;

procedure TtiVTAbstract.VTSearchInsideNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; out Result: boolean);
 begin
   // Override this in concrete descendants for listview or treeview node context
   Result := false;
 end;

function TtiVTAbstract.WrapToBottom: PVirtualNode;
begin
  Result := GetLastNode;
  SP.Wrapped(false);
end;

function TtiVTAbstract.WrapToTop: PVirtualNode;
begin
  Result := VT.GetFirst;
  SP.Wrapped(true);
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
  var ALineBreakStyle: TVTTooltipLineBreakStyle; var AHintText: UnicodeString);
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

{$ENDIF}

end.

