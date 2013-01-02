unit GUISearchController;

interface

uses
  ComCtrls,
  GUISearchPanel,
  Menus;

type

  TGetFirstNodeProc = function: TTreeNode of object;
  TGetNextNodeProc = function(Node: TTreeNode): TTreeNode of object;

  TGUISearchController = class
  private
    FSP: TGUISearchPanel;
    FVT: TTreeView;
    FLastSearchedNode, FLastMatchedNode: TTreeNode;
    FSPFindNext: TGetNextNodeProc;
    FSPWrapToNode: TGetFirstNodeProc;
    procedure SPFindText(const ASender: TGUISearchPanel;
      const AFindText: string; out ATextFound: boolean);
    procedure SPFindTextChange(const ASender: TGUISearchPanel;
      const AFindText: string);
    procedure SPFindNext(Sender: TObject);
    procedure SPFindPrevious(Sender: TObject);
    function WrapToBottom: TTreeNode;
    function WrapToTop: TTreeNode;
    function GetLastNode: TTreeNode;

    procedure SPEnterFindEdit(Sender: TObject);
    procedure SPExitFindEdit(Sender: TObject);
    procedure VTSearchInsideNode(Sender: TTreeView; Node: TTreeNode;
      const SearchText: string; out Result: boolean); virtual;
    function GetNextTreeNode(Node: TTreeNode): TTreeNode;
    function GetPreviousTreeNode(Node: TTreeNode): TTreeNode;

  public
    constructor Create(const ASearchPanel: TGUISearchPanel;
      const ATreeView: TTreeView);
    destructor Destroy; override;
  property SP: TGUISearchPanel read FSP;
  property VT: TTreeView read FVT;
  end;

implementation

uses
  SysUtils,
  Classes;      // ssCtrl

{ TGUISearchController }

constructor TGUISearchController.Create(const ASearchPanel: TGUISearchPanel;
  const ATreeView: TTreeView);
begin
  FSP := ASearchPanel;
  FVT := ATreeView;
  FSP.OnFindTextChange := SPFindTextChange;
  FSP.OnFindNext := SPFindNext;
  FSP.OnFindPrevious := SPFindPrevious;
  FSP.OnEnterFindEdit:= SPEnterFindEdit;
  FSP.OnExitFindEdit:= SPExitFindEdit;
end;

destructor TGUISearchController.Destroy;
begin

  inherited;
end;

function TGUISearchController.GetLastNode: TTreeNode;
var
 LNode: TTreeNode;
 LCount: integer;

begin
  LNode := nil;
  LCount := VT.Items.Count;

  if LCount > 0 then
  begin
    LNode := VT.Items[LCount - 1];
    Result := LNode.GetLastChild;

    while Assigned(Result) do
    begin
      LNode := Result;
      Result := LNode.GetLastChild;
    end;

  end;

  Result := LNode;
end;

function TGUISearchController.GetNextTreeNode(Node: TTreeNode): TTreeNode;
begin

  if Assigned(Node) then
    Result := Node.GetNext
  else
    Result := Node;

end;

function TGUISearchController.GetPreviousTreeNode(Node: TTreeNode): TTreeNode;
begin

  if Assigned(Node) then
    Result := Node.GetPrev
  else
    Result := Node;

end;

procedure TGUISearchController.SPEnterFindEdit(Sender: TObject);
begin
  // Do nothing
end;

procedure TGUISearchController.SPExitFindEdit(Sender: TObject);
begin
  // Do nothing
end;

procedure TGUISearchController.SPFindNext(Sender: TObject);
var
  LTextFound: boolean;

begin
  FSPFindNext := GetNextTreeNode;
  FSPWrapToNode := WrapToTop;

  if Assigned(FSPFindNext(FLastMatchedNode)) then
    FLastSearchedNode := FSPFindNext(FLastMatchedNode)
  else
    FLastSearchedNode := FSPWrapToNode;

  SPFindText(SP, SP.SearchText, LTextFound);
  SP.TextMatching := LTextFound;
end;

procedure TGUISearchController.SPFindPrevious(Sender: TObject);
var
  LTextFound: boolean;

begin
  FSPFindNext := GetPreviousTreeNode;
  FSPWrapToNode := WrapToBottom;

  if Assigned(FSPFindNext(FLastMatchedNode)) then
    FLastSearchedNode := FSPFindNext(FLastMatchedNode)
  else
    FLastSearchedNode := FSPWrapToNode;

  SPFindText(SP, SP.SearchText, LTextFound);
  SP.TextMatching := LTextFound;
end;

procedure TGUISearchController.SPFindText(const ASender: TGUISearchPanel;
  const AFindText: string; out ATextFound: boolean);
var
  LLength: integer;
  LLapGuardNode: TTreeNode;

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
        FLastSearchedNode.Selected := false;
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
          FLastMatchedNode.Selected := false;

        // update matched node reference
        FLastMatchedNode := FLastSearchedNode;
      end;

    end;

  end
  else
  begin
    // search text is empty

    if Assigned(FLastMatchedNode) then
      FLastMatchedNode.Selected := false;

    FLastSearchedNode := VT.Items.GetFirstNode;
    FLastMatchedNode := nil;
    SP.TextMatching := true;
  end;

  if ATextFound then
    FLastSearchedNode.Selected := true
  // search failed - set next search start point to last success
  else if Assigned(FLastMatchedNode) then
    FLastSearchedNode := FLastMatchedNode;

end;

procedure TGUISearchController.SPFindTextChange(const ASender: TGUISearchPanel;
  const AFindText: string);
var
  LTextFound: boolean;

begin
  FSPFindNext := GetNextTreeNode;
  FSPWrapToNode := WrapToTop;
  SPFindText(ASender, AFindText, LTextFound);
  SP.TextMatching := LTextFound or (Length(AFindText) = 0);
end;

procedure TGUISearchController.VTSearchInsideNode(Sender: TTreeView;
  Node: TTreeNode; const SearchText: string; out Result: boolean);
 var
   LSearchText, LNodeText: string;

 begin
   LSearchText := AnsiUpperCase(SearchText);
   LNodeText := AnsiUpperCase(Node.Text);
   // look for partial match on SearchText as well as complete match
   Result := (AnsiStrPos(PChar(LNodeText), PChar(LSearchText)) <> nil);
 end;

function TGUISearchController.WrapToBottom: TTreeNode;
begin
  Result := GetLastNode;
  SP.Wrapped(false);
end;

function TGUISearchController.WrapToTop: TTreeNode;
begin
  Result := VT.Items.GetFirstNode;
  SP.Wrapped(true);
end;


end.
