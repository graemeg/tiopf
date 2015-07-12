unit tiVirtualListViewMediatorView;

interface

uses
  Classes, Graphics, ImgList, SysUtils, StdCtrls, ComCtrls, VirtualTrees,
  tiBaseMediator, tiObject;



type
  TtiVirtualTreeMediatorFieldInfo = class(TtiMediatorFieldInfo)
  private
    FLayout: TVTHeaderColumnLayout;
    FDefaultSortDirection: TSortDirection;
    FHint: UnicodeString;
    FCaptionAlignment: TAlignment;
    FCheckState: TCheckState;
    FMaxWidth: Integer;
    FColor: TColor;
    FTag: Integer;
    FSpacing: Integer;
    FMinWidth: Integer;
    FCheckType: TCheckType;
    FMargin: Integer;
    FText: UnicodeString;
    FStyle: TVirtualTreeColumnStyle;
    FOptions: TVTColumnOptions;
    FCheckBox: Boolean;
    FPosition: TColumnPosition;
    FImageIndex: TImageIndex;
    FBiDiMode: TBiDiMode;
    procedure SetOptions(const Value: TVTColumnOptions);
  protected
    //GetAsString and SetAsString overrides to allow column declaration in text
    //mode trough AddComposite method
    function GetAsString: string; override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property BiDiMode: TBiDiMode read FBiDiMode write FBiDiMode default bdLeftToRight;
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write FCaptionAlignment default taLeftJustify;
    property CheckType: TCheckType read FCheckType write FCheckType default ctCheckBox;
    property CheckState: TCheckState read FCheckState write FCheckState default csUncheckedNormal;
    property CheckBox: Boolean read FCheckBox write FCheckBox default false;
    property Color: TColor read FColor write FColor default clWindow;
    property DefaultSortDirection: TSortDirection read FDefaultSortDirection
      write FDefaultSortDirection default sdAscending;
    property Hint: UnicodeString read FHint write FHint;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
    property Layout: TVTHeaderColumnLayout read FLayout write FLayout default blGlyphLeft;
    property Margin: Integer read FMargin write FMargin default 4;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 10000;
    property MinWidth: Integer read FMinWidth write FMinWidth default 10;
    property Options: TVTColumnOptions read FOptions write SetOptions default DefaultColumnOptions;
    property Position: TColumnPosition read FPosition write FPosition;
    property Spacing: Integer read FSpacing write FSpacing default 4;
    property Style: TVirtualTreeColumnStyle read FStyle write FStyle;
    property Tag: Integer read FTag write FTag;
    property Text: UnicodeString read FText write FText;
  end;

  TtiVirtualListViewItemMediator = class;

  TtiVirtualListViewMediatorView = class(TtiCustomListMediatorView)
  private
    FSelectedObject: TtiObject;
    FOnInitNode: TVTInitNodeEvent;
    FOnInitChildren: TVTInitChildrenEvent;
  protected
    procedure ClearList; override;
    procedure CreateColumns; override;
    procedure CreateSubMediators; override;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: Integer): TtiListItemMediator; override;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal);
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure GetNodeFromObjectIterateCallBack(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean); virtual;
    function GetSelectedObject: TtiObject; override;
    function MediatorFieldInfoClass: TtiMediatorFieldInfoClass; override;
    procedure RebuildList; override;
    procedure SetActive(const AValue: Boolean); override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure SetupGUIandObject; override;
    procedure VTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure VTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  public
    class function ComponentClass: TClass; override;
    class function CompositeMediator: Boolean; override;

    constructor Create; override;
    constructor CreateCustom(AModel: TtiObjectList; AView: TVirtualStringTree;
      ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    destructor Destroy; override;

    function GetItemMediatorFromNode(const ANode: PVirtualNode): TtiVirtualListViewItemMediator;
    function GetNodeFromObject(const AObject: TtiObject): PVirtualNode;
    function GetObjectFromNode(const ANode: PVirtualNode): TtiObject;
    procedure HandleSelectionChanged; override;
    procedure ItemAdded(const ASubject: TtiObject); virtual;
    procedure ItemDeleted(const ASubject: TtiObject); override;
    procedure SetView(const AValue: TComponent); override;
    procedure Update(ASubject: TtiObject; AOperation : TNotifyOperation); override;
    function View: TVirtualStringTree; reintroduce;

    property OnInitChildren: TVTInitChildrenEvent read FOnInitChildren write FOnInitChildren;
    property OnInitNode: TVTInitNodeEvent read FOnInitNode write FOnInitNode;
  end;

  TtiVirtualListViewItemMediator = class(TtiListItemMediator)
  private
  public
    constructor CreateCustom(AModel: TtiObject; AMediatorView: TtiVirtualListViewMediatorView;
      const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean);

    procedure Update(ASubject: TtiObject); override;
  published
  end;

  PtiVirtualNodeData = ^TtiVirtualNodeData;
  TtiVirtualNodeData = packed record
    AObject: TtiVirtualListViewItemMediator;
  end;



  procedure RegisterVirtualListViewMediator;


implementation



procedure RegisterVirtualListViewMediator;
begin
  gMediatorManager.RegisterMediator(TtiVirtualListViewMediatorView, TtiObjectList);
end;

{ TtiVirtualListViewMeditorView }

procedure TtiVirtualListViewMediatorView.ClearList;
begin
  MediatorList.Clear;

  if View <> nil then
    View.Clear;

  NotifyObserversHelper;
end;

class function TtiVirtualListViewMediatorView.ComponentClass: TClass;
begin
  result := TVirtualStringTree;
end;

class function TtiVirtualListViewMediatorView.CompositeMediator: Boolean;
begin
  result := True;
end;

constructor TtiVirtualListViewMediatorView.Create;
begin
  inherited Create;

  FOnInitChildren := nil;
  FOnInitNode := nil;
  FSelectedObject := nil;
end;

procedure TtiVirtualListViewMediatorView.CreateColumns;
var
  c: Integer;
  lc: TVirtualTreeColumn;
  lInfo: TtiVirtualTreeMediatorFieldInfo;
begin
  If (View.Header.Columns.Count <> FieldsInfo.Count) then
    View.Header.Columns.Clear;

  if View.Header.Columns.Count = 0 then
  begin
    for c := 0 to FieldsInfo.Count - 1 do
    begin
      lInfo := TtiVirtualTreeMediatorFieldInfo(FieldsInfo[c]);

      lc := View.Header.Columns.Add;

      //Options property must be set first
      lc.Options := lInfo.Options;
      lc.Alignment := lInfo.Alignment;
      lc.BiDiMode := lInfo.BiDiMode;
      lc.CaptionAlignment := lInfo.CaptionAlignment;
      lc.CheckBox := lInfo.CheckBox;
      lc.CheckState := lInfo.CheckState;
      lc.CheckType := lInfo.CheckType;
      lc.Color := lInfo.Color;
      lc.DefaultSortDirection := lInfo.DefaultSortDirection;
      lc.Hint := lInfo.Hint;
      lc.ImageIndex := lInfo.ImageIndex;
      lc.Layout := lInfo.Layout;
      lc.Margin := lInfo.Margin;
      lc.MinWidth := lInfo.MinWidth;
      lc.MaxWidth := lInfo.MaxWidth;
      lc.Position := lInfo.Position;
      lc.Spacing := lInfo.Spacing;
      lc.Style := lInfo.Style;
      lc.Text := lInfo.Caption;
      lc.Tag := lInfo.Tag;
      lc.Width := lInfo.FieldWidth;
    end;
  end;
end;

constructor TtiVirtualListViewMediatorView.CreateCustom(AModel: TtiObjectList;
  AView: TVirtualStringTree; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  DisplayNames := ADisplayNames; // Will call ParseDisplaynames.
  ShowDeleted := false;
  Subject := AModel;
  SetView(AView); // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving; // Will attach/Detach
end;

procedure TtiVirtualListViewMediatorView.CreateSubMediators;
var
  I: integer;
  lItemMediator: TtiListItemMediator;
begin
  MediatorList.Clear;

  for I := 0 to Model.Count - 1 do
  begin
    LItemMediator := DoCreateItemMediator(Model.Items[I], I);
    LItemMediator.ListMediator := Self;
  end;
end;

destructor TtiVirtualListViewMediatorView.Destroy;
begin
  IsObserving := False;
  inherited;
end;

function TtiVirtualListViewMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: Integer): TtiListItemMediator;
begin
  DataAndPropertyValid(AData);

  View.BeginUpdate;

  try
    result := TtiVirtualListViewItemMediator.CreateCustom(AData, self, FieldsInfo, Active);
    MediatorList.Add(result);
  finally
    View.EndUpdate;
  end;
end;

procedure TtiVirtualListViewMediatorView.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if Assigned(FOnInitChildren) then
    FOnInitChildren(View, Node, ChildCount);
end;

procedure TtiVirtualListViewMediatorView.DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Assigned(FOnInitNode) then
    FOnInitNode(View, ParentNode, Node, InitialStates);
end;

function TtiVirtualListViewMediatorView.GetItemMediatorFromNode(const ANode: PVirtualNode): TtiVirtualListViewItemMediator;
var
  NodeData: PtiVirtualNodeData;
begin
  result := nil;

  if View = nil then
    exit;

  NodeData := View.GetNodeData(ANode);

  if Assigned(NodeData) then
  begin
    result := NodeData.AObject;
  end;
end;

function TtiVirtualListViewMediatorView.GetNodeFromObject(const AObject: TtiObject): PVirtualNode;
begin
  result := View.IterateSubtree(nil, GetNodeFromObjectIterateCallBack, AObject);
end;

procedure TtiVirtualListViewMediatorView.GetNodeFromObjectIterateCallBack(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var
  NodeData: PtiVirtualNodeData;
begin
  NodeData := Sender.GetNodeData(Node);

  Abort := Data = NodeData.AObject.Model;
end;

function TtiVirtualListViewMediatorView.GetObjectFromNode(const ANode: PVirtualNode): TtiObject;
var
  ItemMediator: TtiVirtualListViewItemMediator;
begin
  result := nil;

  ItemMediator := GetItemMediatorFromNode(ANode);

  if Assigned(ItemMediator) then
    result := ItemMediator.Model;
end;

function TtiVirtualListViewMediatorView.GetSelectedObject: TtiObject;
begin
  result := GetObjectFromNode(View.FocusedNode);
end;

procedure TtiVirtualListViewMediatorView.HandleSelectionChanged;
var
  I: Integer;
  lObserversInTransit: TList;
begin
  if SelectedObject <> FSelectedObject then
  begin
    lObserversInTransit := TList.Create;

    try
      { If an item is already selected, assign the item's List of observers to a
        temporary container. This is done so that the same observers can be
        assigned to the new item. }
      if Assigned(FSelectedObject) then
        for i := FSelectedObject.ObserverList.Count - 1 downto 0 do
          if TObject(FSelectedObject.ObserverList.Items[i])
            is TtiMediatorView then
            lObserversInTransit.Add(FSelectedObject.ObserverList.Extract
              (FSelectedObject.ObserverList.Items[i]));

      FSelectedObject := SelectedObject;

      { If an object was selected, copy the old item's observer List
        to the new item's observer List. }
      // TODO: Only move meditatorview observers
      if Assigned(FSelectedObject) then
      begin
        if lObserversInTransit.Count > 0 then
          for i := 0 to lObserversInTransit.Count - 1 do
            FSelectedObject.ObserverList.Add
              (lObserversInTransit.Extract(lObserversInTransit.Items[i]));

        { Set the Observers Subject property to the selected object }
        // TODO: Only move meditatorview observers
        for i := FSelectedObject.ObserverList.Count - 1 downto 0 do
          if TObject(FSelectedObject.ObserverList.Items[i])
            is TtiMediatorView then
            TtiMediatorView(FSelectedObject.ObserverList.Items[i]).Subject :=
              FSelectedObject;

        // execute the NotifyObservers event to update the observers.
        FSelectedObject.NotifyObservers;
      end;
    finally
      lObserversInTransit.Free;
    end;
  end;
end;

procedure TtiVirtualListViewMediatorView.ItemAdded(const ASubject: TtiObject);
begin
  View.RootNodeCount := Succ(View.RootNodeCount);
end;

procedure TtiVirtualListViewMediatorView.ItemDeleted(const ASubject: TtiObject);
var
  SubjectNode,
  FocusedNode,
  NewSelectedNode: PVirtualNode;
begin
  SubjectNode := GetNodeFromObject(ASubject);
  FocusedNode := View.FocusedNode;

  inherited ItemDeleted(ASubject);

  if Active then
    View.DeleteNode(SubjectNode);
end;

function TtiVirtualListViewMediatorView.MediatorFieldInfoClass: TtiMediatorFieldInfoClass;
begin
  result := TtiVirtualTreeMediatorFieldInfo;
end;

procedure TtiVirtualListViewMediatorView.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. You can always override
    this in your mediators to create a more optimised rebuild. }
  View.BeginUpdate;

  try
    View.Clear;
    CreateColumns;  //Delete columns if necessary
    CreateSubMediators;
    View.RootNodeCount := MediatorList.Count;
  finally
    View.EndUpdate;
    NotifyObserversHelper;
  end;
end;

procedure TtiVirtualListViewMediatorView.SetActive(const AValue: Boolean);
begin
  if AValue = Active then
    Exit; // ==>

  if not AValue then
    ClearList;

  inherited SetActive(AValue);

  if AValue then
    View.RootNodeCount := MediatorList.Count;
end;

procedure TtiVirtualListViewMediatorView.SetSelectedObject(const AValue: TtiObject);
begin
  View.Selected[GetNodeFromObject(AValue)] := true;
end;

procedure TtiVirtualListViewMediatorView.SetupGUIandObject;
begin
  // Can be removed
  View.Header.Columns.Clear;
end;

procedure TtiVirtualListViewMediatorView.SetView(const AValue: TComponent);
begin
  if AValue = View then
    Exit; // ==>

  if View <> nil then
    View.Clear;

  inherited SetView(AValue);

  if (View <> nil) then
  begin
    View.NodeDataSize := SizeOf(TtiVirtualNodeData);
    View.OnInitNode := VTInitNode;
    View.OnInitChildren := VTInitChildren;
  end;

  if Active then
    View.RootNodeCount := MediatorList.Count;
end;

procedure TtiVirtualListViewMediatorView.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
var
  M: TtiListItemMediator;
begin
  // Do not call inherited, it will rebuild the list !!
  Case AOperation of
    noAddItem    : begin
                     M := DoCreateItemMediator(ASubject, Model.Count - 1); // always at the end...
                     M.ListMediator := Self;
                     ItemAdded(M);
                   end;
    noDeleteItem,
    noFree,
    noChanged,
    noReSort     : inherited Update(ASubject, AOperation);
  end;
end;

function TtiVirtualListViewMediatorView.View: TVirtualStringTree;
begin
  result := TVirtualStringTree(inherited View);
end;

procedure TtiVirtualListViewMediatorView.VTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
begin

end;

procedure TtiVirtualListViewMediatorView.VTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  NodeData: PtiVirtualNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  NodeData.AObject := TtiVirtualListViewItemMediator(MediatorList.Items[Node.Index]);

  if NodeData.AObject.Model.Deleted and not ShowDeleted then
    Exclude(Node.States, vsVisible);

  DoInitNode(ParentNode, Node, InitialStates);
end;

{ TtiVirtualListViewItemMediator }

constructor TtiVirtualListViewItemMediator.CreateCustom(AModel: TtiObject;
  AMediatorView: TtiVirtualListViewMediatorView; const AFieldsInfo: TtiMediatorFieldInfoList;
  AIsObserving: Boolean);
begin
  inherited Create;

  Model := AModel;
  FFieldsInfo := AFieldsInfo;
  Active := AIsObserving; // Will attach
end;

procedure TtiVirtualListViewItemMediator.Update(ASubject: TtiObject);
var
  Node: PVirtualNode;
begin
  Assert(Model = ASubject);

  inherited Update(ASubject);

  Node := TtiVirtualListViewMediatorView(ListMediator).GetNodeFromObject(ASubject);

  if Assigned(Node) then
  begin
    TtiVirtualListViewMediatorView(ListMediator).View.IsVisible[Node] := not (ASubject.Deleted and not ListMediator.ShowDeleted);
//    TtiVirtualListViewMediatorView(ListMediator).View.InvalidateNode(Node);
  end;
end;

{ TtiVirtualTreeMediatorFieldInfo }

constructor TtiVirtualTreeMediatorFieldInfo.Create(Collection: TCollection);
begin
  inherited;

  FMinWidth := 10;
  FMaxWidth := 10000;
  FImageIndex := -1;
  FMargin := 4;
  FSpacing := 4;
  FText := '';
  FOptions := DefaultColumnOptions;
  FBiDiMode := bdLeftToRight;
  FColor := clWindow;
  FLayout := blGlyphLeft;
  FCaptionAlignment := taLeftJustify;
  FCheckType := ctCheckBox;
  FCheckState := csUncheckedNormal;
  FCheckBox := False;
  FDefaultSortDirection := sdAscending;
  FPosition := Index;
end;

function TtiVirtualTreeMediatorFieldInfo.GetAsString: string;
begin
  result := inherited;
end;

procedure TtiVirtualTreeMediatorFieldInfo.SetAsString(const AValue: string);
begin
  inherited;
end;

procedure TtiVirtualTreeMediatorFieldInfo.SetOptions(
  const Value: TVTColumnOptions);
begin
  FOptions := Value;
end;


end.
