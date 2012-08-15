unit tiVirtualTreeMediatorView;

interface

uses
  Classes, Graphics, ImgList, SysUtils, StdCtrls, ComCtrls, VirtualTrees,
  tiBaseMediator, tiObject, tiVirtualStringTree;

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
  public
    constructor Create(Collection: TCollection); override;
  published
    property BiDiMode: TBiDiMode read FBiDiMode write FBiDiMode;
    property CaptionAlignment: TAlignment read FCaptionAlignment
      write FCaptionAlignment;
    property CheckType: TCheckType read FCheckType write FCheckType;
    property CheckState: TCheckState read FCheckState write FCheckState;
    property CheckBox: Boolean read FCheckBox write FCheckBox;
    property Color: TColor read FColor write FColor;
    property DefaultSortDirection: TSortDirection read FDefaultSortDirection
      write FDefaultSortDirection;
    property Hint: UnicodeString read FHint write FHint;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
    property Layout: TVTHeaderColumnLayout read FLayout write FLayout;
    property Margin: Integer read FMargin write FMargin;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property Options: TVTColumnOptions read FOptions;
    property Position: TColumnPosition read FPosition write FPosition;
    property Spacing: Integer read FSpacing write FSpacing;
    property Style: TVirtualTreeColumnStyle read FStyle write FStyle;
    property Tag: Integer read FTag write FTag;
    property Text: UnicodeString read FText write FText;
  end;

  TtiVirtualTreeMediatorView = class(TtiCustomListMediatorView)
  private
    FSelectedObject: TtiObject;
  protected
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: Integer)
      : TtiListItemMediator; override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
    procedure SetActive(const AValue: Boolean); override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TtiVirtualStringTree;
      ADisplayNames: string; AIsObserving: Boolean = True);
      reintroduce; overload;
    class function ComponentClass: TClass; override;
    class function CompositeMediator: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function View: TtiVirtualStringTree; reintroduce;
    procedure SetView(const AValue: TComponent); override;
    procedure HandleSelectionChanged; override;
    procedure ItemDeleted(const ASubject: TtiObject); override;
  end;

  TtiVirtualTreeItemMediator = class(TtiListItemMediator)
  private
    FView: TtiVirtualStringTree;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TtiVirtualStringTree;
      const AFieldsInfo: TtiMediatorFieldInfoList;
      AIsObserving: Boolean = True); reintroduce; overload;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TtiVirtualStringTree read FView;
  end;

implementation

{ TtiVirtualTreeListViewMeditor }

procedure TtiVirtualTreeMediatorView.ClearList;
begin
  MediatorList.Clear;
end;

class function TtiVirtualTreeMediatorView.ComponentClass: TClass;
begin
  result := TtiVirtualStringTree;
end;

class function TtiVirtualTreeMediatorView.CompositeMediator: Boolean;
begin
  result := True;
end;

constructor TtiVirtualTreeMediatorView.Create;
begin
  inherited Create;

  FSelectedObject := nil;
end;

procedure TtiVirtualTreeMediatorView.CreateColumns;
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

      lc.Width := lInfo.FieldWidth;
      lc.Text := lInfo.Caption;
      lc.Alignment := lInfo.Alignment;
      lc.BiDiMode := lInfo.BiDiMode;
      lc.CaptionAlignment := lInfo.CaptionAlignment;
      lc.CheckType := lInfo.CheckType;
      lc.CheckState := lInfo.CheckState;
      lc.CheckBox := lInfo.CheckBox;
      lc.Color := lInfo.Color;
      lc.DefaultSortDirection := lInfo.DefaultSortDirection;
      lc.Hint := lInfo.Hint;
      lc.ImageIndex := lInfo.ImageIndex;
      lc.Layout := lInfo.Layout;
      lc.Margin := lInfo.Margin;
      lc.MaxWidth := lInfo.MaxWidth;
      lc.MinWidth := lInfo.MinWidth;
      lc.Options := lInfo.Options;
      lc.Position := lInfo.Position;
      lc.Spacing := lInfo.Spacing;
      lc.Style := lInfo.Style;
      lc.Tag := lInfo.Tag;
    end;
  end;
end;

constructor TtiVirtualTreeMediatorView.CreateCustom(AModel: TtiObjectList;
  AView: TtiVirtualStringTree; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  DisplayNames := ADisplayNames; // Will call ParseDisplaynames.
  Subject := AModel;
  SetView(AView); // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving; // Will attach/Detach
end;

destructor TtiVirtualTreeMediatorView.Destroy;
begin
  IsObserving := False;
  inherited;
end;

function TtiVirtualTreeMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: Integer): TtiListItemMediator;
begin
  DataAndPropertyValid(AData);

  View.BeginUpdate;

  try
    result := TtiVirtualTreeItemMediator.CreateCustom(AData, View,
      FieldsInfo, Active);
    MediatorList.Add(result);

    if Active then
      View.Refresh; // (View.SelectedData);
  finally
    View.EndUpdate;
  end;
end;

function TtiVirtualTreeMediatorView.GetSelectedObject: TtiObject;
begin
  result := View.SelectedData;
end;

procedure TtiVirtualTreeMediatorView.HandleSelectionChanged;
var
  i: Integer;
  LObserversInTransit: TList;
begin
  if SelectedObject <> FSelectedObject then
  begin
    LObserversInTransit := TList.Create;

    try
      { If an item is already selected, assign the item's List of observers to a
        temporary container. This is done so that the same observers can be
        assigned to the new item. }
      if Assigned(FSelectedObject) then
        for i := FSelectedObject.ObserverList.Count - 1 downto 0 do
          if TObject(FSelectedObject.ObserverList.Items[i])
            is TtiMediatorView then
            LObserversInTransit.Add(FSelectedObject.ObserverList.Extract
              (FSelectedObject.ObserverList.Items[i]));

      FSelectedObject := SelectedObject;

      { If an object was selected, copy the old item's observer List
        to the new item's observer List. }
      // TODO: Only move meditatorview observers
      if Assigned(FSelectedObject) then
      begin
        if LObserversInTransit.Count > 0 then
          for i := 0 to LObserversInTransit.Count - 1 do
            FSelectedObject.ObserverList.Add
              (LObserversInTransit.Extract(LObserversInTransit.Items[i]));

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
      LObserversInTransit.Free;
    end;
  end;
end;

procedure TtiVirtualTreeMediatorView.ItemDeleted(const ASubject: TtiObject);
var
  // LIndex: integer;
  SubjectNode: PVirtualNode;
  NewSelectedNode: PVirtualNode;
begin
  SubjectNode := View.GetNodeFromObject(ASubject);

  inherited ItemDeleted(ASubject);

  if Active then
  begin
    if SubjectNode = View.FocusedNode then
      NewSelectedNode := SubjectNode.NextSibling
    else
      NewSelectedNode := View.GetLast(nil);

    View.Refresh;

    View.FocusedNode := NewSelectedNode;
    // LIndex := View.SelectedIndex;
    // View.Refresh;
    // View.SelectedIndex := LIndex; // Next or last item
  end;
end;

procedure TtiVirtualTreeMediatorView.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. You can always override
    this in your mediators to create a more optimised rebuild. }
  View.BeginUpdate;

  try
    CreateColumns;
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

procedure TtiVirtualTreeMediatorView.SetActive(const AValue: Boolean);
begin
  if AValue = Active then
    Exit; // ==>

  if not AValue then
    ClearList;

  if View <> nil then
    if AValue then
      View.Data := Model
    else
      View.Data := nil;

  inherited SetActive(AValue);
end;

procedure TtiVirtualTreeMediatorView.SetSelectedObject(const AValue: TtiObject);
begin
  View.SelectedData := AValue;
end;

procedure TtiVirtualTreeMediatorView.SetupGUIandObject;
begin
  // Can be removed
  View.Header.Columns.Clear;
end;

procedure TtiVirtualTreeMediatorView.SetView(const AValue: TComponent);
begin
  if AValue = View then
    Exit; // ==>

  if View <> nil then
    View.Data := nil;

  inherited SetView(AValue);

  if (View <> nil) and Active then
    View.Data := Model
end;

function TtiVirtualTreeMediatorView.View: TtiVirtualStringTree;
begin
  result := TtiVirtualStringTree( inherited View);
end;

{ TtiVirtualTreeListViewItemMediator }

constructor TtiVirtualTreeItemMediator.CreateCustom(AModel: TtiObject;
  AView: TtiVirtualStringTree; const AFieldsInfo: TtiMediatorFieldInfoList;
  AIsObserving: Boolean);
begin
  inherited Create;

  Model := AModel;
  FView := AView;
  FFieldsInfo := AFieldsInfo;
  Active := AIsObserving; // Will attach
end;

procedure TtiVirtualTreeItemMediator.Update(ASubject: TtiObject);
begin
  Assert(Model = ASubject);

  inherited Update(ASubject);

  if (not ASubject.Deleted) or (ListMediator.ShowDeleted) then
    View.RefreshObject(Model);
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
end;

end.
