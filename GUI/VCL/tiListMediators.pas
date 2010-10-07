{
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.
}
unit tiListMediators;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  ,StdCtrls
  ,ComCtrls
  ,Grids
  ,tiBaseMediator
  ,tiObject
  ,tiVTListView
  ,tiVirtualTrees
  ;

type
  { Composite mediator for TListView }
  TtiListViewMediatorView = class(TtiCustomListMediatorView)
  protected
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator); override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
    procedure SetActive(const AValue: Boolean); override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView; ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView; AOnBeforeSetupField: TtiOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    class function ComponentClass: TClass; override;
    function GetObjectFromItem(AItem: TListItem): TtiObject;
    destructor Destroy; override;
    function  View: TListView; reintroduce;
    procedure HandleSelectionChanged; override;
  end;


  { Composite mediator for TtiVTListView }
  TtiVTListViewMediatorView = class(TtiCustomListMediatorView)
  private
    FSelectedObject: TtiObject;
  protected
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
    procedure SetActive(const AValue: Boolean); override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TtiVTListView; ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    class function ComponentClass: TClass; override;
    class function CompositeMediator: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function  View: TtiVTListView; reintroduce;
    procedure SetView(const AValue: TComponent); override;
    procedure HandleSelectionChanged; override;
    procedure ItemDeleted(const ASubject: TtiObject); override;
  end;


  { Composite mediator for TStringGrid }
  TtiStringGridMediatorView = class(TtiCustomListMediatorView)
  protected
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator); override;
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AGrid: TStringGrid; ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    class function ComponentClass: TClass; override;
    class function CompositeMediator: Boolean; override;
    function GetObjectFromRow(ARow: Integer): TtiObject;
    function  View: TStringGrid; reintroduce;
  end;


  { Used internally for sub-mediators in ListView mediator. Moved to interface
    section so it can be overridden. }
  TtiListViewListItemMediator = class(TtiListItemMediator)
  private
    FView: TListItem;
    procedure SetupFields; virtual;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TListItem; const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean = True); reintroduce; overload;
    constructor CreateCustom(AModel: TtiObject; AView: TListItem; AOnBeforeSetupField: TtiOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean = True); reintroduce; overload;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TListItem read FView;
  end;


  { Used internally for sub-mediators in tiVTListView mediator. Moved to interface
    section so it can be overridden. }
  TtiVTListViewListItemMediator = class(TtiListItemMediator)
  private
    FView: TtiVTListView;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TtiVTListView; const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean = True); reintroduce; overload;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TtiVTListView read FView;
  end;


  { Used internally for sub-mediators in StringGrid mediator. Moved to interface
    section so it can be overridden. }
  TtiStringGridRowMediator = class(TtiListItemMediator)
  private
    FView: TStringGrid;
    FRowIndex: integer;
  public
    constructor CreateCustom(AModel: TtiObject; AGrid: TStringGrid; const AFieldsInfo: TtiMediatorFieldInfoList; ARowIndex: integer; AIsObserving: Boolean = True); reintroduce; overload;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TStringGrid read FView;
    property RowIndex: integer read FRowIndex;
  end;


procedure RegisterFallBackListMediators;

implementation

uses
  tiRTTI
  ;

type
  // friend class to get access to protected methods
  THackStringGrid = class(TStringGrid);


procedure RegisterFallBackListMediators;
begin
  gMediatorManager.RegisterMediator(TtiListViewMediatorView, TtiObjectList);
  gMediatorManager.RegisterMediator(TtiVTListViewMediatorView, TtiObjectList);
  gMediatorManager.RegisterMediator(TtiStringGridMediatorView, TtiObjectList);
end;


{ TtiListViewMediatorView }

function TtiListViewMediatorView.View: TListView;
begin
  result := TListView(inherited View);
end;

procedure TtiListViewMediatorView.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to View.Items.Count - 1 do
    if TtiListItemMediator(View.Items[i].Data).Model = AValue then
    begin
      View.Selected:=View.Items.Item[i];
      //HandleSelectionChanged;
      Exit; //==>
    end;
end;

function TtiListViewMediatorView.GetSelectedObject: TtiObject;
begin
  Result := GetObjectFromItem(View.Selected);
end;

function TtiListViewMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: integer): TtiListItemMediator;
var
  li: TListItem;
begin
  DataAndPropertyValid(AData);
  { Create ListItem and Mediator }
  View.Items.BeginUpdate;
  try
    li:=View.Items.Add;
    result := TtiListViewListItemMediator.CreateCustom(AData, li, OnBeforeSetupField, FieldsInfo, Active);
    li.Data := result;
    MediatorList.Add(result);
  finally
    View.Items.EndUpdate;
  end;
end;

procedure TtiListViewMediatorView.CreateColumns;
var
  c: integer;
  lc: TListColumn;
  lInfo: TtiMediatorFieldInfo;
begin
  If (View.Columns.Count<>FieldsInfo.Count) then
    View.Columns.Clear;
  if View.Columns.Count=0 then
  begin
    for c := 0 to FieldsInfo.Count-1 do
    begin
      lInfo        := FieldsInfo[c];
      lc           := View.Columns.Add;
      lc.AutoSize  := False;
      lc.Caption   := lInfo.Caption;
      lc.Width     := lInfo.FieldWidth;
      lc.Alignment := lInfo.Alignment;
    end;
  end;
end;

procedure TtiListViewMediatorView.SetupGUIandObject;
begin
  { Setup TListView defaults }
  View.Columns.Clear;
  View.Items.Clear;
  View.ViewStyle         := vsReport;
  View.ShowColumnHeaders:=True;
  View.RowSelect         := True;
  View.ReadOnly:=True;
  //  View.AutoSize          := False;
//  View.ScrollBars        := ssAutoBoth;
end;

procedure TtiListViewMediatorView.ClearList;
begin
  MediatorList.Clear;
  If View <> nil then
    View.Items.Clear;
end;

procedure TtiListViewMediatorView.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. You can always override
    this in your mediators to create a more optimised rebuild. }
  View.Items.BeginUpdate;
  try
    CreateColumns;
    CreateSubMediators;
  finally
    View.Items.EndUpdate;
  end;
end;

procedure TtiListViewMediatorView.SetActive(const AValue: Boolean);
begin
  if not AValue then
    ClearList;
  inherited SetActive(AValue);
end;

procedure TtiListViewMediatorView.DoDeleteItemMediator(AIndex: Integer;
  AMediator: TtiListItemMediator);
begin
  View.Items.Delete(TtiListViewListItemMediator(AMediator).FView.Index);
  inherited DoDeleteItemMediator(AIndex, AMediator);
end;

constructor TtiListViewMediatorView.CreateCustom(AModel: TtiObjectList; AView: TListView; AOnBeforeSetupField: TtiOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  OnBeforeSetupField := AOnBeforeSetupField;
  DisplayNames := ADisplayNames;      // Will call ParseDisplaynames.
  Subject := AModel;
  SetView(AView);               // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving;         // Will attach/Detach
end;

class function TtiListViewMediatorView.ComponentClass: TClass;
begin
  Result := TListView;
end;

function TtiListViewMediatorView.GetObjectFromItem(AItem: TListItem): TTiObject;
begin
  if (AItem = nil) or (AItem.Data = nil) then
    Result := nil
  else
    Result := TtiListItemMediator(AItem.Data).Model;
end;

constructor TtiListViewMediatorView.CreateCustom(AModel: TtiObjectList;
    AView: TListView; ADisplayNames: string; AIsObserving: Boolean);
var
  p: TtiOnBeforeSetupField;
begin
  p := nil; // For some reason Delphi doesn't allow nil directly.
  CreateCustom(AModel, AView, p, ADisplayNames, AIsObserving);
end;

destructor TtiListViewMediatorView.Destroy;
begin
  IsObserving := False;
  inherited;
end;

procedure TtiListViewMediatorView.HandleSelectionChanged;
var
  i: integer;
  LObserversInTransit: TList;
begin
  if View.Selected = nil then
    SelectedObject := nil
  else
  begin
    LObserversInTransit := TList.Create;
    try
      { If an item is already selected, assign the item's List of observers to a
        temporary container. This is done so that the same observers can be
        assigned to the new item. }
      if Assigned(SelectedObject) then
        LObserversInTransit.Assign(SelectedObject.ObserverList);

      // Assign Newly selected item to SelectedObject Obj.
      SelectedObject := TtiObject(View.Selected.Data);

      { If an object was selected, copy the old item's observer List
        to the new item's observer List. }
      if LObserversInTransit.Count > 0 then
        SelectedObject.ObserverList.Assign(LObserversInTransit);

      { Set the Observers Subject property to the selected object }
      for i := 0 to SelectedObject.ObserverList.Count - 1 do
        TtiMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;

      // execute the NotifyObservers event to update the observers.
      SelectedObject.NotifyObservers;
    finally
      LObserversInTransit.Free;
    end;
  end;
end;

{ TtiListViewListItemMediator }

procedure TtiListViewListItemMediator.SetupFields;
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  lMemberName := FFieldsInfo[0].PropName;
  lValue      := tiGetPropertyCoalesce(Model, lMemberName);
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(Model, lMemberName, lValue);
  FView.Caption := lValue;
  for c := 1 to FFieldsInfo.Count - 1 do
  begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue      := tiGetPropertyCoalesce(Model, lMemberName);
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lMemberName, lValue);
    FView.SubItems.Add(lValue);
  end;
end;

constructor TtiListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TListItem; const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean);
var
  p: TtiOnBeforeSetupField;
begin
  p := nil;
  CreateCustom(AModel, AView, p, AFieldsInfo, AIsObserving);
end;

constructor TtiListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TListItem; AOnBeforeSetupField: TtiOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; AIsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AView;
  FFieldsInfo := AFieldsInfo;
  OnBeforeSetupField := AOnBeforeSetupField;
  SetupFields;
  Active      := AIsObserving; // Will attach
end;

procedure TtiListViewListItemMediator.Update(ASubject: TtiObject);
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  Assert(Model = ASubject);
  for c := 0 to FFieldsInfo.Count - 1 do
  begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue      := tiGetPropertyCoalesce(Model, lMemberName);
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lMemberName, lValue);
    If c=0 Then
      FView.Caption := lValue
    else
      FView.SubItems[c - 1] := lValue;
  end;
end;


{ TtiVTListViewMediatorView }

constructor TtiVTListViewMediatorView.Create;
begin
  inherited Create;
  FSelectedObject := nil;
end;

constructor TtiVTListViewMediatorView.CreateCustom(AModel: TtiObjectList;
  AView: TtiVTListView; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  DisplayNames := ADisplayNames;      // Will call ParseDisplaynames.
  Subject := AModel;
  SetView(AView);               // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving;         // Will attach/Detach
end;

destructor TtiVTListViewMediatorView.Destroy;
begin
  IsObserving := False;
  inherited;
end;

procedure TtiVTListViewMediatorView.ClearList;
begin
  MediatorList.Clear;
end;

class function TtiVTListViewMediatorView.ComponentClass: TClass;
begin
  result := TtiVTListView;
end;

class function TtiVTListViewMediatorView.CompositeMediator: Boolean;
begin
  result := true;
end;

procedure TtiVTListViewMediatorView.CreateColumns;
var
  c: integer;
  lc: TtiVTColumn;
  lInfo: TtiMediatorFieldInfo;
begin
  If (View.Header.Columns.Count<>FieldsInfo.Count) then
    View.Header.Columns.Clear;
  if View.Header.Columns.Count=0 then
  begin
    for c := 0 to FieldsInfo.Count-1 do
    begin
      lInfo := FieldsInfo[c];
      lc := View.AddColumn(lInfo.PropName, vttkString, lInfo.Caption, lInfo.FieldWidth);
      lc.Alignment := lInfo.Alignment;
    end;
  end;
end;

function TtiVTListViewMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: integer): TtiListItemMediator;
begin
  DataAndPropertyValid(AData);

  View.BeginUpdate;
  try
    result := TtiVTListViewListItemMediator.CreateCustom(AData, View,
        FieldsInfo, Active);
    MediatorList.Add(result);
    if Active then
      View.Refresh(View.SelectedData);
  finally
    View.EndUpdate;
  end;
end;

procedure TtiVTListViewMediatorView.ItemDeleted(const ASubject: TtiObject);
var
  LIndex: integer;
begin
  inherited ItemDeleted(ASubject);
  if Active then
  begin
    LIndex := View.SelectedIndex;
    View.Refresh;
    View.SelectedIndex := LIndex; // Next or last item
  end;
end;

function TtiVTListViewMediatorView.View: TtiVTListView;
begin
  result := TtiVTListView(inherited View);
end;

function TtiVTListViewMediatorView.GetSelectedObject: TtiObject;
begin
  result := View.SelectedData;
end;

procedure TtiVTListViewMediatorView.HandleSelectionChanged;
var
  i: integer;
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
          if TObject(FSelectedObject.ObserverList.Items[i]) is TtiMediatorView then
            LObserversInTransit.Add(FSelectedObject.ObserverList.Extract(FSelectedObject.ObserverList.Items[i]));

      FSelectedObject := SelectedObject;

      { If an object was selected, copy the old item's observer List
        to the new item's observer List. }
      //TODO: Only move meditatorview observers
      if Assigned(FSelectedObject) then
      begin
        if LObserversInTransit.Count > 0 then
          for i := 0 to LObserversInTransit.Count - 1 do
            FSelectedObject.ObserverList.Add(LObserversInTransit.Extract(LObserversInTransit.Items[i]));

        { Set the Observers Subject property to the selected object }
        //TODO: Only move meditatorview observers
        for i := FSelectedObject.ObserverList.Count - 1 downto 0 do
          if TObject(FSelectedObject.ObserverList.Items[i]) is TtiMediatorView then
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

procedure TtiVTListViewMediatorView.RebuildList;
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

procedure TtiVTListViewMediatorView.SetActive(const AValue: Boolean);
begin
  if AValue = Active then
    Exit; //==>

  if not AValue then
    ClearList;

  if View <> nil then
    if AValue then
      View.Data := Model
    else
      View.Data := nil;

  inherited SetActive(AValue);
end;

procedure TtiVTListViewMediatorView.SetView(const AValue: TComponent);
begin
  if AValue = View then
    Exit; //==>

  if View <> nil then
    View.Data := nil;

  inherited SetView(AValue);

  if (View <> nil) and Active then
    View.Data := Model
end;

procedure TtiVTListViewMediatorView.SetSelectedObject(const AValue: TtiObject);
begin
  View.SelectedData := AValue;
end;

procedure TtiVTListViewMediatorView.SetupGUIandObject;
begin
  { Setup TtiVTListView defaults }
  View.Header.Columns.Clear;
  View.RowSelect := true;
end;


{ TtiVTListViewListItemMediator }

constructor TtiVTListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TtiVTListView; const AFieldsInfo: TtiMediatorFieldInfoList;
  AIsObserving: Boolean);
begin
  inherited Create;
  Model := AModel;
  FView := AView;
  FFieldsInfo := AFieldsInfo;
  Active := AIsObserving; // Will attach
end;

procedure TtiVTListViewListItemMediator.Update(ASubject: TtiObject);
begin
  Assert(Model = ASubject);
  inherited Update(ASubject);
  if (not ASubject.Deleted) or (ListMediator.ShowDeleted) then
    View.RefreshObject(Model);
end;


{ TtiStringGridMediatorView }

function TtiStringGridMediatorView.GetSelectedObject: TtiObject;

begin
  Result := GetObjectFromRow(View.Row);
end;

procedure TtiStringGridMediatorView.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
  o : TObject;
begin
  for i := 0 to View.RowCount - 1 do
    begin
    O := View.Objects[0, i];
    if assigned (O) and (TtiListItemMediator(O).Model = AValue) then
    begin
      View.Row := i;
      Exit; //==>
    end;
    end;
end;

function TtiStringGridMediatorView.View: TStringGrid;
begin
  result := TStringGrid(inherited View);
end;

function TtiStringGridMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: integer): TtiListItemMediator;
var
  i: integer;
  lFieldName: string;
begin
// graeme
//  View.BeginUpdate;
  try
    if ARowIdx+1 = View.RowCount then // In case of add notification
      View.RowCount:=View.RowCount+1;
    for i := 0 to FieldsInfo.Count - 1 do
    begin
      lFieldName := FieldsInfo[i].PropName;
      View.Cells[i, ARowIdx+1] := tiGetPropertyCoalesce(AData, lFieldName);  // set Cell text
    end;
    result := TtiStringGridRowMediator.CreateCustom(AData, View, FieldsInfo, ARowIdx+1, Active);
    View.Objects[0, ARowIdx+1] := result;   // set Object reference inside grid
    MediatorList.Add(result);
  finally
// graeme
//    View.EndUpdate;
  end;
end;

procedure TtiStringGridMediatorView.DoDeleteItemMediator(AIndex: Integer;
  AMediator: TtiListItemMediator);
begin
  THackStringGrid(View).DeleteRow(AIndex+1);
  Inherited DoDeleteItemMediator(AIndex,AMediator);
end;

procedure TtiStringGridMediatorView.CreateColumns;
var
  i: integer;
  lColumnTotalWidth: integer;
  lGridNonContentWidth: integer;
  lLastColumnWidth: integer;
begin
  lColumnTotalWidth := 0;
  View.ColCount := 0;  // clear columns
  View.ColCount := FieldsInfo.Count;
  // Grid is 2 pixel border left + right, 1 pixel col gridline separator
  lGridNonContentWidth := 2 + 2 + (FieldsInfo.Count - 1);
  for i := 0 to FieldsInfo.Count - 1 do
  begin
    View.ColWidths[i] := FieldsInfo[i].FieldWidth;
    View.Cols[i].Text  := FieldsInfo[i].Caption;
// graeme
//    C.Alignment := FieldsInfo[i].Alignment;
    //resize the last column to fill the grid.
    if i = FieldsInfo.Count - 1 then
      begin
      if View.Width > (lColumnTotalWidth + lGridNonContentWidth) then
      begin
        lLastColumnWidth := View.Width - (lColumnTotalWidth + lGridNonContentWidth);
        if lLastColumnWidth > 10 then
          View.ColWidths[i] := lLastColumnWidth;
      end;
    end
    else
      lColumnTotalWidth := lColumnTotalWidth + View.ColWidths[i];
  end;
  if ShowDeleted then
    View.RowCount := Model.Count+1
  else
    View.RowCount := Model.CountNotDeleted+1;
end;

procedure TtiStringGridMediatorView.SetupGUIandObject;

begin
  //Setup default properties for the StringGrid
  View.Options:=View.Options+[goRowSelect];
  // Rowcount is set after columns are created, because clearing columns
  //  resets rowcount.
end;

procedure TtiStringGridMediatorView.ClearList;
begin
  MediatorList.Clear;
  if View <> nil then
    View.RowCount:=1;
end;

procedure TtiStringGridMediatorView.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. }
// graeme
//  View.BeginUpdate;
  try
    SetupGUIandObject;
    MediatorList.Clear;
    CreateSubMediators;
  finally
// graeme  
//    View.EndUpdate;
  end;
end;

constructor TtiStringGridMediatorView.CreateCustom(AModel: TtiObjectList; AGrid: TStringGrid; ADisplayNames: string; AIsObserving: Boolean);
begin
  inherited Create;
  DisplayNames := ADisplayNames;
  Subject := AModel;
  SetView(AGrid);
  CreateSubMediators;
  IsObserving  := AIsObserving;
end;

class function TtiStringGridMediatorView.ComponentClass: TClass;
begin
  Result := TStringGrid;
end;

class function TtiStringGridMediatorView.CompositeMediator: Boolean;
begin
  Result:=true;
end;

function TtiStringGridMediatorView.GetObjectFromRow(ARow: Integer): TTiObject;

var
  O : TObject;

begin
  if View.RowCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  if ARow = -1 then
    Result := nil
  else
    begin
    O:=View.Objects[0, ARow];
    If O<>Nil then
      Result := TtiListItemMediator(O).Model
    else
      Result:=Nil;
    end;
end;


{ TtiStringGridRowMediator }

constructor TtiStringGridRowMediator.CreateCustom(AModel: TtiObject; AGrid: TStringGrid; const AFieldsInfo: TtiMediatorFieldInfoList; ARowIndex: integer; AIsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AGrid;
  FFieldsInfo := AFieldsInfo;
  FRowIndex   := ARowIndex;
  Active      := AIsObserving; // Will attach
end;

procedure TtiStringGridRowMediator.Update(ASubject: TtiObject);
var
  i: integer;
  lvalue, lFieldName: string;
begin
  Assert(Model = ASubject);
  for i := 0 to FFieldsInfo.Count - 1 do
  begin
    lFieldName := FFieldsInfo[I].PropName;
    lValue     := tiGetPropertyCoalesce(Model, lFieldName);
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lFieldName, lValue);
    FView.Cells[i, FRowIndex] := lValue;
  end;
end;


end.
