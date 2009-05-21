{
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.
}
unit tiCompositeMediators;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  ,tiObject
  ,ComCtrls   { TListView }
  ,Contnrs    { TObjectList }
  ,Grids      { TStringGrid }
  ;


type
  TtiMediatorFieldInfo = class
  private
    FWidth: integer;
    FCaption: string;
    FPropName: string;
    FAlign: TAlignment;
  public
    property Caption : string read FCaption write FCaption;
    property PropName : string read FPropName write FPropName;
    property FieldWidth : integer read FWidth write FWidth;
    property Alignment : TAlignment read FAlign write FAlign default taLeftJustify;
  end;

  { Event object used for OnBeforeSetupField event. Is used to allow formatting
    of fields before written to listview Caption or Items. }
  TOnBeforeSetupField = procedure(AObject: TtiObject;
    const AFieldName: string; var AValue: string) of object;

  { Composite mediator for TListView }
  TCompositeListViewMediator = class(TtiObject)
  private
    FOnBeforeSetupField: TOnBeforeSetupField;
    function    GetSelectedObject: TtiObject;
    procedure   SetSelectedObject(const AValue: TtiObject);
    procedure   SetShowDeleted(const AValue: Boolean);
    procedure   DoCreateItemMediator(const AData: TtiObject);
    procedure   SetOnBeforeSetupField(const Value: TOnBeforeSetupField);
  protected
    FIsObserving: Boolean;
    FDisplayNames: string;
    FShowDeleted: Boolean;
    FView: TListView;
    FModel: TtiObjectList;
    FMediatorList: TObjectList;
    FObserversInTransit: TList;
    FSelectedObject: TtiObject;
    FFieldsInfo : TStringList;
    procedure ParseDisplayNames;
    procedure CleanUpFieldInfos;
    procedure   CreateSubMediators; virtual;
    procedure   SetupGUIandObject; virtual;
    procedure   RebuildList; virtual;
    function    DataAndPropertyValid(const AData: TtiObject): Boolean;
  public
    constructor Create; override;
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView; ADisplayNames: string; IsObserving: Boolean = True); overload;
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView;
      AOnBeforeSetupField: TOnBeforeSetupField; ADisplayNames: string; IsObserving: Boolean = True); overload;
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
    { Called from the GUI to trigger events }
    procedure   HandleSelectionChanged; virtual;
    { Event handler to allow formatting of fields before they are written. }
    property    OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write SetOnBeforeSetupField;
  published
    property    View: TListView read FView;
    property    Model: TtiObjectList read FModel;
    property    DisplayNames: string read FDisplayNames;
    property    IsObserving: Boolean read FIsObserving;
    property    SelectedObject: TtiObject read GetSelectedObject write SetSelectedObject;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
  end;


  { Composite mediator for TStringGrid }
  TCompositeStringGridMediator = class(TtiObject)
  private
    function    GetSelectedObjected: TtiObject;
    procedure   SetSelectedObject(const AValue: TtiObject);
    procedure   SetShowDeleted(const AValue: Boolean);
    procedure   DoCreateItemMediator(AData: TtiObject); overload;
    procedure   DoCreateItemMediator(AData: TtiObject; pRowIdx : Integer); overload;
  protected
    FDisplayNames: string;
    FIsObserving: boolean;
    FShowDeleted: Boolean;
    FView: TStringGrid;
    FModel: TtiObjectList;
    FMediatorList: TObjectList;
    procedure   CreateSubMediators; virtual;
    procedure   SetupGUIandObject; virtual;
    procedure   RebuildStringGrid; virtual;
    function    DataAndPropertyValid(const AData: TtiObject): Boolean;
  public
    constructor CreateCustom(AModel: TtiObjectList; AGrid : TStringGrid; ADisplayNames : string; IsObserving: Boolean = True);
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    View: TStringGrid read FView;
    property    Model: TtiObjectList read FModel;
    property    DisplayNames: string read FDisplayNames;
    property    IsObserving: boolean read FIsObserving;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
    property    SelectedObject: TtiObject read GetSelectedObjected write SetSelectedObject;
  end;


  { Used internally for sub-mediators in ListView mediator. Moved to interface
    section so it can be overridden. }
  TListViewListItemMediator = class(TtiObject)
  private
    FOnBeforeSetupField: TOnBeforeSetupField;
    procedure SetOnBeforeSetupField(const Value: TOnBeforeSetupField);
  protected
    FModel: TtiObject;
    FView: TListItem;
    FDisplayNames: string;
    procedure   SetupFields; virtual;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TListItem; const ADisplayNames: string; IsObserving: Boolean = True); overload;
    constructor CreateCustom(AModel: TtiObject; AView: TListItem;
      AOnBeforeSetupField: TOnBeforeSetupField; const ADisplayNames: string; IsObserving: Boolean = True); overload;
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
    { Event handler to allow formatting of fields before they are written. }
    property    OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write SetOnBeforeSetupField;
  published
    property    View: TListItem read FView;
    property    Model: TtiObject read FModel;
    property    DisplayNames: string read FDisplayNames;
  end;


  { Used internally for sub-mediators in StringGrid mediator. Moved to interface
    section so it can be overridden. }
  TStringGridRowMediator = class(TtiObject)
  private
    FDisplayNames: string;
    FView: TStringGrid;
    FModel: TtiObject;
    FRowIndex : Integer;
  protected
//    procedure   SetupFields;
  public
    constructor CreateCustom(AModel: TtiObject; AGrid: TStringGrid; ADisplayNames: string; pRowIndex: integer; IsObserving: Boolean = True);
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    Model: TtiObject read FModel;
    property    View: TStringGrid read FView;
    property    DisplayNames: string read FDisplayNames;
  end;


function tiFieldName(const AField: string): string;
function tiFieldWidth(const AField: string): integer;
function tiFieldCaption(const AField: string): string;
function tiFieldAlignment(const AField : string) : TAlignment;

implementation

uses
  tiUtils
  ,StdCtrls
  ,typinfo
  ,tiExcept
//  ,tiGenericEditMediators
  ;

const
  cFieldDelimiter = ';';
  cBrackets = '()';
  


{ Helper functions }

{ Extract the field name part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Quantity
  Width and Field Caption is optional }
function tiFieldName(const AField: string): string;
begin
  Result := tiToken(AField, cBrackets[1], 1);
end;

{ Extract the width part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")  will return: 25
  Width and Field Caption is optional }
function tiFieldWidth(const AField: string): integer;
var
  s: string;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) = '' then
    Result := 75  // default width
  else
    Result := StrToInt(tiToken(s, ',', 1));
end;

{ Extracts the alignment from the AField string which is in the format
 fieldname(width,"field caption",a)  where a is the alignment character.
 Legal values for the alignment character are :-
 < - left aligned
 > - right aligned
 | - centre aligned
  eg:  Quantity(25,"Qty",>)   will return: Qty with a width of 25 and
 the column will be right-aligned. Width, Field Caption and alignment
 are optional }
function tiFieldAlignment(const AField : string) : TAlignment;
var
  s, a : string;
  lAlignChar : Char;
begin
  result := taLeftJustify;
  s := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) <> '' then
  begin
    a := tiToken(s, ',', 3);
    if a <> '' then
    begin
      lAlignChar := a[1];
      Case lAlignChar Of
        '<' : Result := taLeftJustify;
        '>' : Result := taRightJustify;
        '|' : Result := taCenter;
      end; { case }
    end;
  end;
end;

{ Extract the field caption part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Qty
  Width and Field Caption is optional }
function tiFieldCaption(const AField: string): string;
var
  s: string;
  p: pchar;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2]);
  if (trim(s) = '') or (Pos(',', s) = 0) then
    // It's only got a width or is blank, so we default to field name
    Result := tiFieldName(AField)
  else
  begin
    s := tiToken(s, ',', 2);
    p := PChar(s);
    Result := AnsiExtractQuotedStr(p, '"');
  end;
end;


{ TStringGridRowMediator }

//procedure TStringGridRowMediator.SetupFields;
//begin
//  {$ifdef fpc} {$Note Add the appropriate code here} {$endif}
//end;

constructor TStringGridRowMediator.CreateCustom(AModel: TtiObject; AGrid : TStringGrid; ADisplayNames: string; pRowIndex : integer; IsObserving: Boolean);
begin
  inherited Create;
  FModel        := AModel;
  FView         := AGrid;
  FDisplayNames := ADisplayNames;
  FRowIndex     := pRowIndex;

  if IsObserving then
    FModel.AttachObserver(self);
end;

procedure TStringGridRowMediator.BeforeDestruction;
begin
  FModel.DetachObserver(self);
  FModel := nil;
  inherited BeforeDestruction;
end;

procedure TStringGridRowMediator.Update(ASubject: TtiObject);
var
  i: Integer;
  lField: string;
  lFieldName: string;
begin
  Assert(FModel = ASubject);
  for i := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, i);
    lFieldName := tiFieldName(lField);
    
    FView.Cells[i, FRowIndex] := FModel.PropValue[lFieldName];
  end;
end;


{ TListViewListItemMediator }

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TListItem; AOnBeforeSetupField: TOnBeforeSetupField;
  const ADisplayNames: string; IsObserving: Boolean);
begin
  inherited Create;
  FModel              := AModel;
  FView               := AView;
  FDisplayNames       := ADisplayNames;
  FOnBeforeSetupField := AOnBeforeSetupField;

  SetupFields;

  if IsObserving then
    FModel.AttachObserver(self);
end;

procedure TListViewListItemMediator.SetOnBeforeSetupField(
  const Value: TOnBeforeSetupField);
begin
  FOnBeforeSetupField := Value;
end;

procedure TListViewListItemMediator.SetupFields;
var
  c: integer;
  lField: string;
  lMemberName: string;
  lValue: string;
begin
  lField := tiToken(FDisplayNames, cFieldDelimiter, 1);
  lMemberName := tiFieldName(lField);
  lValue := FModel.PropValue[lMemberName];

  if Assigned(FOnBeforeSetupField) then
    FOnBeforeSetupField(FModel, lMemberName, lValue);

  FView.Caption := lValue;

  for c := 2 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, c);
    lMemberName := tiFieldName(lField);
    lValue := FModel.PropValue[lMemberName];

    if Assigned(FOnBeforeSetupField) then
      FOnBeforeSetupField(FModel, lMemberName, lValue);

    FView.SubItems.Add(lValue);
  end;
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TListItem; const ADisplayNames: string; IsObserving: Boolean);
begin
  inherited Create;
  FModel              := AModel;
  FView               := AView;
  FDisplayNames       := ADisplayNames;

  SetupFields;

  if IsObserving then
    FModel.AttachObserver(self);
end;

procedure TListViewListItemMediator.BeforeDestruction;
begin
  FModel.DetachObserver(self);
  FModel  := nil;
  FView   := nil;
  inherited BeforeDestruction;
end;

procedure TListViewListItemMediator.Update(ASubject: TtiObject);
var
  c: integer;
  lField: string;
  lMemberName: string;
  lValue: string;
begin
  Assert(FModel = ASubject);

  lField := tiToken(DisplayNames, cFieldDelimiter, 1);
  lMemberName := tiFieldName(lField);
  lValue := FModel.PropValue[lMemberName];

  if Assigned(FOnBeforeSetupField) then
    FOnBeforeSetupField(FModel, lMemberName, lValue);

  FView.Caption := lValue;

  for c := 2 to tiNumToken(DisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(DisplayNames, cFieldDelimiter, c);
    lMemberName := tiFieldName(lField);
    lValue := FModel.PropValue[lMemberName];

    if Assigned(FOnBeforeSetupField) then
      FOnBeforeSetupField(FModel, lMemberName, lValue);
    FView.SubItems[c-2] := lValue;
  end;
end;

{ TCompositeListViewMediator }

procedure TCompositeListViewMediator.SetOnBeforeSetupField(
  const Value: TOnBeforeSetupField);
begin
  FOnBeforeSetupField := Value;
end;

procedure TCompositeListViewMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to FView.Items.Count-1 do
  begin
    if TtiObject(FView.Items[i].Data) = AValue then
    begin
      FView.Selected := FView.Items[i];
      HandleSelectionChanged;
      Exit; //==>
    end;
  end;
end;

function TCompositeListViewMediator.GetSelectedObject: TtiObject;
begin
  if FView.SelCount = 0 then
    FSelectedObject := nil
  else
    FSelectedObject := TtiObject(FView.Selected.Data);
  result := FSelectedObject;
end;

procedure TCompositeListViewMediator.SetShowDeleted(const AValue: Boolean);
begin
  if FShowDeleted = AValue then
    Exit; //==>
    
  BeginUpdate;
  try
    FShowDeleted := AValue;
    RebuildList;
  finally
    EndUpdate;
  end;
end;

procedure TCompositeListViewMediator.DoCreateItemMediator(const AData: TtiObject);
var
  li: TListItem;
  m: TListViewListItemMediator;
begin
  DataAndPropertyValid(AData);
  
  { Create ListItem and Mediator }
  li        := FView.Items.Add;
  li.Data   := AData;
  m         := TListViewListItemMediator.CreateCustom(AData, li, FOnBeforeSetupField, FDisplayNames, FIsObserving);

  FMediatorList.Add(m);
end;

procedure TCompositeListViewMediator.CreateSubMediators;
var
  C: integer;
  lc: TListColumn;
  lInfo : TtiMediatorFieldInfo;
begin
  if View.Columns.Count = 0 then
  begin
    { Create column headers }
    for C := 0 to Pred(FFieldsInfo.Count) do
    begin
      lInfo := TtiMediatorFieldInfo(FFieldsInfo.Objects[C]);
      lc            := FView.Columns.Add;
      lc.AutoSize   := False;
      lc.Caption    := lInfo.Caption;
      lc.Width      := lInfo.FieldWidth;
      lc.Alignment  := lInfo.Alignment;
    end;
  end;
  FModel.ForEach(DoCreateItemMediator, FShowDeleted);
end;

procedure TCompositeListViewMediator.SetupGUIandObject;
begin
  { Setup TListView defaults }
  FView.Columns.Clear;
  FView.Items.Clear;
  FView.ViewStyle         := vsReport;
  FView.ShowColumnHeaders := True;
  FView.RowSelect         := True;
  {$IFDEF FPC}
  FView.AutoSize          := False;
  FView.ScrollBars        := ssAutoBoth;
  {$ENDIF}
end;

procedure TCompositeListViewMediator.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. You can always override
    this in your mediators to create a more optimised rebuild. }
  {$ifdef fpc}
  View.BeginUpdate;
  {$else}
  View.Items.BeginUpdate;
  {$endif}
  try
    FMediatorList.Clear;
    if View.Columns[0].Caption = '' then
      View.Columns.Clear;
    View.Items.Clear;
    CreateSubMediators;
  finally
  {$ifdef fpc}
  View.EndUpdate;
  {$else}
  View.Items.EndUpdate;
  {$endif}
  end;
end;

function TCompositeListViewMediator.DataAndPropertyValid(const AData: TtiObject): Boolean;
var
  c: integer;
  lField: string;
begin
  result := (FModel <> nil) and (FDisplayNames <> '');
  if not result then
    Exit; //==>

  for c := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, c);
    { WRONG!!  We should test the items of the Model }
    result := (IsPublishedProp(AData, tiFieldName(lField)));
    if not result then
      raise Exception.CreateFmt('<%s> is not a property of <%s>',
                               [tiFieldName(lField), AData.ClassName ]);
  end;
end;

constructor TCompositeListViewMediator.Create;
begin
  inherited Create;
  FObserversInTransit := TList.Create;
  FFieldsInfo := TStringList.Create;
end;

constructor TCompositeListViewMediator.CreateCustom(AModel: TtiObjectList;
  AView: TListView; AOnBeforeSetupField: TOnBeforeSetupField;
  ADisplayNames: string; IsObserving: Boolean);
begin
  Create; // don't forget this

  FModel        := AModel;
  FView         := AView;
  FMediatorList := TObjectList.Create;
  FIsObserving  := IsObserving;
  FDisplayNames := ADisplayNames;
  FShowDeleted  := False;

  Assert(Assigned(AOnBeforeSetupField), 'OnBeforeSetupField not assigned');
  FOnBeforeSetupField := AOnBeforeSetupField;

  SetupGUIandObject;

  { TODO: This must be improved. If no ADisplayNames value maybe default to a
   single column listview using the Caption property }
  if (ADisplayNames <> '') and (tiNumToken(ADisplayNames, cFieldDelimiter) > 0) then
  begin
    ParseDisplayNames;
    CreateSubMediators;
  end;

  if IsObserving then
    FModel.AttachObserver(self);

end;

constructor TCompositeListViewMediator.CreateCustom(AModel: TtiObjectList;
  AView: TListView; ADisplayNames: string; IsObserving: Boolean);
begin
  Create; // don't forget this

  FModel        := AModel;
  FView         := AView;
  FMediatorList := TObjectList.Create;
  FIsObserving  := IsObserving;
  FDisplayNames := ADisplayNames;
  FShowDeleted  := False;

  SetupGUIandObject;

  { TODO: This must be improved. If no ADisplayNames value maybe default to a
   single column listview using the Caption property }
  if (ADisplayNames <> '') and (tiNumToken(ADisplayNames, cFieldDelimiter) > 0) then
  begin
    ParseDisplayNames;
    CreateSubMediators;
  end;

  if IsObserving then
    FModel.AttachObserver(self);
end;

procedure TCompositeListViewMediator.BeforeDestruction;
begin
  CleanUpFieldInfos;
  FObserversInTransit.Free;
  FMediatorList.Free;
  FModel.DetachObserver(self);
  FModel  := nil;
  FView   := nil;
  inherited BeforeDestruction;
end;

procedure TCompositeListViewMediator.Update(ASubject: TtiObject);
begin
  Assert(FModel = ASubject);
  RebuildList;
end;

{ TODO: This is not working 100% yet. Be warned! }
procedure TCompositeListViewMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.SelCount = 0 then
    FSelectedObject := nil
  else
  begin
    FObserversInTransit.Clear;
    { If an item is already selected, assign the item's List of observers to a
      temporary container. This is done so that the same observers can be
      assigned to the new item. }
    if Assigned(FSelectedObject) then
      FObserversInTransit.Assign(FSelectedObject.ObserverList);

    // Assign Newly selected item to SelectedObject Obj.
    FSelectedObject := TtiObject(View.Selected.Data);

    { If an object was selected, copy the old item's observer List
      to the new item's observer List. }
    if FObserversInTransit.Count > 0 then
      FSelectedObject.ObserverList.Assign(FObserversInTransit);

    { Set the Observers Subject property to the selected object }
    for i := 0 to FSelectedObject.ObserverList.Count-1 do
    begin
Assert(False, 'Under Construction');
//      TMediatorView(FSelectedObject.ObserverList.Items[i]).Subject :=
//          FSelectedObject;
    end;

    // execute the NotifyObservers event to update the observers.
    FSelectedObject.NotifyObservers;
  end;
end;

procedure TCompositeListViewMediator.ParseDisplayNames;
Var
  I : Integer;
  lField : String;
  lInfo : TtiMediatorFieldInfo;
begin
  for I := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, I);
    lInfo := TtiMediatorFieldInfo.Create;
    lInfo.PropName := tiFieldName(lField);
    lInfo.Caption := tiFieldCaption(lField);
    lInfo.FieldWidth := tiFieldWidth(lField);
    lInfo.Alignment := tiFieldAlignment(lField);
    FFieldsInfo.AddObject(lInfo.Caption + '=' + tiFieldName(lField), lInfo);
  end; { Loop }
end;

procedure TCompositeListViewMediator.CleanUpFieldInfos;
var
  I : Integer;
begin
  for I := Pred(FFieldsInfo.Count) downto 0 do
      FFieldsInfo.Objects[I].Free;
  FFieldsInfo.Free;
end;

{ TCompositeStringGridMediator }

function TCompositeStringGridMediator.GetSelectedObjected: TtiObject;
begin
  if FView.Selection.Top = 0 then
    Result := nil
  else
    Result := TtiObject(FView.Objects[1, FView.Selection.Top]);
end;

procedure TCompositeStringGridMediator.SetSelectedObject(const AValue: TtiObject);
var
  i : integer;
begin
  for i := 1 to FView.RowCount - 1 do
  begin
    if TtiObject(FView.Objects[1, i]) = AValue then
    begin
    
      FView.Row := i;
      Exit; //==>
    end;
  end;
end;

procedure TCompositeStringGridMediator.SetShowDeleted(const AValue: Boolean);
begin
  if FShowDeleted = AValue then
    Exit; //==>
  
  BeginUpdate;
  try
    FShowDeleted := AVAlue;
    RebuildStringGrid;
  finally
    EndUpdate;
  end;
end;

procedure TCompositeStringGridMediator.DoCreateItemMediator(AData: TtiObject);
begin
  DataAndPropertyValid(AData);
end;

procedure TCompositeStringGridMediator.DoCreateItemMediator(AData: TtiObject; pRowIdx: Integer);
var
  i: Integer;
  lField: string;
  lFieldName: string;
  lMediatorView: TStringGridRowMediator;
begin
  FView.Objects[1, pRowIdx + 1] := AData;
  for i := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, i);
    lFieldName := tiFieldName(lField);
    FView.Cells[i, pRowIdx + 1] := AData.PropValue[lFieldName];
    
    lMediatorView := TStringGridRowMediator.CreateCustom(AData, FView, FDisplayNames, pRowIdx +  1, FIsObserving);
    FMediatorList.Add(lMediatorView);
  end;
end;


procedure TCompositeStringGridMediator.CreateSubMediators;
var
  i: integer;
  lField: string;
  lColumnTotalWidth: integer;
begin
  lColumnTotalWidth:= 0;
  for i := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, i);
    FView.Cells[i, 0]   := tiFieldCaption(lField);
    FView.ColWidths[i]  := tiFieldWidth(lField);

    //resize the last column to fill the grid.
    if i = tiNumToken(FDisplayNames, cFieldDelimiter) then
      FView.ColWidths[i] := FView.width - lColumnTotalWidth + 10
    else
      lColumnTotalWidth := lColumnTotalWidth + FView.ColWidths[i] + 20;
  end;
  
  for i := 0 to FModel.Count-1 do
  begin
    if (not FModel.Items[i].Deleted) or FShowDeleted then
      DoCreateItemMediator(FModel.Items[i], i);
  end;
end;

procedure TCompositeStringGridMediator.SetupGUIandObject;
begin
  //Setup default properties for the StringGrid
  {$ifdef fpc}
  FView.Clear;
  FView.Columns.Clear;
  {$endif}
  FView.Options       := FView.Options + [goRowSelect];
  FView.ColCount      := tiNumToken(FDisplayNames, cFieldDelimiter) + 1;
  if FShowDeleted then
    FView.RowCount := FModel.Count + 1
  else
    FView.RowCount := FModel.CountNotDeleted + 1;
  FView.FixedCols     := 1;
  FView.FixedRows     := 1;
  FView.ColWidths[0]  := 20;

  {$IFDEF FPC}
  FView.AutoSize := False;
  FView.ScrollBars := ssAutoBoth;
  {$ENDIF}
end;

procedure TCompositeStringGridMediator.RebuildStringGrid;
begin
  { This rebuilds the whole list. Not very efficient. }
//  View.BeginUpdate;
  try
    FMediatorList.Clear;
//    View.Columns.Clear;
    CreateSubMediators;
  finally
//    View.EndUpdate;
  end;

  { Do nothing. Can be implement as you see fit. A simple example is given
    in the Demos/GenericMediatingViews/Composite_ListView_Mediator }
//  raise EtiOPFProgrammerException.Create('You are trying to call ' + Classname
//    + '.RebuildStringGrid, which must be overridden in the concrete class.');
end;

function TCompositeStringGridMediator.DataAndPropertyValid(const AData: TtiObject): Boolean;
var
  i: Integer;
  lField: string;
begin
  Result := (FModel <> nil) and (FDisplayNames <> '');
  
  if not Result then
    Exit; //==>

  for i := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, i);
    Result := IsPublishedProp(AData, tiFieldName(lField));
    
    if not Result then
      raise Exception.CreateFmt('<%s> is not a property of <%s>',
            [tiFieldName(lField), AData.ClassName]);
  end;
end;

constructor TCompositeStringGridMediator.CreateCustom(AModel: TtiObjectList;
  AGrid: TStringGrid; ADisplayNames: string; IsObserving: Boolean);
begin
  inherited Create;
  
  FModel        := AModel;
  FView         := AGrid;
  FMediatorList := TObjectList.Create;
  FIsObserving  := IsObserving;
  FDisplayNames := ADisplayNames;
  FShowDeleted  := False;
  
  SetupGUIandObject;
  
  if (FDisplayNames <> '') and (tiNumToken(ADisplayNames, cFieldDelimiter) > 0) then
    CreateSubMediators;

  if IsObserving then
    FModel.AttachObserver(Self);
end;

procedure TCompositeStringGridMediator.BeforeDestruction;
begin
  FMediatorList.Free;
  if Assigned(FModel) then
    FModel.DetachObserver(Self);
  FModel  := nil;
  FView   := nil;
  
  inherited BeforeDestruction;
end;

procedure TCompositeStringGridMediator.Update(ASubject: TtiObject);
begin
  Assert(FModel = ASubject);
  RebuildStringGrid;
end;

end.

