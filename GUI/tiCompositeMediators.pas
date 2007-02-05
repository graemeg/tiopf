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

  { Composite mediator for TListView }
  TCompositeListViewMediator = class(TtiObject)
  private
    FIsObserving: Boolean;
    FDisplayNames: string;
    FShowDeleted: Boolean;
    function GetSelectedObject: TtiObject;
    procedure   SetSelectedObject(const AValue: TtiObject);
    procedure   SetShowDeleted(const AValue: Boolean);
    procedure   DoCreateItemMediator(AData: TtiObject);
  protected
    FView: TListView;
    FModel: TtiObjectList;
    FMediatorList: TObjectList;
    procedure   CreateSubMediators;
    procedure   SetupGUIandObject; virtual;
    procedure   RebuildList; virtual;
    function    DataAndPropertyValid(const AData: TtiObject): Boolean;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView; ADisplayNames: string; IsObserving: Boolean = True);
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    View: TListView read FView;
    property    Model: TtiObjectList read FModel;
    property    DisplayNames: string read FDisplayNames;
    property    IsObserving: Boolean read FIsObserving;
    property    SelectedObject: TtiObject read GetSelectedObject write SetSelectedObject;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
  end;



implementation

uses
  tiUtils
  ,StdCtrls
  ,typinfo
  ,tiExcept
  ;
  
const
  cFieldDelimiter = ';';
  cBrackets = '()';
  
type
  TListViewListItemMediator = class(TtiObject)
  private
    FModel: TtiObject;
    FView: TListItem;
    FDisplayNames: string;
    procedure   SetupFields;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TListItem; const ADisplayNames: string; IsObserving: Boolean = True);
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    View: TListItem read FView;
    property    Model: TtiObject read FModel;
    property    DisplayNames: string read FDisplayNames;
  end;


{ Helper functions }

{ Extract the field name part from the AField string which is in the format
  fieldname(width)   eg:  Caption(25)   will return: Caption }
function tiFieldName(AField: string): string;
begin
  Result := tiToken(AField, cBrackets[1], 1);
end;

{ Extract the width part from the AField string which is in the format
  fieldname(width)   eg:  Caption(25)   with return: 25 }
function tiFieldWidth(AField: string): integer;
var
  s: string;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) = '' then
    Result := 75  // default width
  else
    Result := StrToInt(s);
end;


{ TListViewListItemMediator }

procedure TListViewListItemMediator.SetupFields;
var
  c: integer;
  lField: string;
begin
  lField := tiToken(FDisplayNames, cFieldDelimiter, 1);
  FView.Caption := FModel.PropValue[tiFieldName(lField)];

  for c := 2 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(FDisplayNames, cFieldDelimiter, c);
    FView.SubItems.Add(FModel.PropValue[tiFieldName(lField)]);
  end;
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TListItem; const ADisplayNames: string; IsObserving: Boolean);
begin
  inherited Create;
  FModel        := AModel;
  FView         := AView;
  FDisplayNames := ADisplayNames;

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
begin
  Assert(FModel = ASubject);
  
  lField := tiToken(DisplayNames, cFieldDelimiter, 1);
  FView.Caption := FModel.PropValue[tiFieldName(lField)];

  for c := 2 to tiNumToken(DisplayNames, cFieldDelimiter) do
  begin
    lField := tiToken(DisplayNames, cFieldDelimiter, c);
    FView.SubItems[c-2] := FModel.PropValue[tiFieldName(lField)];
  end;
end;

{ TCompositeListViewMediator }

procedure TCompositeListViewMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to FView.Items.Count - 1 do
  begin
    if TtiObject(FView.Items[i].Data) = AValue then
    begin
      FView.Selected := FView.Items[i];
      exit;
    end;
  end;
end;

function TCompositeListViewMediator.GetSelectedObject: TtiObject;
begin
  if FView.SelCount = 0 then
    result := nil
  else
    result := TtiObject(FView.Selected.Data);
end;

procedure TCompositeListViewMediator.SetShowDeleted(const AValue: Boolean);
begin
  if FShowDeleted = AValue then
    exit; //==>
    
  BeginUpdate;
  try
    FShowDeleted := AValue;
    RebuildList;
  finally
    EndUpdate;
  end;
end;

procedure TCompositeListViewMediator.DoCreateItemMediator(AData: TtiObject);
var
  li: TListItem;
  m: TListViewListItemMediator;
begin
  DataAndPropertyValid(AData);
  
  { Create ListItem and Mediator }
  li  := FView.Items.Add;
  li.Data := AData;
  m   := TListViewListItemMediator.CreateCustom(AData, li, FDisplayNames, FIsObserving);
  FMediatorList.Add(m);
end;

procedure TCompositeListViewMediator.CreateSubMediators;
var
  c: integer;
  lc: TListColumn;
  lField: string;
begin
  { Create column headers }
  for c := 1 to tiNumToken(FDisplayNames, cFieldDelimiter) do
  begin
    lc            := FView.Columns.Add;
    lc.AutoSize   := False;
    lField        := tiToken(FDisplayNames, cFieldDelimiter, c);
    lc.Caption    := tiFieldName(lField);
    lc.Width      := tiFieldWidth(lField);
  end;

  FModel.ForEach(DoCreateItemMediator, FShowDeleted);
end;

procedure TCompositeListViewMediator.SetupGUIandObject;
begin
  { Setup TListView }
  FView.Columns.Clear;
  FView.Items.Clear;
  FView.ViewStyle         := vsReport;
  FView.ShowColumnHeaders := True;
  {$IFDEF FPC}
  FView.AutoSize          := False;
  FView.ScrollBars        := ssAutoBoth;
  {$ENDIF}
end;

procedure TCompositeListViewMediator.RebuildList;
begin
  { Do nothing. Can be implement as you see fit. A simple example is given
    in the Demos/GenericMediatingViews/Composite_ListView_Mediator }
  raise EtiOPFProgrammerException.Create('You are trying to call ' + Classname
    + '.RebuildList, which must be overridden in the concrete class.');
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

constructor TCompositeListViewMediator.CreateCustom(AModel: TtiObjectList;
  AView: TListView; ADisplayNames: string; IsObserving: Boolean);
begin
  inherited Create;
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
    CreateSubMediators;
  end;
    
  if IsObserving then
    FModel.AttachObserver(self);
end;

procedure TCompositeListViewMediator.BeforeDestruction;
begin
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

end.

