unit tiCompositeMediators;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

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
    FSelectedObject: TtiObject;
    FShowDeleted: Boolean;
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
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TListView; ADisplayNames: string; IsObserving: Boolean = True);
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    View: TListView read FView;
    property    Model: TtiObjectList read FModel;
    property    DisplayNames: string read FDisplayNames;
    property    IsObserving: Boolean read FIsObserving;
    property    SelectedObject: TtiObject read FSelectedObject write SetSelectedObject;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
  end;



implementation

uses
  tiUtils
  ,StdCtrls
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
begin
  if FSelectedObject=AValue then exit;
  FSelectedObject:=AValue;
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
    CreateSubMediators;
    
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

