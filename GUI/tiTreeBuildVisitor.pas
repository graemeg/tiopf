
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Purpose:
  A visitor that will build a TreeView based on the Mappings given.

Usage:
  var
    lVisitor: TVisObjToTree;
  begin
    lVisitor := TVisObjToTree.Create(FTreeView);
    lVisitor.DataMappings := DataMappings;
    lVisitor.IncludeDeleted := False;
    Data.Iterate( lVisitor );
  end;

ToDo:
  * Minimise the refresh of the treeview when deleting a TTreeNode
  * Unit tests
  * Make the treeview a Observer. Somehow?

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit tiTreeBuildVisitor;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes
  ,ComCtrls
  ,tiObject
  ,tiVisitor
  ;


type
  TtiTVDataMappings = class;
  TtiTVDataMapping  = class;



  { Abstract visitor for building a TTreeview }
  TVisTree = class( TtiVisitor )
  private
    FTree: TTreeView;
    FLastNode: TTreeNode;
    FRoot: TTreeNode;
  protected
    procedure   AddChild(pNode: TTreeNode; const pValue: string; pImageIndex: Integer); virtual;
    property    Root: TTreeNode read FRoot;
    property    LastNode: TTreeNode read FLastNode;
  public
    constructor Create(pTreeView: TTreeView); reintroduce;
    destructor  Destroy; override;
    property    Tree: TTreeView read FTree;
  end;


  { Stream a TtiObject out as TreeView }

  { TVisObjToTree }

  TVisObjToTree = class( TVisTree )
  private
    FbIncludeDeleted: boolean;
    FTVDataMappings: TtiTVDataMappings;
    procedure   SetTVDataMappings(const Value: TtiTVDataMappings);
    function    FindMapping(pData: TtiObject): TtiTVDataMapping;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    constructor Create(pTreeView: TTreeView); reintroduce;
    destructor  Destroy; override;
    procedure   Execute(const pVisited: TtiVisited); override;
    property    IncludeDeleted : boolean read FbIncludeDeleted write FbIncludeDeleted ;
    property    DataMappings: TtiTVDataMappings read FTVDataMappings write SetTVDataMappings;
  end;


  { Mappings used by the tree visitors to know what must go into the tree }
  TtiTVDataMappings = class(TCollection)
  private
    FTreeView: TTreeView;
    FiItemNo: integer;
    function    GetItem(Index: Integer): TtiTVDataMapping;
    procedure   SetItem(Index: Integer; const Value: TtiTVDataMapping);
  protected
    function    GetOwner: TPersistent; override;
    procedure   Update(Item: TCollectionItem); override;
    function    ItemNo(pClassName: string): Integer;
  public
    constructor Create(pTreeView: TTreeView);
    property    Items[Index: Integer]: TtiTVDataMapping read GetItem write SetItem;
    function    Add: TtiTVDataMapping; overload ;
    function    Add( const pClass: TtiObjectClass; const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping; overload;
    function    FindMapping(pClassName: string): TtiTVDataMapping;
  end;


  TTVNodeConfirmEvent = procedure(pTreeView: TTreeView;
    pNode: TTreeNode;
    pData: TObject;
    pParentNode: TTreeNode;
    pParentData: TObject;
    var pConfirm: boolean) of object;


  TTVNodeEvent = procedure(pTreeView: TTreeView;
    pNode: TTreeNode;
    pData: TObject;
    pParentNode: TTreeNode;
    pParentData: TObject) of object;



  { TtiTVDataMapping }

  TtiTVDataMapping = class(TCollectionItem)
  private
    FsDisplayPropName: string;
    FsDataClassName: string;
    FiImageIndex: integer;
    FName: TComponentName;
    FbHasChildren: boolean;
    FbCanInsert:  boolean;
    FbCanEdit:    boolean;
    FbCanDelete:  boolean;

    FOnInsert:    TTVNodeEvent;
    FOnEdit:      TTVNodeEvent;
    FOnDelete:    TTVNodeEvent;

    FOnCanInsert: TTVNodeConfirmEvent;
    FOnCanEdit:   TTVNodeConfirmEvent;
    FOnCanDelete: TTVNodeConfirmEvent;


//    FOnDrop: TtiTVDragDropEvent;
//    FOnCanView: TTVNodeConfirmEvent;
//    FOnGetDataPage: TTVGetDataPageEvent;
//    FOnCanAcceptDrop: TtiTVDragDropConfirmEvent;
//    FbCanView: boolean;
//    FbCanDragCopy: boolean;
//    FbCanDragMove: boolean;
    procedure   SetDataClassName(const Value: string);
    function    ClassNameToCollectionItemName(const pValue: string): string;
  protected
    function    GetDisplayName: string; override;
//    property    OnGetDataPage: TTVGetDataPageEvent read FOnGetDataPage write FOnGetDataPage;
  published
    { Properties }
    property    CanInsert: boolean read FbCanInsert write FbCanInsert default false;
    property    CanEdit:   boolean read FbCanEdit write FbCanEdit default false;
    property    CanDelete: boolean read FbCanDelete write FbCanDelete default false;
//    property CanView:   boolean read FbCanView write FbCanView default false;
//    property CanDragCopy: boolean read FbCanDragCopy write FbCanDragCopy default false;
//    property CanDragMove: boolean read FbCanDragMove write FbCanDragMove default false;

    property    DataClass: string read FsDataClassName write SetDataClassName;
    property    DisplayPropName: string read FsDisplayPropName write FsDisplayPropName;
    property    ImageIndex: integer read FiImageIndex write FiImageIndex default -1;
    property    HasChildren: boolean read FbHasChildren write FbHasChildren default true;
    property    Name: TComponentName read FName write FName;

    { Events }
    property    OnInsert: TTVNodeEvent read FOnInsert write FOnInsert;
    property    OnEdit: TTVNodeEvent read FOnEdit write FOnEdit;
    property    OnDelete: TTVNodeEvent read FOnDelete write FOnDelete;
//    property    OnExpand: TTVNodeEvent read FOnExpand write FOnExpand;
//    property OnDrop: TtiTVDragDropEvent read FOnDrop write FOnDrop;

//    property OnCanView:   TtiTVNodeConfirmEvent read FOnCanView write FOnCanView;
    property    OnCanInsert: TTVNodeConfirmEvent read FOnCanInsert write FOnCanInsert;
    property    OnCanEdit: TTVNodeConfirmEvent read FOnCanEdit write FOnCanEdit;
    property    OnCanDelete: TTVNodeConfirmEvent read FOnCanDelete write FOnCanDelete;
//    property OnCanAcceptDrop: TtiTVDragDropConfirmEvent read FOnCanAcceptDrop write FOnCanAcceptDrop;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;
    procedure   Assign(Source: TtiTVDataMapping); reintroduce;
  end;


implementation
uses
  SysUtils
  ,TypInfo
  ,tiUtils
  ;


const
  cMethodPrefix = 'tiTVMapping';


{ TVisTree }

procedure TVisTree.AddChild(pNode: TTreeNode; const pValue: string; pImageIndex: Integer);
var
  lNode: TTreeNode;
begin
  if Trim(pValue) = '' then
    Exit; //==>

  lNode := FTree.Items.AddChildObject(pNode, pValue, Visited);
  lNode.ImageIndex    := pImageIndex;
  lNode.SelectedIndex := pImageIndex;

  FLastNode := lNode;
end;


constructor TVisTree.Create(pTreeView: TTreeView);
begin
  inherited Create;
  FTree := pTreeView;
end;


destructor TVisTree.Destroy;
begin
  { Don't free the tree, as it was passed into this Class, and we only store
    a reference to it. }
  inherited;
end;


{ TVisObjToTree }

function TVisObjToTree.AcceptVisitor: boolean;
var
  lDataMapping: TtiTVDataMapping;
begin
  result := ( Visited is TtiObject );

  if not result then
    Exit; //==>

  lDataMapping := FindMapping( TtiObject(Visited) );
  result := lDataMapping <> nil;

  if not result then
    Exit; //==>

  result := (( not TtiObject( Visited ).Deleted ) or
             ( TtiObject( Visited ).Deleted and IncludeDeleted ));
end;


constructor TVisObjToTree.Create(pTreeView: TTreeView);
begin
  inherited;
  FbIncludeDeleted := False;
  FTVDataMappings := TtiTVDataMappings.Create(nil);
end;


destructor TVisObjToTree.Destroy;
begin
  FTVDataMappings.Free;
  inherited;
end;


procedure TVisObjToTree.Execute(const pVisited: TtiVisited);
var
  Prev: TTreeNode;
  i: integer;
  lDataMapping: TtiTVDataMapping;
  lsCaption: String;
  lDepth, lIndent: Integer;
begin
  inherited;
  if not AcceptVisitor then
    Exit ; //==>

  { At this point we know there must be a mapping, as it passed the AcceptVisitor }
  lDataMapping := FindMapping( TtiObject(Visited) );
  { Extract the correct property value to display in the treeview, based on what
    is set in the mapping. }
  lsCaption := GetPropValue(pVisited, lDataMapping.DisplayPropName);

  { At what level should we add the new node? }
  if not Assigned(LastNode) then
  begin
    AddChild( Tree.Items.GetFirstNode, lsCaption, lDataMapping.ImageIndex );
  end
  else
  begin
    lDepth := DataMappings.ItemNo(pVisited.ClassName);
    lIndent := LastNode.Level;
    if lIndent = lDepth then
      AddChild( LastNode.Parent, lsCaption, lDataMapping.ImageIndex )
    else if lIndent < lDepth then
      AddChild( LastNode, lsCaption, lDataMapping.ImageIndex )
    else if lIndent > lDepth then
    begin
      { Using the Depth property, we need to track back into the TreeView nodes,
        to that same Level (Depth), before we can add the next Node. }
      Prev := LastNode;
      for i := 0 to (lIndent - lDepth) do
      begin
        Prev := Prev.Parent;
      end;
      { We should now be at the correct Level in the TreeView, to add the next
        Node. }
      AddChild( Prev, lsCaption, lDataMapping.ImageIndex );
    end;
  end;
  LastNode.Text := LastNode.Text;
end;


function TVisObjToTree.FindMapping(pData: TtiObject): TtiTVDataMapping;
var
  i: integer;
  lDataMapping: TtiTVDataMapping;
begin
  result := nil;
  for i := 0 to DataMappings.Count - 1 do
  begin
    if tiIsClassOfType(pData, DataMappings.Items[i].DataClass) then
    begin
      lDataMapping := DataMappings.Items[i];
      if IsPublishedProp(pData, lDataMapping.DisplayPropName) then
      begin
        result := lDataMapping;
        Break; //==>
      end;
    end;
  end;
end;


procedure TVisObjToTree.SetTVDataMappings(const Value: TtiTVDataMappings);
var
  lNew: TtiTVDataMapping;
  i: Integer;
begin
  for i := 0 to Value.Count - 1 do
  begin
    lNew := FTVDataMappings.Add;
    lNew.Assign( Value.Items[i]);
  end;
end;


{ TtiTVDataMapping }

procedure TtiTVDataMapping.Assign(Source: TtiTVDataMapping);
begin
  DataClass         := Source.DataClass;
  DisplayPropName   := Source.DisplayPropName;
  ImageIndex        := Source.ImageIndex;
//  OnGetDataPage     := Source.OnGetDataPage;
  HasChildren       := Source.HasChildren;
end;


function TtiTVDataMapping.ClassNameToCollectionItemName(const pValue: string): string;
begin
  if pValue = '' then
    Exit; //==>
  result := cMethodPrefix + Copy(pValue, 2, Length(pValue) - 1);
end;


constructor TtiTVDataMapping.Create(ACollection: TCollection);
begin
  inherited;
  FsDisplayPropName := 'Caption';
  FsDataClassName   := 'TtiObject';
  FName             := ClassNameToCollectionItemName(FsDataClassName);
  FiImageIndex      := -1;
  FbHasChildren     := true;
  FbCanInsert       := false;
  FbCanEdit         := false;
  FbCanDelete       := false;
//  FbCanDragCopy := false;
//  FbCanDragMove := false;
end;


destructor TtiTVDataMapping.Destroy;
begin
  inherited;
end;


function TtiTVDataMapping.GetDisplayName: string;
begin
//  if Result = '' then
    Result := inherited GetDisplayName
//  else
//    Result := Result;
end;


procedure TtiTVDataMapping.SetDataClassName(const Value: string);
begin
  if FName = ClassNameToCollectionItemName(FsDataClassName) then
    FName := ClassNameToCollectionItemName(Value);
  FsDataClassName := Value;
end;


{ TtiTVDataMappings }

function TtiTVDataMappings.Add: TtiTVDataMapping;
begin
  result := TtiTVDataMapping(inherited Add);
end;


function TtiTVDataMappings.Add(const pClass: TtiObjectClass;
  const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping;
begin
  Result                  := Self.Add;
  Result.DataClass        := pClass.ClassName;
  Result.DisplayPropName  := pDisplayPropName;
  Result.ImageIndex       := pImageIndex;
end;


constructor TtiTVDataMappings.Create(pTreeView: TTreeView);
begin
  inherited Create(TtiTVDataMapping);
  FTreeView := pTreeView;
  FiItemNo := 0;
end;


function TtiTVDataMappings.FindMapping(pClassName: string): TtiTVDataMapping;
begin
  Result := Self.Items[ ItemNo(pClassName) ];
end;


function TtiTVDataMappings.GetItem(Index: Integer): TtiTVDataMapping;
begin
  Result := TtiTVDataMapping(inherited GetItem(Index));
end;


function TtiTVDataMappings.GetOwner: TPersistent;
begin
  result := TPersistent(FTreeView);
end;


function TtiTVDataMappings.ItemNo(pClassName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  
  for i := 0 to Count - 1 do
  begin
    if Items[i].DataClass = pClassName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


procedure TtiTVDataMappings.SetItem(Index: Integer; const Value: TtiTVDataMapping);
begin
  inherited SetItem(Index, Value);
end;


procedure TtiTVDataMappings.Update(Item: TCollectionItem);
begin
  inherited;
end;

end.
