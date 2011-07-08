{
    This file is part of the tiOPF project.

    See the file license.txt, included in this distribution,
    for details about redistributing tiOPF.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A visitor that will build a TreeView based on the Mappings given.
      
      Usage:
        var
          lVisitor: TtiVisObjToTree;
        begin
          lVisitor := TtiVisObjToTree.Create(FTreeView);
          lVisitor.DataMappings := DataMappings;
          lVisitor.IncludeDeleted := False;
          Data.Iterate( lVisitor );
        end;
      
      ToDo:
        * Minimise the refresh of the treeview when deleting a TTreeNode
        * Unit tests
        * Make the treeview a Observer. Somehow?
}
unit tiTreeBuildVisitor; 

{$I tiDefines.inc}

interface

uses
  Classes
  ,fpg_tree
  ,tiVisitor
  ,tiObject
  ;

type
  TtiTVDataMappings = class;
  TtiTVDataMapping  = class;

  { Abstract visitor for building a Treeview }
  TVisTree = class(TtiVisitor)
  private
    FLastNode: TfpgTreeNode;
    FRoot: TfpgTreeNode;
    FTree: TfpgTreeView;
  protected
    procedure   AddChild(pNode: TfpgTreeNode; const pValue: string; pImageIndex: Integer); virtual;
    property    Root: TfpgTreeNode read FRoot;
    property    LastNode: TfpgTreeNode read FLastNode;
  public
    constructor Create(pTreeView: TfpgTreeView); virtual; reintroduce;
    property    Tree: TfpgTreeview read FTree;
  end;


  TtiVisObjToTree = class(TVisTree)
  private
    FbIncludeDeleted: boolean;
    FTVDataMappings: TtiTVDataMappings;
    FIndent: Integer;
    procedure   SetTVDataMappings(const AValue: TtiTVDataMappings);
  protected
    function    FindMapping(pData: TtiObject): TtiTVDataMapping;
    function    AcceptVisitor: Boolean; override;
  public
    constructor Create(pTreeView: TfpgTreeView); override;
    destructor  Destroy; override;
    procedure   Execute(const pVisited: TtiVisited); override;
    property    IncludeDeleted : boolean read FbIncludeDeleted write FbIncludeDeleted ;
    property    DataMappings: TtiTVDataMappings read FTVDataMappings write SetTVDataMappings;
  end;


  { Mappings used by the tree visitors to know what must go into the tree }
  TtiTVDataMappings = class(TCollection)
  private
    FTreeView: TfpgTreeView;
    FiItemNo: integer;
    function    GetItem(Index: Integer): TtiTVDataMapping;
    procedure   SetItem(Index: Integer; const Value: TtiTVDataMapping);
  protected
    function    GetOwner: TPersistent; override;
    function    ItemNo(pClassName: string): Integer;
  public
    constructor Create(pTreeView: TfpgTreeView);
    property    Items[Index: Integer]: TtiTVDataMapping read GetItem write SetItem;
    function    Add: TtiTVDataMapping; overload;
    function    Add(const pClass: TtiObjectClass; const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping; overload;
    function    FindMapping(pClassName: string): TtiTVDataMapping;
  end;


  TTVNodeConfirmEvent = procedure(pTreeView: TfpgTreeView; pNode: TfpgTreeNode;
      pData: TObject; pParentNode: TfpgTreeNode;  pParentData: TObject;
      var pConfirm: boolean) of object;


  TTVNodeEvent = procedure(pTreeView: TfpgTreeView; pNode: TfpgTreeNode; pData: TObject;
      pParentNode: TfpgTreeNode; pParentData: TObject) of object;


  { TtiTVDataMapping }

  TtiTVDataMapping = class(TCollectionItem)
  private
    FsDisplayPropName: string;
    FsDataClassName: string;
    FiImageIndex: integer;
    FName: TComponentName;
    FbHasChildren: boolean;
    FbCanInsert: boolean;
    FbCanEdit: boolean;
    FbCanDelete: boolean;
    FbCanInsertModule: boolean;
    FbCanProcessModule: boolean;
    FbCanDeleteModule: boolean;
    FbCanCompileModule: boolean;
    FbCanSignOffModule: boolean;
    // events
    FOnInsert: TTVNodeEvent;
    FOnEdit: TTVNodeEvent;
    FOnDelete: TTVNodeEvent;
    FOnInsertModule: TTVNodeEvent;
    FOnProcessModule: TTVNodeEvent;
    FDeleteModule: TTVNodeEvent;
    FOnCompileModule: TTVNodeEvent;
    FOnSignOffModule: TTVNodeEvent;
    FOnCanInsert: TTVNodeConfirmEvent;
    FOnCanEdit: TTVNodeConfirmEvent;
    FOnCanDelete: TTVNodeConfirmEvent;
    FOnCanInsertModule: TTVNodeConfirmEvent;
    FOnCanProcessModule: TTVNodeConfirmEvent;
    FOnCanDeleteModule: TTVNodeConfirmEvent;
    FOnCanCompileModule: TTVNodeConfirmEvent;
    FOnCanSignOffModule: TTVNodeConfirmEvent;

    procedure   SetDataClassName(const Value: string);
    function    ClassNameToCollectionItemName(const pValue: string): string;
  protected
    function    GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure   Assign(Source: TtiTVDataMapping); reintroduce;
  published
    { Properties }
    property    CanInsert: boolean read FbCanInsert write FbCanInsert default false;
    property    CanEdit: boolean read FbCanEdit write FbCanEdit default false;
    property    CanDelete: boolean read FbCanDelete write FbCanDelete default false;
    property    CanInsertModule: boolean read FbCanInsertModule write FbCanInsertModule;
    property    CanProcessModule: boolean read FbCanProcessModule write FbCanProcessModule;
    property    CanDeleteModule: boolean read FbCanDeleteModule write FbCanDeleteModule;
    property    CanCompileModule: boolean read FbCanCompileModule write FbCanCompileModule;
    property    CanSignOffModule: boolean read FbCanSignOffModule write FbCanSignOffModule;
    property    DataClass: string read FsDataClassName write SetDataClassName;
    property    DisplayPropName: string read FsDisplayPropName write FsDisplayPropName;
    property    ImageIndex: integer read FiImageIndex write FiImageIndex default -1;
    property    HasChildren: boolean read FbHasChildren write FbHasChildren default true;
    property    Name: TComponentName read FName write FName;
    { Events }
    property    OnInsert: TTVNodeEvent read FOnInsert write FOnInsert;
    property    OnEdit: TTVNodeEvent read FOnEdit write FOnEdit;
    property    OnDelete: TTVNodeEvent read FOnDelete write FOnDelete;
    property    OnInsertModule: TTVNodeEvent read FOnInsertModule write FOnInsertModule;
    property    OnProcessModule: TTVNodeEvent read FOnProcessModule write FOnProcessModule;
    property    OnDeleteModule: TTVNodeEvent read FDeleteModule write FDeleteModule;
    property    OnCompileModule: TTVNodeEvent read FOnCompileModule write FOnCompileModule;
    property    OnSignOffModule: TTVNodeEvent read FOnSignOffModule write FOnSignOffModule;
    property    OnCanInsert: TTVNodeConfirmEvent read FOnCanInsert write FOnCanInsert;
    property    OnCanEdit: TTVNodeConfirmEvent read FOnCanEdit write FOnCanEdit;
    property    OnCanDelete: TTVNodeConfirmEvent read FOnCanDelete write FOnCanDelete;
    property    OnCanInsertModule: TTVNodeConfirmEvent read FOnCanInsertModule write FOnCanInsertModule;
    property    OnCanProcessModule: TTVNodeConfirmEvent read FOnCanProcessModule write FOnCanProcessModule;
    property    OnCanDeleteModule: TTVNodeConfirmEvent read FOnCanDeleteModule write FOnCanDeleteModule;
    property    OnCanCompileModule: TTVNodeConfirmEvent read FOnCanCompileModule write FOnCanCompileModule;
    property    OnCanSignOffModule: TTVNodeConfirmEvent read FOnCanSignOffModule write FOnCanSignOffModule;
  end;


implementation

uses
  SysUtils
  ,TypInfo
  ,tiUtils
  ,tiLog
  ,fpg_base
  ;


const
  cMethodPrefix = 'tiTVMapping';

{ TtiVisObjToTree }

procedure TtiVisObjToTree.SetTVDataMappings(const AValue: TtiTVDataMappings);
var
  lNew: TtiTVDataMapping;
  i: Integer;
begin
  FTVDataMappings.Clear;
  for i := 0 to AValue.Count - 1 do
  begin
    lNew := FTVDataMappings.Add;
    lNew.Assign(AValue.Items[i]);
  end;
end;

function TtiVisObjToTree.FindMapping(pData: TtiObject): TtiTVDataMapping;
var
  i: Integer;
  lDataMapping: TtiTVDataMapping;
begin
  Result := nil;
  for i := 0 to DataMappings.count - 1 do
  begin
    if tiIsClassOfType(pData, DataMappings.Items[i].DataClass) then
    begin
      lDataMapping := DataMappings.Items[i];
      if IsPublishedProp(pData, lDataMapping.DisplayPropName) then
      begin
        Result := lDataMapping;
        Break; //==>
      end;
    end;
  end; { for }
end;

function TtiVisObjToTree.AcceptVisitor: Boolean;
var
  lDataMapping: TtiTVDataMapping;
begin
  Result := (Visited is TtiObject);
  if not Result then
    Exit; //==>

  lDataMapping := FindMapping(TtiObject(Visited));
  Result := lDataMapping <> nil;
  if not Result then
    Exit; //==>

  Result := ((not TtiObject(Visited).Deleted) or
             (TtiObject(Visited).Deleted and IncludeDeleted));
end;

constructor TtiVisObjToTree.Create(pTreeView: TfpgTreeView);
begin
  inherited Create(pTreeView);
  FbIncludeDeleted := False;
  FTVDataMappings := TtiTVDataMappings.Create(pTreeView);
  FIndent := 0;
end;

destructor TtiVisObjToTree.Destroy;
begin
  FTVDataMappings.Free;
  inherited Destroy;
end;

procedure TtiVisObjToTree.Execute(const pVisited: TtiVisited);
var
  Prev: TfpgTreeNode;
  i: integer;
  lDataMapping: TtiTVDataMapping;
  lsCaption: string;
  Counter: Integer;
begin
  inherited;

  if not AcceptVisitor then
    Exit;  //==>

  { At this point we know there must be a mapping, as it passed the AcceptVisitor }
  lDataMapping := FindMapping(TtiObject(Visited));

  { Extract the correct property value to display in the treeview, based on what
    is set in the mapping. }
  lsCaption := GetPropValue(pVisited, lDataMapping.DisplayPropName);

  { At what level should we add the new node?
   LastNode is set in the AddChild method of TvisTree(parent class)}
  if not Assigned(LastNode) then
  begin
    AddChild(Tree.RootNode, lsCaption, lDataMapping.ImageIndex);
  end
  else
  begin
    if (Depth > FIndent) then
    begin
      AddChild(LastNode, lsCaption, lDataMapping.ImageIndex);
    end
    else if (Depth < FIndent) then
    begin
      Prev := LastNode;
      Counter := (FIndent - Depth);
      Counter := (Counter div 2);

      for i := 0 to Counter do
      begin
        if Prev.Parent <> nil then
          Prev := Prev.Parent;
      end;

      AddChild(Prev, lsCaption, lDataMapping.ImageIndex);
    end
    else
    begin
      AddChild(LastNode.Parent, lsCaption, lDataMapping.ImageIndex);
    end;
  end; { if/else }

  FIndent := Depth;
  LastNode.Text := LastNode.Text;
end;

{ TtiTVDataMappings }

function TtiTVDataMappings.GetItem(Index: Integer): TtiTVDataMapping;
begin
  Result := TtiTVDataMapping(inherited GetItem(Index));
end;

procedure TtiTVDataMappings.SetItem(Index: Integer; const Value: TtiTVDataMapping);
begin
  inherited SetItem(Index, Value);
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

constructor TtiTVDataMappings.Create(pTreeView: TfpgTreeView);
begin
  inherited Create(TtiTVDataMapping);
  FTreeView := pTreeView;
  FiItemNo := 0;
end;

function TtiTVDataMappings.Add: TtiTVDataMapping;
begin
  Result := TtiTVDataMapping(inherited Add);
end;

function TtiTVDataMappings.Add(const pClass: TtiObjectClass;
  const pDisplayPropName: string; pImageIndex: Integer): TtiTVDataMapping;
begin
  Result                  := Self.Add;
  Result.DataClass        := pClass.ClassName;
  Result.DisplayPropName  := pDisplayPropName;
  Result.ImageIndex       := pImageIndex;
end;

function TtiTVDataMappings.FindMapping(pClassName: string): TtiTVDataMapping;
begin
  Result := Self.Items[ItemNo(pClassName)];
end;

{ TVisTree }

procedure TVisTree.AddChild(pNode: TfpgTreeNode; const pValue: string; pImageIndex: Integer);
var
  lNode: TfpgTreeNode;
begin
  if Trim(pValue) = '' then
    Exit; //==>

  lNode := pNode.AppendText(pValue);
  lNode.ImageIndex := pImageIndex;
  lNode.Data := Visited;
  FLastNode := lNode;
end;

constructor TVisTree.Create(pTreeView: TfpgTreeView);
begin
  inherited Create;
  FTree := pTreeView;
end;


{ TtiTVDataMapping }

procedure TtiTVDataMapping.SetDataClassName(const Value: string);
begin
  if FName = ClassNameToCollectionItemName(FsDataClassName) then
    FName := ClassNameToCollectionItemName(Value);
    
  FsDataClassName := Value;
end;

function TtiTVDataMapping.ClassNameToCollectionItemName(const pValue: string): string;
begin
  if pValue = '' then
    Exit; //==>

  Result := cMethodPrefix + Copy(pValue, 2, Length(pValue) - 1);
end;

function TtiTVDataMapping.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

constructor TtiTVDataMapping.Create(ACollection: TCollection);
begin
  inherited;
  FsDisplayPropName := 'Caption';
  FsDataClassName   := 'TtiObject';
  FName             := ClassNameToCollectionItemName(FsDataClassName);
  FiImageIndex      := -1;
  FbHasChildren     := True;
  FbCanInsert       := False;
  FbCanEdit         := False;
  FbCanDelete       := False;
end;

procedure TtiTVDataMapping.Assign(Source: TtiTVDataMapping);
begin
  DataClass         := Source.DataClass;
  DisplayPropName   := Source.DisplayPropName;
  ImageIndex        := Source.ImageIndex;
  HasChildren       := Source.HasChildren;
end;

end.

