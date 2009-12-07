
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
The contents of this file are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

If you make any changes or enhancements, which you think will
benefit other developers and will not break any existing code,
please forward your changes (well commented) to graemeg@gmail.com
and I will make them permanent.

Revision history:

  2005-09-01: First release by Graeme Geldenhuys (graemeg@gmail.com)

Purpose:
    Mediator for a standard TreeView component.  It builds the treeview and
    maintains a Popup Menu automatically. The mediator is not a Observer!

ToDo:
  * Minimise the refresh of the treeview when deleting a TTreeNode
  * Unit tests
  * Make the treeview a Observer. Somehow?

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


unit tiGenericTreeViewMediator;

{$I tiDefines.inc}

interface
uses
  ComCtrls
  ,Menus
  ,tiTreeBuildVisitor
  ,tiObject
  ;

type
  TTreeViewMediator = class(TObject)
  private
    FShowPopupMenu: Boolean;
    FPopupMenu: TPopupMenu;
    FTreeView: TTreeView;
    FDataMappings: TtiTVDataMappings;
    FData: TtiObject;
    procedure   SetShowPopupMenu(const Value: Boolean);
    procedure   SetTreeView(const Value: TTreeView);
    procedure   SetData(const Value: TtiObject);
    procedure   miInsertClick(Sender: TObject);
    procedure   miEditClick(Sender: TObject);
    procedure   miDeleteClick(Sender: TObject);
    function    CanDelete(pData: TtiObject): Boolean;
    function    CanInsert(pData: TtiObject): Boolean;
    function    CanEdit(pData: TtiObject): Boolean;
  protected
    procedure   CreatePopupMenu; virtual;
    procedure   OnExpandAllClick(Sender: TObject);
    procedure   OnExpandCollapseClick(Sender: TObject);
    procedure   OnMenuPopUp(Sender: TObject);
    procedure   BuildTreeView; virtual;
    procedure   SetupDataMappings; virtual; abstract;
  public
    constructor Create;
    constructor CreateCustom( pTreeView: TTreeView; pShowPopupMenu: Boolean = False); virtual;
    destructor  Destroy; override;
    procedure   InitializeTreeview;
    property    ShowPopupMenu: Boolean read FShowPopupMenu write SetShowPopupMenu;
    property    PopupMenu: TPopupMenu read FPopupMenu;
    property    TreeView: TTreeView read FTreeView write SetTreeView;
    property    DataMappings: TtiTVDataMappings read FDataMappings write FDataMappings;
    property    Data: TtiObject read FData write SetData;
  end;



implementation
uses
  SysUtils
  ;
  
resourcestring
  cExpandAll      = 'Expand all';
  cExpand         = 'Expand';
  cInsert         = 'Insert';
  cEdit           = 'Edit';
  cDelete         = 'Delete';


{ TTreeViewMediator }

procedure TTreeViewMediator.BuildTreeView;
var
  lVisitor: TVisObjToTree;
begin
  lVisitor := TVisObjToTree.Create(FTreeView);
  lVisitor.DataMappings := DataMappings;

  lVisitor.IncludeDeleted := False;
  Data.Iterate( lVisitor );
end;


function TTreeViewMediator.CanDelete(pData: TtiObject): Boolean;
var
  lMapping: TtiTVDataMapping;
  lbCanDelete: Boolean;
begin
  lMapping := DataMappings.FindMapping( pData.ClassName );

  if lMapping = nil then
    Result := False
  else
  begin
    lbCanDelete := lMapping.CanDelete;

    if Assigned(lMapping.OnCanDelete) then
      lMapping.OnCanDelete(Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
        Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data), lbCanDelete);

    Result := lbCanDelete;
  end;
end;


function TTreeViewMediator.CanEdit(pData: TtiObject): Boolean;
var
  lMapping: TtiTVDataMapping;
  lbCanEdit: Boolean;
begin
  lMapping := DataMappings.FindMapping( pData.ClassName );

  if lMapping = nil then
    Result := False
  else
  begin
    lbCanEdit := lMapping.CanEdit;

    if Assigned(lMapping.OnCanEdit) then
      lMapping.OnCanEdit(Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
        Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data), lbCanEdit);

    Result := lbCanEdit;
  end;
end;


function TTreeViewMediator.CanInsert(pData: TtiObject): Boolean;
var
  lMapping: TtiTVDataMapping;
  lbCanInsert: Boolean;
begin
  lMapping := DataMappings.FindMapping( pData.ClassName );

  if lMapping = nil then
    Result := False
  else
  begin
    lbCanInsert := lMapping.CanInsert;

    if Assigned(lMapping.OnCanInsert) then
      lMapping.OnCanInsert(Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
        Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data), lbCanInsert);

    Result := lbCanInsert;
  end;
end;


constructor TTreeViewMediator.Create;
begin
  CreatePopupMenu;
  FDataMappings := TtiTVDataMappings.Create(nil);
end;


constructor TTreeViewMediator.CreateCustom(pTreeView: TTreeView; pShowPopupMenu: Boolean);
begin
  FTreeview := pTreeView;
  Create;
  ShowPopupMenu := pShowPopupMenu;

  SetupDataMappings;
end;


procedure TTreeViewMediator.CreatePopupMenu;
var
  lItem: TMenuItem;
begin
  FPopupMenu := TPopupMenu.Create(nil);
  FPopupMenu.OnPopup := OnMenuPopup;

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := cExpandAll;
  lItem.Name        := 'miExpandAll';
  lItem.OnClick     := OnExpandAllClick;
  FPopupMenu.Items.Add(lItem);

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := cExpand;
  lItem.Name        := 'miExpandCollapse';
  lItem.OnClick     := OnExpandCollapseClick;
  lItem.ShortCut    := 13;   { Enter key }
  FPopupMenu.Items.Add(lItem);

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := '-';
  lItem.Name        := 'N1';
  FPopupMenu.Items.Add(lItem);

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := cInsert;
  lItem.Name        := 'miInsert';
  lItem.OnClick     := miInsertClick;
  lItem.ShortCut    := 45;   { Ins key }
  FPopupMenu.Items.Add(lItem);

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := cEdit;
  lItem.Name        := 'miEdit';
  lItem.OnClick     := miEditClick;
  lItem.ShortCut    := 16397;   { Ctrl + Enter key }
  FPopupMenu.Items.Add(lItem);

  lItem := TMenuItem.Create(FPopupMenu);
  lItem.Caption     := cDelete;
  lItem.Name        := 'miDelete';
  lItem.OnClick     := miDeleteClick;
  lItem.ShortCut    := 46;   { Del key }
  FPopupMenu.Items.Add(lItem);
end;


destructor TTreeViewMediator.Destroy;
begin
  FTreeView.PopupMenu := nil;
  FDataMappings.Free;
  FPopupMenu.Free;
  inherited;
end;


procedure TTreeViewMediator.InitializeTreeview;
begin
  {$IFDEF FPC}
    FTreeView.BeginUpdate;
  {$ELSE}
    FTreeView.Items.BeginUpdate;
  {$ENDIF}
  try
    FTreeView.Items.Clear;
    if Data = nil then
      Exit; //==>

    BuildTreeView;
    if FTreeView.Items.Count > 0 then
    begin
      FTreeView.Items.GetFirstNode.Expanded := True;
      FTreeView.Items.GetFirstNode.Selected := True;
      FTreeView.Items.GetFirstNode.Focused := True;
    end;
  finally
    {$IFDEF FPC}
      FTreeView.EndUpdate;
    {$ELSE}
      FTreeView.Items.EndUpdate;
    {$ENDIF}
  end;
end;


procedure TTreeViewMediator.miDeleteClick(Sender: TObject);
var
  lData: TtiObject;
  lMapping: TtiTVDataMapping;
begin
  lData := TtiObject(Treeview.Selected.Data);
  lMapping := DataMappings.FindMapping( lData.ClassName );

  if lMapping = nil then
  begin
    Exit;  //==>
  end
  else
  begin
    { if we have a event handler, execute it }
    if Assigned(lMapping.OnDelete) then
      lMapping.OnDelete( Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
        Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data) );
  end;
//  OnDeleteClick(TtiObject(Treeview.Selected.Data));
end;


procedure TTreeViewMediator.miEditClick(Sender: TObject);
var
  lData: TtiObject;
  lMapping: TtiTVDataMapping;
begin
  lData := TtiObject(Treeview.Selected.Data);
  lMapping := DataMappings.FindMapping( lData.ClassName );

  if lMapping = nil then
  begin
    Exit;  //==>
  end
  else
  begin
    { if we have a event handler, execute it }
    if Assigned(lMapping.OnEdit) then
      lMapping.OnEdit( Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
        Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data) );
  end;
end;


procedure TTreeViewMediator.miInsertClick(Sender: TObject);
var
  lData: TtiObject;
  lMapping: TtiTVDataMapping;
begin
  lData := TtiObject(Treeview.Selected.Data);
  lMapping := DataMappings.FindMapping( lData.ClassName );

  if lMapping = nil then
  begin
    Exit;  //==>
  end
  else
  begin
    { if we have a event handler, execute it }
    if Assigned(lMapping.OnInsert) then
    begin
      if Treeview.Selected.Parent = nil then
        lMapping.OnInsert( Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
          nil, nil )
      else
        lMapping.OnInsert( Treeview, Treeview.Selected, TObject(Treeview.Selected.Data),
          Treeview.Selected.Parent, TObject(Treeview.Selected.Parent.Data) );
    end;
  end;
end;


procedure TTreeViewMediator.OnExpandAllClick(Sender: TObject);
begin
  Treeview.Selected.Expand(True);
end;


procedure TTreeViewMediator.OnExpandCollapseClick(Sender: TObject);
begin
  Treeview.Selected.Expanded := not Treeview.Selected.Expanded;
end;


procedure TTreeViewMediator.OnMenuPopUp(Sender: TObject);
begin

  if (Treeview.Items.Count = 0) or (Treeview.Selected = nil) then
  begin
    FPopupMenu.Items.Items[0].Enabled := False;
    FPopupMenu.Items.Items[1].Enabled := False;

    FPopupMenu.Items.Find('Insert').Enabled := False;
    FPopupMenu.Items.Find('Edit').Enabled   := False;
    FPopupMenu.Items.Find('Delete').Enabled := False;
    Exit; //==>
  end;

  FPopupMenu.Items.Items[0].Enabled := True;
  FPopupMenu.Items.Items[1].Enabled := True;

  FPopupMenu.Items.Find('Insert').Enabled := CanInsert(TtiObject(Treeview.Selected.Data));
  FPopupMenu.Items.Find('Edit').Enabled   := CanEdit(TtiObject(Treeview.Selected.Data));
  FPopupMenu.Items.Find('Delete').Enabled := CanDelete(TtiObject(Treeview.Selected.Data));

  if Treeview.Selected.Expanded then
    FPopupMenu.Items.Items[1].Caption := 'Collapse'
  else
    FPopupMenu.Items.Items[1].Caption := 'Expand';
end;


procedure TTreeViewMediator.SetData(const Value: TtiObject);
begin
  FData := Value;
  if FDataMappings.Count < 1 then
    raise Exception.Create('Unable to build tree. No Data Mappings have been setup.');
  InitializeTreeview;
end;


procedure TTreeViewMediator.SetShowPopupMenu(const Value: Boolean);
begin
  FShowPopupMenu := Value;

  if Value then
    FTreeView.PopupMenu := PopupMenu
  else
    FTreeView.PopupMenu := nil;
end;


procedure TTreeViewMediator.SetTreeView(const Value: TTreeView);
begin
  FTreeView := Value;
end;

end.
