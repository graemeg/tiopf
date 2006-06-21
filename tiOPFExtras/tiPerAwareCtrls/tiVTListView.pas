{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    September 2003, Andrew Denton created.
    October 2003, Andrew Denton Added Filter events
                  Added SelectedItem Property
                  Added Derived Column support (sortable too!)
                  Added HeaderPopUpMenu for run-time column selection
                  Added pop-up menu for editing and filtering.
                  Added filtering code.
                  Added Export framework.
    November 2003, AD - Fixed bug where a filter that returned no data disabled
                  the Filter menu item making it imposiible to clear or modify
                  the filter.
                  Added ItemCount property.
                  Added Columns Editor and about box to speed menu.

  Purpose:
     To provide an alternative to the tiListView based on Mike Lischke's Virtual
     Treeview component. Should prove more attractive, faster and more memory
     efficient than the TListView based tiListView.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
Unit tiVTListView;

Interface

Uses Classes, Controls, Menus, TypInfo, VirtualTrees, VTHeaderPopup, tiPtnVisPerObj;

Const
  FilterConjs : Array[0..2] Of String = ('',
    'And',
    'Or');

  FilterOps : Array[0..8] Of String = ('',
    'Equals',
    'Does Not Equal',
    'Is Greater Than',
    'Is Greater Than Or Equal To',
    'Is Less Than',
    'Is Less Than Or Equal To',
    'Contains',
    'Does Not Contain'
    );

  ctkString = [tkChar, tkString, tkWChar, tkLString, tkWString];
  ctkInt = [tkInteger, tkInt64];
  ctkFloat = [tkFloat];
  ctkSimple = ctkString + ctkInt + ctkFloat;

Type
  TtiVTListView = Class;
  TtiLVTypeKind = (tiTKInteger, tiTKFloat, tiTKString);

  TFilterConj = (fcNone, fcAnd, fcOr);

  TFilterOp = (foNone, foEqual, foNotEqual,
    foGreater, foGreaterEqual,
    foLess, foLessEqual,
    foContains, foNotContains
    );

  TLVFilter = Class
  Private
    FJoin : TFilterConj;
    FOperator : TFilterOp;
    FFieldName : String;
    FValue : String;
    FListView : TtiVTListView;
    Function GetSimplePropType(pPersistent : TPersistent; pPropName : String) : TtiLVTypeKind;
    Function WildCardMatch(pInputStr, pWilds : String; pIgnoreCase : Boolean) : Boolean;
    Function GetStringExpression : String;
    Function StringPropPasses(pValue : TPersistent) : Boolean;
    Function IntPropPasses(pValue : TPersistent) : Boolean;
    Function FloatPropPasses(pValue : TPersistent) : Boolean;
  Public
    Constructor Create;
    Function PassesFilter(pData : TPersistent) : Boolean;
    Property ListView : TtiVTListView Read FListView;
    Property Join : TFilterConj Read FJoin Write FJoin;
    Property PropName : String Read FFieldName Write FFieldName;
    Property Operator : TFilterOp Read FOperator Write FOperator;
    Property Value : String Read FValue Write FValue;
    Property StringExpression : String Read GetStringExpression;
  End;

  TtiVTListColumn = Class;

  TlvTypeKind = (lvtkString, lvtkInt, lvtkFloat, lvtkDateTime, lvtkCurrency);

  TtiLVOnFilterDataEvent = Procedure(pData : TPersistent; Var pInclude : Boolean) Of Object;
  TtiLVOnDeriveColumnEvent = Procedure(pData : TPersistent; pColumn : TtiVTListColumn; Var pResult : String) Of Object;
  TtiLVItemEditEvent = Procedure(pData : TPersistent) Of Object;

  TtiVTListColumn = Class(TVirtualTreeColumn)
  Private
    FOnDeriveColumn : TtiLVOnDeriveColumnEvent;
  Protected
    FDerived : Boolean;
    FDisplayMask : String;
    FFieldName : String;
    FFieldType : TlvTypeKind;
    FVisible : Boolean;
  Public
    Constructor Create(Collection : TCollection); Override;
  Published
    Property Alignment;
    Property BiDiMode;
    Property Color;
    Property FieldName : String Read FFieldName Write FFieldName;
    Property FieldType : TlvTypeKind Read FFieldType Write FFieldType;
    Property DisplayMask : String Read FDisplayMask Write FDisplayMask;
    Property Derived : Boolean Read FDerived Write FDerived;
    Property Hint;
    Property ImageIndex;
    Property Layout;
    Property Margin;
    Property MaxWidth;
    Property MinWidth;
    Property Options;
    Property Position;
    Property Spacing;
    Property Style;
    Property Tag;
    Property Text;
    Property Visible : Boolean Read FVisible Write FVisible;
    Property Width;
    Property OnDeriveColumn : TtiLVOnDeriveColumnEvent Read FOnDeriveColumn Write FOnDeriveColumn;
  End;

  TtiVTHeader = Class(TVTHeader)
  Protected
  Public
    Constructor Create(AOwner : TBaseVirtualTree); Override;
    Destructor Destroy; Override;
  Published
    Property Options;
  End;

  TtiVTListView = Class(TCustomVirtualStringTree)
  Private
    FRuntimeFiltering : Boolean;
    FFiltered : Boolean;
    FIsEditable : Boolean;
    FHeaderPopup : TVTHeaderPopUpMenu;
    FPopUpMenu : TPopUpMenu;
    FFilters : TList;
    FOnEditItem : TtiLVItemEditEvent;
    FOnInsertItem : TtiLVItemEditEvent;
    FOnDeleteItem : TtiLVItemEditEvent;
    FOnFilterData : TtiLVOnFilterDataEvent;
    Function ItemPassesFilter(pObject : TPersistent) : Boolean;
    Function GetFilteredDataCount : Integer;
    Procedure SetHeader(Const Value : TtiVTHeader);
    Function GetHeader : TtiVtHeader;
    Procedure PopulateListView;
    Function GetSelectedItem : TPersistent;
    Procedure InsertClick(Sender : TObject);
    Procedure EditClick(Sender : TObject);
    Procedure DeleteClick(Sender : TObject);
    Procedure FilterClick(Sender : TObject);
    Procedure ExportItemClick(Sender : TObject);
    Procedure OnMenuPopup(Sender : TObject);
    Procedure AddPopupItems;
    Function GetCanDelete : Boolean;
    Function GetCanInsert : Boolean;
    Function GetCanEdit : Boolean;
    Function GetVisibleColumnCount : Integer;
    Function GetItemCount : Integer;
  Protected
    FObjectList : TPerObjList;
    Procedure SetObjectList(Const Value : TPerObjList);
    Function GetOptionsClass : TTreeOptionsClass; Override;
    Procedure VTGetText(Sender : TBaseVirtualTree;
      Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
      Var CellText : WideString);
    Procedure VTCompareNodes(Sender : TBaseVirtualTree; Node1,
      Node2 : PVirtualNode; Column : TColumnIndex; Var Result : Integer);
    Procedure VTHeaderClick(Sender : TVTHeader; Column : TColumnIndex;
      Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Function GetHeaderClass : TVTHeaderClass; Override;
    Function GetColumnClass : TVirtualTreeColumnClass; Override;
    Procedure SetFiltered(Value : Boolean);
    Procedure SetIsEditable(Value : Boolean);
    Function GetDerivedValue(pListColumn : TtiVTListColumn; pData : TPersistent) : String;
  Public
    Procedure DoInsert;
    Procedure DoEditItem;
    Procedure DoDelete;
    Procedure DoFilter;
    Procedure ClearFilters;
    Procedure Refresh;
    Function ObjectFromNode(pNode : PVirtualNode) : TObject;
    Procedure ObjectToNode(pNode : PVirtualNode; pObj : TObject);
    Function GetRowAsString(pNode : PVirtualNode; Const pColStartDelimiter, pColEndDelimiter : String) : String;
    Procedure DeleteItem(pData : TPersistent);
    Constructor Create(Owner : TComponent); Override;
    Destructor Destroy; Override;
    Property Canvas;
    Property ObjectList : TPerObjList Read FObjectList Write SetObjectList;
    Property Filters : TList Read FFilters Write FFilters;
    Property ItemCount : Integer Read GetItemCount;
    Property FilteredDataCount : Integer Read GetFilteredDataCount;
    Property SelectedItem : TPersistent Read GetSelectedItem;
    Property CanDelete : Boolean Read GetCanDelete;
    Property CanInsert : Boolean Read GetCanInsert;
    Property CanEditItem : Boolean Read GetCanEdit;
    Property VisibleColumnCount : Integer Read GetVisibleColumnCount;
  Published
    Property Action;
    Property Align;
    Property Alignment;
    Property Anchors;
    Property AnimationDuration;
    Property AutoExpandDelay;
    Property AutoScrollDelay;
    Property AutoScrollInterval;
    Property Background;
    Property BackgroundOffsetX;
    Property BackgroundOffsetY;
    Property BiDiMode;
    Property BevelEdges;
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;
    Property BorderStyle;
    Property ButtonFillMode;
    Property ButtonStyle;
    Property BorderWidth;
    Property ChangeDelay;
    Property CheckImageKind;
    Property ClipboardFormats;
    Property Color;
    Property Colors;
    Property Constraints;
    Property Ctl3D;
    Property CustomCheckImages;
    Property DefaultNodeHeight;
    Property DefaultPasteMode;
    Property DefaultText;
    Property DragCursor;
    Property DragHeight;
    Property DragKind;
    Property DragImageKind;
    Property DragMode;
    Property DragOperations;
    Property DragType;
    Property DragWidth;
    Property DrawSelectionMode;
    Property EditDelay;
    Property Enabled;
    Property Filtered : Boolean Read FFiltered Write SetFiltered;
    Property Font;
    Property Header : TtiVTHeader Read GetHeader Write SetHeader;
    Property HintAnimation;
    Property HintMode;
    Property HotCursor;
    Property Images;
    Property IncrementalSearch;
    Property IncrementalSearchDirection;
    Property IncrementalSearchStart;
    Property IncrementalSearchTimeout;
    Property Indent;
    Property IsEditable : Boolean Read FIsEditable Write SetIsEditable;
    Property LineMode;
    Property LineStyle;
    Property Margin;
    Property NodeAlignment;
    Property NodeDataSize;
    Property ParentBiDiMode;
    Property ParentColor Default False;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property RootNodeCount;
    Property RunTimeFiltering : Boolean Read FRuntimeFiltering Write FRuntimeFiltering Default True;
    Property ScrollBarOptions;
    Property SelectionBlendFactor;
    Property SelectionCurveRadius;
    Property ShowHint;
    Property StateImages;
    Property TabOrder;
    Property TabStop Default True;
    Property TextMargin;
    Property TreeOptions;
    Property Visible;
    Property WantTabs;

    Property OnAdvancedHeaderDraw;
    Property OnAfterCellPaint;
    Property OnAfterItemErase;
    Property OnAfterItemPaint;
    Property OnAfterPaint;
    Property OnBeforeCellPaint;
    Property OnBeforeItemErase;
    Property OnBeforeItemPaint;
    Property OnBeforePaint;
    Property OnChange;
    Property OnChecked;
    Property OnChecking;
    Property OnClick;
    Property OnCollapsed;
    Property OnCollapsing;
    Property OnColumnClick;
    Property OnColumnDblClick;
    Property OnColumnResize;
    Property OnCompareNodes;
{$IFDEF COMPILER_5_UP}
    Property OnContextPopup;
{$ENDIF COMPILER_5_UP}
    Property OnCreateDataObject;
    Property OnCreateDragManager;
    Property OnCreateEditor;
    Property OnDblClick;
    Property OnDragAllowed;
    Property OnDragOver;
    Property OnDragDrop;
    Property OnEditCancelled;
    Property OnEdited;
    Property OnEditing;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnExpanded;
    Property OnExpanding;
    Property OnFilterData : TtiLVOnFilterDataEvent Read FOnFilterData Write FOnFilterData;
    Property OnFocusChanged;
    Property OnFocusChanging;
    Property OnFreeNode;
    Property OnGetCursor;
    Property OnGetHeaderCursor;
    //Property OnGetText;
    Property OnPaintText;
    Property OnGetHelpContext;
    Property OnGetImageIndex;
    Property OnGetHint;
    Property OnGetLineStyle;
    Property OnGetNodeDataSize;
    Property OnGetPopupMenu;
    Property OnGetUserClipboardFormats;
    Property OnHeaderClick;
    Property OnHeaderDblClick;
    Property OnHeaderDragged;
    Property OnHeaderDraggedOut;
    Property OnHeaderDragging;
    Property OnHeaderDraw;
    Property OnHeaderDrawQueryElements;
    Property OnHeaderMouseDown;
    Property OnHeaderMouseMove;
    Property OnHeaderMouseUp;
    Property OnHotChange;
    Property OnIncrementalSearch;
    Property OnInitChildren;
    Property OnInitNode;
    Property OnItemDelete : TtiLVItemEditEvent Read FOnDeleteItem Write FOnDeleteItem;
    Property OnItemEdit : TtiLVItemEditEvent Read FOnEditItem Write FOnEditItem;
    Property OnItemInsert : TtiLVItemEditEvent Read FOnInsertItem Write FOnInsertItem;
    Property OnKeyAction;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoadNode;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnNewText;
    Property OnNodeCopied;
    Property OnNodeCopying;
    Property OnNodeMoved;
    Property OnNodeMoving;
    Property OnPaintBackground;
    Property OnRenderOLEData;
    Property OnResetNode;
    Property OnResize;
    Property OnSaveNode;
    Property OnScroll;
    Property OnShortenString;
    Property OnStartDock;
    Property OnStartDrag;
    Property OnStateChange;
    Property OnStructureChange;
    Property OnUpdating;
  End;

Implementation

Uses Dialogs, tiLog, SysUtils, tiVTFilter, tiVTExportFactory, tiVTExportAbs;

{ TtiVTListView }

Procedure TtiVTListView.AddPopupItems;
Var
  I : Integer;
  lMenuItem : TMenuItem;
  lSubItem : TMenuItem;
Begin
  With FPopUpMenu Do
  Begin
    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := 'Insert New Item';
    lMenuItem.Tag := 1;
    lMenuItem.OnClick := InsertClick;
    Items.Add(lMenuItem);

    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := 'Edit This Item';
    lMenuItem.OnClick := EditClick;
    lMenuItem.Tag := 2;
    Items.Add(lMenuItem);

    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := 'Delete This Item';
    lMenuItem.OnClick := DeleteClick;
    lMenuItem.Tag := 3;
    Items.Add(lMenuItem);

    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := '&Filter Data';
    lMenuItem.OnClick := FilterClick;
    lMenuItem.Tag := 4;
    Items.Add(lMenuItem);

    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := 'E&xport';
    lMenuItem.Tag := 5;
    Items.Add(lMenuItem);
    For I := 0 To FactoryExport.RegisteredExports.Count - 1 Do
    Begin
      lSubItem := TMenuItem.Create(Self);
      lSubItem.Caption := FactoryExport.RegisteredExports.Strings[I];
      lSubItem.OnClick := ExportItemClick;
      lMenuItem.Add(lSubItem);
    End; { Loop }
  End;
End;

Procedure TtiVTListView.ClearFilters;
Begin
  FFilters.Clear;
End;

Constructor TtiVTListView.Create;
Begin
  Inherited;
  FOnFilterData := Nil;
  Indent := 0;
  FRuntimeFiltering := True;
  FFilters := TList.Create;
  FPopupMenu := TPopupMenu.Create(Self);
  FPopUpMenu.OnPopup := OnMenuPopUp;
  AddPopUpItems;
  FFiltered := False;
  FHeaderPopUp := TVTHeaderPopupMenu.Create(Self);
  Header.Options := [hoVisible, hoColumnResize, hoShowSortGlyphs, hoVisible, hoHotTrack];
  Header.Style := hsXPStyle;
  Header.PopupMenu := FHeaderPopUp;
  With TStringTreeOptions(TreeOptions) Do
  Begin
    PaintOptions := [toHideFocusRect, toShowButtons, toShowDropMark,
      toShowRoot, toThemeAware, toUseBlendedImages];
    SelectionOptions := SelectionOptions + [toFullRowSelect];
  End;
  PopUpMenu := FPopupMenu;
  OnGetText := VTGetText;
  OnCompareNodes := VTCompareNodes;
  OnHeaderClick := VTHeaderClick;
End;

Procedure TtiVTListView.DeleteClick(Sender : TObject);
Begin
  DoDelete;
End;

Procedure TtiVTListView.DeleteItem(pData : TPersistent);
Var
  lPos : Integer;
Begin
  If Assigned(pData) Then
  Begin
    lPos := FObjectList.IndexOf(pData);
    FObjectList.Delete(lPos);
    Refresh;
  End;
End;

Destructor TtiVTListView.Destroy;
Begin
  FHeaderPopUp.Free;
  FPopUpMenu.Free;
  FFilters.Free;
  Inherited;
End;

Procedure TtiVTListView.DoDelete;
Begin
  If Assigned(FOnDeleteItem) And CanDelete Then
    FOnDeleteItem(SelectedItem);
End;

Procedure TtiVTListView.DoEditItem;
Begin
  If Assigned(FOnEditItem) And CanEditItem Then
    FOnEditItem(SelectedItem);
End;

Procedure TtiVTListView.DoFilter;
Var
  lObject : TPersistent;
Begin
  lObject := FObjectList.First;
  If TfrmLVFilter.EditFilter(lObject, FFilters) Then
  Begin
    FFiltered := True;
    Refresh;
  End;
End;

Procedure TtiVTListView.DoInsert;
Begin
  If Assigned(FOnInsertItem) And CanInsert Then
    FOnInsertItem(Nil);
End;

Procedure TtiVTListView.EditClick(Sender : TObject);
Begin
  DoEditItem;
End;

Procedure TtiVTListView.ExportItemClick(Sender : TObject);
Var
  lExport : TtiVTExportAbs;
  lExportName : String;
Begin
  lExportName := (Sender As TMenuItem).Caption;
  Delete(lExportName, Pos('&', lExportName), 1);
  lExport := FactoryExport.GetExport(lExportName);
  Try
    lExport.SourceListView := Self;
    lExport.Execute;
  Finally
    lExport.Free;
  End;
End;

Procedure TtiVTListView.FilterClick(Sender : TObject);
Begin
  DoFilter;
End;

Function TtiVTListView.GetCanDelete : Boolean;
Begin
  Result := FIsEditable And (SelectedItem <> Nil);
End;

Function TtiVTListView.GetCanEdit : Boolean;
Begin
  Result := FIsEditable And (SelectedItem <> Nil);
End;

Function TtiVTListView.GetCanInsert : Boolean;
Begin
  Result := FIsEditable;
End;

Function TtiVTListView.GetColumnClass : TVirtualTreeColumnClass;
Begin
  Result := TtiVTListColumn;
End;

Function TtiVTListView.GetDerivedValue(pListColumn : TtiVTListColumn; pData : TPersistent) : String;
Begin
  Try
    If Assigned(pListColumn.OnDeriveColumn) Then
    Begin
      Result := '';
      pListColumn.OnDeriveColumn(pData, pListColumn, Result);
    End
    Else
      Result := 'Unknown';
  Except
    On E : Exception Do
    Begin
      Result := 'Error in deriving column "' + pListColumn.Text + '" :- ' + E.Message;
    End;
  End;
End;

Function TtiVTListView.GetFilteredDataCount : Integer;
Var
  I : Integer;
  lFilterCount : Integer;
Begin
  If Assigned(FOnFilterData) Or FFiltered Then
  Begin
    lFilterCount := 0;
    For I := 0 To FObjectList.Count - 1 Do
    Begin
      If ItemPassesFilter(FObjectList.Items[I]) Then
        Inc(lFilterCount);
    End; { Loop }
    Result := lFilterCount;
  End
  Else
    Result := GetItemCount;
End;

Function TtiVTListView.GetHeader : TtiVtHeader;
Begin
  Result := TtiVTHeader(Inherited Header);
End;

Function TtiVTListView.GetHeaderClass : TVTHeaderClass;
Begin
  Result := TtiVTHeader;
End;

Function TtiVTListView.GetItemCount : Integer;
Begin
  If Assigned(FObjectList) Then
    Result := FObjectList.Count
  Else
    Result := 0;
End;

Function TtiVTListView.GetOptionsClass : TTreeOptionsClass;
Begin
  Result := TStringTreeOptions;
End;

Function TtiVTListView.GetRowAsString(pNode : PVirtualNode; Const pColStartDelimiter, pColEndDelimiter : String) : String;
Var
  I : Integer;
  lCellText : WideString;
Begin
  Result := '';
  lCellText := '';
  For I := 0 To TtiVTHeader(FHeader).FColumns.Count - 1 Do
  Begin
    VTGetText(Self, pNode, I, ttNormal, lCellText);
    If (coVisible In FHeader.Columns.Items[I].Options) Then
      Result := Result + pColStartDelimiter + lCellText + pColEndDelimiter;
  End; { Loop }
End;

Function TtiVTListView.GetSelectedItem : TPersistent;
Begin
  If (FocusedNode <> Nil) Then
    Result := TPersistent(ObjectFromNode(FocusedNode))
  Else
    Result := Nil;
End;

Function TtiVTListView.GetVisibleColumnCount : Integer;
Var
  I : Integer;
  lColumn : TColumnIndex;
Begin
  I := 0;
  lColumn := Header.Columns.GetFirstVisibleColumn;
  While (lColumn > InvalidColumn) Do
  Begin
    Inc(I);
    lColumn := Header.Columns.GetNextVisibleColumn(lColumn);
  End;
  Result := I;
End;

Procedure TtiVTListView.InsertClick(Sender : TObject);
Begin
  DoInsert;
End;

Function TtiVTListView.ItemPassesFilter(pObject : TPersistent) : Boolean;
Var
  I : Integer;
  lResult : Boolean;
  lFilter : TLVFilter;
  lPrevConj : TFilterConj;
Begin
  If FFiltered Then
  Begin
    If Assigned(FOnFilterData) Then
    Begin
      FOnFilterData(pObject, lResult);
      Result := lResult;
    End
    Else
      Result := True;
    If Result Then // No point checking if it doesn't pass the first stage.
    Begin
      lPrevConj := fcAnd; // We need to AND it with any Filter event result.
      For I := 0 To FFilters.Count - 1 Do
      Begin
        lFilter := TLVFilter(FFilters.Items[I]);
        Case lPrevConj Of
          fcOr : Result := Result Or lFilter.PassesFilter(pObject);
          fcAnd : Result := Result And lFilter.PassesFilter(pObject);
          fcNone : Break;
        End; { Case }
        lPrevConj := lFilter.Join;
      End; { Loop }
    End;
  End
  Else
    Result := True;
End;

Function TtiVTListView.ObjectFromNode(pNode : PVirtualNode) : TObject;
Var
  lData : Pointer;
Begin
  //Assert(pNode <> Nil);
  lData := GetNodeData(pNode);
  If lData = Nil Then
    Result := Nil
  Else
    Result := TObject(lData^);
End;

Procedure TtiVTListView.ObjectToNode(pNode : PVirtualNode; pObj : TObject);
Var
  lData : Pointer;
Begin
  Assert(pNode <> Nil);
  lData := GetNodeData(pNode);
  Assert(lData <> Nil);
  TObject(lData^) := pObj;
End;

Procedure TtiVTListView.OnMenuPopup(Sender : TObject);
Var
  I : Integer;
Begin
  With FPopupMenu Do
  Begin
    For I := 0 To Items.Count - 1 Do
    Begin
      Case Items[I].Tag Of
        1 : Items[I].Visible := CanInsert;
        2 : Items[I].Visible := CanEditItem;
        3 : Items[I].Visible := CanDelete;
        4 : Items[I].Visible := FRunTimeFiltering And (GetItemCount > 0);
      End; { Case }
    End; { Loop }
  End;
End;

Procedure TtiVTListView.PopulateListView;
Var
  lCount : Integer;
  lIndex : Integer;
  Node : PVirtualNode;
Begin
  lCount := GetFilteredDataCount;
  lIndex := 0;
  If (lCount <> 0) Then
    NodeDataSize := SizeOf(FObjectList.Items[0])
  Else
    NodeDataSize := SizeOf(TPerObjAbs);
  RootNodeCount := lCount;
  BeginUpdate;
  Try
    Node := GetFirst;
    While (Node <> Nil) Do
    Begin
      If Not FFiltered Or ItemPassesFilter(FObjectList.Items[lIndex]) Then
      Begin
        ObjectToNode(Node, FObjectList.Items[lIndex]);
        Node := Node.NextSibling;
      End;
      Inc(lIndex);
    End;
  Finally
    EndUpdate;
  End;
End;

Procedure TtiVTListView.Refresh;
Begin
  PopulateListView;
  With Header Do
  Begin
    If (SortColumn > NoColumn) Then
      SortTree(SortColumn, SortDirection, False);
  End;
End;

Procedure TtiVTListView.SetFiltered(Value : Boolean);
Begin
  FFiltered := Value;
 // Refresh;
End;

Procedure TtiVTListView.SetHeader(Const Value : TtiVTHeader);
Begin
  Inherited Header.Assign(Value);
End;

Procedure TtiVTListView.SetIsEditable(Value : Boolean);
Begin
  FIsEditable := Value;
End;

Procedure TtiVTListView.SetObjectList(Const Value : TPerObjList);
Begin
  FObjectList := Value;
  PopulateListView;
End;

Procedure TtiVTListView.VTCompareNodes(Sender : TBaseVirtualTree; Node1,
  Node2 : PVirtualNode; Column : TColumnIndex; Var Result : Integer);
Var
  lObject1,
    lObject2 : TObject;
  lColumn : TtiVTListColumn;
  lValue1,
    lValue2 : Variant;
Begin
  lColumn := TtiVTListColumn(Header.Columns[Column]);
  lObject1 := ObjectFromNode(Node1);
  lObject2 := ObjectFromNode(Node2);
  If lColumn.Derived Then
  Begin
    lValue1 := GetDerivedValue(lColumn, TPersistent(lObject1));
    lValue2 := GetDerivedValue(lColumn, TPersistent(lObject2));
  End
  Else
  Begin
    lValue1 := GetPropValue(lObject1, lColumn.FFieldName);
    lValue2 := GetPropValue(lObject2, lColumn.FFieldName);
  End;
  Try
    If (lValue1 < lValue2) Then
      Result := -1
    Else If (lValue1 > lValue2) Then
      Result := 1
    Else
      Result := 0;
  Except
    Raise Exception.Create('Internal Sort Error');
  End;
End;

Procedure TtiVTListView.VTGetText(Sender : TBaseVirtualTree;
  Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
  Var CellText : WideString);
Var
  lObject : TObject;
  lPropName : String;
  lCurrency : Currency;
  lFloat : Real;
  lInt : Integer;
  lDate : TDateTime;
  lsDisplayMask : String;
  lColumn : TtiVTListColumn;
Begin
  CellText := 'Undefined....';
  If (FHeader = Nil) Then
    Exit;
  If ((Column < TtiVTHeader(FHeader).FColumns.Count) And (Column <> -1)) Then
  Begin
    lColumn := TtiVTListColumn(Header.Columns[Column]);
    If lColumn.Visible Then
    Begin
      lObject := ObjectFromNode(Node);
      If lColumn.Derived Then
        CellText := GetDerivedValue(lColumn, TPersistent(lObject))
      Else
      Begin
        lPropName := lColumn.FieldName;
        If (Trim(lPropName) <> '') Then
        Begin
          Try
            lsDisplayMask := lColumn.DisplayMask;
            Case lColumn.FFieldType Of
              lvtkString : CellText := GetPropValue(lObject, lPropName);
              lvtkCurrency :
                Begin
                  lCurrency := GetPropValue(lObject, lPropName);
                  CellText := Format('%m', [lCurrency]);
                End;
              lvtkFloat :
                Begin
                  lFloat := GetPropValue(lObject, lPropName);
                  CellText := FormatFloat(lsDisplayMask, lFloat);
                End;
              lvtkDateTime :
                Begin
                  lDate := GetPropValue(lObject, lPropName);
                  CellText := FormatDateTime(lsDisplayMask, lDate);
                End;
              lvtkInt :
                Begin
                  lInt := GetPropValue(lObject, lPropName);
                  CellText := FormatFloat(lsDisplayMask, lInt);
                End;
            End; { Case }
          Except
            On E : Exception Do
            Begin
              CellText := Format('Error %s getting property %s for Object %s.', [E.Message, lPropName, lObject.ClassName]);
            End;
          End;
        End;
      End;
    End;
  End;
End;

Procedure TtiVTListView.VTHeaderClick(Sender : TVTHeader; Column : TColumnIndex;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Begin
  If (Button = mbLeft) Then
  Begin
    With Sender, Treeview Do
    Begin
      If SortColumn > NoColumn Then
        Columns[SortColumn].Options := Columns[SortColumn].Options + [coParentColor];
      If (SortColumn = NoColumn) Or (SortColumn <> Column) Then
      Begin
        SortColumn := Column;
        SortDirection := sdAscending;
      End
      Else If SortDirection = sdAscending Then
        SortDirection := sdDescending
      Else
        SortDirection := sdAscending;
      Columns[SortColumn].Color := $F7F7F7;
      SortTree(SortColumn, SortDirection, False);
    End;
  End;
End;

{ TtiVTHeader }

Constructor TtiVTHeader.Create(AOwner : TBaseVirtualTree);
Begin
  Inherited;
End;

Destructor TtiVTHeader.Destroy;
Begin
  Inherited;
End;

{ TtiVTListColumn }

Constructor TtiVTListColumn.Create(Collection : TCollection);
Begin
  Inherited;
  FFieldName := 'PropertyName';
  FFieldType := lvtkString;
  FVisible := True;
End;

{ TLVFilter }

Constructor TLVFilter.Create;
Begin
  Inherited;
  Join := fcNone;
  Operator := foNone;
  Value := '';
  PropName := '';
End;

Function TLVFilter.FloatPropPasses(pValue : TPersistent) : Boolean;
Var
  lsProp : String;
  lrProp : Extended;
  lrVal : Extended;
  lRetCode : Integer;
  lValidNum : Boolean;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from FloatPropPasses', [FFieldName, E.Message]);
  End;

  lrProp := StrToFloat(lsProp);

  Val(Value, lrVal, lRetCode);
  lValidNum := (lRetCode = 0);
  If Not lValidNum Then
  Begin
    Try
      lrVal := StrToDateTime(Value);
      lValidNum := True;
    Except
      lValidNum := False;
    End;
  End;
  If lValidNum Then
  Begin
    Case Operator Of
      foEqual : Result := (lrProp = lrVal);
      foNotEqual : Result := (lrProp <> lrVal);
      foGreater : Result := (lrProp > lrVal);
      foGreaterEqual : Result := (lrProp >= lrVal);
      foLess : Result := (lrProp < lrVal);
      foLessEqual : Result := (lrProp <= lrVal);
    Else
      Raise Exception.Create('Invalid operator passed to FloatPropPasses.');
    End;
  End
  Else
    Result := False;
End;

Function TLVFilter.GetSimplePropType(pPersistent : TPersistent; pPropName : String) : TtiLVTypeKind;
Var
  lPropType : TTypeKind;
Begin
  Try
    lPropType := PropType(pPersistent, pPropName);
  Except
    On E : Exception Do
      Raise Exception.Create('Error in tiGetSimpleTypeKind ' + 'Message: ' + E.Message);
  End;
  Case lPropType Of
    tkInteger,
      tkInt64,
      tkEnumeration : Result := tiTKInteger;
    tkFloat : Result := tiTKFloat;
    tkString,
      tkChar,
      tkWChar,
      tkLString,
      tkWString : Result := tiTKString;
  Else
    Raise Exception.Create('Invalid property type passed to ' +
      'tiGetSimpleTypeKind');
  End;
End;

Function TLVFilter.GetStringExpression : String;
Begin
  Result := Format('%s %s "%s" %s', [FFieldName, FilterOps[Ord(FOperator)], FValue, FilterConjs[Ord(FJoin)]]);
End;

Function TLVFilter.IntPropPasses(pValue : TPersistent) : Boolean;
Var
  lsProp : String;
  liProp : Integer;
  liVal : Integer;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from IntPropPasses', [FFieldName, E.Message]);
  End;
  Try
    liProp := StrToInt(lsProp);
  Except
    liProp := 0
  End;
  Try
    liVal := StrToInt(Value);
  Except
    liVal := 0
  End;
  Case Operator Of
    foEqual : Result := (liProp = liVal);
    foNotEqual : Result := (liProp <> liVal);
    foGreater : Result := (liProp > liVal);
    foGreaterEqual : Result := (liProp >= liVal);
    foLess : Result := (liProp < liVal);
    foLessEqual : Result := (liProp <= liVal);
  Else
    Raise Exception.Create('Invalid operator passed to IntPropPasses');
  End;
End;

Function TLVFilter.PassesFilter(pData : TPersistent) : Boolean;
Var
  lPropType : TtiLVTypeKind;
Begin
  lPropType := GetSimplePropType(pData, FFieldName);
  Case lPropType Of
    tiTKInteger : Result := IntPropPasses(pData);
    tiTKFloat : Result := FloatPropPasses(pData);
    tiTKString : Result := StringPropPasses(pData);
  Else
    Raise Exception.Create('Invalid property type passed to TVFilter.PassesFilter');
  End;
End;

Function TLVFilter.StringPropPasses(pValue : TPersistent) : Boolean;
Var
  lsProp : String;
  lsVal : String;
Begin
  Try
    lsProp := UpperCase(GetPropValue(pValue, FFieldName));
  Except
    On E : Exception Do
      Raise Exception.CreateFmt('Error reading property <%s> %s. Called from StringPropPasses', [FFieldName, E.Message]);
  End;
  lsVal := UpperCase(Value);
  Case Operator Of
    foEqual : Result := (lsProp = lsVal);
    foNotEqual : Result := (lsProp <> lsVal);
    foGreater : Result := (lsProp > lsVal);
    foGreaterEqual : Result := (lsProp >= lsVal);
    foLess : Result := (lsProp < lsVal);
    foLessEqual : Result := (lsProp <= lsVal);
    foContains : Result := WildcardMatch(lsProp, '*' + lsVal + '*', True);
    foNotContains : Result := (Not WildcardMatch(lsProp, '*' + lsVal + '*', True));
  Else
    Raise Exception.Create('Invalid operator passed to TestStringFilter');
  End;
End;

Function FindPart(Const pHelpWilds, pInputStr : String) : Integer;
Var
  I, J : Integer;
  Diff : Integer;
Begin
  I := Pos('?', pHelpWilds);
  If I = 0 Then
  Begin
    { if no '?' in pHelpWilds }
    Result := Pos(pHelpWilds, pInputStr);
    Exit;
  End;
  { '?' in pHelpWilds }
  Diff := Length(pInputStr) - Length(pHelpWilds);
  If Diff < 0 Then
  Begin
    Result := 0;
    Exit;
  End;
  { now move pHelpWilds over pInputStr }
  For I := 0 To Diff Do
  Begin
    For J := 1 To Length(pHelpWilds) Do
    Begin
      If (pInputStr[I + J] = pHelpWilds[J]) Or
        (pHelpWilds[J] = '?') Then
      Begin
        If J = Length(pHelpWilds) Then
        Begin
          Result := I + 1;
          Exit;
        End;
      End
      Else
        Break;
    End;
  End;
  Result := 0;
End;

Function TLVFilter.WildCardMatch(pInputStr, pWilds : String; pIgnoreCase : Boolean) : Boolean;

  Function SearchNext(Var pWilds : String) : Integer;
 { looking for next *, returns position and string until position }
  Begin
    Result := Pos('*', pWilds);
    If Result > 0 Then
      pWilds := Copy(pWilds, 1, Result - 1);
  End;

Var
  CWild, CInputWord : Integer; { counter for positions }
  I, LenHelpWilds : Integer;
  MaxInputWord, MaxWilds : Integer; { Length of pInputStr and pWilds }
  HelpWilds : String;
Begin
  If pWilds = pInputStr Then
  Begin
    Result := True;
    Exit;
  End;
  Repeat { delete '**', because '**' = '*' }
    I := Pos('**', pWilds);
    If I > 0 Then
      pWilds := Copy(pWilds, 1, I - 1) + '*' + Copy(pWilds, I + 2, MaxInt);
  Until I = 0;
  If pWilds = '*' Then
  Begin { for fast end, if pWilds only '*' }
    Result := True;
    Exit;
  End;
  MaxInputWord := Length(pInputStr);
  MaxWilds := Length(pWilds);
  If pIgnoreCase Then
  Begin { upcase all letters }
    pInputStr := AnsiUpperCase(pInputStr);
    pWilds := AnsiUpperCase(pWilds);
  End;
  If (MaxWilds = 0) Or (MaxInputWord = 0) Then
  Begin
    Result := False;
    Exit;
  End;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  Repeat
    If pInputStr[CInputWord] = pWilds[CWild] Then
    Begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    End;
    If pWilds[CWild] = '?' Then
    Begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    End;
    If pWilds[CWild] = '*' Then
    Begin { handling of '*' }
      HelpWilds := Copy(pWilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      If I = 0 Then
      Begin
        { no '*' in the rest, compare the ends }
        If HelpWilds = '' Then
          Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        For I := 0 To LenHelpWilds - 1 Do
        Begin
          If (HelpWilds[LenHelpWilds - I] <> pInputStr[MaxInputWord - I]) And
            (HelpWilds[LenHelpWilds - I] <> '?') Then
          Begin
            Result := False;
            Exit;
          End;
        End; { Loop }
        Exit;
      End;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(pInputStr, CInputWord, MaxInt));
      If I = 0 Then
      Begin
        Result := False;
        Exit;
      End;
      CInputWord := I + LenHelpWilds;
      Continue;
    End;
    Result := False;
    Exit;
  Until (CInputWord > MaxInputWord) Or (CWild > MaxWilds);
  { no completed evaluation }
  If CInputWord <= MaxInputWord Then
    Result := False;
  If (CWild <= MaxWilds) And (pWilds[MaxWilds] <> '*') Then
    Result := False;
End;


End.
