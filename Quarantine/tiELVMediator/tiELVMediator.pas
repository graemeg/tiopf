Unit tiELVMediator;

{ Composite mediator for TEasyListView (http://www.mustangpeak.net)

  19th June 2009 - AD - Initial Version

}

Interface

Uses Classes, MPCommonObjects, EasyListview, tiObject, Contnrs, tiBaseMediator;

Type
  { Composite mediator for TEasyListView }
  TtiEasyListViewMediatorView = Class(TtiCustomListMediatorView)
  Private
    FObserversInTransit : TList;
    Function ItemCompare(Sender : TCustomEasyListview;
      Column : TEasyColumn; Group : TEasyGroup; Item1, Item2 : TEasyItem;
      Var DoDefault : Boolean) : Integer;
  Protected
    Function GetSelectedObject : TtiObject; Override;
    Procedure SetSelectedObject(Const AValue : TtiObject); Override;
    Procedure CreateColumns; Override;
    Function DoCreateItemMediator(AData : TtiObject; ARowIdx : Integer) : TtiListItemMediator; Override;
    Procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator); Override;
    Procedure SetupGUIandObject; Override;
    Procedure ClearList; Override;
    Procedure RebuildList; Override;
    Procedure SetActive(Const AValue : Boolean); Override;
  Public
    Function PropNameFromCaption(Const pCaption : String) : String;
    Function CaptionFromPropName(Const pPropName : String) : String;
    Constructor CreateCustom(AModel : TtiObjectList; AView : TEasyListView; ADisplayNames : String; AIsObserving : Boolean = True); Reintroduce; Overload;
    Constructor CreateCustom(AModel : TtiObjectList; AView : TEasyListView; AOnBeforeSetupField : TtiOnBeforeSetupField; ADisplayNames : String; AIsObserving : Boolean = True); Reintroduce; Overload;
    Class Function ComponentClass : TClass; Override;
    Class Function CompositeMediator : Boolean; Override;
    Function GetObjectFromItem(AItem : TEasyItem) : TtiObject;
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure HandleSelectionChanged; Override;
  Published
    function View : TEasyListview; reintroduce;
  End;

  TtiEasyItemMediator = Class(TtiListItemMediator)
  Private
    FView : TEasyItem;
    Procedure SetupFields; Virtual;
  Public
    Constructor CreateCustom(AModel : TtiObject; AView : TEasyItem; Const AFieldsInfo : TtiMediatorFieldInfoList; IsObserving : Boolean = True); Reintroduce; Overload;
    Constructor CreateCustom(AModel : TtiObject; AView : TEasyItem; AOnBeforeSetupField : TtiOnBeforeSetupField; Const AFieldsInfo : TtiMediatorFieldInfoList; IsObserving : Boolean = True); Reintroduce; Overload;
    Procedure Update(ASubject : TtiObject); Override;
  Published
    Property View : TEasyItem Read FView;
  End;


Implementation

Uses tiLog, Variants, SysUtils;

{ TtiEasyListViewMediatorView }

Function TtiEasyListViewMediatorView.CaptionFromPropName(Const pPropName : String) : String;
Var
  I : Integer;
Begin
  Result := '';
  For I := 0 To Pred(FFieldsInfo.Count) Do
  Begin
    If (FFieldsInfo.FieldInfo[I].PropName = pPropName) Then
    Begin
      Result := FFieldsInfo.FieldInfo[I].Caption;
      Break;
    End;
  End; { Loop }
End;

Procedure TtiEasyListViewMediatorView.ClearList;
Begin
  MediatorList.Clear;
  If View <> Nil Then
    View.Items.Clear;
End;

Class Function TtiEasyListViewMediatorView.ComponentClass : TClass;
Begin
  Result := TEasyListview;
End;

Class Function TtiEasyListViewMediatorView.CompositeMediator : Boolean;
Begin
  Result := True;
End;

Constructor TtiEasyListViewMediatorView.Create;
Begin
  Inherited Create;
  FObserversInTransit := TList.Create;
End;

Procedure TtiEasyListViewMediatorView.CreateColumns;
Var
  C : Integer;
  lColumn : TEasyColumn;
  lInfo : TtiMediatorFieldInfo;
Begin
  If (View.Header.Columns.Count <> FieldsInfo.Count) Then
    View.Header.Columns.Clear;
  If (View.Header.Columns.Count = 0) Then
  Begin
    For C := 0 To FieldsInfo.Count - 1 Do
    Begin
      lInfo := FieldsInfo[C];
      lColumn := View.Header.Columns.Add;
      lColumn.Caption := lInfo.Caption;
      lColumn.Width := lInfo.FieldWidth;
      lColumn.Alignment := lInfo.Alignment;
//      lColumn.AutoSortOnClick := True;
    End;
  End;
End;

Constructor TtiEasyListViewMediatorView.CreateCustom(AModel : TtiObjectList;
  AView : TEasyListView; AOnBeforeSetupField : TtiOnBeforeSetupField;
  ADisplayNames : String; AIsObserving : Boolean);
Begin
  Create; // don't forget this
  OnBeforeSetupField := AOnBeforeSetupField;
  DisplayNames := ADisplayNames; // Will call ParseDisplaynames.
  Subject := AModel;
  SetView(AView); // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving; // Will attach/Detach
End;

Constructor TtiEasyListViewMediatorView.CreateCustom(AModel : TtiObjectList;
  AView : TEasyListView; ADisplayNames : String; AIsObserving : Boolean);
Var
  p : TtiOnBeforeSetupField;
Begin
  p := Nil;
  CreateCustom(AModel, AView, p, ADisplayNames, AIsObserving);
End;

Destructor TtiEasyListViewMediatorView.Destroy;
Begin
  IsObserving := False;
 // FView := Nil;
  Inherited;
End;

Function TtiEasyListViewMediatorView.DoCreateItemMediator(AData : TtiObject; ARowIdx : Integer) : TtiListItemMediator;
Var
  lItem : TEasyItem;
  M : TtiEasyItemMediator;
Begin
  DataAndPropertyValid(AData);
  View.BeginUpdate;
  Try
    lItem := View.Items.Add;
    M := TtiEasyItemMediator.CreateCustom(AData, lItem, OnBeforeSetupField, FieldsInfo, Active);
    lItem.Data := M;
    MediatorList.Add(M);
    Result := M;
  Finally
    View.EndUpdate;
  End;
End;

Procedure TtiEasyListViewMediatorView.DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator);
Begin
  View.Items.Delete(TtiEasyItemMediator(AMediator).View.Index);
  Inherited DoDeleteItemMediator(AIndex, AMediator);
End;

{Function TtiEasyListViewMediatorView.GetGUIControl : TComponent;
Begin
  Result := FView;
End;}

Function TtiEasyListViewMediatorView.GetObjectFromItem(AItem : TEasyItem) : TtiObject;
Begin
  If (AItem = Nil) Or (AItem.Data = Nil) Then
    Result := Nil
  Else
    Result := TtiEasyItemMediator(AItem.Data).Model;
End;

Function TtiEasyListViewMediatorView.GetSelectedObject : TtiObject;
Begin
  If (View.Selection.Count = 0) Then
    Result := Nil
  Else
    Result := GetObjectFromItem(View.Selection.First);
End;

Procedure TtiEasyListViewMediatorView.HandleSelectionChanged;
Var
  I : Integer;
Begin
  If (View.Selection.Count = 0) Then
    SelectedObject := Nil
  Else
  Begin
    FObserversInTransit.Clear;
    { If an item is already selected, assign the item's List of observers to a
      temporary container. This is done so that the same observers can be
      assigned to the new item. }
    If Assigned(SelectedObject) Then
      FObserversInTransit.Assign(SelectedObject.ObserverList);

    // Assign Newly selected item to SelectedObject Obj.
    SelectedObject := TtiObject(View.Selection.First);

    { If an object was selected, copy the old item's observer List
      to the new item's observer List. }
    If FObserversInTransit.Count > 0 Then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    { Set the Observers Subject property to the selected object }
    For I := 0 To SelectedObject.ObserverList.Count - 1 Do
      TtiMediatorView(SelectedObject.ObserverList.Items[I]).Subject :=
        SelectedObject;

    // execute the NotifyObservers event to update the observers.
    SelectedObject.NotifyObservers;
  End;
End;

Function TtiEasyListViewMediatorView.ItemCompare(Sender : TCustomEasyListview;
  Column : TEasyColumn; Group : TEasyGroup; Item1, Item2 : TEasyItem;
  Var DoDefault : Boolean) : Integer;
Var
  lValue1 : Variant;
  lValue2 : Variant;
  lPropName : String;
Begin
  Result := 0;
  lPropName := PropNameFromCaption(Column.Caption);
  If Assigned(Item1.Data) And Assigned(Item2.Data) Then
  Begin
    lValue1 := TtiEasyItemMediator(Item1.Data).Model.PropValue[lPropName];
    lValue2 := TtiEasyItemMediator(Item2.Data).Model.PropValue[lPropName];
    If (lValue1 < lValue2) Then
    Begin
      If Column.SortDirection = esdAscending Then
        Result := -1
      Else
        Result := 1;
    End
    Else If (lValue1 > lValue2) Then
    Begin
      If Column.SortDirection = esdAscending Then
        Result := 1
      Else
        Result := -1;
    End;
    DoDefault := False;
  End;
End;

Function TtiEasyListViewMediatorView.PropNameFromCaption(Const pCaption : String) : String;
Var
  lFieldInfo : TtiMediatorFieldInfo;
Begin
  lFieldInfo := FFieldsInfo.FieldInfoByCaption(pCaption);
  If Assigned(lFieldInfo) Then
    Result := lFieldInfo.PropName
  Else
    Result := '';
End;

Procedure TtiEasyListViewMediatorView.RebuildList;
Begin
  If (View <> Nil) Then
  Begin
    View.BeginUpdate;
    Try
      CreateColumns;
      CreateSubMediators;
    Finally
      View.EndUpdate;
    End;
  End;
End;

Procedure TtiEasyListViewMediatorView.SetActive(Const AValue : Boolean);
Begin
  If Not AValue Then
    ClearList;
  Inherited SetActive(AValue);
End;

{Procedure TtiEasyListViewMediatorView.SetGUIControl(Const AValue : TComponent);
Begin
  FView := AValue As TEasyListview;
  Inherited SetGUIControl(AValue);
End;}

Procedure TtiEasyListViewMediatorView.SetSelectedObject(Const AValue : TtiObject);
Var
  I : Integer;
Begin
  For I := 0 To Pred(View.Items.Count) Do
  Begin
    If TtiObject(View.Items[I].Data) = AValue Then
    Begin
      View.Items[I].Selected := True;
      Exit; //==>
    End;
  End; { Loop }
End;

Procedure TtiEasyListViewMediatorView.SetupGUIandObject;
Begin
  View.Header.Columns.Clear;
  View.Items.Clear;
  View.View := elsReport;
  View.Header.Visible := True;
  View.Header.Draggable := True;
  View.DragManager.Enabled := True;
  View.Selection.FullRowSelect := True;
  View.Selection.FullItemPaint := True;
  View.Sort.AutoSort := True;
  View.OnItemCompare := ItemCompare;
End;

function TtiEasyListViewMediatorView.View: TEasyListview;
begin
  Result := TEasyListview(Inherited View);
end;

{ TtiEasyItemMediator }

Constructor TtiEasyItemMediator.CreateCustom(AModel : TtiObject;
  AView : TEasyItem; Const AFieldsInfo : TtiMediatorFieldInfoList;
  IsObserving : Boolean);
Var
  p : TtiOnBeforeSetupField;
Begin
  p := Nil;
  CreateCustom(AModel, AView, p, AFieldsInfo, IsObserving);
End;

Constructor TtiEasyItemMediator.CreateCustom(AModel : TtiObject;
  AView : TEasyItem; AOnBeforeSetupField : TtiOnBeforeSetupField;
  Const AFieldsInfo : TtiMediatorFieldInfoList; IsObserving : Boolean);
Begin
  Inherited Create;
  Model := AModel;
  FView := AView;
 // AView.Data := AModel;
  FFieldsInfo := AFieldsInfo;
  OnBeforeSetupField := AOnBeforeSetupField;
  SetupFields;
  Active := IsObserving; // Will attach
End;

Procedure TtiEasyItemMediator.SetupFields;
Var
  C : Integer;
  lInfo : TtiMediatorFieldInfo;
  lValue : String;
  lColumn : TEasyColumn;
  lLV : TEasyListView;
Begin
  C := 0;
  lLV := TEasyListView(View.OwnerListview);
  lColumn := lLV.Header.FirstColumnByPosition;
  While (lColumn <> Nil) Do
  Begin
    lInfo := FieldsInfo.FieldInfoByCaption(lColumn.Caption);
    If Assigned(lInfo) Then
    Begin
     // Log('Found field info for %s!',[lColumn.Caption]);
      lValue := Model.PropValue[lInfo.PropName];
      If Assigned(OnBeforeSetupField) Then
        OnBeforeSetupField(Model, lInfo.PropName, lValue);
//      Log('Setting Caption %d to %s.',[C, lValue]);
      FView.Captions[C] := lValue;
      Inc(C);
    End
    Else
      Log('Couldn''t find fieldinfo for %s', [lColumn.Caption]);
    lColumn := lLV.Header.NextColumnByPosition(lColumn);
  End;
End;

Procedure TtiEasyItemMediator.Update(ASubject : TtiObject);
Var
  C : Integer;
  lInfo : TtiMediatorFieldInfo;
  lValue : String;
  lColumn : TEasyColumn;
  lLV : TEasyListView;
Begin
  C := 0;
  lLV := TEasyListView(View.OwnerListview);
  lColumn := lLV.Header.FirstColumnByPosition;
  While (lColumn <> Nil) Do
  Begin
    lInfo := FieldsInfo.FieldInfoByCaption(lColumn.Caption);
    If Assigned(lInfo) Then
    Begin
      lValue := Model.PropValue[lInfo.PropName];
      If Assigned(OnBeforeSetupField) Then
        OnBeforeSetupField(Model, lInfo.PropName, lValue);
      View.Captions[C] := lValue;
      Inc(C);
    End
    Else
      Log('Couldn''t find field info for %s.', [lColumn.Caption]);
    lColumn := lLV.Header.NextColumnByPosition(lColumn);
  End;
End;

End.
