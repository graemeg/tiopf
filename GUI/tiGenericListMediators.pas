(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.

ToDo:
  * Unit tests
  * More refactoring
  * Implement a View Manager class, so we can remove the View Lists
    created in each Form using mediating views.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit tiGenericListMediators;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$I tiDefines.inc}

interface
uses
  tiObject
  ,tiGenericEditMediators
  ,StdCtrls     // TListBox, TComboBox
  ,ComCtrls     // TListView
  ,Controls     // TControl
  ,Menus        // TPopupMenu
  ,Classes
  ;


type
  { Used so we know what needs updating, the Internal List, or just the
    Selected Object. }
  TUpdateMode = (umSelectedObject, umObjectList);

  { Abstract class that observes a list object }

  { TListMediator }

  TListMediator = class(TtiObject)
  private
    FObjectList: TtiObjectList;
    FControl: TControl;
    FSelectedObject: TtiObject;
//    FPopupMenu: TPopupMenu;
    FShowDeleted: Boolean;
    procedure   SetSelectedObject(const Value: TtiObject);
//    procedure   BuildPopupMenu;
    procedure   SetShowDeleted(const Value: Boolean);
  protected
    FObserversInTransit: TList;
    FUpdateMode: TUpdateMode;
    function    GetModel: TtiObjectList; virtual;
    procedure   SetModel(const Value: TtiObjectList); virtual;
    function    GetView: TControl; virtual;
    procedure   SetView(const Value: TControl); virtual;
    procedure   RebuildList; virtual; abstract;
    { Used to setup things like the MaxLength of a edit box, etc. }
    procedure   SetupGUIandObject; virtual;
  public
    constructor Create; override;
    constructor CreateCustom(pObjectList: TtiObjectList; pView: TControl); virtual;
    destructor  Destroy; override;
    procedure   Update(pSubject: TtiObject); override;

    { Called from GUI to trigger events }
    procedure   HandleDeleteItem; virtual;
    procedure   HandleListChanged; virtual;
    procedure   HandleSelectionChanged; virtual; abstract;
    procedure   MenuItemAddClick(Sender: TObject); virtual;
    procedure   MenuItemEditClick(Sender: TObject); virtual;
    procedure   MenuItemDeleteClick(Sender: TObject); virtual;

    property    SelectedObject: TtiObject read FSelectedObject write SetSelectedObject;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
    property    Model: TtiObjectList read GetModel write SetModel;
    property    View: TControl read GetView;
  end;


  { Observes a list object - TListBox }

  TListBoxMediator = class(TListMediator)
  private
    OldPos: Integer;
    NewPos: Integer;
  protected
    function    GetView: TListBox; reintroduce;
    procedure   RebuildList; override;
    procedure   SaveBookmark;
    procedure   RestoreBookmark;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TListBox read GetView;
  end;


  { Observes a list object - TComboBox }

  TComboBoxMediator = class(TListMediator)
  protected
    function    GetView: TComboBox; reintroduce;
    procedure   RebuildList; override;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TComboBox read GetView;
  end;


  { Observes a list object - TListView }

  TListViewMediator = class(TListMediator)
  protected
    function    GetView: TListView; reintroduce;
    procedure   RebuildList; override;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TListView read GetView;
  end;


implementation
uses
  SysUtils
  ;


{ TListBoxMediator }

function TListBoxMediator.GetView: TListBox;
begin
  result := TListBox(inherited GetView);
end;


procedure TListBoxMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.ItemIndex = -1 then
    SelectedObject := nil
  else
  begin
    {if an item is already selected, assign the item's List of observers to a temporary container.
    This is done so that the same observers can be assigned to the new item.}
    if Assigned(SelectedObject) then //  and Assigned(SelectedObject.ObserverList)
      FObserversInTransit.Assign( SelectedObject.ObserverList);

    //Assign Newly selected item to SelectedObject Obj.
    SelectedObject := TtiObject(View.Items.Objects[View.ItemIndex]);

    {if an object was selected, copy the old item's observer List
    to the new item's observer List.}
    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    {set the observers to the object}
    for i := 0 to SelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    //execute the NotifyObservers event to update the observers.
    SelectedObject.NotifyObservers;
  end;
end;


procedure TListBoxMediator.RebuildList;
var
  i: Integer;
  ptr: TNotifyEvent;
  selected: integer;
begin
  selected := -1;
  if (Model.CountNotDeleted-1) >= View.ItemIndex then
    selected := View.ItemIndex;

  ptr := View.OnClick;
  View.OnClick := nil;
  View.Items.BeginUpdate;
  try
    View.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        View.Items.AddObject(Model.Items[i].Caption, Model.Items[i]);
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      if selected = -1 then
        selected := 0;
      View.ItemIndex := selected;
    end;
  finally
    View.Items.EndUpdate;
    View.OnClick := ptr;
    HandleSelectionChanged;
  end;
end;


procedure TListBoxMediator.RestoreBookmark;
begin
  if OldPos > Pred(View.Items.Count) then
    NewPos := Pred(View.Items.Count)
  else if OldPos = 0 then
    NewPos := 0
  else
    NewPos := OldPos - 1;
  View.ItemIndex := NewPos;
  HandleSelectionChanged;
end;


procedure TListBoxMediator.SaveBookmark;
begin
  OldPos := View.ItemIndex;
end;


{ TComboBoxMediator }

function TComboBoxMediator.GetView: TComboBox;
begin
  result := TComboBox(inherited GetView);
end;


procedure TComboBoxMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.ItemIndex = -1 then
    SelectedObject := nil
  else
  begin
    if Assigned(SelectedObject) then
      FObserversInTransit.Assign(SelectedObject.ObserverList);

    SelectedObject := TtiObject(View.Items.Objects[View.ItemIndex]);

    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    for i := 0 to SelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    SelectedObject.NotifyObservers;
  end;
end;


procedure TComboBoxMediator.RebuildList;
var
  i: Integer;
  ptr: TNotifyEvent;
  selected: integer;
begin
  selected := -1;
  if (Model.CountNotDeleted-1) >= View.ItemIndex then
    selected := View.ItemIndex;

  ptr := View.OnChange;
  View.OnChange := nil;
  View.Items.BeginUpdate;
  try
    View.Items.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        View.Items.AddObject( Model.Items[i].Caption, Model.Items[i] );
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      if selected = -1 then
        selected := 0;
      View.ItemIndex := selected;
    end;
  finally
    View.Items.EndUpdate;
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;


{ TListViewMediator }

function TListViewMediator.GetView: TListView;
begin
  result := TListView(inherited GetView);
end;


procedure TListViewMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if not Assigned(View.Selected) then
    SelectedObject := nil
  else
  begin
    if Assigned(SelectedObject) then  // and Assigned(SelectedObject.ObserverList)
      FObserversInTransit.Assign( SelectedObject.ObserverList);

    SelectedObject := TtiObject(View.Selected.Data);

    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    for i := 0 to SelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    SelectedObject.NotifyObservers;
  end;
end;


procedure TListViewMediator.RebuildList;
var
  i: Integer;
  lItem: TListItem;
  ptr: TLVChangeEvent;
begin
  ptr := View.OnChange;
  View.OnChange := nil;
  View.Items.BeginUpdate;
  try
    View.Items.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        lItem := View.Items.Add;
        lItem.Caption := Model.Items[i].Caption;
        lItem.Data := Model.Items[i];
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      SelectedObject := Model.Items[0];
      View.Selected := View.Items[0];
    end;
  finally
    {$IFDEF FPC}
      View.EndUpdate;
    {$ELSE}
      View.Items.EndUpdate;
    {$ENDIF}
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;


{ TListMediator }

//procedure TListMediator.BuildPopupMenu;
//var
//  lItem: TMenuItem;
//begin
//  FPopupMenu.Create(View);
//  lItem := TMenuItem.Create(FPopupMenu);
//  lItem.Caption := 'Add';
//  lItem.OnClick := {$IFDEF FPC}@{$ENDIF}MenuItemAddClick;
//
//  lItem := TMenuItem.Create(FPopupMenu);
//  lItem.Caption := 'Edit';
//  lItem.OnClick := {$IFDEF FPC}@{$ENDIF}MenuItemEditClick;
//
//  lItem := TMenuItem.Create(FPopupMenu);
//  lItem.Caption := 'Delete';
//  lItem.OnClick := {$IFDEF FPC}@{$ENDIF}MenuItemDeleteClick;
//end;


constructor TListMediator.Create;
begin
  inherited;
  FObserversInTransit := TList.Create;
  FShowDeleted  := False;
  { This is under construction. }
  FUpdateMode   := umObjectList;
end;


constructor TListMediator.CreateCustom(pObjectList: TtiObjectList; pView: TControl);
begin
  Create;
  Model := pObjectList;
  FControl := pView;
//  BuildPopupMenu;
  Model.AttachObserver( Self );
  SetupGUIandObject;
  Update(nil);
end;


destructor TListMediator.Destroy;
begin
  FObserversInTransit.Free;
  inherited;
end;


function TListMediator.GetModel: TtiObjectList;
begin
  Result := FObjectList;
end;


function TListMediator.GetView: TControl;
begin
  Result := FControl;
end;


procedure TListMediator.HandleDeleteItem;
begin
  if not Assigned(SelectedObject) then
    Exit; //==>

  BeginUpdate;
  try
    SelectedObject.Deleted := True;
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.HandleListChanged;
begin
  BeginUpdate;
  try
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.MenuItemAddClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.MenuItemDeleteClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.MenuItemEditClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.SetModel(const Value: TtiObjectList);
begin
  FObjectList := Value;
  if FObjectList.Count > 0 then
    SelectedObject := FObjectList.Items[0];
end;


procedure TListMediator.SetSelectedObject(const Value: TtiObject);
begin
  FSelectedObject := Value;
end;


procedure TListMediator.SetShowDeleted(const Value: Boolean);
begin
  BeginUpdate;
  try
    FShowDeleted := Value;
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.SetView(const Value: TControl);
begin
  FControl := Value;
end;


procedure TListMediator.SetupGUIandObject;
begin
  { Do nothing. Can be implemented in decendant classes. }
end;


procedure TListMediator.Update(pSubject: TtiObject);
begin
  BeginUpdate;
  try
//    inherited Update(pSubject);
    RebuildList;
  finally
    EndUpdate
  end;
end;


end.
