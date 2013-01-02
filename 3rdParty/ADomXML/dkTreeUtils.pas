unit dkTreeUtils;

{$I ADOMXMLWarn.inc}

// dkTreeUtils 1.0.6
// Delphi 4 to 2009 and Kylix 3 Implementation
// December 2007
//
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.1 (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
// "http://www.mozilla.org/MPL/"
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The Original Code is "dkTreeUtils.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2007 Dieter Köhler. All Rights Reserved.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU General Public License Version 2 or later (the "GPL"), in which case the
// provisions of the GPL are applicable instead of those above. If you wish to
// allow use of your version of this file only under the terms of the GPL, and
// not to allow others to use your version of this file under the terms of the
// MPL, indicate your decision by deleting the provisions above and replace them
// with the notice and other provisions required by the GPL. If you do not delete
// the provisions above, a recipient may use your version of this file under the
// terms of any one of the MPL or the GPL.

// HISTORY
//
// 2007-12-03 1.0.6 Made .NET compliant.
// 2005-07-09 1.0.5 TCustomOwnedObject.DoBeforeAttach added.
// 2005-01-28 1.0.4 Delphi 4 support added.  Thanks to Stefan Bellon.
// 2004-06-01 1.0.3 TCustomOwnedObject.InternalClear: bug fixed.  Thanks to Henner Kollmann.
// 2004-01-04 1.0.2 Small revisions.
// 2003-10-01 1.0.1 Bug fixes.
// 2003-08-03 1.0.0

{$IFDEF VER130}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER160}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER170}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER180}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER185}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER190}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER200}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER220}
  {$DEFINE VER130+}
{$ENDIF}
{$IFDEF VER230}
  {$DEFINE VER130+}
{$ENDIF}

interface

uses
{$IFDEF VER130+}
  Contnrs,
{$ENDIF}
  Classes, SysUtils;

type

  // Exception classes:
  EHierarchyRequestError = class(EListError);
  ENoModificationAllowedError = class(EListError);
  ENotAssignedError = class(EListError);
  ENotFoundError = class(EListError);
  EWrongOwnerError = class(EListError);

  TCustomOwnedObject = class(TPersistent)
  private
    FOwnedObjects: TList;
    FOwner: TCustomOwnedObject;
    procedure Attach(Obj: TCustomOwnedObject);
    procedure Detach(Obj: TCustomOwnedObject);
    function GetOwnedObject(Index: Integer): TCustomOwnedObject;
    function GetOwnedObjectsCount: Integer;
    procedure InternalClear;
  protected
    procedure Adopt(const Obj: TCustomOwnedObject); virtual;
    procedure Clear; virtual;
    procedure DoBeforeAttach(const Obj: TCustomOwnedObject); virtual;
    function GetOwner: TPersistent; override;
    property OwnedObject[Index: Integer]: TCustomOwnedObject read
      GetOwnedObject;
    property OwnedObjectsCount: Integer read GetOwnedObjectsCount;
  public
    constructor Create(const AOwner: TCustomOwnedObject);
    destructor Destroy; override;
  end;

  TCustomOwnedNode = class(TCustomOwnedObject)
  private
    FItems: TList;
    FNextSibling: TCustomOwnedNode;
    FParent: TCustomOwnedNode;
    FPreviousSibling: TCustomOwnedNode;
    FReadOnly: Boolean;
    function GetItem(Index: Integer): TCustomOwnedNode;
    procedure InternalAdopt(NewOwner: TCustomOwnedNode; Node: TCustomOwnedNode);
    procedure InternalClear;
    procedure InternalInsertBefore(NewChild,
      RefChild: TCustomOwnedNode);
    procedure InternalRemove(OldChild: TCustomOwnedNode);
  protected
    procedure Adopt(const Node: TCustomOwnedNode); reintroduce; virtual;
    function Append(const NewChild: TCustomOwnedNode): TCustomOwnedNode;
      virtual;
    procedure CheckAssigned(const Node: TCustomOwnedNode);
    procedure CheckDissimilarity(const Node_1, Node_2: TCustomOwnedNode);
    procedure CheckHasChild(const Node: TCustomOwnedNode);
    procedure CheckNotAncestorOrSelf(const Node: TCustomOwnedNode);
    procedure CheckNotReadOnly;
    procedure CheckParentNotReadOnly(const Node: TCustomOwnedNode);
    procedure CheckSameOwner(const Node: TCustomOwnedNode);
    procedure Clear; override;
    procedure DoAfterAddition(const Node: TCustomOwnedNode); virtual;
    procedure DoAfterClear; virtual;
    procedure DoAfterRemoval(const Node: TCustomOwnedNode); virtual;
    procedure DoBeforeAddition(const Node: TCustomOwnedNode); virtual;
    procedure DoBeforeClear; virtual;
    procedure DoBeforeRemoval(const Node: TCustomOwnedNode); virtual;
    function GetCount: Integer; virtual;
    function GetFirstChild: TCustomOwnedNode; virtual;
    function GetLastChild: TCustomOwnedNode; virtual;
    function GetNextSibling: TCustomOwnedNode; virtual;
    function GetParent: TCustomOwnedNode; virtual;
    function GetPreviousSibling: TCustomOwnedNode; virtual;
    function GetReadOnly: Boolean; virtual;
    function HasAsAncestor(Node: TCustomOwnedNode): Boolean; virtual;
    function HasChildren: Boolean; virtual;
    function IndexOf(Node: TCustomOwnedNode): Integer; virtual;
    function InsertBefore(const NewChild,
      RefChild: TCustomOwnedNode): TCustomOwnedNode; virtual;
    procedure RaiseException(const E: {$IFDEF CLR}ExceptionClass{$ELSE}ExceptClass{$ENDIF}); virtual;
    function Remove(const OldChild: TCustomOwnedNode): TCustomOwnedNode;
      virtual;
    function Replace(const NewChild,
      OldChild: TCustomOwnedNode): TCustomOwnedNode; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    property Count: Integer read GetCount;
    property FirstChild: TCustomOwnedNode read GetFirstChild;
    property Item[Index: Integer]: TCustomOwnedNode read GetItem; default;
    property Items: TList read FItems;
    property LastChild: TCustomOwnedNode read GetLastChild;
    property NextSibling: TCustomOwnedNode read GetNextSibling;
    property Parent: TCustomOwnedNode read GetParent;
    property PreviousSibling: TCustomOwnedNode read GetPreviousSibling;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  public
    constructor Create(const AOwner: TCustomOwnedObject);
    destructor Destroy; override;
  end;

resourcestring
  SHierarchyRequestError = 'Hierarchy Request Error';
  SListIndexError = 'List index out of bounds (%d)';
  SNoModificationAllowedError = 'No Modification Allowed Error';
  SNotAssignedError = 'Parameter Node Not Assigned Error';
  SNotFoundError = 'Node Not Found Error.';
  SWrongOwnerError = 'Wrong Owner Error';

implementation

{ TCustomOwnedObject }

constructor TCustomOwnedObject.Create(const AOwner: TCustomOwnedObject);
begin
  inherited Create;
  if Assigned(AOwner) then
    AOwner.Attach(Self);
end;

destructor TCustomOwnedObject.Destroy;
begin
  InternalClear;
  if Assigned(FOwner) then
    FOwner.Detach(Self);
  inherited;
end;

procedure TCustomOwnedObject.Adopt(const Obj: TCustomOwnedObject);
begin
  if not Assigned(Obj) then
    Exit;
  if (Obj <> Self) and (Obj.GetOwner <> Self) then
  begin
    (Obj.GetOwner as TCustomOwnedObject).Detach(Obj);
    Attach(Obj);
  end;
end;

procedure TCustomOwnedObject.Attach(Obj: TCustomOwnedObject);
begin
  DoBeforeAttach(Obj);
  if not Assigned(FOwnedObjects) then
    FOwnedObjects := TList.Create;
  FOwnedObjects.Add(Obj);
  Obj.FOwner := Self;
end;

procedure TCustomOwnedObject.Clear;
begin
  InternalClear;
end;

procedure TCustomOwnedObject.Detach(Obj: TCustomOwnedObject);
begin
  Obj.FOwner := nil;
  FOwnedObjects.Remove(Obj);
  if FOwnedObjects.Count = 0 then
  begin
    FOwnedObjects.Free;
    FOwnedObjects := nil;
  end;
end;

procedure TCustomOwnedObject.DoBeforeAttach(
  const Obj: TCustomOwnedObject);
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

function TCustomOwnedObject.GetOwnedObject(Index: Integer): TCustomOwnedObject;
begin
  if Assigned(FOwnedObjects) then
    Result := TCustomOwnedObject(FOwnedObjects[Index])
  else
    raise EListError.CreateFmt(SListIndexError, [Index]);
end;

function TCustomOwnedObject.GetOwnedObjectsCount: Integer;
begin
  if Assigned(FOwnedObjects) then
    Result := FOwnedObjects.Count
  else
    Result := 0;
end;

function TCustomOwnedObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCustomOwnedObject.InternalClear;
var
  I: Integer;
  Temp: TCustomOwnedObject;
begin
  // Free owned nodes:
  if Assigned(FOwnedObjects) then
  begin
    I := Pred(FOwnedObjects.Count);
    while I >= 0 do
    begin
      Temp := OwnedObject[I];
      Temp.FOwner := nil;      // For a better performance of larger collections: ...
      FOwnedObjects.Delete(I); // ... Avoids the call to FOwner.Detach(Self) in ...
      Dec(I);                  // ... the destructor of the owned objects to be freed.
      Temp.Free;
      if not assigned(FOwnedObjects) then // During clearing it may happen that ...
        exit;                             // ... FOwnedObjects is destroyed; in this case just exit.
      if I >= FOwnedObjects.Count then
        I := Pred(FOwnedObjects.Count);
    end;
    FOwnedObjects.Free;
    FOwnedObjects := nil;
  end;
end;

{ TCustomOwnedNode }

constructor TCustomOwnedNode.Create(const AOwner: TCustomOwnedObject);
begin
  inherited;
  FItems := TList.Create;
end;

destructor TCustomOwnedNode.Destroy;
begin
  if Assigned(FParent) then
    FParent.InternalRemove(Self);
  InternalClear;
  FItems.free;
  inherited;
end;

procedure TCustomOwnedNode.Adopt(const Node: TCustomOwnedNode);
begin
  CheckAssigned(Node);
  if Assigned(Node.Parent) then
    Node.Parent.Remove(Node);
  InternalAdopt(Self, Node);
end;

function TCustomOwnedNode.Append(const NewChild: TCustomOwnedNode):
  TCustomOwnedNode;
begin
  CheckAssigned(NewChild);
  CheckSameOwner(NewChild);
  CheckNotAncestorOrSelf(NewChild); // Test for circularity
  CheckNotReadOnly;
  CheckParentNotReadOnly(NewChild);

  DoBeforeAddition(NewChild);
  InternalInsertBefore(NewChild, nil);
  DoAfterAddition(NewChild);

  Result := NewChild;
end;

procedure TCustomOwnedNode.CheckAssigned(const Node: TCustomOwnedNode);
begin
  if not Assigned(Node) then
    RaiseException(ENotAssignedError);
end;

procedure TCustomOwnedNode.CheckDissimilarity(const Node_1,
  Node_2: TCustomOwnedNode);
begin
  if Node_1 = Node_2 then
    RaiseException(EHierarchyRequestError);
end;

procedure TCustomOwnedNode.CheckHasChild(const Node: TCustomOwnedNode);
begin
  if FItems.IndexOf(Node) = -1 then
    RaiseException(ENotFoundError);
end;

procedure TCustomOwnedNode.CheckNotAncestorOrSelf(const Node: TCustomOwnedNode);
begin
  if HasAsAncestor(Node) or (Node = Self) then
    RaiseException(EHierarchyRequestError);
end;

procedure TCustomOwnedNode.CheckNotReadOnly;
begin
  if ReadOnly then
    RaiseException(ENoModificationAllowedError);
end;

procedure TCustomOwnedNode.CheckParentNotReadOnly(const Node: TCustomOwnedNode);
begin
  if Assigned(Node.Parent) then
    if Node.Parent.ReadOnly then
      RaiseException(ENoModificationAllowedError);
end;

procedure TCustomOwnedNode.CheckSameOwner(const Node: TCustomOwnedNode);
begin
  if (GetOwner <> Node.GetOwner) and (Node.GetOwner <> Self) then
    RaiseException(EWrongOwnerError);
end;

procedure TCustomOwnedNode.Clear;
begin
  CheckNotReadOnly;

  DoBeforeClear;
  InternalClear;
  DoAfterClear;
end;

procedure TCustomOwnedNode.DoAfterAddition(const Node: TCustomOwnedNode);
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

procedure TCustomOwnedNode.DoAfterClear;
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

procedure TCustomOwnedNode.DoAfterRemoval(const Node: TCustomOwnedNode);
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

procedure TCustomOwnedNode.DoBeforeAddition(const Node: TCustomOwnedNode);
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

procedure TCustomOwnedNode.DoBeforeClear;
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

procedure TCustomOwnedNode.DoBeforeRemoval(const Node: TCustomOwnedNode);
begin
  // By default do nothing.  Descendant classes can override this procedure
  // to implement specific behavior.
end;

function TCustomOwnedNode.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomOwnedNode.GetFirstChild: TCustomOwnedNode;
begin
  if FItems.Count > 0 then
    Result := TCustomOwnedNode(FItems.First)
  else
    Result := nil;
end;

function TCustomOwnedNode.GetItem(Index: Integer): TCustomOwnedNode;
begin
  Result := TCustomOwnedNode(FItems[Index]);
end;

function TCustomOwnedNode.GetLastChild: TCustomOwnedNode;
begin
  if FItems.Count > 0 then
    Result := TCustomOwnedNode(FItems.Last)
  else
    Result := nil;
end;

function TCustomOwnedNode.GetNextSibling: TCustomOwnedNode;
begin
  Result := FNextSibling;
end;

function TCustomOwnedNode.GetParent: TCustomOwnedNode;
begin
  Result := FParent;
end;

function TCustomOwnedNode.GetPreviousSibling: TCustomOwnedNode;
begin
  Result := FPreviousSibling;
end;

function TCustomOwnedNode.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TCustomOwnedNode.HasAsAncestor(Node: TCustomOwnedNode): Boolean;
begin
  // No test for circular references is performed here.  Descendant classes
  // which allow circular references must include such a test in
  // HasAsAncestor().
  if Assigned(Parent) then
  begin
    if Parent = Node then
      Result := True
    else
      Result := Parent.HasAsAncestor(Node);
  end
  else
    Result := False;
end;

function TCustomOwnedNode.HasChildren: Boolean;
begin
  Result := (FItems.count > 0);
end;

function TCustomOwnedNode.IndexOf(Node: TCustomOwnedNode): Integer;
begin
  Result := FItems.IndexOf(Node);
end;

function TCustomOwnedNode.InsertBefore(const NewChild,
  RefChild: TCustomOwnedNode): TCustomOwnedNode;
begin
  CheckAssigned(NewChild);
  CheckSameOwner(NewChild);
  CheckNotAncestorOrSelf(NewChild); // Test for circularity
  CheckNotReadOnly;
  CheckParentNotReadOnly(NewChild);
  CheckDissimilarity(NewChild, RefChild);
  if Assigned(RefChild) then
    CheckHasChild(RefChild);

  DoBeforeAddition(NewChild);
  InternalInsertBefore(NewChild, RefChild);
  DoAfterAddition(NewChild);

  Result := NewChild;
end;

procedure TCustomOwnedNode.InternalAdopt(NewOwner, Node: TCustomOwnedNode);
var
  I: Integer;
begin
  (Node.GetOwner as TCustomOwnedObject).Detach(Node);
  NewOwner.Attach(Node);
  for I := 0 to Pred(Node.Count) do
    InternalAdopt(NewOwner, Node[I]);
end;

procedure TCustomOwnedNode.InternalClear;
var
  I: Integer;
  Node: TCustomOwnedNode;
begin
  inherited clear;

  // Free child nodes:
  with FItems do
  begin
    I := Pred(Count);
    while I >= 0 do
    begin
      Node := TCustomOwnedNode(Items[I]);
      Node.FParent := nil;
        // For a better performance on larger trees: Avoids the ...
      FItems.Delete(I);
        // ... call to FParent.InternalRemove(Self) in  the destructor.
      Node.Free;
      Dec(I);
      if I >= Count then
        I := Pred(Count);
    end;
  end;
end;

procedure TCustomOwnedNode.InternalInsertBefore(NewChild,
  RefChild: TCustomOwnedNode);
var
  Index: Integer;
begin
  if Assigned(NewChild.Parent) then
    NewChild.Parent.InternalRemove(NewChild);
  if Assigned(RefChild) then
  begin
    Index := FItems.IndexOf(RefChild);
    FItems.Insert(Index, NewChild);

    // Adjust NextSibling / PreviousSibling properties:
    NewChild.FNextSibling := RefChild;
    NewChild.FPreviousSibling := RefChild.FPreviousSibling;
    RefChild.FPreviousSibling := NewChild;
    if Assigned(NewChild.FPreviousSibling) then
      NewChild.FPreviousSibling.FNextSibling := NewChild;

  end
  else
  begin
    Index := FItems.Add(NewChild);
    if Index > 0 then
    begin
      NewChild.FPreviousSibling := TCustomOwnedNode(FItems[Index - 1]);
      NewChild.FPreviousSibling.FNextSibling := NewChild;
    end;
  end;
  NewChild.FParent := Self;
end;

procedure TCustomOwnedNode.InternalRemove(OldChild: TCustomOwnedNode);
begin
  FItems.Remove(OldChild);
  with OldChild do
  begin
    FParent := nil;

    // Adjust NextSibling / PreviousSibling properties:
    if Assigned(FPreviousSibling) then
      FPreviousSibling.FNextSibling := FNextSibling;
    if Assigned(FNextSibling) then
      FNextSibling.FPreviousSibling := FPreviousSibling;
    FNextSibling := nil;
    FPreviousSibling := nil;

  end;
end;

procedure TCustomOwnedNode.RaiseException(const E: {$IFDEF CLR}ExceptionClass{$ELSE}ExceptClass{$ENDIF});
begin
  if E = EHierarchyRequestError then
    raise E.Create(SHierarchyRequestError)
  else if E = ENoModificationAllowedError then
    raise E.Create(SNoModificationAllowedError)
  else if E = ENotAssignedError then
    raise E.Create(SNotAssignedError)
  else if E = ENotFoundError then
    raise E.Create(SNotFoundError)
  else if E = EWrongOwnerError then
    raise E.Create(SWrongOwnerError)
  else
    raise E.Create(E.ClassName);
end;

function TCustomOwnedNode.Remove(const OldChild: TCustomOwnedNode):
  TCustomOwnedNode;
begin
  CheckAssigned(OldChild);
  CheckNotReadOnly;
  CheckHasChild(OldChild);

  DoBeforeRemoval(OldChild);
  InternalRemove(OldChild);
  DoAfterRemoval(OldChild);

  Result := OldChild;
end;

function TCustomOwnedNode.Replace(const NewChild,
  OldChild: TCustomOwnedNode): TCustomOwnedNode;
var
  RefChild: TCustomOwnedNode;
begin
  CheckAssigned(NewChild);
  CheckAssigned(OldChild);
  CheckSameOwner(NewChild);
  CheckNotAncestorOrSelf(NewChild); // Test for circularity
  CheckNotReadOnly;
  CheckParentNotReadOnly(NewChild);
  CheckHasChild(OldChild);

  if NewChild <> OldChild then
  begin

    if Assigned(NewChild.Parent) then
    begin
      DoBeforeRemoval(NewChild);
      NewChild.Parent.InternalRemove(NewChild);
      DoAfterRemoval(NewChild);
    end;

    RefChild := OldChild.NextSibling;

    DoBeforeRemoval(OldChild);
    InternalRemove(OldChild);
    DoAfterRemoval(OldChild);

    DoBeforeAddition(NewChild);
    InternalInsertBefore(NewChild, RefChild);
    DoAfterAddition(NewChild);
  end;

  Result := OldChild;
end;

procedure TCustomOwnedNode.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

end.

