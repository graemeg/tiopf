{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit pbpEvents;

interface

uses
  Classes;
  
type
  IEventListener = interface;

  TEnumListenerProc = procedure(Listener: IEventListener) of object;

  IEventListener = interface(IUnknown)
    ['{7461C599-974C-4592-8173-A3550BB267D9}']
  end;

  IEventListenerList = interface(IUnknown)
    ['{C4EDBB39-9BA6-46B1-8103-C6F103979FF9}']
    function Add(Item: IEventListener): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EnumListeners(EnumListenerProc: TEnumListenerProc);
    function GetCount: Integer; 
    function GetItems(Index: Integer): IEventListener;
    procedure Remove(Item: IEventListener);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IEventListener read GetItems;
  end;

  TEventListenerList = class(TInterfacedObject, IEventListenerList)
  private
    FListeners: IInterfaceList;
  public
    { IEventListenerList }
    function Add(Item: IEventListener): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EnumListeners(EnumListenerProc: TEnumListenerProc);
    function GetCount: Integer;
    function GetItems(Index: Integer): IEventListener;
    procedure Remove(Item: IEventListener);
    constructor Create;
  end;
  
implementation

{ TEventListenerList }

function TEventListenerList.Add(Item: IEventListener): Integer;
begin
  Assert(Item <> nil);
  Result := FListeners.IndexOf(Item);
  if Result = -1 then // Constraint: Items cannot be repeated in list.
    Result := FListeners.Add(Item);
end;

procedure TEventListenerList.Clear;
begin
  FListeners.Clear;
end;

constructor TEventListenerList.Create;
begin
  FListeners := TInterfaceList.Create;
end;

procedure TEventListenerList.Delete(Index: Integer);
begin
  FListeners.Delete(Index);
end;

procedure TEventListenerList.EnumListeners(EnumListenerProc: TEnumListenerProc);
var
  Counter: Integer;
  LastCount: Integer;
begin
  LastCount := Self.GetCount;
  Counter := LastCount - 1;
  while Counter >= 0 do
  begin
    EnumListenerProc(IEventListener(Self.GetItems(Counter)));
{ TODO -oPBPPRO -cSMELL : 
This could be improved. Just because the listener count has not changed
doesn't mean that no listeners have been removed (eg. one removed, one added)
Better to keep a bucket of 'visited' listeners. Once the bucket is empty then
no more calls are to be made }
    if LastCount > Self.GetCount then
      Dec(Counter, LastCount - Self.GetCount + 1)  //In case of subscriber get removed
    else                                           //while enumerating
      Dec(Counter);
    LastCount := Self.GetCount;
  end;
end;

function TEventListenerList.GetCount: Integer;
begin
  Result := FListeners.Count;
end;

function TEventListenerList.GetItems(Index: Integer): IEventListener;
begin
  Result := IEventListener(FListeners.Items[Index]);
end;

procedure TEventListenerList.Remove(Item: IEventListener);
begin
  FListeners.Remove(Item);
end;

end.
