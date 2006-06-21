unit sclEvents;

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
    { IEventListenerList }
    function Add(Item: IEventListener): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EnumListeners(EnumListenerProc: TEnumListenerProc);
    function GetCount: Integer;
    function GetItems(Index: Integer): IEventListener;
    procedure Remove(Item: IEventListener); 
  public
    constructor Create; 
  end;
  
implementation

{ TEventListenerList }

function TEventListenerList.Add(Item: IEventListener): Integer;
begin
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
begin
  for Counter := 0 to Self.GetCount -1 do
    EnumListenerProc(IEventListener(Self.GetItems(Counter)));
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
