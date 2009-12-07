{
  Unit that implements the interfaces defined in tiValueTypeIntf.
}
unit tiValueType;

{$I vtfDefines.inc}

interface

uses
  Classes,
  SysUtils,
  tiValueTypeIntf,
  tiTypes,
  tiSubjectIntf,
  tiObserverIntf,
  SyncObjs;
  
  
type

  { Object that is owned by another object. It also implements the
     ItiOwnedObject interface. }
  TtiOwnedObject = class(TInterfacedObject, ItiOwnedObject)
  private
    FOwner: Pointer;
  protected
    function    GetOwner: IInterface;
    procedure   SetOwner(const Value: IInterface);
    function    Implementor: TObject;
    function    ClassName: string;
    property    Owner: IInterface read GetOwner write SetOwner;
  public
    constructor Create(const AOwner: IInterface = nil); virtual;
  end;


  TtiValueType = class(TtiOwnedObject, ItiValueType, ItiSubject)
  private
    FState: ItiState;
    FSubject: ItiSubject;
    FLock: TCriticalSection;
  protected
    { These can all be protected because we only talk via interfaces }
    procedure   Lock;
    procedure   Unlock;
    function    Clone: ItiValueType; virtual;
    function    GetNotifying: boolean;
    function    GetState: ItiState;
    function    GetSubject: ItiSubject;
    procedure   Attach(const Observer: ItiObserver);
    procedure   Detach(const Observer: ItiObserver);
    procedure   Notify(const Sender: IInterface; const NotifyType: TtiNotifyType); virtual;
    procedure   Assign(const Source: ItiValueType); virtual;
    procedure   LoadFromStream(const Reader: TReader); virtual;
    procedure   SaveToStream(const Writer: TWriter); virtual;
    property    Notifying: boolean read GetNotifying;
    property    State: ItiState read GetState;
    property    Subject: ItiSubject read GetSubject;
  public
    constructor Create(const AOwner: IInterface = nil); override;
    destructor  Destroy; override;
//    class function NewList: IObjectListType;
  end;


implementation

{ TtiOwnedObject }

function TtiOwnedObject.GetOwner: IInterface;
begin
  Result := IInterface(FOwner);
end;

procedure TtiOwnedObject.SetOwner(const Value: IInterface);
begin
  FOwner := Pointer(Value);
end;

function TtiOwnedObject.Implementor: TObject;
begin
  Result := Self;
end;

function TtiOwnedObject.ClassName: string;
begin
  Result := Implementor.ClassName;
end;

constructor TtiOwnedObject.Create(const AOwner: IInterface);
begin
  inherited Create;
  if FOwner = nil then
    FOwner:= Pointer(AOwner);
end;

{ TtiValueType }

procedure TtiValueType.Lock;
begin

end;

procedure TtiValueType.Unlock;
begin

end;

function TtiValueType.Clone: ItiValueType;
begin

end;

function TtiValueType.GetNotifying: boolean;
begin

end;

function TtiValueType.GetState: ItiState;
begin

end;

function TtiValueType.GetSubject: ItiSubject;
begin

end;

procedure TtiValueType.Attach(const Observer: ItiObserver);
begin

end;

procedure TtiValueType.Detach(const Observer: ItiObserver);
begin

end;

procedure TtiValueType.Notify(const Sender: IInterface;
  const NotifyType: TtiNotifyType);
begin

end;

procedure TtiValueType.Assign(const Source: ItiValueType);
begin

end;

procedure TtiValueType.LoadFromStream(const Reader: TReader);
begin

end;

procedure TtiValueType.SaveToStream(const Writer: TWriter);
begin

end;

constructor TtiValueType.Create(const AOwner: IInterface);
begin
  inherited Create(AOwner);
end;

destructor TtiValueType.Destroy;
begin
  inherited Destroy;
end;

end.

