{
  TtiMethodList adapted from code in a variety of posts in the newsgroup:
    borland.public.delphi.language.delphi.win32
  Multicast event code adapted from code by Ivo Bauer in the post on
  09-Jul-2007 to the newsgroup:
    borland.public.delphi.language.delphi.win32

Version History:
  24-Jul-2007 Jarrod Hollingworth  Initial implementation
}

{:@summary Multicast Event System

  @desc A multicast event system that allows multiple event handlers to be
        called when an event is fired.

  @example Implementing a new multicast event type is now a matter of

 1) Implementing a descendant of TtiMulticastEventArgs:
 e.g.
   TtiSampleEventArgs = class(TtiMulticastEventArgs)
   private
     FInOutParam: Integer;
     FInParam: string;
   public
     constructor Create(const AInParam: string);
     property InOutParam: Integer read FInOutParam write FInOutParam;
     property InParam: string read FInParam;
   end;

 2) Declaring a prototype for the event handler:
 e.g.
   TtiSampleEventHandler = procedure (ASender: TObject; AArgs: TtiMulticastEventArgs) of object;

 3) Implementing a descendant of TtiMulticastEventDispatcherAbs:
 e.g.
   TtiSampleMulticastEventDispatcher = class(TtiMulticastEventDispatcherAbs)
   protected
     function GetEventArgsClass: TtiMulticastEventArgsClass; override;
     procedure InvokeHandler(AMethod: TMethod; ASender: TObject; AArgs: TtiMulticastEventArgs); override;
   end;
}
unit tiMulticastEvent;

interface

uses
  // Delphi
  Classes
  // tiOPF
  ,tiBaseObject
  ;

type
  {: A list of method pointers. This stores the list of event handlers that
     need to be called when the event is fired. }
  TtiMethodList = class
  private
    FMethods: TList;
  protected
    function GetItem(AIndex: Integer): TMethod;
    procedure SetItem(AIndex: Integer; const AMethod: TMethod);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AMethod: TMethod);
    procedure Remove(const AMethod: TMethod);
    property Items[AIndex: Integer]: TMethod read GetItem write SetItem;
    property Count: Integer read GetCount;
  end;

  {: The base class for event arguments. If parameters need to be passed or
     returned to the event handler create a descendant with input parameters
     passed to the constructor and expose them as read-only properties, and
     include read-write properties for in-out parameters. }
  TtiMulticastEventArgs = class { empty } end;
  TtiMulticastEventArgsClass = class of TtiMulticastEventArgs;
  {: The base event - notification only, no args parameters }
  TtiMulticastEvent = procedure (ASender: TObject; AArgs: TtiMulticastEventArgs) of object;

  {: Abstract base class for event dispatchers. Create a descendant for each
     event type (uniqe list of event arguments). The class which contains the
     event should create a private dispatcher instance, exposing it as a public
     property or using a public method to allow users of the class to register
     and unregister event handlers for the event.

     There can be zero or one default event handler. It is typically used when
     replacing an existing event with a multicast event by setting the default
     event handler when the OnEvent property of the class is assigned to. }
  TtiMulticastEventDispatcherAbs = class(TtiBaseObject)
  private
    FEventHandlers: TtiMethodList;
    FDefaultEventHandler: TMethod;
  protected
    procedure RegisterEventHandlerMethod(
        const AHandler: TMethod;
        const AIsDefaultHandler: Boolean); overload;
    procedure UnregisterEventHandlerMethod(const AHandler: TMethod); overload;
    function GetEventHandlerCount: Integer;
    procedure SetDefaultEventHandlerMethod(const AHandler: TMethod);
    function GetDefaultEventHandler: TtiMulticastEvent;
    procedure SetDefaultEventHandler(const AValue: TtiMulticastEvent);
    function GetEventArgsClass: TtiMulticastEventArgsClass; virtual; abstract;
    procedure InvokeHandler(AMethod: TMethod; ASender: TObject;
        AArgs: TtiMulticastEventArgs); virtual; abstract;

    property EventHandlers: TtiMethodList read FEventHandlers;
    property DefaultEventHandlerMethod: TMethod read FDefaultEventHandler write SetDefaultEventHandlerMethod;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterEventHandler(
        const AHandler: TtiMulticastEvent;
        const AIsDefaultHandler: Boolean = False); overload;
    procedure UnregisterEventHandler(const AHandler: TtiMulticastEvent); overload;
    procedure DispatchEvent(ASender: TObject; AArgs: TtiMulticastEventArgs);

    property EventHandlerCount: Integer read GetEventHandlerCount;
    property DefaultEventHandler: TtiMulticastEvent read GetDefaultEventHandler write SetDefaultEventHandler;
  end;

  {: A simple notification multiclass event }
  TtiMulticastEventDispatcher = class(TtiMulticastEventDispatcherAbs)
  protected
    function GetEventArgsClass: TtiMulticastEventArgsClass; override;
    procedure InvokeHandler(AMethod: TMethod; ASender: TObject; AArgs: TtiMulticastEventArgs); override;
  end;

implementation

uses
  SysUtils
  ;

resourcestring
  RIncompatibleClassType = '%s (or descendant of) class required';

{ TtiMethodList }

constructor TtiMethodList.Create;
begin
  inherited;
  FMethods := TList.Create;
end;

destructor TtiMethodList.Destroy;
begin
  FMethods.Free;
  inherited;
end;

function TtiMethodList.GetCount: Integer;
begin
  Result := FMethods.Count div 2; // List holds data-code pairs.
end;

function TtiMethodList.GetItem(AIndex: Integer): TMethod;
var
  LIndex: Integer;
begin
  LIndex := AIndex * 2;
  Result.Data := FMethods[LIndex];
  Result.Code := FMethods[LIndex + 1];
end;

procedure TtiMethodList.SetItem(AIndex: Integer; const AMethod: TMethod);
var
  LIndex: Integer;
begin
  LIndex := AIndex * 2;
  FMethods[LIndex] := AMethod.Data;
  FMethods[LIndex + 1] := AMethod.Code;
end;

procedure TtiMethodList.Add(const AMethod: TMethod);
begin
  FMethods.Add(AMethod.Data);
  FMethods.Add(AMethod.Code);
end;

procedure TtiMethodList.Remove(const AMethod: TMethod);
var
  LIndex: Integer;
begin
  LIndex := Count - 1 - 1; // Start (data) of last item
  while LIndex >= 0 do
  begin
    if (FMethods.List[LIndex] {Data} = AMethod.Data) and
       (FMethods.List[LIndex + 1] {Code} = AMethod.Code) then
    begin
      FMethods.Delete(LIndex); // Data
      FMethods.Delete(LIndex); // Code
      Break;
    end;
    Dec(LIndex, 2); // Move to previous item (data/code pair)
  end;
end;

{ TtiMulticastEventDispatcherAbs }

constructor TtiMulticastEventDispatcherAbs.Create;
begin
  inherited;
  FEventHandlers := TtiMethodList.Create;
end;

destructor TtiMulticastEventDispatcherAbs.Destroy;
begin
  FEventHandlers.Free;
  inherited;
end;

function TtiMulticastEventDispatcherAbs.GetEventHandlerCount: Integer;
begin
  Result := EventHandlers.Count;
end;

procedure TtiMulticastEventDispatcherAbs.SetDefaultEventHandlerMethod(
  const AHandler: TMethod);
begin
  RegisterEventHandlerMethod(AHandler, {AIsDefaultHandler} True);
end;

function TtiMulticastEventDispatcherAbs.GetDefaultEventHandler: TtiMulticastEvent;
begin
{$WARN UNSAFE_CAST OFF}
  Result := TtiMulticastEvent(DefaultEventHandlerMethod);
{$WARN UNSAFE_CAST ON}
end;

procedure TtiMulticastEventDispatcherAbs.SetDefaultEventHandler(
  const AValue: TtiMulticastEvent);
begin
  RegisterEventHandler(AValue, {AIsDefaultHandler} True);
end;

procedure TtiMulticastEventDispatcherAbs.DispatchEvent(
  ASender: TObject; AArgs: TtiMulticastEventArgs);
var
  LClass: TtiMulticastEventArgsClass;
  I: Integer;
begin
  LClass := GetEventArgsClass;
  if AArgs is LClass then
  begin
    for I := 0 to Pred(EventHandlers.Count) do
      InvokeHandler(EventHandlers.Items[I], ASender, AArgs);
  end
  else
    raise Exception.CreateFmt(RIncompatibleClassType, [LClass.ClassName]);
end;

procedure TtiMulticastEventDispatcherAbs.RegisterEventHandlerMethod(
  const AHandler: TMethod; const AIsDefaultHandler: Boolean);
begin
  if AIsDefaultHandler then
  begin
    // There can only be one default handler.
    if (FDefaultEventHandler.Data <> nil) or (FDefaultEventHandler.Code <> nil) then
      EventHandlers.Remove(DefaultEventHandlerMethod);
    FDefaultEventHandler.Data := AHandler.Data;
    FDefaultEventHandler.Code := AHandler.Code;
  end;
  if (AHandler.Data <> nil) or (AHandler.Code <> nil) then
    EventHandlers.Add(AHandler);
end;

procedure TtiMulticastEventDispatcherAbs.UnregisterEventHandlerMethod(
  const AHandler: TMethod);
begin
  if (AHandler.Data = DefaultEventHandlerMethod.Data) and
     (AHandler.Code = DefaultEventHandlerMethod.Code) then
  begin
    FDefaultEventHandler.Data := nil;
    FDefaultEventHandler.Code := nil;
  end;
  EventHandlers.Remove(AHandler);
end;

procedure TtiMulticastEventDispatcherAbs.RegisterEventHandler(
  const AHandler: TtiMulticastEvent; const AIsDefaultHandler: Boolean);
begin
{$WARN UNSAFE_CAST OFF}
  RegisterEventHandlerMethod(TMethod(AHandler), AIsDefaultHandler);
{$WARN UNSAFE_CAST ON}
end;

procedure TtiMulticastEventDispatcherAbs.UnregisterEventHandler(
  const AHandler: TtiMulticastEvent);
begin
{$WARN UNSAFE_CAST OFF}
  UnregisterEventHandlerMethod(TMethod(AHandler));
{$WARN UNSAFE_CAST ON}
end;

{ TtiMulticastEventDispatcher }

function TtiMulticastEventDispatcher.GetEventArgsClass: TtiMulticastEventArgsClass;
begin
  Result := TtiMulticastEventArgs;
end;

procedure TtiMulticastEventDispatcher.InvokeHandler(AMethod: TMethod;
  ASender: TObject; AArgs: TtiMulticastEventArgs);
begin
{$WARN UNSAFE_CAST OFF}
  TtiMulticastEvent(AMethod)(ASender, AArgs);
{$WARN UNSAFE_CAST ON}
end;

end.
