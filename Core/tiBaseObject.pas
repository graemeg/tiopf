{: Unit containing base class used by all tiOPF classes - implements
   IInterface and optional reference counting.}
unit tiBaseObject;

{$I tiDefines.inc}

interface

uses
  Classes;

type

  {:Abstract base class for all tiOPF objects. Implements IInterface and
    optional reference counting.}
  TtiBaseObject = class(TObject, IInterface)
  private
    {$IFDEF REFERENCE_COUNTING}
    FRefCounting: Boolean;
    FRefCount: Integer;
    {$ENDIF}
  protected
    function    QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function    _AddRef: Integer; stdcall;
    function    _Release: Integer; stdcall;
  public
    constructor Create;
    destructor  Destroy; override;
    {$IFDEF REFERENCE_COUNTING}
    constructor CreateWithRefCounting;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}

    {: Checks that self <> nil (which is not always very
       useful but still worth checking.)}
    function TestValid(AClassType: TClass = NIL; AAllowNil: boolean = False): boolean; overload;
    function TestValid(AAllowNil: boolean): boolean; overload;

  end;

  TtiBaseObjectClass = class of TtiBaseObject;

implementation

uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows    // needed for InterlockedDecrement()
  {$ENDIF}
  ,SyncObjs   // This unit must always appear after the Windows unit!
  ;

const
  ASSERT_UNIT = 'IdSoapDebug';

{$IFNDEF DELPHI5ORABOVE}
procedure FreeAndNil(var AObj);
var
  Temp: TObject;
begin
  Temp          := TObject(AObj);
  Pointer(AObj) := NIL;
  Temp.Free;
end;
{$ENDIF}

//break point into the debugger if there is one;
procedure IdBreakpoint;
begin
  try
    asm
      int $03
    end;
  except
    // on some poorly configured Windows systems int $03 can cause unhandled
    // exceptions with improperly installed Dr Watsons etc....
  end;
end;

function TtiBaseObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TtiBaseObject._AddRef: Integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result:= InterlockedIncrement(FRefCount);
  {$else}
  Result:= 0;
  {$ENDIF}
end;

function TtiBaseObject._Release: Integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedDecrement(FRefCount);
  if FRefCounting then
    if Result = 0 then
      Destroy;
  {$else}
  Result:= 0;
  {$ENDIF}
end;

constructor TtiBaseObject.Create;
begin
  inherited;
end;

{ Set an implicit refcount so that refcounting during construction won't
  destroy the object. }
{$IFDEF REFERENCE_COUNTING}
constructor TtiBaseObject.CreateWithRefCounting;
begin
  Create;
  FRefCounting := True;
end;
{$ENDIF}

destructor TtiBaseObject.Destroy;
begin
  inherited;
end;

{$IFDEF REFERENCE_COUNTING}
procedure TtiBaseObject.AfterConstruction;
begin
  inherited AfterConstruction;
  // Release the constructor's implicit refcount
  if FRefCounting then
    InterlockedDecrement(FRefCount);
end;
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TtiBaseObject.BeforeDestruction;
begin
  if FRefCounting then
    if FRefCount <> 0 then
      System.Error(reInvalidPtr);
  inherited BeforeDestruction;
end;

class function TtiBaseObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TtiBaseObject(Result).FRefCount := 1;
end;
{$ENDIF}

function TtiBaseObject.TestValid(AClassType: TClass = NIL; AAllowNil: boolean = False): boolean;
begin
  Result := AAllowNil or Assigned(self);
  if Result and Assigned(self) and Assigned(AClassType) then
    Result := Self is AClassType;
end;

function TtiBaseObject.TestValid(AAllowNil: boolean): boolean;
begin
  result:= TestValid(TtiBaseObject, AAllowNil);
end;

end.






