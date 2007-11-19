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
    FRefCounting: boolean;
    FRefCount:    integer;
    {$ENDIF}
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  public
    {$IFDEF REFERENCE_COUNTING}
    constructor CreateWithRefCounting;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}

    {: Checks that self <> nil (which is not always very
       useful but still worth checking.)}
    function TestValid(AClassType: TClass = nil; AAllowNil: boolean = False): boolean; overload;
    function TestValid(AAllowNil: boolean): boolean; overload;

  end;

  TtiBaseObjectClass = class of TtiBaseObject;

implementation

function TtiBaseObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TtiBaseObject._AddRef: integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedIncrement(FRefCount);
  {$else}
  Result := 0;
  {$ENDIF}
end;

function TtiBaseObject._Release: integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedDecrement(FRefCount);
  if FRefCounting then
    if Result = 0 then
      Destroy;
  {$else}
  Result := 0;
  {$ENDIF}
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

function TtiBaseObject.TestValid(AClassType: TClass = nil; AAllowNil: boolean = False): boolean;
begin
  Result := AAllowNil or Assigned(self);
  if Result and Assigned(self) and Assigned(AClassType) then
    Result := Self is AClassType;
end;

function TtiBaseObject.TestValid(AAllowNil: boolean): boolean;
begin
  Result := TestValid(TtiBaseObject, AAllowNil);
end;

end.
