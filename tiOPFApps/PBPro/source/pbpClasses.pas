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
unit pbpClasses;

interface

uses
  Sysutils, Forms, Contnrs,

  tiPtnVisPerObj;

type
  TInterfacedForm = class(TForm, IUnknown)
  private
    FRefCount: Integer;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  TInterfacedPerObjAbs = class(TPerObjAbs, IUnknown)
  private
    FRefCounting: Boolean;
    FRefCount: Integer;
  protected  
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor CreateWithRefCounting;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;

  TFormStack = class(TObjectStack)
  private
    function GetItem(Index: Integer): TForm;
    procedure SetItem(Index: Integer; const Value: TForm);
  public
    procedure Push(AForm: TForm); reintroduce;
    function Pop: TForm; reintroduce;
    function Peek: TForm; reintroduce;
    property Items[Index: Integer]: TForm read GetItem write SetItem;
  end;

implementation

const
  kernel = 'kernel32.dll';
{ Internal runtime error codes }
  reInvalidPtr        = 2;

function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedIncrement';

function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedDecrement';

{ TInterfacedForm }

function TInterfacedForm._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedForm._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TInterfacedForm.AfterConstruction;
begin
  inherited;
  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TInterfacedForm.BeforeDestruction;
begin
  if FRefCount <> 0 then raise Exception.Create('Invalid pointer');
  inherited;
end;

class function TInterfacedForm.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedForm(Result).FRefCount := 1;
end;

function TInterfacedForm.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

{ TInterfacedPerObjAbs }

procedure TInterfacedPerObjAbs.AfterConstruction;
begin
// Release the constructor's implicit refcount
  if FRefCounting then
    InterlockedDecrement(FRefCount);
end;

procedure Error(errorCode: Byte);
begin
  raise Exception.Create('Unknown error code #' + IntToStr(errorCode));
end;

procedure TInterfacedPerObjAbs.BeforeDestruction;
begin
  if FRefCounting then
    if FRefCount <> 0 then Error(reInvalidPtr);
  inherited;
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
constructor TInterfacedPerObjAbs.CreateWithRefCounting;
begin
  inherited Create;
  FRefCounting := True;
end;

class function TInterfacedPerObjAbs.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedPerObjAbs(Result).FRefCount := 1;
end;

function TInterfacedPerObjAbs.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TInterfacedPerObjAbs._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedPerObjAbs._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if FRefCounting then
    if Result = 0 then
      Destroy;
end;

{ TFormStack }

function TFormStack.GetItem(Index: Integer): TForm;
begin
  Result := TObject(List.Items[Index]) as TForm;
end;

function TFormStack.Peek: TForm;
begin
  Result := (inherited Peek) as TForm;
end;

function TFormStack.Pop: TForm;
begin
  Result := (inherited Pop) as TForm;
end;

procedure TFormStack.Push(AForm: TForm);
begin
  inherited Push(AForm);
end;

procedure TFormStack.SetItem(Index: Integer; const Value: TForm);
begin
  List.Items[Index] := Value;
end;

end.
