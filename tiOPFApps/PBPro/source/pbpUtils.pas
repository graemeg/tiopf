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

unit pbpUtils;

interface

uses
  Sysutils, Stdctrls, ActiveX, Graphics, ComCtrls,

  jclStrings;

type
  TEditKeyPressUtility = class
    class procedure OnEditKeyPress_DisallowNonNumerics(Sender: TObject; var Key: Char);
    class procedure OnEditKeyPress_DisallowNonIntegers(Sender: TObject; var Key: Char);
  end;

  // case statement support methods
  function CaseObject(AObject: TObject; AObjectArray: array of TObject): Integer;
  function CaseClass(AClass: TClass; AClassArray: array of TClass): Integer; overload;
  function CaseClass(AObject: TObject; AClassArray: array of TClass): Integer; overload;
  function CaseGUID(GUID: TGUID; GUIDArray: array of TGUID): Integer;
  function CaseInterface(AInterface: IUnknown; AClassArray: array of IUnknown): Integer;
  function CaseString(S: string; SArray: array of string): Integer;
  function CasePointer(P: Pointer; PArray: array of Pointer): Integer;

  procedure SetControlReadonly(Control: TRichEdit; Value: Boolean); overload;
  procedure SetControlReadonly(Control: TEdit; Value: Boolean); overload;

  procedure SetEditReadOnly(Control: TEdit; Value: Boolean);
  procedure SetRichEditReadonly(Control: TRichEdit; Value: Boolean);

implementation

function CaseObject(AObject: TObject; AObjectArray: array of TObject): Integer;
var
  Counter: Integer;
begin
  Result := -1;
  for Counter := 0 to Length(AObjectArray)-1 do
  begin
    if AObject = AObjectArray[Counter] then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;
end;

function CaseClass(AClass: TClass; AClassArray: array of TClass): Integer;
var
  Counter: Integer;
begin
  Result := -1;
  for Counter := 0 to Length(AClassArray)-1 do
  begin
    if AClass = AClassArray[Counter] then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;

end;

function CaseClass(AObject: TObject; AClassArray: array of TClass): Integer;
begin
  Result := CaseClass(AObject.ClassType, AClassArray);
end;

function CaseGUID(GUID: TGUID; GUIDArray: array of TGUID): Integer;
var
  Counter: Integer;
begin
  // Thanks to Billy Egge for the CaseXX function idea. I'm not sure this is how he implements them,
  // but the output is the same.
  Result := -1;
  for Counter := 0 to Length(GUIDArray)-1 do
  begin
    if IsEqualGUID(GUID, GUIDArray[Counter]) then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;
end;

function CaseInterface(AInterface: IUnknown; AClassArray: array of IUnknown): Integer;
var
  Counter: Integer;
begin
  Result := -1;
  for Counter := 0 to Length(AClassArray)-1 do
  begin
    if AInterface = AClassArray[Counter] then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;
end;

function CaseString(S: string; SArray: array of string): Integer;
var
  Counter: Integer;
begin
  Result := -1;
  for Counter := 0 to Length(SArray)-1 do
  begin
    if S = SArray[Counter] then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;
end;

function CasePointer(P: Pointer; PArray: array of Pointer): Integer;
var
  Counter: Integer;
begin
  Result := -1;
  for Counter := 0 to Length(PArray)-1 do
  begin
    if P = PArray[Counter] then
    begin
      Result := Counter;
      Exit; //==>
    end;
  end;
end;

procedure SetControlReadonly(Control: TRichEdit; Value: Boolean); overload;
begin
  SetRichEditReadOnly(Control, Value);
end;

procedure SetControlReadonly(Control: TEdit; Value: Boolean); overload;
begin
  SetEditReadOnly(Control, Value);
end;

procedure SetEditReadOnly(Control: TEdit; Value: Boolean);
begin
  case Value of
    False : begin
              Control.ParentColor := False;
              Control.ReadOnly := False;
              Control.Color := clWindow;
            end;
    True  : begin
              Control.ReadOnly := True;
              if Control.Parent <> nil then
                Control.ParentColor := True
              else
                Control.Color := clBtnFace

            end;
  end;
end;

procedure SetRichEditReadonly(Control: TRichEdit; Value: Boolean);
begin
  case Value of
    False : begin
              Control.ParentColor := False;
              Control.ReadOnly := False;
              Control.Color := clWindow;
            end;
    True  : begin
              Control.ReadOnly := True;
              if Control.Parent <> nil then
                Control.ParentColor := True
              else
                Control.Color := clBtnFace

            end;
  end;
end;

{ TEditKeyPressUtility }

class procedure TEditKeyPressUtility.OnEditKeyPress_DisallowNonIntegers(
  Sender: TObject; var Key: Char);
begin
  Assert(Sender is TEdit);

  if (Key <> AnsiBackspace) and (Key <> AnsiCarriageReturn) then
  begin
    if not CharIsDigit(Key) then
      Abort;
  end;

end;

class procedure TEditKeyPressUtility.OnEditKeyPress_DisallowNonNumerics(
  Sender: TObject; var Key: Char);
var
  Code: Integer;
  Dummy: Double;
begin
  Assert(Sender is TEdit);

  if ( Key <> AnsiBackspace ) and (Key <> AnsiCarriageReturn) then
  begin
    Val((Sender as TEdit).Text + Key, Dummy, Code);
    if Code <> 0 then
      Abort;
  end;
end;

end.
