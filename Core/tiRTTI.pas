{
  This unit contains many Runtime Type Information (RTTI) utility functions.
}
unit tiRTTI;

{$I tiDefines.inc}

interface

uses
  Classes
  ,TypInfo
  ;


function  tiGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;
function  tiGetProperty(AObject: TObject; PropPath: string): Variant;
procedure tiSetProperty(AObject: TObject; PropPath: string; Value: Variant);
function  tiGetPropInfo(AClass: TClass; PropPath: string; PInstance: Pointer): PPropInfo;
procedure tiGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings; PrefixLen: Integer = 0);


implementation

uses
  SysUtils
  ,Variants
  ;


function AccessProperty(AObject: TObject; PropPath: string; Value: Variant): Variant;
var
  PropInfo: PPropInfo;
begin
  if Assigned(AObject) then
  begin
    if SameText(PropPath, 'self') then
    begin
      Result := Integer(AObject);
      Exit; //==>
    end;
    PropInfo := tiGetPropInfo(AObject.ClassType, PropPath, @AObject);
    if not Assigned(AObject) then
      VarClear(Result)
    else if Assigned(PropInfo) then
    begin
      if not VarIsNull(Value) and Assigned(PropInfo.SetProc) then
      begin
        case tiGetTypeInfo(PropInfo)^.Kind of
          tkClass:
            SetObjectProp(AObject, PropInfo, TObject(Integer(Value)));
          tkEnumeration:
            begin
              {$IFDEF DELPHI6ORABOVE}
              if VarIsStr(Value) and (VarToStr(Value) = '') then
              {$ELSE}
              if Value = '' then
              {$ENDIF}
                Value := 0;
              SetPropValue(AObject, PropInfo^.Name, Value);
            end;
          tkSet:
            if VarToStr(Value) = '' then
              SetPropValue(AObject, PropInfo^.Name, '[]')
            else
              SetPropValue(AObject, PropInfo^.Name, Value);
        else
          SetPropValue(AObject, PropInfo^.Name, Value);
        end;  { case }
      end;  { if }
      Result := GetPropValue(AObject, PropInfo^.Name);
    end
    else
      Result := Null;
  end
  else
    VarClear(Result);
end;


function tiGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType;
{$ELSE}
  Result := PropInfo^.PropType^;
{$ENDIF}
end;

function tiGetProperty(AObject: TObject; PropPath: string): Variant;
begin
  Result := AccessProperty(AObject, PropPath, Null);
end;

procedure tiSetProperty(AObject: TObject; PropPath: string; Value: Variant);
begin
  AccessProperty(AObject, PropPath, Value);
end;

function tiGetPropInfo(AClass: TClass; PropPath: string; PInstance: Pointer): PPropInfo;
var
  FirstDot: Integer;
  PropName: string;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  if Assigned(AClass) then
  begin
    FirstDot := Pos('.', PropPath);
    if FirstDot = 0 then
      Result := GetPropInfo(AClass, PropPath)
    else
    begin
      PropName := Copy(PropPath, 1, FirstDot - 1);
      System.Delete(PropPath, 1, FirstDot);
      PropInfo := GetPropInfo(AClass, PropName);
      if Assigned(PropInfo) and (PropInfo^.PropType^.Kind = tkClass) then
      begin
        if Assigned(PInstance) and Assigned(TObject(PInstance^)) then
          TObject(PInstance^) := GetObjectProp(TObject(PInstance^), PropInfo);
        TypeData := GetTypeData(tiGetTypeInfo(PropInfo));
        if Assigned(TypeData) then
          Result := tiGetPropInfo(TypeData.ClassType, PropPath, PInstance)
        else
          Result := nil;
      end
      else
        Result := nil;
    end;  { if }
  end
  else
    Result := nil;
end;

procedure tiGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings; PrefixLen: Integer = 0);
var
  TypeData: PTypeData;
  I: Integer;
  S: string;
begin
  TypeData := GetTypeData(TypeInfo);
  Names.BeginUpdate;
  try
    Names.Clear;
    for I := TypeData^.MinValue to TypeData^.MaxValue do
    begin
      S := GetEnumName(TypeInfo, I);
      Delete(S, 1, PrefixLen);
      Names.Add(S);
    end;
  finally
    Names.EndUpdate;
  end;
end;

end.

