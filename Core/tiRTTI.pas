{
  This unit contains many Runtime Type Information (RTTI) utility functions.
}
unit tiRTTI;

{$I tiDefines.inc}

interface

uses
  tiBaseObject,
  TypInfo,
  Classes
  ;


const

  CErrorSettingProperty      = 'Error setting property %s.%s Message %s';
  CErrorGettingProperty      = 'Error getting property %s.%s Message %s';

  // Type kinds for use with tiGetPropertyNames
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString {$IFDEF FPC},tkAString{$ENDIF}{$IFDEF UNICODE} , tkUString {$ENDIF} ];
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 {$IFDEF FPC},tkBool{$ENDIF}];
  // Float type properties
  ctkFloat  = [ tkFloat ];
  // Numeric type properties
  ctkNumeric = [tkInteger, tkInt64, tkFloat];
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat;

  // All types that can be handled by tiOPF
  ctkAll = ctkSimple + [tkClass, tkEnumeration];

  // These are the leftovers
  // tkUnknown, tkMethod,
  // tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

  // These are all the possibilities
  // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
  // tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
  // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);

  cErrorInvalidTtiTypeKind = 'Invalid TtiTypeKind';

type
  // Simple TypeKinds, as summary of the TTypeKinds available in TypInfo
  TtiTypeKind =  (tiTKInteger, tiTKFloat , tiTKString, tiTKDateTime, tiTKBoolean, tiTKBinary);

  // Convert a property from Delphi's TTypeKind to TtiSimpleTypeKind
  // EG: Change tkInteger, tkInt64 and tkEnumeration to tkInteger
  function tiGetSimplePropType(const AObject : TtiBaseObject; const APropName : string): TtiTypeKind;
  function tiVarSimplePropType(AValue : Variant): TtiTypeKind;

  // Is this a numeric property ?
  function tiIsNumericProp(AObject : TtiBaseObject; APropName : string): boolean;

  // Read a TtiBaseObject's published properties into a TStringList
  procedure tiGetPropertyNames(AObject : TtiBaseObject;
                                AStringList : TStringList;
                                APropFilter : TTypeKinds = ctkSimple); overload;

  procedure tiGetPropertyNames(AClass : TtiBaseObjectClass;
                                AStringList : TStringList;
                                APropFilter : TTypeKinds = ctkSimple); overload;

  // Is a property a read & write property
  function tiIsReadWriteProp(const AData : TtiBaseObject; const APropName : string): boolean; overload;
  function tiIsReadWriteProp(const AData : TtiBaseObjectClass; const APropName : string): boolean; overload;

  function  tiGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;
  function  tiGetProperty(const AObject: TObject; const APropPath: string): Variant;
  procedure tiSetProperty(const AObject: TObject; const APropPath: string; const APropValue: Variant);
  function  tiGetPropInfo(AClass: TClass; PropPath: string; PInstance: Pointer): PPropInfo;
  procedure tiGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings; PrefixLen: Integer = 0);
  function  tiPropertyInheritsFrom(AClass: TClass; PropPath: string; AParentClass: TClass): boolean;
  function  tiGetPropertyClass(AClass: TClass; PropPath: string): TClass;
  function  tiIsPublishedProp(Instance: TObject; PropPath: string): boolean;


implementation

uses
  tiUtils,
  tiExcept,
  tiConstants,
  SysUtils,
  Variants;

procedure tiSetBooleanPropValue(
  const AObject: TObject;
  const APropName: string;
  const APropValue: Variant);
var
  LStr: string;
  LInt: Integer;
  LBool: Boolean;
begin
  case tiVarSimplePropType(APropValue) of
  tiTKString : begin
                 LStr := APropValue;
                 if SameText(LStr, 'true') or
                   (LStr = '1') or
                   SameText(LStr, 't') then
                   TypInfo.SetPropValue(AObject, APropName, 1)
                 else
                   TypInfo.SetPropValue(AObject, APropName, 0);
               end;
  tiTKInteger : begin
                  LInt := APropValue;
                  if LInt = 0 then
                    TypInfo.SetPropValue(AObject, APropName, 0)
                  else
                    TypInfo.SetPropValue(AObject, APropName, 1);
                end;
  tiTKBoolean : begin
                  LBool := APropValue;
                  if LBool then
                    TypInfo.SetPropValue(AObject, APropName, 1)
                  else
                    TypInfo.SetPropValue(AObject, APropName, 0);
                end;
  else
    raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
      [AObject.ClassName, APropName, 'Unknown type' ]);
  end
end;

function tiGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType;
{$ELSE}
  Result := PropInfo^.PropType^;
{$ENDIF}
end;

function tiGetProperty(const AObject: TObject; const APropPath: string): Variant;
var
  LPropInfo: PPropInfo;
begin
  // ToDo: I'm not sure that invalid AObject or APropPath should be swallowed with null returned
  //       Would it be better if an exception was raised?
  //       How should we differentiate between invalid parameters, and an actual null result?
  if Assigned(AObject) then
  begin
    if not SameText(APropPath, 'self') then
    begin
      if Assigned(AObject) then
      begin
        LPropInfo := tiGetPropInfo(AObject.ClassType, APropPath, @AObject);
        if Assigned(LPropInfo) and Assigned(LPropInfo.GetProc) then
          Result := GetPropValue(AObject, LPropInfo^.Name)
        else
          Result:= Null;
      end;
    end else
      Result := Integer(AObject);
  end else
    Result:= Null;
end;

procedure tiSetProperty(const AObject: TObject; const APropPath: string; const APropValue: Variant);
var
  LPropInfo: PPropInfo;
  LValue: Variant;
begin
  Assert(Assigned(AObject), CTIErrorInvalidObject);
  Assert(APropPath <> '', 'APropPath not aassigned');
  LPropInfo := tiGetPropInfo(AObject.ClassType, APropPath, @AObject);
  if Assigned(LPropInfo) and Assigned(LPropInfo.SetProc) then
  begin
    case tiGetTypeInfo(LPropInfo)^.Kind of
      tkClass:
        SetObjectProp(AObject, LPropInfo, TObject(Integer(APropValue)));
      tkEnumeration{$IFDEF FPC},tkBool{$ENDIF}:
        begin
          if VarIsStr(APropValue) and (VarToStr(APropValue) = '') then
            LValue:= 0
          else
            LValue:= APropValue;
          // Special handling if it's a boolean
          if SameText(LPropInfo^.PropType^.Name, 'Boolean') then
            tiSetBooleanPropValue(AObject, LPropInfo^.Name, LValue)
          else
            SetPropValue(AObject, LPropInfo^.Name, LValue);
        end;
      tkSet:
        if VarToStr(APropValue) = '' then
          SetPropValue(AObject, LPropInfo^.Name, '[]')
        else
          SetPropValue(AObject, LPropInfo^.Name, APropValue);
    else
      SetPropValue(AObject, LPropInfo^.Name, APropValue);
    end;  { case }
  end;
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

function tiPropertyInheritsFrom(AClass: TClass; PropPath: string; AParentClass: TClass): boolean;
var
  PropertyClass: TClass;
begin
  PropertyClass := tiGetPropertyClass(AClass, PropPath);
  result := Assigned(PropertyClass) and PropertyClass.InheritsFrom(AParentClass);
end;

function tiGetPropertyClass(AClass: TClass; PropPath: string): TClass;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
begin
  result := nil;

  PropInfo := tiGetPropInfo(AClass, PropPath, nil);
  if Assigned(PropInfo) then
  begin
    TypeInfo := tiGetTypeInfo(PropInfo);
    if TypeInfo.Kind = tkClass  then
    begin
      TypeData := GetTypeData(TypeInfo);
      if Assigned(TypeData) then
        result := TypeData.ClassType;
    end;
  end;
end;

function tiIsPublishedProp(Instance: TObject; PropPath: string): boolean;
var
  FirstDot: Integer;
  PropName: string;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  if Assigned(Instance) then
  begin
    FirstDot := Pos('.', PropPath);
    if FirstDot = 0 then
      Result := IsPublishedProp(Instance, PropPath)
    else
    begin
      PropName := Copy(PropPath, 1, FirstDot - 1);
      System.Delete(PropPath, 1, FirstDot);
      PropInfo := GetPropInfo(Instance.ClassType, PropName);
      if Assigned(PropInfo) and (PropInfo^.PropType^.Kind = tkClass) then
      begin
        TypeData := GetTypeData(tiGetTypeInfo(PropInfo));
        if Assigned(TypeData) then
          begin
            result := IsPublishedProp(TypeData.ClassType, PropPath);
          end
        else
          Result := false;
      end
      else
        Result := false;
    end;  { if }
  end
  else
    Result := false;

end;

procedure tiGetPropertyNames(AObject : TtiBaseObject; AStringList : TStringList;
                              APropFilter : TTypeKinds = ctkSimple);
begin
  Assert(AObject <> nil, 'pPersistent not assigned.');
  tiGetPropertyNames(TtiBaseObjectClass(AObject.ClassType),
                      AStringList,
                      APropFilter);
end;


procedure tiGetPropertyNames(AClass : TtiBaseObjectClass;
                              AStringList : TStringList;
                              APropFilter : TTypeKinds = ctkSimple);
var
  lCount : integer;
  lSize : integer;
  lList : PPropList;
  i : integer;
  lPropFilter : TTypeKinds;
begin
  {$ifdef fpc} LList := nil; {$endif}
  Assert(AStringList <> nil, 'pSL not assigned.');
  lPropFilter := APropFilter;

  AStringList.Clear;

  lCount := GetPropList(AClass.ClassInfo
                         ,lPropFilter
                         ,nil
                         {$ifdef Delphi6OrAbove},false{$endif});
  lSize  := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
     GetPropList(AClass.ClassInfo
                 ,lPropFilter
                 ,LList
                 {$ifdef Delphi6OrAbove},false{$endif});
    for i := 0 to lcount - 1 do
      AStringList.Add(lList^[i]^.Name);
  finally
    FreeMem(lList, lSize);
  end;
end;


function tiIsReadWriteProp(const AData: TtiBaseObject;
    const APropName: string): boolean;
begin
  result := tiIsReadWriteProp(TtiBaseObjectClass(AData.ClassType), APropName);
end;


function tiIsReadWriteProp(const AData: TtiBaseObjectClass;
    const APropName: string): boolean;
var
  lPropInfo : PPropInfo;
begin
  Assert(AData <> nil, 'AData not assigned');
  Assert(IsPublishedProp(AData, APropName), APropName
      + ' not a published property on ' + AData.ClassName);
  try
    lPropInfo := GetPropInfo(AData, APropName);
    result   := (lPropInfo^.GetProc <> nil) and (lPropInfo^.SetProc <> nil);
  except
    on e:exception do
      raise exception.CreateFmt(
          'Error calling tiIsReadWriteProp with class: %s and property %s',
          [AData.ClassName, APropName]);
  end;
end;


function tiGetSimplePropType(const AObject: TtiBaseObject;
    const APropName: string): TtiTypeKind;
var
  lPropType : TTypeKind;
  lPropTypeName : string;
begin

  Assert(AObject <> nil, 'pPersistent is nil');

  lPropTypeName := GetPropInfo(AObject, APropName)^.PropType^.Name;

  // Check for a TDateTime
  if SameText(lPropTypeName, 'TDateTime') then
  begin
    result := tiTKDateTime;
    Exit; //==>
  end;

  // Check for a Boolean
  if SameText(lPropTypeName, 'Boolean') then
  begin
    result := tiTKBoolean;
    Exit; //==>
  end;

  try
    lPropType := PropType(AObject, APropName);
  except
    on e:exception do
      raise exception.Create('Error in tiGetSimpleTypeKind ' + Cr +
                              'Property name: ' + APropName + Cr +
                              'Message: ' + e.message);
  end;

  // ToDo: Detection of stream properties could be better
  if (lPropType = tkClass) and
     ((SameText('TStream', lPropTypeName)) or
      (SameText('TMemoryStream', lPropTypeName)) or
      (SameText('TFileStream', lPropTypeName)) or
      (SameText('TStringStream', lPropTypeName))) then
  begin
    result := tiTKBinary;
    Exit; //==>
  end;

  case lPropType of
  tkInteger,
  tkInt64,
  tkEnumeration : result := tiTKInteger;

  tkFloat      : result := tiTKFloat;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  {$IFDEF UNICODE}
  tkUString,
  {$ENDIF}
  {$IFDEF FPC}
  tkAString,
  {$ENDIF}
  tkWString    : result := tiTKString;

  {$IFDEF FPC}
  tkBool       : result := tiTKBoolean;
  {$ENDIF}
  else
    raise exception.Create('Invalid property type passed to ' +
                            'tiGetSimplePropType. ClassName <' +
                            AObject.ClassName +
                            '> Property name <' +
                            APropName + '>');
  end;
end;


function tiVarSimplePropType(AValue : Variant): TtiTypeKind;
begin
{
varEmpty        The variant is Unassigned.
varNull	        The variant is Null.
VarSmallint     16-bit signed integer (type Smallint).
varInteger      32-bit signed integer (type Integer).
varSingle       Single-precision floating-point value (type Single).
varDouble       Double-precision floating-point value (type Double).
varCurrency     Currency floating-point value (type Currency).
varDate         Date and time value (type TDateTime).
varOLEStr       Reference to a dynamically allocated UNICODE string.
varDispatch     Reference to an Automation object (an IDispatch interface pointer).
varError        Operating system error code.
varBoolean      16-bit boolean (type WordBool).
varUnknown      Reference to an unknown COM object (an IUnknown interface pointer).
varByte         8-bit unsigned integer (type Byte).
varString       Reference to a dynamically allocated Pascal string (type AnsiString).
varTypeMask     Bit mask for extracting type code.
varArray        Bit indicating variant array.
varByRef        Bit indicating variant contains a reference (rather than a value).
varUString      Delphi 2009 up, unicode string
}

  if tiIsVariantOfType(AValue, varSmallint) or
     tiIsVariantOfType(AValue, varInteger) or
     {$ifdef Delphi6OrAbove}
     tiIsVariantOfType(AValue, varWord) or
     tiIsVariantOfType(AValue, varLongWord) or
     tiIsVariantOfType(AValue, varInt64) or
     tiIsVariantOfType(AValue, varShortInt) or
     {$endif}
     tiIsVariantOfType(AValue, varByte) then
    Result := tiTKInteger
  else if tiIsVariantOfType(AValue, varSingle) or
          tiIsVariantOfType(AValue, varDouble) or
          tiIsVariantOfType(AValue, varCurrency) then
    Result := tiTKFloat
  else if tiIsVariantOfType(AValue, varString) or
  {$ifdef UNICODE}
          tiIsVariantOfType(AValue, varUString) or
  {$endif}
          tiIsVariantOfType(AValue, varOLEStr) then
    Result := tiTKString
  else if tiIsVariantOfType(AValue, varDate) then
    Result := tiTKDateTime
  else if tiIsVariantOfType(AValue, varBoolean) then
    Result := tiTKBoolean
  else
  begin
    raise EtiOPFInternalException.Create(cErrorInvalidVariantType);
    Result := tiTKInteger; // Just to shut the compiler up. Won't get here.
  end;
end;


function tiIsNumericProp(AObject : TtiBaseObject; APropName : string): boolean;
var
  lPropType : TTypeKind;
begin
  try
    lPropType := PropType(AObject, APropName);
  except
    on e:exception do
      raise exception.Create('Error in tiGetSimpleTypeKind ' +
                              'Message: ' + e.message);
  end;
  result := lPropType in [ tkInteger, tkInt64,tkEnumeration, tkFloat ];
end;


end.

