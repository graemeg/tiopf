
{*******************************************************}
{                                                       }
{       Sage Code Library - RegistryAPI                 }
{       TSclRegistryFile classes                        }
{                                                       }
{       Copyright (c) 2001 Sage Automation              }
{                                                       }
{*******************************************************}

unit SclRegistryFile;

interface

uses
  Sysutils, Classes, Dialogs, IniFiles, Windows, Registry, Consts, JclSysInfo,
  JclFileUtils, JclStrings, JclStrHashMap, SclConsts, SclRegistry;

type
  TSclRegistryFile = class(TInterfacedObject, IRegistry)
  private
    FAccess: LongWord;
    FCurrentPath: string;
    FIniFile: TIniFile;
    FIniFileName: TFileName;
    FLazyWrite: Boolean;
    FReadOnly: Boolean;
    FRootKey: HKEY;
    function CurrentPathAsSection: string;
    function EscapedStringToStr(AString: string): string;
    function GetParentKey(Key: string): string;
    function GetPath(Key: string): string;
    function GetIniFile: TIniFile;
    function GetKeyPathAsSection(Key: string): string;
    function ExtractKeyName(Key: string): string;
    function ValueNameToRawName(Name: string): string;
    function RawNameToValueName(RawName: string): string;
    function InternalValueExists(const Section, Name: string): Boolean;
    procedure InternalWriteOutStringValue(const Section, Name, Value: string; RegDataType: TRegDataType);
    function InternalReadInStringValue(const Section, Name: string;
      var Value: string; var RegDataType: TRegDataType): Boolean;
    procedure InternalWriteBinaryData(const Name: string; var Buffer; BufSize: Integer;
      IsExpandString: Boolean);
    function InternalOpenKey(const Key: string; CanCreate: Boolean): Boolean;
    function IsWhiteSpace(AChar: Char): Boolean;
    function GetSectionAsKeyPath(Section: string): string;
    function StrToEscapedString(const AString: string): string;
  public
    procedure CloseKey; stdcall;
    function CreateKey(const Key: string): Boolean; stdcall;
    function DeleteKey(const Key: string): Boolean; stdcall;
    function DeleteValue(const Name: string): Boolean; stdcall;
    function GetAccess: LongWord; stdcall;
    function GetCurrentKey: HKEY;
    function GetCurrentPath: string;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean; stdcall;
    function GetDataSize(const ValueName: string): Integer; stdcall;
    function GetDataType(const ValueName: string): TRegDataType; stdcall;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean; stdcall;
    procedure GetKeyNames(Strings: TStrings); stdcall;
    function GetLazyWrite: Boolean; stdcall;
    function GetRootKey: HKEY; stdcall;
    procedure GetValueNames(Strings: TStrings); stdcall;
    function HasSubKeys: Boolean; stdcall;
    function KeyExists(const Key: string): Boolean; stdcall;
    function LoadKey(const Key, FileName: string): Boolean; stdcall;
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean); stdcall;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean; stdcall;
    function OpenKeyReadOnly(const Key: String): Boolean; stdcall;
    function ReadCurrency(const Name: string): Currency; stdcall;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer; stdcall;
    function ReadBool(const Name: string): Boolean; stdcall;
    function ReadDate(const Name: string): TDateTime; stdcall;
    function ReadDateTime(const Name: string): TDateTime; stdcall;
    function ReadFloat(const Name: string): Double; stdcall;
    function ReadInteger(const Name: string): Integer; stdcall;
    function ReadString(const Name: string): string; stdcall;
    function ReadTime(const Name: string): TDateTime; stdcall;
    function RegistryConnect(const Name: string): Boolean; stdcall;
    procedure RenameValue(const OldName, NewName: string); stdcall;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean; stdcall;
    function RestoreKey(const Key, FileName: string): Boolean; stdcall;
    function SaveKey(const Key, FileName: string): Boolean; stdcall;
    procedure SetAccess(const Value: LongWord); stdcall;
    procedure SetLazyWrite(const Value: Boolean); stdcall;
    procedure SetRootKey(const Value: HKEY); stdcall;
    function UnLoadKey(const Key: string): Boolean; stdcall;
    function ValueExists(const Name: string): Boolean; stdcall;
    procedure WriteCurrency(const Name: string; Value: Currency); stdcall;
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer); stdcall;
    procedure WriteBool(const Name: string; Value: Boolean); stdcall;
    procedure WriteDate(const Name: string; Value: TDateTime); stdcall;
    procedure WriteDateTime(const Name: string; Value: TDateTime); stdcall;
    procedure WriteFloat(const Name: string; Value: Double); stdcall;
    procedure WriteInteger(const Name: string; Value: Integer); stdcall;
    procedure WriteString(const Name, Value: string); stdcall;
    procedure WriteExpandString(const Name, Value: string); stdcall;
    procedure WriteTime(const Name: string; Value: TDateTime); stdcall;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property CurrentKey: HKEY read GetCurrentKey;
    property CurrentPath: string read GetCurrentPath;
    property LazyWrite: Boolean read GetLazyWrite write SetLazyWrite;
    property RootKey: HKEY read GetRootKey write SetRootKey;
    property Access: LongWord read GetAccess write SetAccess;
  end;


const
  REG_FILE_EXTENSION = '.REG';

type
  TEscapedToken =
    ( etAmpersand,
      etCarriageReturn,
      etLineFeed,
      etRightSquareBracket,
      etLeftSquareBracket
     );

  TEscapedTokenStrings = array[TEscapedToken] of string;

const
  ESC_ESCAPE = '&#38';

  AMPERSAND_TOKEN         = '&';
  CR_TOKEN                = #13;
  LF_TOKEN                = #10;
  RIGHT_SQR_BRACKET_TOKEN = ']';
  LEFT_SQR_BRACKET_TOKEN  = '[';

  REPLACE_AMPERSAND_TOKEN         = '&#38';
  REPLACE_CR_TOKEN                = '&#13';
  REPLACE_LF_TOKEN                = '&#10';
  REPLACE_RIGHT_SQR_BRACKET_TOKEN = '&#93';
  REPLACE_LEFT_SQR_BRACKET_TOKEN  = '&#91';

  ESCAPED_TOKENS: TEscapedTokenStrings =
    ( AMPERSAND_TOKEN,
      CR_TOKEN,
      LF_TOKEN,
      RIGHT_SQR_BRACKET_TOKEN,
      LEFT_SQR_BRACKET_TOKEN
    );

  REPLACEMENT_TOKENS: TEscapedTokenStrings =
    ( ESC_ESCAPE + REPLACE_AMPERSAND_TOKEN,
      ESC_ESCAPE + REPLACE_CR_TOKEN,
      ESC_ESCAPE + REPLACE_LF_TOKEN,
      ESC_ESCAPE + REPLACE_RIGHT_SQR_BRACKET_TOKEN,
      ESC_ESCAPE + REPLACE_LEFT_SQR_BRACKET_TOKEN
     );

implementation

const
  REG_DEFAULT_KEY = '@';
  REG_NO_VALUE = '~~REG~NO~VALUE~~' + #255;

  REG_INTEGER_VALUE_TOKEN = 'dword:';
  REG_BINARY_VALUE_TOKEN  = 'hex:';
  REG_EXPAND_STRING_VALUE_TOKEN = 'hex(2):';

{ TSclRegistryFile }

procedure TSclRegistryFile.AfterConstruction;
begin
  inherited;
  FAccess := KEY_ALL_ACCESS; // Not used in this implementation.
  FRootKey := HKEY_CURRENT_USER;
  FLazyWrite := TRUE; // Not used in this implementation.
end;


procedure TSclRegistryFile.BeforeDestruction;
begin
  inherited;
  if Assigned(FIniFile) then
    FIniFile.Free;
end;

procedure TSclRegistryFile.CloseKey;
begin
  FCurrentPath := '';
end;

function TSclRegistryFile.CreateKey(const Key: string): Boolean;
var
  ParentKey: string;
begin
  if not KeyExists(Key) then
  begin
    ParentKey := GetParentKey(Key);
    if ParentKey <> '' then
      CreateKey(ParentKey);
    GetIniFile.WriteString(GetKeyPathAsSection(Key), REG_DEFAULT_KEY, '""');
    GetIniFile.UpdateFile;
    FCurrentPath := GetPath(Key);
  end;
  Result := TRUE;
end;

function TSclRegistryFile.CurrentPathAsSection: string;
begin
  Result := GetKeyPathAsSection(FCurrentPath);
end;

function TSclRegistryFile.DeleteKey(const Key: string): Boolean;
var
  Counter: Integer;
  OldKey: string;
  Section: string;
  SubKeys: TStringList;
begin
  OldKey := FCurrentPath;
  try
    if KeyExists(Key) then
    begin
      FCurrentPath := GetPath(Key);
      SubKeys := TStringList.Create;
      try
        GetKeyNames(SubKeys);
        for Counter := 0 to SubKeys.Count -1 do
          DeleteKey(SubKeys[Counter]);
      finally
        SubKeys.Free;
      end;
      Section := GetKeyPathAsSection(FCurrentPath);
      GetIniFile.EraseSection(Section);
      GetIniFile.UpdateFile;
      Result := TRUE;
    end
    else
    begin
      Result := FALSE;
    end;
  finally
    if (OldKey <> Key) then
      FCurrentPath := OldKey
    else
      FCurrentPath := '';
  end;
end;

function TSclRegistryFile.DeleteValue(const Name: string): Boolean;
var
  Section: string;
begin
  if ValueExists(Name) then
  begin
    Section := GetKeyPathAsSection(FCurrentPath);
    GetIniFile.DeleteKey(Section, ValueNameToRawName(Name));
    GetIniFile.UpdateFile;
    Result := TRUE;
  end
  else
  begin
    Result := FALSE;
  end;
end;

function TSclRegistryFile.EscapedStringToStr(AString: string): string;
var
  Counter: TEscapedToken;
begin
  Result := AString;
  for Counter := Low(TEscapedToken) to High(TEscapedToken) do
  begin
    Result := StringReplace(Result, REPLACEMENT_TOKENS[Counter],
      ESCAPED_TOKENS[Counter], [rfReplaceAll]);
  end;
end;

function TSclRegistryFile.ExtractKeyName(Key: string): string;
begin
  Result := StrRestOf(Key, StrILastPos(sSlash, Key)+1);
end;

function TSclRegistryFile.GetAccess: LongWord;
begin
  Result := FAccess;
end;

function TSclRegistryFile.GetCurrentKey: HKEY;
begin
  Result := StrHash(GetKeyPathAsSection(FCurrentPath));
end;

function TSclRegistryFile.GetCurrentPath: string;
begin
  Result := FCurrentPath;
end;

function TSclRegistryFile.GetDataInfo(const ValueName: string;
  var Value: TRegDataInfo): Boolean;

  function BinaryDataByteCount(S: string): Integer;
  var
    ByteAsHexCharacters: string;
    Counter: Integer;
  begin
    Result := 0;
    Counter := 1;
    while (Counter < Length(S)) do
    begin
      if IsWhiteSpace(S[Counter]) then
        inc(Counter)
      else
      begin
        { TODO -oTT -cDAF: StrToInt is used to verfify that the
          ByteAsHexCharacters is actually a hex value. There's got to be
          a better way. }
        ByteAsHexCharacters := '$' + S[Counter] + S[Counter+1];
        StrToInt(ByteAsHexCharacters);
        inc(Counter, 3);
        inc(Result);
      end;
    end;
  end;

var
  ValueAsString: string;
begin
  if InternalReadInStringValue(CurrentPathAsSection, ValueName, ValueAsString, Value.RegData) then
  begin
    Result := TRUE;
    case Value.RegData of
      rdString       : Value.DataSize := Length(ValueAsString);
      rdExpandString : Value.DataSize := BinaryDataByteCount(ValueAsString);
      rdInteger      : Value.DataSize := SizeOf(Integer);
      rdBinary       : Value.DataSize := BinaryDataByteCount(ValueAsString);
    else
      Result := FALSE;
    end;
  end
  else
    Result := FALSE;
end;

function TSclRegistryFile.GetDataSize(const ValueName: string): Integer;
var
  Value: TRegDataInfo;
begin
  if GetDataInfo(ValueName, Value) then
    Result := Value.DataSize
  else
    Result := 0;
end;

function TSclRegistryFile.GetDataType(
  const ValueName: string): TRegDataType;
var
  Value: TRegDataInfo;
begin
  if GetDataInfo(ValueName, Value) then
    Result := Value.RegData
  else
    Result := rdUnknown;
end;

function TSclRegistryFile.GetPath(Key: string): string;
begin
  if IsRelative(Key) then
  begin
    Result := FCurrentPath + sSlash + Key
  end
  else
    Result := Key;
end;

function TSclRegistryFile.GetKeyPathAsSection(Key: string): string;
begin
  if FRootKey <> 0 then
    Result := HKEYToString(FRootKey) + StrToEscapedString(GetPath(Key))
  else
    Result := '';
end;

function TSclRegistryFile.GetIniFile: TIniFile;

  function GetDefaultIniFileName: string;
  begin
    Result := PathRemoveExtension(ParamStr(0));
    Result := PathAddExtension(Result, REG_FILE_EXTENSION);
  end;

begin
  if FIniFileName = '' then
    FIniFileName := GetDefaultIniFileName;

  if Assigned(FIniFile) and (FIniFile.FileName <> FIniFileName) then
    FreeAndNil(FIniFile);

  if not Assigned(FIniFile) then
    FIniFile := TIniFile.Create(FIniFileName);

  Result := FIniFile;
end;

function TSclRegistryFile.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
var
  Counter: Integer;
  SubKeys: TStringList;
  Values: TStringList;
begin
  SubKeys := TStringList.Create;
  Values := TStringList.Create;
  try
    { NumSubKeys }
    GetKeyNames(SubKeys);
    Value.NumSubKeys := SubKeys.Count;
    { MaxSubKeyLen }
    Value.MaxSubKeyLen := 0;
    for Counter := 0 to SubKeys.Count - 1 do
      if Length(SubKeys[Counter]) > Value.MaxSubKeyLen then
        Value.MaxSubKeyLen := Length(SubKeys[Counter]);
    { NumValues }
    GetValueNames(Values);
    Value.NumValues := Values.Count;
    { MaxValueLen / MaxDataLen }
    Value.MaxValueLen := 0;
    Value.MaxDataLen := 0;
    for Counter := 0 to Values.Count - 1 do
    begin
      if Length(Values[Counter]) > Value.MaxValueLen then
        Value.MaxValueLen := Length(Values[Counter]);
      if GetDataSize(Values[Counter]) > Value.MaxDataLen then
        Value.MaxDataLen := GetDataSize(Values[Counter]);
    end;
    { FileTime }
    Value.FileTime.dwLowDateTime := 0; { TODO -oTT -cNot implemented }
    Value.FileTime.dwHighDateTime := 0;
  finally
    SubKeys.Free;
    Values.Free;
  end;
  Result := TRUE;
end;

procedure TSclRegistryFile.GetKeyNames(Strings: TStrings);

  function IsSubSection(SubSection, RootSection: string): Boolean;
  begin
    if Pos(RootSection + sSlash, SubSection) = 1 then
      Result := Pos(sSlash, StrRestOf(SubSection, Length(RootSection)+2)) = 0
    else
      Result := FALSE;
  end;

var
  Counter: Integer;
  Sections: TStringList;
  BaseSection: string;
begin
  Assert(Strings <> nil, 'Strings cannot be nil');

  { TODO -oTT -cIncomplete: Not optimized }
  Strings.Clear;
  BaseSection := GetKeyPathAsSection(FCurrentPath);
  Sections := TStringList.Create;
  Sections.Sorted := TRUE;
  try
    GetIniFile.ReadSections(Sections);
    Counter := Sections.IndexOf(BaseSection);
    if Counter <> -1 then
    begin
      inc(Counter);
      while (Counter < Sections.Count) and (Sections[Counter] < (BaseSection + #255)) do
      begin
        if IsSubSection(Sections[Counter], BaseSection) then
        begin
          Strings.Add(ExtractKeyName(GetSectionAsKeyPath(Sections[Counter])));
        end;
        inc(Counter);
      end;
    end;
  finally
    Sections.Free;
  end;
end;

function TSclRegistryFile.GetLazyWrite: Boolean;
begin
  Result := FLazyWrite;
end;

function TSclRegistryFile.GetParentKey(Key: string): string;
var
  ChildKeyStartPos: Integer;
begin
  ChildKeyStartPos := StrLastPos(sSlash, Key);
  if ChildKeyStartPos > 1 then
    Result := StrLeft(Key, ChildKeyStartPos-1)
  else
    Result := '';
end;

function TSclRegistryFile.GetRootKey: HKEY;
begin
  Result := FRootKey;
end;

function TSclRegistryFile.GetSectionAsKeyPath(Section: string): string;

  function StripLeadingAndTrailingBrackets(S: string): string;
  begin
    Result := StrTrimCharRight(StrTrimCharLeft(S, '['), ']');
  end;

  function StripLeadingHKEYString(S: string): string;
  var
    Counter: Integer;
    HKEYAsString: string;
  begin
    Result := S;
    for Counter := Low(HKEYS) to High(HKEYS) do
    begin
      HKEYAsString := HKEYToString(HKEYS[Counter]);
      if Pos(HKEYAsString, Result) = 1 then
      begin
        Result := StrRestOf(Result, Length(HKEYAsString)+1);
        Exit;
      end;
    end;
  end;

begin
  Result := StripLeadingAndTrailingBrackets(Section);
  Result := StripLeadingHKEYString(Result);
  Result := EscapedStringToStr(Result);
end;

procedure TSclRegistryFile.GetValueNames(Strings: TStrings);
var
  Counter: Integer;
  Section: string;
  TempValueName: string;
  Values: TStringList;
begin
  Assert(Strings <> nil, 'Strings cannot be nil');

  Strings.Clear;
  Section := GetKeyPathAsSection(FCurrentPath);
  Values := TStringList.Create;
  try
    GetIniFile.ReadSectionValues(Section , Values);
    for Counter := 0 to Values.Count-1 do
    begin
      TempValueName := Values.Names[Counter];
      if TempValueName <> REG_DEFAULT_KEY then
      begin
        TempValueName := StrTrimQuotes(TempValueName);
        Strings.Add(TempValueName);
      end;
    end;
  finally
    Values.Free;
  end;
end;

function TSclRegistryFile.HasSubKeys: Boolean;
var
  TempKeys: TStringList;
begin
  TempKeys := TStringList.Create;
  try
    GetKeyNames(TempKeys);
    Result := TempKeys.Count > 0;
  finally
    TempKeys.Free;
  end;
end;

function TSclRegistryFile.InternalOpenKey(const Key: string;
  CanCreate: Boolean): Boolean;
begin
  if CanCreate = TRUE then
  begin
    if not KeyExists(Key) then
      CreateKey(Key)
    else
      FCurrentPath := GetPath(Key);
    Result := TRUE;
  end
  else
  begin
    if KeyExists(Key) then
    begin
      FCurrentPath := GetPath(Key);
      Result := TRUE;
    end
    else
      Result := FALSE;
  end;
end;

function TSclRegistryFile.IsWhiteSpace(AChar: Char): Boolean;
begin
  Result := (AChar = #32) or (AChar = #9) or (AChar = sSlash)
    or (AChar = #10) or (AChar = #13);
end;

function TSclRegistryFile.KeyExists(const Key: string): Boolean;
var
  Section: string;
begin
  Section := GetKeyPathAsSection(Key);
  Result := GetIniFile.SectionExists(Section);
end;

function TSclRegistryFile.LoadKey(const Key, FileName: string): Boolean;
begin
  raise Exception.Create('Not implemented');
  { TODO -oTT -cNot implemented }
end;

procedure TSclRegistryFile.MoveKey(const OldName, NewName: string;
  Delete: Boolean);

  procedure MoveSectionValues(OldSection, NewSection: string);
  var
    Counter: Integer;
    SectionValues: TStringList;
  begin
    SectionValues := TStringList.Create;
    try
      GetIniFile.ReadSectionValues(OldSection, SectionValues);
      for Counter := 0 to SectionValues.Count-1 do
      begin
        GetIniFile.WriteString(NewSection, SectionValues.Names[Counter],
          SectionValues.Values[SectionValues.Names[Counter]]);
      end;
      GetIniFile.UpdateFile;
    finally
      SectionValues.Free;
    end;
  end;

var
  Counter: Integer;
  Keys: TStringList;
  OldCurrentPath: string;
  OldNameFullPath: string;
  NewNameFullPath: string;
begin
  OldCurrentPath := FCurrentPath;
  try
    if KeyExists(OldName) and not KeyExists(NewName) then
    begin
      OldNameFullPath := GetPath(OldName);
      NewNameFullPath := GetPath(NewName);
      Keys := TStringList.Create;
      try
        OpenKey(OldNameFullPath, FALSE);
        GetKeyNames(Keys);
        for Counter := 0 to Keys.Count-1 do
        begin
          MoveKey(OldNameFullPath + sSlash + Keys[Counter], NewNameFullPath + sSlash + Keys[Counter], Delete);
          if Delete then
            DeleteKey(OldNameFullPath + sSlash + Keys[Counter]);
        end;

        MoveSectionValues(GetKeyPathAsSection(OldNameFullPath), GetKeyPathAsSection(NewNameFullPath));
        if Delete then
          DeleteKey(OldNameFullPath);
      finally
        Keys.Free;
      end;
    end;
  finally
    FCurrentPath := OldCurrentPath;
  end;
end;

function TSclRegistryFile.OpenKey(const Key: string;
  CanCreate: Boolean): Boolean;
begin
  Result := InternalOpenKey(Key, CanCreate);
  if Result = TRUE then
    FReadOnly := FALSE;
end;

function TSclRegistryFile.OpenKeyReadOnly(const Key: String): Boolean;
begin
  FReadOnly := TRUE;
  Result := InternalOpenKey(Key, FALSE);
end;

function TSclRegistryFile.ReadBinaryData(const Name: string; var Buffer;
  BufSize: Integer): Integer;
var
  Counter: Integer;
  Value: string;
  ValueType: TRegDataType;
  HexAsChars: string;
begin
//  Assert(@Pointer(Buffer) <> 0, 'Buffer cannot be a nil pointer.');

  Result := 0;
  if InternalReadInStringValue(CurrentPathAsSection, Name, Value, ValueType)
    and (ValueType = rdBinary) then
  begin
    Counter := 1;
    while (Counter < Length(Value)) and (Result <= BufSize) do
    begin
      if IsWhiteSpace(Value[Counter]) then
        inc(Counter)
      else
      begin
        HexAsChars := '$' + Value[Counter] + Value[Counter+1];
        inc(Counter, 3);
        inc(Result);
        if Result > BufSize then
          ReadError(Name);
        PChar(Buffer)[Result-1] := Char(StrToInt(HexAsChars));
      end;
    end;
  end
  else
    ReadError(Name);
end;

function TSclRegistryFile.ReadBool(const Name: string): Boolean;
begin
  Result := ReadInteger(Name) = 1;
end;

function TSclRegistryFile.ReadCurrency(const Name: string): Currency;
var
  Value: ^Currency;
begin
  Value := AllocMem(SizeOf(Currency));
  try
    ReadBinaryData(Name, Value, SizeOf(Currency));
    Result := Value^;
  finally
    FreeMem(Value);
  end;
end;

function TSclRegistryFile.ReadDate(const Name: string): TDateTime;
begin
  Result := ReadFloat(Name);
end;

function TSclRegistryFile.ReadDateTime(const Name: string): TDateTime;
begin
  Result := ReadFloat(Name);
end;

function TSclRegistryFile.ReadFloat(const Name: string): Double;
var
  Data: Pointer;
begin
  Result := 0;
  Data := @Result;
  ReadBinaryData(Name, Data, SizeOf(Double));
end;

function TSclRegistryFile.ReadInteger(const Name: string): Integer;
var
  Value: string;
  ValueType: TRegDataType;
begin
  Result := 0;
  if InternalReadInStringValue(CurrentPathAsSection, Name, Value, ValueType)
    and (ValueType = rdInteger) then
  begin
    Value := '$' + Value;
    Result := StrToInt64(Value);
  end
  else
    ReadError(Name);
end;

function TSclRegistryFile.ReadString(const Name: string): string;
var
  Value: string;
  ValueBuffer: PChar;
  ValueType: TRegDataType;
  RegDataInfo: TRegDataInfo;
begin
  result := '';
  if GetDataInfo(Name, RegDataInfo) then
  begin
    case RegDataInfo.RegData of
      rdString:
      begin
        InternalReadInStringValue(CurrentPathAsSection, Name, Value, ValueType);
        Result := Value;
      end;

      rdExpandString:
      begin
        ValueBuffer := AllocMem(RegDataInfo.DataSize);
        try
          ReadBinaryData(Name, ValueBuffer, RegDataInfo.DataSize);
          Result := ValueBuffer;
          if not ExpandEnvironmentVar(Result) then
            ReadError(Name);
        finally
          FreeMem(ValueBuffer);
        end;
      end;
    else
      ReadError(Name);
    end;
  end;
end;

function TSclRegistryFile.ReadTime(const Name: string): TDateTime;
begin
  Result := ReadFloat(Name);
end;

function TSclRegistryFile.RegistryConnect(const Name: string): Boolean;
begin
  FIniFileName := Name;
  Result := GetIniFile <> nil;
end;

function TSclRegistryFile.StrToEscapedString(
  const AString: string): string;
var
  Counter: TEscapedToken;
begin
  Assert(AMPERSAND_TOKEN = ESCAPED_TOKENS[Low(TEscapedToken)],
    'Ampersand must be first processed to avoid expanding other tokens');

  Result := AString;
  for Counter := Low(TEscapedToken) to High(TEscapedToken) do
  begin
    Result := StringReplace(Result, ESCAPED_TOKENS[Counter],
      REPLACEMENT_TOKENS[Counter], [rfReplaceAll]);
  end;
end;

function TSclRegistryFile.ValueNameToRawName(Name: string): string;
begin
  if (Name = '') or (Name = REG_DEFAULT_KEY) then
    Result := REG_DEFAULT_KEY
  else
    Result := StrDoubleQuote(StrToEscapedString(Name));
end;

function TSclRegistryFile.RawNameToValueName(RawName: string): string;
begin
  if (RawName = '') or (RawName = REG_DEFAULT_KEY) then
    Result := REG_DEFAULT_KEY
  else
    Result := StrTrimQuotes(EscapedStringToStr(RawName));
end;

function TSclRegistryFile.InternalReadInStringValue(const Section, Name: string; var Value: string;
  var RegDataType: TRegDataType): Boolean;
var
  RawValue: string;
  RawName: string;
begin
  RawName := ValueNameToRawName(Name);

  RawValue := GetIniFile.ReadString(Section, RawName, REG_NO_VALUE);

  if RawValue <> REG_NO_VALUE then
  begin
    StrTrimQuotes(RawValue);
    Result := TRUE;
    if Pos(REG_INTEGER_VALUE_TOKEN, RawValue) = 1 then
    begin
      RegDataType := rdInteger;
      Value := StrRestOf(RawValue, Length(REG_INTEGER_VALUE_TOKEN)+1);
    end
    else if Pos(REG_BINARY_VALUE_TOKEN, RawValue) = 1 then
    begin
      RegDataType := rdBinary;
      Value := StrRestOf(RawValue, Length(REG_BINARY_VALUE_TOKEN)+1);
    end
    else if Pos(REG_EXPAND_STRING_VALUE_TOKEN, RawValue) = 1 then
    begin
      RegDataType := rdExpandString;

      Value := StrRestOf(RawValue, Length(REG_EXPAND_STRING_VALUE_TOKEN)+1);
    end
    else
    begin
      RegDataType := rdString;
      Value := EscapedStringToStr(RawValue);
    end;
  end
  else
    Result := FALSE;
end;


function TSclRegistryFile.InternalValueExists(const Section,
  Name: string): Boolean;
var
  RawName: string;
begin
  RawName := ValueNameToRawName(Name);
  Result := GetIniFile.ValueExists(Section, RawName);
end;

procedure TSclRegistryFile.InternalWriteBinaryData(const Name: string;
  var Buffer; BufSize: Integer; IsExpandString: Boolean);
var
  CharAsByte: Byte;
  Counter: Integer;
  RegDataType: TRegDataType;
  ValueAsString: string;
begin
  ValueAsString := '';
  for Counter := 0 to BufSize-1 do
  begin
    CharAsByte := Integer(PChar(Buffer)[Counter]);
    ValueAsString := ValueAsString + IntToHex(CharAsByte, 2);
    if Counter < BufSize-1 then
      ValueAsString := ValueAsString + ',';
  end;

  if IsExpandString then
    RegDataType := rdExpandString
  else
    RegDataType := rdBinary;

  InternalWriteOutStringValue(CurrentPathAsSection, Name, ValueAsString, RegDataType);
end;


procedure TSclRegistryFile.InternalWriteOutStringValue(const Section, Name, Value: string;
  RegDataType: TRegDataType);
var
  RawName: string;
  RawValue: string;
begin
  Assert(Section <> '', 'Section cannot be blank');
  Assert(Name <> '', 'Name cannot be blank');

  if FReadOnly = TRUE then
    raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [Name]);

  RawName := ValueNameToRawName(Name);

  case RegDataType of
    rdUnknown        : RawValue := StrDoubleQuote(Value);
    rdString         : RawValue := StrDoubleQuote(StrToEscapedString(Value));
    rdExpandString   : RawValue := REG_EXPAND_STRING_VALUE_TOKEN + Value;
    rdInteger        : RawValue := REG_INTEGER_VALUE_TOKEN + Value;
    rdBinary         : RawValue := REG_BINARY_VALUE_TOKEN + Value;
  end;

  GetIniFile.WriteString(Section, RawName, RawValue);
  GetIniFile.UpdateFile;
end;

procedure TSclRegistryFile.RenameValue(const OldName, NewName: string);
var
  Value: string;
  RegDataType: TRegDataType;
begin
  if ValueExists(OldName) and not ValueExists(NewName) then
  begin
    if InternalReadInStringValue(CurrentPathAsSection, OldName, Value, RegDataType) then
    begin
      InternalWriteOutStringValue(CurrentPathAsSection, NewName, Value, RegDataType);
      DeleteValue(OldName);
    end;
  end;
end;

function TSclRegistryFile.ReplaceKey(const Key, FileName,
  BackUpFileName: string): Boolean;
begin
  raise Exception.Create('Not implemented');
  { TODO -oTT -cNot implemented }
end;

function TSclRegistryFile.RestoreKey(const Key, FileName: string): Boolean;
begin
  raise Exception.Create('Not implemented');
  { TODO -oTT -cNot implemented }
end;

function TSclRegistryFile.SaveKey(const Key, FileName: string): Boolean;
begin
  raise Exception.Create('Not implemented');
  { TODO -oTT -cNot implemented }
end;

procedure TSclRegistryFile.SetAccess(const Value: LongWord);
begin
  { TODO -oTT -cIncomplete: Access not implemented }
  FAccess := Value;
end;

procedure TSclRegistryFile.SetLazyWrite(const Value: Boolean);
begin
  { TODO -oTT -cIncomplete: LazyWrite not implemented }
  FLazyWrite := Value;
end;

procedure TSclRegistryFile.SetRootKey(const Value: HKEY);
begin
  if FRootKey <> Value then
  begin
    FRootKey := Value;
    CloseKey;
  end;
end;

function TSclRegistryFile.UnLoadKey(const Key: string): Boolean;
begin
  raise Exception.Create('Not implemented');
  { TODO -oTT -cNot implemented }
end;

function TSclRegistryFile.ValueExists(const Name: string): Boolean;
begin
  Result := InternalValueExists(CurrentPathAsSection, Name);
end;

procedure TSclRegistryFile.WriteBinaryData(const Name: string; var Buffer;
  BufSize: Integer);
begin
  InternalWriteBinaryData(Name, Buffer, BufSize, FALSE);
end;

procedure TSclRegistryFile.WriteBool(const Name: string; Value: Boolean);
begin
  WriteInteger(Name, ord(Value));
end;

procedure TSclRegistryFile.WriteCurrency(const Name: string;
  Value: Currency);
var
  ResultValue: ^Currency;
begin
  ResultValue := AllocMem(SizeOf(Currency));
  try
    ResultValue^ := Value;
    WriteBinaryData(Name, ResultValue, SizeOf(Currency));
  finally
    FreeMem(ResultValue);
  end;

end;

procedure TSclRegistryFile.WriteDate(const Name: string; Value: TDateTime);
begin
  WriteFloat(Name, Value);
end;

procedure TSclRegistryFile.WriteDateTime(const Name: string;
  Value: TDateTime);
begin
  WriteFloat(Name, Value);
end;

procedure TSclRegistryFile.WriteExpandString(const Name, Value: string);
var
  Buffer: PChar;
  BufferSize: Integer;
begin
  BufferSize := Length(Value) + 1;
  Buffer := AllocMem(BufferSize);
  try
    InternalWriteBinaryData(Name, Buffer, BufferSize, TRUE);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TSclRegistryFile.WriteFloat(const Name: string; Value: Double);
var
  Data: Pointer;
begin
  Data := @Value;
  WriteBinaryData(Name, Data, SizeOf(Double));
end;

procedure TSclRegistryFile.WriteInteger(const Name: string;
  Value: Integer);
var
  ValueAsString: string;
begin
  ValueAsString := IntToHex(Value, 8);
  InternalWriteOutStringValue(CurrentPathAsSection, Name, ValueAsString, rdInteger);
end;

procedure TSclRegistryFile.WriteString(const Name, Value: string);
begin
  InternalWriteOutStringValue(CurrentPathAsSection, Name, Value, rdString);
end;

procedure TSclRegistryFile.WriteTime(const Name: string; Value: TDateTime);
begin
  WriteDateTime(Name, Value);
end;



end.
