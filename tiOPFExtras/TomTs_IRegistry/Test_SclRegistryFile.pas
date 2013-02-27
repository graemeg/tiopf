unit Test_SclRegistryFile;

interface

uses
  Windows, Sysutils, TestFramework, JclFileUtils, Test_SclRegistry, SclRegistry,
  SclRegistryFile, Dialogs;

type
  TTestSclRegistryFile = class(TTestSclRegistry)
  private
    procedure CheckReadWriteAllValueTypes(const KeyName: string);
  protected
    function BuildRegistry: IRegistry; override;
  published
    procedure VerifyRegistryConnect;
    procedure VerifyReadWriteLargeBuffer;
    procedure VerifyReadWriteStringWithCarriageReturns;
    procedure VerifyReadWriteStringWithEscapedTokens;
    procedure VerifyReadWriteKeyWithEscapedTokens;
    procedure VerifyReadWriteKeyWithEndBracket;
  end;

implementation
uses classes;
{ TTestSclRegistryFile }

function TTestSclRegistryFile.BuildRegistry: IRegistry;
begin
  Result := TSclRegistryFile.Create;
end;

procedure TTestSclRegistryFile.CheckReadWriteAllValueTypes(const KeyName: string);
const
  EXPANDED_STRING_VALUE: string = 'Hope the %PATH% doesn''t change while testing';
  INTEGER_VALUE: Integer = 1234;
  STRING_VALUE: string = 'This is string value, a fabulous string value';
  FLOAT_VALUE: Double = 1234.5678;
  BOOLEAN_VALUE: Boolean = TRUE;
begin
  { TODO -oTT -cIncomplete: This really should be expanded to test a range of
    known values for each key type. One possibility is to generate test data from
    a seed, e.g. a random number seed against a known psuedo-random number
    generator algorithm -> the same seed will produce the same set of psuedo-
    random numbers. Tricker to do with string sets, still possible I'm sure. }
  Registry.OpenKey('\Test', TRUE);
  try
    { TODO -oTT -cIncomplete: Not testing expanded string yet }
 //   Registry.WriteExpandString('ExpandedStringValue', EXPANDED_STRING_VALUE);
    Registry.WriteInteger('IntegerValue', INTEGER_VALUE);
    Registry.WriteString('StringValue', STRING_VALUE);
    Registry.WriteFloat('FloatValue', FLOAT_VALUE);
    Registry.WriteBool('BooleanValueTRUE', TRUE);
    Registry.WriteBool('BooleanValueFALSE', FALSE);

    Registry.CloseKey;

    Registry.OpenKey('\Test', FALSE);
    Check(Registry.ReadInteger('IntegerValue') = INTEGER_VALUE);
    Check(Registry.ReadString('StringValue') = STRING_VALUE);
    Check(Registry.ReadFloat('FloatValue') = FLOAT_VALUE);
    Check(Registry.ReadBool('BooleanValueTRUE') = TRUE);
    Check(Registry.ReadBool('BooleanValueFALSE') = FALSE);

  finally
    Registry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistryFile.VerifyReadWriteStringWithEscapedTokens;
var
  Counter: TEscapedToken;
  RegValueWithSingleToken: string;
  RegValueWithMultipleTokens: string;
const
  REG_SINGLE_TOKEN: string = 'StringWithSingleToken';
  REG_MULTIPLE_TOKENS: string = 'StringWithMultipleToken';
begin
  for Counter := Low(TEscapedToken) to High(TEscapedToken) do
  begin
    RegValueWithSingleToken := Format('This is a string %s with an expandable token',
      [ESCAPED_TOKENS[Counter]]);

    RegValueWithMultipleTokens := Format('This is a string %s with %s multiple %s expandable %s tokens',
      [ESCAPED_TOKENS[Counter], ESCAPED_TOKENS[Counter],
      ESCAPED_TOKENS[Counter], ESCAPED_TOKENS[Counter]]);

    Registry.RootKey := HKEY_CURRENT_USER;
    Registry.OpenKey('\TestString', TRUE);
    try
      Registry.WriteString(REG_SINGLE_TOKEN, RegValueWithSingleToken);
      Registry.WriteString(REG_MULTIPLE_TOKENS, RegValueWithMultipleTokens);
      Registry.CloseKey;

      Check(Registry.OpenKey('\TestString', FALSE), 'Key failed to reopen');
      Check(Registry.ReadString(REG_SINGLE_TOKEN) = RegValueWithSingleToken,
        'String is not expected value');
      Check(Registry.ReadString(REG_MULTIPLE_TOKENS) = RegValueWithMultipleTokens,
        'String is not expected value');
    finally
      Registry.DeleteKey('\TestString');
    end;
  end;
end;

procedure TTestSclRegistryFile.VerifyReadWriteKeyWithEscapedTokens;
var
  Counter: TEscapedToken;
  KeyWithSingleToken: string;
  KeyNameWithMultipleTokens: string;
begin
  for Counter := Low(TEscapedToken) to High(TEscapedToken) do
  begin
    KeyWithSingleToken := Format('KeyWithSingle%sExpandedToken',
      [ESCAPED_TOKENS[Counter]]);

    KeyNameWithMultipleTokens := Format('Key%sWith%sMultiple%sExpanded%sTokens',
      [ESCAPED_TOKENS[Counter], ESCAPED_TOKENS[Counter],
      ESCAPED_TOKENS[Counter], ESCAPED_TOKENS[Counter]]);

    CheckReadWriteAllValueTypes(KeyWithSingleToken);
    CheckReadWriteAllValueTypes(KeyWithSingleToken + '/' + 'NonExpandingSubkey');
    CheckReadWriteAllValueTypes(KeyWithSingleToken + '/' +
      Format('ExpandingSubkey%s', [ESCAPED_TOKENS[Counter]]));

    CheckReadWriteAllValueTypes(KeyNameWithMultipleTokens);
    CheckReadWriteAllValueTypes(KeyNameWithMultipleTokens + '/' + 'NonExpandingSubkey');
    CheckReadWriteAllValueTypes(KeyNameWithMultipleTokens + '/' +
      Format('Expanding%sSubkey%s', [ESCAPED_TOKENS[Counter], ESCAPED_TOKENS[Counter]]));
  end;
end;

procedure TTestSclRegistryFile.VerifyReadWriteLargeBuffer;
var
  Buffer: PChar;
  Counter: Integer;
begin
  Buffer := AllocMem(1024);
  for Counter := 0 to 1023 do
  begin
    Buffer[Counter] := Char('A');
  end;
  Registry.RootKey := HKEY_CURRENT_USER;
  Registry.OpenKey('\TestBuffer', TRUE);
  Registry.WriteBinaryData('Buffer', Buffer, 1024);
  { TODO -oTT -cIncomplete : Add check code you dummy }
end;

procedure TTestSclRegistryFile.VerifyReadWriteStringWithCarriageReturns;
const
  SINGLE_CR_IN_STRING = 'This is a string' + #13 +
    'with a carriage return';
  SINGLE_CR_IN_STRING_VALUE = 'StringWithSingleCR';
  MULTIPLE_CR_IN_STRING = 'This is a string' + #13 +
    'with' + #13 + 'multiple' + #13 + 'carriage' + #13 + 'returns';
  MULTIPLE_CR_IN_STRING_VALUE = 'StringWithMultipleCarriageReturns';
begin
  Registry.RootKey := HKEY_CURRENT_USER;
  Registry.OpenKey('\TestString', TRUE);
  try
    Registry.WriteString(SINGLE_CR_IN_STRING_VALUE, SINGLE_CR_IN_STRING);
    Registry.WriteString(MULTIPLE_CR_IN_STRING_VALUE, MULTIPLE_CR_IN_STRING);
    Registry.CloseKey;

    Check(Registry.OpenKey('\TestString', FALSE), 'Key failed to reopen');
    Check(Registry.ReadString(SINGLE_CR_IN_STRING_VALUE) = SINGLE_CR_IN_STRING,
      'String is not expected value');
    Check(Registry.ReadString(MULTIPLE_CR_IN_STRING_VALUE) = MULTIPLE_CR_IN_STRING,
      'String is not expected value');
  finally
    Registry.DeleteKey('\TestString');
  end;
end;

procedure TTestSclRegistryFile.VerifyRegistryConnect;
var
  RegistryFileName1: TFileName;
  RegistryFileName2: TFileName;
begin
  inherited;
  RegistryFileName1 := PathRemoveExtension(ParamStr(0));
  RegistryFileName1 := PathAddExtension(RegistryFileName1 + '1', REG_FILE_EXTENSION);
  RegistryFileName2 := PathRemoveExtension(ParamStr(0));
  RegistryFileName2 := PathAddExtension(RegistryFileName2 + '2', REG_FILE_EXTENSION);

  Check(Registry.RegistryConnect(RegistryFileName1), 'Registry failed to connect.');
  Registry.RootKey := HKEY_LOCAL_MACHINE;
  Registry.OpenKey('\Test1', TRUE);

  Check(Registry.RegistryConnect(RegistryFileName2), 'Registry failed to connect.');
  Registry.RootKey := HKEY_LOCAL_MACHINE;
  Registry.OpenKey('\Test2', TRUE);

  Registry.RegistryConnect(RegistryFileName1);
  Check(Registry.KeyExists('\Test1'), 'Failed to find valid key.');
  Check(not Registry.KeyExists('\Test2'), 'Found invalid key.');

  Registry.RegistryConnect(RegistryFileName2);
  Check(Registry.KeyExists('\Test2'), 'Failed to find valid key.');
  Check(not Registry.KeyExists('\Test1'), 'Found invalid key.');

  { Clean up }
  Registry.DeleteKey('\Test2');
  Registry.RegistryConnect(RegistryFileName1);
  Registry.DeleteKey('\Test1');
  Registry.RegistryConnect('');
end;

procedure TTestSclRegistryFile.VerifyReadWriteKeyWithEndBracket;
var TestRegistry: IRegistry;
    FileName, TestKey, BaseKey, SubKey: String;
    KeyNames: TStringList;
begin
  FileName := ExtractFilePath(ParamStr(0)) + '\TESTREG.REG';
  BaseKey := '\Base';
  SubKey := 'Test]';
  TestKey := BaseKey + '\' + SubKey;
  if FileExists(FileName) then
    DeleteFile(FileName);
  TestRegistry := BuildRegistry;
  TestRegistry.RegistryConnect(FileName);
  TestRegistry.OpenKey(TestKey, TRUE);
  TestRegistry.WriteInteger('IntegerValue', 1);
  TestRegistry.CloseKey;
  TestRegistry.OpenKey(TestKey, TRUE);
  TestRegistry.WriteString('StringValue', 'Test');
  TestRegistry.CloseKey;
  KeyNames := TStringList.Create;
  try
  TestRegistry.OpenKey(BaseKey, False);
  TestRegistry.GetKeyNames(KeyNames);
  Check(KeyNames.Count = 1, 'Incorrect Keys created');
  Check(KeyNames[0] = SubKey, 'Key Incorrectly Returned on read');
  finally
    KeyNames.Free;
  end;
end;

initialization
  TestFramework.RegisterTest(TTestSclRegistryFile.Suite);

end.
