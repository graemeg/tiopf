unit Test_SclRegistry;

interface

uses
  Sysutils, Windows, Classes, Dialogs, Math, Registry, JCLSysInfo,
  TestFrameWork,  SclRegistry;

type
  TTestSclRegistryUtilityMethods = class(TTestCase)
  published
    procedure VerifyIsRelative;
    procedure VerifyRegDataToDataType;
    procedure VerifyDataTypeToRegData;
    procedure VerifyHKEYToString;
    procedure VerifyStringToHKEY;
  end;

  TTestSclRegistry = class(TTestCase) { abstract }
  private
    FRegistry: IRegistry;
  protected
    function BuildRegistry: IRegistry; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    property Registry: IRegistry read FRegistry; 
  published
    procedure VerifyCloseKey; dynamic;
    procedure VerifyCreateKey; dynamic;
    procedure VerifyDeleteKey; dynamic;
    procedure VerifyDeleteValue; dynamic;
    procedure VerifyGetCurrentKey; dynamic;
    procedure VerifyGetCurrentPath; dynamic;
    procedure VerifyGetDataInfo; dynamic;
    procedure VerifyGetDataSize; dynamic;
    procedure VerifyGetDataType; dynamic;
    procedure VerifyGetKeyInfo; dynamic;
    procedure VerifyGetKeyNames; dynamic;
    procedure VerifyGetValueNames; dynamic;
    procedure VerifyHasSubKeys; dynamic;
    procedure VerifyKeyExists; dynamic;
    procedure VerifyLoadKey; dynamic;
    procedure VerifyMoveKey; dynamic;
    procedure VerifyOpenKey; dynamic;
    procedure VerifyOpenKeyReadOnly; dynamic;
    procedure VerifyRenameValue; dynamic;
    procedure VerifyReplaceKey; dynamic;
    procedure VerifyRestoreKey; dynamic;
    procedure VerifySaveKey; dynamic;
    procedure VerifySetGetAccess; dynamic;
    procedure VerifySetGetLazyWrite; dynamic;
    procedure VerifySetGetRootKey; dynamic;
    procedure VerifySwitchRootKey; dynamic;
    procedure VerifyUnLoadKey; dynamic;
    procedure VerifyValueExists; dynamic;
    procedure VerifyReadWriteCurrency; dynamic;
    procedure VerifyReadWriteBinaryData; dynamic;
    procedure VerifyReadWriteBool; dynamic;
    procedure VerifyReadWriteDate; dynamic;
    procedure VerifyReadWriteDateTime; dynamic;
    procedure VerifyReadWriteFloat; dynamic;
    procedure VerifyReadWriteInteger; dynamic;
    procedure VerifyReadWriteString; dynamic;
    procedure VerifyReadWriteExpandString; dynamic;
    procedure VerifyReadWriteTime; dynamic;
    procedure VerifyReadDefaultString; dynamic;
    procedure VerifySeperateInstancesAreThreadSafe; dynamic;
    procedure VerifyCreateKeyCreatesAllKeysInPath; dynamic;
  end;

implementation

type
  TRegistryAccessThread = class(TThread)
  private
    FRegistry: IRegistry;
  protected
    procedure Execute; override;
  public
    constructor Create(Registry: IRegistry);
  end;

{ TTestSclRegistryUtilityMethods }

procedure TTestSclRegistryUtilityMethods.VerifyDataTypeToRegData;
var
  REG_UNKNOWN: Integer;
begin
  REG_UNKNOWN := High(Integer);

  Check(DataTypeToRegData(REG_SZ) = rdString,
    'DataTypeToRegData did not return expected value.');

  Check(DataTypeToRegData(REG_EXPAND_SZ) = rdExpandString,
    'DataTypeToRegData did not return expected value.');

  Check(DataTypeToRegData(REG_DWORD) = rdInteger,
    'DataTypeToRegData did not return expected value.');

  Check(DataTypeToRegData(REG_BINARY) = rdBinary,
    'DataTypeToRegData did not return expected value.');

  Check(DataTypeToRegData(REG_UNKNOWN) = rdUnknown,
    'DataTypeToRegData did not return expected value.');
end;

procedure TTestSclRegistryUtilityMethods.VerifyHKEYToString;
begin
  Check(HKEYToString(HKEY_CLASSES_ROOT) = 'HKEY_CLASSES_ROOT',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_CURRENT_USER) = 'HKEY_CURRENT_USER',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_LOCAL_MACHINE) = 'HKEY_LOCAL_MACHINE',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_USERS) = 'HKEY_USERS',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_PERFORMANCE_DATA) = 'HKEY_PERFORMANCE_DATA',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_CURRENT_CONFIG) = 'HKEY_CURRENT_CONFIG',
    'HKEYToString did not return expected value.');

  Check(HKEYToString(HKEY_DYN_DATA) = 'HKEY_DYN_DATA',
    'HKEYToString did not return expected value.');
end;

procedure TTestSclRegistryUtilityMethods.VerifyIsRelative;
const
  RELATIVE_PATH = 'test';
  NOT_RELATIVE_PATH = '\test';
begin
  Check(IsRelative(RELATIVE_PATH), 'Incorrect value for relative path');
  Check(not IsRelative(NOT_RELATIVE_PATH), 'Incorrect value for non-relative path');
end;

procedure TTestSclRegistryUtilityMethods.VerifyRegDataToDataType;
begin
  Check(RegDataToDataType(rdString) = REG_SZ,
    'RegDataToDataType did not return expected value.');

  Check(RegDataToDataType(rdExpandString) = REG_EXPAND_SZ,
    'RegDataToDataType did not return expected value.');

  Check(RegDataToDataType(rdInteger) = REG_DWORD,
    'RegDataToDataType did not return expected value.');

  Check(RegDataToDataType(rdBinary) = REG_BINARY,
    'RegDataToDataType did not return expected value.');

  Check(RegDataToDataType(rdUnknown) = REG_NONE,
    'RegDataToDataType did not return expected value.');
end;

procedure TTestSclRegistryUtilityMethods.VerifyStringToHKEY;
begin
  Check(StringToHKEY('HKEY_CLASSES_ROOT') = HKEY_CLASSES_ROOT,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_CURRENT_USER') = HKEY_CURRENT_USER,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_LOCAL_MACHINE') = HKEY_LOCAL_MACHINE,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_USERS') = HKEY_USERS,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_PERFORMANCE_DATA') = HKEY_PERFORMANCE_DATA,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_CURRENT_CONFIG') = HKEY_CURRENT_CONFIG,
    'StringToHKEY did not return expected value.');
  Check(StringToHKEY('HKEY_DYN_DATA') = HKEY_DYN_DATA,
    'StringToHKEY did not return expected value.');
end;

{ TTestSclRegistry }

procedure TTestSclRegistry.SetUp;
begin
  inherited;
  FRegistry := BuildRegistry;
end;

procedure TTestSclRegistry.TearDown;
begin
  inherited;
  FRegistry := nil;
end;

procedure TTestSclRegistry.VerifyCloseKey;
begin
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.CloseKey;
    Check(FRegistry.GetCurrentPath = '', 'Key has not closed.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyCreateKey;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  if FRegistry.KeyExists('\Test') then
    FRegistry.DeleteKey('\Test');

  FRegistry.CreateKey('\Test');
  Check(FRegistry.KeyExists('\Test'), 'Failed to create key.');
  FRegistry.CreateKey('Subkey');
  Check(FRegistry.KeyExists('\Test\Subkey'), 'Failed to create key.');
  FRegistry.DeleteKey('\Test');
end;

procedure TTestSclRegistry.VerifyDeleteKey;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.CreateKey('\Test');
  FRegistry.CreateKey('Subkey1');
  FRegistry.OpenKey('\Test', FALSE);
  Check(FRegistry.KeyExists('Subkey1'), 'Failed to create key.');
  FRegistry.DeleteKey('Subkey1');
  Check(not FRegistry.KeyExists('Subkey1'), 'Failed to delete relative key.');
  FRegistry.CreateKey('Subkey2');
  FRegistry.DeleteKey('\Test');
  Check(not FRegistry.KeyExists('\Test'), 'Failed to delete root key.');
  Check(not FRegistry.KeyExists('\Test\Subkey2'), 'Failed to delete subkey.');
end;

procedure TTestSclRegistry.VerifyDeleteValue;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.CreateKey('\Test');
  FRegistry.CreateKey('Subkey1');
  try
    FRegistry.WriteString('String', 'Test string');
    Check(FRegistry.ValueExists('String'), 'Failed to create string value.');
    FRegistry.WriteInteger('Integer', 1234);
    Check(FRegistry.ValueExists('Integer'), 'Failed to create integer value.');
    FRegistry.WriteFloat('Float', 1234.5678);
    Check(FRegistry.ValueExists('Float'), 'Failed to create float value.');

    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.OpenKey('Subkey1', FALSE);

    FRegistry.DeleteValue('String');
    Check(not FRegistry.ValueExists('String'), 'Failed to delete string value.');
    FRegistry.DeleteValue('Integer');
    Check(not FRegistry.ValueExists('Integer'), 'Failed to delete integer value.');
    FRegistry.DeleteValue('Float');
    Check(not FRegistry.ValueExists('Float'), 'Failed to delete float value.');
  finally
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.DeleteKey('Subkey1');
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyGetCurrentKey;
var
  TestKey: HKEY;
  SubKeyKey: HKEY;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    TestKey := FRegistry.GetCurrentKey;
    FRegistry.OpenKey('Subkey', TRUE);
    SubKeyKey := FRegistry.GetCurrentKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.CurrentKey = TestKey, 'CurrentKey is not expected value.');
    FRegistry.OpenKey('Subkey', FALSE);
    Check(FRegistry.CurrentKey = SubKeyKey, 'CurrentKey is not expected value.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyGetCurrentPath;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    Check(FRegistry.CurrentPath = '\Test', 'CurrentPath is not expected value.');
    FRegistry.OpenKey('Subkey', TRUE);
    Check(FRegistry.CurrentPath = '\Test\Subkey', 'CurrentPath is not expected value.');
    FRegistry.CloseKey;
    Check(FRegistry.CurrentPath = '', 'CurrentPath is not expected value.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyGetDataInfo;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyGetDataSize;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyGetDataType;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyGetKeyInfo;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyGetKeyNames;
var
  SubKeys: TStringList;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.OpenKey('Test_SubKey1', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.OpenKey('Test_SubKey2', TRUE);
    FRegistry.OpenKey('Test_SubKey2_SubKey2', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.OpenKey('Test_SubKey3', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    SubKeys := TStringList.Create;
    try
      { Check 'Test' keys }
      FRegistry.GetKeyNames(SubKeys);
      Check(SubKeys.Count = 3, 'Subkey count is not expected value.');
      Check(SubKeys.IndexOf('Test_SubKey1') <> -1, 'Missing Test_SubKey1 key');
      Check(SubKeys.IndexOf('Test_SubKey2') <> -1, 'Missing Test_SubKey2 key');
      Check(SubKeys.IndexOf('Test_SubKey3') <> -1, 'Missing Test_SubKey3 key');
      { Check 'Test\Test_SubKey2' keys }
      FRegistry.OpenKey('\Test\Test_SubKey2', FALSE);
      FRegistry.GetKeyNames(SubKeys);
      Check(SubKeys.Count = 1, 'Subkey count is not expected value.');
      Check(SubKeys.IndexOf('Test_SubKey2_SubKey2') <> -1, 'Missing Test_SubKey2_SubKey2 key');
    finally
      SubKeys.Free;
    end;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyGetValueNames;
var
  ValueNames: TStringList;
begin
  ValueNames := TStringList.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteString('String1', 'This is a string');
    FRegistry.WriteString('String2', 'This is a string');
    FRegistry.WriteString('String3', 'This is a string');

    FRegistry.OpenKey('Test_SubKey1', TRUE);
    FRegistry.WriteString('String', 'This is a string');
    FRegistry.WriteBool('Bool', TRUE);
    FRegistry.WriteFloat('Float', 12345.6789);
    FRegistry.WriteInteger('Integer', 1234567);


    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.GetValueNames(ValueNames);
    Check(ValueNames.Count = 3, 'Unexpected number of values found.');
    Check(ValueNames.IndexOf('String1') <> -1, '"String1" value name not found.');
    Check(ValueNames.IndexOf('String2') <> -1, '"String1" value name not found.');
    Check(ValueNames.IndexOf('String3') <> -1, '"String1" value name not found.');

    FRegistry.OpenKey('Test_SubKey1', FALSE);
    FRegistry.GetValueNames(ValueNames);
    Check(ValueNames.Count = 4, 'Unexpected number of values found.');
    Check(ValueNames.IndexOf('String') <> -1, '"String" value name not found.');
    Check(ValueNames.IndexOf('Bool') <> -1, '"Bool" value name not found.');
    Check(ValueNames.IndexOf('Float') <> -1, '"Float" value name not found.');
    Check(ValueNames.IndexOf('Integer') <> -1, '"Integer" value name not found.');
  finally
    ValueNames.Free;
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyHasSubKeys;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.OpenKey('Test_SubKey1', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.OpenKey('Test_SubKey2', TRUE);
    FRegistry.OpenKey('Test_SubKey2_SubKey2', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.OpenKey('Test_SubKey3', TRUE);
    FRegistry.CloseKey;

    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.HasSubKeys, 'HasSubKeys did not return expected value.');
    FRegistry.OpenKey('Test_SubKey2', FALSE);
    Check(FRegistry.HasSubKeys, 'HasSubKeys did not return expected value.');
    FRegistry.DeleteKey('Test_SubKey2_SubKey2');
    Check(not FRegistry.HasSubKeys, 'HasSubKeys did not return expected value.');
  finally
    FRegistry.OpenKey('\Test', TRUE);
  end;
end;

procedure TTestSclRegistry.VerifyKeyExists;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.OpenKey('Test_SubKey1', TRUE);
    FRegistry.OpenKey('Test_SubKey1_SubKey1', TRUE);
    FRegistry.CloseKey;
    { Verify using relative keys }
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.KeyExists('Test_SubKey1'), 'Key does not exist.');
    FRegistry.OpenKey('Test_SubKey1', FALSE);
    Check(FRegistry.KeyExists('Test_SubKey1_SubKey1'), 'Key does not exist.');
    { Verify using non-relative keys }
    Check(FRegistry.KeyExists('\Test'), 'Key does not exist.');
    Check(FRegistry.KeyExists('\Test\Test_SubKey1'), 'Key does not exist.');
    Check(FRegistry.KeyExists('\Test\Test_SubKey1\Test_SubKey1_SubKey1'), 'Key does not exist.');
    { Verify after deleting keys }
    FRegistry.DeleteKey('\Test\Test_SubKey1\Test_SubKey1_SubKey1');
    Check(not FRegistry.KeyExists('\Test\Test_SubKey1\Test_SubKey1_SubKey1'), 'Key still exists.');
    FRegistry.DeleteKey('\Test\Test_SubKey1');
    Check(not FRegistry.KeyExists('\Test\Test_SubKey1'), 'Key still exists.');
  finally
    FRegistry.OpenKey('\Test', TRUE);
  end;
end;

procedure TTestSclRegistry.VerifyLoadKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyMoveKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyOpenKey;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  { OpenKey using basic relative path (relative to root) }
  FRegistry.OpenKey('Test', TRUE);
  Check(FRegistry.CurrentPath = '\Test', 'CurrentPath is not exptected value');
  FRegistry.CloseKey;
  { OpenKey using non-relative path }
  FRegistry.OpenKey('\Test', TRUE);
  Check(FRegistry.CurrentPath = '\Test', 'CurrentPath is not exptected value');
  FRegistry.CloseKey;
  { Open nested keys }
  FRegistry.OpenKey('Test', TRUE);
  FRegistry.OpenKey('Test', TRUE);
  Check(FRegistry.CurrentPath = '\Test\Test', 'CurrentPath is not exptected value');
  FRegistry.CloseKey;
  { Cleanup }
  FRegistry.DeleteKey('Test');
end;

procedure TTestSclRegistry.VerifyOpenKeyReadOnly;
var
  RegistryExceptionOccured: Boolean;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.CloseKey;

    RegistryExceptionOccured := FALSE;
    FRegistry.OpenKeyReadOnly('\Test');
    try
      FRegistry.WriteString('String', 'This is a string');
    except
      on ERegistryException do
        RegistryExceptionOccured := TRUE;
    end;
    FRegistry.CloseKey;
    Check(RegistryExceptionOccured, 'Exception did not occur when writing to read only key.');

    RegistryExceptionOccured := FALSE;
    FRegistry.OpenKeyReadOnly('\Test');
    try
      FRegistry.WriteInteger('Integer', 12345678);
    except
      on ERegistryException do
        RegistryExceptionOccured := TRUE;
    end;
    FRegistry.CloseKey;
    Check(RegistryExceptionOccured, 'Exception did not occur when writing to read only key.');

    RegistryExceptionOccured := FALSE;
    FRegistry.OpenKeyReadOnly('\Test');
    try
      FRegistry.WriteFloat('Float', 123456.789);
    except
      on ERegistryException do
        RegistryExceptionOccured := TRUE;
    end;
    FRegistry.CloseKey;
    Check(RegistryExceptionOccured, 'Exception did not occur when writing to read only key.');

    FRegistry.OpenKey('\Test', FALSE);
    RegistryExceptionOccured := FALSE;
    try
      FRegistry.WriteString('String', 'This is a string');
    except
      on ERegistryException do
        RegistryExceptionOccured := TRUE;
    end;
    FRegistry.CloseKey;
    Check(not RegistryExceptionOccured, 'Exception occurred when writing to writeable key.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyRenameValue;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteString('String', 'This is the original string');
    FRegistry.RenameValue('String', 'NewString');
    Check(not FRegistry.ValueExists('String'), 'String has not been removed.');
    Check(FRegistry.ValueExists('NewString'), 'String has not been renamed.');
    Check(FRegistry.ReadString('NewString') = 'This is the original string',
      'String is not expected value.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReplaceKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyRestoreKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifySaveKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifySetGetAccess;
var
  Counter: Integer;
begin
  for Counter := Low(ACCESS_KEYS) to High(ACCESS_KEYS) do
  begin
    FRegistry.SetAccess(ACCESS_KEYS[Counter]);
    Check(FRegistry.GetAccess = ACCESS_KEYS[Counter], 'Access is not expected value.');
  end;  
end;

procedure TTestSclRegistry.VerifySetGetLazyWrite;
begin
  FRegistry.SetLazyWrite(TRUE);
  Check(FRegistry.GetLazyWrite = TRUE, 'LazyWrite is not expected value');
  FRegistry.SetLazyWrite(FALSE);
  Check(FRegistry.GetLazyWrite = FALSE, 'LazyWrite is not expected value');
end;

procedure TTestSclRegistry.VerifySetGetRootKey;
begin
  FRegistry.SetRootKey(HKEY_CLASSES_ROOT);
  Check(FRegistry.GetRootKey = HKEY_CLASSES_ROOT);
  FRegistry.SetRootKey(HKEY_CURRENT_USER);
  Check(FRegistry.GetRootKey = HKEY_CURRENT_USER);
  FRegistry.SetRootKey(HKEY_LOCAL_MACHINE);
  Check(FRegistry.GetRootKey = HKEY_LOCAL_MACHINE);
  FRegistry.SetRootKey(HKEY_USERS);
  Check(FRegistry.GetRootKey = HKEY_USERS);
  FRegistry.SetRootKey(HKEY_PERFORMANCE_DATA);
  Check(FRegistry.GetRootKey = HKEY_PERFORMANCE_DATA);
  FRegistry.SetRootKey(HKEY_DYN_DATA);
  Check(FRegistry.GetRootKey = HKEY_DYN_DATA);
end;

procedure TTestSclRegistry.VerifySwitchRootKey;
begin
  FRegistry.SetRootKey(HKEY_CLASSES_ROOT);
  FRegistry.OpenKey('\HKEY_CLASSES_ROOT_Test', TRUE);
  Check(FRegistry.KeyExists('\HKEY_CLASSES_ROOT_Test'), 'Key failed to create.');

  FRegistry.SetRootKey(HKEY_CURRENT_USER);
  FRegistry.OpenKey('\HKEY_CURRENT_USER_Test', TRUE);
  Check(FRegistry.KeyExists('\HKEY_CURRENT_USER_Test'), 'Key failed to create.');
  Check(not FRegistry.KeyExists('\HKEY_CLASSES_ROOT_Test'), 'Invalid key found.');

  FRegistry.SetRootKey(HKEY_CLASSES_ROOT);
  Check(FRegistry.KeyExists('\HKEY_CLASSES_ROOT_Test'), 'Key not found.');
  Check(not FRegistry.KeyExists('\HKEY_CURRENT_USER_Test'), 'Invalid key found.');

  FRegistry.DeleteKey('\HKEY_CLASSES_ROOT_Test');
  FRegistry.SetRootKey(HKEY_CURRENT_USER);
  FRegistry.DeleteKey('\HKEY_CURRENT_USER_Test');
end;

procedure TTestSclRegistry.VerifyUnLoadKey;
begin
  { TODO -oTT -cNot implemented }
end;

procedure TTestSclRegistry.VerifyValueExists;
begin
  FRegistry.SetRootKey(HKEY_CLASSES_ROOT);
  FRegistry.OpenKey('\Test', TRUE);
  FRegistry.WriteString('String', 'Test string');
  FRegistry.OpenKey('Subkey', TRUE);
  FRegistry.WriteInteger('Integer', 12345678);
  FRegistry.OpenKey('\Test', FALSE);
  Check(FRegistry.ValueExists('String'), 'Value not found.');
  Check(not FRegistry.ValueExists('Integer'), 'Invalid value found.');
  FRegistry.OpenKey('Subkey', FALSE);
  Check(FRegistry.ValueExists('Integer'), 'Value not found.');
  Check(not FRegistry.ValueExists('String'), 'Invalid value not found.');
end;

procedure TTestSclRegistry.VerifyReadWriteBinaryData;
const
  TEST_DATA: string = 'Test string';
var
  ReadBuffer: PChar;
  ReadBufferSize: Integer;
  WriteBuffer: PChar;
  WriteBufferSize: Integer;
begin
  WriteBufferSize := Length(TEST_DATA) + 1;
  ReadBuffer := AllocMem(WriteBufferSize);
  WriteBuffer := AllocMem(WriteBufferSize);
  try
    StrPCopy(WriteBuffer, TEST_DATA);
    FRegistry.RootKey := HKEY_CURRENT_USER;
    FRegistry.OpenKey('\Test', TRUE);
    try
      FRegistry.WriteBinaryData('BinaryData', WriteBuffer, WriteBufferSize);
      FRegistry.CloseKey;
      FRegistry.OpenKey('\Test', FALSE);
      ReadBufferSize := FRegistry.ReadBinaryData('BinaryData', ReadBuffer, WriteBufferSize);
      Check(ReadBufferSize = WriteBufferSize, 'ReadBufferSize and WriteBufferSize are different.');
      Check(ReadBuffer^ = WriteBuffer^, 'ReadBuffer and WriteBuffer contain different values.');
      FRegistry.CloseKey;
    finally
      FRegistry.DeleteKey('\Test');
    end;
  finally
    FreeMem(ReadBuffer);
    FreeMem(WriteBuffer);
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteBool;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteBool('Bool', TRUE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadBool('Bool') = TRUE, 'Bool is not expected value.');
    FRegistry.OpenKey('\Test', FALSE);
    FRegistry.WriteBool('Bool', FALSE);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadBool('Bool') = FALSE, 'Bool is not expected value.');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteCurrency;
var
  CurrencyValue: Currency;
begin
  CurrencyValue := 123456.78;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteCurrency('Currency', CurrencyValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadCurrency('Currency') = CurrencyValue, 'Currency is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteDate;
var
  DateValue: TDateTime;
begin
  DateValue := Date;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteDate('Date', DateValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadDate('Date') = DateValue, 'Date is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteDateTime;
var
  DateTimeValue: TDateTime;
begin
  DateTimeValue := Now;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteDate('DateTime', DateTimeValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadDate('DateTime') = DateTimeValue, 'DateTime is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteExpandString;
var
  StringValue: string;
  ExpandStringValue: string;
begin
  StringValue := 'Expand string containing %PATH%';

  ExpandStringValue := StringValue;
  if not ExpandEnvironmentVar(ExpandStringValue) then
    Fail('ExpandEnvironmentVar failed');

  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteString('ExpandString', ExpandStringValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadString('ExpandString') = ExpandStringValue, 'ExpandString is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteFloat;
var
  FloatValue: Double;
begin
  FloatValue := 123456.789;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteFloat('Float', FloatValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(abs(FRegistry.ReadFloat('Float') - FloatValue)<1E-10, 'Float is unexpected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteInteger;
var
  IntegerValue: Integer;
begin
  IntegerValue := 12345678;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteInteger('Integer', IntegerValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadInteger('Integer') = IntegerValue, 'Integer is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteString;
var
  StringValue: string;
begin
  StringValue := 'test string';

  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteString('String', StringValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadString('String') = StringValue, 'String is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyReadWriteTime;
var
  TimeValue: TDateTime;
begin
  TimeValue := Now;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    FRegistry.WriteTime('Time', TimeValue);
    FRegistry.CloseKey;
    FRegistry.OpenKey('\Test', FALSE);
    Check(FRegistry.ReadTime('Time') = TimeValue, 'Time is not expected value.');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;



procedure TTestSclRegistry.VerifySeperateInstancesAreThreadSafe;
var
  Counter: Integer;
  ThreadRegistry1: IRegistry;
  ThreadRegistry2: IRegistry;
  Thread1: TRegistryAccessThread;
  Thread2: TRegistryAccessThread;
  StartTime: TDateTime;
  EndTime: TDateTime;
  FloatValue: Double;
begin
  Check(TRUE);
  ThreadRegistry1 := BuildRegistry;
  ThreadRegistry2 := BuildRegistry;
  try
    Thread1 := TRegistryAccessThread.Create(ThreadRegistry1);
    Thread2 := TRegistryAccessThread.Create(ThreadRegistry2);
    try
      StartTime := Now;
      EndTime := StartTime + EncodeTime(0, 0, 5, 0);
      Counter := 0;
      Thread1.Resume;
      Thread2.Resume;
      repeat
        FloatValue := 123456.789;
        FRegistry.RootKey := HKEY_LOCAL_MACHINE;
        FRegistry.OpenKey('\ThreadTest', TRUE);
        FRegistry.OpenKey('\ThreadTest\Test\' + IntToStr(Counter), TRUE);
        FRegistry.WriteString('String', 'This is a test string');
        FRegistry.WriteFloat('Float', FloatValue);
        Check(FRegistry.ReadString('String') = 'This is a test string', 'String is unexpected value.');
        Check(abs(FRegistry.ReadFloat('Float') - FloatValue)<1E-10, 'Float is unexpected value.');
        FRegistry.CloseKey;
        inc(Counter);
        if Counter > 12 then
        begin
          FRegistry.DeleteKey('\ThreadTest\Test');
          Counter := 0;
        end;
      until Now > EndTime;
      Thread1.Terminate;
      Thread2.Terminate;
      FRegistry.DeleteKey('\ThreadTest');
    finally
      Thread1.Free;
      Thread2.Free;
    end;
  finally
    ThreadRegistry1 := nil;
    ThreadRegistry2 := nil;
  end;
end;

procedure TTestSclRegistry.VerifyReadDefaultString;
begin
  { Registry should return a empty string if the Name doesn't exist }
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\Test', TRUE);
  try
    Check(FRegistry.ReadString('BadName') = '', 'Failed to return default value');
    FRegistry.CloseKey;
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

procedure TTestSclRegistry.VerifyCreateKeyCreatesAllKeysInPath;
begin
  // Verify that creating a key creates all the root keys,
  // e.g. Create \HKEY_CURRENT_USER\Foo\Bar creates both
  // \HKEY_CURRENT_USER\Foo and \HKEY_CURRENT_USER\Foo\Bar
  FRegistry.RootKey := HKEY_CURRENT_USER;
  if FRegistry.KeyExists('\Test') then
    FRegistry.DeleteKey('\Test');
  Check(not FRegistry.KeyExists('\Test'), 'Key \Test still exists');

  FRegistry.OpenKey('\Test\Foo\Bar', TRUE);
  try
    Check(FRegistry.KeyExists('\Test\Foo\Bar'), 'Key \Test\Foo\Bar not created');
    Check(FRegistry.KeyExists('\Test\Foo'), 'Key \Test\Foo not created');
    Check(FRegistry.KeyExists('\Test'), 'Key \Test not created');
  finally
    FRegistry.DeleteKey('\Test');
  end;
end;

{ TRegistryAccessThread }

constructor TRegistryAccessThread.Create(Registry: IRegistry);
begin
  FRegistry := Registry;
  inherited Create(TRUE);
end;

procedure TRegistryAccessThread.Execute;
var
  Counter: Integer;
begin
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('\ThreadTest', TRUE);
  Counter := 0;
  repeat
    FRegistry.OpenKey('\ThreadTest\Test\' + IntToStr(Counter), TRUE);
    FRegistry.WriteString('String', 'This is a test string');
    FRegistry.WriteFloat('Float', 1234567.89);
    FRegistry.CloseKey;
    inc(Counter);
    if Counter > 12 then
    begin
      FRegistry.DeleteKey('\ThreadTest\Test');
      Counter := 0;
    end;
  until Terminated;
  FRegistry.DeleteKey('\ThreadTest');
end;

initialization
  TestFramework.RegisterTest(TTestSclRegistryUtilityMethods.Suite);

end.
