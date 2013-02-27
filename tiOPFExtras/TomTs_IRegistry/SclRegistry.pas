{*******************************************************}
{                                                       }
{       Sage Code Library - RegistryAPI                 }
{       Copyright (c) 2001 Sage Automation              }
{                                                       }
{       Based on VCL unit Registry.pas                  }
{       Copyright (c) 1995,98 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit sclRegistry;

interface

uses
  Classes, SysUtils, Windows, Registry, Consts, JclStrings, SclConsts;

type
  IRegistry = interface (IUnknown)
    ['{2FA13A2E-6DD5-4D41-84DD-C50E538A4294}']
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
    property CurrentKey: HKEY read GetCurrentKey;
    property CurrentPath: string read GetCurrentPath;
    property LazyWrite: Boolean read GetLazyWrite write SetLazyWrite;
    property RootKey: HKEY read GetRootKey write SetRootKey;
    property Access: LongWord read GetAccess write SetAccess;
  end;

  IRegIniFile = interface(IRegistry)
    ['{40102B75-3823-43D6-A9FC-29AF811DEC1C}']
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    function ReadString(const Section, Ident, Default: string): string; stdcall;
    function ReadInteger(const Section, Ident: string;
      Default: Longint): Longint; stdcall;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); stdcall;
    procedure WriteString(const Section, Ident, Value: String); stdcall;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; stdcall;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); stdcall;
    procedure ReadSection(const Section: string; Strings: TStrings); stdcall;
    procedure ReadSections(Strings: TStrings); stdcall;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); stdcall;
    procedure EraseSection(const Section: string); stdcall;
    procedure DeleteKey(const Section, Ident: String); stdcall;
    property FileName: string read GetFileName write SetFileName;
  end;

const
  HKEYS: array[0..6] of HKEY = ( HKEY_CLASSES_ROOT,
                                 HKEY_CURRENT_USER,
                                 HKEY_LOCAL_MACHINE,
                                 HKEY_USERS,
                                 HKEY_PERFORMANCE_DATA,
                                 HKEY_CURRENT_CONFIG,
                                 HKEY_DYN_DATA );


  ACCESS_KEYS: array[0..9] of LongWord = ( KEY_QUERY_VALUE,
                                           KEY_SET_VALUE,
                                           KEY_CREATE_SUB_KEY,
                                           KEY_ENUMERATE_SUB_KEYS,
                                           KEY_NOTIFY,
                                           KEY_CREATE_LINK,
                                           KEY_READ,
                                           KEY_WRITE,
                                           KEY_EXECUTE,
                                           KEY_ALL_ACCESS );

function IsRelative(const Value: string): Boolean;
function RegDataToDataType(Value: TRegDataType): Integer;
function DataTypeToRegData(Value: Integer): TRegDataType;

function HKEYToString(Key: HKEY): string;
function StringToHKEY(S: string): HKEY;

procedure ReadError(const Name: string);

implementation

function IsRelative(const Value: string): Boolean;
begin
  Result := not ((Value <> '') and (Value[1] = sSlash));
end;

function RegDataToDataType(Value: TRegDataType): Integer;
begin
  case Value of
    rdString: Result := REG_SZ;
    rdExpandString: Result := REG_EXPAND_SZ;
    rdInteger: Result := REG_DWORD;
    rdBinary: Result := REG_BINARY;
  else
    Result := REG_NONE;
  end;
end;

function DataTypeToRegData(Value: Integer): TRegDataType;
begin
  if Value = REG_SZ then Result := rdString
  else if Value = REG_EXPAND_SZ then Result := rdExpandString
  else if Value = REG_DWORD then Result := rdInteger
  else if Value = REG_BINARY then Result := rdBinary
  else Result := rdUnknown;
end;

function HKEYToString(Key: HKEY): string;
begin
  case Key of
    HKEY_CLASSES_ROOT     : Result := 'HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER     : Result := 'HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE    : Result := 'HKEY_LOCAL_MACHINE';
    HKEY_USERS            : Result := 'HKEY_USERS';
    HKEY_PERFORMANCE_DATA : Result := 'HKEY_PERFORMANCE_DATA';
    HKEY_CURRENT_CONFIG   : Result := 'HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA         : Result := 'HKEY_DYN_DATA';
  else
    raise EConvertError.Create('Unknown HKEY');
  end;
end;

function StringToHKEY(S: string): HKEY;
begin
  if S = 'HKEY_CLASSES_ROOT' then
    Result := HKEY_CLASSES_ROOT
  else if S = 'HKEY_CURRENT_USER' then
    Result := HKEY_CURRENT_USER
  else if S = 'HKEY_LOCAL_MACHINE' then
    Result := HKEY_LOCAL_MACHINE
  else if S = 'HKEY_USERS' then
    Result := HKEY_USERS
  else if S = 'HKEY_PERFORMANCE_DATA' then
    Result := HKEY_PERFORMANCE_DATA
  else if S = 'HKEY_CURRENT_CONFIG' then
    Result := HKEY_CURRENT_CONFIG
  else if S = 'HKEY_DYN_DATA' then
    Result := HKEY_DYN_DATA
  else
    raise EConvertError.Create('Unknown key ' + StrSingleQuote(S));
end;

procedure ReadError(const Name: string);
begin
  raise ERegistryException.CreateResFmt(@SInvalidRegType, [Name]);
end;

end.

