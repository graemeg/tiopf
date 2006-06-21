unit SclNet;

interface

uses
  Classes, SysUtils, Dialogs, JclStrings;

type
  EInvalidNetAddress = class(Exception);

  TNetAddressType = (natIPAddress, natServerName);

  procedure DecodeNetAddress(const Address: string; var AddressType: TNetAddressType;
      var AddressName: string; var AddressPort: Integer);

const
  PORT_DELIMITER = ':';

implementation

function ExtractPortFromNetAddress(const Address: String): Integer;
var
  DelimiterPos: Integer;
  sPort: String;
begin
  { Port Value is the portion of the address after the Port Delimiter if present
    and is a value between 1 and 9999. If no port value is present zero (0) is
    resturned. If a port value exists outside of the allowed range an error is
    raised
  }
  result := 0;

  DelimiterPos := Pos(PORT_DELIMITER,Address);
  if DelimiterPos <> 0 then
  begin
    // A Port Value exists, extract it and validate.
    sPort := StrRight(Address, Length(Address) - DelimiterPos);
    try
      result := StrToInt(sPort);
    except
      on EConvertError do
        raise EInvalidNetAddress.Create('Invalid Port Value "' + sPort + '"');
    end;
    if (result < 1) or (result > 9999) then
      raise EInvalidNetAddress.Create('Invalid Port Value "' + sPort + '"');
  end;
end;

function ExtractAddressFromNetAddress(const Address:String): String;
var
  DelimiterPos: Integer;
begin
  { The Address portion of the NetAddress is anything preceeding the
    Port Delimiter if present
  }
  result := Address;

  DelimiterPos := Pos(PORT_DELIMITER,Address);
  if DelimiterPos <> 0 then
  begin
    result := StrLeft(Address, DelimiterPos - 1);
  end;
end;

function IsValidIPAddress(const Address: String): Boolean;
var Counter: Integer;
begin
  result := True;
  for Counter := 1 to Length(Address) do
  begin
    case Ord(Address[counter]) of
      46,48..57: // Numeric and/or decimal
    else
      result := False;
      exit;
    end;
  end;
end;

function IsValidServerName(const S: String): Boolean;
var
  I: Integer;
  C: AnsiChar;
begin
  // First Charcter must per Alpha
  Result := CharIsAlpha(S[1]);
  // Other Characters must ne AlphaNumeric, '_', or '.'
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if not (CharIsAlphaNum(C) or (C = '_') or (C = '.')) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure DecodeNetAddress(const Address: string; var AddressType: TNetAddressType;
  var AddressName: string; var AddressPort: Integer);
begin
  { There are 4 possible address formats allowed
    A) <IP Address>
    B) <Server Name>
    C) <IP Address>:<Port>
    D) <Server Name>:<Port>

    The following rules are used:
    1) Only one ':' can exists, and denotes the addition of a <Port> value
    2) A <Port> value must be integer value between 1 and 9999.
    3) A <IP Address> must contain numerals and '.' only.
    4) A <Server Name> must contain at least 1 Alpha character.

    Using the above rules we can easliy decode a NetAddress to the intended
    format without having to worring about 100% validating the legality
    of the address. After all the connection will fail if the address
    doesn't actually exists, even if it actually legal, so why both
  }

  // Check Rule 1: Only one ':' can exists, and denotes the addition of
  //               a <Port> value
  if StrCharCount(Address, PORT_DELIMITER) > 1 then
  begin
    // too many port delimeters, not a valid Net Address
    raise EInvalidNetAddress.Create('Too Many Port Delimiter Characters');
  end;

  // Check for a Port Value and validate it to Rule 2
  AddressPort := ExtractPortFromNetAddress(Address);

  // Extract Server/IP Portion from NetAddress
  AddressName := ExtractAddressFromNetAddress(Address);

  // Classify The Address Type, if the address does meet the required
  // rules raise an error
  if IsValidIPAddress(AddressName) then
  begin
    AddressType := natIPAddress;
  end
  else
  begin
    if IsValidServerName(AddressName) then
    begin
      AddressType := natServerName;
    end
    else
    begin
      raise EInvalidNetAddress.Create('Invalid Address Value "'
                                      + AddressName + '"');
    end;
  end;

end;

end.
