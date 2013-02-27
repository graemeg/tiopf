unit Test_SclNet;

interface

uses
  TestFrameWork, SclNet;

type
  TDecodeNetAddressTest = class(TTestCase)
  published
    procedure TestIPAddress;
    procedure TestBadIPAddress;
    procedure TestIPAddressWithPort;
    procedure TestServerAddressNoPort;
    procedure TestServerAddressWithGoodPort;
    procedure TestServerAddressWithPortAboveValidRange;
    procedure TestServerAddressWithPortBelowValidRange;
    procedure TestServerAddressWithTextInPort;
  end;

implementation

procedure TDecodeNetAddressTest.TestIPAddress;
const
  Address: string = '192.168.0.2';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
begin
  DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  Check(AddressName = Address, 'Incorrect address name returned.');
  Check(AddressType = natIPAddress, 'Incorrect address type returned.');
end;

procedure TDecodeNetAddressTest.TestBadIPAddress;
const
  Address = '192.and.0.2';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
  ExceptionFlag: Boolean;
begin
  ExceptionFlag := False;
  try
    DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  except
    on EInvalidNetAddress do
      ExceptionFlag := true;
  end;
  Check(ExceptionFlag, 'Exception Not Raised');
end;

procedure TDecodeNetAddressTest.TestIPAddressWithPort;
const
  Address = '192.168.0.2:8080';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
begin
  DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  Check(AddressType = natIPAddress, 'Incorrect address type returned.');
  Check(AddressName = '192.168.0.2', 'Incorrect address name returned.');
  Check(AddressPort = 8080, 'Incorrect address port returned.');
end;

procedure TDecodeNetAddressTest.TestServerAddressNoPort;
const
  Address = 'www.my_server.com';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
begin
  DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  Check(AddressName = Address, 'Incorrect server address name returned.');
  Check(AddressType = natServerName, 'Incorrect address type returned.');
end;

procedure TDecodeNetAddressTest.TestServerAddressWithGoodPort;
const
  Address = 'frontline:8080';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
begin
  DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  Check(AddressName = 'frontline', 'Address Name incorrectly returned');
  Check(AddressType = natServerName, 'Address Type incorrectly returned');
  Check(AddressPort = 8080, 'Address Port incorrectly returned');
end;

procedure TDecodeNetAddressTest.TestServerAddressWithPortAboveValidRange;
const
  Address = 'frontline:80800';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
  ExceptionFlag: Boolean;
begin
  ExceptionFlag := False;
  try
    DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  except
    on EInvalidNetAddress do
      ExceptionFlag := True;
  end;
  Check(ExceptionFlag = True, 'Port Number above valid range.');
end;

procedure TDecodeNetAddressTest.TestServerAddressWithPortBelowValidRange;
const
  Address = 'frontline:-1';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
  ExceptionFlag: Boolean;
begin
  ExceptionFlag := False;
  try
    DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  except
    on EInvalidNetAddress do
      ExceptionFlag := True;
  end;
  Check(ExceptionFlag = True, 'Port Number below valid range.');
end;

procedure TDecodeNetAddressTest.TestServerAddressWithTextInPort;
const
  Address = 'frontline:80a80';
var
  AddressType: TNetAddressType;
  AddressName: string;
  AddressPort: Integer;
  ExceptionFlag: Boolean;
begin
  ExceptionFlag := False;
  try
    DecodeNetAddress(Address, AddressType, AddressName, AddressPort);
  except
    on EInvalidNetAddress do
      ExceptionFlag := True;
  end;
  Check(ExceptionFlag = True, 'Text in Port Number.');
end;

initialization
 TestFramework.RegisterTest(TDecodeNetAddressTest.Suite);

end.


