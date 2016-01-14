{$I tiDefines.inc}

unit tiEncrypt;

interface

uses
  Classes
  ,Contnrs
 ;

const
  cuSeedString : String = '12%6348i(oikruK**9oi57&^1`!@bd)';

type

  // Base encryption component. Wrappers Scott's encryption algorithm and
  // exposes the EncryptString & EncryptStream interface. Introduces the
  // NewSeed method to increase security in comms apps
  TtiEncryptAbs = class(TObject)
  private
    FSeed : ansistring;
    FIntSeed : Int64;
  protected
    procedure SetSeed   (const AValue: ansistring); virtual;
    procedure SetIntSeed(const AValue: Int64); virtual;
  public
    constructor Create; virtual;
    function    EncryptString(const psData : AnsiString): AnsiString; virtual; abstract;
    function    DecryptString(const psData : AnsiString): AnsiString; virtual; abstract;
    procedure   EncryptStream(const pSrc, pDest : TStream); virtual; abstract;
    procedure   DecryptStream(const pSrc, pDest : TStream); virtual; abstract;
    property    Seed   : ansistring read FSeed    write SetSeed;
    property    IntSeed : Int64  read FIntSeed write SetIntSeed;
    procedure   NewSeed;
  end;

  // A class reference for the TtiEncrypt descendants
  TtiEncryptClass = class of TtiEncryptAbs;

  // A class to hold the TtiEncrypt class mappings. The factory maintains
  // a list of these and uses the EncryptClass property to create the objects.
  TtiEncryptClassMapping = class(TObject)
  private
    FsMappingName : string;
    FEncryptClass : TtiEncryptClass;
  public
    Constructor Create(const AMappingName : string;
                        pEncryptClass     : TtiEncryptClass);
    property    MappingName : string read FsMappingName;
    property    EncryptClass : TtiEncryptClass read FEncryptClass;
  end;

  // Factory pattern - Create a descendant of the TtiEncrypt at runtime.
  TtiEncryptFactory = class(TObject)
  private
    FList : TObjectList;
    FsDefaultEncryptionType: string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   RegisterClass(const psEncryptionType : string;
                                     pEncryptClass : TtiEncryptClass);
    function    CreateInstance(const psEncryptionType : string): TtiEncryptAbs; overload;
    function    CreateInstance : TtiEncryptAbs; overload;
    procedure   AssignEncryptionTypes(AStrings : TStrings);
    property    DefaultEncryptionType : string
                read  FsDefaultEncryptionType
                write FsDefaultEncryptionType;

  end;


// The EncryptFactory is a singleton
function gEncryptFactory : TtiEncryptFactory;

var
  gTiEncryptClass : TtiEncryptClass;

implementation
uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiEncryptAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiEncryptAbs.Create;
begin
  inherited;
  FSeed := cuSeedString;
end;

procedure TtiEncryptAbs.NewSeed;
var
  i : integer;
  ls : string;
begin
  ls := '';
  for i := 1 to length(FSeed) do
    ls := ls + Char(Random(254) + 1);
  FSeed := ls;
end;

// A var to hold our single instance of the TtiEncryptFactory
var
  uEncryptFactory : TtiEncryptFactory;

// The EncryptFactory is a singleton
function gEncryptFactory : TtiEncryptFactory;
begin
  if uEncryptFactory = nil then
    uEncryptFactory := TtiEncryptFactory.Create;
  result := uEncryptFactory;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiEncryptFactory
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiEncryptFactory.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

// Assing the registered list of TtiEncrypt names to a stringList
// This can be used to populate a combobox with the available TtiEncrypt
// class types.
procedure TtiEncryptFactory.AssignEncryptionTypes(AStrings: TStrings);
var
  i : integer;
begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    AStrings.Add(TtiEncryptClassMapping(FList.Items[i]).MappingName);
end;

// Call the factory to create an instance of TtiEncrypt
function TtiEncryptFactory.CreateInstance(const psEncryptionType: string): TtiEncryptAbs;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if UpperCase(TtiEncryptClassMapping(FList.Items[i]).MappingName) =
         UpperCase(psEncryptionType) then begin
      result := TtiEncryptClassMapping(FList.Items[i]).EncryptClass.Create;
      Break; //==>
    end;

  Assert(result <> nil,
          Format('<%s> does not identify a registered Encryption class.',
                   [psEncryptionType]));

end;

function TtiEncryptFactory.CreateInstance: TtiEncryptAbs;
begin
  result := CreateInstance(FsDefaultEncryptionType);
end;

destructor TtiEncryptFactory.Destroy;
begin
  FList.Free;
  inherited;
end;

// Register a TtiEncrypt class for creation by the factory
procedure TtiEncryptFactory.RegisterClass(
  const psEncryptionType: string; pEncryptClass: TtiEncryptClass);
var
  i : integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TtiEncryptClassMapping(FList.Items[i]).MappingName) =
         UpperCase(psEncryptionType) then
      Assert(false,
              Format('Encryption class <%s> already registered.',
                      [psEncryptionType]));
  FList.Add(TtiEncryptClassMapping.Create(psEncryptionType, pEncryptClass));
  FsDefaultEncryptionType := UpperCase(psEncryptionType);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiEncryptClassMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Overloaded constructor - used to create an instance ot TtiEncryptClassMapping
// and to preset it's properties.
constructor TtiEncryptClassMapping.Create(const AMappingName: string;
  pEncryptClass: TtiEncryptClass);
begin
  inherited Create;
  FsMappingName :=  AMappingName;
  FEncryptClass := pEncryptClass;
end;

procedure TtiEncryptAbs.SetIntSeed(const AValue: Int64);
begin
  FIntSeed := AValue;
end;

procedure TtiEncryptAbs.SetSeed(const AValue: ansistring);
begin
  FSeed := AValue;
end;

initialization

finalization
  // Free the TtiEncryptFactory
  uEncryptFactory.Free;

end.

