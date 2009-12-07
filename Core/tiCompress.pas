unit tiCompress;

{$I tiDefines.inc}

interface
uses
  Classes
  ,Contnrs
  ,tiConstants
 ;

type

  // Pure abstract class defining interface of TtiCompress classes
  TtiCompressAbs = class(TObject)
  public
    function  CompressStream(  AFrom : TStream; ATo : TStream): Extended; virtual; abstract;
    procedure DecompressStream(AFrom : TStream; ATo : TStream); virtual; abstract;
    function  CompressBuffer(  const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer): Extended; virtual; abstract;
    procedure DecompressBuffer(const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer); virtual; abstract;
    function  CompressString(  const AFrom : string; var ATo : string): Extended; virtual; abstract;
    procedure DecompressString(const AFrom : string; var ATo : string); virtual; abstract;
    function  CompressFile(    const AFrom : string; const ATo : string): Extended; virtual; abstract;
    procedure DecompressFile(  const AFrom : string; const ATo : string); virtual; abstract;
  end;

  // A class reference for the TtiCompress descendants
  TtiCompressClass = class of TtiCompressAbs;

  // A class to hold the TtiCompress class mappings. The factory maintains
  // a list of these and uses the CompressClass property to create the objects.
  TtiCompressClassMapping = class(TObject)
  private
    FsMappingName : string;
    FCompressClass : TtiCompressClass;
  public
    Constructor Create(const AMappingName : string;
                        ACompressClass     : TtiCompressClass);
    property    MappingName : string read FsMappingName;
    property    CompressClass : TtiCompressClass read FCompressClass;
  end;

  // Factory pattern - Create a descendant of the TtiCompress at runtime.
  TtiCompressFactory = class(TObject)
  private
    FList : TObjectList;
    FDefaultCompressionType: string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   RegisterClass(const ACompressionName : string;
                                     ACompressClass : TtiCompressClass);
    function    CreateInstance(const ACompressionName : string): TtiCompressAbs; overload;
    function    CreateInstance : TtiCompressAbs; overload;
    procedure   AssignCompressionTypes(AStrings : TStrings);
    property    DefaultCompressionType : string
                read  FDefaultCompressionType
                write FDefaultCompressionType;

  end;


// The CompressFactory is a singleton
function  gCompressFactory : TtiCompressFactory;
function  tiCompressString(const AString: string; const pCompress: string = cgsCompressZLib): string;
procedure tiCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
procedure tiCompressStringToFile(const AString: string; const AFileName: string; const ACompress: string = cgsCompressZLib);

function  tiDeCompressString(const AString: string; const pCompress: string = cgsCompressZLib): string;
procedure tiDeCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
function  tiDecompressFileToString(const AFileName: string; const ACompress: string = cgsCompressZLib): string;
procedure tiDecompressFileToStream(const AFileName: string; const AStream: TStream; const ACompress: string = cgsCompressZLib);

var
  gTiCompressClass : TtiCompressClass;

implementation
uses
   SysUtils
  ,tiUtils
 ;

// A var to hold our single instance of the TtiCompressFactory
var
  uCompressFactory : TtiCompressFactory;

// The CompressFactory is a singleton
function gCompressFactory : TtiCompressFactory;
begin
  if uCompressFactory = nil then
    uCompressFactory := TtiCompressFactory.Create;
  result := uCompressFactory;
end;

function tiCompressString(const AString: string; const pCompress: string = cgsCompressZLib): string;
var
  lCompress: TtiCompressAbs;
begin
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.CompressString(AString, result);
  finally
    lCompress.Free;
  end;
end;

function tiDeCompressString(const AString: string; const pCompress: string = cgsCompressZLib): string;
var
  lCompress: TtiCompressAbs;
begin
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.DeCompressString(AString, result);
  finally
    lCompress.Free;
  end;
end;

procedure tiCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
var
  lCompress: TtiCompressAbs;
begin
  Assert(AStreamFrom<>nil, 'AStreamFrom not assigned');
  Assert(AStreamTo<>nil, 'AStreamTo not assigned');
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.CompressStream(AStreamFrom, AStreamTo);
  finally
    lCompress.Free;
  end;
end;

procedure tiDeCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
var
  lCompress: TtiCompressAbs;
begin
  Assert(AStreamFrom<>nil, 'AStreamFrom not assigned');
  Assert(AStreamTo<>nil, 'AStreamTo not assigned');
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.DecompressStream(AStreamFrom, AStreamTo);
  finally
    lCompress.Free;
  end;
end;

procedure tiCompressStringToFile(const AString: string; const AFileName: string; const ACompress: string = cgsCompressZLib);
var
  LStreamFrom: TMemoryStream;
  LStreamTo: TMemoryStream;
begin
  Assert(AFileName<>'', 'AFileName not assigned');
  LStreamFrom:= nil;
  LStreamTo:= nil;
  try
    LStreamFrom:= TMemoryStream.Create;
    LStreamTo:= TMemoryStream.Create;
    tiStringToStream(AString, LStreamFrom);
    tiCompressStream(LStreamFrom, LStreamTo, ACompress);
    LStreamTo.SaveToFile(AFileName);
  finally
    LStreamFrom.Free;
    LStreamTo.Free;
  end;
end;

procedure tiDecompressFileToStream(const AFileName: string; const AStream: TStream; const ACompress: string = cgsCompressZLib);
var
  LStreamFrom: TMemoryStream;
begin
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AStream<>nil, 'AStream not assigned');
  LStreamFrom:= TMemoryStream.Create;
  try
    LStreamFrom.LoadFromFile(AFileName);
    tiDecompressStream(LStreamFrom, AStream, ACompress);
  finally
    LStreamFrom.Free;
  end;
end;

function  tiDecompressFileToString(const AFileName: string; const ACompress: string = cgsCompressZLib): string;
var
  LStreamTo: TMemoryStream;
begin
  Assert(AFileName<>'', 'AFileName not assigned');
  LStreamTo:= TMemoryStream.Create;
  try
    tiDecompressFileToStream(AFileName, LStreamTo, ACompress);
    result:= tiStreamToString(LStreamTo);
  finally
    LStreamTo.Free;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressFactory
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCompressFactory.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

// Assing the registered list of TtiCompress names to a stringList
// This can be used to populate a combobox with the available TtiCompress
// class types.
procedure TtiCompressFactory.AssignCompressionTypes(AStrings: TStrings);
var
  i : integer;
begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    AStrings.Add(TtiCompressClassMapping(FList.Items[i]).MappingName);
end;

// Call the factory to create an instance of TtiCompress
function TtiCompressFactory.CreateInstance(const ACompressionName: string): TtiCompressAbs;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if UpperCase(TtiCompressClassMapping(FList.Items[i]).MappingName) =
         UpperCase(ACompressionName) then begin
      result := TtiCompressClassMapping(FList.Items[i]).CompressClass.Create;
      Break; //==>
    end;

  Assert(result <> nil,
          Format('<%s> does not identify a registered compression class.',
                   [ACompressionName]));

end;

function TtiCompressFactory.CreateInstance: TtiCompressAbs;
begin
  result := CreateInstance(FDefaultCompressionType);
end;

destructor TtiCompressFactory.Destroy;
begin
  FList.Free;
  inherited;
end;

// Register a TtiCompress class for creation by the factory
procedure TtiCompressFactory.RegisterClass(
  const ACompressionName: string; ACompressClass: TtiCompressClass);
var
  i : integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TtiCompressClassMapping(FList.Items[i]).MappingName) =
         UpperCase(ACompressionName) then
      Assert(false,
              Format('Compression class <%s> already registered.',
                      [ACompressionName]));
  FList.Add(TtiCompressClassMapping.Create(ACompressionName, ACompressClass));
  FDefaultCompressionType := UpperCase(ACompressionName);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressClassMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Overloaded constructor - used to create an instance ot TtiCompressClassMapping
// and to preset it's properties.
constructor TtiCompressClassMapping.Create(const AMappingName: string;
  ACompressClass: TtiCompressClass);
begin
  inherited Create;
  FsMappingName :=  AMappingName;
  FCompressClass := ACompressClass;
end;

initialization

finalization
  // Free the TtiCompressFactory
  uCompressFactory.Free;

end.

















