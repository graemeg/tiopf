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
  TtiCompressAbs = class( TObject )
  public
    function  CompressStream(   pFrom : TStream ; pTo : TStream ): Extended; virtual; abstract;
    procedure DecompressStream( pFrom : TStream ; pTo : TStream ); virtual; abstract;
    function  CompressBuffer(   const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer): Extended; virtual; abstract;
    procedure DecompressBuffer( const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer); virtual; abstract;
    function  CompressString(   const psFrom : string ; var psTo : string ): Extended; virtual; abstract;
    procedure DecompressString( const psFrom : string ; var psTo : string ); virtual; abstract;
    function  CompressFile(     const psFrom : string ; const psTo : string ): Extended; virtual; abstract;
    procedure DecompressFile(   const psFrom : string ; const psTo : string ); virtual; abstract;
  end ;

  // A class reference for the TtiCompress descendants
  TtiCompressClass = class of TtiCompressAbs ;

  // A class to hold the TtiCompress class mappings. The factory maintains
  // a list of these and uses the CompressClass property to create the objects.
  TtiCompressClassMapping = class( TObject )
  private
    FsMappingName  : string;
    FCompressClass : TtiCompressClass;
  public
    Constructor Create( const psMappingName : string ;
                        pCompressClass      : TtiCompressClass ) ;
    property    MappingName : string read FsMappingName ;
    property    CompressClass : TtiCompressClass read FCompressClass ;
  end ;

  // Factory pattern - Create a descendant of the TtiCompress at runtime.
  TtiCompressFactory = class( TObject )
  private
    FList : TObjectList ;
    FsDefaultCompressionType: string;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   RegisterClass( const psCompressionType : string ;
                                     pCompressClass : TtiCompressClass ) ;
    function    CreateInstance( const psCompressionType : string ) : TtiCompressAbs ; overload ;
    function    CreateInstance : TtiCompressAbs ; overload ;
    procedure   AssignCompressionTypes( pStrings : TStrings ) ;
    property    DefaultCompressionType : string
                read  FsDefaultCompressionType
                write FsDefaultCompressionType ;

  end ;


// The CompressFactory is a singleton
function  gCompressFactory : TtiCompressFactory ;
function  tiCompressString(const pString: string; const pCompress: string = cgsCompressZLib): string;
function  tiDeCompressString(const pString: string; const pCompress: string = cgsCompressZLib): string;
procedure tiDeCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
procedure tiCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);

var
  gTiCompressClass : TtiCompressClass ;

implementation
uses
  SysUtils
  ;

// A var to hold our single instance of the TtiCompressFactory
var
  uCompressFactory : TtiCompressFactory ;

// The CompressFactory is a singleton
function gCompressFactory : TtiCompressFactory ;
begin
  if uCompressFactory = nil then
    uCompressFactory := TtiCompressFactory.Create ;
  result := uCompressFactory ;
end ;

function tiCompressString(const pString: string; const pCompress: string = cgsCompressZLib): string;
var
  lCompress: TtiCompressAbs ;
begin
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.CompressString(pString, result);
  finally
    lCompress.Free;
  end;
end;

function tiDeCompressString(const pString: string; const pCompress: string = cgsCompressZLib): string;
var
  lCompress: TtiCompressAbs ;
begin
  lCompress:= gCompressFactory.CreateInstance(pCompress);
  try
    lCompress.DeCompressString(pString, result);
  finally
    lCompress.Free;
  end;
end;

procedure tiCompressStream(AStreamFrom, AStreamTo: TStream; const pCompress: string = cgsCompressZLib);
var
  lCompress: TtiCompressAbs ;
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
  lCompress: TtiCompressAbs ;
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

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressFactory
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCompressFactory.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

// Assing the registered list of TtiCompress names to a stringList
// This can be used to populate a combobox with the available TtiCompress
// class types.
procedure TtiCompressFactory.AssignCompressionTypes(pStrings: TStrings);
var
  i : integer ;
begin
  pStrings.Clear ;
  for i := 0 to FList.Count - 1 do
    pStrings.Add( TtiCompressClassMapping( FList.Items[i] ).MappingName ) ;
end;

// Call the factory to create an instance of TtiCompress
function TtiCompressFactory.CreateInstance( const psCompressionType: string): TtiCompressAbs;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FList.Count - 1 do
    if UpperCase( TtiCompressClassMapping( FList.Items[i] ).MappingName ) =
         UpperCase( psCompressionType ) then begin
      result := TtiCompressClassMapping( FList.Items[i] ).CompressClass.Create ;
      Break ; //==>
    end ;

  Assert( result <> nil,
          Format( '<%s> does not identify a registered compression class.',
                   [psCompressionType] )) ;

end;

function TtiCompressFactory.CreateInstance: TtiCompressAbs;
begin
  result := CreateInstance( FsDefaultCompressionType ) ;
end;

destructor TtiCompressFactory.Destroy;
begin
  FList.Free ;
  inherited;
end;

// Register a TtiCompress class for creation by the factory
procedure TtiCompressFactory.RegisterClass(
  const psCompressionType: string; pCompressClass: TtiCompressClass);
var
  i : integer ;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase( TtiCompressClassMapping( FList.Items[i] ).MappingName ) =
         UpperCase( psCompressionType ) then
      Assert( false,
              Format( 'Compression class <%s> already registered.',
                      [psCompressionType] )) ;
  FList.Add( TtiCompressClassMapping.Create( psCompressionType, pCompressClass )) ;
  FsDefaultCompressionType := UpperCase( psCompressionType ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressClassMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Overloaded constructor - used to create an instance ot TtiCompressClassMapping
// and to preset it's properties.
constructor TtiCompressClassMapping.Create(const psMappingName: string;
  pCompressClass: TtiCompressClass);
begin
  inherited Create ;
  FsMappingName :=  psMappingName ;
  FCompressClass := pCompressClass ;
end;

initialization

finalization
  // Free the TtiCompressFactory
  uCompressFactory.Free ;

end.






