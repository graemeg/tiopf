{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    June 2000, Peter Hinrichsen, Created

  Purpose:
    Provide a wrapper around various compression libraries so they can be
    used interchangeably.

  Classes:
    TtiCompress:             Pure abstract class defining interface of TtiCompress
                             classes
    TtiCompressClass:        A class reference for the TtiCompress descendants
    TtiCompressClassMapping: A class to hold the TtiCompress class mappings.
                             The factory maintains a list of these and uses the
                             CompressClass property to create the objects.
    TtiCompressFactory:      Factory pattern - Create a descendant of the
                             TtiCompress at runtime.

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiCompressAbs;

interface
uses
  Classes
  ,Contnrs
  ;

type

  // Pure abstract class defining interface of TtiCompress classes
  // ---------------------------------------------------------------------------
  TtiCompressAbs = class( TObject )
  public
    function  CompressStream(   pFrom : TStream ; pTo : TStream ) : real ; virtual ; abstract ;
    procedure DecompressStream( pFrom : TStream ; pTo : TStream ) ; virtual ; abstract ;
    function  CompressBuffer(   const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) : real ; virtual ; abstract ;
    procedure DecompressBuffer( const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) ; virtual ; abstract ;
    function  CompressString(   const psFrom : string ; var psTo : string )   : real ; virtual ; abstract ;
    procedure DecompressString( const psFrom : string ; var psTo : string )   ; virtual ; abstract ;
    function  CompressFile(     const psFrom : string ; const psTo : string ) : real ; virtual ; abstract ;
    procedure DecompressFile(   const psFrom : string ; const psTo : string ) ; virtual ; abstract ;
  end ;

  // A class reference for the TtiCompress descendants
  // ---------------------------------------------------------------------------
  TtiCompressClass = class of TtiCompressAbs ;

  // A class to hold the TtiCompress class mappings. The factory maintains
  // a list of these and uses the CompressClass property to create the objects.
  // ---------------------------------------------------------------------------
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
  // ---------------------------------------------------------------------------
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
// -----------------------------------------------------------------------------
function gCompressFactory : TtiCompressFactory ;

var
  gTiCompressClass : TtiCompressClass ;

implementation
uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ;

// A var to hold our single instance of the TtiCompressFactory
var
  uCompressFactory : TtiCompressFactory ;

// The CompressFactory is a singleton
// -----------------------------------------------------------------------------
function gCompressFactory : TtiCompressFactory ;
begin
  if uCompressFactory = nil then
    uCompressFactory := TtiCompressFactory.Create ;
  result := uCompressFactory ;
end ;

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
// -----------------------------------------------------------------------------
procedure TtiCompressFactory.AssignCompressionTypes(pStrings: TStrings);
var
  i : integer ;
begin
  pStrings.Clear ;
  for i := 0 to FList.Count - 1 do
    pStrings.Add( TtiCompressClassMapping( FList.Items[i] ).MappingName ) ;
end;

// Call the factory to create an instance of TtiCompress
// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
function TtiCompressFactory.CreateInstance: TtiCompressAbs;
begin
  result := CreateInstance( FsDefaultCompressionType ) ;
end;

// -----------------------------------------------------------------------------
destructor TtiCompressFactory.Destroy;
begin
  FList.Free ;
  inherited;
end;

// Register a TtiCompress class for creation by the factory
// -----------------------------------------------------------------------------
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






