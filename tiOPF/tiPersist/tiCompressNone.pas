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
    Provide no compression.

  Classes:
    TtiCompressNone: Implement the NullObject pattern - provide no compression
                     but make the application think compression is being used.

  ToDo:
    None.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiCompressNone;

interface
uses
  tiCompressAbs
  ,Classes
  ;

type

  // Null object pattern - implement the TtiCompress, but with no compression
  // ---------------------------------------------------------------------------
  TtiCompressNone = class( TtiCompressAbs )
  private
    procedure CopyFile( const psFrom, psTo : string ) ;
  public
    function  CompressStream(   pFrom : TStream ; pTo : TStream ) : real ; override ;
    procedure DecompressStream( pFrom : TStream ; pTo : TStream ) ; override ;
    function  CompressBuffer(   const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) : real ; override ;
    procedure DecompressBuffer( const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) ; override ;
    function  CompressString(   const psFrom : string ; var psTo : string )   : real ; override ;
    procedure DecompressString( const psFrom : string ; var psTo : string )   ; override ;
    function  CompressFile(     const psFrom : string ; const psTo : string ) : real ; override ;
    procedure DecompressFile(   const psFrom : string ; const psTo : string ) ; override ;
  end ;

implementation
uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,ctiPersist
  ,tiUtils
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressNone
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Compress a buffer
function TtiCompressNone.CompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer) : real ;
begin
  Assert( false, 'Not implemented yet.' ) ;
  result := 0 ;
end;

// Compress a file
// -----------------------------------------------------------------------------
function TtiCompressNone.CompressFile(const psFrom, psTo: string) : real ;
begin
  CopyFile( psFrom, psTo ) ;
  result := 1 ;
end;

// Compress a TStream
// -----------------------------------------------------------------------------
function TtiCompressNone.CompressStream(pFrom, pTo: TStream) : real ;
begin
  pFrom.Seek( 0, soFromBeginning ) ;
  pTo.CopyFrom( pFrom, pFrom.Size ) ;
  pFrom.Seek( 0, soFromBeginning ) ;
  pTo.Seek( 0, soFromBeginning ) ;
  result := 100 ;
end;

// Compress a string
// -----------------------------------------------------------------------------
function TtiCompressNone.CompressString(const psFrom: string;
  var psTo: string) : real ;
begin
  psTo := psFrom ;
  result := 100 ;
end;

// Copy a file from one place to another
// -----------------------------------------------------------------------------
procedure TtiCompressNone.CopyFile(const psFrom, psTo: string);
begin
  if FileExists( psTo ) then
    SysUtils.DeleteFile( psTo );
  tiCopyFile( psFrom, psTo );
end;

// Decompress a buffer
// -----------------------------------------------------------------------------
procedure TtiCompressNone.DecompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer);
begin
  Assert( false, 'Not implemented yet.' ) ;
end;

// Decompress a file
// -----------------------------------------------------------------------------
procedure TtiCompressNone.DecompressFile(const psFrom, psTo: string);
begin
  CopyFile( psFrom, psTo ) ;
end;

// Decompress a TStream
// -----------------------------------------------------------------------------
procedure TtiCompressNone.DecompressStream(pFrom, pTo: TStream);
begin
  pFrom.Seek( 0, soFromBeginning ) ;
  pTo.CopyFrom( pFrom, pFrom.Size ) ;
  pFrom.Seek( 0, soFromBeginning ) ;
  pTo.Seek( 0, soFromBeginning ) ;
end;

// Decompress a string
// -----------------------------------------------------------------------------
procedure TtiCompressNone.DecompressString(const psFrom: string;
  var psTo: string);
begin
  psTo := psFrom ;
end;


initialization
  // Register the TtiCompress with the CompressFactory
  gCompressFactory.RegisterClass( cgsCompressNone, TtiCompressNone ) ;
  gtiCompressClass := TtiCompressNone ;

end.

