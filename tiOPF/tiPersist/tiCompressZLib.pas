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
    Provide ZLib compression.

  Classes:
    TtiCompressZLib:         Implement TtiCompress using the ZLib library that
                             comes with Delphi

  ToDo:
    Implement string compression using a buffer, not via a TStringStream.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiCompressZLib;

interface
uses
  tiCompressAbs
  ,Classes
  ;

type

  // Implement TtiCompress using the ZLib library that comes with Delphi
  // ---------------------------------------------------------------------------
  TtiCompressZLib = class( TtiCompressAbs )
  private
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
  ZLib
  ,SysUtils
  ,cTIPersist
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressZLib
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Compress a buffer
function TtiCompressZLib.CompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer) : real ;
begin
  ZLib.CompressBuf( pFrom, piFromSize, pTo, piToSize ) ;
  if piFromSize <> 0 then
    result := piToSize / piFromSize * 100
  else
    result := 0 ;
end;

// Compress a file
// -----------------------------------------------------------------------------
function TtiCompressZLib.CompressFile(const psFrom : string ; const psTo: string) : real ;
var
  lStreamFrom : TFileStream ;
  lStreamTo   : TFileStream ;
begin
  lStreamFrom := TFileStream.Create( psFrom, fmOpenRead or fmShareExclusive ) ;
  try
    lStreamTo   := TFileStream.Create( psTo, fmCreate or fmShareExclusive ) ;
    try
      result := CompressStream( lStreamFrom, lStreamTo ) ;
    finally
      lStreamTo.Free ;
    end;
  finally
    lStreamFrom.Free ;
  end;
end;

// Compress a stream
// -----------------------------------------------------------------------------
function TtiCompressZLib.CompressStream(pFrom, pTo: TStream) : real ;
var
  liFromSize : integer ;
  liToSize   : integer ;
  lBufFrom   : Pointer ;
  lBufTo     : Pointer ;
begin
  Assert( pFrom <> nil, 'From stream unassigned' ) ;
  Assert( pTo <> nil, 'To stream unassigned' ) ;

  try
    pFrom.Position := 0 ;

    if pFrom.Size = 0 then
    begin
      pTo.Size := 0 ;
      result := 0 ;
      Exit ; //==>
    end ;

    liFromSize := pFrom.Size ;
    GetMem( lBufFrom, liFromSize ) ;
    try
      pFrom.ReadBuffer( lBufFrom^, liFromSize ) ;
      try
        result := CompressBuffer( lBufFrom, liFromSize, lBufTo, liToSize ) ;
        pTo.Size := 0 ;
        pTo.WriteBuffer( lBufTo^, liToSize ) ;
      finally
        FreeMem( lBufTo ) ;
      end ;
    finally
      FreeMem( lBufFrom ) ;
    end ;

    pFrom.Position := 0 ;
    pTo.Position := 0 ;

  except
    on e:exception do
      raise exception.Create( 'Error in TtiCompressZLib.CompressStream. Message: ' +
                              e.message ) ;
  end ;

end;

// Compress a string
// -----------------------------------------------------------------------------
function TtiCompressZLib.CompressString(const psFrom: string;
  var psTo: string) : real ;
var
  lStreamFrom : TStringStream ;
  lStreamTo   : TStringStream ;
begin
  { TODO : Perform this compression directly on the string as a buffer,
           don't go through the stream stage. }
  lStreamFrom := TStringStream.Create( psFrom ) ;
  try
    lStreamTo   := TStringStream.Create( '' ) ;
    try
      result := CompressStream( lStreamFrom, lStreamTo ) ;
      psTo   := lStreamTo.DataString ;
    finally
      lStreamTo.Free ;
    end;
  finally
    lStreamFrom.Free ;
  end;
end;

// Decompress a buffer
// -----------------------------------------------------------------------------
procedure TtiCompressZLib.DecompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer);
begin
  ZLib.DecompressBuf( pFrom, piFromSize, piFromSize*2, pTo, piToSize ) ;
end;

// Decompress a file
// -----------------------------------------------------------------------------
procedure TtiCompressZLib.DecompressFile(const psFrom, psTo: string);
var
  lStreamFrom : TFileStream ;
  lStreamTo   : TFileStream ;
begin
  lStreamFrom := TFileStream.Create( psFrom, fmOpenRead or fmShareExclusive ) ;
  try
    lStreamTo   := TFileStream.Create( psTo, fmCreate or fmShareExclusive ) ;
    try
      DecompressStream( lStreamFrom, lStreamTo ) ;
    finally
      lStreamTo.Free ;
    end;
  finally
    lStreamFrom.Free ;
  end;
end;

// Decompress a stream
// -----------------------------------------------------------------------------
procedure TtiCompressZLib.DecompressStream(pFrom, pTo: TStream);
var
  liToSize : integer ;
  liFromSize : integer ;
  lBufFrom : Pointer ;
  lBufTo   : Pointer ;
begin

  try
    if pFrom.Size = 0 then
    begin
      pTo.Size := 0 ;
      Exit ; //==>
    end ;

    pFrom.Position := 0 ;
    pTo.Size := 0 ;
    liFromSize := pFrom.Size ;
    GetMem( lBufFrom,    liFromSize ) ;
    try
      try
        pFrom.ReadBuffer( lBufFrom^, liFromSize ) ;
        DecompressBuffer( lBufFrom, liFromSize, lBufTo, liToSize ) ;
        pTo.Size := 0 ;
        pTo.WriteBuffer( lBufTo^, liToSize ) ;
      finally
        FreeMem( lBufTo ) ;
      end ;
    finally
      FreeMem( lBufFrom ) ;
    end ;
    pFrom.Position := 0 ;
    pTo.Position := 0 ;
  except
    on e:exception do
      raise exception.Create( 'Error in TtiCompressZLib.DeCompressStream. Message: ' +
                              e.message ) ;
  end ;

end;

// Decompress a string
// -----------------------------------------------------------------------------
procedure TtiCompressZLib.DecompressString(const psFrom: string;
  var psTo: string);
var
  lStreamFrom : TStringStream ;
  lStreamTo   : TStringStream ;
begin
  lStreamFrom := TStringStream.Create( psFrom ) ;
  try
    lStreamTo   := TStringStream.Create( '' ) ;
    try
      DecompressStream( lStreamFrom, lStreamTo ) ;
      psTo   := lStreamTo.DataString ;
    finally
      lStreamTo.Free ;
    end;
  finally
    lStreamFrom.Free ;
  end;
end;

initialization
  // Register the TtiCompressZLib class with the factory
  gCompressFactory.RegisterClass( cgsCompressZLib, TtiCompressZLib ) ;
  gtiCompressClass := TtiCompressZLib ;
  
end.

